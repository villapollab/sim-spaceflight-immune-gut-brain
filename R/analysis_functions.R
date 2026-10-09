# Shared statistical analysis functions for HU x IR x Sex outcome analysis
#
# Decision tree per outcome:
#   1. Pool the outcome's data (all Sex/HU/IR groups) -> Shapiro-Wilk test for normality.
#   2. If normal (p > alpha):
#        - 2-way ANOVA (IR * HU, Type III SS) run separately within each Sex
#        - 3-way ANOVA (IR * HU * Sex, Type III SS)
#        - Any significant interaction (3-way, or a 2-way collapsed from the 3-way model,
#          or the within-sex IR:HU interaction) triggers Tukey HSD (via emmeans) on the
#          relevant group combinations.
#      If not normal (p <= alpha):
#        - GLM (family chosen by data shape) with Type III (car::Anova) tested the same way,
#          in place of ANOVA
#        - Any significant interaction triggers Dunn's test (rank-based, FSA::dunnTest) on
#          the relevant raw-data group combinations.
#
# GLM family selection (choose_glm_family):
#   - all values non-negative integers          -> quasipoisson(link = "log")
#   - all values strictly positive, continuous   -> Gamma(link = "log")
#   - non-negative, contains zero, continuous     -> Gamma(link = "log") on values shifted to be >0
#   - contains negative values                    -> gaussian() on rank-transformed values
#     (rank-based GLM; a Gamma/log-link model is undefined for negative data)

suppressPackageStartupMessages({
  library(dplyr)
  library(car)
  library(emmeans)
  library(FSA)
  library(readxl)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(tidyr)
  library(showtext)
  library(sysfonts)
  library(ARTool)
})

# Axis labels copied from the original Prism figures. Callers set the working
# directory to the project root; the second path covers being sourced from
# inside rmarkdown_analyses/.
local({
  for (p in c("R/prism_axis_labels.R", "../R/prism_axis_labels.R")) {
    if (file.exists(p)) { source(p); break }
  }
})

# ---- Fonts ---------------------------------------------------------------
# The base pdf() device cannot render the Greek/Delta characters used in the
# Prism axis labels, and R's built-in "ArialMT" mapping has no glyph coverage
# for them either. showtext draws real system Arial instead, which covers
# everything except U+207A (superscript plus) -- that character is normalised
# to a plain "+" in prism_axis_labels.R.
PLOT_FONT <- "Arial"

.setup_plot_font <- function() {
  arial_dir <- "/System/Library/Fonts/Supplemental"
  reg <- file.path(arial_dir, "Arial.ttf")
  bld <- file.path(arial_dir, "Arial Bold.ttf")
  ital <- file.path(arial_dir, "Arial Italic.ttf")
  bi <- file.path(arial_dir, "Arial Bold Italic.ttf")
  if (file.exists(reg) && !(PLOT_FONT %in% sysfonts::font_families())) {
    sysfonts::font_add(family = PLOT_FONT, regular = reg,
                        bold = if (file.exists(bld)) bld else reg,
                        italic = if (file.exists(ital)) ital else reg,
                        bolditalic = if (file.exists(bi)) bi else reg)
  }
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
}
.setup_plot_font()

ALPHA <- 0.05

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ---- Data loading -----------------------------------------------------

load_outcome_data <- function(outcome_name, block = 1, xlsx_path) {
  df <- suppressMessages(read_excel(xlsx_path, sheet = "Long_Format_Data"))
  d <- df %>% filter(Outcome == outcome_name, Block == block)
  d$HU  <- factor(d$HU, levels = c("Sham", "HU"))
  d$IR  <- factor(d$IR)
  d$Sex <- factor(d$Sex, levels = c("Females", "Males"))
  d <- droplevels(as.data.frame(d))
  d
}

# ---- Outlier detection (ROUT method, Q = 1% FDR by default) ---------------
#
# Adaptation of the ROUT method (Motulsky & Brown, 2006, BMC Bioinformatics)
# to a simple grouped column (no regression model): the "fit" for a group is
# its median (a robust estimate of center); a robust SD is estimated from the
# smallest 68.27% of squared residuals (the fraction expected within 1 SD
# under normality, so it is not inflated by any true outliers in the upper
# tail); each point's residual is converted to a two-sided p-value via a
# t-distribution, and the Benjamini-Hochberg FDR procedure at the given Q
# flags outliers among that one group's points.

#' Flag ROUT outliers within a single numeric vector. Returns a logical
#' vector (same length/order as x, NA entries pass through as FALSE).
#' Removal is capped so at least `min_remaining` points always survive: if
#' more points qualify than that allows, only the most extreme ones (smallest
#' p-value) are kept flagged.
rout_outliers <- function(x, Q = 0.01, min_remaining = 3) {
  out <- rep(FALSE, length(x))
  idx <- which(!is.na(x))
  xv <- x[idx]
  n <- length(xv)
  if (n < 3) return(out)  # can't robustly test fewer than 3 points

  k <- 1  # one fitted parameter: the median
  med <- median(xv)
  resid <- xv - med

  # Robust Standard Deviation of the Residuals, exactly as defined in
  # Motulsky & Brown (2006): the 68.27th percentile of the ABSOLUTE residuals,
  # scaled by N/(N-K).
  #
  # Do NOT substitute "root-mean-square of the smallest 68.27% of squared
  # residuals" -- that estimates E[z^2 | |z|<1]*sigma^2 = 0.291*sigma^2, i.e.
  # roughly half of sigma, which makes every residual look about twice as
  # extreme and flags a false outlier in 20-75% of perfectly clean cells.
  rsdr <- as.numeric(stats::quantile(abs(resid), 0.6827, type = 7)) * n / (n - k)
  if (!is.finite(rsdr) || rsdr <= 0) return(out)

  t_stat <- abs(resid) / rsdr
  df_t <- max(n - k, 1)
  p_val <- 2 * pt(t_stat, df = df_t, lower.tail = FALSE)

  # Benjamini-Hochberg FDR (base R's p.adjust step-up implementation is used
  # rather than a hand-rolled rank/threshold comparison so that points with
  # identical p-values -- common here, since replicate values are often
  # tied, e.g. several exact zeros -- always receive identical q-values and
  # therefore an identical outlier call).
  q_val <- stats::p.adjust(p_val, method = "BH")
  is_outlier <- q_val <= Q

  max_removable <- max(n - min_remaining, 0)
  if (sum(is_outlier) > max_removable) {
    if (max_removable == 0) {
      is_outlier <- rep(FALSE, n)
    } else {
      flagged_idx <- which(is_outlier)
      keep_removed <- flagged_idx[order(p_val[flagged_idx])][seq_len(max_removable)]
      is_outlier <- rep(FALSE, n)
      is_outlier[keep_removed] <- TRUE
    }
  }

  out[idx] <- is_outlier
  out
}

#' Apply rout_outliers() separately within each Sex x HU x IR cell of `d`
#' and split into cleaned/removed data frames.
apply_outlier_removal <- function(d, Q = 0.01, min_remaining = 3) {
  d$.rout_outlier <- FALSE
  for (s in unique(d$Sex)) {
    for (hu in unique(d$HU)) {
      for (ir in unique(d$IR)) {
        sel <- which(d$Sex == s & d$HU == hu & d$IR == ir)
        if (length(sel) == 0) next
        d$.rout_outlier[sel] <- rout_outliers(d$Value[sel], Q = Q, min_remaining = min_remaining)
      }
    }
  }
  removed <- d[d$.rout_outlier, setdiff(names(d), ".rout_outlier"), drop = FALSE]
  cleaned <- d[!d$.rout_outlier, setdiff(names(d), ".rout_outlier"), drop = FALSE]
  list(cleaned = cleaned, removed = removed, n_removed = nrow(removed), Q = Q)
}

#' Load an outcome's data and immediately apply ROUT outlier removal.
#' Returns list(raw, cleaned, removed, n_removed, Q).
get_clean_data <- function(outcome_name, block = 1, xlsx_path, Q = 0.01, min_remaining = 3) {
  d_raw <- load_outcome_data(outcome_name, block, xlsx_path)
  or <- apply_outlier_removal(d_raw, Q = Q, min_remaining = min_remaining)
  c(list(raw = d_raw), or)
}

# ---- Normality ----------------------------------------------------------

#' Shapiro-Wilk on the RESIDUALS of the full 3-way model.
#'
#' The parametric-vs-nonparametric decision is about whether the ANOVA's error
#' term is normal, which is a statement about residuals, not about the raw
#' Value column. Testing pooled raw values conflates genuine IR/HU/Sex mean
#' differences with non-normality -- and that bias is one-directional, since a
#' real treatment effect makes the pooled data look non-normal and pushes the
#' outcome onto the nonparametric branch for no good reason. Residuals remove
#' the group means first, so only the shape of the error is tested.
#'
#' Falls back to the pooled raw values only if the model cannot be fit.
run_shapiro <- function(d) {
  vals <- d$Value[!is.na(d$Value)]
  if (length(vals) < 3 || length(unique(vals)) < 2) {
    return(list(W = NA_real_, p = NA_real_, n = length(vals),
                note = "Insufficient variation/N for Shapiro-Wilk", basis = NA_character_))
  }

  dd <- droplevels(d[!is.na(d$Value), ])
  resid <- NULL
  if (nlevels(dd$IR) >= 2 && nlevels(dd$HU) >= 2 && nlevels(dd$Sex) >= 2) {
    resid <- tryCatch(residuals(lm(Value ~ IR * HU * Sex, data = dd)), error = function(e) NULL)
  }

  basis <- "3-way model residuals"
  test_vals <- resid
  note <- NA_character_
  if (is.null(resid) || length(unique(round(resid, 12))) < 3) {
    basis <- "pooled raw values (model residuals unavailable)"
    test_vals <- vals
    note <- "Shapiro-Wilk fell back to pooled raw values (3-way model could not be fit)"
  }

  st <- tryCatch(shapiro.test(test_vals), error = function(e) NULL)
  if (is.null(st)) {
    return(list(W = NA_real_, p = NA_real_, n = length(vals),
                note = "Shapiro-Wilk failed to run", basis = basis))
  }
  list(W = unname(st$statistic), p = st$p.value, n = length(vals), note = note, basis = basis)
}

# ---- Parametric models (Type III SS via sum-to-zero contrasts) ----------

fit_type3_lm <- function(formula, data) {
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op))
  m <- lm(formula, data = data)
  at <- car::Anova(m, type = 3)
  list(model = m, anova = at)
}

run_2way_by_sex <- function(d) {
  out <- list()
  for (s in levels(d$Sex)) {
    sub <- droplevels(d[d$Sex == s & !is.na(d$Value), ])
    min_n <- nlevels(sub$IR) * nlevels(sub$HU) + 1
    if (nlevels(sub$IR) < 2 || nlevels(sub$HU) < 2 || nrow(sub) < min_n) {
      out[[s]] <- list(fit = NULL, note = "Insufficient design/data for 2-way ANOVA")
      next
    }
    fit <- tryCatch(fit_type3_lm(Value ~ IR * HU, sub), error = function(e) NULL)
    if (is.null(fit)) {
      out[[s]] <- list(fit = NULL, note = "2-way model fit failed")
      next
    }
    out[[s]] <- list(fit = fit, data = sub, note = NA_character_)
  }
  out
}

run_3way <- function(d) {
  sub <- droplevels(d[!is.na(d$Value), ])
  if (nlevels(sub$IR) < 2 || nlevels(sub$HU) < 2 || nlevels(sub$Sex) < 2) {
    return(list(fit = NULL, note = "Insufficient design for 3-way ANOVA"))
  }
  fit <- tryCatch(fit_type3_lm(Value ~ IR * HU * Sex, sub), error = function(e) NULL)
  if (is.null(fit)) return(list(fit = NULL, note = "3-way model fit failed"))
  list(fit = fit, data = sub, note = NA_character_)
}

term_p <- function(anova_tab, term) {
  if (is.null(anova_tab)) return(NA_real_)
  # ART's anova table keeps terms in a "Term" column; car::Anova uses rownames.
  if ("Term" %in% names(anova_tab)) {
    i <- which(anova_tab$Term == term)
    if (!length(i)) return(NA_real_)
    return(anova_tab[["Pr(>F)"]][i])
  }
  rn <- rownames(anova_tab)
  if (!(term %in% rn)) return(NA_real_)
  anova_tab[term, "Pr(>F)"]
}

#' TRUE if any interaction term involving IR is significant in a 3-way table.
#' When one is, the IR main effect is not the right thing to interpret, so the
#' main-effect post-hoc is suppressed in favour of the interaction comparisons.
ir_interaction_sig <- function(anova_tab, alpha = ALPHA) {
  ps <- vapply(c("IR:HU", "IR:Sex", "IR:HU:Sex"),
                function(t) term_p(anova_tab, t), numeric(1))
  any(!is.na(ps) & ps < alpha)
}

# ---- Nonparametric GLM branch --------------------------------------------

choose_glm_family <- function(x) {
  x <- x[!is.na(x)]
  if (all(x >= 0) && all(abs(x - round(x)) < 1e-8)) {
    # Non-negative counts.
    "quasipoisson"
  } else if (all(x > 0)) {
    # Strictly positive continuous -> Gamma with a log link.
    "Gamma_log"
  } else {
    # Anything with a zero OR a negative value -> shift-free rank-based
    # Gaussian GLM. A Gamma/log model is undefined at 0, and the old fix
    # (add half the smallest positive value) made p-values swing by orders of
    # magnitude with the size of that shift -- especially for the near-zero
    # perivascular-IgG outcomes, where the shifted zeros sat several log units
    # below the data and dominated the log-link fit. Ranking removes the
    # dependence on any shift entirely and is a standard nonparametric
    # analysis, which is what the non-normal branch is meant to be.
    "gaussian_rank"
  }
}

family_object <- function(fam) {
  switch(fam,
    quasipoisson  = quasipoisson(link = "log"),
    Gamma_log     = Gamma(link = "log"),
    gaussian_rank = gaussian()
  )
}

prep_glm_value <- function(sub, fam) {
  if (fam == "gaussian_rank") {
    sub$Value <- rank(sub$Value)
  }
  sub
}

run_glm_2way_by_sex <- function(d) {
  out <- list()
  for (s in levels(d$Sex)) {
    sub <- droplevels(d[d$Sex == s & !is.na(d$Value), ])
    if (nlevels(sub$IR) < 2 || nlevels(sub$HU) < 2) {
      out[[s]] <- list(fit = NULL, family = NA_character_, note = "Insufficient design for 2-way GLM")
      next
    }
    fam <- choose_glm_family(sub$Value)
    sub2 <- prep_glm_value(sub, fam)
    op <- options(contrasts = c("contr.sum", "contr.poly"))
    m <- tryCatch(glm(Value ~ IR * HU, data = sub2, family = family_object(fam),
                       control = glm.control(maxit = 200)), error = function(e) NULL)
    if (is.null(m)) {
      options(op)
      out[[s]] <- list(fit = NULL, family = fam, note = "2-way GLM fit failed")
      next
    }
    # car::Anova(type=3) on a GLM refits reduced models internally, so contr.sum
    # must still be in effect here -- restore defaults only after this call.
    at <- tryCatch(car::Anova(m, type = 3, test.statistic = "F"), error = function(e) NULL)
    options(op)
    out[[s]] <- list(fit = list(model = m, anova = at), family = fam, data = sub, note = NA_character_)
  }
  out
}

run_glm_3way <- function(d) {
  sub <- droplevels(d[!is.na(d$Value), ])
  if (nlevels(sub$IR) < 2 || nlevels(sub$HU) < 2 || nlevels(sub$Sex) < 2) {
    return(list(fit = NULL, family = NA_character_, note = "Insufficient design for 3-way GLM"))
  }
  fam <- choose_glm_family(sub$Value)
  sub2 <- prep_glm_value(sub, fam)
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  m <- tryCatch(glm(Value ~ IR * HU * Sex, data = sub2, family = family_object(fam),
                     control = glm.control(maxit = 200)), error = function(e) NULL)
  if (is.null(m)) {
    options(op)
    return(list(fit = NULL, family = fam, note = "3-way GLM fit failed"))
  }
  # car::Anova(type=3) on a GLM refits reduced models internally, so contr.sum
  # must still be in effect here -- restore defaults only after this call.
  at <- tryCatch(car::Anova(m, type = 3, test.statistic = "F"), error = function(e) NULL)
  options(op)
  list(fit = list(model = m, anova = at), family = fam, data = sub, note = NA_character_)
}

# ---- Unified model fitter -------------------------------------------------
#
# Fits ONE model for a design formula, dispatching on the outcome's branch:
#   parametric              -> lm  (Type III via contr.sum)
#   nonparametric, positive -> Gamma(log) or quasipoisson(log) GLM (Type III)
#   nonparametric, zeros/neg-> ART (Aligned Rank Transform; ARTool::art)
#
# ART is used instead of a rank-transformed Gaussian GLM because the plain
# rank transform gives INVALID factorial interaction tests, and this design's
# central claims are IR x HU and Sex interactions. ART aligns each effect
# before ranking (Wobbrock et al. 2011) and yields valid interaction tests;
# its contrasts (ART-C, Elkin et al. 2021) come from ARTool::art.con.
#
# Returns list(model, anova, type in {"lm","glm","art"}, family, data) or NULL.
fit_one <- function(formula, data, normal) {
  data <- droplevels(data[!is.na(data$Value), ])

  if (normal) {
    op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
    m <- tryCatch(lm(formula, data = data), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    at <- tryCatch(car::Anova(m, type = 3), error = function(e) NULL)
    return(list(model = m, anova = at, type = "lm", family = NA_character_, data = data))
  }

  fam <- choose_glm_family(data$Value)
  if (fam == "gaussian_rank") {
    m <- tryCatch(ARTool::art(formula, data = data), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    at <- tryCatch(as.data.frame(anova(m)), error = function(e) NULL)
    return(list(model = m, anova = at, type = "art",
                family = "ART (aligned rank transform)", data = data))
  }

  op <- options(contrasts = c("contr.sum", "contr.poly"))
  m <- tryCatch(glm(formula, data = data, family = family_object(fam),
                     control = glm.control(maxit = 200)), error = function(e) NULL)
  if (is.null(m)) { options(op); return(NULL) }
  at <- tryCatch(car::Anova(m, type = 3, test.statistic = "F"), error = function(e) NULL)
  options(op)
  list(model = m, anova = at, type = "glm", family = fam, data = data)
}

# ---- Post-hoc tests -------------------------------------------------------

tukey_posthoc <- function(fit, specs) {
  em <- emmeans(fit$model, specs)
  pw <- pairs(em, adjust = "tukey")
  as.data.frame(summary(pw))
}

make_group_var <- function(d, vars) {
  d$Group <- interaction(d[vars], sep = " | ", drop = TRUE)
  d
}

#' Match an emmeans contrast token (possibly factor-name-prefixed, e.g. "IR50")
#' back to one of a factor's level labels.
.match_level <- function(token, levs) {
  token <- trimws(token)
  if (token %in% levs) return(token)
  hit <- levs[vapply(levs, function(l) endsWith(token, l), logical(1))]
  if (length(hit)) hit[which.max(nchar(hit))] else NA_character_
}

#' Standardised pairwise contrasts from a fitted model (lm OR glm) via emmeans.
#' This is used for BOTH the parametric and nonparametric branches -- for the
#' nonparametric branch the model is a GLM (Gamma/log, quasipoisson/log, or a
#' rank-based Gaussian GLM), so the contrasts marginalise the fitted model
#' properly rather than pooling raw data across factors.
#'
#' type:
#'   "IR_within_HU"     IR doses compared within each HU level  (bracketable)
#'   "HU_within_IR"     Sham vs HU compared within each IR dose (bracketable)
#'   "Sex_within_group" Female vs Male within each IR x HU group (drawn as #)
#'
#' Returns a tidy data.frame (p_raw only; the outcome-wide BH correction is
#' applied once by the caller across every contrast for the outcome).
#' Resolve one side of an IR*HU emmeans contrast label (e.g. "0 Sham",
#' "IR0 HU", "100 HU") to its Prism group label, by matching each whitespace/
#' comma-separated token against the IR and HU level sets.
.side_to_group <- function(side, lev_IR, lev_HU) {
  toks <- trimws(strsplit(side, "[[:space:],]+")[[1]])
  ir <- NA_character_; hu <- NA_character_
  for (t in toks) {
    if (is.na(ir)) { mi <- .match_level(t, lev_IR); if (!is.na(mi)) { ir <- mi; next } }
    if (is.na(hu)) { mh <- .match_level(t, lev_HU); if (!is.na(mh)) hu <- mh }
  }
  ir_hu_to_group(ir, hu)
}

#' Parse one side of a pairwise-contrast label (from emmeans OR ARTool::art.con)
#' into its factor levels, by matching whitespace/comma-separated tokens against
#' the level sets. emmeans uses spaces ("0 Sham"), art.con uses commas
#' ("0,Sham,Females"); the split handles both. Returns c(IR=, HU=, Sex=).
.parse_cell <- function(side, lev_IR, lev_HU, lev_Sex) {
  toks <- trimws(strsplit(side, "[[:space:],]+")[[1]])
  ir <- NA_character_; hu <- NA_character_; sx <- NA_character_
  for (t in toks) {
    if (is.na(ir)) { m <- .match_level(t, lev_IR); if (!is.na(m)) { ir <- m; next } }
    if (is.na(hu)) { m <- .match_level(t, lev_HU); if (!is.na(m)) { hu <- m; next } }
    if (is.na(sx)) { m <- .match_level(t, lev_Sex); if (!is.na(m)) sx <- m }
  }
  c(IR = ir, HU = hu, Sex = sx)
}

#' All cell-pairwise contrasts from a fitted model, in one uniform long schema,
#' for BOTH engines: emmeans for lm/glm, ARTool::art.con for ART. Unadjusted
#' p-values only -- each named family applies its own correction downstream.
#' dims = "IRxHU" (within-sex model) or "IRxHUxSex" (3-way model).
cell_pairwise_raw <- function(fit, dims) {
  if (is.null(fit) || is.null(fit$model)) return(NULL)
  if (isTRUE(fit$type == "art")) {
    term <- if (dims == "IRxHU") "IR:HU" else "IR:HU:Sex"
    pw <- tryCatch(as.data.frame(ARTool::art.con(fit$model, term, adjust = "none")),
                    error = function(e) NULL)
  } else {
    spec <- if (dims == "IRxHU") ~ IR * HU else ~ IR * HU * Sex
    em <- tryCatch(emmeans::emmeans(fit$model, spec), error = function(e) NULL)
    pw <- if (is.null(em)) NULL else
      tryCatch(as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "none")),
               error = function(e) NULL)
  }
  if (is.null(pw) || nrow(pw) == 0) return(NULL)
  stat_col <- intersect(c("t.ratio", "z.ratio"), names(pw))[1]
  mf <- if (isTRUE(fit$type == "art")) fit$data else model.frame(fit$model)
  lev_IR <- levels(factor(mf$IR)); lev_HU <- levels(factor(mf$HU)); lev_Sex <- levels(factor(mf$Sex))

  rows <- lapply(seq_len(nrow(pw)), function(i) {
    sides <- strsplit(as.character(pw$contrast[i]), " - ", fixed = TRUE)[[1]]
    if (length(sides) != 2) return(NULL)
    a <- .parse_cell(sides[1], lev_IR, lev_HU, lev_Sex)
    b <- .parse_cell(sides[2], lev_IR, lev_HU, lev_Sex)
    data.frame(IR1 = a["IR"], HU1 = a["HU"], Sex1 = a["Sex"],
                IR2 = b["IR"], HU2 = b["HU"], Sex2 = b["Sex"],
                estimate = pw$estimate[i],
                Statistic = if (!is.na(stat_col)) pw[[stat_col]][i] else NA_real_,
                p_raw = pw$p.value[i], stringsAsFactors = FALSE, row.names = NULL)
  })
  out <- dplyr::bind_rows(rows)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out
}

#' Build the family-structured post-hoc table for one outcome.
#'
#' Comparison families (each a standard "simple main effects" decomposition,
#' Holm-adjusted WITHIN the family for family-wise error control -- Holm 1979,
#' which applies identically to the ANOVA, GLM, and ART branches):
#'   * IR within HU   -- IR doses compared within Sham, and within HU (the
#'                       "different IR doses when HU is introduced" question).
#'   * HU within IR   -- Sham vs HU compared at each IR dose.
#'   * vs Sham        -- every treatment/combined group vs the Sham control.
#'   * Female vs Male -- within each of the 6 groups (from the 3-way model).
#' A within-sex family runs only when that sex's IR / HU / IR:HU omnibus term is
#' significant; the sex family runs only when a Sex main effect or Sex
#' interaction is significant.
build_posthoc <- function(outcome_name, block, two, three, alpha = ALPHA) {
  rows <- list()
  add <- function(df) if (!is.null(df) && nrow(df) > 0) rows[[length(rows) + 1]] <<- df

  holm_within <- function(df, by = NULL) {
    df$p_adj <- NA_real_
    if (is.null(by)) { df$p_adj <- stats::p.adjust(df$p_raw, "holm"); return(df) }
    for (g in unique(df[[by]])) { idx <- which(df[[by]] == g); df$p_adj[idx] <- stats::p.adjust(df$p_raw[idx], "holm") }
    df
  }
  std <- function(df, panel, family, comparison) {
    data.frame(Panel = panel, Family = family, Comparison = comparison,
                Group1 = df$G1, Group2 = df$G2,
                estimate = round(df$estimate, 4), Statistic = round(df$Statistic, 4),
                p_raw = df$p_raw, p_adj = df$p_adj, stringsAsFactors = FALSE)
  }

  for (s in names(two)) {
    fit_s <- two[[s]]$fit
    if (is.null(fit_s) || is.null(fit_s$model)) next
    at <- fit_s$anova
    p_ir <- term_p(at, "IR"); p_hu <- term_p(at, "HU"); p_int <- term_p(at, "IR:HU")
    if (!any(c(p_ir, p_hu, p_int) < alpha, na.rm = TRUE)) next
    cp <- cell_pairwise_raw(fit_s, "IRxHU")
    if (is.null(cp)) next
    cp$G1 <- mapply(ir_hu_to_group, cp$IR1, cp$HU1)
    cp$G2 <- mapply(ir_hu_to_group, cp$IR2, cp$HU2)
    cp <- cp[!is.na(cp$G1) & !is.na(cp$G2) & cp$G1 != cp$G2, , drop = FALSE]
    if (!nrow(cp)) next

    f1 <- cp[cp$HU1 == cp$HU2, , drop = FALSE]              # IR within HU
    if (nrow(f1)) { f1 <- holm_within(f1, by = "HU1")
      add(std(f1, s, paste0("IR within HU (", s, ")"),
              sprintf("%s vs %s (IR, HU=%s)", f1$IR1, f1$IR2, f1$HU1))) }

    f2 <- cp[cp$IR1 == cp$IR2, , drop = FALSE]              # HU within IR
    if (nrow(f2)) { f2 <- holm_within(f2)
      add(std(f2, s, paste0("HU within IR (", s, ")"),
              sprintf("Sham vs HU (IR=%s)", f2$IR1))) }

    f3 <- cp[cp$G1 == "Sham" | cp$G2 == "Sham", , drop = FALSE]  # vs Sham control
    if (nrow(f3)) {
      other <- ifelse(f3$G1 == "Sham", f3$G2, f3$G1)
      f3$G1 <- other; f3$G2 <- "Sham"
      f3 <- holm_within(f3)
      add(std(f3, s, paste0("vs Sham control (", s, ")"),
              sprintf("%s vs Sham", other))) }
  }

  if (!is.null(three$fit) && !is.null(three$fit$model)) {
    at3 <- three$fit$anova
    sex_sig <- any(c(term_p(at3, "Sex"), term_p(at3, "IR:Sex"),
                      term_p(at3, "HU:Sex"), term_p(at3, "IR:HU:Sex")) < alpha, na.rm = TRUE)
    if (sex_sig) {
      cp3 <- cell_pairwise_raw(three$fit, "IRxHUxSex")
      if (!is.null(cp3)) {
        f4 <- cp3[cp3$IR1 == cp3$IR2 & cp3$HU1 == cp3$HU2 & cp3$Sex1 != cp3$Sex2, , drop = FALSE]
        if (nrow(f4)) {
          f4$G1 <- mapply(ir_hu_to_group, f4$IR1, f4$HU1); f4$G2 <- f4$G1
          f4 <- f4[!is.na(f4$G1), , drop = FALSE]
          if (nrow(f4)) { f4 <- holm_within(f4)
            add(std(f4, "Sex", "Female vs Male within group",
                    sprintf("Female vs Male (%s)", f4$G1))) }
        }
      }
    }
  }

  if (!length(rows)) return(NULL)
  out <- dplyr::bind_rows(rows)
  out$Outcome <- outcome_name; out$Block <- block
  out$Significance <- vapply(out$p_adj, function(p) { st <- p_to_stars(p); if (is.na(st)) "ns" else st }, character(1))
  out$Significant_0.05 <- ifelse(!is.na(out$p_adj) & out$p_adj < alpha, "Yes", "No")
  out
}

#' Female-vs-Male per-group table for the plot "#" markers and the Excel sheet.
#' Descriptive means/N per group, joined to the Sex-panel rows of the unified
#' post-hoc so the p-values share the outcome-wide BH correction.
build_sex_diff_table <- function(d, posthoc_df) {
  dd <- add_group_col(d)
  dd <- dd[!is.na(dd$Value) & !is.na(dd$Group), ]
  desc <- dd %>%
    dplyr::group_by(Group, Sex) %>%
    dplyr::summarise(n = dplyr::n(), mean = mean(Value), .groups = "drop")
  agg <- function(sx, col) vapply(GROUP_LEVELS, function(g) {
    v <- desc[[col]][desc$Group == g & desc$Sex == sx]
    if (length(v)) v[1] else NA_real_
  }, numeric(1))

  out <- data.frame(
    Group = factor(GROUP_LEVELS, levels = GROUP_LEVELS),
    n_Females = agg("Females", "n"), n_Males = agg("Males", "n"),
    Mean_Females = agg("Females", "mean"), Mean_Males = agg("Males", "mean"),
    p_raw = NA_real_, p_adj = NA_real_, stringsAsFactors = FALSE)

  if (!is.null(posthoc_df) && "Panel" %in% names(posthoc_df)) {
    sx <- posthoc_df[posthoc_df$Panel == "Sex", , drop = FALSE]
    if (nrow(sx)) {
      idx <- match(as.character(out$Group), as.character(sx$Group1))
      out$p_raw <- sx$p_raw[idx]
      out$p_adj <- sx$p_adj[idx]
    }
  }
  out$Hashes <- vapply(out$p_adj, p_to_hashes, character(1))
  out
}

# ---- Master per-outcome analysis ------------------------------------------

#' Run the full decision-tree analysis for one Outcome (and Block):
#' ROUT outlier removal -> Shapiro-Wilk -> ANOVA/GLM -> Tukey/Dunn's.
#' Returns list(summary, posthoc, outliers_removed, data_clean).
analyze_outcome <- function(outcome_name, block = 1, xlsx_path, alpha = ALPHA, outlier_Q = 0.01, outlier_min_remaining = 3) {
  cd <- get_clean_data(outcome_name, block, xlsx_path, Q = outlier_Q, min_remaining = outlier_min_remaining)
  d <- cd$cleaned
  notes <- character(0)
  if (cd$n_removed > 0) {
    notes <- c(notes, sprintf("%d outlier(s) removed via ROUT method (Q=%.0f%% FDR, per Sex x HU x IR cell); see Outliers_Removed.",
                               cd$n_removed, outlier_Q * 100))
  }

  sh <- run_shapiro(d)
  normal <- isTRUE(!is.na(sh$p) && sh$p > alpha)
  if (is.na(sh$p)) notes <- c(notes, sh$note, "Defaulted to nonparametric branch due to inconclusive Shapiro-Wilk test")

  res <- list(
    Outcome = outcome_name, Block = block,
    N_original = nrow(cd$raw), N_removed = cd$n_removed, N_total = nrow(d),
    Outlier_Q = outlier_Q,
    Shapiro_W = sh$W, Shapiro_p = sh$p, Shapiro_basis = sh$basis, Normal = normal,
    Branch = if (normal) "Parametric (ANOVA)" else "Nonparametric (GLM)",
    GLM_family = NA_character_
  )

  add_terms <- function(res, at, prefix) {
    for (term in c("IR", "HU", "IR:HU")) {
      res[[paste0(prefix, gsub(":", "x", term), "_p")]] <- term_p(at, term)
    }
    res
  }
  add_terms3 <- function(res, at) {
    for (term in c("IR", "HU", "Sex", "IR:HU", "IR:Sex", "HU:Sex", "IR:HU:Sex")) {
      res[[paste0("3way_", gsub(":", "x", term), "_p")]] <- term_p(at, term)
    }
    res
  }

  # ---- Fit models (branch-dependent) and record omnibus p-values ----------
  # Within each sex: 2-way IR*HU model. Across sexes: 3-way IR*HU*Sex model.
  # fit_one dispatches lm (parametric) / GLM (positive nonparametric) / ART
  # (zero- or negative-containing nonparametric).
  two <- list()
  for (s in levels(d$Sex)) {
    sub <- droplevels(d[d$Sex == s & !is.na(d$Value), ])
    if (nlevels(sub$IR) < 2 || nlevels(sub$HU) < 2) {
      two[[s]] <- list(fit = NULL, data = sub, note = "Insufficient design/data for within-sex model")
      next
    }
    f <- fit_one(Value ~ IR * HU, sub, normal)
    two[[s]] <- list(fit = f, data = sub,
                      note = if (is.null(f)) "within-sex model fit failed" else NA_character_)
  }
  three_sub <- droplevels(d[!is.na(d$Value), ])
  three_fit <- if (nlevels(three_sub$IR) < 2 || nlevels(three_sub$HU) < 2 || nlevels(three_sub$Sex) < 2) NULL
               else fit_one(Value ~ IR * HU * Sex, three_sub, normal)
  three <- list(fit = three_fit, data = three_sub,
                 family = if (!is.null(three_fit)) three_fit$family else NA_character_,
                 note = if (is.null(three_fit)) "Insufficient design or 3-way model fit failed" else NA_character_)

  for (s in names(two)) {
    if (!is.null(two[[s]]$fit) && !is.null(two[[s]]$fit$anova)) {
      res <- add_terms(res, two[[s]]$fit$anova, paste0("2way_", s, "_"))
    } else {
      notes <- c(notes, paste0("Within-sex model (", s, "): ", two[[s]]$note))
    }
  }
  if (!is.null(three$fit) && !is.null(three$fit$anova)) {
    res <- add_terms3(res, three$fit$anova)
    if (!normal) res$GLM_family <- three$family
  } else {
    notes <- c(notes, paste0("3-way model: ", three$note))
    if (!normal && !is.na(three$family)) res$GLM_family <- three$family
  }

  # ---- Post-hoc: family-structured simple main effects --------------------
  # (IR within HU, HU within IR, vs-Sham control, Female-vs-Male) each
  # Holm-adjusted within the family. See build_posthoc().
  posthoc_df <- build_posthoc(outcome_name, block, two, three, alpha)

  res$PostHoc_Performed <- !is.null(posthoc_df)

  # Female-vs-Male per-group table (drives the plot "#" markers and the Excel
  # sheet), with p-values taken from the unified post-hoc so they share the
  # outcome-wide BH correction.
  sexdiff <- build_sex_diff_table(d, posthoc_df)
  res$Sex_Diff_Sig_Groups <- paste(sexdiff$Group[!is.na(sexdiff$Hashes)], collapse = "; ")
  if (!nzchar(res$Sex_Diff_Sig_Groups)) res$Sex_Diff_Sig_Groups <- NA_character_

  res$Notes <- if (length(notes)) paste(unique(notes), collapse = "; ") else NA_character_

  # A fit that fails entirely (e.g. non-convergent GLM) leaves its p-value
  # columns absent rather than NA -- fill them in so every summary row has the
  # same shape and downstream code can rely on NA rather than NULL.
  expected_cols <- c(
    paste0("2way_", rep(c("Females", "Males"), each = 3), "_", rep(c("IR", "HU", "IRxHU"), 2), "_p"),
    paste0("3way_", gsub(":", "x", c("IR", "HU", "Sex", "IR:HU", "IR:Sex", "HU:Sex", "IR:HU:Sex")), "_p")
  )
  for (col in expected_cols) if (is.null(res[[col]])) res[[col]] <- NA_real_

  list(summary = as.data.frame(res, stringsAsFactors = FALSE), posthoc = posthoc_df,
       data_clean = d, outliers_removed = cd$removed, sex_diff = sexdiff)
}

# ---- Prism-style plotting --------------------------------------------------
#
# Reproduces the visual style of the original Prism exports (Figures/prism
# plots/*.pdf): 6 bars per Sex panel (Sham / 50cGy / 100cGy / HU / 50cGy +HU /
# 100cGy +HU), mean + SEM, individual points jittered on top (open triangles
# for Females, open circles for Males), significance brackets drawn from the
# within-sex IRxHU post-hoc comparisons (Tukey HSD or Dunn's test, whichever
# branch the outcome used).

GROUP_LEVELS <- c("Sham", "50cGy", "100cGy", "HU", "50cGy +HU", "100cGy +HU")

GROUP_COLORS <- c(
  "Sham"        = "#A6A6A6",
  "50cGy"       = "#F4A9A0",
  "100cGy"      = "#EE1D25",
  "HU"          = "#7EC8E3",
  "50cGy +HU"   = "#A6A6E8",
  "100cGy +HU"  = "#1E3FCB"
)

ir_hu_to_group <- function(ir, hu) {
  ir <- as.character(ir); hu <- as.character(hu)
  dplyr::case_when(
    hu == "Sham" & ir == "0"   ~ "Sham",
    hu == "Sham" & ir == "50"  ~ "50cGy",
    hu == "Sham" & ir == "100" ~ "100cGy",
    hu == "HU"   & ir == "0"   ~ "HU",
    hu == "HU"   & ir == "50"  ~ "50cGy +HU",
    hu == "HU"   & ir == "100" ~ "100cGy +HU",
    TRUE ~ NA_character_
  )
}

add_group_col <- function(d) {
  d$Group <- ir_hu_to_group(d$IR, d$HU)
  d$Group <- factor(d$Group, levels = GROUP_LEVELS)
  d
}

#' Convert a p-value to standard significance stars.
p_to_stars <- function(p) {
  if (is.null(p) || is.na(p)) return(NA_character_)
  if (p < 0.0001) "****"
  else if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
}

#' Same significance scale, rendered with "#" -- used for Female vs Male
#' differences so they read distinctly from the within-sex "*" comparisons.
p_to_hashes <- function(p) {
  if (is.null(p) || is.na(p)) return(NA_character_)
  if (p < 0.0001) "####"
  else if (p < 0.001) "###"
  else if (p < 0.01) "##"
  else if (p < 0.05) "#"
  else NA_character_
}

# ---- Sex differences within each treatment group ---------------------------
#
# For each of the 6 IR x HU treatment groups, Females are compared with Males.
# The comparison is a simple-effects contrast taken from the outcome's already
# fitted 3-way model (ANOVA on the parametric branch, GLM on the nonparametric
# branch) via emmeans -- i.e. it pools the error term across the design, which
# is what Prism's post-ANOVA multiple comparisons do, and is far better powered
# than testing 4-5 mice per cell in isolation. Sidak-adjusted across the 6
# treatment groups.
#
# If the 3-way model is unavailable (fit failure), it falls back to independent
# per-group tests: Welch t-test (parametric) or Mann-Whitney (nonparametric).
#
# Significant results are drawn as # / ## / ### / #### above the MALE bar of
# that treatment group only, matching the convention in the Prism figures.

#' Test Females vs Males within each treatment group.
#' `model3` is the fitted 3-way model (or NULL to force the fallback).
#' Returns a data.frame with one row per treatment group.
sex_difference_tests <- function(d, normal, model3 = NULL) {
  d <- add_group_col(d)
  d <- d[!is.na(d$Value) & !is.na(d$Group), ]

  desc <- d %>%
    dplyr::group_by(Group, Sex) %>%
    dplyr::summarise(n = dplyr::n(), mean = mean(Value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Sex, values_from = c(n, mean))
  for (cc in c("n_Females", "n_Males", "mean_Females", "mean_Males")) {
    if (!cc %in% names(desc)) desc[[cc]] <- NA
  }
  desc <- desc %>%
    dplyr::transmute(Group = as.character(Group),
                      n_Females = n_Females, n_Males = n_Males,
                      Mean_Females = mean_Females, Mean_Males = mean_Males)

  res <- NULL
  if (!is.null(model3)) {
    res <- tryCatch({
      em <- emmeans::emmeans(model3, ~ Sex | IR * HU)
      pw <- as.data.frame(summary(pairs(em, adjust = "none")))
      pw$Group <- ir_hu_to_group(pw$IR, pw$HU)
      stat_col <- intersect(c("t.ratio", "z.ratio"), names(pw))[1]
      data.frame(Group = pw$Group,
                  Test = if (normal) "emmeans contrast (3-way ANOVA)" else "emmeans contrast (3-way GLM)",
                  Statistic = if (!is.na(stat_col)) pw[[stat_col]] else NA_real_,
                  p_raw = pw$p.value, stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }

  if (is.null(res)) {
    rows <- lapply(GROUP_LEVELS, function(g) {
      f <- d$Value[d$Group == g & d$Sex == "Females"]
      m <- d$Value[d$Group == g & d$Sex == "Males"]
      base <- data.frame(Group = g, Test = NA_character_, Statistic = NA_real_,
                          p_raw = NA_real_, stringsAsFactors = FALSE)
      if (length(f) < 2 || length(m) < 2) return(base)
      if (normal) {
        tt <- tryCatch(stats::t.test(f, m), error = function(e) NULL)
        if (!is.null(tt)) { base$Test <- "Welch t-test"; base$Statistic <- unname(tt$statistic); base$p_raw <- tt$p.value }
      } else {
        wt <- tryCatch(suppressWarnings(stats::wilcox.test(f, m, exact = FALSE)), error = function(e) NULL)
        if (!is.null(wt)) { base$Test <- "Mann-Whitney"; base$Statistic <- unname(wt$statistic); base$p_raw <- wt$p.value }
      }
      base
    })
    res <- dplyr::bind_rows(rows)
  }

  out <- dplyr::left_join(desc, res, by = "Group")
  out$p_adj <- NA_real_
  ok <- !is.na(out$p_raw)
  # Sidak across the 6 treatment-group comparisons.
  if (any(ok)) out$p_adj[ok] <- pmin(1, 1 - (1 - out$p_raw[ok])^sum(ok))
  out$Hashes <- vapply(out$p_adj, p_to_hashes, character(1))
  out$Group <- factor(out$Group, levels = GROUP_LEVELS)
  out[order(out$Group), , drop = FALSE]
}

#' Compact "significant effects" subtitle from the 3-way omnibus tests.
#' Lists each significant IR / HU / Sex main effect and interaction with stars,
#' so main and interaction effects that do not map to a single pair of bars
#' (e.g. a main effect of Sex) are still visible on the figure.
effects_subtitle <- function(summary_row, alpha = ALPHA) {
  # Column names carry an "X" prefix once the res list becomes a data.frame
  # (names starting with a digit are not syntactic), so accept either form.
  terms <- c(IR = "3way_IR_p", HU = "3way_HU_p", Sex = "3way_Sex_p",
             "IR×HU" = "3way_IRxHU_p", "IR×Sex" = "3way_IRxSex_p",
             "HU×Sex" = "3way_HUxSex_p", "IR×HU×Sex" = "3way_IRxHUxSex_p")
  getp <- function(nm) {
    v <- summary_row[[nm]]; if (is.null(v)) v <- summary_row[[paste0("X", nm)]]
    v
  }
  parts <- character(0)
  for (i in seq_along(terms)) {
    p <- getp(terms[i])
    if (length(p) == 1 && !is.na(p) && p < alpha) {
      st <- p_to_stars(p); if (is.na(st)) st <- ""
      parts <- c(parts, paste0(names(terms)[i], " ", st))
    }
  }
  if (length(parts) == 0) return("3-way ANOVA/GLM: no significant main or interaction effects")
  paste0("Significant effects (3-way): ", paste(parts, collapse = "   "))
}

#' Build a data.frame of significance brackets for one Sex panel from the
#' unified post-hoc table (Panel / Group1 / Group2 / p_adj). Draws every
#' within-sex IR-within-HU and HU-within-IR comparison that survives the
#' outcome-wide correction, stacked by span so wider brackets sit higher.
build_brackets <- function(posthoc_df, sex_name, y_top, step, alpha = ALPHA) {
  if (is.null(posthoc_df) || nrow(posthoc_df) == 0) return(NULL)
  if (!all(c("Panel", "Group1", "Group2", "p_adj") %in% names(posthoc_df))) return(NULL)
  sub <- posthoc_df[posthoc_df$Panel == sex_name &
                       !is.na(posthoc_df$p_adj) & posthoc_df$p_adj < alpha, , drop = FALSE]
  if (nrow(sub) == 0) return(NULL)
  x1 <- match(sub$Group1, GROUP_LEVELS)
  x2 <- match(sub$Group2, GROUP_LEVELS)
  keep <- !is.na(x1) & !is.na(x2) & x1 != x2
  if (!any(keep)) return(NULL)
  d <- data.frame(x1 = pmin(x1[keep], x2[keep]), x2 = pmax(x1[keep], x2[keep]),
                   p = sub$p_adj[keep], stringsAsFactors = FALSE)
  # A group pair can be significant in more than one family (e.g. Sham-vs-HU is
  # in both "HU within IR" and "vs Sham"); draw each pair once, at its smallest
  # adjusted p.
  d <- d[order(d$x1, d$x2, d$p), ]
  d <- d[!duplicated(d[c("x1", "x2")]), , drop = FALSE]
  d <- d[order(abs(d$x2 - d$x1), d$x1), , drop = FALSE]
  data.frame(
    Sex = sex_name,
    x1 = d$x1, x2 = d$x2,
    y = y_top + step * seq_len(nrow(d)),
    stars = vapply(d$p, p_to_stars, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Build the Prism-style bar plot for one outcome.
#' `d`        outcome data (outlier-cleaned).
#' `posthoc`  res$posthoc (unified emmeans contrasts) or NULL.
#' `sex_diff` res$sex_diff (Female-vs-Male per group) or NULL.
#' `summary_row` res$summary one-row data.frame, used for the effects subtitle.
make_prism_plot <- function(outcome_name, d, posthoc, y_label = NULL, sex_diff = NULL,
                             summary_row = NULL, base_size = 18) {
  if (is.null(y_label)) y_label <- prism_y_label(outcome_name)

  d <- add_group_col(d)
  d <- d[!is.na(d$Value) & !is.na(d$Group), ]

  stats <- d %>%
    dplyr::group_by(Sex, Group) %>%
    dplyr::summarise(n = dplyr::n(), mean = mean(Value), sd = sd(Value),
                      sem = ifelse(n > 1, sd / sqrt(n), 0), .groups = "drop")

  y_top <- max(c(stats$mean + stats$sem, d$Value), na.rm = TRUE)
  y_range <- y_top - min(0, min(d$Value, na.rm = TRUE))
  step <- y_range * 0.09
  tick <- y_range * 0.02

  brackets <- dplyr::bind_rows(
    build_brackets(posthoc, "Females", y_top, step),
    build_brackets(posthoc, "Males", y_top, step)
  )
  subtitle <- if (!is.null(summary_row)) effects_subtitle(summary_row) else NULL

  # Female-vs-Male markers ("#"), drawn only on the Males panel, above that
  # treatment group's bar, matching the convention in the original Prism figures.
  hash_df <- NULL
  if (!is.null(sex_diff) && nrow(sex_diff) > 0) {
    sd_sig <- sex_diff[!is.na(sex_diff$Hashes), , drop = FALSE]
    if (nrow(sd_sig) > 0) {
      male_top <- stats %>%
        dplyr::filter(Sex == "Males") %>%
        dplyr::mutate(top = mean + sem) %>%
        dplyr::select(Group, top)
      pt_top <- d %>%
        dplyr::filter(Sex == "Males") %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(pmax = max(Value), .groups = "drop")
      hash_df <- sd_sig %>%
        dplyr::left_join(male_top, by = "Group") %>%
        dplyr::left_join(pt_top, by = "Group") %>%
        dplyr::mutate(Sex = factor("Males", levels = levels(d$Sex)),
                       y = pmax(top, pmax, na.rm = TRUE) + y_range * 0.04)
    }
  }

  sex_labels <- c(Females = "Females", Males = "Males")

  p <- ggplot() +
    geom_col(data = stats, aes(x = Group, y = mean, fill = Group),
             width = 0.7, color = "black", linewidth = 0.4) +
    geom_errorbar(data = stats, aes(x = Group, ymin = mean, ymax = mean + sem),
                   width = 0.25, linewidth = 0.5) +
    geom_jitter(data = d, aes(x = Group, y = Value, shape = Sex),
                width = 0.12, size = 2.3, stroke = 0.8, color = "black") +
    scale_fill_manual(values = GROUP_COLORS, drop = FALSE) +
    scale_shape_manual(values = c(Females = 2, Males = 1)) +
    scale_x_discrete(drop = FALSE) +
    facet_wrap(~Sex, scales = "free_x", labeller = as_labeller(sex_labels)) +
    labs(x = NULL, y = y_label, title = outcome_name, subtitle = subtitle) +
    theme_classic(base_size = base_size, base_family = PLOT_FONT) +
    theme(
      legend.position = "none",
      text = element_text(family = PLOT_FONT),
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85), colour = "black"),
      axis.text.y = element_text(size = rel(0.9), colour = "black"),
      axis.title.y = element_text(size = rel(1.0), face = "bold", margin = margin(r = 8)),
      axis.line = element_line(linewidth = 0.6, colour = "black"),
      axis.ticks = element_line(linewidth = 0.6, colour = "black"),
      plot.subtitle = element_text(hjust = 0.5, size = rel(0.8)),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.05), face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.1)),
      plot.margin = margin(10, 12, 6, 10)
    )

  if (!is.null(brackets) && nrow(brackets) > 0) {
    p <- p +
      geom_segment(data = brackets, aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = FALSE, linewidth = 0.5) +
      geom_segment(data = brackets, aes(x = x1, xend = x1, y = y, yend = y - tick), inherit.aes = FALSE, linewidth = 0.5) +
      geom_segment(data = brackets, aes(x = x2, xend = x2, y = y, yend = y - tick), inherit.aes = FALSE, linewidth = 0.5) +
      geom_text(data = brackets, aes(x = (x1 + x2) / 2, y = y, label = stars),
                vjust = -0.25, size = base_size * 0.28, family = PLOT_FONT, inherit.aes = FALSE) +
      expand_limits(y = max(brackets$y) * 1.10)
  }

  if (!is.null(hash_df) && nrow(hash_df) > 0) {
    p <- p +
      geom_text(data = hash_df, aes(x = Group, y = y, label = Hashes),
                vjust = 0, size = base_size * 0.28, fontface = "bold",
                family = PLOT_FONT, inherit.aes = FALSE) +
      expand_limits(y = max(hash_df$y, na.rm = TRUE) * 1.10)
  }

  p
}

# ---- Decision-tree summary table ------------------------------------------

#' Build a compact 3-row decision-tree table (Normality -> Test -> Post-hoc)
#' from analyze_outcome()'s `res$summary` row.
make_decision_tree_table <- function(res) {
  s <- res$summary
  normal <- isTRUE(s$Normal)

  fmt_p <- function(x) {
    if (length(x) == 0 || is.na(x)) return("NA")
    paste0(sprintf("%.4f", x), ifelse(x < ALPHA, " *", ""))
  }

  q_pct <- if (is.na(s$Outlier_Q)) "1" else sprintf("%.0f", s$Outlier_Q * 100)
  step0 <- if (isTRUE(s$N_removed > 0)) {
    sprintf("ROUT method (Q=%s%% FDR, per Sex x HU x IR cell): %d of %d point(s) flagged and removed as outliers -> N = %d used below (see table above / Outliers_Removed).",
            q_pct, s$N_removed, s$N_original, s$N_total)
  } else {
    sprintf("ROUT method (Q=%s%% FDR, per Sex x HU x IR cell): no outliers detected -> N = %d used below.", q_pct, s$N_total)
  }

  step1 <- sprintf(
    "Shapiro-Wilk on %s: W = %s, p = %s -> %s",
    if (is.null(s$Shapiro_basis) || is.na(s$Shapiro_basis)) "3-way model residuals" else s$Shapiro_basis,
    ifelse(is.na(s$Shapiro_W), "NA", sprintf("%.4f", s$Shapiro_W)),
    ifelse(is.na(s$Shapiro_p), "NA", sprintf("%.5f", s$Shapiro_p)),
    if (normal) "NORMAL (p > 0.05) -> parametric branch" else "NOT normal (p <= 0.05) -> nonparametric branch"
  )

  test_name <- if (normal) {
    "Type III ANOVA"
  } else if (identical(s$GLM_family, "ART (aligned rank transform)")) {
    "Aligned Rank Transform (ART), Type III"
  } else {
    paste0("Type III GLM (", ifelse(is.na(s$GLM_family), "NA", s$GLM_family), ")")
  }
  step2 <- paste0(
    test_name, ".  ",
    "Within-Sex 2-way (IRxHU) -- Females: IR ", fmt_p(s$X2way_Females_IR_p),
    ", HU ", fmt_p(s$X2way_Females_HU_p), ", IRxHU ", fmt_p(s$X2way_Females_IRxHU_p),
    " | Males: IR ", fmt_p(s$X2way_Males_IR_p), ", HU ", fmt_p(s$X2way_Males_HU_p),
    ", IRxHU ", fmt_p(s$X2way_Males_IRxHU_p),
    ".  3-way -- IR ", fmt_p(s$X3way_IR_p), ", HU ", fmt_p(s$X3way_HU_p), ", Sex ", fmt_p(s$X3way_Sex_p),
    ", IRxHU ", fmt_p(s$X3way_IRxHU_p), ", IRxSex ", fmt_p(s$X3way_IRxSex_p),
    ", HUxSex ", fmt_p(s$X3way_HUxSex_p), ", IRxHUxSex ", fmt_p(s$X3way_IRxHUxSex_p)
  )

  step3 <- if (isTRUE(s$PostHoc_Performed)) {
    engine <- if (normal) "estimated marginal means (emmeans) on the ANOVA"
              else if (identical(s$GLM_family, "ART (aligned rank transform)")) "ART contrasts (ARTool::art.con)"
              else "estimated marginal means (emmeans) on the GLM"
    paste0("Simple main-effects decomposition via ", engine, ", each family Holm-adjusted within itself. ",
           "Families (run within each sex when that sex's IR/HU/IR:HU term is significant): IR within HU ",
           "(dose response with and without HU), HU within IR (Sham vs HU at each dose), and each ",
           "treatment/combined group vs the Sham control. Female-vs-Male within each group runs when a Sex ",
           "main effect or Sex interaction is significant. See the Family column in PostHoc_Comparisons / below.")
  } else {
    "Not performed -- no IR/HU/Sex effect or interaction that gates a comparison family reached p < 0.05."
  }

  data.frame(
    Step = c("1. Outlier detection (ROUT)", "2. Normality (Shapiro-Wilk)", "3. Statistical test", "4. Multiple comparisons"),
    Result = c(step0, step1, step2, step3),
    stringsAsFactors = FALSE
  )
}

#' Word-wrap a string to a fixed character width, joined with "\n".
wrap_text <- function(x, width = 105) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
}

#' Build a gridExtra tableGrob of the decision-tree table, word-wrapped for
#' printing under a plot.
decision_tree_grob <- function(dt, width = 100, step_col_frac = 0.16) {
  dt$Result <- wrap_text(dt$Result, width = width)
  tt <- gridExtra::ttheme_minimal(
    core = list(fg_params = list(hjust = 0, x = 0.01, fontsize = 8, fontfamily = "sans"),
                bg_params = list(fill = c("#F2F2F2", "#FFFFFF"), col = NA)),
    colhead = list(fg_params = list(hjust = 0, x = 0.01, fontsize = 9, fontface = "bold", fontfamily = "sans"),
                   bg_params = list(fill = "#D9E1F2", col = NA)),
    padding = unit(c(4, 3), "mm")
  )
  gridExtra::tableGrob(dt, rows = NULL, theme = tt,
                        widths = unit(c(step_col_frac, 1 - step_col_frac), "npc"))
}

#' Render one outcome's Prism-style plot + decision-tree table as a single
#' one-page PDF at `out_path`.
render_outcome_pdf <- function(outcome_name, d, res, out_path, y_label = NULL,
                                width = 9, height = 9.5) {
  p <- make_prism_plot(outcome_name, d, res$posthoc, y_label = y_label,
                        sex_diff = res$sex_diff, summary_row = res$summary)
  dt <- make_decision_tree_table(res)
  tg <- decision_tree_grob(dt)
  title_grob <- grid::textGrob("Decision tree: outliers -> normality -> statistical test -> multiple comparisons",
                                x = 0, hjust = 0,
                                gp = grid::gpar(fontsize = 10, fontface = "bold", fontfamily = PLOT_FONT))
  key_grob <- grid::textGrob(
    "Subtitle = significant 3-way effects.    * bracket = significant within-sex simple-effect comparison (IR within HU, Sham vs HU within a dose, or group vs Sham control).    # = Female vs Male within that group.    *p<0.05 **p<0.01 ***p<0.001 ****p<0.0001 (Holm-adjusted within each comparison family).",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8, fontfamily = PLOT_FONT))

  grDevices::pdf(out_path, width = width, height = height)
  gridExtra::grid.arrange(
    p, key_grob, title_grob, tg,
    ncol = 1,
    heights = grid::unit.c(unit(1, "null"), unit(1.1, "lines"), unit(1.2, "lines"),
                            sum(tg$heights) + unit(4, "mm"))
  )
  grDevices::dev.off()
  invisible(out_path)
}

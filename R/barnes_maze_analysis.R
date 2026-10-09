# Barnes Maze heatmap statistics: Memory Approach (ordinal 1-4) and Latency to
# Entry (continuous), analysed WITHIN each trial (each of trials 1-8 is an
# independent cross-section of animals).
#
# Three comparison families, each drawn on the Trial x Group heatmap with a
# distinct symbol and each Holm-adjusted within the family:
#   *  vs Sham  -- each treatment/combined group vs the Sham control, within a
#                  sex and trial (family = the 5 comparisons in that Sex x Trial).
#   ★  dose     -- 50cGy vs 100cGy, and HU+50cGy vs HU+100cGy, within a sex and
#                  trial (family = those 2 comparisons in that Sex x Trial);
#                  the symbol is drawn on the higher-dose cell.
#   #  sex      -- Female vs Male for the same group and trial (family = the 6
#                  groups within that trial); drawn on the Male-panel cell.
#
# Test choice:
#   Memory Approach -- ordinal 1-4 -> Mann-Whitney (Wilcoxon rank-sum) throughout.
#   Latency         -- Shapiro-Wilk on the residuals of Value ~ Trial*Group*Sex;
#                      normal -> Welch t-test, else Mann-Whitney (matches the
#                      decision tree used elsewhere in the paper).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(showtext); library(sysfonts); library(openxlsx)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))

ARIAL   <- "/System/Library/Fonts/Supplemental/Arial.ttf"
ARIAL_U <- "/Library/Fonts/Arial Unicode.ttf"
if (!file.exists(ARIAL_U)) ARIAL_U <- "/System/Library/Fonts/Supplemental/Arial Unicode.ttf"
if (!("Arial" %in% sysfonts::font_families())) sysfonts::font_add("Arial", regular = ARIAL,
    bold = "/System/Library/Fonts/Supplemental/Arial Bold.ttf")
if (!("ArialU" %in% sysfonts::font_families())) sysfonts::font_add("ArialU", regular = ARIAL_U)
showtext_auto(); showtext_opts(dpi = 300)

GROUPS <- c("Sham", "50cGy", "100cGy", "HU", "HU+50cGy", "HU+100cGy")
ALPHA  <- 0.05

d <- read.csv("barnes_data_long.csv", stringsAsFactors = FALSE, check.names = FALSE)
d$Group <- factor(d$Group, levels = GROUPS)
d$Sex   <- factor(d$Sex, levels = c("Females", "Males"))

# ---- symbol scaling -------------------------------------------------------
sym_n <- function(p_adj) {
  if (is.na(p_adj)) return(0L)
  if (p_adj < 1e-4) 4L else if (p_adj < 1e-3) 3L else if (p_adj < 1e-2) 2L else if (p_adj < 0.05) 1L else 0L
}
reps <- function(ch, n) if (n > 0) strrep(ch, n) else ""

# ---- test branch per outcome ---------------------------------------------
branch_for <- function(sub) {
  # Memory Approach is ordinal -> always nonparametric.
  if (unique(sub$Outcome)[1] == "Memory Approach") return(list(normal = FALSE, basis = "ordinal score -> nonparametric"))
  s <- sub[!is.na(sub$Value), ]
  r <- tryCatch(residuals(lm(Value ~ factor(Trial) * Group * Sex, data = s)), error = function(e) NULL)
  if (is.null(r) || length(unique(round(r, 10))) < 3) return(list(normal = FALSE, basis = "residuals unavailable -> nonparametric"))
  p <- tryCatch(shapiro.test(r)$p.value, error = function(e) NA)
  list(normal = isTRUE(!is.na(p) && p > ALPHA),
       basis = sprintf("Shapiro-Wilk on Trial*Group*Sex residuals p=%.4g -> %s", p, if (isTRUE(p > ALPHA)) "parametric (Welch t)" else "nonparametric (Mann-Whitney)"))
}

# pairwise test between two numeric vectors
pair_test <- function(x, y, normal) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(list(stat = NA_real_, p = NA_real_, test = NA_character_))
  if (normal) {
    tt <- tryCatch(t.test(x, y), error = function(e) NULL)
    if (is.null(tt)) return(list(stat = NA_real_, p = NA_real_, test = "Welch t-test (failed)"))
    list(stat = unname(tt$statistic), p = tt$p.value, test = "Welch t-test")
  } else {
    wt <- tryCatch(suppressWarnings(wilcox.test(x, y, exact = FALSE)), error = function(e) NULL)
    if (is.null(wt)) return(list(stat = NA_real_, p = NA_real_, test = "Mann-Whitney (failed)"))
    list(stat = unname(wt$statistic), p = wt$p.value, test = "Mann-Whitney")
  }
}

vals <- function(sub, sx, g, tr) sub$Value[sub$Sex == sx & sub$Group == g & sub$Trial == tr]

# ---- run all comparisons --------------------------------------------------
all_rows <- list()
branches <- list()

for (oc in unique(d$Outcome)) {
  sub <- d[d$Outcome == oc, ]
  br <- branch_for(sub); branches[[oc]] <- br$basis
  normal <- br$normal

  # vs Sham (per Sex, Trial): 5 comparisons -> Holm within family
  for (sx in levels(sub$Sex)) for (tr in sort(unique(sub$Trial))) {
    fam <- list()
    for (g in setdiff(GROUPS, "Sham")) {
      r <- pair_test(vals(sub, sx, g, tr), vals(sub, sx, "Sham", tr), normal)
      fam[[g]] <- data.frame(Outcome = oc, Family = "vs Sham", Symbol = "*", Sex = sx, Trial = tr,
                              Group = g, Ref = "Sham", Test = r$test, Statistic = r$stat, p_raw = r$p,
                              stringsAsFactors = FALSE)
    }
    fam <- dplyr::bind_rows(fam); fam$p_adj <- p.adjust(fam$p_raw, "holm")
    all_rows[[length(all_rows) + 1]] <- fam
  }

  # dose (per Sex, Trial): 50 vs 100, HU+50 vs HU+100 -> Holm within family
  dose_pairs <- list(c("100cGy", "50cGy"), c("HU+100cGy", "HU+50cGy"))
  for (sx in levels(sub$Sex)) for (tr in sort(unique(sub$Trial))) {
    fam <- list()
    for (pr in dose_pairs) {
      r <- pair_test(vals(sub, sx, pr[1], tr), vals(sub, sx, pr[2], tr), normal)
      fam[[pr[1]]] <- data.frame(Outcome = oc, Family = "dose", Symbol = "star", Sex = sx, Trial = tr,
                                 Group = pr[1], Ref = pr[2], Test = r$test, Statistic = r$stat, p_raw = r$p,
                                 stringsAsFactors = FALSE)
    }
    fam <- dplyr::bind_rows(fam); fam$p_adj <- p.adjust(fam$p_raw, "holm")
    all_rows[[length(all_rows) + 1]] <- fam
  }

  # sex (per Trial): Female vs Male for each of 6 groups -> Holm within family
  for (tr in sort(unique(sub$Trial))) {
    fam <- list()
    for (g in GROUPS) {
      r <- pair_test(vals(sub, "Females", g, tr), vals(sub, "Males", g, tr), normal)
      fam[[g]] <- data.frame(Outcome = oc, Family = "sex (F vs M)", Symbol = "#", Sex = "Males", Trial = tr,
                             Group = g, Ref = "Females vs Males", Test = r$test, Statistic = r$stat, p_raw = r$p,
                             stringsAsFactors = FALSE)
    }
    fam <- dplyr::bind_rows(fam); fam$p_adj <- p.adjust(fam$p_raw, "holm")
    all_rows[[length(all_rows) + 1]] <- fam
  }
}

res <- dplyr::bind_rows(all_rows)
res$n_sym <- vapply(res$p_adj, sym_n, integer(1))
res$Significance <- mapply(function(sym, n) {
  ch <- switch(sym, "*" = "*", "star" = "★", "#" = "#")
  if (n > 0) strrep(ch, n) else "ns"
}, res$Symbol, res$n_sym)
res$Significant_0.05 <- ifelse(!is.na(res$p_adj) & res$p_adj < ALPHA, "Yes", "No")

dir.create("results", showWarnings = FALSE)
saveRDS(res, "results/barnes_results.rds")
write.csv(res[, c("Outcome","Family","Sex","Trial","Group","Ref","Test","Statistic","p_raw","p_adj","Significance","Significant_0.05")],
          "results/barnes_results.csv", row.names = FALSE)

cat("Comparisons:", nrow(res), " significant:", sum(res$Significant_0.05 == "Yes"), "\n")
for (oc in names(branches)) cat("  ", oc, "->", branches[[oc]], "\n")

# ---- per-cell symbol labels for the heatmap -------------------------------
# Each cell (Sex, Group, Trial) collects: * (vs Sham, if this group differs
# from Sham), ★ (dose, if this is the higher-dose cell and it differs from its
# lower-dose partner), and # (sex, on the Male panel if F vs M differ).
cell_label <- function(oc, sx, g, tr) {
  star <- ""; dagger <- ""; hash <- ""
  vs <- res[res$Outcome==oc & res$Family=="vs Sham" & res$Sex==sx & res$Group==g & res$Trial==tr, ]
  if (nrow(vs)) star <- reps("*", vs$n_sym[1])
  dz <- res[res$Outcome==oc & res$Family=="dose" & res$Sex==sx & res$Group==g & res$Trial==tr, ]
  if (nrow(dz)) dagger <- reps("★", dz$n_sym[1])
  if (sx == "Males") {
    sxr <- res[res$Outcome==oc & res$Family=="sex (F vs M)" & res$Group==g & res$Trial==tr, ]
    if (nrow(sxr)) hash <- reps("#", sxr$n_sym[1])
  }
  paste0(star, dagger, hash)
}

# ---- heatmaps -------------------------------------------------------------
sex_labs <- c(Females = "Females", Males = "Males")

make_heatmap <- function(oc, fill_label, palette_opt, breaks = waiver(), br_labels = waiver(), limits = NULL) {
  sub <- d[d$Outcome == oc, ]
  cell <- sub %>% group_by(Sex, Group, Trial) %>% summarise(mean = mean(Value, na.rm = TRUE), .groups = "drop")
  cell$label <- mapply(cell_label, oc, as.character(cell$Sex), as.character(cell$Group), cell$Trial)

  ggplot(cell, aes(x = Group, y = Trial, fill = mean)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = label), color = "white", family = "ArialU", size = 5.2, lineheight = 0.8) +
    facet_wrap(~Sex, labeller = as_labeller(sex_labs)) +
    scale_y_reverse(breaks = 1:8, expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    viridis::scale_fill_viridis(option = palette_opt, name = fill_label, breaks = breaks,
                                 labels = br_labels, limits = limits, direction = 1) +
    labs(x = NULL, y = "Trial", title = paste0("Barnes Maze — ", oc)) +
    theme_classic(base_size = 15, base_family = "Arial") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
      axis.text.y = element_text(colour = "black"),
      axis.title.y = element_text(face = "bold"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = rel(1.1)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.spacing = unit(1, "lines")
    )
}

dir.create("generated_plots", showWarnings = FALSE)

# Memory Approach: ordinal 1-4, magma, with strategy labels.
p1 <- make_heatmap("Memory Approach", "Memory\nApproach", "magma",
                   breaks = 1:4,
                   br_labels = c("1 Spatial", "2 Spatial/Seq.", "3 Sequential", "4 Mixed/Random"),
                   limits = c(1, 4))
ggsave("generated_plots/BarnesMaze_MemoryApproach_heatmap.pdf", p1, width = 9, height = 4.6, dpi = 300)

# Latency: continuous seconds, magma.
p2 <- make_heatmap("Latency", "Latency to\nEntry (sec)", "magma")
ggsave("generated_plots/BarnesMaze_Latency_heatmap.pdf", p2, width = 9, height = 4.6, dpi = 300)

# ---- Excel workbook -------------------------------------------------------
wb <- createWorkbook(); options("openxlsx.font" = "Arial")
hdr <- createStyle(fontName = "Arial", textDecoration = "bold", fgFill = "#D9E1F2", halign = "center", border = "Bottom")
sig <- createStyle(fontName = "Arial", fgFill = "#FCE4D6")

tab <- res %>%
  transmute(Outcome, Family, Symbol = ifelse(Symbol == "star", "★", Symbol),
            Sex, Trial, Group, Ref, Test,
            Statistic = round(Statistic, 4), p_raw = round(p_raw, 5), p_adj = round(p_adj, 5),
            Significance, Significant_0.05) %>%
  arrange(Outcome, Family, Sex, Trial, Group)

addWorksheet(wb, "Comparisons")
writeData(wb, "Comparisons", tab, headerStyle = hdr)
freezePane(wb, "Comparisons", firstRow = TRUE); setColWidths(wb, "Comparisons", 1:ncol(tab), "auto")
addFilter(wb, "Comparisons", 1, 1:ncol(tab))
conditionalFormatting(wb, "Comparisons", cols = which(names(tab) == "p_adj"), rows = 2:(nrow(tab)+1), rule = "<0.05", style = sig)

# means-per-cell reference sheet
means <- d %>% group_by(Outcome, Sex, Group, Trial) %>%
  summarise(n = dplyr::n(), mean = round(mean(Value, na.rm = TRUE), 3), sd = round(sd(Value, na.rm = TRUE), 3), .groups = "drop") %>%
  arrange(Outcome, Sex, Group, Trial)
addWorksheet(wb, "Cell_Means"); writeData(wb, "Cell_Means", means, headerStyle = hdr)
freezePane(wb, "Cell_Means", firstRow = TRUE); setColWidths(wb, "Cell_Means", 1:ncol(means), "auto")

info <- data.frame(Field = c(
  "Analysis", "Design", "Unit of analysis", "Comparison: * (vs Sham)",
  "Comparison: star (dose)", "Comparison: # (sex)", "Multiplicity",
  "Memory Approach test", "Latency test", "Symbols"),
  Detail = c(
  "Barnes Maze heatmaps analysed within each trial (each trial an independent cross-section).",
  "6 treatment groups x 2 sexes x 8 trials; one value per mouse per trial.",
  "One animal per value.",
  "Each treatment/combined group vs Sham control, within each Sex and Trial (family = 5 comparisons).",
  "50cGy vs 100cGy and HU+50cGy vs HU+100cGy, within each Sex and Trial (family = 2); drawn on the higher-dose cell.",
  "Female vs Male for the same group and trial (family = 6 groups within a Trial); drawn on the Male panel.",
  "Holm step-down within each family.",
  branches[["Memory Approach"]],
  branches[["Latency"]],
  "* p<0.05 ** p<0.01 *** p<0.001 **** p<0.0001 (same scaling for star and #), on the Holm-adjusted p-value."))
addWorksheet(wb, "Methods"); writeData(wb, "Methods", info, headerStyle = hdr)
setColWidths(wb, "Methods", 1, 34); setColWidths(wb, "Methods", 2, 110)
addStyle(wb, "Methods", createStyle(fontName = "Arial", wrapText = TRUE, valign = "top"), rows = 1:(nrow(info)+1), cols = 1:2, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, "BarnesMaze_Statistical_Results.xlsx", overwrite = TRUE)
cat("Wrote BarnesMaze_Statistical_Results.xlsx and 2 heatmap PDFs\n")

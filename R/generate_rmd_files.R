# Generates one self-contained .Rmd report per Outcome x Block analysis unit
# into rmarkdown_analyses/. Each file sources ../R/analysis_functions.R and
# runs the full Shapiro-Wilk -> (ANOVA + Tukey) / (GLM + Dunn) decision tree
# for that single outcome.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))

XLSX <- "HU_IR_Sex_LongFormat_FROM_PRISM.xlsx"
long <- suppressMessages(read_excel(XLSX, sheet = "Long_Format_Data"))

units <- long %>% distinct(Outcome, Block) %>% arrange(Outcome, Block)
block_counts <- long %>% distinct(Outcome, Block) %>% count(Outcome)

slugify <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

dir.create("rmarkdown_analyses", showWarnings = FALSE)

rmd_template <- '---
title: "Statistical Analysis: %s"
date: "%s"
output:
  html_document:
    toc: true
    toc_float: true
    df_print: kable
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = TRUE, message = FALSE, fig.width = 8, fig.height = 5)
library(dplyr)
library(ggplot2)
source("../R/analysis_functions.R")
xlsx_path  <- "../HU_IR_Sex_LongFormat_FROM_PRISM.xlsx"
outcome_name <- "%s"
block <- %d
```

## Outcome

**Outcome:** `%s`%s

## Raw data (before outlier removal)

```{r load-data}
d_raw <- load_outcome_data(outcome_name, block, xlsx_path)
knitr::kable(head(d_raw, 10), caption = "First 10 rows")
cat("Total N (raw, before outlier removal):", nrow(d_raw))
```

## Step 1: Outlier detection (ROUT method, Q = 1%% FDR)

```{r analyze, include=FALSE}
res <- analyze_outcome(outcome_name, block, xlsx_path)
d <- res$data_clean
```

Applied separately within each Sex x HU x IR cell: a robust (median-based) fit, a robust SD
estimated from the smallest 68.27%% of squared residuals, and a Benjamini-Hochberg FDR
procedure at Q = 1%% flag outliers, which are then removed before any statistics below.
Removal is capped so at least 3 points always remain in each cell; where more points
qualified, only the most extreme were removed.

```{r outliers}
if (nrow(res$outliers_removed) > 0) {
  knitr::kable(res$outliers_removed[, c("Sex","HU","IR","Value")], digits = 5,
               caption = paste0(nrow(res$outliers_removed), " point(s) removed"))
} else {
  cat("No outliers detected.")
}
cat("N after outlier removal:", nrow(d), "of", nrow(d_raw), "\\n")
```

## Descriptive summary by group (cleaned data)

```{r desc}
d %%>%%
  group_by(Sex, HU, IR) %%>%%
  summarise(n = n(), mean = mean(Value, na.rm = TRUE), sd = sd(Value, na.rm = TRUE), .groups = "drop") %%>%%
  knitr::kable(digits = 4)
```

## Bar plot (Prism-style) and decision tree

```{r plot, fig.width=9, fig.height=6}
make_prism_plot(outcome_name, d, res$posthoc, sex_diff = res$sex_diff, summary_row = res$summary)
```

**Symbols.** The subtitle lists the significant 3-way main and interaction effects. `*`
brackets are within-sex pairwise comparisons (IR doses within an HU level, or Sham vs HU
within a dose); `#` marks a **Female vs Male** difference within that treatment group, drawn
above the male bar. All pairwise p-values share one Benjamini-Hochberg correction across every
comparison for this outcome (see Step 5).

```{r sexdiff}
knitr::kable(res$sex_diff[, c("Group","n_Females","n_Males","Mean_Females","Mean_Males",
                               "p_raw","p_adj","Hashes")],
             digits = 5, caption = "Female vs Male within each treatment group (from the unified post-hoc; NA where the Sex family was not triggered)")
```

```{r decision-tree}
knitr::kable(make_decision_tree_table(res), digits = 5)
```

## Step 2: Normality (Shapiro-Wilk on 3-way model residuals)

Normality is judged on the residuals of the `IR * HU * Sex` model, not on the raw
pooled values: pooling conflates real treatment-group mean differences with
non-normality and would push outcomes with strong effects onto the nonparametric
branch for the wrong reason.

```{r shapiro}
sh <- run_shapiro(d)
data.frame(W = sh$W, p_value = sh$p, N = sh$n, Tested = sh$basis) %%>%% knitr::kable(digits = 5)
normal <- isTRUE(!is.na(sh$p) && sh$p > ALPHA)
cat(if (normal) "Residuals ARE consistent with normality (p > 0.05) -> parametric ANOVA branch." else "Residuals are NOT consistent with normality (p <= 0.05), or Shapiro-Wilk was inconclusive -> nonparametric GLM branch.")
```

## Step 3: `r if (normal) "Two-way ANOVA (IR x HU) within each Sex" else "Nonparametric GLM (IR x HU) within each Sex"`

```{r step3, results="asis"}
if (normal) {
  two <- run_2way_by_sex(d)
} else {
  two <- run_glm_2way_by_sex(d)
}
for (s in names(two)) {
  cat("\\n### ", s, "\\n")
  if (!is.null(two[[s]]$fit) && !is.null(two[[s]]$fit$anova)) {
    print(knitr::kable(as.data.frame(two[[s]]$fit$anova), digits = 5))
  } else {
    cat("Not available:", two[[s]]$note %%||%% "model could not be fit", "\\n")
  }
}
```

## Step 4: `r if (normal) "Three-way ANOVA (IR x HU x Sex)" else "Nonparametric GLM (IR x HU x Sex)"`

```{r step4}
if (normal) {
  three <- run_3way(d)
} else {
  three <- run_glm_3way(d)
  cat("GLM family used:", three$family, "\\n")
}
if (!is.null(three$fit) && !is.null(three$fit$anova)) {
  knitr::kable(as.data.frame(three$fit$anova), digits = 5)
} else {
  cat("Not available:", three$note %%||%% "model could not be fit")
}
```

## Step 5: Post-hoc comparisons (simple main effects)

Comparisons are simple main-effects contrasts on the fitted model (%s), so sexes are never
pooled. Named families, each Holm-adjusted within itself: **IR within HU** (dose response with
and without HU), **HU within IR** (Sham vs HU at each dose), and each **treatment/combined
group vs the Sham control** -- these run within each sex when that sex\'s IR/HU/IR:HU term is
significant. **Female-vs-Male** within each group runs when a Sex main effect or Sex
interaction is significant. A pair can appear in more than one family (standard simple-effects
reporting); see the `Family` column.

```{r posthoc}
if (isTRUE(res$summary$PostHoc_Performed) && !is.null(res$posthoc)) {
  knitr::kable(res$posthoc[, c("Panel","Family","Comparison","Group1","Group2",
                                "estimate","Statistic","p_raw","p_adj","Significance")], digits = 5)
} else {
  cat("No effect/interaction gated a comparison family -> post-hoc comparisons not performed.")
}
```

## Summary row

```{r summary-row}
knitr::kable(t(res$summary))
if (!is.na(res$summary$Notes)) cat("**Notes:** ", res$summary$Notes)
```
'

generated <- character(0)

for (i in seq_len(nrow(units))) {
  oc <- units$Outcome[i]
  bl <- units$Block[i]
  has_multi_block <- block_counts$n[block_counts$Outcome == oc] > 1
  label <- if (has_multi_block) paste0(oc, " [Block ", bl, "]") else oc
  block_note <- if (has_multi_block) paste0("  \n**Block:** ", bl, " (this Prism table stacks two testing blocks; each is analyzed as its own unit.)") else ""

  posthoc_desc <- "an ANOVA lm for the parametric branch, a GLM for the nonparametric branch"

  content <- sprintf(rmd_template,
                      label, format(Sys.Date(), "%Y-%m-%d"),
                      oc, bl,
                      oc, block_note,
                      posthoc_desc)

  fname <- paste0(sprintf("%02d", i), "_", slugify(label), ".Rmd")
  writeLines(content, file.path("rmarkdown_analyses", fname))
  generated <- c(generated, fname)
}

cat("Generated", length(generated), "Rmd files in rmarkdown_analyses/\n")

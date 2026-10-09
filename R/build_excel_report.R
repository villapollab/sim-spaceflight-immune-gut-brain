# Assembles the final Excel workbook from results/summary_results.rds and
# results/posthoc_results.rds: Full_Results, PostHoc_Comparisons, Summary sheets.

suppressPackageStartupMessages({
  library(dplyr)
  library(openxlsx)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))
source("R/analysis_functions.R")

summary_df <- readRDS("results/summary_results.rds")
posthoc_df <- readRDS("results/posthoc_results.rds")
outliers_df <- readRDS("results/outliers_removed.rds")
sexdiff_df <- readRDS("results/sex_differences.rds")

# ---- Full_Results sheet: clean column names, sensible order ----------------

clean_names <- function(nm) sub("^X", "", nm)
names(summary_df) <- clean_names(names(summary_df))

full_results <- summary_df %>%
  transmute(
    Outcome, Block, Analysis_Unit,
    N_Original = N_original,
    N_Removed = N_removed,
    N_Total = N_total,
    Shapiro_W = round(Shapiro_W, 4),
    Shapiro_p = round(Shapiro_p, 5),
    Shapiro_Basis = Shapiro_basis,
    Normal = ifelse(Normal, "Yes", "No"),
    Branch,
    GLM_Family = GLM_family,
    Females_2way_IR_p   = round(`2way_Females_IR_p`, 5),
    Females_2way_HU_p   = round(`2way_Females_HU_p`, 5),
    Females_2way_IRxHU_p = round(`2way_Females_IRxHU_p`, 5),
    Males_2way_IR_p     = round(`2way_Males_IR_p`, 5),
    Males_2way_HU_p     = round(`2way_Males_HU_p`, 5),
    Males_2way_IRxHU_p  = round(`2way_Males_IRxHU_p`, 5),
    ThreeWay_IR_p       = round(`3way_IR_p`, 5),
    ThreeWay_HU_p       = round(`3way_HU_p`, 5),
    ThreeWay_Sex_p      = round(`3way_Sex_p`, 5),
    ThreeWay_IRxHU_p    = round(`3way_IRxHU_p`, 5),
    ThreeWay_IRxSex_p   = round(`3way_IRxSex_p`, 5),
    ThreeWay_HUxSex_p   = round(`3way_HUxSex_p`, 5),
    ThreeWay_IRxHUxSex_p = round(`3way_IRxHUxSex_p`, 5),
    PostHoc_Performed = ifelse(PostHoc_Performed, "Yes", "No"),
    Notes
  ) %>%
  arrange(Outcome, Block)

# ---- PostHoc_Comparisons sheet (family-structured simple-effect contrasts) --

posthoc_clean <- posthoc_df %>%
  transmute(
    Outcome, Block, Analysis_Unit, Panel, Family, Comparison, Group1, Group2,
    Estimate = round(estimate, 4),
    Statistic = round(Statistic, 4),
    P_raw = round(p_raw, 5), P_adj = round(p_adj, 5),
    Significance, Significant_0.05
  ) %>%
  arrange(Outcome, Block, Family, P_adj)

# ---- Summary sheet -----------------------------------------------------

n_units <- nrow(full_results)
n_outcomes <- length(unique(full_results$Outcome))
n_normal <- sum(full_results$Normal == "Yes")
n_nonparam <- sum(full_results$Normal == "No")
n_posthoc <- sum(full_results$PostHoc_Performed == "Yes")
n_posthoc_rows <- nrow(posthoc_clean)
n_sig_posthoc <- sum(posthoc_clean$Significant_0.05 == "Yes", na.rm = TRUE)
fam_counts <- table(full_results$GLM_Family[full_results$Branch == "Nonparametric (GLM)"], useNA = "ifany")

n_sexdiff_sig <- sum(!is.na(sexdiff_df$Hashes))
n_units_sexdiff <- length(unique(sexdiff_df$Analysis_Unit[!is.na(sexdiff_df$Hashes)]))

n_outliers_total <- nrow(outliers_df)
n_points_total <- sum(full_results$N_Original, na.rm = TRUE)
n_units_with_outliers <- sum(full_results$N_Removed > 0, na.rm = TRUE)

fit_failed_mask <- !is.na(full_results$Notes) &
  grepl("GLM (2-way|3-way).*: NA|could not be fit|fit failed", full_results$Notes)
n_fit_failed_units <- sum(fit_failed_mask)
fit_failed_outcomes <- full_results$Analysis_Unit[fit_failed_mask]

summary_lines <- list(
  c("HU x IR x Sex Outcome Analysis - Summary", ""),
  c("", ""),
  c("Generated", format(Sys.Date(), "%Y-%m-%d")),
  c("Source data", "HU_IR_Sex_LongFormat_FROM_PRISM.xlsx (Long_Format_Data sheet)"),
  c("", ""),
  c("Analysis units (Outcome x Block)", n_units),
  c("Unique Outcomes", n_outcomes),
  c("  - of which split into 2 Blocks", "1 (Fig ROTAROD baseline: two stacked testing days in one Prism table, analyzed as separate units)"),
  c("", ""),
  c("METHODOLOGY", ""),
  c("Step 1", "ROUT outlier detection (Motulsky & Brown 2006) within each Sex x HU x IR cell: residuals from the cell median, a Robust SD of Residuals (RSDR = 68.27th percentile of the absolute residuals x N/(N-1), the published estimator), and Benjamini-Hochberg FDR at Q = 1% flag and remove outliers before any statistics below. Removal is capped so at least 3 points always remain per cell (if more qualify, only the most extreme are removed)."),
  c("Step 2", "Shapiro-Wilk normality test on the RESIDUALS of the 3-way IR x HU x Sex model (post-outlier-removal), NOT on the pooled raw values. Testing pooled values conflates real treatment-group mean differences with non-normality; testing residuals isolates the shape of the error term. Falls back to pooled values only if the 3-way model cannot be fit (see Shapiro_Basis column)."),
  c("Step 3 (if normal, p > 0.05)", "Type III 2-way ANOVA (IR x HU) run separately within each Sex, plus a Type III 3-way ANOVA (IR x HU x Sex)."),
  c("Step 3 (if not normal, p <= 0.05)", "Same factorial structure via a model chosen per outcome by data shape (see 'nonparametric model selection'), with Type III tests, in place of ANOVA."),
  c("Step 4 (post-hoc)", "Simple main-effects decomposition on the FITTED model -- estimated marginal means (emmeans) for the ANOVA and GLM branches, ART contrasts (ARTool::art.con) for the ART branch -- so factors are marginalised properly and sexes are never pooled. Named comparison families, each Holm-adjusted WITHIN the family (Holm 1979; applies identically across the ANOVA/GLM/ART branches): (a) IR within HU -- IR doses compared within Sham and within HU (the 'different IR doses when HU is introduced' question); (b) HU within IR -- Sham vs HU at each IR dose; (c) each treatment/combined group vs the Sham control; (a-c) run within a sex when that sex's IR/HU/IR:HU term is significant. (d) Female-vs-Male within each of the 6 groups, run when a Sex main effect or any Sex interaction is significant. A given group pair can appear in more than one family (e.g. Sham-vs-HU is in both (b) and (c)); this is standard simple-effects reporting. See the Family column."),
  c("Plot annotation", "The subtitle lists the significant 3-way main and interaction effects (IR / HU / Sex / interactions). '*' brackets show significant within-sex simple-effect comparisons (families a-c above), each pair drawn once at its smallest adjusted p. '#' above the male bar shows a significant Female-vs-Male difference (d). Stars: *p<0.05 **p<0.01 ***p<0.001 ****p<0.0001, on the Holm-adjusted p-value."),
  c("Nonparametric model selection", "Strictly positive continuous -> Gamma(log) GLM. Non-negative integers -> quasipoisson(log) GLM. ANY zero or negative value -> Aligned Rank Transform (ARTool::art; Wobbrock et al. 2011). ART is used instead of a plain rank-transformed GLM because the plain rank transform gives INVALID factorial interaction tests (Sawilowsky 1990) and this design's central claims are IR:HU and Sex interactions; ART aligns each effect before ranking and yields valid interaction tests, with contrasts via ART-C (Elkin et al. 2021). (An earlier Gamma-on-shifted-values approach was also dropped: p-values were highly sensitive to the arbitrary zero-shift.)"),
  c("Significance threshold", "alpha = 0.05 throughout (interaction triggers, post-hoc significance)."),
  c("", ""),
  c("RESULTS OVERVIEW", ""),
  c("Outliers removed (ROUT, Q=1% FDR)", sprintf("%d of %d points (%.1f%%), across %d/%d units", n_outliers_total, n_points_total, 100 * n_outliers_total / n_points_total, n_units_with_outliers, n_units)),
  c("Parametric (ANOVA) branch", n_normal),
  c("Nonparametric (GLM) branch", n_nonparam),
  c("Units with >=1 significant interaction -> post-hoc run", n_posthoc),
  c("Total pairwise post-hoc comparisons", n_posthoc_rows),
  c("  - significant at adjusted p < 0.05", n_sig_posthoc),
  c("Significant Female vs Male differences", sprintf("%d treatment-group comparisons, across %d units (of %d tested)",
      n_sexdiff_sig, n_units_sexdiff, nrow(sexdiff_df))),
  c("", ""),
  c("GLM family usage (nonparametric branch only)", "")
)

fam_lines <- lapply(names(fam_counts), function(nm) c(paste0("  - ", ifelse(is.na(nm) | nm == "NA", "(model fit failed)", nm)), unname(fam_counts[nm])))

caveat_lines <- list(
  c("", ""),
  c("DATA CAVEATS", ""),
  c("Unit of analysis", "Each row in the source data is one Prism data point. The source file's Instructions tab notes that per-cell replicate counts (n_per_cell_summary) exceed typical mouse-level group sizes (n=4-6) for several outcomes, suggesting technical replicates (e.g. multiple ROIs/images per mouse) rather than one row per mouse. The Prism export used here has no Mouse ID, so replicates could not be averaged to the mouse level; every row was treated as an independent observation in all models below."),
  c("Outlier removal on small groups", sprintf("ROUT at Q=1%% removed %d/%d points (%.1f%%) across %d/%d units. Removal is capped so at least 3 points always remain in each Sex x HU x IR cell (when more points qualified, only the most extreme were removed); treat any single-outcome outlier count in the context of its group sizes (see Outliers_Removed sheet for every removed point).", n_outliers_total, n_points_total, 100 * n_outliers_total / n_points_total, n_units_with_outliers, n_units)),
  c("Incomplete factorial design", "FIG HSP25, FIG HSP90, and Fig BDNF have no IR=50 group (only IR=0 and IR=100 present) -> IR modeled with 2 levels for these 3 outcomes only."),
  c("Small/zero cell", "SMI312 cingulate gyrus has one HU x IR x Sex cell with n=1 pre-outlier-removal; models still fit but that cell contributes no within-cell variance."),
  c("Model fit not obtainable for one stratum",
    if (n_fit_failed_units > 0) {
      paste0(n_fit_failed_units, " outcome(s) had at least one GLM stratum (see Notes column in Full_Results) that could not be fit due to numerical non-convergence on very small-scale, near-zero-heavy data: ",
             paste(fit_failed_outcomes, collapse = "; "), ". All other terms for these outcomes fit normally; only the specific flagged stratum is missing.")
    } else {
      "None -- every GLM/ANOVA stratum across all 80 units fit successfully."
    }),
  c("Rmd reports", "One self-contained .Rmd file per analysis unit is in rmarkdown_analyses/ (knit to HTML in the same folder); each sources R/analysis_functions.R and reproduces its row of this workbook in full, including the outlier-detection step, descriptive tables, and the Prism-style bar plot."),
  c("Standalone plots", "One Prism-style bar plot + decision-tree PDF per analysis unit is in generated_plots/, built from the outlier-cleaned data. Grouped multi-panel layouts are in grouped_plots/."),
  c("Plot y-axis labels", "Taken from the text layer of the original Prism exports in Figures/prism plots/ (see R/prism_axis_labels.R). Superscript-plus was normalised to '+'. Two source discrepancies were corrected there and are documented in that file: 'Ratio MAP2:SMI312 SSC.pdf' carries an axis label naming the opposite ratio, and 'Persistent *CD8_IFN-g Dissection.pdf' carries the TNF-a axis label. Five outcomes have no Prism PDF (Colon Morphology Score, Fig Rotarod normalized-baseline MvsF, CD19 [IR+1], CD19 Dissection, CD19/CD25, CD19/CD25 Dissection) and were given descriptive labels consistent with their siblings."),
  c("Sex-difference markers vs original figures", "The # markers are computed fresh by this pipeline (emmeans Female-vs-Male contrast on the fitted model, BH-adjusted across the whole outcome). They will NOT always reproduce the # annotations in the original Prism figures, which were computed in Prism on data that had not been through this ROUT step and may have used a different contrast/adjustment.")
)

wb <- createWorkbook()
options("openxlsx.font" = "Arial")

header_style <- createStyle(fontName = "Arial", textDecoration = "bold", fgFill = "#D9E1F2",
                             halign = "center", border = "Bottom", wrapText = TRUE)
sig_style <- createStyle(fontName = "Arial", fgFill = "#FCE4D6")
body_style <- createStyle(fontName = "Arial")

# --- Full_Results ---
addWorksheet(wb, "Full_Results")
writeData(wb, "Full_Results", full_results, headerStyle = header_style)
addStyle(wb, "Full_Results", body_style, rows = 2:(nrow(full_results) + 1), cols = 1:ncol(full_results), gridExpand = TRUE)
freezePane(wb, "Full_Results", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "Full_Results", cols = 1:ncol(full_results), widths = "auto")
setColWidths(wb, "Full_Results", cols = which(names(full_results) == "Notes"), widths = 60)
p_cols <- grep("_p$", names(full_results))
for (cc in p_cols) {
  conditionalFormatting(wb, "Full_Results", cols = cc, rows = 2:(nrow(full_results) + 1),
                         rule = "<0.05", style = sig_style)
}
addFilter(wb, "Full_Results", row = 1, cols = 1:ncol(full_results))

# --- PostHoc_Comparisons ---
addWorksheet(wb, "PostHoc_Comparisons")
writeData(wb, "PostHoc_Comparisons", posthoc_clean, headerStyle = header_style)
addStyle(wb, "PostHoc_Comparisons", body_style, rows = 2:(nrow(posthoc_clean) + 1), cols = 1:ncol(posthoc_clean), gridExpand = TRUE)
freezePane(wb, "PostHoc_Comparisons", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "PostHoc_Comparisons", cols = 1:ncol(posthoc_clean), widths = "auto")
padj_col <- which(names(posthoc_clean) == "P_adj")
conditionalFormatting(wb, "PostHoc_Comparisons", cols = padj_col, rows = 2:(nrow(posthoc_clean) + 1),
                       rule = "<0.05", style = sig_style)
addFilter(wb, "PostHoc_Comparisons", row = 1, cols = 1:ncol(posthoc_clean))

# --- Sex_Differences ---
sexdiff_clean <- sexdiff_df %>%
  transmute(Outcome, Block, Analysis_Unit,
             Treatment_Group = as.character(Group),
             n_Females, n_Males,
             Mean_Females = round(Mean_Females, 4), Mean_Males = round(Mean_Males, 4),
             p_raw = round(p_raw, 5), p_adj = round(p_adj, 5),
             Significance = ifelse(is.na(Hashes), "ns", Hashes),
             Significant_0.05 = ifelse(!is.na(Hashes), "Yes", "No")) %>%
  arrange(Outcome, Block, Treatment_Group)
addWorksheet(wb, "Sex_Differences")
writeData(wb, "Sex_Differences", sexdiff_clean, headerStyle = header_style)
if (nrow(sexdiff_clean) > 0) {
  addStyle(wb, "Sex_Differences", body_style, rows = 2:(nrow(sexdiff_clean) + 1), cols = 1:ncol(sexdiff_clean), gridExpand = TRUE)
  conditionalFormatting(wb, "Sex_Differences", cols = which(names(sexdiff_clean) == "p_adj"),
                         rows = 2:(nrow(sexdiff_clean) + 1), rule = "<0.05", style = sig_style)
}
freezePane(wb, "Sex_Differences", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "Sex_Differences", cols = 1:ncol(sexdiff_clean), widths = "auto")
addFilter(wb, "Sex_Differences", row = 1, cols = 1:ncol(sexdiff_clean))

# --- Outliers_Removed ---
outliers_clean <- outliers_df %>%
  transmute(Outcome, Block, Analysis_Unit, Sex, HU, IR, Value = round(Value, 6)) %>%
  arrange(Outcome, Block, Sex, HU, IR)
addWorksheet(wb, "Outliers_Removed")
writeData(wb, "Outliers_Removed", outliers_clean, headerStyle = header_style)
if (nrow(outliers_clean) > 0) {
  addStyle(wb, "Outliers_Removed", body_style, rows = 2:(nrow(outliers_clean) + 1), cols = 1:ncol(outliers_clean), gridExpand = TRUE)
}
freezePane(wb, "Outliers_Removed", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "Outliers_Removed", cols = 1:ncol(outliers_clean), widths = "auto")
addFilter(wb, "Outliers_Removed", row = 1, cols = 1:ncol(outliers_clean))

# --- Summary ---
addWorksheet(wb, "Summary")
all_lines <- c(summary_lines, fam_lines, caveat_lines)
summary_mat <- do.call(rbind, lapply(all_lines, function(x) c(as.character(x[1]), as.character(x[2]))))
writeData(wb, "Summary", summary_mat, colNames = FALSE)
addStyle(wb, "Summary", createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1)
section_rows <- which(summary_mat[, 2] == "" & summary_mat[, 1] != "")
for (r in section_rows) {
  addStyle(wb, "Summary", createStyle(fontName = "Arial", textDecoration = "bold", fgFill = "#D9E1F2"), rows = r, cols = 1:2, gridExpand = TRUE)
}
addStyle(wb, "Summary", createStyle(fontName = "Arial", wrapText = TRUE, valign = "top"),
         rows = 1:nrow(summary_mat), cols = 1:2, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Summary", cols = 1, widths = 42)
setColWidths(wb, "Summary", cols = 2, widths = 90)

out_path <- "HU_IR_Sex_Statistical_Results.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("Wrote", out_path, "\n")
cat("Full_Results rows:", nrow(full_results), "\n")
cat("PostHoc_Comparisons rows:", nrow(posthoc_clean), "\n")

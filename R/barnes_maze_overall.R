# Barnes Maze OVERALL (omnibus) effects, via the decision tree.
#
# Unlike barnes_maze_analysis.R (within-trial pairwise comparisons for the
# heatmap), this fits the full factorial IR x HU x Sex x Trial with Trial as a
# repeated measure (each mouse ran all 8 trials; Subject recovered from the
# Prism column position) and reports the OVERALL main effects and interactions.
#
# Decision tree (same logic as the main pipeline):
#   normality: Shapiro-Wilk on the residuals of the fixed-effects factorial model.
#     normal    -> linear mixed model (lmerTest), Type III Satterthwaite F-tests.
#     not normal-> Aligned Rank Transform with a Subject random effect
#                  (ARTool::art ... + (1|Subject)), which gives valid factorial
#                  main-effect AND interaction tests for repeated-measures ranks.
# (ROUT outlier removal is not applied here: Memory Approach is an ordinal 1-4
#  score, and per-observation removal is inappropriate for repeated measures.)

suppressPackageStartupMessages({
  library(dplyr); library(ARTool); library(lmerTest); library(openxlsx)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))
ALPHA <- 0.05

d <- read.csv("barnes_data_subject.csv", stringsAsFactors = FALSE, check.names = FALSE)
d$IR      <- factor(d$IR, levels = c("0", "50", "100"))
d$HU      <- factor(d$HU, levels = c("Sham", "HU"))
d$Sex     <- factor(d$Sex, levels = c("Females", "Males"))
d$Trial   <- factor(d$Trial, levels = as.character(1:8))
d$Subject <- factor(d$Subject)

TERMS <- c("IR","HU","Sex","Trial","IR:HU","IR:Sex","IR:Trial","HU:Sex","HU:Trial",
           "Sex:Trial","IR:HU:Sex","IR:HU:Trial","IR:Sex:Trial","HU:Sex:Trial","IR:HU:Sex:Trial")

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 1e-4) "****" else if (p < 1e-3) "***" else if (p < 1e-2) "**" else if (p < 0.05) "*" else "ns"
}

analyze_overall <- function(sub) {
  oc <- sub$Outcome[1]
  # branch: Shapiro on residuals of the fixed-effects factorial model
  fixed <- lm(Value ~ IR * HU * Sex * Trial, data = sub)
  r <- residuals(fixed)
  shap <- tryCatch(shapiro.test(r), error = function(e) NULL)
  p_shap <- if (is.null(shap)) NA_real_ else shap$p.value
  normal <- isTRUE(!is.na(p_shap) && p_shap > ALPHA)

  if (normal) {
    m <- lmerTest::lmer(Value ~ IR * HU * Sex * Trial + (1 | Subject), data = sub)
    at <- as.data.frame(anova(m, type = 3))       # Type III Satterthwaite
    tab <- data.frame(Term = rownames(at), Statistic = round(at[["F value"]], 3),
                       df1 = at[["NumDF"]], df2 = round(at[["DenDF"]], 1),
                       p = at[["Pr(>F)"]], stringsAsFactors = FALSE)
    method <- sprintf("Linear mixed model (lmerTest, Type III), Trial repeated within Subject. Shapiro p=%.3g -> parametric.", p_shap)
  } else {
    m <- ARTool::art(Value ~ IR * HU * Sex * Trial + (1 | Subject), data = sub)
    at <- as.data.frame(anova(m))
    fcol <- intersect(c("F value", "F"), names(at))[1]
    tab <- data.frame(Term = at$Term, Statistic = round(at[[fcol]], 3),
                       df1 = at$Df, df2 = round(at$Df.res, 1),
                       p = at[["Pr(>F)"]], stringsAsFactors = FALSE)
    method <- sprintf("Aligned Rank Transform (ARTool) with Subject random effect. Shapiro p=%.3g -> nonparametric.", p_shap)
  }
  tab$Term <- gsub(" ", "", tab$Term)
  tab <- tab[match(TERMS, tab$Term), ]
  tab$Term <- TERMS
  tab$Significance <- vapply(tab$p, stars, character(1))
  tab$Sig <- ifelse(!is.na(tab$p) & tab$p < ALPHA, "Yes", "No")
  list(outcome = oc, table = tab, method = method, normal = normal)
}

results <- list()
for (oc in unique(d$Outcome)) {
  cat("=====", oc, "=====\n")
  res <- analyze_overall(droplevels(d[d$Outcome == oc, ]))
  results[[oc]] <- res
  cat(res$method, "\n")
  print(res$table[, c("Term","Statistic","df1","df2","p","Significance")], row.names = FALSE)
  cat("\n")
}

# ---- Excel ----------------------------------------------------------------
wb <- createWorkbook(); options("openxlsx.font" = "Arial")
hdr <- createStyle(fontName = "Arial", textDecoration = "bold", fgFill = "#D9E1F2", halign = "center", border = "Bottom")
sig <- createStyle(fontName = "Arial", fgFill = "#FCE4D6")

for (oc in names(results)) {
  res <- results[[oc]]
  t <- res$table
  t$p <- round(t$p, 5)
  sheet <- substr(gsub("[^A-Za-z0-9]+", "_", oc), 1, 28)
  addWorksheet(wb, sheet)
  writeData(wb, sheet, data.frame(Effect = t$Term, Statistic_F = t$Statistic,
                                   df1 = t$df1, df2 = t$df2, p_value = t$p,
                                   Significance = t$Significance, Significant_0.05 = t$Sig),
            headerStyle = hdr)
  freezePane(wb, sheet, firstRow = TRUE); setColWidths(wb, sheet, 1:7, "auto")
  conditionalFormatting(wb, sheet, cols = 5, rows = 2:(nrow(t)+1), rule = "<0.05", style = sig)
  writeData(wb, sheet, data.frame(Method = res$method), startRow = nrow(t) + 3, headerStyle = hdr)
}

# combined overview sheet
overview <- do.call(rbind, lapply(names(results), function(oc) {
  t <- results[[oc]]$table
  data.frame(Outcome = oc, Effect = t$Term, F = t$Statistic, p = round(t$p, 5),
             Significance = t$Significance, Significant = t$Sig, stringsAsFactors = FALSE)
}))
addWorksheet(wb, "Overview"); writeData(wb, "Overview", overview, headerStyle = hdr)
freezePane(wb, "Overview", firstRow = TRUE); setColWidths(wb, "Overview", 1:6, "auto")
conditionalFormatting(wb, "Overview", cols = 4, rows = 2:(nrow(overview)+1), rule = "<0.05", style = sig)

info <- data.frame(Field = c("Analysis","Design","Repeated measure","Decision tree",
                              "Memory Approach","Latency","Note"),
  Detail = c(
    "Barnes Maze OVERALL effects: full factorial IR x HU x Sex x Trial.",
    "IR (0/50/100 cGy) x HU (Sham/HU) x Sex (F/M) x Trial (1-8).",
    "Trial is a repeated measure; each mouse ran all 8 trials (Subject random effect).",
    "Shapiro-Wilk on the fixed-model residuals -> parametric linear mixed model (Type III) OR Aligned Rank Transform with Subject random effect.",
    results[["Memory Approach"]]$method,
    results[["Latency"]]$method,
    "This complements (does not replace) the within-trial pairwise heatmap results in BarnesMaze_Statistical_Results.xlsx."))
addWorksheet(wb, "Methods"); writeData(wb, "Methods", info, headerStyle = hdr)
setColWidths(wb, "Methods", 1, 22); setColWidths(wb, "Methods", 2, 110)
addStyle(wb, "Methods", createStyle(fontName = "Arial", wrapText = TRUE, valign = "top"),
         rows = 1:(nrow(info)+1), cols = 1:2, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, "BarnesMaze_OverallEffects.xlsx", overwrite = TRUE)
saveRDS(results, "results/barnes_overall.rds")
cat("Wrote BarnesMaze_OverallEffects.xlsx\n")

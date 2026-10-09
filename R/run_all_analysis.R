# Master driver: run analyze_outcome() for every Outcome x Block unit and
# collect combined summary + post-hoc results.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

here <- dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1]))))
if (length(here) == 0 || is.na(here) || here == "") here <- getwd()
setwd(here)

source("R/analysis_functions.R")

XLSX <- "HU_IR_Sex_LongFormat_FROM_PRISM.xlsx"

long <- suppressMessages(read_excel(XLSX, sheet = "Long_Format_Data"))

units <- long %>%
  distinct(Outcome, Block) %>%
  arrange(Outcome, Block)

cat("Analysis units:", nrow(units), "\n")

summaries <- list()
posthocs <- list()
outliers <- list()
sexdiffs <- list()
errors <- list()

for (i in seq_len(nrow(units))) {
  oc <- units$Outcome[i]
  bl <- units$Block[i]
  label <- if (length(unique(long$Block[long$Outcome == oc])) > 1) {
    paste0(oc, " [Block ", bl, "]")
  } else {
    oc
  }
  res <- tryCatch(analyze_outcome(oc, bl, XLSX), error = function(e) {
    errors[[length(errors) + 1]] <<- data.frame(Outcome = oc, Block = bl, Error = conditionMessage(e))
    NULL
  })
  if (is.null(res)) {
    cat(sprintf("[%2d/%2d] ERROR   %s\n", i, nrow(units), label))
    next
  }
  res$summary$Analysis_Unit <- label
  summaries[[length(summaries) + 1]] <- res$summary
  if (!is.null(res$posthoc)) {
    res$posthoc$Analysis_Unit <- label
    posthocs[[length(posthocs) + 1]] <- res$posthoc
  }
  if (!is.null(res$outliers_removed) && nrow(res$outliers_removed) > 0) {
    ord <- res$outliers_removed
    ord$Analysis_Unit <- label
    outliers[[length(outliers) + 1]] <- ord
  }
  if (!is.null(res$sex_diff) && nrow(res$sex_diff) > 0) {
    sd <- as.data.frame(res$sex_diff)
    sd$Outcome <- oc; sd$Block <- bl; sd$Analysis_Unit <- label
    sexdiffs[[length(sexdiffs) + 1]] <- sd
  }
  cat(sprintf("[%2d/%2d] OK      %-55s Normal=%-5s Branch=%-22s Removed=%d PostHoc=%s\n",
              i, nrow(units), label, res$summary$Normal, res$summary$Branch, res$summary$N_removed, res$summary$PostHoc_Performed))
}

summary_df <- dplyr::bind_rows(summaries)
posthoc_df <- if (length(posthocs)) dplyr::bind_rows(posthocs) else data.frame()
outliers_df <- if (length(outliers)) dplyr::bind_rows(outliers) else data.frame()
sexdiff_df <- if (length(sexdiffs)) dplyr::bind_rows(sexdiffs) else data.frame()
error_df <- if (length(errors)) dplyr::bind_rows(errors) else data.frame()

dir.create("results", showWarnings = FALSE)
saveRDS(summary_df, "results/summary_results.rds")
saveRDS(posthoc_df, "results/posthoc_results.rds")
saveRDS(outliers_df, "results/outliers_removed.rds")
saveRDS(sexdiff_df, "results/sex_differences.rds")
saveRDS(error_df, "results/errors.rds")
write.csv(summary_df, "results/summary_results.csv", row.names = FALSE)
write.csv(posthoc_df, "results/posthoc_results.csv", row.names = FALSE)
write.csv(outliers_df, "results/outliers_removed.csv", row.names = FALSE)
write.csv(sexdiff_df, "results/sex_differences.csv", row.names = FALSE)
if (nrow(error_df)) write.csv(error_df, "results/errors.csv", row.names = FALSE)

cat("\n==== DONE ====\n")
cat("Units analyzed:", nrow(summary_df), "of", nrow(units), "\n")
cat("Errors:", nrow(error_df), "\n")
if (nrow(error_df)) print(error_df)
cat("Parametric:", sum(summary_df$Normal, na.rm = TRUE), "  Nonparametric:", sum(!summary_df$Normal, na.rm = TRUE), "\n")
cat("Post-hoc performed:", sum(summary_df$PostHoc_Performed, na.rm = TRUE), "\n")
cat("Total post-hoc pairwise rows:", nrow(posthoc_df), "\n")
cat("Total outliers removed (ROUT, Q=1%):", nrow(outliers_df), "of", sum(summary_df$N_original, na.rm = TRUE), "points\n")
cat("Outcomes with >=1 outlier removed:", sum(summary_df$N_removed > 0, na.rm = TRUE), "\n")

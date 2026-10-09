# Generates one standalone PDF per Outcome x Block unit into generated_plots/:
# a Prism-style bar plot on top, with the normality -> test -> post-hoc
# decision-tree table directly below it on the same page.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))

source("R/analysis_functions.R")

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

dir.create("generated_plots", showWarnings = FALSE)

failed <- character(0)
for (i in seq_len(nrow(units))) {
  oc <- units$Outcome[i]
  bl <- units$Block[i]
  has_multi_block <- block_counts$n[block_counts$Outcome == oc] > 1
  label <- if (has_multi_block) paste0(oc, " [Block ", bl, "]") else oc
  fname <- paste0(sprintf("%02d", i), "_", slugify(label), ".pdf")
  out_path <- file.path("generated_plots", fname)

  ok <- tryCatch({
    res <- analyze_outcome(oc, bl, XLSX)
    render_outcome_pdf(label, res$data_clean, res, out_path, y_label = prism_y_label(oc))
    TRUE
  }, error = function(e) {
    cat("FAILED:", label, "-", conditionMessage(e), "\n")
    FALSE
  })
  cat(sprintf("[%2d/%2d] %s %s\n", i, nrow(units), if (ok) "OK  " else "FAIL", fname))
  if (!ok) failed <- c(failed, label)
}

cat("\n==== PLOT GENERATION DONE ====\n")
cat("Succeeded:", nrow(units) - length(failed), "/", nrow(units), "\n")
if (length(failed)) { cat("Failed:\n"); print(failed) }

# Generates multi-panel grouped PDFs into grouped_plots/: outcomes that share
# a marker across regions/tissues are tiled onto one page, each panel showing
# its Prism-style bar plot with its decision-tree table directly beneath.
#
# Immune flow cytometry outcomes are split into two groups by timepoint:
# "[IR+1]" (acute) and "Dissection" (persistent).

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(gridExtra)
  library(grid)
})

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))
source("R/analysis_functions.R")

XLSX <- "HU_IR_Sex_LongFormat_FROM_PRISM.xlsx"

# ---- Group definitions ----------------------------------------------------
# Each entry: list(title = page title, outcomes = character vector of Outcome names).
# Outcomes not listed in any group keep only their individual PDF in generated_plots/.

GROUPS <- list(
  list(file = "01_IgG_vessel_area",
       title = "Vessel IgG+ area (%) - by brain region",
       outcomes = c("%area-IgG-vessel CA1", "%area-IgG-vessel CG",
                     "%area-IgG-vessel dentate gyrus", "%area-IgG-vessel SSC")),

  list(file = "02_IgG_perivascular_area",
       title = "Perivascular IgG+ area per vessel - by brain region",
       outcomes = c("%area-per-vessel_IgG-perivascular CA1", "%area-per-vessel_IgG-perivascular CG",
                     "%area-per-vessel_IgG-perivascular dentate gyrus",
                     "%area-per-vessel_IgG-perivascular HYP", "%area-per-vessel_IgG-perivascular SST")),

  list(file = "03_Cldn5_vessel",
       title = "Claudin-5 vessel coverage - by brain region",
       outcomes = c("Cldn5-vessel CA1", "Cldn5-vessel CG", "Cldn5-vessel dentate gyrus",
                     "Cldn5-vessel HYP", "Cldn5-vessel SST")),

  list(file = "04_GFAP",
       title = "GFAP (astrocytes) - by brain region",
       outcomes = c("GFAP CA1", "GFAP cingulate gyrus", "GFAP corpus callosum", "GFAP dentate gyrus")),

  list(file = "05_Iba1",
       title = "Iba-1 (microglia) - by brain region",
       outcomes = c("Iba-1 CA1", "Iba-1 cingulate gyrus", "Iba-1 corpus callosum", "Iba-1 dentate gyrus")),

  list(file = "06_SMI312_MAP2",
       title = "SMI312 / MAP2 (axons & dendrites) - by brain region",
       outcomes = c("SMI312 cingulate gyrus", "SMI312 corpus callosum", "SMI312 external capsule",
                     "SMI312 SSC", "MAP2 SSC", "Ratio MAP2:SMI312 SSC", "Ratio SMI312:MAP2 SSC")),

  list(file = "07_immune_acute_IR1",
       title = "Immune flow cytometry - ACUTE [IR+1]",
       outcomes = c("Acute Cytotoxic T cells **CD3/CD8 [IR+1]", "CD11b+ [IR+1]", "CD11b/TNFa [IR+1]",
                     "CD19 [IR+1]", "CD19/CD25", "CD3/CD4 [IR+1]", "CD4/FoxP3 [IR+1]",
                     "CD4/IFN-g [IR+1]", "CD4/IL-4 [IR+1]", "CD8/IFN-g [IR+1]", "CD8/TNF-a [IR+1]",
                     "NK1.1 [IR+1]", "NK1.1/IL-10 [IR+1]", "NK1.1/TNF-a and/or IFN-g [IR+1]")),

  list(file = "08_immune_persistent_Dissection",
       title = "Immune flow cytometry - PERSISTENT (Dissection)",
       outcomes = c("CD11b+ Dissection", "CD11b/TNFa. Dissection", "CD19 Dissection",
                     "CD19/CD25 Dissection", "CD3/CD4 Dissection", "CD3/CD8 Dissection",
                     "CD4/FoxP3 Dissection", "CD4/IFN-g Dissection", "CD4/IL-4 Dissection",
                     "CD8/IFN-g Dissection", "CD8/TNF-a Dissection", "NK1.1 Dissection",
                     "NK1.1/IL-10 Dissection", "NK1.1/TNF-a and/or IFN-g Dissection")),

  list(file = "09_gut_CD45",
       title = "CD45 (leukocyte infiltration) - by gut tissue",
       outcomes = c("CD45 Colon", "CD45 Ileum", "CD45 Jejunum")),

  list(file = "10_gut_ZO1",
       title = "ZO-1 (tight junctions) - by gut tissue",
       outcomes = c("ZO-1 Colon", "ZO-1 ileum", "ZO-1 Jejunum")),

  list(file = "11_gut_morphology",
       title = "Gut morphology - Alcian Blue, Villi:Crypt ratio, Colon score",
       outcomes = c("IL Alcian Blue", "JE Alcian Blue", "IL Villi:Crypt Ratio",
                     "JE Villi:Crypt Ratio", "Colon Morphology Score")),

  list(file = "12_behavior_rotarod",
       title = "Behavior - Rotarod",
       outcomes = c("Fig ROTAROD baseline", "Fig Rotarod difference MvsF",
                     "Fig Rotarod normalized-baseline MvsF")),

  list(file = "13_behavior_NOR",
       title = "Behavior - Novel Object Recognition (NOR)",
       outcomes = c("FIG *NOR Discrimination Index", "Fig NOR Recognition Index (%)")),

  list(file = "14_behavior_other",
       title = "Behavior - EPM, FST, Barnes maze",
       outcomes = c("FIGURE *EPM Closed Arms", "FIGURE *FST Time Immobille 2",
                     "SUPP BarnesM probe target time")),

  list(file = "15_stress_neurotrophin",
       title = "Stress proteins & neurotrophin - HSP25, HSP90, BDNF",
       outcomes = c("FIG HSP25", "FIG HSP90", "Fig BDNF"))
)

# ---- Build panels ---------------------------------------------------------

long <- suppressMessages(read_excel(XLSX, sheet = "Long_Format_Data"))
block_counts <- long %>% distinct(Outcome, Block) %>% count(Outcome)

#' One panel = plot on top, its decision-tree table beneath, as a single grob.
build_panel <- function(outcome_name, block, label) {
  res <- analyze_outcome(outcome_name, block, XLSX)
  p <- make_prism_plot(label, res$data_clean, res$posthoc,
                        y_label = prism_y_label(outcome_name),
                        sex_diff = res$sex_diff, summary_row = res$summary, base_size = 15)
  dt <- make_decision_tree_table(res)
  tg <- decision_tree_grob(dt, width = 78, step_col_frac = 0.24)
  gridExtra::arrangeGrob(
    ggplot2::ggplotGrob(p), tg,
    ncol = 1,
    heights = grid::unit.c(unit(1, "null"), sum(tg$heights) + unit(3, "mm"))
  )
}

dir.create("grouped_plots", showWarnings = FALSE)

PANEL_W <- 8.0    # inches per panel column
PANEL_H <- 7.0    # inches per panel row (plot + table)

failed <- character(0)
for (g in GROUPS) {
  panels <- list()
  for (oc in g$outcomes) {
    blocks <- long$Block[long$Outcome == oc]
    if (length(blocks) == 0) {
      cat("  MISSING outcome (skipped):", oc, "\n")
      next
    }
    for (bl in sort(unique(blocks))) {
      has_multi <- block_counts$n[block_counts$Outcome == oc] > 1
      label <- if (has_multi) paste0(oc, " [Block ", bl, "]") else oc
      pn <- tryCatch(build_panel(oc, bl, label), error = function(e) {
        cat("  FAILED panel:", label, "-", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(pn)) panels[[length(panels) + 1]] <- pn
    }
  }

  if (length(panels) == 0) {
    failed <- c(failed, g$file)
    next
  }

  ncol <- if (length(panels) == 1) 1 else 2
  nrow <- ceiling(length(panels) / ncol)
  out_path <- file.path("grouped_plots", paste0(g$file, ".pdf"))

  grDevices::pdf(out_path, width = PANEL_W * ncol, height = PANEL_H * nrow + 0.8)
  gridExtra::grid.arrange(
    grobs = panels, ncol = ncol,
    top = grid::textGrob(
      paste0(g$title,
             "\n*  within-sex comparison (post-hoc)      #  Female vs Male within that treatment group"),
      gp = grid::gpar(fontsize = 15, fontface = "bold", fontfamily = PLOT_FONT))
  )
  grDevices::dev.off()

  cat(sprintf("OK   %-42s %2d panel(s) -> %d x %d grid\n", basename(out_path), length(panels), nrow, ncol))
}

cat("\n==== GROUPED PDF GENERATION DONE ====\n")
cat("Groups written:", length(GROUPS) - length(failed), "/", length(GROUPS), "\n")

# Report which outcomes were left ungrouped (individual plots only).
grouped_outcomes <- unlist(lapply(GROUPS, function(g) g$outcomes))
ungrouped <- setdiff(unique(long$Outcome), grouped_outcomes)
if (length(ungrouped)) {
  cat("Ungrouped outcomes (individual plots only):\n")
  cat(paste0("  - ", ungrouped, collapse = "\n"), "\n")
} else {
  cat("All outcomes assigned to a group.\n")
}

# Y-axis labels for each Outcome, taken from the original Prism exports in
# ../Figures/prism plots/*.pdf (extracted from the PDF text layer, then
# normalised: superscript-plus U+207A -> "+", since the Prism labels already
# mix the two forms and Arial has no U+207A glyph).
#
# Outcomes with no corresponding Prism PDF are marked NO_PRISM_PDF below and
# were given a descriptive label consistent with their sibling outcomes.

PRISM_Y_LABELS <- c(
  # ---- Brain: IgG / vessels -------------------------------------------
  "%area-IgG-vessel CA1"                            = "Vessel IgG+ area (%)",
  "%area-IgG-vessel CG"                             = "Vessel IgG+ area (%)",
  "%area-IgG-vessel SSC"                            = "Vessel IgG+ area (%)",
  "%area-IgG-vessel dentate gyrus"                  = "Vessel IgG+ area (%)",

  "%area-per-vessel_IgG-perivascular CA1"           = "Perivascular IgG+ area / vessel area (%)",
  "%area-per-vessel_IgG-perivascular CG"            = "Perivascular IgG+ area / vessel area (%)",
  "%area-per-vessel_IgG-perivascular HYP"           = "Perivascular IgG+ area / vessel area (%)",
  "%area-per-vessel_IgG-perivascular SST"           = "Perivascular IgG+ area / vessel area (%)",
  "%area-per-vessel_IgG-perivascular dentate gyrus" = "Perivascular IgG+ area / vessel area (%)",

  "Cldn5-vessel CA1"                                = "Claudin-5 (%area)",
  "Cldn5-vessel CG"                                 = "Claudin-5 (%area)",
  "Cldn5-vessel HYP"                                = "Claudin-5 (%area)",
  "Cldn5-vessel SST"                                = "Claudin-5 (%area)",
  "Cldn5-vessel dentate gyrus"                      = "Claudin-5 (%area)",

  # ---- Brain: glia ----------------------------------------------------
  "GFAP CA1"                                        = "GFAP (%area)",
  "GFAP cingulate gyrus"                            = "GFAP (%area)",
  "GFAP corpus callosum"                            = "GFAP (%area)",
  "GFAP dentate gyrus"                              = "GFAP (%area)",

  "Iba-1 CA1"                                       = "Iba-1+ (% area)",
  "Iba-1 cingulate gyrus"                           = "Iba-1+ (% area)",
  "Iba-1 corpus callosum"                           = "Iba-1+ (% area)",
  "Iba-1 dentate gyrus"                             = "Iba-1+ (% area)",

  # ---- Brain: neurites ------------------------------------------------
  "SMI312 SSC"                                      = "SMI-312 (% area)",
  "SMI312 cingulate gyrus"                          = "SMI-312 (% area)",
  "SMI312 corpus callosum"                          = "SMI-312 (% area)",
  "SMI312 external capsule"                         = "SMI-312 (% area)",
  "MAP2 SSC"                                        = "MAP2+ (% area)",
  # NOTE: the single Prism file "*Ratio MAP2:SMI312 SSC.pdf" carries the
  # y-label "Ratio SMI-312:MAP2+", i.e. its filename and axis disagree.
  # Each ratio outcome is labelled here to match its own name.
  "Ratio SMI312:MAP2 SSC"                           = "Ratio SMI-312:MAP2+",
  "Ratio MAP2:SMI312 SSC"                           = "Ratio MAP2:SMI-312+",

  # ---- Stress proteins / neurotrophin ---------------------------------
  "FIG HSP25"                                       = "HSP25 (fold expression)",
  "FIG HSP90"                                       = "HSP90 (fold expression)",
  "Fig BDNF"                                        = "BDNF (fold expression)",

  # ---- Gut -------------------------------------------------------------
  "CD45 Colon"                                      = "CD45 (+cells/field)",
  "CD45 Ileum"                                      = "CD45 (+cells/field)",
  "CD45 Jejunum"                                    = "CD45 (+cells/field)",
  "ZO-1 Colon"                                      = "ZO-1+ (%area)",
  "ZO-1 Jejunum"                                    = "ZO-1+ (%area)",
  "ZO-1 ileum"                                      = "ZO-1+ (%area)",
  "IL Alcian Blue"                                  = "Mucin (% area)",
  "JE Alcian Blue"                                  = "Mucin (% area)",
  "IL Villi:Crypt Ratio"                            = "Villi:Crypt Ratio",
  "JE Villi:Crypt Ratio"                            = "Villi:Crypt Ratio",
  "Colon Morphology Score"                          = "Colon Morphology Score",   # NO_PRISM_PDF

  # ---- Behavior ---------------------------------------------------------
  "FIG *NOR Discrimination Index"                   = "Discrimination Index",
  "Fig NOR Recognition Index (%)"                   = "Recognition Index (%)",
  "Fig ROTAROD baseline"                            = "Latency to fall (sec)",
  "Fig Rotarod difference MvsF"                     = "Latency to fall (Δ from baseline)",
  "Fig Rotarod normalized-baseline MvsF"            = "Latency to fall (normalized to baseline)",  # NO_PRISM_PDF
  "FIGURE *EPM Closed Arms"                         = "Time in Closed Arms (seconds)",
  "FIGURE *FST Time Immobille 2"                    = "Time Immobile (seconds)",
  "SUPP BarnesM probe target time"                  = "Target Time (sec)",

  # ---- Immune flow cytometry: ACUTE [IR+1] -----------------------------
  "Acute Cytotoxic T cells **CD3/CD8 [IR+1]"        = "CD8+ cells (% of CD3+ cells)",
  "CD3/CD4 [IR+1]"                                  = "CD4+ cells (% of CD3+ cells)",
  "CD11b+ [IR+1]"                                   = "% CD11b+ (of CD45+ cells)",
  "CD11b/TNFa [IR+1]"                               = "TNF-α+ cells (% of CD11b+ cells)",
  "NK1.1 [IR+1]"                                    = "% NK1.1+ cells (of CD45+ cells)",
  "NK1.1/TNF-a and/or IFN-g [IR+1]"                 = "IFN-γ+ TNF-α+ cells (% of NK1.1+ cells)",
  "NK1.1/IL-10 [IR+1]"                              = "IL-10+ cells (% of NK1.1+ cells)",
  "CD4/IFN-g [IR+1]"                                = "IFN-γ+ cells (% of CD4+ T cells)",
  "CD4/IL-4 [IR+1]"                                 = "IL-4+ cells (% of CD4+ cells)",
  "CD4/FoxP3 [IR+1]"                                = "CD4+ FoxP3+ cells (% of CD3+ cells)",
  "CD8/IFN-g [IR+1]"                                = "% IFN-g+ within CD8+",
  "CD8/TNF-a [IR+1]"                                = "% TNF-a+ within CD8+",
  "CD19 [IR+1]"                                     = "% CD19+ (of CD45+ cells)",          # NO_PRISM_PDF
  "CD19/CD25"                                       = "CD25+ cells (% of CD19+ cells)",    # NO_PRISM_PDF

  # ---- Immune flow cytometry: PERSISTENT (Dissection) ------------------
  "CD3/CD8 Dissection"                              = "CD8+ cells (% of CD3+ cells)",
  "CD3/CD4 Dissection"                              = "CD4+ cells (% of CD3+ cells)",
  "CD11b+ Dissection"                               = "% CD11b+ (of CD45+ cells)",
  "CD11b/TNFa. Dissection"                          = "TNF-α+ cells (% of CD11b+ cells)",
  "NK1.1 Dissection"                                = "% NK1.1+ cells (of CD45+ cells)",
  "NK1.1/TNF-a and/or IFN-g Dissection"             = "IFN-γ+ TNF-α+ cells (% of NK1.1+ cells)",
  "NK1.1/IL-10 Dissection"                          = "IL-10+ cells (% of NK1.1+ cells)",
  "CD4/IFN-g Dissection"                            = "IFN-γ+ cells (% of CD4+ T cells)",
  "CD4/IL-4 Dissection"                             = "IL-4+ cells (% of CD4+ cells)",
  "CD4/FoxP3 Dissection"                            = "CD4+ FoxP3+ cells (% of CD3+ cells)",
  # NOTE: the Prism file "Persistent *CD8_IFN-g Dissection.pdf" carries the
  # y-label "% TNF-a+ within CD8+", which duplicates its TNF-a sibling and is
  # inconsistent with the outcome name -- corrected to IFN-g here.
  "CD8/IFN-g Dissection"                            = "% IFN-g+ within CD8+",
  "CD8/TNF-a Dissection"                            = "% TNF-a+ within CD8+",
  "CD19 Dissection"                                 = "% CD19+ (of CD45+ cells)",          # NO_PRISM_PDF
  "CD19/CD25 Dissection"                            = "CD25+ cells (% of CD19+ cells)"     # NO_PRISM_PDF
)

#' Y-axis label for an outcome; falls back to "Value" if unmapped.
prism_y_label <- function(outcome_name) {
  lbl <- PRISM_Y_LABELS[[outcome_name]]
  if (is.null(lbl) || is.na(lbl)) "Value" else lbl
}

Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

setwd(dirname(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))))

files <- sort(list.files("rmarkdown_analyses", pattern = "\\.Rmd$", full.names = TRUE))
cat("Rendering", length(files), "Rmd files...\n")

failed <- character(0)
for (i in seq_along(files)) {
  f <- files[i]
  ok <- tryCatch({
    rmarkdown::render(f, quiet = TRUE, envir = new.env())
    TRUE
  }, error = function(e) {
    cat("FAILED:", f, "-", conditionMessage(e), "\n")
    FALSE
  })
  cat(sprintf("[%2d/%2d] %s %s\n", i, length(files), if (ok) "OK  " else "FAIL", basename(f)))
  if (!ok) failed <- c(failed, f)
}

cat("\n==== RENDER DONE ====\n")
cat("Succeeded:", length(files) - length(failed), "/", length(files), "\n")
if (length(failed)) {
  cat("Failed files:\n")
  print(failed)
}

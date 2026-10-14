# Rebuild the analysis and PDF using the existing saved data.
# Run from this directory: Rscript build.R
# R packages: knitr, ggplot2, gridExtra. TeX: pdflatex, biber (and lipo on macOS for fallback).
args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
build_dir <- if (length(args)) dirname(normalizePath(sub("^--file=", "", args[1]))) else normalizePath(".")
setwd(build_dir)
Sys.setenv(AMBIGUITY_DIR = build_dir)
check_appendix_order <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  marker <- regexpr("(?m)^\\\\appendix[ \\t]*$", text, perl = TRUE)
  if (marker < 1L) stop("The manuscript is missing its Online Appendix.")
  main <- substr(text, 1L, marker - 1L)
  appendix <- substring(text, marker + nchar("\\appendix"))
  labels_in <- function(text, command) {
    matches <- regmatches(text, gregexpr(paste0("\\\\", command, "\\{[^}]+\\}"),
                                        text, perl = TRUE))[[1L]]
    sub("^.*\\{([^}]+)\\}$", "\\1", matches)
  }
  definitions <- labels_in(appendix, "label")
  citations <- unique(labels_in(main, "(?:ref|eqref|autoref)"))
  section_positions <- as.integer(gregexpr("\\\\section\\{", appendix, perl = TRUE)[[1L]])
  label_positions <- as.integer(gregexpr("\\\\label\\{[^}]+\\}", appendix, perl = TRUE)[[1L]])
  topic_by_label <- setNames(findInterval(label_positions, section_positions), definitions)
  mentioned <- citations[citations %in% definitions]
  topics <- unname(topic_by_label[mentioned])
  if (any(diff(topics) < 0L)) {
    stop("Online Appendix sections must follow the topics' first appearance in the main paper.\n",
         "Check these references: ", paste(mentioned, collapse = " -> "))
  }
  for (kind in c("fig", "tab")) {
    prefix <- paste0(kind, ":")
    mentioned <- citations[startsWith(citations, prefix) & citations %in% definitions]
    displayed <- definitions[definitions %in% mentioned]
    if (!identical(mentioned, displayed)) {
      stop("Online Appendix ", c(fig = "figures", tab = "tables")[[kind]],
           " must follow their first citation in the main paper.\nMain: ",
           paste(mentioned, collapse = " -> "), "\nAppendix: ",
           paste(displayed, collapse = " -> "))
    }
  }
  invisible(TRUE)
}
check_appendix_order("Ambiguity.rnw")
knitr::knit("Ambiguity.rnw", output = "Ambiguity.tex", quiet = TRUE)
run_checked <- function(command, arguments) {
  if (!nzchar(command)) stop("Required TeX tool is unavailable.")
  status <- system2(command, arguments, stdout = "build_output.log", stderr = "build_output.log")
  if (status != 0) stop("Build step failed. See build_output.log. Status: ", status)
}
tex <- unname(Sys.which("pdflatex"))
biber <- unname(Sys.which("biber"))
tex_args <- c("-interaction=nonstopmode", "-halt-on-error", "-synctex=1", "Ambiguity.tex")
run_checked(tex, tex_args)
status <- system2(biber, "Ambiguity", stdout = "build_output.log", stderr = "build_output.log")
if (status != 0 && Sys.info()[["sysname"]] == "Darwin" && nzchar(Sys.which("lipo"))) {
  # Some universal macOS Biber builds fail their own architecture extraction.
  # Extract the installed binary to a temporary path; leave the TeX installation unchanged.
  thin_biber <- tempfile("ambiguity-biber-")
  arch <- if (Sys.info()[["machine"]] == "arm64") "arm64" else "x86_64"
  run_checked(unname(Sys.which("lipo")), c(shQuote(biber), "-thin", arch, "-output", shQuote(thin_biber)))
  Sys.chmod(thin_biber, "0755")
  status <- system2(thin_biber, "Ambiguity", stdout = "build_output.log", stderr = "build_output.log")
  unlink(thin_biber)
}
if (status != 0) stop("Biber failed. See build_output.log.")
run_checked(tex, tex_args)
run_checked(tex, tex_args)
log_lines <- readLines("Ambiguity.log", warn = FALSE)
if (any(grepl("undefined|Overfull|LaTeX Error", log_lines))) stop("Inspect LaTeX diagnostics before using PDF.")
cat("Rebuilt analysis and Ambiguity.pdf successfully.\n")

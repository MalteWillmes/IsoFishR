# Replace non-ASCII characters in R/*.R with \uXXXX escapes, which R CMD
# check requires. Run from the repository root:  Rscript dev/ascii_escape.R

for (f in Sys.glob("R/*.R")) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  y <- vapply(x, \(line) {
    chars <- strsplit(line, "")[[1]]
    codes <- vapply(chars, utf8ToInt, integer(1))
    chars[codes > 127] <- sprintf("\\u%04x", codes[codes > 127])
    paste(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  if (!identical(x, y)) {
    writeLines(y, f, useBytes = TRUE)
    message("Escaped non-ASCII characters in ", f)
  }
}

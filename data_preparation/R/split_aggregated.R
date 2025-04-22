split_aggregated <- function(x) {
  if (is.character(x) && grepl("\\.\\.", x[1])) {
    strsplit(x, "\\.\\.")
  } else {
    list(x)
  }
}

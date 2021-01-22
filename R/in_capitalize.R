capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

unordered_combination <- function(..., sep = " "){
  apply(cbind(...), FUN = function(x){paste0(x[order(x, na.last = NA)], collapse = sep)}, MARGIN = 1)
}

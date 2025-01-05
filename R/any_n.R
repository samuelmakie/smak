any_n <- function(x, n, any_symbol = "*", sep = "|"){
  n_any_symbol <- length(x) - n
  any_symbol_loc <- combn(1:length(x), m = n_any_symbol)
  xs <- list()
  for(i in 1:ncol(any_symbol_loc)){
    x_i <- x
    x_i[any_symbol_loc[,i]] <- any_symbol
    xs[[i]] <- x_i
  }
  gv_instruction <- paste0(sapply(xs, FUN = paste0, collapse = ""), collapse = sep)
  return(gv_instruction)
}


#' rbind
#'
#' @param ... Objects of class data.frame
#' @param col_names Names of the columns of the returned dataframe.
#' Defaults to NULL, which would create new column names in the form "col_n". It could take a character vector.
#' Column names could be inherited from a specific dataframe by specifying col_names as an integer indicating which dataframe from those provided by it's order or
#' by specifying a dataframe object the column names would be inherited from.
#'
#'
#' @param col_classes Class of the column vectors in the returned dataframe.
#' Defaults to NULL, which would create classes naturally.
#' It could take a character vector of coersion functions (e.g. c(as.character, as.factor, as.Date2)).
#' Classes could be inherited from a specific dataframe by specifying col_names as an integer indicating which dataframe from those provided by it's order or
#' by specifying a dataframe object the classes would be inherited from.
#'
#' @return An object of class data.frame
#' @export
#'
#' @examples
rbind <- function(..., col_names = NULL, col_classes = NULL){
  df_list <- list(...)

  col_names <- col_names_f(col_names, df_list)

  output <- lapply(df_list, FUN = function(x){setNames(x, col_names[1:length(x)])}) |>
    lapply(FUN = function(x){x[, col_names[-c(1:length(x))]] <- NA; return(x)}) |>
    do.call(what = rbind) |>
    col_classes_f(col_classes = col_classes, df_list = df_list)

  return(output)
}

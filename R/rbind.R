
#' rbind
#' @description A modified [base::rbind]. Binds dataframes by rows according to column order from the left (ignores column names, unlike `base::rbind`) while allowing standardising column names and classes.
#' @param ... One or more objects of class data.frame to be bound rowwise.
#' @param col_names  Column names of the returned dataframe.
#' Defaults to NULL, which creates new column names in the form "col_n".
#' Can be a character vector, an integer indicating the nth dataframe among those provided from which the column names are inherited, or a dataframe from which the column names are inherited.
#'
#' @param col_classes Class of the column vectors in the returned dataframe.
#' Defaults to NULL, which creates classes naturally.
#' Can be a vector of coercion functions (e.g. `c(as.character, as.factor, as.Date2))`, an integer indicating the nth dataframe among those provided from which the classes are inherited, or a dataframe from which the classes are inherited.
#' Note that issues will likely arise when dealing with dates. Try [as.Date2] if it didn't work as expected, otherwise, user defined functions can be used (e.g. `c(as.character, function(x){as.Date(x, origin = "1970-01-01")})`).

#' @return An object of class data.frame
#' @export
#' @details Can be used for other goals, particularly standardising column names and/or classes among multiple dataframes
#' @examples
rbind <- function(..., col_names = NULL, col_classes = NULL){

  df_list <- list(...)

  col_names <- rbind_col_names(col_names, df_list)

  output <- lapply(df_list, FUN = function(x){setNames(x, col_names[1:length(x)])}) |>
    lapply(FUN = function(x){x[, col_names[-c(1:length(x))]] <- NA; return(x)}) |>
    do.call(what = base::rbind) |>
    rbind_col_classes(col_classes = col_classes, df_list = df_list)

  return(output)

}

# Helpers ----------------------------------------------------------------------------------------------------

# rbind_col_names
# Different methods for different inputs for the col_names argument in the rbind function
 rbind_col_names <- function(col_names, df_list){
   UseMethod("rbind_col_names")
 }

 rbind_col_names.NULL <- function(col_names, df_list){
   paste0("col_", 1:max(unlist(lapply(df_list, FUN = function(x){length(x)}))))
 }

 rbind_col_names.data.frame <- function(col_names, df_list){
   c(names(col_names), paste0("col_", 1:max(unlist(lapply(df_list, FUN = function(x){length(x)}))))[-c(1:length(names(col_names)))])
 }

 rbind_col_names.numeric <- function(col_names, df_list){
   c(names(df_list[[col_names]]), paste0("col_", 1:max(unlist(lapply(df_list, FUN = function(x){length(x)}))))[-c(1:length(names(df_list[[col_names]])))])
 }

rbind_col_names.character <- function(col_names, df_list){
  c(col_names, paste0("col_", 1:max(unlist(lapply(df_list, FUN = function(x){length(x)}))))[-c(1:length(col_names))])
}


# rbind_col_classes
# Different methods for different inputs for the col_classes argument in the rbind function

rbind_col_classes <- function(col_classes, df_list, output){
  UseMethod("rbind_col_classes")
}

rbind_col_classes.NULL <- function(col_classes, df_list, output){
  return(output)
}

rbind_col_classes.list <- function(col_classes, df_list, output){
  output[, c(1:length(col_classes))] <- eval(parse(text = paste0("list(", paste0(paste0("col_classes[[", 1:length(col_classes), "]](output[ ,", 1:length(col_classes), "])"), collapse = ", "), ")")))
  return(output)
}

rbind_col_classes.numeric <- function(col_classes, df_list, output){
  col_classes <- gsub("Date", "Date2", sapply(df_list[[col_classes]], class))
  output[ ,c(1:length(col_classes))] <- eval(parse(text = paste0("list(", paste0(paste0("as.", col_classes, "(output[, ", 1:length(col_classes), "])"), collapse = ", "), ")")))
  return(output)
}

rbind_col_classes.data.frame <- function(col_classes, df_list, output){
  col_classes <- gsub("Date", "Date2", sapply(col_classes, class))
  output[ ,c(1:length(col_classes))] <- eval(parse(text = paste0("list(", paste0(paste0("as.", col_classes, "(output[, ", 1:length(col_classes), "])"), collapse = ", "), ")")))
  return(output)
}








































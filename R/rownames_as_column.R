rownames_as_column <- function(df, col_name = "var"){
  col <- data.frame(row.names(df))
  names(col) <- col_name
  df <- cbind(col, df)
  row.names(df) <- NULL
  return(df)
}

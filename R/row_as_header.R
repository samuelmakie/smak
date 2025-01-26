row_as_header <- function(df, row = 1, delete_row = T){
  names(df) <- as.data.frame(lapply(df, FUN = function(x){as.character(x)}))[row, ]
  if(delete_row){
    df <- df[-c(row),]
  }
  return(df)
}

# Designed for turning the enemy matrix into a list of dataframes by enemy status where each dataframe is a list of enemies for all items
# All base R (no package dependencies)
colnames_as_value <- function(df){
  # Data prep
  ## Get variable name
  varname <- names(df)[1]
  ## Convert data.frame to matrix while retaining column 1 as rownames
  rownames(df) <- df[,1]
  df <- df[,-1]
  df <- as.matrix(df)
  ## Assign NA to cells with no non-blank values (assuming blanks are NAs)
  df[apply(gsub("[[:space:]]", "", df), MARGIN = c(1, 2), FUN = function(x){nchar(x) == 0 & !is.na(x)})] <- NA


  ## Main function
  # Get unique cell values
  unique_values <- na.omit(unique(as.vector(df)))
  # For each unique value and for each level of the variable, get all corresponding level of the variable
  d2 <- lapply(unique_values, FUN = function(unique_values){apply(df, MARGIN = 1, FUN = function(df){names(na.omit(df[df == unique_values]))})})
  # Name the first level list according to the unique value
  names(d2) <- unique_values
  # Embed max length for each unique value in the same object (for a second lapply below)
  d2 <- lapply(d2, FUN = function(d2){list(x = d2, max = max(unlist(lapply(d2, FUN = function(d2){length(d2)}))))})

  # For the list of corresponding levels of the variable for each unique value and for each level of the variable, add the NAs to the "missing" slots, so that the list of corresponding levels of the variable can be turned into a data.frame for each unique value, then turn them into data.frames
  d2 <- lapply(d2, FUN = function(d2){smak::rownames_as_column(as.data.frame(t(as.data.frame(lapply(d2$x, FUN = function(x){c(x, rep(NA, d2$max - length(x)))})))), varname)})
  return(d2)
}



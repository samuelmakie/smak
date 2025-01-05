cut_sentence <- function(string, max_nchar = NULL, max_nwords = NULL, word_separator = " ", string_end = "..."){
  #string <- c("Psychological", "Hello I am trying to improve my function", "")
  #max_nchar <- NULL
  #max_nwords <- 2
  #string_end <- "..."
  #word_separator <- " "
  string_original <- string
  string <- paste0(gsub(" {2,}", " ", string), " ")
  if(!is.null(max_nchar) && !is.null(max_nwords)){
    stop("Sentences cannot be cut by both max_nhar and max_nwords at the same time. Use one or the other.")
  }
  if(is.null(max_nchar) && is.null(max_nwords)){
    stop("Either max_nchar or max_nwords is required")
  }

  if(!is.null(max_nchar)){
    end_position <- sapply(gregexpr(word_separator, string), FUN = function(x){ifelse(length(x[x <= max_nchar]) == 0, max_nchar, max(x[x <= max_nchar]) - 1)})
    end_position <- ifelse(end_position < 0, max_nchar, end_position)
  }
  if(!is.null(max_nwords)){
    end_position <- sapply(gregexpr(" ", string), FUN = function(x){ifelse(length(c(0, x)) >= max_nwords + 1, max(0, c(0, x)[max_nwords + 1] - 1), ifelse(max_nwords == 0, 0, max(x) - 1))})
  }
  string <- substring(string, 0, end_position)
  string <- ifelse(string != string_original, paste0(string, string_end), string)
  return(string)
}


#' as.Date2
#' @description A modified [base-as.Date] such that the function still runs and Nreturns NA if coercion wasn't successful.
#' @param x A vector
#' @param format See [as.Date]
#' @param tryFormats See [as.Date]
#'
#' @return A date vector
#' @export
#'
#' @examples
as.Date2 <- function(x, format, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y")){
  if(is.logiccal(x)){
    output <- NA
  }else{
    output <- as.Date(x, format, tryFormats, optional = T)
  }
}

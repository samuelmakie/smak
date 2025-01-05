to_string <- function(..., sep = " ", reorder = F, decreasing = F, method = "auto"){
  order2 <- function(x, reorder, decreasing, method){
    if(reorder){
      order(x, decreasing = decreasing, method = method, na.last = NA)
    }else{
      1:length(x)
    }
  }
  apply(cbind(...), FUN = function(x){paste0(x[order2(x = x, reorder = reorder, decreasing = decreasing, method = method)], collapse = sep)}, MARGIN = 1)
}

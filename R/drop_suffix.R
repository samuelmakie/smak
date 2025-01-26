drop_suffix_1 <- function(x, group_nchar){
  group_of_x <- substring(x, 0, group_nchar)
  tail_of_x <- substring(x, group_nchar + 1, length(x))
  groups <- unique(group_of_x)
  group_index_by_group <- setNames(lapply(groups, FUN = function(x){which(group_of_x == x)}), groups)
  tail_of_x_by_group <- lapply(group_index_by_group, FUN = function(x){as.list(unique(tail_of_x[x]))})
  tail_of_x_by_group <- setNames(lapply(names(tail_of_x_by_group), FUN = function(a){setNames(list(tail_of_x_by_group[[a]]), a)}), groups)
  correction <- lapply(tail_of_x_by_group, FUN = function(a){lapply(a, FUN = function(b){lapply(b, FUN = function(c){list(tail = c, position =  which(x == paste0(names(a), c)), amendment = ifelse(length(b) > 1, paste0(names(a), c), names(a)))})})})
  correction <- unlist(unlist(correction, recursive = F), recursive = F)
  correction <- do.call(base::rbind,lapply(correction, FUN = function(k){
    data.frame(position = k$position, amendment = rep(k$amendment, length(k$position)))
  }))
  return(correction[order(correction$position),"amendment"])
}

drop_suffix <- function(x, format){
  group_position_in_x <- suppressWarnings(as.numeric(format))
  n_loops <- length(na.omit(group_position_in_x))
  group_position_in_x[which(is.na(group_position_in_x))] <- nchar(format[is.na(group_position_in_x)])
  group_position_in_x <- cumsum(group_position_in_x)
  for(i in 1:n_loops){x <- setNames(unlist(lapply(list(x = x), FUN = drop_suffix_1, group_nchar = group_position_in_x[(2*i - 1)]), recursive = F), NULL)}
  return(unlist(x))
}

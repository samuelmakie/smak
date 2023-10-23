
#' comb_vars
#'
#' @description
#' Combines multiple variables into one factor variable with level orders being preserved (ascendingly and nested)
#' @param data A table of class data.frame
#' @param vars Variables to be combined. Takes in a list of variables to generalise to multiple combinations with element names taken as names for the combined variables.
#' @param sep Separator for the combined values
#' @param drop Logical, specifies whether variables in the 'vars' argument are dropped
#'
#' @return A manipulated table of class data.frame with the combined variables on the right
#' @export
#'
#' @examples #Make some data
#' son_sep_23 <- readr::read_csv("https://assets.publishing.service.gov.uk/media/65265dd7aea2d0000d219b3e/Management_information_-_state-funded_schools_-_latest_inspections_at_30_Sep_2023.csv") |>
#' janitor::clean_names() |>
#' as_tibble()
#'
#' # Turn variables of interest to the desired data types for demonstration
#' son_sep_23_2 <- son_sep_23 |>
#'   select(ofsted_phase, ofsted_region, total_number_of_pupils, overall_effectiveness, publication_date, safeguarding_is_effective) |>
#'   mutate(ofsted_phase = factor(ofsted_phase, c("Nursery", "Primary", "Secondary", "PRU", "Special")),
#'          ofsted_region = as.character(ofsted_region),
#'          total_number_of_pupils = as.numeric(total_number_of_pupils),
#'          overall_effectiveness = as.character(overall_effectiveness),
#'          publication_date = as.Date(publication_date, tryFormats = "%d/%m/%Y"),
#'          safeguarding_is_effective = ifelse(safeguarding_is_effective == "Yes", T, F))
#'
#' # Induce some NAs randomly
#' {son_sep_23_2$ofsted_phase[sample(nrow(son_sep_23_2), size = 100)] <- NA
#'   son_sep_23_2$overall_effectiveness[sample(nrow(son_sep_23_2), size = 100)] <- NA
#'   son_sep_23_2$ofsted_region[sample(nrow(son_sep_23_2), size = 100)] <- NA
#'   son_sep_23_2$total_number_of_pupils[sample(nrow(son_sep_23_2), size = 100)] <- NA
#'   son_sep_23_2$publication_date[sample(nrow(son_sep_23_2), size = 100)] <- NA
#'   son_sep_23_2$safeguarding_is_effective[sample(nrow(son_sep_23_2), size = 100)] <- NA}
#'
# # comb_vars(data = son_sep_23_2, vars = list(var_1 = c('ofsted_phase', 'overall_effectiveness'), var_2 = c('ofsted_region', 'safeguarding_is_effective')))
comb_vars <- function(data, vars, sep = " ", drop = F){

  # Change variables to factors and relevel them
  ## Unique combinations in order
  unique_combination_ordered <- lapply(vars,FUN = function(x)unique(data[,c(x)])) |>
    # Ordered unique combinations
    lapply(FUN = function(k){eval(parse(text = paste0("k[order(",paste0("k$", names(k), collapse = ", "), "),]")))}) |>
    #Combine names
    lapply(FUN = function(l){eval(parse(text= paste0("paste(", paste0(paste0("l$", names(l)), collapse = ", "), ", sep = sep)")))}) |>
    lapply(FUN = function(m){paste0("c(", paste0("'",m, "'",collapse = ","), ")")})

  # Manipulate the data
  n <- lapply(vars, FUN = function(vars){paste0("paste(", paste0( paste0("data$", vars), collapse = ", "), ", sep = sep)")})
  data[, c(names(vars))] <- eval(parse(text = paste0("list(", paste(paste0("factor(",n, ",",unique_combination_ordered, ")"), collapse  = ", "), ")")))

  #Drop old columns depending on user
  if(drop){
    data <- data[,!(names(data) %in% unlist(vars))]
  }

  return(data)
}

flexmirt_model_spec <- function(ig, group_col, item_col, ncat_col, groups, group_n, observation_id, stem){
  # Data prep
  col <- match(c(deparse(substitute(group_col)), deparse(substitute(item_col)), deparse(substitute(ncat_col))), names(ig))
  # Subset data.frame and reorder columns to g then i
  ig <- ig[,col]
  ## Rename
  names(ig) <- c("g", "i", "n_cat")
  ## Change group level names and convert to factor
  if(length(groups) != length(unique(ig$g))) stop("The number of groups doesn't match the number of groups in the data")
  groups2 <- paste0("G", 1:length(groups))
  ig[,"g"] <- factor(paste0("G", as.integer(factor(ig[,"g"], groups))), groups2)
  ## Create columns
  if(length(observation_id) == 1){
    ig[, "observation_id"] <- observation_id
  }else if(length(observation_id) == length(levels(ig$g))){
    ig[, "observation_id"] <- observation_id[as.integer(ig$g)]
  }else{
    stop("Number of observation ids doesn't match with number of groups")
  }
  if(length(group_n) == 1){
    ig[, "n_observation"] <- group_n
  }else if(length(group_n) == length(levels(ig$g))){
    ig[, "n_observation"] <- group_n[as.integer(ig$g)]
  }else{
    stop("Number of group_n doesn't match with number of groups")
  }

  # Project section
  project_section <- paste0('<Project>\nTitle = "', stem, '";\nDescription = "', stem, '";\n\n')

  # Controls (these are be fixed for now)
  controls_section <- "<Options>\nMode = Calibration;\nSavePRM=Yes;\nSaveSco=Yes;\nScore=EAP;\nMaxE=10000;\nMaxM=2000;\n\n"

  # Groups section
  section_header <- paste0("<Groups>\n\n\n")
  groups_section_g <- lapply(split(ig, ig$g), FUN = function(x){
    subsection_header_g <- paste0("%", as.character(unique(x$g)),"%\n")
    csv_file_g <- paste0("File = ",'"',stem, as.character(unique(x$g)), '.csv";\n')
    ## Varnames and select
    vars_select_g <- paste0(c(paste0("Varnames = ", unique(x$observation_id), ", "), "Select = "),paste0(paste0(x$i, collapse = ", "), ";\n"))
    # case id and sample size
    caseid_n_g <- paste0("CaseID = ", unique(x$observation_id), ";\nN = ", unique(x$n_observation), ";\n")
    ## Ncat and model
    ncat_model_g <- lapply(split(x, x$n_cat), FUN = function(y){
      ncat_model_g <- paste0(
        paste0(c("Ncats(", "Model("),paste0(y$i, collapse = ", ")),
        paste0(") = ", c("", "graded("), unique(y$n_cat), c(";", ");"), "\n")
      )
    }) |> unlist() |> paste0(collapse = "")
    groups_section <- paste0(c(subsection_header_g, csv_file_g, vars_select_g, caseid_n_g, ncat_model_g, "\n"), collapse = "")
    return(groups_section)
  }) |> unlist() |> paste0(collapse = "")
  groups_section <- paste0(c(section_header, groups_section_g), collapse = "")

  # Constraints section
  ## Free means and covariance for focal groups
  constraints_group <- paste0("<Constraints>\n", paste0(as.vector(t(outer(paste0("Free G", 2:length(levels(ig$g)), ", "), c("Mean(1);\n", "Cov(1,1);\n"), paste0))), collapse = ""), "\n\n")

  ## Item parameter invariance for common items
  s <- lapply(split(ig, ig$i), FUN = function(x){list(g = as.character(x$g[order(unique(x$g))]), n_cat = unique(x$n_cat[order(unique(x$g))]))})
  issue_diff_n_cat <- which(lapply(s, FUN = function(x){length(x$n_cat)}) > 1)
  if(length(issue_diff_n_cat) > 0) stop( paste0("Common items across groups cannot have a different number of categories. Please check for the following items:\n", paste0(names(s)[issue_diff_n_cat], collapse = "\n")))
  ### Max groups
  max_length <- max(unlist(lapply(s, FUN = function(x){length(x$g)})))
  s <- lapply(s, FUN = function(x){c(x$g, rep(NA, max_length-length(x$g)), x$n_cat)})
  s <- rownames_as_column(as.data.frame(do.call(base::rbind, s)), "i")
  s[,"i"] <- factor(s[,"i"], unique(ig$i))
  s[,2:(max_length + 1)] <- lapply(s[,2:(max_length + 1)], FUN = function(x){factor(x, groups2)})
  s[,length(s)] <- unlist(lapply(s[,length(s)], FUN = function(x){as.numeric(x)}))
  s <- comb_vars(s, list("l" = paste0("V", 1:(max_length + 1))))
  s[,"l"] <- as.integer(s[,"l"])
  s <- setNames(split(s, s$l), NULL)
  s <- lapply(s, FUN = function(x){
    list(
      g = as.vector(na.omit(t(unique(x[,2:(max_length + 1)])))),
      i = as.character(x$i)[order(x$i)])
  })
  s <- s[which(unlist(lapply(s, FUN = function(x){length(x$g)})) > 1)]
  ### Convert to a string
  s <- lapply(s, FUN = function(x){
    item_set <- paste0(x$g, ", (",paste0(x$i, collapse = ", "), ")")
    paste0(paste0("Equal ", paste0(paste0(item_set, ", Intercept"), collapse = ":\n"), ";\n"), paste0("Equal ", paste0(paste0(item_set, ", Slope"), collapse = ":\n"), ";\n\n") )
  }) |> unlist() |> paste0(collapse = "")
  constraints_item <- s

  # Combine sections
  syntax <- paste0(c(project_section,controls_section, groups_section, constraints_group, constraints_item), collapse = "")
  return(syntax)
}

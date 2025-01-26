# Function: generate_variants
generate_variants <- function(instruction){

  w <- strsplit(gsub("\\|(?![^()]*\\))", "$", instruction, perl = T), "\\$")[[1]]

  # Additives with brackets by main
  w_additives_with_brackets <- regmatches(w, gregexpr("(?=\\().*?(?<=\\))", w, perl = T))

  # Extract main
  w_main <- lapply(w, FUN = function(x){gsub("\\s+", " ", gsub("\\([^()]*\\)", "", x))})

  # Find bracket position
  w_additives_modified_2 <- lapply(w_additives_with_brackets, FUN = function(x){paste0(gsub("\\(", "\\\\\\(", gsub("\\|", " ", x)), collapse = "|")})

  # Make place holder for brackets
  w_2 <- c()
  for(k in 1:length(w)){
    if(w_additives_modified_2[[k]] == ""){
      w_2[k] <- w[k]
    }else{
      w_2[k] <- gsub(w_additives_modified_2[[k]], "~", gsub("\\|", " ", w[k]))
    }
  }

  # Extract additives
  w_additives <- lapply(w_additives_with_brackets, FUN = function(x){strsplit(gsub("\\(|\\)", "", gsub("\\(", "\\(|", x)), "\\|")})
  # Find first ~ and replace with the corresponding level
  # Make list
  variant_id <- list()
  variant_list <- list()
  for(m in 1:length(w_additives)){
    variant_list_m <- list()
    for(b in 1:length(w_additives[[m]])){
      if(length(w_additives[[m]]) == 0) next
      variant_list_m[[b]] <- 0:(length(w_additives[[m]][[b]]) - 1)
    }
    variant_list[[m]] <- variant_list_m

    variant_id[[length(variant_id) + 1]] <- as.matrix(expand.grid(variant_list[[m]]))
  }

  variants <- list()

  for(m in 1:length(variant_id)){
    variants_m <- c()

    if(nrow(variant_id[[m]]) == 0){
      variants_m[length(variants_m) + 1] <- w_2[m]
    }else{
      for(v in 1:nrow(variant_id[[m]])){
        w_main_loop <- w_2[m]

        for(b in 1:length(w_additives[[m]])){
          a <- w_additives[[m]][[b]][variant_id[[m]][v,][b] + 1]
          w_main_loop <- sub("~", a, w_main_loop)
        }
        variants_m[length(variants_m) + 1] <- w_main_loop
      }
    }
    variants[[m]] <- variants_m
  }

  variants <- lapply(variants, FUN = function(x){trimws(sub("$$", " ", sub("^(.{0})(.)", "\\1 \\2", gsub("\\s{2,}", " ", trimws(x)))))})

  return(variants)

}

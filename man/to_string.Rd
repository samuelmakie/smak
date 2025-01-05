\name{to_string}
\alias{to_string}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
to_string
}
\description{
Takes a set of vectors and combine the nth element across the set of vectors as a string with the option to reorder the elements in the string.
}
\usage{
to_string(..., sep = " ", reorder = F, decreasing = F, method = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
A list of vectors of the same length.
}
  \item{sep}{
A string or a numeric placed between the elements (as a separator) in the string. The default is " ".
}
  \item{reorder}{
A logical for specifying whether the elements in the string are to be reordered. If FALSE, the order accords with the order of the vector input. If TRUE, the elements are reordered by \link[base:order]{base::order} and input to its arguments is passed by the aruments \code{decreasing} and \code{method}
}

}

\value{
A character vector of the combinations
}

\examples{
# Create an example data frame
survey <- as.data.frame(lapply(MASS::survey, FUN = as.character))

# Induce some NAs
survey$Sex[sample(1:nrow(survey), 0.3*nrow(survey))] <- NA
survey$Smoke[sample(1:nrow(survey), 0.3*nrow(survey))] <- NA
survey$Exer[sample(1:nrow(survey), 0.3*nrow(survey))] <- NA

# Combine the nth element in the vectors representing the columns Sex, Smoke, and Exer as a string in the order of Sex, followed by Smoke, and followed by Exer
to_string(survey$Sex, survey$Smoke, survey$Exer)

# In addition, the elements are to be separated by "|"
to_string(survey$Sex, survey$Smoke, survey$Exer, sep = "|")

# In addition, the elements are to be reordered and in decending order
to_string(survey$Sex, survey$Smoke, survey$Exer, sep = "|", reorder = T, decreasing = F)

# As this function is vectorised, it an be used within dplyr::mutate
survey |> mutate(combination = to_string(Sex, Smoke, Exer, sep = "|", reorder = T, decreasing = F))

# Elements with a value of NA are shown as "NA" in the resulting string, how NAs are managed is not within the scope of the function. To manage how they are shown, convert NAs to the desired value before using the function
# E.g. To show NAs as "-" for the Sex variable in the resulting string
# Replace NAs with "-" for the Sex variable
survey$Sex <- ifelse(is.na(survey$Sex), "-", survey$Sex)
# Then use the function normally
to_string(survey$Sex, survey$Smoke, survey$Exer)
}


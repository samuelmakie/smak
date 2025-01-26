\name{unordered_combination}
\alias{unordered_combination}
\title{unordered_combination}
\description{
Takes a set of vectors and concatenate values across the vectors as a string such that the resulting strings are order-ignorant combinations
}
\usage{unordered_combination(..., sep = " ")}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Vectors}
  \item{sep}{A seperator between the values in the resulting string}}
\details{This is designed for the use in factor analysis where counter-balanced booklet combinations are treated as the same combination.}
\value{A character vector}

\examples{
# Set up the example data
data <- as.data.frame(apply(as.data.frame(Titanic), MARGIN = 2, FUN = as.character))

# Concatenate columns Class, Sex, and Age as a string of the values in ascending order so that the resulting string is order-ignorant
unordered_combination(data$Class, data$Sex, data$Age, sep = "-")
}


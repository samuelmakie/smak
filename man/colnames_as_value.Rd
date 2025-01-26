\name{colnames_as_value}
\alias{colnames_as_value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
colnames_as_value
}
\description{
Puts column names to cells and shifting all values to the left by original cell value
}
\usage{
colnames_as_value(df)
}
\arguments{
  \item{df}{
An n by 1 + n matrix as an object of class \code{data.frame} where the first column is the rownames.
}
}
\details{
Puts column names to cells in the \code{data.frame} and shifts all new cell values to the left (ignoring NAs) by original cell value.
This is useful for looking at the presence of pairing with the rest of the variables for each variable by their relationship as represented by the original cell values (which is most useful when there are only a few original cell values, i.e. categorical).

This was designed to be a generalisation of "enemisation".

Currently the function only works when there are more than 1 cell value, amendments to include the case when there is only 1 value may be made, but it only makes sense to use this function when there are more than 1 cell value.
}
\value{
A list of dataframes where each is for each original cell value and has a dimension of n by the maximum number of pairing for that cell value.
}

\examples{
bfi_cormat <- cor(data("psych_bfi") use = "pairwise.complete.obs")
bfi_cormat <- ifelse(bfi_cormat > 0 ,"Positive", "Negative") |>
  as.data.frame() |>
  rownames_as_column("Var")

colnames_as_value(bfi_cormat)
}


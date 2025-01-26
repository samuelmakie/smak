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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
An n by 1 + n matrix where the first column is the rownames in the form of an object of class \code{data.frame}
}
}
\details{
Puts column names to cells in the \code{data.frame} and shifts all cell values to the left (ignoring NAs) by original cell value.
This is useful for looking at the presence of other variables for each variable by their relationship as represented by the original cell values (which is most useful when there are only a few original cell values, i.e. categorical).

This was designed to be a generalisation of "enemisation".

Currently the function only works when there are more than 1 cell value, amendments to include the case when there is only 1 value may be made, but it only makes sense to use this function when there are more than 1 cell value.
}
\value{
A list of dataframes where each is for each original cell value and has a dimension of n by the maximum number of pairing for that cell value.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
bfi_cormat <- cor(data("psych_bfi") use = "pairwise.complete.obs")
bfi_cormat <- ifelse(bfi_cormat > 0 ,"Positive", "Negative") |>
  as.data.frame() |>
  rownames_as_column("Var")

colnames_as_value(bfi_cormat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

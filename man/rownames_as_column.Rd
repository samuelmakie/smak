\name{rownames_as_column}
\alias{rownames_as_column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rownames_as_column
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rownames_as_column(df, col_name = "var")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
An object of class \code{data.frame}
}
  \item{col_name}{
Optional. Name of the column. Defaults to "var".
}
}
\details{
Converts rownames in a data.frame as a column. This function is the same as \link[tibble:rownames_to_column]{tibble::rownames_to_column}.
}
\value{
An object of class \code{data.frame}
}


\examples{
# Convert the rownames as a column in the data frame and name this column "Car"
rownames_as_column(mtcars, "Car")
}


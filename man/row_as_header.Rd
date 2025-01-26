\name{row_as_header}
\alias{row_as_header}
\title{row_as_header}
\description{
Converts a specified row in a dataframe as its header.
}
\usage{
row_as_header(df, row = 1, delete_row = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{An object of class \code{data.frame} for the misread dataframe to be amended}
  \item{row}{The index of the row that is meant to be the header. Defaults to 1 as this is most likely the case.}
  \item{delete_row}{Logical. If \code{TRUE}, it removes the row for the header, otherwise, it is retained. Defaults to \code{TRUE}.}
}
\details{
Converts a specified row in a dataframe as its header with the option to remove/retain the row.
}
\value{An object of class \code{data.frame}}

\examples{
# Set up a misread dataframe to be transformed
df <- data.frame(id = NA)
colnames <- names(mtcars)
for(i in colnames){df[, colnames] <- colnames}
df <- setNames(base::rbind(df[,-1], mtcars), paste0("V", 1:length(colnames)))

# Sometimes a data file is read incorrectly where the header is read as a row and the column names are replaced with generic ones
df

# Convert row 1 (which is meant to be the header) as the header and drop row 1 (because it is not meant to be the cell values)
row_as_header(df = df, row = 1, delete_row = T)
}
=

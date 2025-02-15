

\name{rbind}
\alias{rbind}
\title{rbind}
\description{
A modified \link[base:cbind]{base::rbind}. Binds dataframes by rows according to column order from the left (ignores column names, unlike \code{base::rbind}) while allowing standardising column names and classes.
}
\usage{
rbind(..., col_names = NULL, col_classes = NULL)
}
\arguments{
\item{...}{One or more objects of class data.frame to be bound rowwise.}

\item{col_names}{Column names of the returned dataframe.
Defaults to NULL, which creates new column names in the form "col_n".
Can be a character vector, an integer indicating the nth dataframe among those provided from which the column names are inherited, or a dataframe from which the column names are inherited.}

\item{col_classes}{Class of the column vectors in the returned dataframe.
Defaults to NULL, which creates classes naturally.
Can be a vector of coercion functions (e.g. \verb{c(as.character, as.factor, as.Date2))}, an integer indicating the nth dataframe among those provided from which the classes are inherited, or a dataframe from which the classes are inherited.
Note that issues will likely arise when dealing with dates. Try \link{as.Date2} if it didn't work as expected, otherwise, user defined functions can be used (e.g. \code{c(as.character, function(x){as.Date(x, origin = "1970-01-01")})}).}
}
\value{
An object of class data.frame
}
\details{
Can be used for other goals, particularly standardising column names and/or classes among multiple dataframes.
}

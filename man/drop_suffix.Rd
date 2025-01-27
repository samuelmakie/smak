\name{drop_suffix}
\alias{drop_suffix}
\title{drop_suffix}
\description{Drops any unnecessary suffixes for names in the general format \code{name_1}\code{seperator_1}{name_2}{seperator_2}...\code{name_n}{seperator_n}}
\usage{drop_suffix(x, format)}
\arguments{
  \item{x}{A character vector where elements may have unnecessary suffixes to be dropped}
  \item{format}{The naming format of elements in \code{x} as a character vector. The names should follow the general format of \code{name_1}\code{seperator_1}{name_2}{seperator_2}...\code{name_n}{seperator_n} and is specified in the character vector as the number of characters in name_1, followed by seperator_1 as a string, followed by the number of characters in name_2, followed by seperator_2 as a string until seperator_n}
}


\examples{
x <- c(
  "ABC2000_01a", "ABC2000_01b", "ABC2000_02a", "ABC2000_02b", "ABC2000", "ABC2000_01",
  "ABC2001_01a", "ABC2001_02a","ABC2001_03a",
  "ABC2002_01",
  "ABC2003_01a",
  "ABC2004_01b",
  "ABC2005_01", "ABC2005_02", "ABC2005_03",
  "ABC2006_01a.01", "ABC2006_01b.01",
  "ABC2007_07",
  "ABC2008_01b.01", "ABC2008_01b.02",
  "ABC2009_01a.01", "ABC2009_01b.01",
  "ABC2010_01a.01",
  "ABC2011_01a.01",
  "ABC2012_01a.90",
  "ABC2012_01b.90",
  "ABC2012_01c.90",
  "ABC2012_01c.90")

drop_suffix(x = x, format = c(7, "_", 2, "", 1, "."))
}


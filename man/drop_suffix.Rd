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
  "G002000_01a", "G002000_01b", "G002000_02a", "G002000_02b", "G002000", "G002000_01",
  "G002001_01a", "G002001_02a","G002001_03a",
  "G002002_01",
  "G002003_01a",
  "G002004_01b",
  "G002005_01", "G002005_02", "G002005_03",
  "G002006_01a.01", "G002006_01b.01",
  "G002007_07",
  "G002008_01b.01", "G002008_01b.02",
  "G002009_01a.01", "G002009_01b.01",
  "G002010_01a.01",
  "G002011_01a.01",
  "G002012_01a.90",
  "G002012_01b.90",
  "G002012_01c.90",
  "G002012_01c.90")

drop_suffix(x = x, format = c(7, "_", 2, "", 1, "."))
}


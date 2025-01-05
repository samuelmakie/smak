\name{any_n}
\alias{any_n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
any_n
}
\description{
Takes a vector and returns a string of combinations of any n elements with others represented by an any symbol
}
\usage{
any(x, n, any_symbol = "*", sep = "|")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector
}
  \item{n}{
The number of elements
}
  \item{any_symbol}{
A character or numeric to symbolise the meaning of "any"
}
  \item{sep}{
A character or numeric placed between the combinations (as a separator)
}
}

\value{
A character string
}

\examples{
any_n(c(1, 2, 3, 4), 2, any_symbol = "*")
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

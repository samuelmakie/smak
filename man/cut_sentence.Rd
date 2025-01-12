\name{cut_sentence}
\alias{cut_sentence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cut_sentence
}
\description{
Cuts a sentence such that it has a specified maximum number of characters or words while retaining all characters in the last word in the truncated sentence
}
\usage{
cut_sentence(string, max_nchar = NULL, max_nwords = NULL, word_separator = " ", string_end = "...")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
A character vector of sentences
}
  \item{max_nchar}{
Maximum number of characters to retain. Either \code{max_nchar} or \code{max_nwords} needs to be specified.
}
  \item{max_nwords}{
Maximum number of words to retain. Either \code{max_nwords} or \code{max_nchar} needs to be specified.
}
  \item{word_separator}{
The word separator in the sentences. Defaults to " ".
}
  \item{string_end}{
Optional. A string added at the end of the sentence. Intended for indicating the sentence is truncated. Defaults to "...".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character vector of the same length as the input for the \code{string} argument
}


\examples{
x <- c("Big beats are the best, get high all the time", "I've mistakenly run to Windsor")
# Cut the sentence by maximum number of words
cut_sentence(string = x, max_nwords = 3)

# Cut the sentence by the maximum number of characters (this preserves the last word)
cut_sentence(string = x, max_nchar = 20)
}


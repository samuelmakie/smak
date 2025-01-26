\name{generate_variants}
\alias{generate_variants}
\title{generate_variants}
\description{Generates all possible variations given instruction}
\usage{generate_variants(instruction)}

\arguments{
  \item{instruction}{Instruction in a specific format}
}
\details{
Generate all variation given instruction. The instruction is just a string and alternatives are enclosed in brackets and seperated by "|". See example. This function was conceived due to the need to do frequent string search in my job in early 2024, I found it tedious to type all possible strings to be searched, so had the idea of making a function that could generate all possible strings for me, so I decided to do this, I then designed and implemented this in about 1 week's time (or maybe 2, I can't remember). I forgot about this function a week after making it and thought this is useless nevertheless a good experience and didn't include it here until later in 2024 I was in a situation where I found this useful.
}
\value{
A list of character vectors
}

\examples{
generate_variants("I might review this function when I (have time|go mad)|Currently it is (performing|working) (okay|as expected) but I don't know why 'nothing' is also included|I conceived, designed, and implemented this function in a week in (early|the first quarter of) 2024 and I think this is not bad given that I consider myself as a beginner at that time|Initially I didn't want to include this function in this package as it looks too ugly and seems useless, but I found it useful later in 2024, so I've decided to include it but I don't really want to improve it as it is hard to understand, good thing is that it is all base R - Meaning no dependencies!")
}


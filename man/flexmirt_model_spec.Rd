\name{flexmirt_model_spec}
\alias{flexmirt_model_spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{flexmirt_model_spec}
\description{
Generate IRT model specification for the flexMIRT software developed by \href{https://vpgcentral.com/}{Vector Psychometrics Group}.
}
\usage{
flexmirt_model_spec(ig, group_col, item_col, ncat_col, groups, group_n, observation_id, stem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ig}{A data.frame of items as rows and their info as columns in long data format (i.e. Repeated item rows across groups in Multiple Groups). Necessary info are the group and the number of categories}
  \item{group_col}{The name of the group column in ig specified as an object (without quotes)}
  \item{item_col}{The name of the item column in ig specified as an object (without quotes)}
  \item{ncat_col}{The name of the number of categories column in ig specified as an object (without quotes)}
  \item{groups}{A character vector of group names in the order for the model}
  \item{group_n}{A numerical vector of group sample sizes in the same order as the order of the groups specified in \code{groups}. If a single value is specified, it is taken that all groups have the same sample size.}
  \item{observation_id}{A character vector of observation identifier column names for the groups in the same order as the order of the groups specified in \code{groups}. If a single character string is specified, it is taken that the observation identifier column name is the same across groups.}
  \item{stem}{A character string that determines the title, description and csv file names in the model specification}
}
\details{
Currently only applicable to the Unidimensional Single or Multiple-Group 2-Parameter Graded Response Logistic model. For the Multiple-Group model, items having the same label are taken to be the same item and are specified as anchor items across all relevant groups.
}
\value{A character string}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{# Generate items and their random group assignments
set.seed(72)

# Number of items and groups
num_items <- 100
num_groups <- 10
# Create the item-group data structure
item_group <- data.frame(
  item = rep(paste0("item_", 1:num_items), each = 3),
  group = sample(paste("G", 1:num_groups, sep=""), 3 * num_items, replace = TRUE)
)
# Assign random categories between 2 and 4 for each item
categories <- sample(2:4, num_items, replace = TRUE)
# Add a column for the number of categories, ensuring consistency across groups for the same item
item_group$ncat <- rep(categories, each = 3)

# Look at the data
head(item_group, 10)

syntax <- flexmirt_model_spec(
  ig = item_group,
  item_col = item,
  group_col = group,
  ncat_col = ncat,
  groups = paste0("G", 1:num_groups),
  group_n = 100, # All groups have the same sample size
  observation_id = paste0("respondent_id", 1:num_groups), # Different observation identifier column name across groups
  stem = "example"
)
# See it as text
syntax |> cat()

# Write syntax to txt file
# writeLines(syntax, "flexmirt_syntax.txt")
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

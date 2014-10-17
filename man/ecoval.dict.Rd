\name{ecoval.dict}

\alias{ecoval.dict}

\title{
Extracts a Single Dictionary from a Table of Dictionaries
}

\description{
Extracts a dictionary column from a matrix or data frame of dictionaries (translated words) with the languages provided by the column names and the words in the original language provided in the first column.
}

\usage{
ecoval.dict(language,dictionaries=NA)
}

\arguments{
  \item{language}{
A single word specifying the desired language.
}
  \item{dictionaries}{
Matrix or data frame of dictionaries with the languages provided by the column names and the original words provided in the first column.
If dictionary is NA, the default dictionary \code{ecoval.dictionaries.default} is loaded.
}
}

\value{
Vector of translated words labelled by the words in the original language.
}

\seealso{
\code{\link{ecoval.translate}}.
}


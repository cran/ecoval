\name{ecoval.translate}

\alias{ecoval.translate}

\title{
Translates a Word Given a Dictionary
}

\description{
Translates a word given as the first argument using a dictionary provided as the second argument of the function.
The dictionary consists of a character vector of translated words labelled by the words in the original language.
If no translation is found, the given word in the original language is returned.
}

\usage{
ecoval.translate(word,dictionary)
}

\arguments{
  \item{word}{
A single word to be translated.
}
  \item{dictionary}{
Vector of translated words labelled by the words in the original language.
}
}

\value{
The function returns the translated word.
}

\seealso{
\code{\link{ecoval.dict}}.
}

\examples{
ecoval.translate("x",c(x="X",y="Y",z="Z"))
}


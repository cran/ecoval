\name{val.spear.create}

\alias{val.spear.create}

\title{
Creates a Value Function for River Spear Index
}

\description{
Creates a value function for the SPEARpesticides index in rivers.
}

\usage{
val.spear.create(language     = "English",
                 dictionaries = NA,
                 col          = "black")
}

\arguments{
  \item{language}{
Language to be used to denote nodes, attributes and attribute levels.
Must be a column name of the table provided by the second argument.
}
  \item{dictionaries}{
Matrix of dictionaries with the languages provided by the column names and the original words provided as the row names.
If dictionary is NA, the default dictionary \code{ecoval.dictionaries.default} is loaded.
}
  \item{col}{
Color of bounding boxes in objectives hierarchy.
}
}

\value{
The function returns the value function as a class utility.
}

\references{
Langhans, S.D., Lienert, J., Schuwirth, N. and Reichert, P. How to make river assessments comparable: A demonstration for hydromorphology, Ecological Indicators 32, 264-275, 2013. \cr\cr
Langhans, S.D., Reichert, P. and Schuwirth, N. The method matters: indicator aggregation in ecological river assessment. Ecological Indicators 45, 494-507, 2014. \cr\cr
Reichert, P., Schuwirth, N. and Langhans, S. Constructing, evaluating and visualizing value and utility functions for decision support, Environmental Modelling & Software 46, 283-291, 2013. \cr\cr
Reichert, P., Langhans, S., Lienert, J. and Schuwirth, N. The conceptual foundation of environmental decision support. Journal of Environmental Management . In review. \cr\cr
Reichert, P., Borsuk, M., Hostmann, M., Schweizer, S., Sporri, C., Tockner, K. and Truffer, B., Concepts of decision support for river rehabilitation, Environmental Modelling and Software 22, 188-201, 2007.\cr\cr
Beketov M.A., Foit K., Schafer R.B., Schriever C.A., Sacchi A., Capri E., Biggs J., Wells C. & Liess M. SPEAR indicates pesticide effects in streams - Comparative use of species- and family-level biomonitoring data. Environmental Pollution, 157, 1841-1848, 2009.
}

\seealso{
\code{\link[utility:utility-package]{utility}}.
}

\examples{
spear <- val.spear.create()
plot(spear)
spear.german <- val.spear.create("Deutsch")
plot(spear.german)
}


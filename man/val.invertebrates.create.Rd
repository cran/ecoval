\name{val.invertebrates.create}

\alias{val.invertebrates.create}

\title{
Creates an Integrative Value Function for River Invertebrates
}

\description{
Creates a value function for river invertebrates integrating macroinvertebrate indices for organic matter pollution and toxicity.
}

\usage{
val.invertebrates.create(language     = "English",
                         dictionaries = NA,
                         col          = "black",
                         modify       = TRUE)
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
\item{modify}{
Use the biological indicators Makroindex and IBGN in addition to IBCH. Default is TRUE.
}
}

\value{
The function returns the value function as a class utility.
}

\references{
Langhans, S.D., Lienert, J., Schuwirth, N. and Reichert, P. How to make river assessments comparable: A demonstration for hydromorphology, Ecological Indicators 32, 264-275, 2013. \cr\cr
Langhans, S.D., Reichert, P. and Schuwirth, N. The method matters: indicator aggregation in ecological river assessment. Ecological Indicators 45, 494-507, 2014. \cr\cr
Reichert, P., Schuwirth, N. and Langhans, S. Constructing, evaluating and visualizing value and utility functions for decision support, Environmental Modelling & Software 46, 283-291, 2013. \cr\cr
Reichert, P., Langhans, S., Lienert, J. and Schuwirth, N. The conceptual foundation of environmental decision support. Journal of Environmental Management. In review. \cr\cr
Reichert, P., Borsuk, M., Hostmann, M., Schweizer, S., Sporri, C., Tockner, K. and Truffer, B., Concepts of decision support for river rehabilitation, Environmental Modelling and Software 22, 188-201, 2007. \cr\cr
\url{http://www.modul-stufen-konzept.ch}\cr\cr
Stucki P. Methoden zur Untersuchung und Beurteilung der Fliessgewaesser - Makrozoobenthos Stufe F. Bundesamt fuer Umwelt, Bern. Umwelt-Vollzug Nr. 1026: 61 S. 2010. \cr\cr
Index Biologique Global Normalise,  AFNOR T 90-350 \cr\cr    
Guide technique de l'IBGN, Agences de l'Eau, 2eme Edition 2000 \cr\cr
Perret, P. Zustand der schweizerischen Fliessgewaesser in den Jahren 1974 1975. Projekt Mapos. Eidg. Amt fuer Umweltschutz, EAWAG, 276 S., 1977. \cr\cr
Beketov M.A., Foit K., Schafer R.B., Schriever C.A., Sacchi A., Capri E., Biggs J., Wells C. & Liess M. SPEAR indicates pesticide effects in streams - Comparative use of species- and family-level biomonitoring data. Environmental Pollution, 157, 1841-1848, 2009.
}

\seealso{
\code{\link[utility:utility-package]{utility}}.
}

\examples{
invertebrates <- val.invertebrates.create()
plot(invertebrates)
invertebrates.german <- val.invertebrates.create("Deutsch")
plot(invertebrates.german)
}


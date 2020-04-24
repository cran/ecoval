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
Langhans, S.D., Lienert, J., Schuwirth, N. and Reichert, P.
How to make river assessments comparable: A demonstration for hydromorphology, 
Ecological Indicators 32, 264-275, 2013. 
\url{http://dx.doi.org/10.1016/j.ecolind.2013.03.027}\cr\cr
Langhans, S.D., Reichert, P. and Schuwirth, N.
The method matters: indicator aggregation in ecological river assessment. 
Ecological Indicators 45, 494-507, 2014.
\url{http://dx.doi.org/10.1016/j.ecolind.2014.05.014}\cr\cr
Reichert, P., Schuwirth, N. and Langhans, S. 
Constructing, evaluating and visualizing value and utility functions for decision support,
Environmental Modelling & Software 46, 283-291, 2013. 
\url{http://dx.doi.org/10.1016/j.envsoft.2013.01.017}\cr\cr
Reichert, P., Langhans, S., Lienert, J. and Schuwirth, N. 
The conceptual foundation of environmental decision support. 
Journal of Environmental Management. 154, 316-332, 2015.
\url{http://dx.doi.org/10.1016/j.jenvman.2015.01.053}\cr\cr
Reichert, P., Borsuk, M., Hostmann, M., Schweizer, S., Sporri, C., Tockner, K. and Truffer, B.
Concepts of decision support for river rehabilitation,
Environmental Modelling and Software 22, 188-201, 2007.
\url{http://dx.doi.org/10.1016/j.envsoft.2005.07.017}\cr\cr
\url{http://www.modul-stufen-konzept.ch}\cr\cr
Stucki P.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser - Makrozoobenthos Stufe F.
Bundesamt fuer Umwelt, Bern. Umwelt-Vollzug Nr. 1026: 61 S. 2010.
\url{http://www.bafu.admin.ch/uv-1026-d}\cr\cr
Indice Biologique Global Normalise IBGN, NF-T90-350. Guide Technique.
Agences de l'Eau, 2000.
\url{https://hal.archives-ouvertes.fr/hal-00490432/document}\cr\cr
Beketov M.A., Foit K., Schafer R.B., Schriever C.A., Sacchi A., Capri E., Biggs J., Wells C. & Liess M.
SPEAR indicates pesticide effects in streams - Comparative use of species- and family-level biomonitoring data.
Environmental Pollution, 157, 1841-1848, 2009.
\url{http://dx.doi.org/10.1016/j.envpol.2009.01.021}
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


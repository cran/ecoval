\name{val.heavymetals.create}

\alias{val.heavymetals.create}

\title{
Creates a Value Function for River Heavy Metal Concentrations
}

\description{
Creates a value function for heavy metal concentrations in river sediments.
}

\usage{
val.heavymetals.create(language     = "English",
                       dictionaries = NA,
                       col          = "black",
                       version      = "AWEL")
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
  \item{version}{
Option to choose between the version "AWEL", which uses an assessment of AWEL (2006) based on quality criteria of LAWA (1998), or the version "IKSR", which uses the assessment of the IKSR (2009).
}
}

\value{
The function returns the value function as a class utility.
}

\references{
Langhans, S.D., Lienert, J., Schuwirth, N. and Reichert, P.
How to make river assessments comparable: A demonstration for hydromorphology, 
Ecological Indicators 32, 264-275, 2013. 
\doi{10.1016/j.ecolind.2013.03.027}\cr\cr
Langhans, S.D., Reichert, P. and Schuwirth, N.
The method matters: indicator aggregation in ecological river assessment. 
Ecological Indicators 45, 494-507, 2014.
\doi{10.1016/j.ecolind.2014.05.014}\cr\cr
Reichert, P., Schuwirth, N. and Langhans, S. 
Constructing, evaluating and visualizing value and utility functions for decision support,
Environmental Modelling & Software 46, 283-291, 2013. 
\doi{10.1016/j.envsoft.2013.01.017}\cr\cr
Reichert, P., Langhans, S., Lienert, J. and Schuwirth, N. 
The conceptual foundation of environmental decision support. 
Journal of Environmental Management. 154, 316-332, 2015.
\doi{10.1016/j.jenvman.2015.01.053}\cr\cr
Reichert, P., Borsuk, M., Hostmann, M., Schweizer, S., Sporri, C., Tockner, K. and Truffer, B.
Concepts of decision support for river rehabilitation,
Environmental Modelling and Software 22, 188-201, 2007.
\doi{10.1016/j.envsoft.2005.07.017}\cr\cr
\url{https://modul-stufen-konzept.ch}\cr\cr
Langhans, S.D. und Reichert, P., Einbettung von Verfahren zur Fliessgewaesserbewertung in ein uebergeordnetes Gewaessermanagementkonzept - Vorschlaege am Beispiel des Modulstufenkonzepts, Wasser Energie Luft 103(3), 204-214, 2011.\cr\cr 
AWEL Amt fuer Abfall, Wasser, Energie und Luft, Kanton Zuerich, Statusbericht 2006: Wasserqualitaet der Seen, Fliessgewaesser und des Grundwassers im Kanton Zuerich.\cr\cr
LAWA (Laenderarbeitsgemeinschaft Wasser) 1998: Zielvorgaben zum Schutz oberirdischer Binnengewaesser. Band II: Ableitung und Erprobung von Zielvorgaben zum Schutz oberiridischer Binnengewaesser fuer die Schwermetalle Blei, Cadmium, Chrom, Kupfer, Nickel, Quecksilber und Zink. Kulturbuchverlag Berlin GmbH, Berlin.\cr\cr 
IKSR 2009. Bericht Nr. 175, Sedimentmanagementplan Rhein.
}

\seealso{
\code{\link[utility:utility-package]{utility}}.
}

\examples{
heavymetals <- val.heavymetals.create()
plot(heavymetals)
heavymetals.german <- val.heavymetals.create("Deutsch")
plot(heavymetals.german)
heavymetals.IKSR <- val.heavymetals.create(version="IKSR")
plot(heavymetals.IKSR,type="nodes")
}


\name{val.micropoll.create}

\alias{val.micropoll.create}

\title{
Creates a Value Function for River Micropollutants
}

\description{
Creates a value function for river micropollutants with episodic inputs (pesticides, often from diffuse sources) and continous inputs (mainly from point-sources).
Micropollutants with continous inputs are assessed regarding their toxicity to different organism groups. Micropollutants with episodic inputs are grouped according to
their mode of action while exposure patterns are taken into account.
}

\usage{
val.micropoll.create(language     = "English",
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
Junghans, M. Kunz, P., Werner, I.
Toxizitaet von Mischungen, aktuelle praxisorientierte Ansaetze fuer die Beurteilung von Gewaesserproben.
Aqua und Gas 5, 54-61, 2013. \cr\cr
Goetz, Ch. Kase, R., Kienle, C., Hollender, J.
Mikroverunreinigungen aus kommunalem Abwasser: Kombination von Expositions- und oekotoxikologischen Effektdaten.
Gas Wasser Abwasser 7,575-585, 2010.\cr\cr
Goetz, C.W., R. Kase und J. Hollender.
Mikroverunreinigungen - Beurteilungskonzept fuer organische Spurenstoffe aus kommunalem Abwasser. 
Studie im Auftrag des BAFU. Eawag, Duebendorf, 2010. \cr\cr
AWEL Amt fuer Abfall, Wasser, Energie und Luft, Kanton Zuerich, 
Statusbericht 2006: Wasserqualitaet der Seen, Fliessgewaesser und des Grundwasser im Kanton Zuerich.\cr\cr
Balsiger, 
Gewaesserbelastung durch Pestizide, Gas Wasser Abwasser 3/2007, 2007.\cr\cr
Chevre et al. 2006: 
Pestizide in Schweizer Oberflaechengewaessern. Wirkungsbasierte Qualitaetskriterien. 
Gas Wasser Abwasser 4/2006. S. 297-307, 2006
}

\seealso{
\code{\link[utility:utility-package]{utility}}.
\code{\link{val.pesticides.create}}.
}

\examples{
micropoll <- val.micropoll.create()
plot(micropoll)
micropoll.german <- val.micropoll.create("Deutsch")
plot(micropoll.german)
}


\name{lake.morphol.2016.create}

\alias{lake.morphol.2016.create}

\title{
Creates a Value Function for Lake Shore Morphology
}

\description{
Creates a value function for lake shore morphology based on the Swiss concept for lake assessment.
}

\usage{
lake.morphol.2016.create(language     = "English",
                         dictionaries = NA,
                         col          = NA)
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
The function returns the value function as a class \code{utility}.
}

\details{
The following figures show the objectives hierarchy for lake shore morpholgy assessment in English, German and French, as produced by the plot commands shown below:\cr
\if{html}{\figure{lakemorphol2016.png}{options: width=100\%}}
\if{latex}{\figure{lakemorphol2016.pdf}{options: width=5.5in}}
\if{html}{\figure{lakemorphol2016German.png}{options: width=100\%}}
\if{latex}{\figure{lakemorphol2016German.pdf}{options: width=5.5in}}
\if{html}{\figure{lakemorphol2016French.png}{options: width=100\%}}
\if{latex}{\figure{lakemorphol2016French.pdf}{options: width=5.5in}}
}

\author{
Peter Reichert <peter.reichert@eawag.ch>
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
Niederberger, K., Rey, P., Reichert, P., Schlosser, J., Helg, U., Haertel-Borer, S., Binderheim, E.
Methoden zur Untersuchung und Beurteilung der Seen. Modul: Oekomorphologie Seeufer.
Bundesamt fuer Umwelt, Bern. Umwelt-Vollzug Nr. 1632. 73 S. 2016. 
\url{http://www.bafu.admin.ch/publikationen/publikation/01874/index.html?lang=de}\cr\cr
Schlosser, J.A., Haertel-Borer, S., Liechti, P., Reichert, P.
Konzept fuer die Untersuchung und Beurteilung der Seen in der Schweiz. Anleitung zur Entwicklung und Anwendung von Beurteilungsmethoden.
Bundesamt fuer Umwelt, Bern. Umwelt-Wissen Nr. 1326. 38 S. 2013.
\url{http://www.bafu.admin.ch/publikationen/publikation/01749/index.html?lang=de}
}

\seealso{
\code{\link{lake.morphol.2016.read.attrib}},
\code{\link{lake.morphol.2016.plot.val.spatial}},
\code{\link{lake.morphol.2016.aggregate.val.spatial}}.
}

\examples{
morphol <- lake.morphol.2016.create()
plot(morphol,two.lines=TRUE)
morphol.german <- lake.morphol.2016.create("Deutsch")
plot(morphol.german,two.lines=TRUE)
morphol.french <- lake.morphol.2016.create("Francais")
plot(morphol.french,two.lines=TRUE)
}


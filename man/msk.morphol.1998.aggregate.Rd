\name{msk.morphol.1998.aggregate}

\alias{msk.morphol.1998.aggregate}

\title{
Aggregation function of the highest node in the morphology module of the Swiss modular concept for stream assessment, level I (Regional survey) from 1998.
}

\description{
Aggregates the values of the 2 sub-objectives at the highest aggregation level of the morphology module of the Swiss River Assessment Program MSK (1998).
If the river is covered, the value for morphology is zero, otherwise it has the value of the uncovered node.
}

\usage{
msk.morphol.1998.aggregate(u,
                           par = NA)
}

\arguments{
  \item{u}{
Numerical vector of length 2 containing the values that quantify the degree of fulfillment of the 2 sub-objettives.
}
  \item{par}{
Argument added for consistency with the other aggregation procedures. No parameters are needed.
}
}

\value{
The function returns the aggregated value.
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
Huette, M. and Niederhauser P.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser in der Schweiz: Oekomorphologie Stufe F. 
Mitteilungen zum Gewaesserschutz Nr. 27. Bundesamt fuer Umwelt, Wald und Landschaft, BUWAL, Bern. 1998.
\url{https://www.bafu.admin.ch/bafu/de/home/themen/wasser/publikationen-studien/publikationen-wasser/methoden-fliessgewaesser-oekomorphologie-stufe-f.html}
}

\seealso{
\code{\link{msk.morphol.1998.create}},
\code{\link[utility:utility-package]{utility}}.
}

\examples{
morphol <- msk.morphol.1998.create()
plot(morphol)
morphol.german <- msk.morphol.1998.create("Deutsch")
plot(morphol.german)
}


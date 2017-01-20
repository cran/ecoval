\name{msk.hydrol.2011.aggregate}

\alias{msk.hydrol.2011.aggregate}

\title{
Aggregation function of the hydrology module of the Swiss modular concept for stream assessment, level I (Regional survey) from 2011.
}

\description{
Aggregates the values of the 9 sub-objectives at the second-highest aggregation level of the hydrology module of the Swiss River Assessment Program MSK (2011).
}

\usage{
msk.hydrol.2011.aggregate(u,
                          par = NA)
}

\arguments{
  \item{u}{
Numerical vector of length 9 containing the values that quantify the degree of fulfillment of the 9 sub-objettives.
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
Pfaundler M.,Duebendorfer,C, Zysset, A.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser. Hydrologie - Abflussregime Stufe F (flaechendeckend).
Bundesamt fuer Umwelt, Bern. Umwelt-Vollzug Nr. 1107: 113 S, 2011.
\url{http://www.bafu.admin.ch/publikationen/publikation/01603/index.html?lang=de}
}

\seealso{
\code{\link{msk.hydrol.2011.create}},
\code{\link[utility:utility-package]{utility}}.
}

\examples{
hydrol <- msk.hydrol.2011.create()
plot(hydrol)
hydrol.german <- msk.hydrol.2011.create("Deutsch")
plot(hydrol.german)
}


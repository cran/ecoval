\name{msk.macrophytes.2017.plot.typedef}

\alias{msk.macrophytes.2017.plot.typedef}

\title{
Function to visualize the uncertainties of the macrophyte river types definition.
}

\description{
Function to visualize the uncertainties of the macrophyte river types definition.
}

\usage{
msk.macrophytes.2017.plot.typedef(res.calc.types,max.x=NA,max.y=NA,...)
}

\arguments{
  \item{res.calc.types}{
results as provided by one of the functions \code{\link{msk.macrophytes.2017.calc.types}} 
or \code{\link{msk.macrophytes.2017.read.compile.evaluate}}.
}
  \item{max.x}{
named vector of maxima for the extent of the x-axes of the plots (check names of variables from the plot with omitting this argument).
}
  \item{max.y}{
named vector of maxima for the extent of the y-axes of the plots (check names of variables from the plot with omitting this argument).
}
  \item{...}{
additional arguments are passed to the function \code{\link{plot}}.
}
}

\value{
.
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
Kaenel, B., Michel, C., Reichert, P.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser in der Schweiz: Makrophyten. 
Bundesamt fuer Umwelt, Wald und Landschaft, BUWAL, Bern. 2017.
\url{http://www.modul-stufen-konzept.ch}
}

\seealso{
\code{\link{msk.macrophytes.2017.create}},
\code{\link[utility:utility-package]{utility}}.
}



\name{msk.macrophytes.2017.plot.hierarchy}

\alias{msk.macrophytes.2017.plot.hierarchy}

\title{
Function to plot the objectives hierarchy of the macrophyte assessment colored according the valuation.
}

\description{
Function to plot the objectives hierarchy of the macrophyte assessment colored according the valuation.
}

\usage{
msk.macrophytes.2017.plot.hierarchy(res,i,final=TRUE,...)
}

\arguments{
  \item{res}{
results as provided by one of the functions \code{\link{msk.macrophytes.2017.calc.types}} 
or \code{\link{msk.macrophytes.2017.read.compile.evaluate}}.
}
  \item{i}{
row index of site to be plotted.
}
  \item{final}{
logical variable indicating whether to plot the final valuation or the valuation before plausibilization.
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
Kaenel, B., Michel, C., Reichert, P.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser. Makrophyten - Stufe F (flaechendeckend) und Stufe S (systembezogen). 
Entwurf. 
Bundesamt fuer Umwelt, Bern. 119 S. 2017.
\url{https://modul-stufen-konzept.ch/}\cr\cr
}

\seealso{
\code{\link{msk.macrophytes.2017.create}},
\code{\link[utility:utility-package]{utility}}.
}



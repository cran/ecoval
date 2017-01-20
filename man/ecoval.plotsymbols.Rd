\name{ecoval.plotsymbols}

\alias{ecoval.plotsymbols}

\title{
Plot valuations of different sub-objectives as pie charts. 
}

\description{
A function to add a plot with valuations of different sub-objectives as a pie chart to an existing plot e.g. with the river network. 
}

\usage{
ecoval.plotsymbols(nodes,x,y,r,u,
                   square     = F,
                   labels     = NA,
                   col        = utility.calc.colors(),
                   pos.legend = NA,
                   cex.nodes  = 1)
}

\arguments{
  \item{nodes}{
  Nodes of a value function, which should be plotted as pie chart.
}
  \item{x}{
  Vector with x-coordinates for the pie charts.
}
  \item{y}{
   Vector with y-coordinates for the pie charts.
}
  \item{r}{
  Radius of the pie chart.
}
  \item{u}{
  Dataframe or matrix with values between 0 and 1 corresponding to the valuation of the nodes with rows for the different pie charts and columns for each node. The column names have to be identical to the nodes.
}
  \item{square}{
  Logical value, if true the diagram is plotted as square, if false as pie chart.
}
  \item{labels}{
  Labels for the pie charts that are plotted close to the chart.
}
  \item{col}{
  Colour-coding that transforms the numerical values of u into a color. Default is a plot-function from the utility package "utility.calc.colors()" with the following color-classes: 0-0.2 red, 0.2-0.4 orange, 0.4-0.6 yellow, 0.6-0.8 green, 0.8-1 blue.
}
  \item{pos.legend}{
  Vector with x and y-coordinates of the legend.
}
  \item{cex.nodes}{
  A numerical value giving the amount by which plotting text of the legend and the labels should be magnified relative to the default.
}
}


\references{
Reichert, P., Schuwirth, N. and Langhans, S. 
Constructing, evaluating and visualizing value and utility functions for decision support,
Environmental Modelling & Software 46, 283-291, 2013. 
\url{http://dx.doi.org/10.1016/j.envsoft.2013.01.017}\cr\cr
Reichert, P., Langhans, S., Lienert, J. and Schuwirth, N. 
The conceptual foundation of environmental decision support. 
Journal of Environmental Management. 154, 316-332, 2015.
\url{http://dx.doi.org/10.1016/j.jenvman.2015.01.053}\cr\cr
Baumann P., Langhans S.D.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser. Synthese der Beurteilungen auf Stufe F (flaechendeckend).
Umwelt-Vollzug Nr. Bundesamt fuer Umwelt, Bern: 47 S.\cr\cr
\url{http://www.modul-stufen-konzept.ch}
}

\seealso{
\code{\link[utility:utility-package]{utility}},
\code{\link[rivernet:rivernet-package]{rivernet}}.
}

\examples{
plot(1, type="n",axes=FALSE,ann=FALSE,xlim=c(0, 10),ylim=c(0, 10))
u1 <- matrix(data=c(0.1,0.5,1,0.3,0.4,0.7),nrow=2,
             dimnames=list(c(NA,NA),c("morphology","hydrology","nutrients")))

ecoval.plotsymbols(nodes      = c("morphology","hydrology","nutrients"),
                   x          = c(1,2),
                   y          = c(7,8.5),
                   r          = 0.5,
                   u          = u1,
                   col        = utility.calc.colors(),
                   pos.legend = c(7,8.5),
                   cex.nodes  = 1)

u2 <- matrix(data=c(0.1,0.25,0.75,0.6),nrow=1,
             dimnames=list(NA,c("fish","algae","invertebrates","macrophytes")))

ecoval.plotsymbols(nodes      = c("fish","algae","invertebrates","macrophytes"),
                   x          = 1.5,
                   y          = 3,
                   r          = 0.5,
                   u          = u2,
                   square     = TRUE,
                   col        = utility.calc.colors(),
                   pos.legend = c(7,3),
                   cex.nodes  = 1)
}
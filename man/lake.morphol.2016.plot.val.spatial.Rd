\name{lake.morphol.2016.plot.val.spatial}

\alias{lake.morphol.2016.plot.val.spatial}

\title{
Plots valuations along a single spatial dimension.
}

\description{
Plots valuations of different lake shore sections as a line along a single spatial dimension.
}

\usage{
lake.morphol.2016.plot.val.spatial(u,
                                   uref      = NA,
                                   nodes     = NA,
                                   main      = "",
                                   col       = c("red","orange","yellow","green","blue"),
                                   gridlines = FALSE,
                                   ...)
}

\arguments{
  \item{u}{
Data frame with calculated values for nodes (columns) and different shore sections (rows).
The row labels are assumed to contain the spatial information as a string in the format
\code{id | start - end} where \code{id} is the id of the lake, \code{start} is the start
coordinate of the section along the shore line, and \code{end} is the end coordinate along 
the shore line.
The first part, \code{id | }, is optional and can be omitted if the data is from a unique
length measurement (of a single lake).
Single spaces between the items are required.
Note that this format is produced automatically when reading attributes with the function
\code{\link{lake.morphol.2016.read.attrib}} and is then transferred to the values file
when evaluating the value function.
}
  \item{uref}{
(optional) a second value table to compared with the one provided with the argument \code{u}.
This can represent a different aggregation scheme or the effect of a rehabilitation measure.
}
  \item{nodes}{
(optional) a vector of node names to be plotted (in the order provided here).
Default is to plot all nodes.
}
   \item{main}{
(optional) a header written to the top of the plot combined with the lake id if one was provided
(see discussion of argument \code{u}).
}
  \item{col}{
(optional) an optional color coding for the lines to be plotted.
}
  \item{gridlines}{
(optional) a logical variable to specify whether gridlines should mark the start and end points
of the sections.
}
  \item{...}{
(optional) further plot parameters are forwarded to the plot command (e.g \code{lwd} for line width).
}
}

\value{
The function returns a data frame with start and end of the reaches in the first two columns and the attribute levels in subsequent columns.
}

\author{
Peter Reichert <peter.reichert@eawag.ch>
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
Niederberger, K., Rey, P., Reichert, P., Schlosser, J., Helg, U., Haertel-Borer, S., Binderheim, E.
Methoden zur Untersuchung und Beurteilung der Seen. Modul: Oekomorphologie Seeufer.
Bundesamt fuer Umwelt, Bern. Umwelt-Vollzug Nr. 1632. 73 S. 2016. 
\url{http://www.bafu.admin.ch/uv-1632-d}\cr\cr
Schlosser, J.A., Haertel-Borer, S., Liechti, P., Reichert, P.
Konzept fuer die Untersuchung und Beurteilung der Seen in der Schweiz. Anleitung zur Entwicklung und Anwendung von Beurteilungsmethoden.
Bundesamt fuer Umwelt, Bern. Umwelt-Wissen Nr. 1326. 38 S. 2013.
\url{http://www.bafu.admin.ch/uw-1326-d}
}

\seealso{
\code{\link{lake.morphol.2016.create}},
\code{\link{lake.morphol.2016.read.attrib}},
\code{\link{lake.morphol.2016.aggregate.val.spatial}}.
}



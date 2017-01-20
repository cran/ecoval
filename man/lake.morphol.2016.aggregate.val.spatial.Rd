\name{lake.morphol.2016.aggregate.val.spatial}

\alias{lake.morphol.2016.aggregate.val.spatial}

\title{
Aggregate valuations along a single spatial dimension.
}

\description{
Aggregate valuations of different lake shore sections along a single spatial dimension
according to an arbitrarily defined grid.
}

\usage{
lake.morphol.2016.aggregate.val.spatial(u,breakpoints)
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
  \item{breakpoints}{
For a single lake id, a vector of breakpoints that define the intervals to which the values are aggregated.
For multiple lakes, a list of such vectors with as many elements and in the same order as the lake ids in
the data frame provided with the argument \code{u}.
}
}

\value{
A data frame of the same format as the argument \code{u} with the aggregated values.
}

\details{
Aggregation is done by length-weighted averaging of the values at each level of the objectives hierarchy.
Note that this leads to an valuation result that is not compatible with the original value function (valuations of end nodes and aggregated nodes) across the objective hierarchy.
Nevertheless, this seems to be the most meaningful aggregation as we cannot average discrete, non-numeric attributes and then re-calculate the corresponding values.
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
\code{\link{lake.morphol.2016.create}},
\code{\link{lake.morphol.2016.read.attrib}},
\code{\link{lake.morphol.2016.plot.val.spatial}}.
}



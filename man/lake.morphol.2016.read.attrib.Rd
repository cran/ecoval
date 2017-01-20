\name{lake.morphol.2016.read.attrib}

\alias{lake.morphol.2016.read.attrib}

\title{
Reads attributes with different segmentation.
}

\description{
Reads individual attributes with different segmentation and combines them to a data frame with unified segmentation.
}

\usage{
lake.morphol.2016.read.attrib(directory    = ".",
                              attrib.names = c("E01","E02","B02","B01","C06","C01",
                                               "C02","C03","C04","C05","D01","D02"),
                              col.names    = c("Von","Bis","Code","ID_Uferlinie",
                                               "Bemerkungen"))
}

\arguments{
  \item{directory}{
(optional) Directory from which the files are read.
The directory must contain the data files, one file per attribute. 
The file names must start with the attribute names as given by the argument \code{attrib.names} and they must contain the columns as specified by the argument \code{col.names}.
The file format must be comma-separated text with the first row containing the headers as specified by the argument \code{col.names} and subsequent rows the corresponding data.
The files may contain additional columns that will be ignored.
}
  \item{attrib.names}{
(optional) Names of the attributes. These should match the names required by the dictionary used in \code{\link{lake.morphol.2016.create}}.
}
  \item{col.names}{
(optional) Names of the columns to be read from the individual attribute files. 
The first three elements must be present and represent the columns containing the start and end point of the reach (one dimenstional length measure along the shoreline) and the corresponding attribute level.
The fourth element represents an optional id to distinguish different lakes or shoreline measures (for the same id, the lenght measure used to characterize start and end of a reach must be unique). This element can be missing or NA; both indicates a unique lenght measure across all records.
Further column names can be provided and are interpreted as comments. 
These are merged to a single string when compiling the output data frame.
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
\code{\link{lake.morphol.2016.plot.val.spatial}},
\code{\link{lake.morphol.2016.aggregate.val.spatial}}.
}



\name{msk.macrophytes.2017.read.compile.evaluate}

\alias{msk.macrophytes.2017.read.compile.evaluate}

\title{
Function to read and compile data for macrophyte and calculate river types and valuation.
}

\description{
Function to read and compile data for macrophyte and calculate river types and valuation.
}

\usage{
msk.macrophytes.2017.read.compile.evaluate(file.site,
                                           pic.folder        = ".",
                                           file.species      = NA,
                                           file.typeplaus    = NA,
                                           sampling.protocol = "v2018",
                                           sampsize          = 10000,
                                           file.res          = NA,
                                           file.doc          = NA,
                                           file.taxa.used    = NA,
                                           file.taxa.removed = NA,
                                           file.check.msg    = NA,
                                           sep               = "\t",
                                           sep.in            = NA,
                                           sep.out           = NA,
                                           language          = "English",
                                           dictionaries      = NA)
                                           
                                           
}

\arguments{
  \item{file.site}{
name of text file with site characteristics.
}
  \item{pic.folder}{
(optional) name of folder to search for site pictures (pictures found will be displayed).
}
  \item{file.species}{
(optional) name of text file with species observations.
}
  \item{file.typeplaus}{
(optional) name of text file with plausibilized river types.
}
  \item{sampling.protocol}{
(optional) sampling protocol ("v2018" or "v2009").
}
  \item{sampsize}{
(optional) sample size for Monte Carlo calculation of probabilities of macrophyte river types (default is 10000).
}
  \item{file.res}{
Name of text file for results.
}
  \item{file.doc}{
Name of text file for documentation of sites.
}
  \item{file.taxa.used}{
(optional) name of text file for taxa used for assessment.
}
  \item{file.taxa.removed}{
(optional) name of text file for taxa that were removed because of insufficient determination or
because they are not on the taxa list.
}
  \item{file.check.msg}{
(optional) name of text file for warnings and error messages from compilation of species data.
}
  \item{sep}{
Column separator for input and output text files (see also arguments \code{sep.in} and \code{sep.out} below).
}
  \item{sep.in}{
Column separator for input files (only needed if different for input and output files; default is the argument \code{sep} above).
}
  \item{sep.out}{
Column separator for output files (only needed if different for input and output files; default is the argument \code{sep} above).
}
  \item{language}{
Language to be used to denote nodes, attributes and attribute levels.
Must be a column name of the table provided by the second argument.
}
  \item{dictionaries}{
Matrix of dictionaries with the languages provided by the column names and the original words provided as the row names.
If dictionary is NA, the default dictionary \code{ecoval.dictionaries.default} is loaded.
}
}

\value{
Named list of outputs dependent on tasks performed (see \code{Details}).
}

\details{
The tasks and ouput depends on the input provided.
For \code{sampsize} = 0, only checking of the input files is done.
For \code{sampsize} > 0, in addition, Monte Carlo simulation is done to get the probabilities of all river types.
If the file \code{file.species} is provided (and \code{sampsize} > 0), in addition, the valuations of all sites are performed.
If also the file \code{file.typeplaus} is provided, an additional valuation is calculated for the plausibilized river types.
In addition to the list of outputs (see \code{Value}) the output is written to the text file \code{file.res} and to pdf site documentation files with names constructed from \code{file.doc} and the identifiers of the sites, if these file names are provided.
In addition, diagnostics and error messages are written to the files \code{file.taxa.used}, \code{file.taxa.removed} and \code{file.check.msg}.
See \url{http://www.modul-stufen-konzept.ch} for more details regarding the methodology.
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
Kaenel, B., Michel, C., Reichert, P.
Methoden zur Untersuchung und Beurteilung der Fliessgewaesser. Makrophyten - Stufe F (flaechendeckend) und Stufe S (systembezogen). 
Entwurf. 
Bundesamt fuer Umwelt, Bern. 119 S. 2017.
\url{http://www.modul-stufen-konzept.ch/fg/module/wasserpflanzen/index}\cr\cr
\url{http://www.modul-stufen-konzept.ch}\cr\cr
}

\seealso{
\code{\link{msk.macrophytes.2017.create}},
\code{\link[utility:utility-package]{utility}}.
}



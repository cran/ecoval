\name{msk.macrophytes.2017.calc.types}

\alias{msk.macrophytes.2017.calc.types}

\title{
Function to calculate macrophyte river types for a given set of attributes.
}

\description{
Function to calculate macrophyte river types for a given set of attributes.
}

\usage{
msk.macrophytes.2017.calc.types(attrib,
                                sampsize     = 10000,
                                language     = "English",
                                dictionaries = NA)
}

\arguments{
  \item{attrib}{
Data frame with river attributes.
}
  \item{sampsize}{
(optional) sample size for Monte Carlo calculation of probabilities of macrophyte river types (default is 10000).
}
  \item{language}{
(optional) language to be used to denote nodes, attributes and attribute levels.
Must be a column name of the data frame provided by the argument \code{dictionaries} or of the default dictionaries data frame of the package if no data frame is provided under \code{dictionaries}.
}
  \item{dictionaries}{
(optional) data frame of dictionaries with the languages provided by the column names and the original keywords provided as the row names.
If \code{dictionaries} is NA, the default dictionary \code{ecoval.dictionaries.default} is loaded.
}
}

\value{
The function returns a list with the following entries:\cr
\code{typedef}: a list containing the details of the definition of macrophyte river types.\cr
                Important entries: \cr
                \code{thresholds}: nominal thresholds of the attributes used 
                                   to classify rivers into macrophyte river types.\cr
                \code{thresholds.indices}: table of different combinations of lower and upper thresholds.\cr
                \code{types}: table of macrophyte river types corresponding to the combinations 
                              listed under \code{thresholds.indices} 
                              (three different levels of resolution: 
                              according to the river types scheme, 
                              used for valuation, and
                              types for different growth forms).\cr
                \code{thresholds.unc}: definition of probability distributions used to characterize
                                       the uncertainty in the thresholds.\cr
                \code{observations.unc}: definition of probability distributions used to characterize
                                         the uncertainty in attributes.\cr
\code{attrib.types}:   data frame of attributes needed to calculate macrophyte river types.
\code{types.comb.obs}: table of the row indices of the combinations according to \code{typedef$thresholds.indices}
                       that corresponds to the observed river site attributes.\cr
\code{types.scheme.obs}: table of the macrophyte river types according to the river types scheme
                         that corresponds to the observed river site attributes.\cr
\code{types.fields.obs}: table of the row and column indices of the river types scheme
                         that corresponds to the observed river site attributes.\cr\cr
\code{types.comb.probs}: table of probabilities of the row indices of the combinations according to 
                         \code{typedef$thresholds.indices}
                         that correspond to the river sites.\cr
\code{types.scheme.probs}: table of probabilities of river types according to river types scheme
                           that correspond to the river sites.\cr
\code{types.val.probs}: table of probabilities of river types used for valuation
                        that correspond to the river sites.\cr
\code{types.grfo.probs}: table of probabilities of river growth form types
                         that correspond to the river sites.\cr
\code{types.fields.probs}: table of probabilities of row and column indices of the river types scheme
                     that correspond to the river sites.\cr
\code{types.val.obs}: river types used for valuation.\cr
\code{types.scheme.maxprob}: river types according to the river types scheme with maximum probability.\cr
\code{types.val.maxprob}: river types used for valuation with maxiumum probability.\cr
\code{types.table}: data frame of the most important results.\cr
}

\details{
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



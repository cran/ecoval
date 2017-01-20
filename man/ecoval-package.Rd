\name{ecoval-package}

\alias{ecoval-package}
\alias{ecoval}

\docType{package}

\title{
Procedures for the Ecological Assessment of Surface Waters
}

\details{
\tabular{ll}{
Package: \tab ecoval\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2017-01-19\cr
License: \tab GPL (>= 2)\cr
Depends: \tab utility\cr
}
}

\description{
Functions for evaluating and visualizing ecological assessment procedures for surface waters. \cr\cr
Functions to generate branches (modules) of the assessment value function: \cr\cr

Swiss river assessment program and individual modules (MSK, \url{http://www.modul-stufen-konzept.ch}): \cr\cr
\code{\link{msk.create}}, \cr
\code{\link{msk.morphol.1998.create}}, \cr
\code{\link{msk.hydrol.2011.create}}, \cr
\code{\link{msk.physapp.2007.create}}, \cr
\code{\link{msk.nutrients.2010.create}}, \cr
\code{\link{msk.diatoms.2007.create}}, \cr
\code{\link{msk.invertebrates.2010.create}}, \cr
\code{\link{msk.fish.2004.create}}, \cr\cr
Additional modules in the evaluation state:\cr\cr
\code{\link{val.pesticides.create}}, \cr
\code{\link{val.micropoll.create}}, \cr
\code{\link{val.heavymetals.create}}, \cr
\code{\link{val.spear.create}}, \cr\cr
\code{\link{val.invertebrates.create}}, \cr\cr
Integrative assessment value function considering modules of the Swiss assessment program and modules at the evaluation stage:\cr\cr
\code{\link{ecoval.river.create}}, \cr\cr

Moduls of the Swiss lake assessment program (see \url{http://www.modul-stufen-konzept.ch}): \cr\cr
\code{\link{lake.morphol.2016.create}}, \cr\cr

Node names, attribute names and identifiers for attribute levels (if not numeric) can be translated into different languages. See: \cr\cr
\code{\link{ecoval.dict}}, \cr
\code{\link{ecoval.translate}}, \cr
\code{\link{ecoval.dictionaries.default}}, \cr
}

\author{
Nele Schuwirth <nele.schuwirth@eawag.ch> and Peter Reichert <peter.reichert@eawag.ch> 
  with contributions by Simone Langhans

Maintainer: Nele Schuwirth <nele.schuwirth@eawag.ch>
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
Reichert, P., Borsuk, M., Hostmann, M., Schweizer, S., Sporri, C., Tockner, K. and Truffer, B.
Concepts of decision support for river rehabilitation,
Environmental Modelling and Software 22, 188-201, 2007.
\cr\cr
\url{http://www.modul-stufen-konzept.ch}
}

\keyword{ package }

\seealso{
\code{\link[utility:utility-package]{utility}}.
}

\examples{
# creation of individual modules:

morphol <- msk.morphol.1998.create()
plot(morphol)

hydrol <- msk.hydrol.2011.create()
plot(hydrol)

physapp <- msk.physapp.2007.create()
plot(physapp)

nutrients <- msk.nutrients.2010.create()
plot(nutrients)

micropoll <- val.micropoll.create()
plot(micropoll)

heavymetals <- val.heavymetals.create()
plot(heavymetals)

spear <- val.spear.create()
plot(spear)

diatoms <- msk.diatoms.2007.create()
plot(diatoms)

invertebrates <- msk.invertebrates.2010.create()
plot(invertebrates)

fish <- msk.fish.2004.create()
plot(fish)

# creation of three versions of assessment programs:

msk <- msk.create(language="EnglishNodes",col="blue")
plot(msk)
plot(msk,with.attrib=FALSE)

ecoval <- ecoval.river.create(language="EnglishNodes",col="red")
plot(ecoval)
plot(ecoval,with.attrib=FALSE)

beetles_richness <- utility.endnode.parfun1d.create(name.node   = "richness", 
                                                    name.attrib = "gb_richness", 
                                                    range       = c(0,1), 
                                                    name.fun    = "utility.fun.exp", 
                                                    par         = c(2,0,1), 
                                                    utility     = FALSE)
beetles_concordance <- utility.endnode.parfun1d.create(name.node   = "concordance", 
                                                       name.attrib = "gb_concordance", 
                                                       range       = c(0,1), 
                                                       name.fun    = "utility.fun.exp", 
                                                       par         = c(1,0,1), 
                                                       utility     = FALSE)
beetles <-utility.aggregation.create(name.node = "ground beetles",
                                     nodes     = list(beetles_richness,beetles_concordance),
                                     name.fun  = "utility.aggregate.add",
                                     par       = c(0.5,0.5))

ecoval2 <- ecoval.river.create(phys     = list(msk.morphol.1998.create),
                               chem     = list(msk.nutrients.2010.create,
                                               val.pesticides.create,
                                               val.heavymetals.create),
                               biol     = list(msk.invertebrates.2010.create,
                                               msk.fish.2004.create,
                                               beetles),
                               language = "EnglishNodes")
plot(ecoval2)
plot(ecoval2,with.attrib=FALSE)
}

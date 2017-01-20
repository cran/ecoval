\name{ecoval.river.create}

\alias{ecoval.river.create}

\title{
Creates a Value Function for Ecological River Assessment
}

\description{
Creates a value function for ecological river assessment based on physical, chemical and biological sub-objectives.
}

\usage{
ecoval.river.create(phys = list(msk.morphol.1998.create,
                                msk.physapp.2007.create),
                    physagg      = "utility.aggregate.addmin",
                    physpar      = numeric(0),
                    chem         = list(msk.nutrients.2010.create,
                                        val.micropoll.create,
                                        val.heavymetals.create),
                    chemagg      = "utility.aggregate.addmin",
                    chempar      = numeric(0),
                    biol         = list(msk.diatoms.2007.create,
                                        val.invertebrates.create,
                                        msk.fish.2004.create),
                    biolagg      = "utility.aggregate.addmin",
                    biolpar      = numeric(0),
                    ecolagg      = "utility.aggregate.addmin",
                    ecolpar      = numeric(0),
                    language     = "English",
                    dictionaries = NA,
                    col          = "black")
}

\arguments{
  \item{phys}{
List containing either functions to create branches of the value function
or already branches of value functions corresponding to physical sub-objectives.
These branches (produced by the function or given directly) must be in the form 
of a value function definition as provided by the package \code{utility}.
}
  \item{physagg}{
Name of the function to be used for aggregating the value function branches 
specified in the argument \code{phys}.
}
  \item{physpar}{
Vector of parameters (often weights) provided to the aggregation function 
(provided by the argument \code{physagg}).
}
  \item{chem}{
List containing either functions to create branches of the value function
or already branches of value functions corresponding to chemical sub-objectives.
These branches (produced by the function or given directly) must be in the form 
of a value function definition as provided by the package \code{utility}.
}
  \item{chemagg}{
Name of the function to be used for aggregating the value function branches 
specified in the argument \code{chem}.
}
  \item{chempar}{
Vector of parameters (often weights) provided to the aggregation function 
(provided by the argument \code{chemagg}).
}
  \item{biol}{
List containing either functions to create branches of the value function
or already branches of value functions corresponding to biological sub-objectives.
These branches (produced by the function or given directly) must be in the form 
of a value function definition as provided by the package \code{utility}.
}
  \item{biolagg}{
Name of the function to be used for aggregating the value function branches 
specified in the argument \code{biol}.
}
  \item{biolpar}{
Vector of parameters (often weights) provided to the aggregation function 
(provided by the argument \code{biolagg}).
}
  \item{ecolagg}{
Name of the function to be used to aggregate the physical, chemical, and 
biological values.
}
  \item{ecolpar}{
Parameter vector passed to the function specified under \code{ecolpar}.
}
  \item{language}{
Language to be used to denote nodes, attributes and attribute levels.
Must be a column name of the table provided by the second argument.
}
  \item{dictionaries}{
Matrix of dictionaries with the languages provided by the column names and the original words provided as the row names.
If dictionary is NA, the default dictionary \code{ecoval.dictionaries.default} is loaded.
}
  \item{col}{
Color of bounding boxes in objectives hierarchy.
}
}

\value{
The function returns the value function as a class utility.
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
\url{http://dx.doi.org/10.1016/j.envsoft.2005.07.017}\cr\cr
\url{http://www.modul-stufen-konzept.ch}
}

\seealso{
\code{\link[utility:utility-package]{utility}}.
}

\examples{
ecol <- ecoval.river.create()
plot(ecol)
ecol.german <- ecoval.river.create(language="Deutsch")
plot(ecol.german)
}


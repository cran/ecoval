\name{msk.macrophytes.2017.addminbonusmalus}

\alias{msk.macrophytes.2017.addminbonusmalus}

\title{Aggregation technique for MSK module macrophytes}

\description{
Aggregation technique for MSK module macrophytes.
}

\usage{
msk.macrophytes.2017.addminbonusmalus(u, par)
}

\arguments{
  \item{u}{
numeric vector of values or utilities to be aggregated.
}
  \item{par}{
numeric vector with the following components (n is the number of elements to aggregate):\cr
par[1:n]:  weights\cr
par[n+1]:  weight of additive aggregation (weight of minimum aggregation is 1-par[n+1])\cr
par[(n+2):(2*n+1)]:  indicators -1: malus, +1 bonus, 0 addmin.
}
}

\value{
The function returns the aggregated value or utility.
}

\author{
Peter Reichert <peter.reichert@eawag.ch>
}


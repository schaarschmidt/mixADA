\name{propplotreg}
\alias{propplotreg}

\title{
Plot to assess proportionality of negative controsl and sample means 
}
\description{
Scatter plot of sample means vy. negative control means for each experimental unit (run)
}
\usage{
propplotreg(DATINT, normfun = "median", normop = "logdiff")
}

\arguments{
  \item{DATINT}{ a data.frame as can be obtained from \code{\link{adapcheckdatainput}}

}
  \item{normfun}{ name of the function to use for summarizing negative controls and biological samples within experimental runs

}
  \item{normop}{ name of the option for normalization, see \code{\link{normalize}}

}
}

\value{
a plot
}

\keyword{ hplot}
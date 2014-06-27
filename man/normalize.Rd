\name{normalize}
\alias{normalize}

\title{
Normalization of biological samples by summar measure of negative control observations
}
\description{
Within each experimental unit (run), biological observations to be used for model fitting (sampleID, unspiked) are normalized by means or medians of negative control observations from the same experimemntal unit
}
\usage{
normalize(DATINT, normfun = "median", normop = "logdiff")
}

\arguments{
  \item{DATINT}{ a data.frame as can be obtained by using \code{\link{adapcheckdatainput}}

}
  \item{normfun}{name of the function to use for summarizing negative controls within each experimental runs current options: \code{"mean"}, \code{"median"}

}
  \item{normop}{ character string naming options for normalizing  biological samples (\code{sampleID}): current options:
  \code{"logdiff"}: natural log  of the data and substract \code{normfun} of negative controls at log-scale,
  \code{"diff"}: substract \code{normfun} of negative controls,
    \code{"ratio"}: devide by \code{normfun} of negative controls.
}
}

\value{
a list with
\item{NORMDAT }{a data.frame containing the original data as well as the normalized values}
\item{NORMINFO}{character string to be used as description in the GUI}

}

\keyword{ misc }

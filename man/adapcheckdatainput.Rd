\name{adapcheckdatainput}
\alias{adapcheckdatainput}
\alias{adapcheckdatainputsimple}

\title{
Check properties of input data
}
\description{
For internal use in the mixADA GUI.
}
\usage{
adapcheckdatainput(dat, response, treatment, runsnorm, sampleID,
 normalizeby, forfitting, runsmodel = NULL, spiked = NULL)

adapcheckdatainputsimple(dat, response, treatment, sampleID, forfitting,
 runsmodel = NULL, spiked = NULL)
}

\arguments{
  \item{dat}{ a data.frame
}
  \item{response}{ a single character string, naming a column with numeric entries in \code{dat}
}
  \item{treatment}{a single character string, naming a column (factor) in \code{dat}
}
  \item{runsnorm}{a character string or vector, naming those columns (factors) in \code{dat} that, after combining factor levels, uniquely identify technical units within which normalization should take place
}
  \item{sampleID}{a character string or vector, naming those columns (factors) in \code{dat} that, after combining factor levels, uniquely identify biological samples
}
  \item{normalizeby}{ a character string naming the level in \code{dat[,treatment]} which is supposed to serve as negative control for normalization, within each technical unit
}
  \item{forfitting}{ a character string naming the level in \code{dat[,treatment]}, which define that subset of data for which mixture models are to be fitted (the unspiked data, if spiked data are in \code{dat})
}
  \item{runsmodel}{a character string or vector, naming those columns (factors) in \code{dat}, that, after combining factor levels, will define the variance component of technical units in mixed effects and mixture model fitting
}
  \item{spiked}{ a character string naming the level in \code{dat[,treatment]}, which define that subset of data which is to be used for CCP estimation together with that in \code{forfitting}
}
}
\details{
Checks input data for completeness, appropriate properties of specified columns, combines severeal factors to one nwe factor if needed and ought to give comprehensive warnings and errors if data format is insufficient for later use in normalization, model fitting, etc.
}

\value{
a list with elements
\item{DATINT}{The input data set with additional columns specifying subset for later use in normalization, model fitting, CCP estimation}
\item{normTAB }{a table with sample sizes of negative controles for normalization in each technical unit}
\item{textnormunit}{character string, for labelling output in mixADA GUI}
\item{textnormlev}{character string, for labelling output in mixADA GUI}
}

\keyword{misc}


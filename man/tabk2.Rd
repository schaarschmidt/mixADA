\name{tabk2}
\alias{tabk2}

\title{Return data.frame with parameters estimates and no. of subjects/observations}

\description{
For internal use in the mixADA GUI: Return a table with estimated parameters and the number of subjects and observations in the subgroups determined by fitting a two-component mixture model.
}
\usage{
tabk2(resadapmixmod, design)
}

\arguments{
  \item{resadapmixmod}{
an object obtained by fitting a mixture model, by using functions \code{\link{adapmixmod}}
}
  \item{design}{
 character string, one of \code{"c2", "h2", "y"}, indicating the 'design' assumed for the analysis
}
}

\value{
a data.frame
}

\keyword{ misc }


\name{adapperccutpoints}
\alias{adapperccutpoints}

\title{
Percentiles of (subgroups of) observations
}
\description{
For internal use in the mixADA GUI: Computes sample quantiles fpor subgroups of observations, where subgroups are assumed to be determined by a two-component mixture model
}
\usage{
adapperccutpoints(resadapmixmod, level = 0.95, alternative = "less",
 group = c("nonresponder", "responder", "all"))
}

\arguments{
  \item{resadapmixmod}{
 an object obtained by fitting a mixture model, by using functions \code{\link{adapmixmod}}
}
  \item{level}{
 single numeric value, ]0,1[, the levels of percentiles
}
  \item{alternative}{as usual, single character string, "two.sided": two-sided prediction intervals, "less": upper prediction limits only, "greater": lower prediction limits only

}
  \item{group}{a single character string, naming for which subset of biological samples to consider for fitting random effects models and prediction limits

}
}

\value{
A data.frame with 3 columns:
  \item{value }{numeric, percentiles acc. to different methods}
  \item{group }{character, naming a subgroup of the mixture model}
  \item{estimated }{character, labels for the type of percentile used}

}

\keyword{ misc }

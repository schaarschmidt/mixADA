\name{adapmixmod}
\alias{adapmixmod}
\alias{adapmixmodsampleID}

\title{
Fit two-component mixture model
}
\description{
For internal use in the mixADA GUI: Fit a mixture model (with or without additional random effects within subject), order resulting clusters by estimated means of the subgroups.
}
\usage{
adapmixmod(DATINT, nrep = 10, design = c("c2", "h2", "c1", "h1"), 
varfix = c("no", "ran", "res", "bothranres"), aggfun = "mean")

adapmixmodsampleID(DATINT, nrep = 10, 
aggsamples = c("mean", "median"))
}

\arguments{
  \item{DATINT}{ an object obtained from normalizing function xxx

}
  \item{nrep}{single integer, the number of replicated fits in intializing the mixture model, passed to \code{initFlexmix(nrep)}
}
  \item{design}{ 
  \code{"c2", "c1"} invokes fitting a mixture model with between subject variance and additional random effect for the technical replications \code{runsmodel} within subject, and a residual variance.
  \code{"h2"} invokes fitting a mixture model with between subject variance and additional random effect for the interaction \code{sampleID:runs} within subject, and a residual variance.
    \code{"c1"} invokes fitting a mixture model fitting a mixture model with between subject variance and additional random effect for the technical replications \code{runsmodel} within subject, and a residual variance.
  \code{"h1"} invokes fitting a mixture model with random effect subjects and a residual variance term.

}
  \item{varfix}{ a single chracter string, defining which variance components are taken to be  equal between the two subgroups: 
 \code{"no"}: random effects and resiudal variance equal between subgroups,
 \code{"ran"}, random effects equal, resiudal variance different between subgroups,
 \code{"res"}, random effects different, resiudal variance equal between subgroups,
 \code{"bothranres"}, random effects and resiudal variance different between subgroups.
  Note that \code{varfix} is ignored if \code{design="h1"}, as there is only a subject specific variance and a residual error.
}
  \item{aggfun}{ function to summarize observations within each biological sample (\code{sampleID}) for initializing clusters

}
  \item{aggsamples}{function to summarize observations within each biological sample for running a simple mixture model without random effects in \code{adapmixmodsampleID}

}
}

\value{
a list
}


\keyword{ models }

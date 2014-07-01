\name{adaplmmintervals}
\alias{adaplmmintervals}

\title{
Prediction intervals.
}
\description{For internal use in the mixADA GUI: Fits simple random effects models (using package lme4) for a chosen subset of bioogical samples (acc. to previous classification in a mixture model), and computes prediction intervals based on the model fit. Also, percentile intervals for the subsets resulting from previous mixture model fits are returned.
}

\usage{
adaplmmintervals(resadapmixmod, design = c("c2", "h2", "y"), level = 0.95, 
alternative = "less", group = c("nonresponder", "responder", "all"))
}

\arguments{
  \item{resadapmixmod}{ an object obtained by fitting a mixture model, by using functions \code{\link{adapmixmod}}

}
  \item{design}{ one of three character strings naming an experimental design: \code{"c2"} invokes fitting a two-factor random effects model with interaction, \code{"h2"} invokes fitting a two-factor hierarchical random effects model, \code{"y"} invokes a simple model with only one variance for the biological samples

}
  \item{level}{ single numeric value, ]0,1[, the levels of prediction limit
}
  \item{alternative}{ as usual, single character string, "two.sided": two-sided prediction intervals, "less": upper prediction limits only, "greater": lower prediction limits only
  }
  \item{group}{ a single character string, naming for which subset of biological samples to consider for fitting random effects models and prediction limits
}
}

\value{
a list:
  \item{estlimitsd}{a data.frame with the estimated prediction limits or percentiles, incl some labelling}
  \item{TAB}{a table with sample sizes of biological samples within each technical unit, as used for random effects models}
  \item{PIE}{a list, with detailed results of the prediction limit computation}
  \item{DAT}{the data.frame used for model fitting (by default, only the subset classified as non-responder in the mixture model classification)}
and further elements as input

}


\keyword{htest}
\name{adaplmboxcox}
\alias{adaplmboxcox}
\alias{adaplmmboxcox}

\title{
Box-Cox-transformation and LRT for deviation form normality and log-normality
}
\description{
For internal use in the mixADA GUI! Simplistic grid search for Box-Cox-parameter lambda, and subsequent LRT for deviation from normality (lambda=1) and log-normality (lambda=0). 
}
\usage{
adaplmboxcox(resadapmixmod, normop,
 group = c("nonresponder", "responder", "all"))
}

\arguments{
  \item{resadapmixmod}{ an object resulting fromfitting a mixture model xxx, as can be obtained by using

}
  \item{normop}{ character string naiming one  of the options for normalization

}
  \item{group}{ a single character string, naming for which subset of biologoical samples to consider for the choice of the box-cox parameter of the data

}
}
\details{

Uses only the subset specified in \code{group},

backktransforms to origninal scale (if necessary due to normalization specified in \code{normop})

performs grid search for box-cox lambda on the backtransformed data, resulting in a rough estimate for lambda

computes LRT for H0:lamda=1 and H0:lambda=0 based on this rough estimate

returns tables with estimates and corresponding liklihood as well as test results

}
\value{
a list:
\item{tabest}{data.frame with 'estimates' resulting from the grid search }
 \item{tabtest}{ data.fra,me with test results of the 'LRT'}
 further elements to be used as acptions in the GUI output ...
}


\keyword{ htest }


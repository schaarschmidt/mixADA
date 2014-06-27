\name{adaCCP}
\alias{adaCCP}
\title{
Confirmatory cutpoint calculation
}
\description{
For internal use in the mixADA GUI: Compute confirmatory cutoiunts (CCP) as percentiles of ratios or percent inhibition of soiked and unspiked samples.
}
\usage{
adaCCP(fitk2, rdat, ccplevel = 0.99, resp,
 nrdefinition = c("modelclass"), comparison = c("percinhib", "ratio"),
 aggfun = c("mean", "median"), runsmodel)
}

\arguments{
  \item{fitk2}{ the object obtaiend from fitting a mixed effects mixture models using function xxx
}
  \item{rdat}{ the data set obtaiend from the normalization step, using function xxx
}
  \item{ccplevel}{ a single numeric value, ]0,1[, the level of the percentile to be computed
}
  \item{resp}{ a single character strinbg, providing the column name of the response variable in the data
}
  \item{nrdefinition}{
}
  \item{comparison}{ a single character string, specifying the type of comparison between the spiked an unspiked samples of the same subject, with each run
}
  \item{aggfun}{ character string naming the function to be used for aggregating repeated observations of the same subject within a technical replication
}
  \item{runsmodel}{ character string naming the column in the data set whcih summarized the technical replications
}
}
\details{

Repeated observations of each spiked an unspiked sample within each level of \code{runsmodel} are summarized by \code{aggfun}.
The resulting summary statistics are merged by \code{sampleID} and \code{runsmodel}
For each of resulting pairs of spiked unspiked summary satistics for a given \code{sampleID} and \code{runsmodel},
\code{spiked/unspiked} or  \code{100*(1 - spiked/unspiked)} is computed if \code{comparison=="ratio"} or  \code{comparison=="percinhib"}, respectively.
For the resulting values, two-sided percentile intervals with joint level \code{ccplevel} are computed for all \code{sampleID}s, or only for the subset specified as non-reponder in \code{fitk2}.

}

\value{
A list, use
\item{DATCOMP}{a data.frame containing the chosen summary stats for spiked as well as unspiked samples within each run}
\item{ynam}{character string naming the statistic used for comparing spiked and unspiked samples}
\item{limtab}{ a data.frame with estimated percentiles/ limits to be used as CCP}
\item{comparison}{character string for labelling the output  of the GUI}
\item{infoccpmeasure}{character string for labelling the output  of the GUI}
\item{limitexplanation}{character string for labelling the output  of the GUI}
}

\keyword{ inference }

\name{mixADAserver}
\alias{mixADAserver}
\alias{mixADAsimple}

\title{
Opens a graphical user interface 
}
\description{
Opens graphical user interface, based on the server.r and ui.r files in the subfolder mixADAserver.
}
\usage{
mixADAserver(lib.loc = NULL, mustWork=FALSE, \dots)
mixADAsimple(lib.loc = NULL, mustWork=FALSE, \dots)
}
\arguments{
  \item{lib.loc}{optional argument to be passed to \code{system.file}}
  \item{mustWork}{optional argument to be passed to \code{system.file}}
  \item{\dots}{optional arguments to be passed to \code{runApp}, package shiny}
}


\examples{
\dontrun{
mixADAserver()
mixADAsimple()
}
}

\keyword{ model}

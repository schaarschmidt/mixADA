
mixADAserver <- function(lib.loc = NULL, mustWork=FALSE, ...){
runApp(appDir = system.file("mixADAserver", package="mixADA", lib.loc=lib.loc, mustWork=mustWork), ...)
}
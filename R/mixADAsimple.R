
mixADAsimple <- function(lib.loc = NULL, mustWork=FALSE, ...){
  runApp(appDir = system.file("mixADAsimple", package="mixADA", lib.loc=lib.loc, mustWork=mustWork), ...)
}
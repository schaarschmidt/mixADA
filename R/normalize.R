normalize <-
  function(DATINT, normfun="median", normop ="logdiff")
  {
    
    switch(normfun,
           "median"={fun<-median}, "mean"={fun<-mean})
    
    namRHS <-c("sampleID", "runsnorm", "runsmodel", "normalizeby", "subsetforfitting", "datause", "subsetspiked")
    namDATINT <- names(DATINT)
    wRHS <- which(namRHS %in% namDATINT)
    RHS <- paste(namRHS[wRHS], collapse=" + ")
    
    switch(normop,
           "logdiff"={
             if(any(DATINT$response < 0)){warning(paste("Some observations are negative, log-transformation may not be appropriate."))}
             DATINT$lresponse <- log(DATINT$response) 
             FORMULA <- as.formula(paste("cbind(response, lresponse) ~ ", RHS, sep=""))
             DATINTrepvalLL <- aggregate(FORMULA, data=DATINT, FUN=fun)      
             DATINTN <- DATINT[DATINT$normalizeby,]
             DATAGG <- aggregate(lresponse ~ runsnorm, data=DATINTN, fun)
             DATINTAGG <- merge(x=DATINTrepvalLL, y=DATAGG, by.x="runsnorm", by.y="runsnorm")
             DATINTAGG$normresp <- DATINTAGG$lresponse.x - DATINTAGG$lresponse.y 
             NORMINFO <- paste("Data have been normalized by: log-transformation of the data (natural logarithm) and substracting", normfun, sep=" ")
           },
           
           "diff"={
             FORMULA <- as.formula(paste("response ~ ", RHS, sep=""))
             DATINTrepval <- aggregate(FORMULA, data=DATINT, FUN=fun)
             DATINTN <- DATINT[DATINT$normalizeby,]
             DATAGG <- aggregate(response ~ runsnorm, data=DATINTN, fun)
             names(DATAGG)[which(names(DATAGG)=="response")] <- "respagg"
             DATINTAGG <- merge(x=DATINTrepval, y=DATAGG, by.x="runsnorm", by.y="runsnorm")
             DATINTAGG$normresp <- DATINTAGG$response - DATINTAGG$respagg
             NORMINFO <- paste("Data have been normalized by: substracting", normfun, sep=" ")
           }, 
           
           "ratio"={
             FORMULA <- as.formula(paste("response ~ ", RHS, sep=""))
             DATINTrepval <- aggregate(FORMULA, data=DATINT, FUN=fun)
             DATINTN <- DATINT[DATINT$normalizeby,]
             DATAGG <- aggregate(response ~ runsnorm, data=DATINTN, fun)
             if(any(DATAGG$response < sqrt(.Machine$double.eps))){warning(paste(normfun, "in some groups for normalization are close to 0 or negative, dividing by these values may not be appropriate."))}
             names(DATAGG)[which(names(DATAGG)=="response")] <- "respagg"
             DATINTAGG <- merge(x=DATINTrepval, y=DATAGG, by.x="runsnorm", by.y="runsnorm")
             DATINTAGG$normresp <- DATINTAGG$response / DATINTAGG$respagg 
             NORMINFO <- paste("Data have been normalized by: dividing by", normfun, sep=" ")
           })
      
    return(list(NORMDAT=DATINTAGG, NORMINFO=NORMINFO))
    
  }

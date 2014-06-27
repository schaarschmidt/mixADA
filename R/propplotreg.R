propplotreg <-
function(DATINT, normfun="median", normop ="logdiff")
{

if(length(levels(DATINT$runsnorm))<3){return(list(DATAGGNF = NULL, #SFLM=NULL, SCT=NULL,
 CAPP=NULL, CAPREG=NULL))}else{

#print(str(DATINT))

switch(normfun,
"median"={fun<-median}, "mean"={fun<-mean})

if(normop=="logdiff"){
DATINT$lresponse <- log(DATINT$response) 
DATINTN <- DATINT[DATINT$normalizeby,]
DATAGGN <- aggregate(lresponse ~ runsnorm, data=DATINTN, fun)
names(DATAGGN)[2] <- "controls"
DATINTF <- DATINT[DATINT$subsetforfitting,]
DATAGGF <- aggregate(lresponse ~ runsnorm, data=DATINTF, fun)
names(DATAGGF)[2] <- "samples" 
DATAGGNF <- merge(x=DATINTN, y=DATINTF, by.x="runsnorm", by.y="runsnorm")

}else{if(normop %in% c("diff", "ratio")){
DATINTN <- DATINT[DATINT$normalizeby,]
DATAGGN <- aggregate(response ~ runsnorm, data=DATINTN, fun)
names(DATAGGN)[2] <- "controls"
DATINTF <- DATINT[DATINT$subsetforfitting,]
DATAGGF <- aggregate(response ~ runsnorm, data=DATINTF, fun)
names(DATAGGF)[2] <- "samples" 
}}

DATAGGNF <- merge(x=DATAGGN, y=DATAGGF, by.x="runsnorm", by.y="runsnorm")
SFLM <- summary(lm(samples ~ controls, data=DATAGGNF))
#SCT <- cor.test( ~ samples + controls, data=DATAGGNF, method="spearman") 
if(normop=="logdiff"){
CAPP <- paste("Plot of: ", normfun, " of samples vs ", normfun, "of controls, within each run, after log-transformation.")
CAPREG <- paste("Linear regression of ", normfun, " of samples in dependency on ", normfun, " of controls (within each run), after log-transformation, and Spearmans rank correlation.")
#CAPCOR <- paste("Spearman correlation of ", normfun, " of samples vs ", normfun, "of controls (within each run), after log-transformation.")
}else{
CAPP <- paste("Proportionality plot: ", normfun, "of samples vs ", normfun, "of controls, within each run.")
CAPREG <- paste("Linear regression of ", normfun, " of samples in dependency on ", normfun, " of controls (within each run), and Spearmans rank correlation.")
#CAPCOR <- paste("Spearman correlation of ", normfun, " of samples vs ", normfun, "of controls (within each run).")
}


return(list(DATAGGNF = DATAGGNF, SFLM=SFLM, #SCT=SCT,
 CAPP=CAPP, CAPREG=CAPREG))
}
}

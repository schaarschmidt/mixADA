adaCCP <-
function(fitk2, rdat, ccplevel=0.99, resp, nrdefinition=c("modelclass"), 
         comparison=c("percinhib","ratio"), aggfun=c("mean", "median"), runsmodel)
{

aggfun <- match.arg(aggfun)
comparison <- match.arg(comparison)
switch(aggfun, "mean"={AGGFUN<-mean}, "median"={AGGFUN<-median})

if(is.null(runsmodel)){

DATunspiked <- fitk2$DATINT
RHSunspiked <- paste(c("sampleID", "cluster"), collapse="+")
FORMunspiked <- as.formula(paste("response", "~", RHSunspiked, sep=" "))
DATunspikedm <- aggregate(FORMunspiked, data=DATunspiked, FUN=AGGFUN)
 
DATspiked <- droplevels(rdat$DATINT[rdat$DATINT$subsetspiked,])
RHSspiked <- paste(c("sampleID"), collapse="+")
FORMspiked<- as.formula(paste("response", "~", RHSspiked, sep=" "))
DATspikedm <- aggregate(FORMspiked, data=DATspiked, FUN=AGGFUN)

DATCOMP <- merge(x=DATunspikedm, y=DATspikedm, by=c("sampleID"))

respunspiked<-paste("response", ".x", sep="")
respspiked<-paste("response", ".y", sep="")

snam <- paste(resp, "spiked",sep=".")
unam <- paste(resp, "unspiked",sep=".")

PROBS <- c(1-ccplevel, ccplevel)
QNAMS <- paste(c("lower", "upper"), signif(ccplevel, 4), "perc", sep="")

switch(comparison,
"percinhib"={
DATCOMP$percinhib <- 100*(1-DATCOMP[,respspiked]/DATCOMP[,respunspiked])
ynam <- paste("percent inhibition (= 100*(1 - ", snam, "/" , unam, ") %)", sep="")
estimates <- c(median(DATCOMP$percinhib[DATCOMP$cluster=="nonresponder"]),
quantile(x=DATCOMP$percinhib[DATCOMP$cluster=="nonresponder"], probs=PROBS),
quantile(x=DATCOMP$percinhib, probs=PROBS))
},

"ratio"={DATCOMP$ratio <- DATCOMP[,respspiked]/DATCOMP[,respunspiked]
ynam <- paste("ratio (=", snam, "/" , unam, ")", sep="")
estimates <- c(median(DATCOMP$ratio[DATCOMP$cluster=="nonresponder"]),
quantile(x=DATCOMP$ratio[DATCOMP$cluster=="nonresponder"], probs=PROBS),
quantile(x=DATCOMP$ratio, probs=PROBS))
}) 


limtabnam <- c("median", paste(QNAMS, "nonresp", sep="."), paste(QNAMS, "all", sep="."))

limtab<-data.frame("group"=rep(c("nonresponder","all"), c(3,2)), "estimated"=factor(limtabnam, levels=limtabnam), value=estimates)

infoccpmeasure <- paste("For original observations (before normalization), ",aggfun,"s have been computed at each level of sampleID. ", ynam, " has been computed after matching ", aggfun,"s from spiked and unspiked samples.", sep="")


}else{

DATunspiked <- fitk2$DATINT

#cat("unspiked, fitk2 \n");print(str(DATunspiked))

RHSunspiked <- paste(c("sampleID", "runsmodel", "cluster"), collapse="+")
FORMunspiked<- as.formula(paste("response", "~", RHSunspiked, sep=" "))
DATunspikedm <- aggregate(FORMunspiked, data=DATunspiked, FUN=AGGFUN)

#cat("spiked before subset, rdat \n");print(str(rdat$DATINT))

DATspiked <- droplevels(rdat$DATINT[rdat$DATINT$subsetspiked,])
#cat("spiked, rdat \n");print(str(DATspiked))
RHSspiked <- paste(c("sampleID", "runsmodel"), collapse="+")
FORMspiked<- as.formula(paste("response", "~", RHSspiked, sep=" "))
DATspikedm <- aggregate(FORMspiked, data=DATspiked, FUN=AGGFUN)

DATCOMP <- merge(x=DATunspikedm, y=DATspikedm, by=c("sampleID", "runsmodel"))

respunspiked<-paste("response", ".x", sep="")
respspiked<-paste("response", ".y", sep="")

snam <- paste(resp, "spiked",sep=".")
unam <- paste(resp, "unspiked",sep=".")

PROBS <- c(1-ccplevel, ccplevel)
QNAMS <- paste(c("lower", "upper"), signif(ccplevel, 4), "perc", sep="")

switch(comparison,
"percinhib"={
DATCOMP$percinhib <- 100*(1-DATCOMP[,respspiked]/DATCOMP[,respunspiked])
ynam <- paste("percent inhibition (= 100*(1 - ", snam, "/" , unam, ") %)", sep="")
estimates <- c(median(DATCOMP$percinhib[DATCOMP$cluster=="nonresponder"]),
quantile(x=DATCOMP$percinhib[DATCOMP$cluster=="nonresponder"], probs=PROBS),
quantile(x=DATCOMP$percinhib, probs=PROBS))

},

"ratio"={DATCOMP$ratio <- DATCOMP[,respspiked]/DATCOMP[,respunspiked]
ynam <- paste("ratio: ", snam, "/" , unam, sep="")
estimates <- c(median(DATCOMP$ratio[DATCOMP$cluster=="nonresponder"]),
quantile(x=DATCOMP$ratio[DATCOMP$cluster=="nonresponder"], probs=PROBS),
quantile(x=DATCOMP$ratio, probs=PROBS))
}) 



limtabnam <- c("median", paste(QNAMS, "nonresp", sep="."), paste(QNAMS, "all", sep="."))

limtab<-data.frame("group"=rep(c("nonresponder","all"), c(3,2)), "estimated"=factor(limtabnam, levels=limtabnam), value=estimates)

infoccpmeasure <- paste("For original observations (before normalization), ", aggfun,"s have been computed at each level of sampleID and each run. ", ynam, " has been computed after matching ", aggfun,"s from spiked and unspiked samples within each run.", sep="")

}

limitexplanation <- paste("'perc': (nonparametric) percentile of ", ynam, "for those sampleIDs that were classified as 'nonresponder' in the 2-component mixture model;",
"'perc.all': (nonparametric) percentile of ", ynam, "for all sampleIDs, irrespective of the classificiation in the 2-component mixture model.",
 sep="")

return(list(DATCOMP=DATCOMP, ynam=ynam, limtab=limtab, comparison=comparison, infoccpmeasure=infoccpmeasure, limitexplanation=limitexplanation))

}

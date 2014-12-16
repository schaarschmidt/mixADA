adapcheckdatainputsimple <-
function(dat, response, treatment, sampleID, forfitting, runsmodel=NULL, spiked=NULL)
{

if(length(response) > 1){stop("Only one column of the dataset should be specified as response.")}
if(length(treatment) > 1){stop("Only one column of the dataset should be specified as treatment.")}

if(response==treatment){stop("The same column of the data set has been chosen to be response and treatment.")}


DATINT <- dat[,c(response, treatment)]
names(DATINT) <- c("response","treatment")
DATINT$treatment <- factor(DATINT$treatment)

if(!is.numeric(DATINT[,"response", drop=TRUE]) & !is.integer(DATINT[,"response", drop=TRUE])){stop("Selected response variable must be numeric (or integer)!")}


# run definition for the normalization part

#if(any(runsnorm %in% sampleID)){
#wrs <- which(runsnorm %in% sampleID)
#stop(paste(paste(runsnorm[wrs],collapse=", "), ": Variables defining technical replications (runs for normalization) is not allowed to define individual samples (sampleID).", sep=""))}

#if(any(runsnorm == response)){
#wrs <- which(runsnorm %in% response)
#stop(paste(paste(runsnorm[wrs],collapse=", "), ": Variables defining technical replications (runs for normalization) is not allowed to define response variable (measurements to be analyzed) at the same time.", sep=""))}

#if(any(runsnorm == treatment)){
#wrs <- which(runsnorm == treatment)
#stop(paste(paste(runsnorm[wrs],collapse=", "), ": Variables defining technical replications (runs for normalization) should not be selected as variable defining treatment types (for normalization, fitting, etc).", sep=""))}


#if(length(runsnorm)==1){DATINT$runsnorm<-dat[,runsnorm]}else{
#runsnormlist<-lapply(as.list(dat[,runsnorm]), function(x){as.factor(x)})
#DATINT$runsnorm<- interaction(runsnormlist, drop=TRUE)
#}

#DATINT$runsnorm <- factor(DATINT$runsnorm, levels=unique(DATINT$runsnorm))

# sampleID definition, checks with subset for SCP/fitting of mixture models

if(any(response %in% sampleID)){stop("The column of the data set that has been chosen as response, has also been selected as the column distinguishing the individual biological samples.")}
if(any(treatment %in% sampleID)){stop("The column of the data set that has been chosen to distinguish sample types, has also been selected as the column distinguishing the individual biological samples.")}

if(length(sampleID)==1){DATINT$sampleID <- dat[,sampleID]}else{
sampleIDlist<-lapply(as.list(dat[,sampleID]), function(x){as.factor(x)})
DATINT$sampleID <- interaction(sampleIDlist, drop=TRUE)
}
DATINT$sampleID <- factor(DATINT$sampleID, levels=unique(DATINT$sampleID))

#if(!any(normalizeby %in% levels(DATINT$treatment))){stop("Level(s) specified in argument 'normalizeby' can not be found in variable treatment.")}
#DATINT$normalizeby <- DATINT$treatment %in% normalizeby

if(!any(forfitting %in% levels(DATINT$treatment))){stop("Level(s) specified in argument 'forfitting' can not be found in variable treatment.")}
DATINT$subsetforfitting <- DATINT$treatment %in% forfitting

lsID <- levels(droplevels(DATINT[DATINT$subsetforfitting,]$sampleID))
nlsID <- length(lsID)

if(nlsID<=3){ stop(paste("The number of levels in the variable distinguishing biological samples (sampleID) in the subset for fitting, is only 3 or less: (", paste(lsID, collapse=", ") , ")." , sep=""))}


DATINT$datause <- "not used"
DATINT$datause[DATINT$subsetforfitting] <- "for fitting"
#DATINT$datause[DATINT$normalizeby] <- "for normalization"

#TAB <- table(DATINT[,c("runsnorm", "normalizeby")])

#if(any(TAB[,1]==0)){stop(paste("No observations for normalization could be found in 1 or several run(s): ", paste(rownames(TAB[which(TAB[,1]==0),]), collapse=", "),".", sep=""))}

# run definition for the model fitting part
# necessary only after normalization, hence, initially mandatory 

if(!is.null(runsmodel)){

if(any(runsmodel %in% sampleID)){
wrms <- which(runsmodel %in% sampleID)
stop(paste(paste(runsmodel[wrms],collapse=", "), ": Variable defining technical replications (runs for model fitting) is not allowed to define individual samples (sampleID) at the same time.", sep=""))}

if(any(runsmodel == treatment)){
wrms <- which(runsmodel == treatment)
stop(paste(paste(runsmodel[wrms],collapse=", "), ": Variable defining technical replications (runs for model fitting) should not be selected as variable defining treatment types (for normalization, fitting, etc).", sep=""))}

if(length(runsmodel)==1){DATINT$runsmodel<-dat[,runsmodel]}else{
runsmodellist <- lapply(as.list(dat[,runsmodel]), function(x){as.factor(x)})
DATINT$runsmodel <- interaction(runsmodellist, drop=TRUE)
}

DATINT$runsmodel <- factor(DATINT$runsmodel, levels=unique(DATINT$runsmodel))
}

# Spiked data

if(!is.null(spiked)){

if(!any(spiked %in% levels(DATINT$treatment))){stop("Level(s) specified in argument 'forfitting' can not be found in variable treatment.")}
DATINT$subsetspiked <- DATINT$treatment %in% spiked

if(any(spiked %in% forfitting)){stop("Levels(s) defining inhibited (spiked) sample types must not be the same as levels defining uninhibited (unspiked) levels.")}
#if(any(spiked %in% normalizeby)){stop("Levels(s) defining inhibited (spiked) sample types must not be the same as levels defining negative controls for normalization.")}

DATINTspiked <- droplevels(DATINT[DATINT$subsetspiked,])

lspikedsID <- levels(DATINTspiked$sampleID)
nlspikedsID <- length(lspikedsID)

if(nlspikedsID<=3){ stop(paste("The number of levels in the variable distinguishing biological samples (sampleID) in the subset of spiked data is only 3 or less: (", paste(lspikedsID, collapse=", ") , ")." , sep=""))}

wsIDsp <- which(!lsID %in% lspikedsID)
if(length(wsIDsp)>0){warning(paste("Some levels of sampleID in the subset for fitting models can not be found in the subset of spiked data (",paste(lsID[wsIDsp], collapse=", ") ,")." , sep=""))}

DATINT$datause[DATINT$subsetspiked] <- "spiked"
}

#textnormlev <- paste(" of negative control (", paste(normalizeby, collapse=", "), ") as defined in variable ", treatment, collapse="")

#levrunsnorm <- levels(DATINT$runsnorm)
#nlevrunsnorm <- length(levrunsnorm)
#if(nlevrunsnorm > 4){textlevrunsnorm <- paste(c(levrunsnorm[1:2], " ...", levrunsnorm[nlevrunsnorm]), collapse=", ")}else{
#textlevrunsnorm <- paste(levrunsnorm, collapse=", ")
#}
#textnormunit <- paste(", within each of ", nlevrunsnorm ," technical replications (runs), defined as the (joint) levels of variable(s) ", paste(runsnorm, collapse=", "), ", (", textlevrunsnorm, ").", sep="" )

DATINT$datause <- as.factor(DATINT$datause)

datrna <- is.na(DATINT$response)

if(any(datrna)){
 naTAB <- table(droplevels(DATINT[datrna,])$sampleID)
 nna <- sum(datrna) 
 nachar <- paste(paste(names(naTAB), as.vector(naTAB), sep=":"), collapse=", ")
 namessage <- paste(nna, " values omitted due to missing observations in response variable (", response, "), sampleID: ", nachar, ".")
 DATINT <- droplevels(DATINT[!datrna,])
}
else{namessage <- ""}

return(list("DATINT"=DATINT, "NAMESSAGE"=namessage))

}

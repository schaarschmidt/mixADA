
# compute percentile after simplistic outlier exclusion

# data: data frame
# normresp: column name of response
# id: sampleID
# PROBS. probabilit of quantiles, to be passed to argument probs in function quantile

percalloutex <- function(data, normresp, id, PROBS)
{
  
obsq75 <- quantile(x=data[,normresp], probs=c(0.75))
obsiqr <- IQR(x=data[,normresp])
exclulim <- obsq75 + 1.5*obsiqr

wexclobs <- which(data[,normresp] > exclulim)

if(length(wexclobs) == 0){exclunote<-"no observations excluded as outliers"; xex <- data[,normresp] }else{
  exclun <- length(wexclobs)
  exclutab <- table(droplevels(data[wexclobs,id]))
  excluidn <- paste(paste(names(exclutab), as.integer(exclutab), sep=":"), collapse=", ")
  exclunote <- paste(exclun, "observations excluded because they exceed Q75+1.5*IQR=", signif(exclulim, digits=4), "namely from samples", excluidn, sep=" ")
  xex <- data[-wexclobs, normresp]
}

limit <- quantile(x=xex, probs=PROBS)

return(list(limit=limit, exclusionnote=exclunote))
}


adapperccutpoints <-
function(resadapmixmod, level=0.95, alternative="less", group=c("nonresponder", "responder","all"))
{
  group <- match.arg(group)
  alternative <- match.arg(alternative, choices=c("less","two.sided","greater")) 
  switch(alternative,
         "two.sided"={PINAM <- c(paste("lower", signif(1-(1-level)/2,3), sep=""), paste("upper", signif(1-(1-level)/2,3), sep="")); PROBS<- c((1-level)/2, 1-(1-level)/2)},
         "less"={PINAM <- paste("upper", signif(level,3), sep=""); PROBS <- level},
         "greater"={PINAM <- paste("lower", signif(level,3), sep=""); PROBS <- 1-level}
  )
  

  switch(group,       
         "nonresponder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wlower,]
           PPnonresponder <- resadapmixmod$DATINT$postproblower
          # PPsample <- rbinom(n=length(PPnonresponder), size=1, prob=PPnonresponder)
          # empquantpps <- quantile(resadapmixmod$DATINT[PPsample==1,"normresp"], probs=PROBS)
           empquantppsB <- percsimppid(dat=resadapmixmod$DATINT, resp="normresp", id="sampleID", postprob="postproblower", PROBS=PROBS, B=1000)
           empquant <-  quantile(DAT$normresp, probs=PROBS)
           exclunote <- ""
         },
         "responder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wupper,]
           PPresponder <- 1-resadapmixmod$DATINT$postproblower
          # PPsample <- rbinom(n=length(PPresponder), size=1, prob=PPresponder)
          # empquantpps <- quantile(resadapmixmod$DATINT[PPsample==1,"normresp"], probs=PROBS)
           resadapmixmod$DATINT$postprobupper <- 1-resadapmixmod$DATINT$postproblower
           empquantppsB <- percsimppid(dat=resadapmixmod$DATINT, resp="normresp", id="sampleID", postprob="postprobupper", PROBS=PROBS, B=1000)
           empquant <-  quantile(DAT$normresp, probs=PROBS)
           exclunote <- ""
         },
         "all"={
           DAT <- resadapmixmod$DATINT
           empquant <-  quantile(DAT$normresp, probs=PROBS)
           empquantalloutex <-  percalloutex(data=DAT, normresp="normresp", id="sampleID", PROBS=PROBS)
           empquantoutex <- empquantalloutex$limit
           exclunote <- empquantalloutex$exclusionnote
         })
if(group %in% c("nonresponder","responder")){
NAMQ <- paste(PINAM, "perc", sep="")
#NAMQPPS <- paste(PINAM, "postwt.perc", sep="")
NAMQPPSB <- paste(PINAM, "postwt.perc", sep="")
VAL <- c(empquantppsB, empquant)
NAM <- c(NAMQPPSB, NAMQ)
}else{
NAMQ <- paste(PINAM, "perc", ".all", sep="")
NAMQOUTEX <- paste(PINAM, "perc.outlier.excl", ".all", sep="")
VAL <- c(empquant, empquantoutex)
names(VAL)<-NULL
NAM <- c(NAMQ, NAMQOUTEX)}
   
estlimitsd<-data.frame(
  value=VAL,
  group=factor(rep(group, length(VAL))),
  estimated=NAM
  )
attr(estlimitsd, which="exclunote") <- exclunote 
rownames(estlimitsd)<-NULL
  
  return(estlimitsd)
}

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
           PPsample <- rbinom(n=length(PPnonresponder), size=1, prob=PPnonresponder)
           empquantpps <- quantile(resadapmixmod$DATINT[PPsample==1,"normresp"], probs=PROBS)
           empquant <-  quantile(DAT$normresp, probs=PROBS)
         },
         "responder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wupper,]
           PPresponder <- 1-resadapmixmod$DATINT$postproblower
           PPsample <- rbinom(n=length(PPresponder), size=1, prob=PPresponder)
           empquantpps <- quantile(resadapmixmod$DATINT[PPsample==1,"normresp"], probs=PROBS)
           empquant <-  quantile(DAT$normresp, probs=PROBS)
         },
         "all"={
           DAT <- resadapmixmod$DATINT
           empquant <-  quantile(DAT$normresp, probs=PROBS)
           empquantpps <- empquant
         })
if(group %in% c("nonresponder","responder")){
NAMQ <- paste(PINAM, "emp.perc", sep="")
NAMQPPS <- paste(PINAM, "postwt.perc", sep="")
VAL <- c(empquantpps, empquant)
NAM <- c(NAMQPPS, NAMQ)
}else{
NAMQ <- paste(PINAM, "emp.perc", ".all", sep="")
VAL <- empquant
names(VAL)<-NULL
NAM <- NAMQ}
   
estlimitsd<-data.frame(
  value=VAL,
  group=factor(rep(group, length(VAL))),
  estimated=NAM
  )
  
  return(estlimitsd)
}

adaplmmintervals <-
function(resadapmixmod, design=c("c2", "h2", "c1", "h1", "y"), level=0.95, alternative="less", group=c("nonresponder", "responder","all"))
{
  
  group <- match.arg(group)
  alternative <- match.arg(alternative, choices=c("less","two.sided","greater"))
  design <- match.arg(design)  
  
  switch(alternative,
         "two.sided"={PINAM <- c(paste("lower", signif(1-(1-level)/2,3),"pred.limit", sep=""), paste("upper", signif(1-(1-level)/2,3), "pred.limit", sep=""))},
         "less"={PINAM <- paste("upper", signif(level,3),"pred.limit", sep="")},
         "greater"={PINAM <- paste("lower", signif(level,3),"pred.limit", sep="")}
  )
  
  switch(group,
         
         "nonresponder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wlower,]
           
         },
         "responder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wupper,]
           
         },
         "all"={
           DAT <- resadapmixmod$DATINT
           
         })


  
  #DAT<-droplevels(DAT)
  
  switch(design, 
  "c2"={
  FIT <- lmer(normresp ~ 1 + (1|sampleID) + (1|runsmodel) + (1|sampleID:runsmodel), data=DAT)
  PIE <- predint_lmer(fit=FIT, type=design, level=level, alternative=alternative)
  TAB <- table(droplevels(DAT[,c("sampleID", "runsmodel")]))
  VAL <- c(PIE$mu,PIE$predint)
  },
  "h2"={
  DAT$runssampleID <- droplevels(DAT$runsmodel:DAT$sampleID)
  FIT <- lmer(normresp ~ 1 + (1|runsmodel) + (1|runssampleID), data=DAT)
  PIE <- predint_lmer(fit=FIT, type=design, level=level, alternative=alternative)
  TAB <- table(droplevels(DAT[,c("sampleID", "runsmodel")]))
  VAL <- c(PIE$mu,PIE$predint)
  },
  "c1"={
    FIT <- lmer(normresp ~ 1 + (1|sampleID) + (1|runsmodel), data=DAT)
    PIE <- predint_lmer(fit=FIT, type=design, level=level, alternative=alternative)
    TAB <- table(droplevels(DAT[,c("sampleID", "runsmodel")]))
    VAL <- c(PIE$mu,PIE$predint)
  },
  "h1"={
    FIT <- lmer(normresp ~ 1 + (1|sampleID), data=DAT)
    PIE <- predint_lmer(fit=FIT, type=design, level=level, alternative=alternative)
    TAB <- table(droplevels(DAT[,c("sampleID")]))
    VAL <- c(PIE$mu,PIE$predint)
  },
  "y"={  
  FIT <- lm(normresp ~ 1, data=DAT)
  PIE <- predint_y(fit=FIT, type=design, level=level, alternative=alternative)
  TAB <- NULL
  VAL <- c(PIE$mu,PIE$predint)}
  )

  estlimitsd <- data.frame(
    value = VAL,
    group = factor(rep(group, length(VAL))),
    estimated = factor(c("mean", PINAM)))
  
  rownames(estlimitsd)<-NULL
  
  return(list(
    estlimitsd = estlimitsd,
    TAB = TAB,
    PIE = PIE,
    FIT = FIT,
    DAT = DAT,
    group=group, level=level, design=design
  ))
  
}

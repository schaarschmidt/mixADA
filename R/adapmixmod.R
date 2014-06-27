adapmixmod <-
function(DATINT, nrep=10, design=c("c2","h2"), varfix=c("no", "ran", "res", "bothranres"), aggfun="mean")
{
 varfix<- match.arg(varfix)
 design<- match.arg(design)

 DATINTFIT<-droplevels(DATINT[DATINT$subsetforfitting,])

 match.arg(aggfun, choices=c("mean","median"))
 switch(aggfun, "mean"={AGGFUN<-mean}, "median"={AGGFUN<-median})

switch(design, 
"h2"={

 DATINTFIT$runssampleID<- droplevels(DATINTFIT$runsmodel:DATINTFIT$sampleID)

# no initial clustering

  datsm <- aggregate(normresp ~ sampleID, data=DATINTFIT, AGGFUN)
  names(datsm)[2]<-"sampleIDmean"  
  datsm$initcl <- cutree(hclust(dist(datsm$sampleIDmean)), k=2)  
  DATINTFIT<-merge(y=DATINTFIT, x=datsm[, c("sampleID", "initcl")], by="sampleID")

ini1ID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=1, nrep = nrep,
                        model = FLXMRnestedlmm(random = list(~ 1, ~0+runssampleID)),
                        control = list(verbose = 0, iter.max = 2000, minprior = 0))

switch(varfix, "no"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runssampleID)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, different random effects, different residual variance."
},

"ran"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runssampleID), varFix = c(Random = TRUE, Residual = FALSE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, different residual variance."
}, 

"res"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runssampleID), varFix = c(Random = FALSE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, different random effects, equal residual variance."
},

"bothranres"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runssampleID), varFix = c(Random = TRUE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, equal residual variance."
})
},

"c2"={

# no initial clustering

  datsm <- aggregate(normresp ~ sampleID, data=DATINTFIT, AGGFUN)
  names(datsm)[2]<-"sampleIDmean"  
  datsm$initcl <- cutree(hclust(dist(datsm$sampleIDmean)), k=2)  
  DATINTFIT<-merge(y=DATINTFIT, x=datsm[, c("sampleID", "initcl")], by="sampleID")

ini1ID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=1, nrep = nrep,
                        model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel)),
                        control = list(verbose = 0, iter.max = 2000, minprior = 0))

switch(varfix, "no"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, different random effects, different residual variance."
},

"ran"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = FALSE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, different residual variance."
}, 

"res"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = FALSE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, different random effects, equal residual variance."
},

"bothranres"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, equal residual variance."
})
})


  bic12<-BIC(ini1ID, inimixID)
  
  if(bic12[1,"BIC"] < bic12[2,"BIC"]){ warning("A model with only 1 population provides a better fit to this data set."); note<-"A model with only 1 population provides a better fit to this data set."}else{note<-""}
  
  para <- parameters(inimixID)
  wlower <- order(para[1,], decreasing=FALSE)[1]
  wupper <- order(para[1,], decreasing=FALSE)[2]
  
  DATINTFIT$postproblower <- inimixID@posterior$scaled[,wlower]
  DATINTFIT$clusters <- as.factor(clusters(inimixID))
  DATINTFIT$cluster <- DATINTFIT$clusters
  clnames <- c("nonresponder", "responder")[c(wlower, wupper)]
  levels(DATINTFIT$cluster) <- clnames

  return(list(DATINT=DATINTFIT, para=para, wlower=wlower, wupper=wupper, labels=clnames, note=note, varmodelnote=varmodelnote, fitlist=list("k1"=ini1ID, "kk"=inimixID)) ) 
  
}

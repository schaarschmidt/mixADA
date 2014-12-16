adapmixmod <-
function(DATINT, nrep=10, design=c("c2", "h2", "c1", "h1"), varfix=c("no", "ran", "res", "bothranres"), aggfun="mean")
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
varmodelnote <- "Two-component mixture model with different means, different random effects, different residual variance. "
},

"ran"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = FALSE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, different residual variance. "
}, 

"res"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = FALSE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, different random effects, equal residual variance. "
},

"bothranres"={
inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                          model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = TRUE)), concomitant = FLXPmultinom(~initcl),
                          control = list(verbose = 0, iter.max = 2000, minprior = 0))
varmodelnote <- "Two-component mixture model with different means, equal random effects, equal residual variance. "
})
},

"h1"={
  
  DATINTFIT$runssampleID<- droplevels(DATINTFIT$runsmodel:DATINTFIT$sampleID)
  
  # no initial clustering
  
  #datsm <- aggregate(normresp ~ sampleID, data=DATINTFIT, AGGFUN)
  #names(datsm)[2]<-"sampleIDmean"  
  #datsm$initcl <- cutree(hclust(dist(datsm$sampleIDmean)), k=2)  
  #DATINTFIT<-merge(y=DATINTFIT, x=datsm[, c("sampleID", "initcl")], by="sampleID")
  
  ini1ID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=1, nrep = nrep,
                        control = list(verbose = 0, iter.max = 2000, minprior = 0))
  
  inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep, 
                            control = list(verbose = 0, iter.max = 2000, minprior = 0))
  #print(parameters(inimixID))
  varmodelnote <- "Two-component mixture model with different means, different random effects for samples, and different residual variance."

},

"c1"={
  
  # no initial clustering
  
  #datsm <- aggregate(normresp ~ sampleID, data=DATINTFIT, AGGFUN)
  #names(datsm)[2]<-"sampleIDmean"  
  #datsm$initcl <- cutree(hclust(dist(datsm$sampleIDmean)), k=2)  
  
  #DATINTFIT<-merge(y=DATINTFIT, x=datsm[, c("sampleID", "initcl")], by="sampleID")
  
  ini1ID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=1, nrep = nrep,
                        model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel)),
                        control = list(verbose = 0, iter.max = 2000, minprior = 0))
  
  switch(varfix, "no"={
    inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                            model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel)),
                            control = list(verbose = 0, iter.max = 2000, minprior = 0))
    varmodelnote <- "Two-component mixture model with different means, different random effects, different residual variance. "
  },
  
  "ran"={
    inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                            model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = FALSE)), 
                            control = list(verbose = 0, iter.max = 2000, minprior = 0))
    varmodelnote <- "Two-component mixture model with different means, equal random effects, different residual variance. "
  }, 
  
  "res"={
    inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                            model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = FALSE, Residual = TRUE)),
                            control = list(verbose = 0, iter.max = 2000, minprior = 0))
    varmodelnote <- "Two-component mixture model with different means, different random effects, equal residual variance. "
  },
  
  "bothranres"={
    inimixID <- initFlexmix(normresp ~ 1|sampleID, data=DATINTFIT, k=2, nrep = nrep,
                            model = FLXMRnestedlmm(random = list(~ 1, ~0+runsmodel), varFix = c(Random = TRUE, Residual = TRUE)),
                            control = list(verbose = 0, iter.max = 2000, minprior = 0))
    varmodelnote <- "Two-component mixture model with different means, equal random effects, equal residual variance. "
  })
}

)


  bic12 <- BIC(ini1ID, inimixID)
  
  if(bic12[1,"BIC"] < bic12[2,"BIC"]){
    warning("A model with only 1 population provides a better fit to this data set.");
    note <- paste("NOTE: A model with only 1 subgroup (BIC = ", signif(bic12[1,"BIC"], digits=6), ") provides a better fit to this data set than the model with 2 subgroups shown here (BIC = ", signif(bic12[2,"BIC"], digits=6), "). ", sep="")}else{
    note <- paste("A model with 1 subgroup (BIC = ", signif(bic12[1,"BIC"], digits=6), ") DOES NOT provide a better fit to this data set than the model with 2 subgroups shown here (BIC = ", signif(bic12[2,"BIC"], digits=6), "). ", sep="")}
  
  para <- parameters(inimixID)
  wlower <- order(para[1,], decreasing=FALSE)[1]
  wupper <- order(para[1,], decreasing=FALSE)[2]
  
  DATINTFIT$postproblower <- inimixID@posterior$scaled[,wlower]
  DATINTFIT$clusters <- as.factor(clusters(inimixID))
  DATINTFIT$cluster <- DATINTFIT$clusters
  clnames <- c("nonresponder", "responder")[c(wlower, wupper)]
  levels(DATINTFIT$cluster) <- clnames

  #print(bic12)
  #print(note)

  return(list(DATINT=DATINTFIT, para=para, wlower=wlower, wupper=wupper,
              labels=clnames, note=note, varmodelnote=varmodelnote, fitlist=list("k1"=ini1ID, "kk"=inimixID)) ) 
  
}

adapmixmodsampleID <-
function(DATINT, nrep=10, aggsamples=c("mean", "median") )
{
  aggsamples <- match.arg(aggsamples)
  
  switch(aggsamples, "mean"={FUN=mean}, "median"={FUN=median})
  
  DATINTFIT<-droplevels(DATINT[DATINT$subsetforfitting,])
  
           # no initial clustering
           datsm <- aggregate(cbind(normresp, response) ~ sampleID, data=DATINTFIT, FUN=FUN)
           datsm$initcl <- cutree(hclust(dist(datsm$normresp)), k=2)
          # print(str(datsm))

           ini1ID <- initFlexmix(normresp ~ 1, data=datsm, k=1, nrep = nrep,
                                 control = list(verbose = 0, iter.max = 2000, minprior = 0))
           

          inimixID <- initFlexmix(normresp ~ 1, data=datsm, k=2, nrep = nrep,
                                     concomitant = FLXPmultinom(~initcl),
                                     control = list(verbose = 0, iter.max = 2000, minprior = 0))
          varmodelnote <- paste("After computing ",aggsamples," for each sampleID: Two-component mixture model with different means, different between sampleID variance.", sep="")

                  
  aic12<-AIC(ini1ID, inimixID)
  
  if(aic12[1,"AIC"] < aic12[2,"AIC"]){ warning("A model with only 1 population provides a better fit to this data set."); note<-"A model with only 1 population provides a better fit to this data set."}else{note<-""}
  
  para <- parameters(inimixID)
  wlower <- order(para[1,], decreasing=FALSE)[1]
  wupper <- order(para[1,], decreasing=FALSE)[2]
  
  datsm$postproblower <- inimixID@posterior$scaled[,wlower]
  datsm$clusters <- as.factor(clusters(inimixID))
  datsm$cluster <- datsm$clusters
  clnames <- c("nonresponder", "responder")[c(wlower, wupper)]
  levels(datsm$cluster) <- clnames
  
  return(list(DATINT=datsm, para=para, wlower=wlower, wupper=wupper, labels=clnames, note=note, varmodelnote=varmodelnote, fitlist=list("k1"=ini1ID, "kk"=inimixID)) ) 
  
}

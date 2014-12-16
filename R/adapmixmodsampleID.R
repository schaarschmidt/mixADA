adapmixmodsampleID <-
function(DATINT, nrep=10, aggsamples=c("mean", "median") )
{
  aggsamples <- match.arg(aggsamples)
  
  switch(aggsamples, "mean"={FUN=mean}, "median"={FUN=median})
  
  DATINTFIT<-droplevels(DATINT[DATINT$subsetforfitting,])
  
           # no initial clustering
           datsm <- aggregate(cbind(normresp, response) ~ sampleID, data=DATINTFIT, FUN=FUN)
           #datsm$initcl <- cutree(hclust(dist(datsm$normresp)), k=2)
          # print(str(datsm))

          ini1ID <- initFlexmix(normresp ~ 1, data=datsm, k=1, nrep = nrep,
                                 control = list(verbose = 0, iter.max = 2000, minprior = 0))
           

          inimixID <- initFlexmix(normresp ~ 1, data=datsm, k=2, nrep = nrep,
                                     #concomitant = FLXPmultinom(~initcl),
                                     control = list(verbose = 0, iter.max = 2000, minprior = 0))
          varmodelnote <- paste("After computing ",aggsamples," for each sampleID: Two-component mixture model with different means, different between-sampleID variance.", sep="")

                  
  bic12 <- BIC(ini1ID, inimixID)
  
  if(bic12[1,"BIC"] < bic12[2,"BIC"]){
    warning("A model with only 1 subgroup provides a better fit to this data set."); 
    note <- paste("NOTE: A model with only 1 subgroup (BIC = ", signif(bic12[1,"BIC"], digits=6), ") provides a better fit to this data set than the model with 2 subgroups shown here (BIC = ", signif(bic12[2,"BIC"], digits=6), ").", sep="")}else{
    note <- paste("A model with 1 subgroup (BIC = ", signif(bic12[1,"BIC"], digits=6) ,") DOES NOT provide a better fit to this data set than the model with 2 subgroups shown here (BIC = ", signif(bic12[2,"BIC"], digits=6), ").", sep="")}
  
  para <- parameters(inimixID)
  wlower <- order(para[1,], decreasing=FALSE)[1]
  wupper <- order(para[1,], decreasing=FALSE)[2]
  
  datsm$postproblower <- inimixID@posterior$scaled[,wlower]
  datsm$clusters <- as.factor(clusters(inimixID))
  datsm$cluster <- datsm$clusters
  clnames <- c("nonresponder", "responder")[c(wlower, wupper)]
  levels(datsm$cluster) <- clnames
  
  #print(bic12)
  #print(note)
  
  return(list(DATINT=datsm, para=para, wlower=wlower, wupper=wupper,
              labels=clnames, note=note, varmodelnote=varmodelnote, fitlist=list("k1"=ini1ID, "kk"=inimixID)) ) 
  
}

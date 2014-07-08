


percsimppid<- function(dat, resp, id, postprob, PROBS, B=10)
{
  ppid <- unique(dat[, c(id, postprob)])
  if(any(table(ppid[,id])>1)){warning("different posterior probabilities observed within the same biological sample (sampleID)")}
  ridin <- replicate(n=B, expr=rbinom(n=nrow(ppid), size=1, prob=ppid[,postprob]))
  quantsim <- apply(X=ridin, MARGIN=2, FUN=function(x){
    idin <- droplevels(ppid[x==1,id])
    wdatidin <- which(dat[,id] %in% idin)
    quantile(x=dat[wdatidin, resp], probs=PROBS)})      
  quantm <- mean(quantsim)
  return(quantm)
}

#DAT<- data.frame(y=rnorm(2000,0,1), sid=factor(rep(paste("s",1:40, sep=""), each = 5)), ppnr=rep(rbeta(n=40, 0.2,0.2), each=5) ) 
#DAT
#percsimppid(dat=DAT, resp="y", id="sid", postprob="ppnr", PROBS=0.95, B=10000)


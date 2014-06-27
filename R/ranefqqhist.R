ranefqqhist <-
function(resadapintervals, outertitle="")
{
  FIT <- resadapintervals[["FIT"]]
  RAN <- ranef(FIT)
  NRAN <- length(RAN)
  NAMRAN <- names(RAN)
  RESID <- residuals(FIT)
  layout(matrix(1:(2*(NRAN+1)),nrow=2, byrow=FALSE))
  
  hist(RESID, main="residual error", sub="", xlab="")
  qqnorm(RESID, main="residual error"); qqline(RESID)
  for(i in 1:NRAN){
    hist(RAN[[i]][[1]], main=NAMRAN[i], sub="", xlab="") 
    qqnorm(RAN[[i]][[1]], main=NAMRAN[i]); qqline(RAN[[i]][[1]])
  }
  
  mtext(side=3, line=-1.2, outer=TRUE, paste(outertitle," Histograms and Q-Q-plots of random effects, group: ", resadapintervals$group, sep=""))
  
}

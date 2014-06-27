qqhist_y <-
function(resadapintervals, outertitle="")
{
  FIT <- resadapintervals[["FIT"]]
  RESID <- residuals(FIT)
  layout(matrix(1:2,nrow=1))
  
  hist(RESID, main="residual error", sub="", xlab="")
  qqnorm(RESID, main="residual error"); qqline(RESID)

  mtext(side=3, line=-1.2, outer=TRUE, paste(outertitle," Histogram and Q-Q-plot residuals, group: ", resadapintervals$group, sep=""))
  
}

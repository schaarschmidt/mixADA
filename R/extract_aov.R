extract_aov <-
function(fit){

aovtab <- anova(fit)

nam <- rownames(aovtab)
msi <- aovtab[,"Mean Sq"]
dfi <- aovtab[,"Df"]
ni <- c(unlist(lapply(fit$xlevels, length)), "total"=nrow(fit$model))

names(msi) <- nam
names(dfi) <- nam

muest <- mean(fit$model[,1])

return(list("msi"=msi, "dfi"=dfi, "ni"=ni, "mu"=muest))

}

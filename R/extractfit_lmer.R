extractfit_lmer <-
function(x)
{
mu <- fixef(x)
sfitx <- summary(x)
ni <- c(sfitx$ngrps, "total"=nrow(x@frame))
vari <- c(unlist(VarCorr(x)), sfitx$sigma^2)
return(list(mu=mu, ni=ni, vari=vari))
}

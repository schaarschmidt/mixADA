predint <-
function(mu, weighti, vari, dfi, level=0.95, alternative="two.sided")
{

# general Welch Satterthwaite formula
# vari: variances to be combined
# dfi: degrees of freedom
# weights: weights in the linear combination of variances

varsum <- sum(vari*weighti)
dfsum <- dfsatt(vari=vari, dfi=dfi, weighti=weighti)

switch(alternative,
"two.sided"={tquant <- qt(p=c((1-level)/2, 1-(1-level)/2), df=dfsum); nam<-c("lower","upper")},
"less"={tquant <- qt(p=1-(1-level), df=dfsum); nam<-"upper" },
"greater"={tquant <- qt(p=(1-level), df=dfsum); nam<-"lower" })

predint <- mu + tquant*sqrt(varsum)
names(predint) <- nam

return(list(
predint=predint,
dfS=dfsum,
tquant=tquant, 
stderr=sqrt(varsum)))
}

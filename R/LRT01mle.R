LRT01mle <-
function(resp, rhs, dat, LLest)
{
dat["respt0"]<-boxcoxw(dat[, resp], lambda=0)
FORM0 <- as.formula(paste("respt0", rhs, sep=""))
TEMP0 <- lmer(FORM0, data=dat, REML=FALSE)
LL0<-logLik(TEMP0, REML=FALSE)

dat["respt1"]<-boxcoxw(dat[, resp], lambda=1)
FORM1 <- as.formula(paste("respt1", rhs, sep=""))
TEMP1 <- lmer(FORM1, data=dat, REML=FALSE)
LL1<-logLik(TEMP1, REML=FALSE)

TL0 <- -2*(LL0-LLest$LLmax)
pTL0<- 1-pchisq(TL0, df=1)

TL1 <- -2*(LL1-LLest$LLmax)
pTL1<- 1-pchisq(TL1, df=1)

hyp <- c("Normal (lambda=1)", "Lognormal (lambda=0)" )
alt <- c("Dev. from Normal", " Dev. from Lognormal" )
tLRT <- c(TL1, TL0)
pLRT <- c(pTL1, pTL0)

LL <- c(LL1, LL0, LLest$LLmax)
lambda <- c(1, 0, LLest$lmax)
explanation <- c("Assumption: Normal distribution", "Assumption: Lognormal distribution", "Estimate*")
dest <- data.frame(lambda=lambda, LogLikelihood=LL)
rownames(dest) <- explanation
attr(dest, which="caption") <- LLest$note
dtest <- data.frame("H0"=hyp, "HA"=alt, "statLRT"=tLRT, "Pr(>chi(df=1))"=pLRT)
names(dtest)<-c("H0", "HA", "statLRT", "Pr(>chi(df=1))")
attr(dtest, which="caption") <- "LRT for normality and lognormality in mixed model (Gurka, Edwards, Nylander-French, 2007)"

return(list(info=dest, test=dtest))

}

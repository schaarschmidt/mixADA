lambdamlelm <-
function(resp, rhs, dat, lamin=-5, lamax=5, lastep=0.01)
{
laseq <- seq(from=lamin, to=lamax, by=lastep)
LLML <- numeric(length=length(laseq))

for(i in seq(along.with=laseq))
{
dat[,"resptrans"] <- boxcoxw(y=dat[,resp], lambda=laseq[i])
FORM <- as.formula(paste("resptrans", rhs, sep=""))
TEMP <- lm(FORM, data=dat)
LLML[i] <- logLik(TEMP)
}

wmax <- which.max(LLML)
lmax <- laseq[wmax]
LLmax <- LLML[wmax]

note <- paste("Box-Cox lambda, found by grid search from ",lamin," to ", lamax, " by increment ", lastep, sep="")

return(list(lmax=lmax, LLmax=LLmax, lambda=laseq, LLlambda=LLML, note=note))

}

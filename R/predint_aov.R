predint_aov <-
function(fit, type, level=0.95, alternative="two.sided",...)
{
type <- match.arg(type, choices=c("c1","c2", "h1", "h2", "c2h1"))

paraaov <- extract_aov(fit)

switch(type,
"c1"={msiwi <- msiweight_c1(paraaov)},
"c2"={msiwi <- msiweight_c2(paraaov)},
"h1"={msiwi <- msiweight_h1(paraaov)},
"h2"={msiwi <- msiweight_h2(paraaov)},
"c2h1"={msiwi <- msiweight_c2h1(paraaov,...)})

PI<-predint(mu=paraaov$mu, weighti=msiwi$weightisc, vari=msiwi$msisc, dfi=paraaov$dfi, level=level, alternative=alternative)

out<-c(PI, paraaov, msiwi, list(type=type))
return(out)
}

predint_lmer <-
function(fit, type, level=0.95, alternative="two.sided",...)
{
type <- match.arg(type, choices=c("c1", "c2", "h1", "h2", "c2h1"))

paralmer <- extractfit_lmer(fit)

switch(type,
"c1"={dflmer <- df_c1(paralmer)},
"c2"={dflmer <- df_c2(paralmer)},
"h1"={dflmer <- df_h1(paralmer)},
"h2"={dflmer <- df_h2(paralmer)},
"c2h1"={dflmer <- df_c2h1(paralmer,...)})

weighti <- 1 + 1/paralmer$ni

PI<-predint(mu=paralmer$mu, weighti=weighti, vari=paralmer$vari, dfi=dflmer, level=level, alternative=alternative)

out<-c(PI, paralmer, list(dfi=dflmer, type=type))
return(out)
}

predint_y <-
function(fit, level=0.95, alternative="two.sided",...)
{
  
  paralm <- extract_y(fit)
  df <- df_y(fit)
  weight <- 1 + 1/paralm$ni
  
  switch(alternative,
         "two.sided"={tquant <- qt(p=c((1-level)/2, 1-(1-level)/2), df=df); nam<-c("lower","upper")},
         "less"={tquant <- qt(p=1-(1-level), df=df); nam<-"upper" },
         "greater"={tquant <- qt(p=(1-level), df=df); nam<-"lower" })
  
  predint <- paralm$mu + tquant*sqrt(paralm$vari)

  PI<-list(
    predint=predint,
    dfS=df,
    tquant=tquant, 
    stderr=sqrt(sqrt(paralm$vari)))

  out<-c(PI, paralm, df=df, list(type="y"))
  return(out)
}

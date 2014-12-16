adaplmboxcox <-
function(resadapmixmod, normop, group=c("nonresponder", "responder","all"))
{
  
  group <- match.arg(group)

  
  switch(group,
         
         "nonresponder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wlower,]
           
         },
         "responder"={
           DAT <- resadapmixmod$DATINT[resadapmixmod$DATINT$clusters==resadapmixmod$wupper,]
           
         },
         "all"={
           DAT <- resadapmixmod$DATINT
           
         })

  if(normop %in% c("logdiff", "log")){DAT$btnormresp <- exp(DAT$normresp); btnote <- "backtransformed (exp) response (after normalization and classification)" }else{
          mnr <- min(DAT$normresp)
          if(sign(mnr)==-1){DAT$btnormresp <- (DAT$normresp + abs(mnr) + diff(range(DAT$normresp))/1000); btnote <- "response (after normalization, classification and shifting to positive values)"}
          if(sign(mnr)==0){DAT$btnormresp <- (DAT$normresp + diff(range(DAT$normresp))/1000); btnote <- "response (after normalization, classification and shifting to positive values)"}
          if(sign(mnr)==1){DAT$btnormresp <- DAT$normresp; btnote <- "response (after normalization and classification)"}
          }
  

  RHS <- " ~ 1"
   LEST <- lambdamlelm(resp="btnormresp", rhs=RHS, dat=DAT, lamin=-5, lamax=5, lastep=0.01)
   LTEST <- LRT01mlelm(resp="btnormresp", rhs=RHS, dat=DAT, LLest=LEST)


tabest <- LTEST$info
tabestcap <- paste("3rd line: ", attr(LTEST$info, which="caption"), " for ", btnote , sep="")
tabtest <- LTEST$test
tabtestcap <- attr(LTEST$test, which="caption")
header <- paste("Box-Cox-Lambda and LRT for normality and lognormality for sampleID means (", group, ")", sep="")

OUT <- list(tabest=tabest, tabestcap=tabestcap, tabtest=tabtest, tabtestcap=tabtestcap, header=header)

return(OUT)
  
}

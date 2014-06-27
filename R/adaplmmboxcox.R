adaplmmboxcox <-
function(resadapmixmod, design=c("c2","h2"), normop, group=c("nonresponder", "responder","all"))
{
  
  group <- match.arg(group)
  design <- match.arg(design)  

  
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

  if(normop=="logdiff"){DAT$btnormresp <- exp(DAT$normresp); btnote <- "backtransformed (exp) response (after normalization and classification)" }else{
          mnr <- min(DAT$normresp)
          if(sign(mnr)==-1){DAT$btnormresp <- (DAT$normresp + abs(mnr) + diff(range(DAT$normresp))/1000); btnote <- "response (after normalization, classification and shifting to positive values)"}
          if(sign(mnr)==0){DAT$btnormresp <- (DAT$normresp + diff(range(DAT$normresp))/1000); btnote <- "response (after normalization, classification and shifting to positive values)"}
          if(sign(mnr)==1){DAT$btnormresp <- DAT$normresp; btnote <- "response (after normalization and classification)"}
          }

  switch(design, 
  "c2"={
   RHS <- " ~ 1 + (1|sampleID) + (1|runsmodel) + (1|sampleID:runsmodel)"
   LEST <- lambdamle(resp="btnormresp", rhs=RHS, dat=DAT, lamin=-3, lamax=3, lastep=0.1)
   LTEST <- LRT01mle(resp="btnormresp", rhs=RHS, dat=DAT, LLest=LEST)
  },
  "h2"={
   DAT$runssampleID <- droplevels(DAT$runsmodel:DAT$sampleID)
   RHS <- " ~ 1 + (1|runsmodel) + (1|runssampleID)"
   LEST <- lambdamle(resp="btnormresp", rhs=RHS, dat=DAT, lamin=-3, lamax=3, lastep=0.1)
   LTEST <- LRT01mle(resp="btnormresp", rhs=RHS, dat=DAT, LLest=LEST)
})



tabest <- LTEST$info
tabestcap <- paste("*) ", attr(LTEST$info, which="caption"), " for ", btnote , sep="")
tabtest <- LTEST$test
tabtestcap <- attr(LTEST$test, which="caption")
header <- "Box-Cox-Lambda and LRT for normality and lognormality in mixed effects mixture model"

OUT <- list(tabest=tabest, tabestcap=tabestcap, tabtest=tabtest, tabtestcap=tabtestcap, header=header)

print(OUT)

return(OUT)
}

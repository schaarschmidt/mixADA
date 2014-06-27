

shinyServer(function(input, output){
  
  datasetInput <- reactive(x={
    inpd <- input$datafile
    if (is.null(inpd)){
      return(NULL)
    } else {

      return(read.csv(inpd$datapath))
    }
  })
  
  # return column-names of the imported data set
  cnames <- reactive(x={
    if (is.null(datasetInput())) {cnames <- " "}else{cnames <- names(datasetInput())}
  })

  # return column-names of the imported data set which correspond to numeric or integer variables
  numcnames <- reactive(x = {
    if (is.null(datasetInput())) {numcnames <- " "}else{
    cnum <- unlist(lapply(X=as.list(datasetInput()), FUN=function(x){is.numeric(x)|is.integer(x)}))
     #print(cnum)
    if(!any(cnum)){stop("The imported data set contains no column with numeric of integer variables!")}
    numcnames <- names(datasetInput())[cnum]
    }
  })

# return levels of the column selected as treatment variable in the imported data set
treatlevels <- reactive(x = {
  if (is.null(datasetInput())){
    nnames <- " "
  } else {
    if (is.null(input$treatment)||input$treatment == " "){nnames <- " "}else{nnames <- levels(as.factor(datasetInput()[,input$treatment]))}
  }
})  



output$response <- renderUI(expr = {    
  selectInput("response", label="Response variable", choices=c(" ", numcnames()), multiple=FALSE)    
})

output$treatment <- renderUI(expr = {    
  selectInput("treatment", label="Sample type: Variable distinguishing biological samples, negative controls, etc.:", choices=c(cnames()), multiple=FALSE)    
})


output$tfornormalization <- renderUI(expr = {
  selectInput("tfornormalization", label="Sample type level for normalization (negative controls)", choices=c(" ", treatlevels()), multiple=TRUE)    
})

output$tforfitting <- renderUI(expr = {
  selectInput("tforfitting", label="Sample type level(s) indicating (uninhibited, unspiked) biological samples", choices=c(" ", treatlevels()), multiple=TRUE)    
})

output$runsnorm <- renderUI(expr = {    
  selectInput("runsnorm", label="Runs: Variable(s) defining technical units within which normalization should be done", c(" ", cnames()), multiple=TRUE)    
})

output$sampleID <- renderUI(expr = {    
  selectInput("sampleID", label="SampleID: Variable(s) distinguishing individuals in the biological samples:", c(" ", cnames()), multiple=TRUE)    
})


 #


refinedatpp <- reactive(x = {
  if(is.null(datasetInput())||any(c(is.null(input$response), is.null(input$treatment), is.null(input$runsnorm), is.null(input$sampleID), is.null(input$tfornormalization), is.null(input$tforfitting)))||any(c(input$response, input$treatment, input$runsnorm, input$sampleID, input$tfornormalization, input$tforfitting) == " ")){RDAT<-NULL}else{
  RDAT <- adapcheckdatainput(dat=datasetInput(), response=input$response, treatment=input$treatment, runsnorm=input$runsnorm, sampleID=input$sampleID,
                     normalizeby=input$tfornormalization, forfitting=input$tforfitting, runsmodel=input$runsmodel)}
return(RDAT)
})


PPRC <- reactive(x = {
  if(is.null(refinedatpp())){return(NULL)} else {
  if(length(levels(refinedatpp()$DATINT$runsnorm))<3){return(NULL)} else {
    return(propplotreg(DATINT=refinedatpp()$DATINT, normop=input$normop, normfun=input$normfun))}}
})


output$cappropplot <- renderText(expr = {if(is.null(PPRC())){return("")}else{return(paste(PPRC()$CAPP))}})

output$propplot <- renderPlot(expr = {
  if(is.null(PPRC())){return(NULL)} else {
  if(length(levels(refinedatpp()$DATINT$runsnorm))<3){return(NULL)} else {
    op1 <- ggplot(data=PPRC()$DATAGGNF, aes(y=samples, x=controls)) + geom_point(aes(colour=runsnorm)) + 
           geom_abline(intercept = PPRC()$SFLM$coefficients[1,1], slope=PPRC()$SFLM$coefficients[2,1])
    print(op1)
  }}
})


#output$capreg <- renderText(expr = {if(is.null(PPRC())){return(NULL)}else{return(paste(PPRC()$CAPREG))}})
#output$ppreg <- renderPrint(expr = {if(is.null(PPRC())){return(NULL)}else{return(print(PPRC()$SFLM))}})

#output$capcor <- renderText(expr = {if(is.null(PPRC())){return(NULL)}else{return(paste(PPRC()$CAPCOR))}})
#output$ppcor <- renderPrint(expr = {if(is.null(PPRC())){return(NULL)}else{return(print(PPRC()$SCT))}})




refinedata <- reactive(x = {
  if(is.null(datasetInput())||any(c(is.null(input$response), is.null(input$treatment), is.null(input$runsnorm), is.null(input$sampleID), is.null(input$tfornormalization), is.null(input$tforfitting)))||any(c(input$response, input$treatment, input$runsnorm, input$sampleID, input$tfornormalization, input$tforfitting) == " ")){RDAT<-NULL}else{
  RDAT <- adapcheckdatainput(dat=datasetInput(), response=input$response, treatment=input$treatment, runsnorm=input$runsnorm, sampleID=input$sampleID,
                     normalizeby=input$tfornormalization, forfitting=input$tforfitting, runsmodel = input$runsmodel
)
  normDAT <- normalize(DATINT=RDAT$DATINT, normop=input$normop, normfun=input$normfun)
  RDAT$DATINT <- normDAT$NORMDAT
  OUTNORMINFO<- paste( normDAT$NORMINFO, RDAT$textnormlev, RDAT$textnormunit, sep=" ")
  RDAT$NORMINFO <- OUTNORMINFO
}
return(RDAT)
})


output$normalizationplot <- renderPlot(expr = {
  if (is.null(refinedata())){
    return(NULL)
  } else {
    op1 <- ggplot(data=refinedata()$DATINT, aes(x=sampleID, y=normresp, colour=datause)) + geom_point() + facet_wrap(~runsnorm) + ylab("Normalized response") + 
    scale_colour_discrete(name="Data:" ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
    print(op1)
  }
})

#output$normalizationinfo <- refinedata()$NORMINFO
output$normalizationinfo <- renderText(expr = {if(is.null(refinedata())){return("")}else{return(paste(refinedata()$NORMINFO))}})

# END OF NORMALIZATION


# BEGIN of (random effects) mixture model


output$runsmodel <- renderUI(expr = {    
  selectInput("runsmodel", label="Variable(s) defining technical replications (runs) for model fitting", c(input$runsnorm, cnames()), multiple=TRUE)    
})


  FITK2 <- reactive(x = { 
if(input$fitmodel==TRUE && !is.null(refinedata()) && ( ((!is.null(input$runsmodel) && !input$runsmodel == " ") & input$design %in% c("c2","h2")) | input$design == "y")){

    if(input$design %in% c("h2", "c2")){
    fitk2 <- adapmixmod(DATINT=refinedata()$DATINT, nrep=5, varfix=input$ranef, aggfun=input$normfun)
    lmmpi <- adaplmmintervals(resadapmixmod=fitk2,  level=input$level, group="nonresponder", alternative="less", design=input$design)
    eperc <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="nonresponder", alternative="less")
    epercall <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="all", alternative="less")
    limtab <- data.frame(rbind(lmmpi$estlimitsd, eperc, epercall))
    print(limtab)
    }
    if(input$design=="y"){

      fitk2 <- adapmixmodsampleID(refinedata()$DATINT, nrep=10, aggsamples=input$normfun)
      lmmpi <- adaplmmintervals(resadapmixmod=fitk2, level=input$level, group="nonresponder", alternative="less", design="y")
      eperc <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="nonresponder", alternative="less")
      epercall <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="all", alternative="less")
      limtab <- data.frame(rbind(lmmpi$estlimitsd, eperc, epercall))
      print(limtab)
    }
  return(list(fitk2=fitk2, lmmpi=lmmpi, limtab=limtab))
  }else{
  return(list(fitk2=NULL, lmmpi=NULL, limtab=NULL))}
})

    
  output$classpredintcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Data used for fitting, prediction limits and posterior probability for subpopulation 'nonresponder'")}})
  output$classpredintplot <- renderPlot(expr = {
      if(is.null(FITK2()$fitk2)){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        THISFITK2 <- FITK2()
        cpp <- ggplot(data=THISFITK2$fitk2$DATINT, aes(y=normresp, x=sampleID)) + geom_point(aes(shape=cluster, color=postproblower)) + facet_wrap(~runsmodel) + 
          geom_hline(data=THISFITK2$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
          ylab("Normalized response") + 
          scale_colour_continuous(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
          scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) ) +
          theme(axis.text.x = element_text(angle = 90))

        print(cpp)
      }
      if(input$design=="y"){
      THISFITK2 <- FITK2()
      cpp <- ggplot(data=THISFITK2$fitk2$DATINT, aes(y=normresp, x=sampleID)) + geom_point(aes(shape=cluster, color=postproblower)) + 
        geom_hline(data=THISFITK2$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
        ylab("Normalized response") + 
        scale_colour_continuous(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
        scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) ) +
        theme(axis.text.x = element_text(angle = 90))
      print(cpp)
      }}
    })
  
 output$notepredintplot <- renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return(paste(FITK2()$fitk2$varmodelnote, FITK2()$fitk2$note, collapse=" "))}})

  output$classpredinthist <- renderPlot(expr = {
      if(is.null(FITK2()$fitk2)){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        THISFITK2 <- FITK2()
        #print(str(THISFITK2$fitk2$DATINT))
        DDP<-THISFITK2$fitk2$DATINT
        DDP$posteriorpc <- cut(DDP$postproblower, breaks=c(0,0.01,0.05, seq(0.1,0.9,0.1), 0.95, 0.99,1) )
        rx <- diff(range(DDP$normresp, na.rm=TRUE))
        nb <- max(15, nclass.FD(na.omit(DDP$normresp)))
        cpphist <- ggplot(data=DDP, aes(x=normresp)) + geom_histogram(aes(fill=posteriorpc), binwidth=rx/nb) + facet_wrap(~runsmodel) + 
          geom_vline(data=THISFITK2$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab("Normalized response") + 
          scale_fill_discrete(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
          scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) ) #+
         # theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

        print(cpphist)
      }
      if(input$design=="y"){
      THISFITK2 <- FITK2()
      #print(str(THISFITK2$fitk2$DATINT))
        DDP<-THISFITK2$fitk2$DATINT
        DDP$posteriorpc <- cut(DDP$postproblower, breaks=c(0,0.01,0.05, seq(0.1,0.9,0.1), 0.95, 0.99,1) )
        rx <- diff(range(DDP$normresp, na.rm=TRUE))
        nb <- max(15, nclass.FD(na.omit(DDP$normresp)))
        cpphist <- ggplot(data=DDP, aes(x=normresp)) + geom_histogram(aes(fill=posteriorpc), binwidth=rx/nb)  + 
        geom_vline(data=THISFITK2$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
        xlab("Normalized response") + 
        scale_fill_discrete(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
        scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) ) #+
      #  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
      print(cpphist)
      }}
    }) 


  output$classpredinthistpooled <- renderPlot(expr = {
      if(is.null(FITK2()$fitk2)){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        THISFITK2 <- FITK2()
        #print(str(THISFITK2$fitk2$DATINT))
        DDP<-THISFITK2$fitk2$DATINT
        DDP$posteriorpc <- cut(DDP$postproblower, breaks=c(0,0.01,0.05, seq(0.1,0.9,0.1), 0.95, 0.99,1) )
        rx <- diff(range(DDP$normresp, na.rm=TRUE))
        nb <- max(15, nclass.FD(na.omit(DDP$normresp)))

        cpphist <- ggplot(data=DDP, aes(x=normresp)) + geom_histogram(aes(fill=posteriorpc), binwidth=rx/nb) + 
          geom_vline(data=THISFITK2$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab("Normalized response") + 
          scale_fill_discrete(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
          scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) ) #+
         # theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

        print(cpphist)
      }
}
    }) 


 output$diagnosticcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Diagnostic plots for random effect estimates in subpopulation 'nonresponder'")}})  
 output$diagnosticplot <- renderPlot(expr = {
  if(is.null(FITK2()$fitk2)){return(NULL)}else{
    if(input$design %in% c("c2","h2")){ 
      dp <- ranefqqhist(resadapintervals=FITK2()$lmmpi, outertitle="") 
      print(dp)}
    if(input$design=="y"){ 
      dp <- qqhist_y(resadapintervals=FITK2()$lmmpi, outertitle="") 
      print(dp)
   }
  }
})

  output$predinttabcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Estimated mean, prediction limit and quantiles for 'nonresponder'")}}) 
  output$predinttabsub <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return(
" In group 'nonresponder': 
 'pred.int': prediction limit (for 1 future observation) based on fitting a random effects model to those observations that were classified as 'nonresponder' in the 2-component mixture model;
 'postwt.perc' percentile of a sample the original observation, weighted by posterior probability to be member of group 'nonresponder';
 'emp.perc': percentile of those original observations that were classified as 'nonresponder' in the 2-component mixture model;
 'emp.perc.all' in group 'all': percentile of all original observations (irrespective of classification in responders or nonresponders).")}}) 

  output$predinttab <- renderTable(expr = {
    if(is.null(FITK2()$fitk2)){return(NULL)}else{
      data.frame(FITK2()$limtab)
    }
  })

  output$flexmixtabcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Mixture model fit: parameter estimates and size of groups (a posteriori)")}}) 
  output$flexmixtab <- renderTable(expr = {
    if(is.null(FITK2()$fitk2)){return(NULL)}else{
      tabk2((FITK2()$fitk2), design=input$design)
    }
  })
  
BOXCOXLMM <- reactive(x = {if(is.null(FITK2()$fitk2)){return(NULL)}else{
  if(input$design %in% c("c2","h2")){return(adaplmmboxcox(FITK2()$fitk2, group="nonresponder", design=input$design, normop=input$normop))}
    if(input$design=="y"){return(adaplmboxcox(FITK2()$fitk2, group="nonresponder", normop=input$normop))}
}})

output$boxcoxheader <- renderText(expr = {if(is.null(BOXCOXLMM())){return(NULL)}else{BOXCOXLMM()$header}})
output$boxcoxtab <- renderTable(expr = {if(is.null(BOXCOXLMM())){return(NULL)}else{BOXCOXLMM()$tabest}})
output$boxcoxtabcap <- renderText(expr = {if(is.null(BOXCOXLMM())){return(NULL)}else{BOXCOXLMM()$tabestcap}})

output$boxcoxtest <- renderTable(expr = {if(is.null(BOXCOXLMM())){return(NULL)}else{BOXCOXLMM()$tabtest}})
output$boxcoxtestcap <- renderText(expr = {if(is.null(BOXCOXLMM())){return(NULL)}else{BOXCOXLMM()$tabtestcap}})

output$sampleIDNR <- renderText(expr = {
if(input$fitmodel==FALSE || input$showsampleIDNR==FALSE || is.null(FITK2()$fitk2)){return(" ")}else{
DATINT<-FITK2()$fitk2$DATINT
NR <- DATINT[which(DATINT$cluster=="nonresponder"), "sampleID"]
return(paste("Levels of sampleID which were classified as nonresponders in the 2 component mixture model: ", paste(unique(NR), collapse=", "), sep=" "))
}})


# CCP estimation:

# spiked

output$tspiked <- renderUI(expr = {
  selectInput("tspiked", label="Spiked: Sample type level(s) inhibited samples", choices=c(" ", treatlevels()), multiple=TRUE)    
})



refinedataspiked <- reactive(x = {
  if(is.null(datasetInput())||any(c(is.null(input$response), is.null(input$treatment), is.null(input$runsnorm), is.null(input$sampleID), is.null(input$tfornormalization), is.null(input$tforfitting)))||any(c(input$response, input$treatment, input$runsnorm, input$sampleID, input$tfornormalization, input$tforfitting) == " ")){RDAT<-NULL}else{
  RDAT <- adapcheckdatainput(dat=datasetInput(), response=input$response, treatment=input$treatment, runsnorm=input$runsnorm, sampleID=input$sampleID,
                     normalizeby=input$tfornormalization, forfitting=input$tforfitting, runsmodel = input$runsmodel, spiked = input$tspiked
)
  normDAT <- normalize(DATINT=RDAT$DATINT, normop=input$normop, normfun=input$normfun)
  RDAT$DATINT <- normDAT$NORMDAT
  OUTNORMINFO<- paste( normDAT$NORMINFO, RDAT$textnormlev, RDAT$textnormunit, sep=" ")
  RDAT$NORMINFO <- OUTNORMINFO
}
return(RDAT)
})


###

  CCPestimation <- reactive(x = { 
if(input$fitmodel==TRUE && input$computeccp==TRUE && !is.null(refinedata()) && ( ((!is.null(input$runsmodel) && !input$runsmodel == " ") & input$design %in% c("c2","h2")) | input$design == "y") && (!is.null(input$tspiked) && !input$tspiked == " ")){

    if(input$design %in% c("h2", "c2")){
    resCCP <- adaCCP(fitk2=FITK2()$fitk2, rdat=refinedataspiked(), ccplevel=input$ccplevel, resp=input$response, nrdefinition=c("modelclass"), comparison=input$ccpmeasure, aggfun=input$aggfun, runsmodel=input$runsmodel)
    return(resCCP)
    }else{ if(input$design == "y"){

#print(str(FITK2()$fitk2$DATINT)); print(str(refinedataspiked()))

    resCCP <- adaCCP(fitk2=FITK2()$fitk2, rdat=refinedataspiked(), ccplevel=input$ccplevel, resp=input$response, nrdefinition=c("modelclass"), comparison=input$ccpmeasure, aggfun=input$aggfun, runsmodel=NULL)
    return(resCCP)}else{return(NULL)}
    }
}else{return(NULL)}

# end reactive function CCPestimation
})


# scatter plot  
#  output$classCCPplotcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Data used for fitting, prediction limits and posterior probability for subpopulation 'nonresponder'")}})

  output$classCCPplot <- renderPlot(expr = {
      if(is.null(CCPestimation())){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        resCCP <- CCPestimation()

        if(input$ccpmeasure=="ratio"){
        cpp <- ggplot(data=resCCP$DATCOMP, aes(y=ratio, x=sampleID)) + geom_point(aes(color=cluster)) + facet_wrap(~runsmodel) + 
          geom_hline(data=resCCP$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
          ylab(resCCP$ynam) + 
          scale_colour_discrete(name= "Model classification:" ) +
          scale_linetype_discrete(name="CCP: \nestimated") +
          theme(axis.text.x = element_text(angle = 90))}

        if(input$ccpmeasure=="percinhib"){
        cpp <- ggplot(data=resCCP$DATCOMP, aes(y=percinhib, x=sampleID)) + geom_point(aes(color=cluster)) + facet_wrap(~runsmodel) + 
          geom_hline(data=resCCP$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
          ylab(resCCP$ynam) + 
          scale_colour_discrete(name= "Model classification:" ) +
          scale_linetype_discrete(name="CCP: \nestimated") +
          theme(axis.text.x = element_text(angle = 90))}
        print(cpp)
      }
      if(input$design=="y"){
        resCCP <- CCPestimation()
        if(input$ccpmeasure=="ratio"){
        cpp <- ggplot(data=resCCP$DATCOMP, aes(y=ratio, x=sampleID)) + geom_point(aes(color=cluster)) +
          geom_hline(data=resCCP$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
          ylab(resCCP$ynam) + 
          scale_colour_discrete(name= "Model classification:" ) +
          scale_linetype_discrete(name="CCP: \nestimated") +
          theme(axis.text.x = element_text(angle = 90))}
        if(input$ccpmeasure=="percinhib"){
        cpp <- ggplot(data=resCCP$DATCOMP, aes(y=percinhib, x=sampleID)) + geom_point(aes(color=cluster)) +
          geom_hline(data=resCCP$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
          ylab(resCCP$ynam) + 
          scale_colour_discrete(name= "Model classification:" ) +
          scale_linetype_discrete(name="CCP: \nestimated") +
          theme(axis.text.x = element_text(angle = 90))}
        print(cpp)
      }}
    })
  
#  output$classCCPhistcap <-  renderText(expr = {if(is.null(FITK2()$fitk2)){return("")}else{return("Data used for fitting, prediction limits and posterior probability for subpopulation 'nonresponder'")}})

  output$classCCPhist <- renderPlot(expr = {
      if(is.null(CCPestimation())){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        DDccp <- CCPestimation()
         if(input$ccpmeasure=="ratio"){
           rx <- diff(range(DDccp$DATCOMP$ratio, na.rm=TRUE))
           nb <- max(15, nclass.FD(na.omit(DDccp$DATCOMP$ratio)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=ratio)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) + facet_wrap(~runsmodel) + 
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
         if(input$ccpmeasure=="percinhib"){
           rx <- diff(range(DDccp$DATCOMP$percinhib, na.rm=TRUE))
           nb <- max(10, nclass.FD(na.omit(DDccp$DATCOMP$percinhib)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=percinhib)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) + facet_wrap(~runsmodel) + 
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
        print(ccphist)

      }
      if(input$design=="y"){
        resCCP <- FITK2()
        DDccp <- CCPestimation()
         if(input$ccpmeasure=="ratio"){
           rx <- diff(range(DDccp$DATCOMP$ratio, na.rm=TRUE))
           nb <- max(15, nclass.FD(na.omit(DDccp$DATCOMP$ratio)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=ratio)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) +
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
         if(input$ccpmeasure=="percinhib"){
           rx <- diff(range(DDccp$DATCOMP$percinhib, na.rm=TRUE))
           nb <- max(10, nclass.FD(na.omit(DDccp$DATCOMP$percinhib)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=percinhib)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) +
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
        print(ccphist)
      }}
    })

 output$classCCPhistpooled <- renderPlot(expr = {
      if(is.null(CCPestimation())){return(NULL)}else{
       if(input$design %in% c("c2","h2")){ 
        DDccp <- CCPestimation()
         if(input$ccpmeasure=="ratio"){
           rx <- diff(range(DDccp$DATCOMP$ratio, na.rm=TRUE))
           nb <- max(15, nclass.FD(na.omit(DDccp$DATCOMP$ratio)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=ratio)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) +
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
         if(input$ccpmeasure=="percinhib"){
           rx <- diff(range(DDccp$DATCOMP$percinhib, na.rm=TRUE))
           nb <- max(10, nclass.FD(na.omit(DDccp$DATCOMP$percinhib)))
          ccphist <- ggplot(data=DDccp$DATCOMP, aes(x=percinhib)) + geom_histogram(aes(fill=cluster), binwidth=rx/nb) +
          geom_vline(data=DDccp$limtab, aes(xintercept=value, linetype=estimated), show_guide=TRUE) +
          xlab(DDccp$ynam) +  scale_linetype_discrete(name="CCP \nestimated" )}
        print(ccphist)

      }
}
    })

  output$ccptabcap <-  renderText(expr = {if(is.null(CCPestimation())){return("")}else{return(paste("Estimated median and empirical percentiles for", CCPestimation()$ynam, sep=" "))}}) 
  output$ccptabsub <-  renderText(expr = {if(is.null(CCPestimation())){return("")}else{return(paste(CCPestimation()$infoccpmeasure , CCPestimation()$limitexplanation, sep=" "))}}) 

  output$ccptab <- renderTable(expr = {
    if(is.null(CCPestimation())){return(NULL)}else{
      data.frame(CCPestimation()$limtab)
    }
  })

# simplified summary of sampleIDs
  
#  FITK2s <- reactive(x = { 
#    if (input$fitsummary==FALSE || is.null(refinedata())){
#      return(list(fitk2=NULL, lmmpi=NULL, limtab=NULL))
#    } else {
#      fitk2 <- adapmixmodsampleID(refinedata()$DATINT, nrep=10, aggsamples="mean")
#      lmmpi <- adaplmmintervals(resadapmixmod=fitk2, level=input$level, group="nonresponder", alternative="less", design="y")
#      eperc <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="nonresponder", alternative="less")
#      epercall <- adapperccutpoints(resadapmixmod=fitk2, level=input$level, group="all", alternative="less")
#      limtab <- data.frame(rbind(lmmpi$estlimitsd, eperc, epercall))
#      print(limtab)
#      return(list(fitk2=fitk2, lmmpi=lmmpi, limtab=limtab))
#    }
#  })
  
  
#  output$sclasspredintplot <- renderPlot(expr = {
#    if(is.null(FITK2s()$fitk2)){return(NULL)}else{
#      THISFITK2 <- FITK2s()
#      cpp <- ggplot(data=THISFITK2$fitk2$DATINT, aes(y=normresp, x=sampleID)) + geom_point(aes(shape=cluster, color=postproblower)) + 
#        geom_hline(data=THISFITK2$limtab, aes(yintercept=value, linetype=estimated), show_guide=TRUE) +
#        ylab("Normalized response") + 
#        scale_colour_continuous(name=paste( THISFITK2$lmmpi$group,":\nposterior \nprobability", sep="" ) ) +
#        scale_linetype_discrete(name=paste( THISFITK2$lmmpi$group,":\nestimated", sep="" ) )
#      print(cpp)
#    }
#  })
  
#  #output$snotepredintplot <- renderPrint(expr = {if(is.null(FITK2s())){return("")}else{return(paste(FITK2s()$fitk2$varmodelnote, FITK2s()$fitk2$note, collapse=" "))}})



# output$sdiagnosticcap <- renderText(expr = {if(is.null(FITK2s()$fitk2)){return("")}else{return("Diagnostic plots for residuals of 'nonresponder', after summarizing at sampleID level")}})    
#  output$sdiagnosticplot <- renderPlot(expr = {
#    if(is.null(FITK2s()$fitk2)){return(NULL)}else{
#      dp <- qqhist_y(resadapintervals=FITK2s()$lmmpi, outertitle="") 
#      print(dp)
#    }
#  })
  
  
#  output$spredinttabcap <- renderText(expr = {if(is.null(FITK2s()$fitk2)){return("")}else{return("Estimated mean, prediction limit and quantiles for 'nonresponder' at the sampleID level")}})
#  output$spredinttabsub <-  renderText(expr = {if(is.null(FITK2s()$fitk2)){return("")}else{return(
#"'pred.int': prediction limit (for 1 future sampleID-mean) based on assuming normal distribution for those sampleID means that were classified as 'nonresponder' in the 2-component mixture model;
# 'postwt.perc' percentile of a sample of the sampleID-means, weighted by posterior probability to be member of group 'nonresponder';
# 'emp.perc.all': percentile of the sampleID-means that were classified as 'nonresponder' in the 2-component mixture model.")}}) 
#  output$spredinttab <- renderTable(expr = {
#    if(is.null(FITK2s()$fitk2)){return(NULL)}else{
#      data.frame(FITK2s()$limtab)
#   }
#  })
  
  
#  output$sflexmixtabcap <- renderText(expr = {if(is.null(FITK2s()$fitk2)){return("")}else{return("Mixture model fit: parameter estimates and size of groups (a posteriori)")}})    
#  output$sflexmixtab <- renderTable(expr = {
#    if(is.null(FITK2s()$fitk2)){return(NULL)}else{
#      stabk2((FITK2s()$fitk2))
#    }
#  })

#BOXCOXLMs <- reactive(x = {if(is.null(FITK2s()$fitk2)){return(NULL)}else{adaplmboxcox(FITK2s()$fitk2, group="nonresponder", normop=input$normop)}})

#output$sboxcoxheader <- renderText(expr = {if(is.null(BOXCOXLMs())){return(NULL)}else{"Box-Cox-Lambda and LRT for normality and lognormality for sampleID means"}})
#output$sboxcoxtab <- renderTable(expr = {if(is.null(BOXCOXLMs())){return(NULL)}else{BOXCOXLMs()$tabest}})
#output$sboxcoxtabcap <- renderText(expr = {if(is.null(BOXCOXLMs())){return(NULL)}else{BOXCOXLMs()$tabestcap}})

#output$sboxcoxtest <- renderTable(expr = {if(is.null(BOXCOXLMs())){return(NULL)}else{BOXCOXLMs()$tabtest}})
#output$sboxcoxtestcap <- renderText(expr = {if(is.null(BOXCOXLMs())){return(NULL)}else{BOXCOXLMs()$tabtestcap}})


  
})


#
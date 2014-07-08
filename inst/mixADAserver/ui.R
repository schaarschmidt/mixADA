
library(shiny)
shinyUI(pageWithSidebar(
  
  headerPanel("mixADA: Cutpoint selection by mixture models and prediction intervals"),
  
  sidebarPanel(
    h3("Data import"),
    fileInput(inputId="datafile", label="Upload a csv-file with columns separated by comma (,) and decimal points (.)", multiple=FALSE),

    h3("Normalization"),

    h4(textOutput(outputId="plsselect")),
    uiOutput("response"),
    uiOutput("treatment"),
    uiOutput("tfornormalization"),
    uiOutput("tforfitting"),
    uiOutput("sampleID"),

    selectInput(inputId="normop", label="Normalization of samples:", choices=c("log-transform data and substract" = "logdiff", "substract" = "diff", "divide by" = "ratio")),
    selectInput(inputId="normfun", label=NULL, choices=c("mean","median")),

    uiOutput("runsnorm"),

    h3("SCP: Random effects mixture model"),
   
    radioButtons(inputId="ranef", label="Random effects in 2-component mixture model:",
      choices=c( "Both, equal random effects & equal residual variance" = "bothranres", 
            "Equal random effects, different residual variance" = "ran",
            "Equal residual variance, different random effects" = "res",
            "Both, different random effects & different residual variance" = "no")),

#    h4("Select variables for model fitting:") 
    uiOutput("runsmodel"),  
    selectInput(inputId="design", label="Structure of effects", choices=c("Runs crossed with samples" = "c2", "Samples nested in runs" = "h2", "Simplified: pool over runs" = "y")),  
    numericInput(inputId="level", label="Level of prediction limits or quantiles", value=0.95, min=0, max=1),
    checkboxInput(inputId="fitmodel", label="Start model fitting (needs some time)", value=FALSE),
    checkboxInput(inputId="showsampleIDNR", label="Show biological samples classified as nonresponders", value=FALSE),
 # # # #   isolate(submitButton(text = "(Re)start model fitting"))

    h3("CCP estimation"),

    uiOutput("tspiked"),
    radioButtons(inputId="ccpmeasure", label="Compute CCP for", choices=c( "Percent inihibition" = "percinhib",  "Ratio spiked/unspiked" = "ratio")),
    numericInput(inputId="ccplevel", label="Level of quantiles for CCP", value=0.99, min=0, max=1),
    checkboxInput(inputId="computeccp", label="Compute CCP", value=FALSE),

# end of sidebarpanel:
width=3
    ),

  mainPanel(
    h3(textOutput(outputId="propplotheader")),
    plotOutput(outputId="propplot"),
    textOutput(outputId="cappropplot"),

    h3(textOutput(outputId="normalizationheader")),
    plotOutput(outputId="normalizationplot"),
    textOutput(outputId="normalizationinfo"),

    h3(textOutput(outputId="scpheader")),
    h4(textOutput(outputId="classpredintcap")),
    plotOutput(outputId="classpredintplot"),

    plotOutput(outputId="classpredinthist"),

    plotOutput(outputId="classpredinthistpooled"),

    h4(textOutput(outputId="diagnosticcap")),
    plotOutput(outputId="diagnosticplot"),
    textOutput(outputId="notepredintplot"),

    h4(textOutput(outputId="flexmixtabcap")),
    tableOutput(outputId="flexmixtab"),
    textOutput(outputId="sampleIDNR"),
    
    h4(textOutput(outputId="boxcoxheader")),
    tableOutput(outputId="boxcoxtab"),
    textOutput(outputId="boxcoxtabcap"),

    tableOutput(outputId="boxcoxtest"),
    textOutput(outputId="boxcoxtestcap"),

 h4(textOutput(outputId="predinttabcap")),
 tableOutput(outputId="predinttab"),
 textOutput(outputId="predinttabsub"),

    h3(textOutput(outputId="ccpheader")),
    plotOutput(outputId="classCCPplot"),
    plotOutput(outputId="classCCPhist"),
    plotOutput(outputId="classCCPhistpooled"),

    h4(textOutput(outputId="ccptabcap")),
    tableOutput(outputId="ccptab"),
    textOutput(outputId="ccptabsub")


)))
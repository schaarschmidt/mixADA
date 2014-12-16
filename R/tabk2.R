tabk2 <-
function(resadapmixmod, design){

design <- match.arg(design, choices=c("c2","h2","c1","h1","y"))
 
if(design %in% c("c2", "h2", "c1", "h1")){
 
FIT <- resadapmixmod$fitlist$kk
PARATAB <- parameters(FIT)
OBSTAB <- as.vector(table(resadapmixmod$DATINT[,"clusters"])) 
SIDTAB <- table(unique(resadapmixmod$DATINT[,c("sampleID","clusters")])[,"clusters"])
TAB <- rbind(PARATAB, SIDTAB, OBSTAB)
}

if(design=="y"){
FIT <- resadapmixmod$fitlist$kk
PARATAB <- parameters(FIT)
OBSTAB <- as.vector(table(resadapmixmod$DATINT[,"clusters"])) 
TAB <- rbind(PARATAB, OBSTAB)
}

switch(design,
"c2"={rownames(TAB)<-c("mean", "V.ID", "V.runs","V.runsxID", "V.res", "no.ID", "no.obs")},      
"c1"={rownames(TAB)<-c("mean", "V.ID", "V.runs", "V.res", "no.ID", "no.obs")},
"h2"={rownames(TAB)<-c("mean", "V.ID", "V.runsxID", "V.res", "no.ID", "no.obs")},
"h1"={
  rownames(TAB) <- abbreviate(rownames(TAB), minlength=4)
  rownames(TAB)[1] <- "mean"; rownames(TAB)[c(nrow(TAB)-1, nrow(TAB))] <- c("no.ID", "no.obs")},
"y"={rownames(TAB)<-c("mean", "var", "no.obs")}
)

DTAB <- t(TAB)
DTAB <- data.frame(labels=resadapmixmod$labels, DTAB)
return(DTAB)
}

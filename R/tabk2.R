tabk2 <-
function(resadapmixmod, design){

design <- match.arg(design, choices=c("c2","h2","y"))
 
if(design %in% c("c2","h2")){
 
FIT <- resadapmixmod$fitlist$kk
PARATAB <- parameters(FIT)
OBSTAB <- as.vector(table(resadapmixmod$DATINT[,"clusters"])) 
SIDTAB <- table(unique(resadapmixmod$DATINT[,c("sampleID","clusters")])[,"clusters"])
TAB <- rbind(PARATAB, SIDTAB, OBSTAB)
rownames(TAB)<-c("mean", "V.ID", "V.runs.in.ID", "var.res", "no.ID", "no.obs")
DTAB <- t(TAB)
DTAB <- data.frame(labels=resadapmixmod$labels, DTAB)
}
if(design=="y"){
FIT <- resadapmixmod$fitlist$kk
PARATAB <- parameters(FIT)
OBSTAB <- as.vector(table(resadapmixmod$DATINT[,"clusters"])) 
TAB <- rbind(PARATAB, OBSTAB)
rownames(TAB)<-c("mean", "V.res", "no.obs")
DTAB <- t(TAB)
DTAB <- data.frame(labels=resadapmixmod$labels, DTAB)
}
return(DTAB)
}

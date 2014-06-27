df_c2h1 <-
function(fit, fa, fba, fc)
{
if(any(c(length(fa), length(fba), length(fc)) != 1)){stop("fa, fba, fc must be single character strings")}
if(any(!is.character(fa),!is.character(fba),!is.character(fc))){stop("fa, fb, fc must be single character strings")}
if(length(fit$vari)!=6){stop("Fitted model does not contains 6 variance components")}

nam <- names(fit$ni)

if(!all(c(fa, fba, fc) %in% nam)){stop("At least one effect name (fa, fba, fc) can not be found in the fitted model.")}

ia <- which(nam==fa)
iba <- which(nam==fba)
ic <- which(nam==fc)
iac <- which(nam %in% c(paste(fa, fc, sep=":"), paste(fc, fa, sep=":")))

if(any(c(is.null(ia), is.null(iba), is.null(ic), is.null(iac)))){stop("fa, fba, fc or their fa:fc interaction could not be found in the fitted model")}

dfa <- fit$ni[ia]-1
dfba <- fit$ni[iba]-fit$ni[ia]
dfc <- fit$ni[ic]-1

dfac <- (fit$ni[ia]-1)*(fit$ni[ic]-1)

dfbca <- (fit$ni[iba] - fit$ni[ia])*(fit$ni[ic] - 1)

dfe <- fit$ni[6] - fit$ni[iba]*fit$ni[ic]

dfi <- c(dfbca=dfbca, dfac=dfac, dfc=dfc, dfba=dfba, dfa=dfa, dfe=dfe)

# # # ORDER IN THE OUTPUT INAPPROPRIATE

Efforder <- c(1, iac, ic, iba, ia, 6)

dfo <- dfi[order(Efforder)]

names(dfo)<-paste("df", c(nam[1:5], "residual"), sep=".")

# balanced nesting of b in a?
nb <- fit$ni[iba]/fit$ni[ia]
if(abs(nb-round(nb))> sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

# balanced replication at each level of abc
ne <- fit$ni[6]/(fit$ni[iba]*fit$ni[ic])
if(abs(ne-round(ne))> sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

dft<-fit$ni[6]-1

if(sum(dfo)!=dft){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

return(dfo)
}

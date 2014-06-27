df_c2 <-
function(fit)
{
nam <- names(fit$vari)
dfa <- fit$ni[2]-1
dfb <- fit$ni[3]-1
dfab <- dfa*dfb
dfe <- fit$ni[4] - fit$ni[2]*fit$ni[3]

dfi <- c(dfab=dfab, dfa=dfa, dfb=dfb, dfe=dfe)
names(dfi)<-paste("df", c(nam[1:3], "residual"), sep=".")

if(fit$ni[1] != fit$ni[2]*fit$ni[3]){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

ncell <- fit$ni[4]/fit$ni[1] 
if(abs(ncell - round(ncell)) > sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

return(dfi)
}

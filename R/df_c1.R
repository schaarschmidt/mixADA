df_c1 <-
function(fit)
{
if(length(fit$ni)!=3){warning("Model may be inappropriate for given data.")}

nam <- names(fit$ni)

dfa <- fit$ni[1]-1
dfb <- fit$ni[2]-1
dfe <- dfa*dfb

dfi <- c(dfa=dfa, dfb=dfb, dfe=dfe)

names(dfi)<-paste("df", c(nam[1:2], "residual"), sep=".")

dft <- (fit$ni[1]*fit$ni[2]-1)
if(sum(dfi)!= dft){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}
if(fit$ni[3]!= fit$ni[1]*fit$ni[2]){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

return(dfi)
}

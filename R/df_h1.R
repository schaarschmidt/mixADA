df_h1 <-
function(fit)
{
nam <- names(fit$vari)
dfa <- fit$ni[1] - 1
dfe <- fit$ni[2] - fit$ni[1]

dfi <- c(dfa=dfa, dfe=dfe)
names(dfi)<-paste("df", c(nam[1], "residual"), sep=".")

if((fit$ni[2]-1) != sum(dfi)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

na <- fit$ni[2]/fit$ni[1] 
if(abs(na - round(na)) > sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

return(dfi)
}

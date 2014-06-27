df_h2 <-
function(fit)
{
nam <- names(fit$vari)
dfa <- fit$ni[2] - 1
dfb <- fit$ni[1] - fit$ni[2]
dfe <- fit$ni[3] - fit$ni[1]

dfi <- c(dfb=dfb, dfa=dfa, dfe=dfe)
names(dfi)<-paste("df", c(nam[1:2], "residual"), sep=".")
	
if((fit$ni[3]-1) != sum(dfi)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

nba <- fit$ni[3]/fit$ni[1] 
if(abs(nba - round(nba)) > sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

na <- fit$ni[1]/fit$ni[2] 
if(abs(na - round(na)) > sqrt(.Machine$double.eps)){warning("Data may be incomplete or unbalanced, model may be inappropriate for given data.")}

return(dfi)
}

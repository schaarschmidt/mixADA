msiweight_c1 <-
function(eaov){

ni <- eaov$ni
msi <- eaov$msi
ms1sc <- msi[1]/ni[2]     #MSA/b
ms2sc <- msi[2]/ni[1]     #MSB/a
ms3sc <- msi[3]		#MSE

msisc <- c(ms1sc, ms2sc, ms3sc)

w1sc <- (1 + 1/ni[1]) 					# 1 + 1/a
w2sc <- (1 + 1/ni[2])					# 1 + 1/b
w3sc <- (1 - 1/ni[1] - 1/ni[2] - 1/(ni[1]*ni[2]))	# 1 - 1/b - 1/a - 1/(ab)

weightisc <- c(w1sc, w2sc, w3sc)

if(length(ni)!=3){warning("Fitted model is appropriate with type=c1.")}
if(length(eaov$dfi)!=3){warning("Fitted model is appropriate with type=c1.")}
if(abs(ni[3]/ni[1] - ni[2]) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type=c1 inappropriate.")}

return(list(msisc=msisc, weightisc=weightisc))
}

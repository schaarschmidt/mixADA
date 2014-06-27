msiweight_h1 <-
function(eaov){

ni <- eaov$ni
msi <- eaov$msi

nrep <- ni[2]/ni[1]

ms1sc <- msi[1]/nrep    #MSA/n
ms2sc <- msi[2]

msisc <- c(ms1sc, ms2sc)

w1sc <- (1 + 1/ni[1]) 	# 1 + 1/a
w2sc <- (1 - 1/nrep)	# 1 - 1/nrep

weightisc <- c(w1sc, w2sc)

if(length(ni)!=2){warning("Fitted model is appropriate with type=h1.")}
if(length(eaov$dfi)!=2){warning("Fitted model is appropriate with type=h1.")}
if(abs(nrep-round(nrep)) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type = h1 inappropriate.")}

return(list(msisc=msisc, weightisc=weightisc))
}

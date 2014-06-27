msiweight_c2 <-
function(eaov){

ni <- eaov$ni
msi <- eaov$msi

nrep <- ni[3]/(ni[1]*ni[2])

ms1sc <- msi[1]/(ni[2]*nrep)     #MSA/bn
ms2sc <- msi[2]/(ni[1]*nrep)     #MSB/an
ms3sc <- msi[3]/nrep		#MSAB/n
ms4sc <- msi[4]

msisc <- c(ms1sc, ms2sc, ms3sc, ms4sc)

w1sc <- (1 + 1/ni[1]) 					# 1 + 1/a
w2sc <- (1 + 1/ni[2])					# 1 + 1/b
w3sc <- (1 - 1/ni[1] - 1/ni[2] - 1/(ni[1]*ni[2]))	# 1 - 1/b - 1/a - 1/(ab)
w4sc <- (1 - 1/nrep)

weightisc <- c(w1sc, w2sc, w3sc, w4sc)

if(length(ni)!=3){warning("Fitted model is appropriate with type=c2.")}
if(length(eaov$dfi)!=4){warning("Fitted model is appropriate with type=c2.")}
if(abs(nrep-round(nrep)) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type=c2 inappropriate.")}
dfab <-(ni[1]-1)*(ni[2]-1)
if(abs(dfab - eaov$dfi[3]) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type=c2 inappropriate.")}
dft <- ni[3]-1
if(abs(dft - sum(eaov$dfi)) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type=c2 inappropriate.")}


return(list(msisc=msisc, weightisc=weightisc))
}

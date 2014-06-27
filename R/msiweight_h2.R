msiweight_h2 <-
function(eaov){

ni <- eaov$ni
msi <- eaov$msi

nrepba <- ni[3]/ni[2] 	#n (no. repl. at each level b(a) )
nrepa <- ni[2]/ni[1]	#b (no of repl. at each level a) 

ms1sc <- msi[1]/(nrepa*nrepba)   #MSA/bn
ms2sc <- msi[2]/nrepba	#MSB(A)/n
ms3sc <- msi[3]		#MSE

msisc <- c(ms1sc, ms2sc, ms3sc)

w1sc <- (1 + 1/ni[1]) 	# 1 + 1/a
w2sc <- (1 - 1/nrepa)	# 1 - 1/b
w3sc <- (1 - 1/nrepba)	# 1 - 1/n

weightisc <- c(w1sc, w2sc, w3sc)

if(length(ni)!=3){warning("Fitted model is appropriate with type=h2.")}
if(length(eaov$dfi)!=3){warning("Fitted model is appropriate with type=h2.")}
if(abs(nrepa-round(nrepa)) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type = h2 inappropriate.")}
if(abs(nrepba-round(nrepba)) > sqrt(.Machine$double.eps)){warning("Design might be unbalanced or option type = h2 inappropriate.")}

return(list(msisc=msisc, weightisc=weightisc))
}

msiweight_c2h1 <-
function(eaov, fa, fba, fc){

if(any(c(length(fa), length(fba), length(fc)) != 1)){stop("fa, fba, fc must be single character strings")}
if(any(!is.character(fa),!is.character(fba),!is.character(fc))){stop("fa, fb, fc must be single character strings")}

nam <- names(eaov$msi)

if(!all(c(fa, fba, fc) %in% nam)){stop("At least one effect name (fa, fba, fc) can not be found in the fitted model.")}

ia <- which(nam==fa)
iba <- which(nam==fba)
ic <- which(nam==fc)
iac <- which(nam %in% c(paste(fa, fc, sep=":"), paste(fc, fa, sep=":")))

if(any(c(is.null(ia), is.null(iba), is.null(ic), is.null(iac)))){stop("fa, fba, fc or their fa:fc interaction could not be found in the fitted model")}


msi <- eaov$msi

if(length(msi)!=6){stop("Fitted model is inappropriate for type='c2h1': the number of sources of vraiation should be 6 (incl. residual variation) ")}

ni <- eaov$ni

namn<-names(ni)

ina <- which(namn==fa)
inba <- which(namn==fba)
inc <- which(namn==fc)

nrep <- ni[4]/(ni[inba]*ni[inc])
na <- ni[ina]
nba <- ni[inba]
nb <- nba/na
nc <- ni[inc]

if(length(ni)!=4){warning("Fitted model is appropriate with type=c2h1.")}
if(length(eaov$dfi)!=6){warning("Fitted model is appropriate with type=c2h1.")}
if(nba<=na){warning("In the current definition of the model, fba is not nested in fa (no. of levels in fba is less or equal to no. of levels in fa)")}
if(abs(nb-round(nb)) > sqrt(.Machine$double.eps) ){warning("Nesting of fba in fa may be unbalanced.")}
if(abs(nrep-round(nrep)) > sqrt(.Machine$double.eps) ){warning("No. of replications at combinantions of fba:fc may be unbalanced.")}

# scaled Mean squares

msasc <- msi[ia]/(nb*nc*nrep)     #MSA/bcn
msbasc <- msi[iba]/(nc*nrep)      #MSBA/cn
mscsc <- msi[ic]/(nba*nrep)	  #MSC/abn
msacsc <- msi[iac]/(nb*nrep)	  #MSAC/bn
msbcasc <- msi[5]/nrep		  #MSBCA/n
mse<-msi[6]			  #MSE

msisc <- c(msasc, msbasc, mscsc, msacsc, msbcasc, mse)

# weights for linear combinations given scaled mean-squares

wasc <- (1 + 1/na) 					# 1 + 1/a
wcsc <- (1 + 1/nc)					# 1 + 1/c
wbasc <- (1 - 1/nb)					# 1 - 1/b
wacsc <- (1 - 1/nc - 1/na - 1/(na*nc))			# 1 - 1/c - 1/a - 1/ac
wbcasc <- (1 - 1/nb - 1/nc - 1/(nb*nc))			# 1 - 1/b - 1/c + 1/bc
wesc <- (1 - 1/nrep)



weightisc <- c(wasc, wbasc, wcsc, wacsc, wbcasc, wesc)
Efforder <- c(ia, iba, ic, iac, 5, 6)

msisco <- msisc[order(Efforder)]
weightisco <- weightisc[order(Efforder)]

if(any(weightisco<=0)){warning("One of the MS is goint to be weighted with a weight smaller or equal 0. Resaon can be very small sample size for several variance components.")}

names(weightisco)<-names(msisco)

return(list(msisc=msisco, weightisc=weightisco))
}

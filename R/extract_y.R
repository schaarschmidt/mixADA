extract_y <-
function(fit){
 mu <- coef(fit) 
 ni <- nrow(fit$model)
 sfit <- summary(fit)
 vari <- (sfit$sigma)^2
 return(list(mu=mu, ni=ni, vari=vari))}

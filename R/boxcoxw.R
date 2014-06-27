boxcoxw <-
function(y, lambda)
{ytilde<-geommean(y)
if(lambda==0){ytilde*log(y)}else{
((y^lambda)-1)/(lambda*ytilde^(lambda-1))}
}

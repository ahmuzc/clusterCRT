#cv between cluster coefficient variation
#y estimated person-years
#lambda estimated event rate
cluster.eventrate <- function(a=0.05,b=0.2,lambda0,lambda1,cv,y){
  #plus one corrects for small number of clusters
  #c denotes the required number of clusters per arm
  c=1+(qnorm(1-a/2)+qnorm(b,lower.tail = F))^2*((((lambda0+lambda1)/y)+(cv^2)*(lambda0^2+lambda1^2))/
                                                  (lambda0-lambda1)^2)
  
  #py denotes the total person-year per arm
  py=ceiling(c)*y
  #yy denotes the required person-years
  yy=(qnorm(1-a/2)+qnorm(b,lower.tail = F))^2*(lambda0+lambda1)/(lambda0-lambda1)^2
  #de denotes the design effect
  de=round(py/yy,2)
  print(paste0('required No. of clusters per arm ',ceiling(c)))
  print(paste0('total pyears per arm ',py))
  print(paste0('estimated design effect ',de))
}

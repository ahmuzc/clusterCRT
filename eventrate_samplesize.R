#cv between cluster coefficient variation
#y estimated person-years per cluster
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

# Accrombessi M, Cook J, Dangbenon E, et al. Efficacy of pyriproxyfen-pyrethroid long-lasting insecticidal nets (LLINs) and chlorfenapyr-pyrethroid LLINs compared with pyrethroid-only LLINs for malaria control in Benin: a cluster-randomised, superiority trial. Lancet. 2023;401(10375):435-446. doi:10.1016/S0140-6736(22)02319-4
# cluster.eventrate(a=0.025,lambda0 = 1,lambda1 =0.7,cv=0.3,y=20*2)
# "required No. of clusters per arm 20"
# "total pyears per arm 800"
# "estimated design effect 4.46"

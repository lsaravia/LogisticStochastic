
# Simulacion estocastica para modelo logistico
#
# Simulacion estocastica para modelo logistico
# con fluctuación ambiental en r (1+a cos(w t))
#
STO_logistic_ef <- function(x,pars,times)
{
  # Setup an array to store results: time, N
  #
  ltimes <- length(times)
  output <- array(dim=c(ltimes,2),dimnames=list(NULL,names(x)))
  t <- x[1]
  stopifnot(t<=times[1])
  
  # loop until either k > maxstep or
  #
  output[1,] <-x
  k <- 2
  while (k <= ltimes) {
    while (t < times[k]) {
      x <- logistic_ef_onestep(x,pars) 
      t <- x[1]
    }
    while (t >= times[k] && k <= ltimes) {
      output[k,] <- x
      k <- k+1
    }
  }
  as.data.frame(output)
}

# Logistica con fluctuación ambiental en r - Simulacion de Eventos 
#
logistic_ef_onestep<- function(x,pars){
  p<-as.list(pars)
  # 2nd element is the population 
  #
  N<-x[2]
  B<- p$r*(1+p$a*cos(p$omega*x[1]))   # Birth rate 
  if(B<0) B<-0
  R<- B+p$s*N
  event_rate<- B/R
  
  if(N>0){
    if(runif(1) <=event_rate ){
      N<-N+1
    } else {
      N<-N-1
    }
  }
  # Exponential random number
  tau<-rexp(n=1,rate=R)
  
  c(x[1]+tau,N)
}



# Ecuación logistica determinista con fluctuaciones ambientales 
#
logistic_ef_det<-function(t,State,Pars){
  with(as.list(c(State, Pars)), {
    
    dP <- N*(r*(1+a *cos(omega*t))-s*N) 
    
    return(list(c(dP)))
  })
}

# Estimacion de parametros usando Aproximate Bayesian computation 
# 
#
estima_ABC <- function(dat,p,time,dlim,sim=1000)
{

  da <- data.frame(matrix(, nrow = time, ncol = 2))
  
  names(da) <- c("r","s")  
  j <-1
  lendat<-length(dat)
  
  for(i in 1:sim){
    # calcula parametros
    # 
    r<- runif(1,p$r[1],p$r[2])
    s <- runif(1,p$s[1],p$s[2])
    nini <- round(r/s)

    # Corre el modelo
    out <- STO_simul(c(0,nini),c(r=r,s=s),1:time)
    
    # selecciona la ultima parte
    out <- out[(time-lendat+1):time,2]
    
    dis <- sum((dat-out)^2)
    if(dis <= dlim){
      da[j,]<-c(r,s)
      j <- j + 1
    }
    
  }
  return(na.omit(da))
}

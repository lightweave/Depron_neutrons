MY_SMOOTH<-function(X, N){
  K<-length(X)
  rez <- c(1:K)
  for(l in 1:K) {
    a<-l-N
    b<-l+N
    if(a<1) a<-1
    if(b>K) b<-K
    S <- 0
    P <- 0
    for(m in a:b) if(is.numeric (X[m])){
      S <- S + X[m]
      P <-P + 1
    }
    if(P>0) rez[l]<- S/P else rez[l]<- NA
  }
  return(rez)
}
# ===============================================
SMOOTH_mean<-function(X, N){
  K<-length(X)
  rez <- c(1:K)
  for(l in 1:K) {
    a<-l-N
    b<-l+N
    if(a<1) a<-1
    if(b>K) b<-K
    rez[l]<-mean(X[a:b], na.rm = TRUE)
  }
  return(rez)
}
# ===============================================
SMOOTH_Triangle<-function(X, N =5){
  if(N<1) return(X)
  K<-length(X)
  rez <- c(1:K)
  for(l in 1:K) {
    a<-l-N
    b<-l+N
    if(a<1) a<-1
    if(b>K) b<-K
    S <- 0
    P <- 0
    for(m in a:b) if(is.numeric (X[m])){
      G <- N+1-abs(m-l)
      S <- S + X[m]*G
      P <-P +G
    }
    if(P>0) rez[l]<- S/P else rez[l]<- NA
  }
  return(rez)
}
SMOOTH_Triangle<-function(X, order =5){

  if(order < 1) return(X)
  if(length(X)<1) return(X)
  K<-length(X)
  rez <- c(1:K)
  for(l in 1:K) {
    a<-l-order
    b<-l+order
    if(a<1) a<-1
    if(b>K) b<-K
    S <- 0
    P <- 0
    for(m in a:b) if(is.numeric (X[m])){
      G <- order+1-abs(m-l)
      S <- S + X[m]*G
      P <-P +G
    }
    if(P>0 & length(S/P) != 0) rez[l]<- S/P else rez[l]<- NA
      
  }
  return(rez)
}
# ===============================================
# Функции расчета затенения
shade_solid<-function(X) return((1+sqrt(1-(3389.5/(X+3389.5))^2))/2)
shade_nadir<-function(X) return((1+(1-(3389.5/(X+3389.5))^2))/2)
shade_gorisont<-function(X) return((1-0.35*(3389.5/(X+3389.5))^4))
# ===============================================

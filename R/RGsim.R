#' This is package
#' @export
#' @param n Numerical Variable
#' @param nv Numerical Variable


rg<-function(n,nv){
  X<-c();b<-c();num<-c()
  for (i in 1:nv){
    X<-cbind(X,rnorm(n,0,1))
  }
  X<-cbind(1,X)
  for (i in 0:nv){
    b<-cbind(b,sample(seq(2,7),1))
  }
  for (i in 0:nv){
    num<-c(num,paste(c('X',as.character(i)),collapse=""))
  }
  XX<-as.matrix(X)
  Y<-XX%*%t(b)
  Y<-Y+rnorm(n,0,1)
  YX<-cbind(Y,XX)
  colnames(YX)<-c('Y',num)
  lis<-list(data=YX,par=b)
  return(lis)
}




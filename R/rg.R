
#' This is package built by Fadhaa Ali
#' @param n the required sample size
#' @param nv the required number of variables
#' @title This package is to simulate dataset following linear regression model
#' @return The generated dataframe with simple size=n and nv number of independent variables
#' @example rg(100,3)
#' @author Author: Fadhaa Ali
#' @description Choose the n sample size and nv number of variables to get the generated dataset
#' @references Nagarajan R, Scutari M, Lebre S (2013). "Bayesian Networks in R with Applications in Systems Biology". Springer.
#' @concept Linear Regression.
#' @source The data comes from simulation.
#' @export


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

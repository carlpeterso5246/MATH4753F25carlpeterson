#' bootstrapping function
#'
#' @param iter of bootstrap iterations (default=10000)
#' @param x numeric vector. Sample data to bootstrap
#' @param fun statistic to compute ('mean','var',etc.)
#' @param alpha significance level used to form confidence intervals
#' @param ...
#'
#' @returns list with confidence interval, the statistic funciton, original data sampled, bootstrapped statistics over each iteration, and a graph with the statistics and confidence intervals on it.
#' @export
#'
#' @examples \dontrun{
#' set.seed(39)
#' sam <- rnorm(25, mean = 25, sd = 10)
#' myboot2(x = sam, fun = mean, alpha = 0.05)
#'
#' # Using a custom statistic (mean divided by median)
#' ratio_fun <- function(x) mean(x) / median(x)
#' myboot2(x = sam, fun = ratio_fun, alpha = 0.30)
#' }
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,...){

  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  #Now sample with replacement
  y=sample(x,n*iter,replace=TRUE) #A

  # Make a matrix with all the resampled values
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  # xstat is a vector and will have iter values in it

  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #B
  # Nice way to form a confidence interval

  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,main="Histogram of Bootstrap sample statistics",...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=3)

  # added xstat to the list!!!
  return(list(ci=ci,fun=fun,x=x,xstat=xstat))# Some output to use if necessary
}

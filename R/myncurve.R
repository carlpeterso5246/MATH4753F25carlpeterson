#' Normal Distribution Curve function
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a P(X<=a)
#'
#' @returns A graph of a normal distribution curve with plotted region from
#' -inf. to a that represents the probability. Also returns mean, standard
#' deviation, and probability.
#' @export
#'
#' @examples
#'myncurve(7,6,2) #returns a normal curve with mean 7, standard deviation 6,
#'#and has a shaded region from -inf. to 2 that represents the probability
#'#that X is less than 2
myncurve = function(mu, sigma,a){
  # make the curve for a normal distribution
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  # create curves for shaded area
  xcurve <- seq(mu-4*sigma,a,length=10000)
  ycurve <- dnorm(xcurve,mu,sigma)

  # make the shaded region
  polygon(x=c(mu-4*sigma,xcurve,a),y=c(0,ycurve,0),col='Purple')

  # calculate probability
  probability <- round(pnorm(a,mu,sigma),4)
  # print the area/probability on the graph
  title(paste('Area = ',probability))


  list(mu = mu, sigma = sigma, probability = probability)
}

#' Overbooking function
#'
#' @param N number of available seats
#' @param gamma probability of overbooking
#' @param p probability that a passenger shows
#'
#' @returns the number of tickets to sell to make profit while avoiding overbooking, two plots to show the answer graphically, and the p, gamma, and N values (n values and plots have a discrete and continuous exmaple each)
#' @export
#'
#' @examples
#' ntickets(400,0.02,0.95)
ntickets <- function(N, gamma, p) {

  par(mfrow = c(2, 1))

  # define the x and y for the first plot
  x <- seq(N, N + N*(1-p), by=1)
  obj <- 1 - pbinom(N, x, p) - gamma

  # find the number of tickets we want
  nd <- x[which.min(abs(obj))]

  #FIRST PLOT: DISCRETE
  plot(x,obj,
       type = 'b', # plots points and lines
       pch = 21, # plotting character
       bg = ifelse(x != x[which.min(abs(obj))], 'blue', 'red'), # symbol color
       xlab = "n", # x label
       ylab = "Objective", # y label
       main = paste('Objective vs n to find optimal tickets sold (',nd,') gamma = ',gamma,' N = ',N), # main title
       cex.main = 0.8 # title font size
  )
  abline(h = 0, col='red', lwd = 2)
  abline(v = nd, col='red',lwd = 2)


  #THIS WILL BE SECOND PLOT: CONTINUOUS
  # Define the x and y for second plot
  x_cont <- seq(N, N + N*(1-p), length=10000)
  obj_cont <- 1 - pnorm(N+0.5,x_cont*p,sqrt(x_cont*p*(1-p))) - gamma

  # Define a function that we can use for uniroot()$root
  f <- function(n) 1 - pnorm(N+0.5,n*p,sqrt(n*p*(1-p))) - gamma

  # find the number of tickets we want
  nc <- uniroot(f=f,interval=c(N,N+20))$root

  # SECOND PLOT: CONTINUOUS
  plot(x_cont,obj_cont,
       type = 'l', # plots lines only
       pch = 21, # plotting character
       xlab = "n", # x label
       ylab = "Objective", # y label
       main = paste('Objective vs n to find optimal tickets sold (',round(nc,4),') gamma = ',gamma,' N = ',N), # main title
       cex.main = 0.8 # title font size
  )
  abline(h = 0, col='blue', lwd = 2)
  abline(v = nc, col='blue',lwd = 2)

  list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
}

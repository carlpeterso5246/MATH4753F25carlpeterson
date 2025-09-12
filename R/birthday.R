#' Birthday Problem Function
#'
#' @param n number of people
#'
#' @returns probability that 2 or more people will have the same birthday given a certain number of people
#' @export
#'
#' @examples
#' birthday(23) #gives probability that out of 23 people at least 2 will share a bday
birthday <- function(n){
1 - exp(lchoose(365,n) + lfactorial(n) - n*log(365))
}

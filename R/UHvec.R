#' Unit Hydrograph of x
#'
#' The function \code{UHvec()} computes a Unit Hydrograph
#' @param maxL Max distance (from distance distribution, DD)
#' @param speed celerity
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param model model of the UH. \code{list(z,distr,param=c())}
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' UHvec()
#' }
UHvec <-function(maxL,speed,Timeresinsec,model) {

  # model = list(z,distr,param=c(midL,stdL))
  # z: arealfraction of DD (what area with distance zero to river), midL: Mean distance (from distance distribution, DD) for soils
  # distr: dexp, dnorm,...
  # param: c(valpar1,valpar2)

  #Number of timsteps delay
  delaySteps <- dddCelerity::nbSteps(maxL=maxL,speed=speed,Timeresinsec=Timeresinsec)

  x <- seq(0,(delaySteps-1))

  if (model$distr == "dexp") {
     #Scale the moments of the distribution
     param <- model$param/speed/Timeresinsec
     tmp <- dexp(x=x,1/param)
  } else if (model$distr == "dnorm") {
     param1 <- model$param[1]/speed/Timeresinsec
     param2 <- model$param[2]/speed/Timeresinsec
     tmp <- dnorm(x=x,param1,param2)
  }
  # to be written in a nicer way
  # param  <-lapply(model$param,function(x) x/speed/Timeresinsec)
  # l <- c(list(x),param)
  # tmp <- do.call(model$ditrib,l)
  # UHvec <- z+(1-z)*tmp

  res <- model$z+(1-model$z)*tmp
  #renormalise such that the sum is unity
  res <- res/sum(res)

  return(res)
}

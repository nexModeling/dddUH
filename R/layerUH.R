#' Unit Hydrograph of Layers
#'
#' The function \code{layerUH()} compute the UH of the saturation layers
#' @param maxL Max distance (from distance distribution, DD)
#' @param speed celerity
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param model list of parameters about the UH
#'  example: \code{list(z=0.5,distr="dexp",param=c(midL))}
#'  with:
#'  - z: arealfraction of DD (what area with distance zero to river), midL: Mean distance (from distance distribution, DD) for soils
#'  - distr: dexp, dnorm,...
#'  - param: c(valpar1,valpar2)
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' layerUH()
#' }
layerUH <-function(maxL,speed,Timeresinsec,model){
  NoL <- length(speed)
  delaySteps <- dddCelerity::nbSteps(maxL=maxL,speed=speed[NoL],Timeresinsec=Timeresinsec)
  res <- matrix(0,nrow=NoL,ncol=delaySteps)
  for (i in 1:NoL){
     idelay <- dddCelerity::nbSteps(maxL=maxL,speed=speed[i],Timeresinsec=Timeresinsec)
     res[i,1:idelay] <- UHvec(maxL=maxL,speed=speed[i],Timeresinsec=Timeresinsec,model=model)
  }
  return(res)
}

#' Unit Hydrograph of slopes
#'
#' The function \code{UHslopes()} compute the UH of the slopes
#' @param layerUH UH of the saturation layers
#' @param ddist states of each saturation level.
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' UHslopes()
#' }
UHslopes <-function(layerUH,ddist){
  NoL <- length(ddist)
  res <- colSums(ddist[1:NoL]*layerUH[1:NoL,])
  return(res)
}

#' Convolve UH with the one of the river
#'
#' The function \code{xConvRiver()} convolves one UH with the one of the river
#' @param UHx Unit Hydrograph of a specific type of terrain
#' @param UHriver Unit Hydrograp of the river
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' UH.xConvRiver()
#' }
xConvRiver <-function(UHx,UHriver){
  xConvRiver <-  convolve(UHx,UHriver, type="o")
  return(xConvRiver)
}

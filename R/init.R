#' Unit Hydrograph initializing
#'
#' The funtion initialize the main features of the Unit Hydrograph:
#' - The UH of the river,
#' - the UH of the layers
#' - The UH of the Mean Annual Discharge (MAD)
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e), NoL=f)
#' @param modelRiver list of parameters about the river
#'  list(maxL=a,speed=b,nbStepsDelay=c ,z=d,distr="dnorm",param=c(e,f))
#' @param modelMAD list of parameters about the Mean Annual Discharge
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' init()
#' }

init <-function(Timeresinsec,
                   modelLayer,
                   modelRiver,
                   modelMAD){

  ############
  #LayerUH
  ############
  model <- list(z=modelLayer$z,distr=modelLayer$distr,param=modelLayer$param)
  res_layerUH <- layerUH(maxL=modelLayer$maxL,speed=modelLayer$speed,Timeresinsec=Timeresinsec,model=model)


  ###########
  #UH RIVER
  ###########
  model <- list(z=modelRiver$z,distr=modelRiver$distr,param=modelRiver$param)
  if(modelRiver$nbStepsDelay==1) {
    UHriver <- 1
  } else {
    res_UHriver <- UHvec(maxL=modelRiver$maxL,speed=modelRiver$speed,Timeresinsec=Timeresinsec,model=model)
  }

  ############
  #UH MAD
  ############
  model <- list(z=modelMAD$z,distr=modelMAD$distr,param=modelMAD$param)
  res_UHMAD <- UHvec(maxL=modelMAD$maxL,speed=modelMAD$speed,Timeresinsec=Timeresinsec,model=model)


  res <- list( UHriver = res_UHriver,
               layerUH = res_layerUH,
               UHMAD   = res_UHMAD
               )

  rm(res_UHriver,res_layerUH,res_UHMAD)

  return(res)
}

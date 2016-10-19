#' Unit Hydrograph initializing
#'
#' The funtion initialize the main Unit Hydrographs:
#' - The UH of the river,
#' - the UH of the layers
#' - The UH of the Mean Annual Discharge (MAD)
#' @param method method for the initialization, "load", "source", "processed"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param, NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelMAD list of parameters about the Mean Annual Discharge
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @keywords UH
#' @export
#' @examples
#' \dontrun{
#' init.UH()
#' }

init.UH <-function(method=NULL,path=NULL,Timeresinsec,modelLayer,modelRiver,modelMAD){


   UH <- switch(method,
    "processed"    = init.processed(Timeresinsec=Timeresinsec,modelLayer=modelLayer,modelRiver=modelRiver,modelMAD=modelMAD),
    "load"         = init.load(path=path),
    "source"       = init.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(UH)
}

init.load <- function(path){
  load(paste0(path,"UH.rda"))
  return(UH)
}

init.source <- function(path){
  source(paste0(path,"UH.R"),local=TRUE)
  return(UH)
}


init.processed <- function(Timeresinsec,modelLayer,modelRiver,modelMAD) {
   if( (!is.null(Timeresinsec)) && (!is.null(modelLayer)) &&
       (!is.null(modelRiver)) && (!is.null(modelMAD)) ) {

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

   } else stop("NULL arguments in init.processed UH")

}

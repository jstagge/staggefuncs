#' Read in saved data from USGS
#'
#' Reads in local USGS data file using dataRetreival package.
#'
#' @param site_id USGS site id
#' @param param_cv USGS parameter id
#' @param destination_folder location to save file
#' @param time_param time accumulation 
#'
#' @return dl_result a character stating "Success" or "Failure"
#'
#'
#' @export


usgs_readin <- function(site_id, param_cd="00060", time_param="daily", destination_folder = getwd()) {
	require(dataRetrieval)

	### Read in file
	dl_location <- file.path(destination_folder, paste0(site_id,"_",param_cd,"_",time_param,".txt"))
	usgs_data <- importRDB1(dl_location)
	
	return(usgs_data)
	}
	
	
	
	
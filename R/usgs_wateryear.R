#' Determine water year
#'
#' Reads in a year and month and calculates water year.
#'
#' @param year USGS year of observation
#' @param month USGS month of observation
#' @param first_month first month of water year, assumes USGS default as Oct 1
#'
#' @return water_year water year of observation
#'
#'
#' @export


usgs_wateryear <- function(year, month, first_month=10) {
	water_year <- year
	water_year[month >= first_month] <- water_year[month >=first_month] + 1
	return(water_year)
}



#' Inverse of water year calculation
#'
#' Reads in a water year and month and calculates the year.
#'
#' @param water_year water year of observation
#' @param month USGS month of observation
#' @param first_month first month of water year, assumes USGS default as Oct 1
#'
#' @return year year of observation
#'
#'
#' @export



usgs_wateryear_inverse <- function(water_year, month, first_month=10) {
	year <- water_year
	year[month >= first_month] <- year[month >= first_month] - 1
	return(year)
}


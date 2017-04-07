# *------------------------------------------------------------------
# | FUNCTION NAME: date_breaks
# | FILE NAME: date_breaks.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        dates_ts - vector with ordered numbers of dates
# |				   break_length - number of days to include in each break
# |                
# |     Out:       break_list - list with start and end dates, along with count
# | 
# |     Desc:      Calculates breaks in a time series for reading in portions of a 
# | 				large netcdf file.
# |                
# *------------------------------------------------------------------
date_breaks <- function(date_ts, break_length) {
	### Split the dates	
	dates_subset <- split(date_ts, ceiling(seq_along(date_ts)/break_length))

	### Former code
	#length_date <- length(dates_ts)
	#max_start <- floor(length_date/break_length)
	#start <- c(1, 1 + break_length*seq(1,max_start))
	#end <- c(break_length*seq(1,max_start), length_date)
	#count <- end - start + 1
	#break_list<- list(start=start, end=end, count=count)

	break_list<- dates_subset
return(break_list)
}





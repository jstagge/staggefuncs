# *------------------------------------------------------------------
# | FUNCTION NAME: linear_interp
# | FILE NAME: linear_interp.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        ts - regular time series
# |                max_gap - maximum time gap to fill
# |                na_rm  - FALSE for leaving non-adjusted NAs
# |                
# |     Out:       interp_vals - values after interpolation
# |					interp_test - a TRUE (interpolated) FALSE (not interpolated) time series
# | 
# |     Desc:      Accepts a time series and uses linear interpolation to fill gaps.
# |                
# *------------------------------------------------------------------

linear_interp <- function(ts, max_gap, na_rm=FALSE) {
	require(zoo)
	
	interp <- na.approx(ts, na.rm=na_rm, maxgap=max_gap)
	### Create a test for whether it was interpolated
	interp_test <- is.na(ts) & is.finite(interp)

	return(list(interp_vals=interp, interp_test=interp_test))
}
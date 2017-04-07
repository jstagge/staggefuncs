# *------------------------------------------------------------------
# | FUNCTION NAME: last_day_month
# | FILE NAME: last_day_month.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        start_day - first day of sequence
# |                end_day - last day of sequence
# |                
# |     Out:       last_day - sequence of the last day of each month
# | 
# |     Desc:      Accepts a start and end date, creates a sequence of the last day of
# |					each month between the two.
# |                
# *------------------------------------------------------------------

last_day_month <- function(start_day, end_day) {
	require(lubridate)

	### If the date is on the first, push it forward one day to make it clear which month it should be in
	if (day(start_day) == 1) { start_day <- start_day + 1 }
	if (day(end_day) == 1) { end_day <- end_day + 1 }

	### Round start_day and end_day to the first day of next month by moving it forward one month and rounding
	start_day <- floor_date(start_day %m+% months(1)-1, "month")
	end_day <- floor_date(end_day %m+% months(1)-1, "month")

	### Generate sequence of days by month and then subtract one day to get last day of month
	last_day <- seq(start_day, end_day, by="1 month") - 1

	return(last_day)
}

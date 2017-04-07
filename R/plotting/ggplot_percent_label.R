# *------------------------------------------------------------------
# | FUNCTION NAME: ggplot_percent_label
# | FILE NAME: ggplot_percent_label.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        theargument - label for ggplot
# |					siglevel  -  significant figures
# |                
# |     Out:       label for ggplot2
# | 
# |     Desc:      This function allows for custom percentage labels
# |					in ggplot2.
# *------------------------------------------------------------------

ggplot_percent_label <-function(theargument, siglevel=3) { 
 	stopifnot(is.numeric(theargument)) 
 	paste(signif(theargument, siglevel) * 100, "%", sep="") 
} 

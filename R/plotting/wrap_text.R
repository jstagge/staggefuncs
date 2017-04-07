# *------------------------------------------------------------------
# | FUNCTION NAME: wrap_text
# | FILE NAME: wrap_text.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        text - original text
# |                length - maximum number of characters per row
# |                
# |     Out:       final_text - text with line breaks every length-th character
# | 
# |     Desc:      Reads in text and splits into separate lines
# |                
# *------------------------------------------------------------------

wrap_text <- function(text, length=30) {
final_text <- unlist(lapply(flow_obs$site_name[1], function(x) paste(strwrap(text,length), collapse="\n")))
return(final_text)
}
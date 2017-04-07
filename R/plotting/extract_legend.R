# *------------------------------------------------------------------
# | FUNCTION NAME: extract_legend
# | FILE NAME: extract_legend.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        a.gplot - a ggplot2 figure
# |                
# |     Out:       legend - legend extracted from ggplot2
# | 
# |     Desc:      Extract the legend from a ggplot2 object
# |                
# *------------------------------------------------------------------

#Extract Legend 
extract_legend <-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

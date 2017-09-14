
#' Theme Classic New
#'
#' An updated version of theme_classic from ggplot2 using my personal preferences. Had to create this due to a previous error in ggplot2.
#'
#' @param legend_pt font size
#'
#' @return theme_out theme for ggplot2
#'
#'
#' @export
theme_classic_new <- function(legend_pt=10) {
	### Text size is legend_pt * 0.8, axis titles legend_pt*0.9
	theme_out <- theme_classic(legend_pt)
	
	### Make labels bold
	theme_out <- theme_out + theme(legend.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	theme_out <- theme_out + theme(axis.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	
	### Remove grid lines and background
	theme_out <- theme_out + theme(panel.grid.major = element_blank())
	theme_out <- theme_out + theme(panel.grid.minor = element_blank())
	theme_out <- theme_out + theme(panel.background = element_rect(fill = NA, colour = NA))
	#theme_out <- theme_out + theme(panel.background = element_blank())
	
	### Remove grey background from legend
	theme_out <- theme_out + theme(legend.key = element_blank())
	
	### Add a box around facets
	theme_out <- theme_out + theme(strip.background =  element_rect(fill = "grey85", colour = "grey30"))

	theme_out <- theme_out + theme(axis.line.x = element_line(colour = 'black', size=0.3, linetype='solid'))
	theme_out <- theme_out + theme(axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))
			
	return(theme_out)
}


#' Theme Classic New with Major Gridlines
#'
#' An updated version of theme_classic from ggplot2 using my personal preferences with major gridlines
#'
#' @param legend_pt font size
#'
#' @return theme_out theme for ggplot2
#'
#'
#' @export
theme_classic_new_majgrid <- function(legend_pt=10) {
	theme_out <- theme_classic_new(legend_pt)
	theme_out <- theme_out + theme( panel.grid.major = element_line(colour = "grey89", size = 0.25))
			
	return(theme_out)
}

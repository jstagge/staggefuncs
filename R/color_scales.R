#' Color Blind Palette
#'
#' Pulls together some of the best or more common palettes for R, including those focused on colorblindness.
#' wong palette is based on Wong, Bang. "Points of view: Color blindness." nature methods 8.6 (2011): 441-441.
#' few palette is based on qualitative color palettes from Stephen Few, http://www.perceptualedge.com/articles/visual_business_intelligence/rules_for_using_color.pdf.
#' ptol palette is based on qualitative color palettes from Paul Tol, https://personal.sron.nl/~pault/colourschemes.pdf.
#' d3 palette is based on d3 standard. nature is based on Nature Publishing Group
#'
#' @param n numeric number of colors to generate
#' @param pal character string of color palette
#' @param sort TRUE sorts colors based on their distances in HCL space
#'
#' @return pal_result vector color palette in HEX
#'
#' @export
cb_pal <- function(n, pal="wong", sort=TRUE){
require(assertthat)

	if(pal == "wong"){
		### Check if n is too large
		assert_that(n <= 7)
		### return Wong palette
		pal_result <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
		pal_result <- pal_result[1:n]
	}
	if(pal == "custom"){
		### Check if n is too large
		assert_that(n <= 6)
		if (n <= 4){
			pal_result <- c("#4477AA", "#CCBB44", "#EE6677", "#228833")
		} else {
			pal_result <- c("#228833", "#66CCEE", "#EE6677", "#4477AA", "#CCBB44", "#AA3377", "#BBBBBB")
		}
		pal_result <- pal_result[1:n]
	}	
	if (pal == "ptol"){
		require(ggthemes)
		### Check if n is too large
		assert_that(n <= 12)	
		### return Paul Tol palette
		pal_result <- ptol_pal()(n)
	}
	if (pal == "few"){
		require(ggthemes)
		### Check if n is too large
		assert_that(n <= 7)	
		### return Paul Tol palette
		pal_result <- few_pal()(n)
	}	
	if (pal == "d3"){
		require(ggsci)
		### Check if n is too large
		assert_that(n <= 20)		
		### Return d3 palette
		pal_result <- pal_d3(palette = "category20")(n)
	}
	if (pal == "nature"){
		require(ggsci)
		### Check if n is too large
		assert_that(n <= 10)		
		### Return d3 palette
		pal_result <- pal_npg()(n)
	}	

### If Sort is true, order based on largest HCL difference 	
if(sort==TRUE){
pal_result <- separate_qual_colors(pal_result)
}
	
return(pal_result)
}



#' Order Qualitative Colors
#'
#' Orders qualitative colors by finding the largest difference between each color based on HCL
#'
#' @param pal vector of HEX colors
#' @param preserve_first TRUE preserve the first color in the vector.
#'
#' @return color_separated vector of colors separated by HCL
#'
#' @export
separate_qual_colors <- function(pal, preserve_first=TRUE){
### Load required packages
require(colorspace)
require(tidyverse)
require(tspmeta)

### Select the color vector
color_vec <- pal
n_colors <- length(color_vec)

### Catch for double FF at end of HEX
too_long_test <- sapply(color_vec, nchar) > 7 
color_vec[too_long_test] <- substr(color_vec[too_long_test],1,7)

### Convert to HCL
hcl_vec <- color_vec %>% 
	hex2RGB %>%
	as("polarLUV")

### Convert to matrix
hcl_mat <- coords(hcl_vec)

### Process vertices
vertices <- hcl_mat
rownames(vertices) <- seq(1,n_colors)

### Calculate 3-D distance between points
dist_mat <- dist(scale(vertices), method="manhattan") #
dist_mat <- as.matrix(dist_mat)

### Flip the sign of distance matrix and run travelling salesman optimization
node <- data.frame(color=as.numeric(rownames(vertices)),x=as.numeric(hcl_mat[,1]),y=as.numeric(hcl_mat[,2]),z=as.numeric(hcl_mat[,3]))
tsp.ins <- tsp_instance(as.matrix(node[,c(2,3)]), -dist_mat)
tour <- run_solver(tsp.ins, method="2-opt")

### Create new color palette
color_separated <- color_vec[c(tour)]

### If preserve_first is true, reorganize so first color is maintained
first_index <- which(color_separated == color_vec[1])

if (preserve_first == TRUE & first_index != 1) {
	new_order <- c(seq(first_index,n_colors), seq(1, first_index-1))
	color_separated <- color_separated[new_order]
}

### Return result
return(color_separated)
}






#' Create ColorBlind Check Plot
#'
#' Create a plot with all colors in a palette, followed by the same figure with protan, deutan, and tritan color deficiency.
#'
#' @param pal vector of HEX colors
#' @param shape character squares or wheel, defaults to squares.
#'
#' @return color plot
#'
#' @export
cb_check_plot <- function(pal, shape="squares"){
#
require(dichromat)
### Function for wheel
wheel <- function(col, radius = 1, ...)
pie(rep(1, length(col)), col = col, radius = radius, ...)

### Create 4 panels
par(mfrow = c(2, 2))

### Plot colors
if (shape=="squares"){
	require(scales)
	
	show_col(pal) 
	show_col(dichromat(pal, type="protan"))
	show_col(dichromat(pal, type="deutan"))
	show_col(dichromat(pal, type="tritan"))
} else {
	wheel(pal)
	wheel(dichromat(pal, type="protan"))
	wheel(dichromat(pal, type="deutan"))
	wheel(dichromat(pal, type="tritan"))
}
}


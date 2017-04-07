
# *------------------------------------------------------------------
# | FUNCTION NAME: pca_plot_wrapper
# | FILE NAME: pca_plot.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       data - a dataframe with PCA importance data
# |					write_folder  -  location to save plots
# | 				write_file  -  file name for plots
# |
# |     Out:      
# | 
# |     Desc:     Runs the following three PCA diagnostic plots
# |                
# *------------------------------------------------------------------

pca_plot_wrapper <- function(data, write_folder, write_file){
require(ggplot2)
require(svglite)

### Run Eigen Plot
p <- pca_eigen_plot(importance=data)
	
### Save Eigen Plot
ggsave(file.path(write_folder,paste0( write_file, "_eigen.png")), p, width=4, height=3, dpi=600)
ggsave(file.path(write_folder,paste0( write_file, "_eigen.svg")), p, width=4, height=3)
ggsave(file.path(write_folder,paste0( write_file, "_eigen.pdf")), p, width=4, height=3)

### Run Variance Explained Plot
p <- pca_var_plot(importance=data)
	
### Save Eigen Plot
ggsave(file.path(write_folder,paste0( write_file, "_var.png")), p, width=4, height=3, dpi=600)
ggsave(file.path(write_folder,paste0( write_file, "_var.svg")), p, width=4, height=3)
ggsave(file.path(write_folder,paste0( write_file, "_var.pdf")), p, width=4, height=3)


### Run Eigen Plot
p <- pca_cum_var_plot(importance=data)
	
### Save Eigen Plot
ggsave(file.path(write_folder,paste0( write_file, "_cum_var.png")), p, width=4, height=3, dpi=600)
ggsave(file.path(write_folder,paste0( write_file, "_cum_var.svg")), p, width=4, height=3)
ggsave(file.path(write_folder,paste0( write_file, "_cum_var.pdf")), p, width=4, height=3)


}




# *------------------------------------------------------------------
# | FUNCTION NAME: pca_eigen_plot
# | FILE NAME: pca_plot.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       importance - a dataframe with PCA importance data
# |
# |     Out:      p - an eigen value plot
# | 
# |     Desc:     Plots Eigen values from PCA analysis
# |                
# *------------------------------------------------------------------

pca_eigen_plot <- function(importance) {

p <- ggplot(importance, aes(Component,EigenVal)) 
p <- p + geom_hline(yintercept=1, linetype="longdash", color="grey40")
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Eigen Value")
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_correct()
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}

# *------------------------------------------------------------------
# | FUNCTION NAME: pca_var_plot
# | FILE NAME: pca_plot.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       importance - a dataframe with PCA importance data
# |
# |     Out:      p - a plot of Proportion Variance explained
# | 
# |     Desc:     Plots Proportion Variance explained from PCA analysis
# |                
# *------------------------------------------------------------------
pca_var_plot <- function(importance) {

p <- ggplot(importance, aes(Component,Proportion.of.Variance)) 
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Variance Explained", labels = ggplot_percent_label)
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_correct()
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}

# *------------------------------------------------------------------
# | FUNCTION NAME: pca_cum_var_plot
# | FILE NAME: pca_plot.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       importance - a dataframe with PCA importance data
# |
# |     Out:      p - a plot of Cumulative Variance explained
# | 
# |     Desc:     Plots Cumulative Variance explained from PCA analysis
# |                
# *------------------------------------------------------------------
pca_cum_var_plot <- function(importance) {

p <- ggplot(importance, aes(Component,Cumulative.Proportion)) 
p <- p + geom_hline(yintercept=1, linetype="longdash", color="grey40")
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Cumulative Variance Explained", labels = ggplot_percent_label)
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_correct()
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}


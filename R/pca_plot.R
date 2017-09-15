
#' PCA Eigen Plot
#'
#' Plots Eigen values from PCA analysis
#'
#' @param importance a dataframe with PCA importance data
#'
#' @return p an eigen value plot
#'
#'
#' @export
pca_eigen_plot <- function(importance) {

p <- ggplot(importance, aes(Component,EigenVal)) 
p <- p + geom_hline(yintercept=1, linetype="longdash", color="grey40")
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Eigen Value")
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_new(9)
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}


#' PCA Proportion of Variance Explained Plot
#'
#' Plots Proportion of Variance explained from PCA analysis
#'
#' @param importance a dataframe with PCA importance data
#'
#' @return p a plot of Proportion Variance explained
#'
#' @export
pca_var_plot <- function(importance) {

p <- ggplot(importance, aes(Component,Proportion.of.Variance)) 
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Variance Explained", labels = ggplot_percent_label)
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_new(9)
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}


#' PCA Cumulative Variance Explained Plot
#'
#' Plots Cumulative Variance explained from PCA analysis
#'
#' @param importance a dataframe with PCA importance data
#'
#' @return p a plot of Cumulative Variance explained
#'
#' @export
pca_cum_var_plot <- function(importance) {

p <- ggplot(importance, aes(Component,Cumulative.Proportion)) 
p <- p + geom_hline(yintercept=1, linetype="longdash", color="grey40")
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Cumulative Variance Explained", labels = ggplot_percent_label)
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_new(9)
p <- p + coord_cartesian(xlim=c(0.5,14))

return(p)
}


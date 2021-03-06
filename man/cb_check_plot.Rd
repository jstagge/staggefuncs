% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_scales.R
\name{cb_check_plot}
\alias{cb_check_plot}
\title{Create ColorBlind Check Plot}
\usage{
cb_check_plot(pal, shape = "squares")
}
\arguments{
\item{pal}{vector of HEX colors}

\item{shape}{character squares or wheel, defaults to squares.}
}
\value{
color plot
}
\description{
Create a plot with all colors in a palette, followed by the same figure with protan, deutan, and tritan color deficiency.
}

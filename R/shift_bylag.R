#' Shift a time series by a given lag
#'
#' Copied from https://www.r-bloggers.com/generating-a-laglead-variables/
#'
#' @param x vector timeseries
#' @param shift_by numeric number of spaces to shift. Can be positive or negative.
#'
#' @return out resulting numeric vector
#'
#'
#' @export

shift_bylag <-function(x,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
 
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
 
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}
#' Wrap Text
#'
#' Reads in text and splits into separate lines
#'
#' @param text original text
#' @param length maximum number of characters per row
#'
#' @return final_text text with line breaks every length-th character
#'
#'
#' @export

wrap_text <- function(text, length=30) {
final_text <- unlist(lapply(flow_obs$site_name[1], function(x) paste(strwrap(text,length), collapse="\n")))
return(final_text)
}
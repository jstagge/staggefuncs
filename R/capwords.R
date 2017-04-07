#' Capitalize first letter
#'
#' Capitalizes first letter of a string. Copied from https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
#'
#' @param s character string to be capitalized 
#' @param strict TRUE forces everything to 1st letter capital; FALSE allows for things like acronyms to remain all caps; Defaults to TRUE.
#'
#' @return s_cap Original string with first letter capitalization
#'
#' @examples
#' plot_crayons()
#'
#' @export

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    s_cap <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    return(s_cap)
}

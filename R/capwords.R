# *------------------------------------------------------------------
# | FUNCTION NAME: capwords
# | FILE NAME: capwords.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:       s - character string to be capitalized 
# |               strict -  TRUE forces everything to 1st letter capital, even all caps words
# |							FALSE allows for things like acronyms to remain all caps
# |     Out:      s_cap -  Original string with first letter capitalization
# | 
# |     Desc:      This function capitalizes the first letter of each word.
# |                
# *------------------------------------------------------------------

### Copied from https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    s_cap <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
    return(s_cap)
}

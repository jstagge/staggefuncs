# *------------------------------------------------------------------
# | FUNCTION NAME: summary_se
# | FILE NAME: summary_se.R
# | DATE: 
# | CREATED BY:  http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/       
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                measurevar: the name of a column that contains the variable to be summariezed
# |                groupvars: a vector containing names of columns that contain grouping variables
# |                na.rm: a boolean that indicates whether to ignore NA's
# |                conf.interval: the percent range of the confidence interval (default is 95%)
# |					quant: a range of quantiles to calculate, default is 10 and 90%
# |     Out:       datac - the resulting summary
# | 
# |     Desc:      This function provides typical 
# *------------------------------------------------------------------

summary_se <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE, quant=c(0.1,0.9)) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm),
          quantile_low = quantile(xx[[col]], quant[1], na.rm=na.rm),
          quantile_high = quantile(xx[[col]], quant[2], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    names(datac)[[5]] <- paste0(quant[1],"_quant")
    names(datac)[[6]] <- paste0(quant[2],"_quant")

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
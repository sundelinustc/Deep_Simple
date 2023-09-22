### Remove noises (may include linear trends or constants) from data

## Load packages
if (!require("pacman")) install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  dplyr,        # data manipulation
  data.table,   # quick read & write files
  pracma        # detrend data
)

# function to regress out confounds from each variable of interest
# Input
# -- x, a vector (or a dataframe column) of signals (numeric values)
# -- df_con, dataframe of confounds
# -- fdetrend, whether/how to remove the linear trend from signals, e.g., "linear" 
#.   or "constant", or NULL (default, means no detrend)
# Output
# -- a vector of signals after removing the effects of noises
DS_regcon <- function(y, fdetrend=NULL, df_con){
  if(all(is.na(y))){
    # if the variable is all NA, return NA
    return(y)
    
  }else{
    
    # remove linear trend from signals if needed
    if(!is.null(fdetrend)){
      y <- y %>% detrend(tt = fdetrend)
    }
    
    # make the dataframe for linear model
    df <- data.frame(y = y)
    
    # difference in length between signals & noises
    dif_len <- dim(df)[[1]] - dim(df_con)[[1]]
    
    # match signal & noises in length 
    # e.g., the first a few data points of signals but not noises may be 
    # discarded during preprocessing
    if(dif_len > 0){
      # length of signals is greater than that of the noises
      df <- df %>% tail(-dif_len) # keep the tail part of signals
    }else if(dif_len < 0){
      # length of signals is shorter than that of the noises
      df_con <- df_con %>% tail(dif_len) # keep the tail part of noises
    }else{}
    
    # combine signals & noises for the linear regression model
    df <- df %>% cbind(df_con)
    
    
    # get the residuals after removing the effects of confounds
    res <- lm(y ~ ., data=df) %>% residuals()
    # corrected value = residuals + mean
    return(res + mean(y, na.rm=T))
  }
}

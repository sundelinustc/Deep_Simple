### Remove noises from data

## Load packages
if (!require("pacman")) install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  # tidyverse,    # data management + ggplot2 graphics,
  # data.table,   # quick read & write files
  foreach,      # parallel processing
  doParallel   # parallel processing
  # pracma        # detrend data
)


# Input
# -- df, dataframe includes columns df$sig_fullname (each row: fullname of a signal file)
#.       and df$noise_fullname (each row: fullname of the corresponding noise file)
# -- fdetrend, whether/how to remove the linear trend from signals, e.g., "linear" 
#.   or "constant", or NULL (default, means no detrend)
# -- fcons, the names of noises to be removed
# -- fdir_o, the path of output files
# Output
# -- denoised file
DS_denoise <- function(df, fdetrend = "linear", 
                       fcons = c("csf","white_matter","framewise_displacement"), 
                       fdir_o = file.path('..','Processes','ts_corrected_detrend_csf_wm_FD')){
  
  system.time({
    # print beginning information
    print("Denoising signals. Parallel processing. Please be patient...")
    
    # setup parallel backend to use many processors
    cores <- detectCores()
    cl <- makeCluster(cores[1])
    registerDoParallel(cl)
    
    # Run parallel processing
    foreach(i = 1:dim(df)[1], .packages=c('dplyr','data.table')) %dopar% {
      
      source("./DS/DS_regcon.R")
      
      # signals (time series: No. time points x No. brain areas)
      df_sig <- df$sig_fullname[[i]] %>% fread()
      
      # confounds (time series: No. time points x No. confounding variables)
      df_con <- df$noise_fullname[[i]] %>% 
        fread(na.strings = c("n/a")) %>% 
        select(fcons)
      
      # fill missing values with 0
      df_con[is.na(df_con)] <- 0
      
      # combine signals and confounds for regression
      df_sig <- df_sig %>% cbind(df_con)
      
      # get the corrected signals for all variables of interest
      df_sig_corrected <-
        df_sig %>% select(starts_with("V")) %>% 
        sapply(DS_regcon, fdetrend="linear", df_con=df_con) %>% 
        as.data.frame()
      
      ## save into a .csv file
      # create the output path if it does not exist
      dir.create(fdir_o, recursive=TRUE, showWarnings = FALSE)
      # final output filename
      fout <- file.path(fdir_o, paste0(df$fID[[i]], "_signal_denoised.csv"))
      # save matrix into csv file
      df_sig_corrected %>% fwrite(fout)
    }
    
    #stop cluster
    stopCluster(cl)
    
    # print ending information
    print("Completed! Denoising signals. Please double check the outputs!")
    
  })
  
  


  
}
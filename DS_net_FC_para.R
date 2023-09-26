### Calculate static or dynamic functional connectivity using atlas-based time series through parallel processing

## Load packages
if (!require("pacman"))
  install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  dplyr,        # data management
  DescTools,    # for Fisher's r-to-z transformation
  data.table,   # to make read & write files quicker
  foreach,      # parallel processing
  doParallel    # parallel processing
  )
               
               
# Input
# --- fin, filename (full path) of atlas time series (time points x regions)
# --- win_width, window width, default=30 s
# --- win_overlap, window overlap, default=50%, while 100% means static rsFC
# --- TR, default=3 s
# --- idx, a vector of logical values (TRUE or FALSE) of areas of interest
# --- fdir_o, directory of the output files (.csv)
# Output
# --- a matrix of standard deviation for dynamic rsFC, or Fisher-Z-transformed Pearson's correlation coefficients for static rsFC
# --- output file (.csv) of the matrix for correlation coefficients (for static FC) or the standrad seviations of the correlation coefficients (for dynamic FC)
DS_net_FC <-
  function(fin,
           win_width = 30,
           win_overlap = 50,
           TR = 3,
           idx = NULL,
           fdir_o = file.path('..', 'Proesses','FC')
           ) {
    
    ## Load data & manipulations
    # load time series of all areas (Num. of time points x Num. of areas)
    df <- fin %>% fread()# %>% as.data.frame()
    # time series of areas of interest (default=NULL, means all areas)
    if(!is.null(idx)){df <- df[, idx, with=FALSE]}
    # raw column names (may include useless columns)
    col_df <- df %>% colnames() 
    # remove useless columns that are all NA or NaN (cause NA results across the whole matrix)
    df <- df %>% select(where(~ !all(is.na(.)))) 
    # new column names (no useless columns)
    col_df_new <- df %>% colnames() 
    # get column name difference
    col_diff <- setdiff(col_df, col_df_new) # differences
    
    ## dynamic FC
    if (win_overlap < 100) {
      # window width (in number of volumes, time points)
      ww <- round(win_width / TR) 
      # window overlap (in number of volumes, time points)
      wo <- round(win_width * win_overlap / (100 * TR)) 
      # sequence of the start time of each window
      ws <- seq(1, dim(df)[1] - ww + 1, ww - wo) 
      # the last start time must be able to correspond to a complete window
      # dim(df)[1]: length of the time series in number of volumes (time points)
      # ww-wo: step of adjacent windows in number of volumes (time points)
      
      # Z scores of correlation coefficients per time window
      Z <-
        array(0, dim = c(dim(df)[2], dim(df)[2], length(ws))) # an empty matrix to contain Z values
      for (i in 1:length(ws)) {
        Z1 <-
          df[ws[i]:(ws[i] + ww - 1), ] %>% 
          cor(use = 'na.or.complete') %>% 
          FisherZ() # 3D arrays of Z scores of corr coef
        diag(Z1) <- NA # diagonal of matrix to NA
        Z[, , i] <- Z1
      }
      
      # standard deviation matrix
      Z <- Z %>% apply(c(1, 2), sd, na.rm = TRUE)
      colnames(Z) <- col_df_new # this step is needed for SD calculations
      rownames(Z) <- col_df_new # this step is needed for SD calculations
      
    } else{
      # static rsFC
      Z <- array(0, dim = c(dim(df)[2], dim(df)[2])) # an empty matrix to contain Z values
      Z <-
        df %>% 
        cor(use = 'na.or.complete') %>% 
        FisherZ() # matrix of Z scores of correlation coefficients
      diag(Z) <- NA # diagonal of matrix to NA
    }
    
    # add columns & rows of NA for useless features
    if(length(col_diff) > 0){
      # add columns of NA
      new.column <- matrix(NA, 
                           nrow=dim(Z)[1], 
                           ncol=length(col_diff), 
                           dimnames=list(NULL,col_diff))
      Z <- Z %>% cbind(new.column)
      
      # add rows of NA
      new.row <- matrix(NA, 
                           ncol=dim(Z)[2], 
                           nrow=length(col_diff), 
                           dimnames=list(col_diff,NULL))
      Z <- Z %>% rbind(new.row)
    }
    
    # re-order the matrix according to the raw data column names
    Z <- Z[col_df, col_df]
    
    ## save into a .csv file
    # create the output path if it does not exist
    dir.create(fdir_o, recursive=TRUE, showWarnings = FALSE)
    # file extension
    fex  <- strsplit(basename(fin), split="\\.")[[1]][-1] 
    # replace file suffix
    fout <- sub(paste0(".",fex), paste0('_FC_',win_width,'_',win_overlap,'.csv'), fin) # suffix
    # final output filename
    fout <- file.path(fdir_o, basename(fout))
    
    # save matrix into csv file
    Z %>% as.data.table() %>% fwrite(fout)
    print(paste("FC matrix calculated & saved: ",fout))
    
    # return
    # return(Z) # DO NOT return it when parallelize processing
  }
     



# Input
# --- df, dataframe of subjects' info, including fID, path to the data, TR
# --- win_width, window width, default=30 s
# --- win_overlap, window overlap, default=50%, while 100% means static rsFC
# --- idx, a vector of logical values (TRUE or FALSE) of areas of interest
# --- fdir_o, directory of the output files (.csv)
# Output
# --- a matrix of standard deviation for dynamic rsFC, or Fisher-Z-transformed Pearson's correlation coefficients for static rsFC
# --- output file (.csv) of the matrix for correlation coefficients (for static FC) or the standrad seviations of the correlation coefficients (for dynamic FC)
DS_net_FC_para <-
  function(df,
           win_width = 30,
           win_overlap = 50,
           idx = NULL,
           fdir_o = file.path('..', 'Processes','FC_Matrix')
  ) {        
    
    system.time({
      # print beginning information
      print("Static or Dynamic Functional Connection Calculation!!! Parallel processing. Please be patient...")
      print(paste("window width =",win_width, "    window overlap =",win_overlap,"%"))
      
      # setup parallel backend to use many processors
      cores <- detectCores()
      cl <- makeCluster(cores[1])
      registerDoParallel(cl)
      
      # Run parallel processing
      foreach(i = 1:dim(df)[1], .packages=c('dplyr','data.table','DescTools'), 
              .export=c('DS_net_FC')) %dopar% {
                
                # call the function per core
                DS_net_FC(df$sig_denoised[[i]], win_width, win_overlap, df$TR[[i]], NULL, fdir_o) 
              }
      
      #stop cluster
      stopCluster(cl)
      
      # print ending information
      print("Completed! Static or Dynamic Functional Connection Calculation. Please double check the outputs!")
      
    })
    
  }
### Average atlas-based time series (average across regions per time point)

## Load packages
if (!require("pacman"))
  install.packages("pacman") # make sure that pacman is installed

pacman::p_load(tidyverse,    # data management + ggplot2 graphics
               data.table    # to make read & write files quicker
)


# Input
# --- fin, filename (full path) of region-specific time series (time points x regions)
# --- fnet, a vector of name of nets (each net consists of one to several regions), e.g. c('DMN', 'DMN', 'CON'...)
# --- froi, a vector of number representing the index of regions of interest, e.g. c(1:400)
# --- fdir_o, directory of the output files (.csv)
# Output
# --- a dataframe of averaged time series (time points x networks)
# --- output file (.csv) of the averaged time series (time points x networks)
DS_net_avg <- function(fin, fnet, froi, fdir_o){
  ## load data & calculate the mean values per net
  dat <-
    fin %>% fread() %>% t() %>% as.data.frame() %>% mutate(net = fnet) %>%
    filter(row_number() %in% froi) %>%
    group_by(net) %>% # group by network label (17 networks + 4 sub)
    dplyr::summarise_all( ~ mean(., na.rm = TRUE))  # get mean ts per network
  
  ## convert to TRs x networks
  df <- dat[, 2:dim(dat)[[2]]] %>% t() %>% as.data.frame()
  # set column name
  colnames(df) <- dat$net 
  
  ## save into a .csv file
  # create the output path if it does not exist
  dir.create(fdir_o, recursive=TRUE, showWarnings = FALSE)
  # replace suffix
  fout <- sub('.tsv', paste0('_avg.csv'), fin) # suffix
  fseg <- fout %>% strsplit(split = '/') %>% .[[1]]
  len_fseg <- length(fseg) # length of the file segments
  # final output filename
  fout <- file.path(fdir_o, fseg[[len_fseg]])
  # save matrix into csv file
  df %>% fwrite(fout)
  print(paste("Averaged time series calculated & saved: ",fout))
  
    
    # return
    # return(df) # DO NOT return it when parallelize processing
  }
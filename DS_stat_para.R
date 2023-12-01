### General statistical analysis function (lm/lmer, t.test, chisq.test) using parallel processing

## Load packages
if (!require("pacman")) install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  dplyr,        # data management
  stringr,      # character manipulation
  data.table,   # quick load and write files
  foreach,      # parallel processing
  doParallel,   # parallel processing
  
  # car,          # for leveneTest() & outlierTest()
  # FSA,          # for post hoc using dunnTest()
  # rcompanion,   # for post hoc using chisq.test()
  # rstatix,      # statistics
  # corrr,        # correlation analayis for numeric variables
  lme4,         # linear regression models
  lmerTest,     # p-value that are not included in lme4
  
  # gtsummary,    # summary statistics and tests
  # table1,       # to make Table1
  broom,        # tidy & glance function to give model info
  broom.mixed   # broom for linear models with mixed effects
  # janitor,      # adding totals and percents to tables
  # flextable,    # converting tables to HTML
)

# function to extract data from indexed columns in a given file
# Input
# -- fn, fullname of the data file
# -- col_selected, a vector of the index of the specific columns or column names, default=NULL (all columns)
# Output
# -- a vector of the extracted values
myfun_extract <- function(fn, col_selected=NULL){return(fn %>% fread(select=as.vector(col_selected)) %>% as.matrix() %>% c())}

# function to concatenate values of the same row into a vector
# Input
# -- irow, the row index
# -- M, the matrix made by the function of myfun_extract()
# Output
# -- a vector of values of the same row
myfun_concatenate <- function(irow, M){return(M[,irow,] %>% c())}

# main function of apply pre-defined statistical models on each of the dependent variable through a parallel way
# Input
# --- fn_cleaned, filename of cleaned data (.csv), row=subject, column=feature 
#.                (could also be the fullname of data files to extract features.
#.                In such cases, the corresponding column names start with "INFILE_")
# --- fn_info,    filename of the info of cleaned data (.csv), row=feature, column=attributes(domain,atlas, etc.)
# --- Y,          vector of the names of dependent variables, e.g. c("roi_1","roi_2")
# --- fn_models,  filename of the .csv file of statistical models (row=model), e.g. "lm(y ~ Group + Age)" or "lmer(y ~ Group + Age + (1|SITE))"
# --- fdir_o,     the directory of the final outputs (statistical outputs + model estimations)
# --- col_selected, a vector of numeric index of columns or column names to be extracted, default=NULL (all values)
DS_stat_para <- function(fn_cleaned, fn_info=NULL, Y, fn_models, fdir_o, col_selected=NULL) {
  
  # load cleaned data
  df0 <- fn_cleaned %>% fread() %>% as.data.frame()
  
  # re-organize data structure to facilitate the analyses on data from INFILE (individual data files)
  df_in_data     <- df0 %>% select(starts_with("INFILE_"))  # dataframe of individual data filenames
  df_not_in_data <- df0 %>% select(!starts_with("INFILE_")) # dataframe of data not stored in individual data files
  colname_not_in <- df_not_in_data %>% colnames() # get the name of the data NOT from individual files to re-organize Data_cleaned_info.csv
  
  # load fn_info if it exists
  if (!is.null(fn_info)){
    # load cleaned data info
    df0_info  <- fn_info %>% fread() %>% as.data.frame()
    
    # re-organize data_info structure 
    colname_in <- setdiff(df0_info$VarName, colname_not_in)
    df0_info <- df0_info[match(c(colname_not_in, colname_in), df0_info$VarName), ]
  
    # if df0 contains INFILE(s) which need load data fro files
    if(dim(df0)[2] != dim(df0_info)[1]){
      # extract data from indexed columns across datatype (cols) & subjects (rows)
      M <- df_in_data  %>% apply(c(1,2), myfun_extract, col_selected) # ROI x sbj x data_type
      
      # concatenate the data of the same subject into a row in the matrix
      df_data_in1 <- seq(dim(M)[2]) %>% sapply(myfun_concatenate, M) %>% t() %>% as.data.frame() # sbj x (ROI x data_type)
      colnames(df_data_in1) <- colname_in
      
      # re-generate df0 (now actually all data, not data + filenames)
      df0 <- cbind(df_not_in_data, df_data_in1) # first are the data NOT from individual files, and then the data from individual files
      df0[df0==""] <- NA # replace "" with NA, especially for categorical variables using characters as levels
    }
    
  
    # set specific columns as factor
    # if there is NO column called "VarClass" in df0_info, make a coulmn of VarClass
    if (!"VarClass" %in% colnames(df0_info)) {
      df0_info$VarClass <- apply(df0, 2, function(x)
        ifelse(length(unique(x)) > 5, 'Numeric', 'Categorical')) # categorical variables if No. of unique values > 5
    }else{
      # if there IS a column called "VarClass", we need to set categorical variables
      # categorical variables without order
      name_cat_NoOrder <- df0_info %>% filter(VarClass=='Categorical') %>% filter(OrderCategorical=="") %>% .$VarName
      # set the above categorical variables as factors
      df0[name_cat_NoOrder] <- lapply(df0[name_cat_NoOrder], factor)
    
      # categorical variables with order
      name_cat_Order <- df0_info %>% filter(VarClass=='Categorical') %>% filter(OrderCategorical!="") %>% .$VarName
      # set the above categorical variables as factors with order
      for (ncat in name_cat_Order){
        # for each such variable
        nlevels <- df0_info[df0_info$VarName==ncat, "OrderCategorical"] %>% str_split(";") %>% unlist()
        df0[[ncat]] <- df0[[ncat]] %>% factor(levels = nlevels)
      }
    
    }
  
  }
  
  
  # # save data info
  # fout <- file.path('..', 'Processes', 'Data_cleaned_info_all.csv')
  # df0_info %>% fwrite(fout) 
  # 
  # # save data
  # fout <- file.path('..', 'Processes', 'Data_cleaned_all.csv')
  # df0 %>% fwrite(fout) 
  
  
  # find df0 columns of NA or constants (cause no statistical outputs for constants)
  col_NAorConst <- df0 %>% select_if(~all(is.na(.) | length(unique(na.omit(.))) == 1)) %>% colnames()
  
  # remove these columns of NA or constants from cleaned data
  df0 <- df0[, !colnames(df0) %in% col_NAorConst]
  # also remove these columns from the names of dependent variables
  Y <- Y[!Y %in% col_NAorConst]
  # also remove them from data information
  df0_info <- df0_info[!df0_info$VarName %in% col_NAorConst,]
  
  # load the statistical models
  mdls <- fn_models %>% fread() %>% as.data.frame()
  
  # the column of mdls$Filter MUST be character
  mdls$Filter <- mdls$Filter %>% as.character()
  
  # remove the duplicated models based on the columns of Model & Filter
  mdls <- mdls[!duplicated(mdls[,c("Model","Filter")]),]
  
  ## run each model across dependent variables 
  for (j in 1:length(mdls$Model)){
    # current model name
    mtxt <- mdls$Model[j]
    
    # current filter
    mfil <- mdls$Filter[j] %>% ifelse(is.na(.), "", .)
    if(mfil!=""){
      # filter subjects according to the text of filter: for PostHoc Test
      mfil <- gsub("filter(", "filter(df0, ", mfil, fixed=T)
      df <- eval(parse(text = mfil))
    }else{
      # no filter
      df <- df0
    }
    
    
    # X, vector of the names of independent variables, e.g. c("Group","Age")
    idx <- str_detect(mtxt, colnames(df0))
    X <- colnames(df0)[idx] # get the independent variable names (X) that appear in the text of model
    
    # modify the text of model according to model type
    if(grepl("chisq.test", mtxt)){
      # for chisq tests
      mtxt <-mtxt %>% gsub("table[(]", "table(.$", .) %>% gsub(",[[:space:]]", ", .$", .)
    }else {
      # for lm, lmer, t.test
      # here, df1 is to avoid confusion with df0 & df
      mtxt <- mtxt %>% gsub("[)]$", ", data=df1)",.)
    }
    
    
    ## parallel processing across all dependent variables
    time_para <- system.time({
      # print beginning information
      print(paste0("Running statistical model: ",mtxt))
      print(paste0("Filter subjects based on: ",mfil))
      print(paste0("N of depedent variables = ",length(Y)))
                   
      # function to combine outputs
      comb <- function(x, ...) {
        lapply(seq_along(x),
               function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
      }
      
      # setup parallel backend to use many processors
      cores <- detectCores()
      cl <- makeCluster(cores[1])
      registerDoParallel(cl)
      print(paste0("Parallel processing with N of cores = ",cores[1]))
   
      
      # Run parallel processing
     sta_out <- foreach(i = 1:length(Y), 
              .packages=c('dplyr','lme4', 'lmerTest', 'broom', 'broom.mixed'),
              .export=c("X","Y","df","mtxt"),
              .combine='comb', .multicombine=T, .init=list(list(), list())) %dopar% {
        
        # name of a dependent variable
        Yname <- Y[i]
        # dataframe of all independent variables
        df1 <- df[, X]
        # add the dependent variable into the dataframe
        df1$y <- df[, Yname]
        
        # run the statistical model
        mdl <- eval(parse(text = mtxt))
        # get tidied statistical outputs (statistics & p values)
        tidied <- mdl %>% tidy(effects = "fixed") %>% mutate(Yname=Yname)
        # get tidied model estimations
        glanced <- mdl %>% glance() %>% mutate(Yname=Yname)
        
        # return for each of Y
        list(tidied, glanced)
      }
      
      #stop cluster
      stopCluster(cl)
      
      # print ending information
      print("Completed! Results of Statistical Models. Please double check the outputs!")
      
    })
    print(time_para)
    
    # list to dataframe of outputs
    df_tidied  <- sta_out[[1]] %>% do.call(rbind, .) # statistical outputs (coef., t-value, p-value, etc.)
    df_glanced <- sta_out[[2]] %>% do.call(rbind, .) # model estimations
    
    ## Save outputs
    ## results (2 tables: statistical outputs & model estimations)
    # make the directory of results if it does not exist
    if (!dir.exists(fdir_o)) {dir.create(fdir_o, recursive=TRUE)}
    # replace illegal characters from file name (based on statistical model) with "_"
    fout1 <- mdls$Model[j] %>% gsub("\\|", "_", .) %>% gsub("\\*", "x", .) %>% gsub("\\:", "x=", .)
    # fout1 <- mtxt %>% gsub("[^[:alnum:]._-]", "_", .) # this function changes too much!
    # add filter for Post Hoc tests
    mfil <- mdls$Filter[j] %>% ifelse(is.na(.), "", .)
    if(mfil != ""){
      # if there IS a filter
      fout1 <- paste0(fout1, ", PostHoc, ",mfil)
    }
    
    
    # full path & name of the statistics output file
    fout <- file.path(fdir_o, paste0('Statistics_', fout1, ".csv"))
    # write statistical outputs
    df_tidied %>% fwrite(fout)
    # full path & name of the models estimation output file
    fout <- file.path(fdir_o, paste0('Models_', fout1, ".csv"))
    # write model estimation outputs
    df_glanced %>% fwrite(fout)
    
    
  }
  
  
}
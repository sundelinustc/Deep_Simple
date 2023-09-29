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


# Input
# --- fn_cleaned, filename of cleaned data (.csv), row=subject, column=feature
# --- fn_info,    filename of the info of cleaned data (.csv), row=feature, column=attributes(domain,atlas, etc.)
# --- Y,          vector of the names of dependent variables, e.g. c("roi_1","roi_2")
# --- fn_mtxt,    filename of the .csv file of statistical models (row=model), e.g. "lm(y ~ Group + Age)" or "lmer(y ~ Group + Age + (1|SITE))"
# --- fdir, the directory of the final outputs (statistical outputs + model estimations)
DS_stat_para <- function(fn_cleaned, fn_info=NULL, Y, fn_mtxt, fdir) {
  
  # load cleaned data
  df0      <- fn_cleaned %>% fread() %>% as.data.frame()
  # find df0 columns (also df0_info rows) of NA or constants (cause no statistical outputs for constants)
  col_NAorConst <- df0 %>% select_if(~all(is.na(.) | length(unique(na.omit(.))) == 1)) %>% 
    colnames()
  # remove these columns of NA or constants from cleaned data
  df0      <- df0[, !colnames(df0) %in% col_NAorConst]
  
  # # names of columns as dependent variables (Y)
  # Y <- df0 %>% select(starts_with("DFC"), starts_with("SFC")) %>% colnames()
  # also remove these columns from the dependent variables
  Y <- Y[!Y %in% col_NAorConst]
  
  # load fn_info if it exists
  if (!is.null(fn_info)){
    # load cleaned data info
    df0_info  <- fn_info %>% fread() %>% as.data.frame()
    # remove the variables just of NA or constants
    df0_info <- df0_info[!df0_info$VarName %in% col_NAorConst,]
    # set specific columns as factor
    # if there is no column called "VarClass" in df0_info, make a coulmn of VarClass
    if (!"VarClass" %in% colnames(df0_info)) {
      df0_info$VarClass <- apply(df0, 2, function(x)
        ifelse(length(unique(x)) > 5,
               'numeric',
               'Categorical')) # categorical variables if No. of unique values > 5
    }else{
      # if there is no fn_info, we have to make some related info by ourselves
      
    }
    
    # set categorical variables following VarClass
    name_cat <- df0_info %>% filter(VarClass == 'Categorical') %>% .$VarName
    df0[name_cat] <- lapply(df0[name_cat], factor)
    
  }
  
  # load the statistical models
  mdls <- fn_mtxt %>% fread() %>% as.data.frame() %>% .$V1 %>% as.character()
  
  ## run each model across dependent variables 
  for (mtxt in mdls){
    # X, vector of the names of independent variables, e.g. c("Group","Age")
    idx <- str_detect(mtxt, colnames(df0))
    X <- colnames(df0)[idx] # get the independent variable names (X) that appear in the text of model
    
    # modify the text of model according to model type
    if(grepl("chisq.test", mtxt)){
      # for chisq tests
      mtxt <-mtxt %>% gsub("table[(]", "table(.$", .) %>% gsub(",[[:space:]]", ", .$", .)
    }else {
      # for lm, lmer, t.test
      mtxt <- mtxt %>% gsub("[)]$", ", data=df)",.)
    }
    
    ## parallel processing across all dependent variables
    system.time({
      # print beginning information
      print("Running statistical model shown below. Parallel processing across depedent variables. Please be patient...")
      print(mtxt)
      
      # function to combine outputs
      comb <- function(x, ...) {
        lapply(seq_along(x),
               function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
      }
      
      # setup parallel backend to use many processors
      cores <- detectCores()
      cl <- makeCluster(cores[1])
      registerDoParallel(cl)
      
      # Run parallel processing
     sta_out <- foreach(i = 1:length(Y), .packages=c('dplyr','lme4', 'lmerTest', 'broom', 'broom.mixed'),
              .combine='comb', .multicombine=T, .init=list(list(), list())) %dopar% {
        
        # name of a dependent variable
        Yname <- Y[i]
        # dataframe of all independent variables
        df <- df0[, X]
        # add the dependent variable into the dataframe
        df$y <- df0[, Yname]
        
        # run the statistical model
        mdl <- eval(parse(text=mtxt))
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
    
    # list to dataframe
    df_tidied  <- sta_out[[1]] %>% do.call(rbind, .)
    df_glanced <- sta_out[[2]] %>% do.call(rbind, .)
  }
  
  
}
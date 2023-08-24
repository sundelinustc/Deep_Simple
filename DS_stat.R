### General statistical analysis function (lm/lmer, t.test, chisq.test)

## Load packages
if (!require("pacman")) install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics,
  
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
  broom.mixed  # broom for linear models with mixed effects
  # janitor,      # adding totals and percents to tables
  # flextable,    # converting tables to HTML
  # 
  # sjPlot,       # linear regression plots
  # ggpubr       # easy plots following publication styles
)



# Input
# --- df0, dataframe of cleaned data: rows=subjects, columns=features
# --- df0_info, dataframe of the info of cleaned data: class, domains
# --- Y, vector of the names of dependent variables, e.g. c("roi_1","roi_2")
# --- mtxt, model, e.g. "lm(y ~ Group + Age)" or "lmer(y ~ Group + Age + (1|SITE))"
# --- fdir, the directory of the final outputs (statistical outputs + model estimations)
# --- suff, suffix to add more info, e.g. '_posthoc_PTSD_only'
DS_stat <- function(df0, df0_info=NULL, Y, mtxt, fdir, suff='') {
  # remove df0 columns (df0_info rows) of NA or constants (cause no statistical outputs for constants)
  col_NAorConst <- df0 %>% select_if(~all(is.na(.) | length(unique(na.omit(.))) == 1)) %>% 
    colnames()
  df0      <- df0[, !colnames(df0) %in% col_NAorConst]
  Yname <- Yname[!Yname %in% col_NAorConst]
  
  # if df0_info exists
  if (!is.null(df0_info)){
    # remove the variables just of NA or constants
    df0_info <- df0_info[!df0_info$VarName %in% col_NAorConst,]
    # set specific columns as factor
    # if there is no column called "VarClass" in df0_info, make a coulmn of VarClass
    if (!"VarClass" %in% colnames(df0_info)) {
      df0_info$VarClass <- apply(df0, 2, function(x)
        ifelse(length(unique(x)) > 5,
               'numeric',
               'Categorical')) # categorical variables if No. of unique values > 5
    }
    
    # set categorical variables following VarClass
    name_cat <- df0_info %>% filter(VarClass == 'Categorical') %>% .$VarName
    df0[name_cat] <- lapply(df0[name_cat], factor)
    
  }
  
  
  # X, vector of the names of independent variables, e.g. c("Group","Age")
  idx <- str_detect(mtxt, colnames(df0))
  X <- colnames(df0)[idx] # get the independent variable names (X) that appear in the text of model
  
  # wide-to-long transformation
  # dependent variables to 2 columns: yname & y
  df <- df0 %>% 
    select(all_of(c(X, Yname))) %>% 
    pivot_longer(-c(1:length(X)), names_to = "yname", values_to = "y")
  
  # modify the text of model according to model type
  if(grepl("chisq.test", mtxt)){
    # for chisq tests
    mtxt <-mtxt %>% gsub("table[(]", "table(.$", .) %>% gsub(",[[:space:]]", ", .$", .)
  }else {
    # for lm, lmer, t.test
    mtxt <- mtxt %>% gsub("[)]$", ", data=.)",.)
  }
  
  # broom style
  model01 <-
    df %>%
    nest(data = c(-yname)) %>%
    mutate(
      fit = map(data, ~ eval(parse(text=mtxt)))
      ,      tidied = map(fit, tidy, effects = "fixed")
      ,      glanced = map(fit, glance)
      # ,augmented = map(fit, augment)
    )
  
  # statistics including p-values
  rm_col <- c("data","fit","glanced","augmented","effect") # colnames to be removed
  tidied <- model01 %>%  unnest(tidied) %>% select(setdiff(names(.),rm_col))# %>% filter(!term == "(Intercept)")
  
  # model estimation
  rm_col <- c("data","fit","tidied","augmented") # colnames to be removed
  glanced <- model01 %>% unnest(glanced) %>% select(setdiff(names(.),rm_col))
  
  ## Save outputs
  ## results (2 tables: statistical outputs & model estimations)
  # make the directory of results if it does not exist
  if (!dir.exists(fdir)) {dir.create(fdir, recursive=TRUE)}
  # replace illegal characters from file name (based on statistical model) with "_"
  fout1 <- mtxt %>% gsub("\\|", "_", .) %>% gsub("\\*", "x", .) %>% gsub("\\:", "x=", .)
  # fout1 <- mtxt %>% gsub("[^[:alnum:]._-]", "_", .) # this function changes too much!
  
  # full path & name of the statistics output file
  fout <- file.path(fdir, paste0('Statistics_', fout1, suff, ".csv"))
  # write statistical outputs
  tidied %>% fwrite(fout)
  # full path & name of the models estimation output file
  fout <- file.path(fdir, paste0('Models_', fout1, suff, ".csv"))
  # write model estimation outputs
  glanced %>% fwrite(fout)
  
  
  # return a list of 2 tables
  return(list(tidied, glanced))
}
## Visualization of the statistical outputs
# -- an overall picture based on atlas
# -- tables of statistical results sorted by T, p, or some other values
# -- bar or line plots for the most representative results (e.g., most significant, or the largest effect size)

## Load packages
if (!require("pacman")) install.packages("pacman") # make sure that pacman is installed

pacman::p_load(
  tidyverse,    # data management & ggplot2
  data.table,   # quick load and write files
  openxlsx,     # read & save .xlsx files
  
  ggseg,        # atlas-based brain maps, Note: 1st time install ggseg & ggsegExtra need the instructions on their github pages
  ggsegExtra,   # more atlases for ggseg
  ggpubr,        # plot multiple panels

  # car,          # for leveneTest() & outlierTest()
  # rcompanion,   # for post hoc using chisq.test()
  # rstatix,      # statistics
  # corrr,        # correlation analysis for numeric variables
  lme4,         # linear regression models
  lmerTest,     # p-value that are not included in lme4
  sjPlot       # plot for lm() or lmer() models
  
  # gtsummary,    # summary statistics and tests
  # table1,       # to make Table1
  # janitor,      # adding totals and percents to tables
  # flextable,    # converting tables to HTML
)

cat("\nCurrent working directory = \n", getwd())

# function to expand a row, i.e., 1 row may be expanded to multiple rows
# Input
# --- Vstr, a vector of strings, could be a row of a dataframe, e.g., df_mdl
# Output
# --- a dataframe in which each row is the combination of elements in the input vector of strings
myfun_expand_row <- function(Vstr) {
  Vstr %>% lapply(function(x) unlist(str_split(x, ";"))) %>% 
    expand.grid(stringsAsFactors = F) %>% return()
}

# function to find the common section across a list of strings
# Input
# --- list_str, a list of strings
myfun_common <- function(list_str){
  # Split each string into words
  words_list <- strsplit(list_str, "_")
  
  # Find the intersection of all word lists
  common_words <- Reduce(intersect, words_list)
  
  # Combine the common words into a single string
  common_string <- paste(common_words, collapse = "_")
  
  return(common_string)
}


# function to make the tables of the top-n significant findings
# Input
# --- df_tidied, the dataframe of outputs from a statistical model
# --- VOI, variables of interest, e.g., "FC_0_100_Left-Amygdala"
# --- Ntop, number of top-n regions showing the most significant pos/neg findings
# --- fthr, p threshold, default=0.05
# Output
# --- a dataframe (table) of the top-n significant results
myfun_table <- function(df_tidied, VOI="FC_0_100_Left-Amygdala", Ntop=5, fthr=0.05){
  # index of the variables of interest
  idx <- VOI %>% sapply(function(x) grep(x, df_tidied$Yname,value=F)) %>% c()
  df <- df_tidied[idx,]
  
  # row index of the top-n largest & smallest results
  idx_largest <- order(df$statistic, decreasing=T)[1:Ntop]
  idx_smallest <- order(df$statistic, decreasing=F)[1:Ntop] %>% rev()
  
  # extract these top-n results
  df1 <- df[c(idx_largest, idx_smallest),] %>% filter(p_corr < fthr)
  
  # assign NA across columns if no results survived correction
  if (nrow(df1) == 0) {df1 <- df1 %>% sapply(function(x) x=NA) %>% t() %>% as.data.frame()}
  
  # return
  return(df1)
}

# function to make the general maps of results based on statistics (T-value) 
# that are thresholded by predefined threshold
# Input
# --- VOI, variables of interest, e.g., "FC_0_100_Left-Amygdala"
# --- df_merged, the dataframe of statistical outputs that are merged with Data_clneaned_info.csv
# --- ylim, the max absolute value of all statistics
# Output
# --- a merged plot
myfun_map <- function(VOI="FC_0_100_Left-Amygdala", df_merged, ylim){
  # index of the variables of interest
  idx <- VOI %>% sapply(function(x) grep(x, df_merged$Yname,value=F)) %>% c()
  df <- df_merged[idx,]
  
  # only the columns of statistic & label are needed for maps
  df <- df %>% mutate(label=atlas) %>% select(c("statistic","label"))
  
  # assign 0s to df$statistic if there is no numerical values
  if(sum(is.na(df$statistic)) == dim(df)[1]){df$statistic[df$label=="Left_Accumbens_Missing"] <- 0}
  # add a 0 to a region that would not be shown to avoid all NA
  
  # Subcortical plots
  plt_sub  <-
    df %>% ggseg(atlas=aseg, mapping=aes(fill=statistic), color='white', position='dispersed', size=0.5) +
    scale_fill_distiller(type='div', palette='RdBu', direction=-1, limits=c(-ylim, ylim), name="T-value") +
    # theme(axis.title.x=element_blank(), axis.text.x=element_blank())
    theme_void() +
    # theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) + 
    # ylab(paste0("",str_extract(VOI, "(?<=_)[^_]+$")))
    labs(title=VOI)
  
  # Cortical plots
  plt_cort <-
    df %>% ggplot() + 
    geom_brain(atlas=schaefer17_400, position = position_brain(hemi ~ side), aes(fill=statistic)) +
    scale_fill_distiller(type='div', palette='RdBu', direction=-1, limits=c(-ylim, ylim), name="T-value") +
    theme_void()
    # theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) + 
    # ylab(paste0("",str_extract(VOI, "(?<=_)[^_]+$")))
    # labs(title=VOI)
  
  # merged plots (cortical + subcortical)
  plt_merged<- ggarrange(plotlist=list(plt_sub, plt_cort), nrow=1, common.legend=T, legend='right')
  
  # return plot
  return(plt_merged)
}


# function to plot the effects of interest on the two variables with 
# the largest (most positive) & smallest (most negative) statistics
# Input
# --- VOI, variables of interest, e.g., "FC_0_100_Left-Amygdala"
# --- df_tidied, the dataframe of statistical outputs 
# --- df0, dataframe of cleaned data
# --- mtxt, text of the statistical model
# --- feffect, the effect of interest
# Output
# --- a merged plot
myfun_pltextreme <- function(VOI="FC_30_50_Left-Amygdala", 
                             df_tidied, 
                             df0=df_data,
                             mtxt="lmer(y ~ curr_ptsd_dx + Age + Sex + (1|SITE))", 
                             feffect="curr_ptsd_dx")
{
  
  # index of the variables of interest
  idx <- VOI %>% sapply(function(x) grep(x, df_tidied$Yname,value=F)) %>% c()
  df <- df_tidied[idx,]
  
  # find the two variables of the extreme statistics
  varname_max <- df$Yname[df$statistic == max(df$statistic)]
  varname_min <- df$Yname[df$statistic == min(df$statistic)]
  
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
  
  # dataframe of all independent variables
  df1 <- df0 %>% select(X)
  
  # subtitle name
  sub_name_max <- setdiff(strsplit(varname_max, "_")[[1]], strsplit(VOI, "_")[[1]]) %>% paste(collapse="_")
  sub_name_min <- setdiff(strsplit(varname_min, "_")[[1]], strsplit(VOI, "_")[[1]]) %>% paste(collapse="_")
  
  
  # Plot for variable of max
  # add the dependent variable into the dataframe
  df1$y <- df0 %>% select(all_of(varname_max)) %>% unlist()
  # run the statistical model & make a plot
  plt_max <- eval(parse(text = mtxt)) %>% plot_model("pred", terms= unlist(str_split(feffect,":")), 
    mdrt.values = "meansd", colors="Set2") + ylab("") + theme_blank() + 
    labs(title=VOI, subtitle="Max", caption=sub_name_max)
  # Plot for variable of min
  # add the dependent variable into the dataframe
  df1$y <- df0 %>% select(all_of(varname_min)) %>% unlist()
  # run the statistical model & make a plot
  plt_min <- eval(parse(text = mtxt)) %>% plot_model("pred", terms= unlist(str_split(feffect,":")), 
    mdrt.values = "meansd", colors="Set2") + ylab("") + theme_blank() + 
    labs(title=VOI, subtitle="Min", caption=sub_name_min)
  
  # merged plots
  plt_merged<- ggarrange(plotlist=list(plt_max, plt_min), nrow=1, common.legend=T, legend='right')
  
  # return
  return(plt_merged)
}



# function to get the dataframe of statistical outputs of interest
# Input
# --- Vstr, a vector of strings, could be a row of a dataframe, e.g., df_mdl
# --- fdir_in, path to the folder containing all statistical outputs
# --- Ntop, number of top-n regions showing the most significant pos/neg findings
# --- fthr, p threshold, default=0.05
# --- df_data, the dataframe of Data_cleaned.csv which has the cleaned data
# --- df_info, the dataframe of Data_cleaned_info.csv which has the atlas labels
# --- fdir_o, the directory for outputs
# Output
# --- a dataframe ready for plots & tables
myfun_df <- function(Vstr, fdir_in, Ntop=5, fthr=0.05, df_data, df_info, fdir_o){
  # elements of Vstr
  # 1=Moddel, 2=Filter, 3=Effects, 4=Terms, 5=PCorrMethod, 6=Pthr, 7=Interest, 8=NoInterest
  
  # for test purpose only
  # Vstr <- df_mdl[1,] %>% as.character()
  
  cat("\nVisulization (tables, maps, & plots) for")
  print(Vstr)
  
  # corresponding filename (some characters can't be used for filename and thus must be converted)
  f1 <- Vstr[1] %>% gsub("\\|", "_", .) %>% gsub("\\*", "x", .) %>% gsub("\\:", "x=", .)
  # add filter for Post Hoc analysis
  mfil <- Vstr[2] %>% ifelse(is.na(.), "", .)
  if(mfil != ""){f1 <- paste0(f1, ", PostHoc, ",mfil)}
  
  # fullname of the statistical outputs
  fname <- file.path(fdir_in, paste0('Statistics_', f1, ".csv"))
  if (!file.exists(fname)){
    # if the statistical outputs file does NOT exist
    warning(paste("Can't find file:", fname))
    return(NA)
  }else{
    # load statistical outputs
    df_tidied <- fname %>% fread()
    
    # keep the effects of interest
    df_tidied <- df_tidied %>% filter(term == Vstr[3])
    
    # remove the variables of NO interest
    if(Vstr[8] != ""){
      # find the corresponding index if there is at least one variable of NO interest
      idx <- Vstr[8] %>% str_split(";") %>% unlist() %>% sapply(function(x) grep(x, df_tidied$Yname,value=F)) %>% unlist() %>% c() # idx should be numeric if some Yname contain the strings of NO interest
      # remove the variables of NO interest if we have the corresponding (numeric) index
      if(length(idx)!=0){df_tidied <- df_tidied[-idx,]}
    }
    
    # keep the variables of interest
    idx <- Vstr[7] %>% str_split(";") %>% unlist() %>% sapply(function(x) grep(x, df_tidied$Yname,value=F)) %>% unlist() %>% c()
    if(length(idx)!=0){df_tidied <- df_tidied[idx,]}else{warning("!!! No Variables of Interest!!!")}
    
    # upper & lower limits of significant findings (non-significant results are NOT shown)
    ylim <- df_tidied$statistic %>% abs() %>% max()
    print(paste('Colorbar range:',round(-1*ylim,3), 'to' , round(ylim,3)))
    
    # sort outputs based on p-values
    df_tidied <-df_tidied[order(df_tidied$p.value),]
    
    # add adjusted p-values
    df_tidied <- df_tidied %>% add_column(p_corr=p.adjust(df_tidied$p.value, method=Vstr[5]), .before="Yname")
    
    
    ### Tables (top-n), overall maps, and plots (for the most significant findings)
    # path to the directory of output for the corresponding statistical model
    fdir_o1 <-  file.path(fdir_o, f1)
    
    # make the folder for the model output if it does not exist
    if (!dir.exists(fdir_o1)) {dir.create(fdir_o1, recursive=T)}
    
    # filename for the effect of interest
    fn0 <- Vstr[3] %>% gsub("\\|", "_", .) %>% gsub("\\*", "x", .) %>% 
      gsub("\\:", "x=", .)
    
    # unique values of variables of interest
    VOIs <- Vstr[7] %>% str_split(";") %>% unlist()
    
    
    ## (1) Make the tables of top-n significant results
    # folder name = model name + posthoc filter
    # excel filename = effect of interest
    # excel sheetname = Variable of interest
    
    # excel filename
    fn <- file.path(fdir_o1, paste0("Table_",fn0,'.xlsx'))
    
    # save the results
    for (VOI in VOIs){
      print(paste0("Making Excel sheet for ",VOI))
      # load an excel file if it does EXIST, or create a new one if it does NOT exist
      if(file.exists(fn)){
        wb <- loadWorkbook(fn)
      }else{
        wb <- createWorkbook()
      }
      # wb <- ifelse(file.exists(fn), loadWorkbook(fn), createWorkbook()) # errors reported, weird!
      
      # add a new sheet if it does NOT exist, or remove 
      if (VOI %in% names(wb)){
        warning(paste("Excel worksheet EXISTS & will be UPDATED: ",VOI))
        removeWorksheet(wb, sheet = VOI)
      }
       # add a new sheet 
      wb %>% addWorksheet(VOI)
      # write data
      wb %>% writeData(sheet=VOI, x=myfun_table(df_tidied, VOI, Ntop, fthr))
      # Export the file
      wb %>% saveWorkbook(fn, overwrite=T)
    }
    print(paste0("Excel files for top-",Ntop," results saved in"))
    print(fn)
    
    
    
    ## (2) Make the overall maps of T-values (thresholded by corrected p < p_threshold) 
    if(!"atlas" %in% colnames(df_info)){
      # if there is NO atlas information, skip the step of making maps
      warning("No atlas information! Can NOT make overall maps of statistics.")
    }else{
      # map figure name
      # get the common section across all variable names
      fn <- file.path(fdir_o1, paste0("Map_",fn0,'_', myfun_common(VOIs) ,'.png'))
      
      # merge results & atlas
      df_atlas <- df_info %>% select(c('VarName', 'atlas'))
      df_merged <- df_tidied %>% merge(df_atlas, by.x = 'Yname', by.y = 'VarName')
      
      # set non-significant variables as NA (i.e., do NOT show them)
      df_merged <- df_merged %>% mutate(statistic = ifelse(p_corr < fthr, statistic, NA))
      
      # make the list of maps
      list_maps <- VOIs %>% lapply(myfun_map, df_merged=df_merged, ylim=ylim)
      map_merged <- ggarrange(plotlist=list_maps, ncol=1, common.legend=T, legend='right')
      
      # save images
      png(fn, width = 6, height = 1.5*length(list_maps), units = "in", res = 600)
      print(map_merged) 
      dev.off()
      
      print("Overall maps of statistic saved in")
      print(fn)
    }
    
    
    ## (3) Plots of the largest (most positive) & smallest (most negative) statistics

    # figure name
    # get the common section across all variable names
    fn <- file.path(fdir_o1, paste0("Plot_",fn0,'_', myfun_common(VOIs) ,'.png'))

    # make the list of plots
    list_plts <- VOIs %>% lapply(myfun_pltextreme, df_tidied=df_tidied, df0=df_data, mtxt=Vstr[1], feffect=Vstr[4])
    plt_merged <- ggarrange(plotlist=list_plts, ncol=1, common.legend=T, legend='right')

    # save images
    png(fn, width = 6, height = 2.5*length(list_plts), units = "in", res = 600)
    print(plt_merged)
    dev.off()

    print("Plots of Max & Min Statistics saved in")
    print(fn)
    
    
    # return the dataframe of interest
    return(df_tidied)
    
  }
  
  
}


# Input
# --- Var_In,    the strings to represent the variables to be included, e.g., "FC_0_100_Left-Amygdala", 
#.       and importantly, also serve as a factor, e.g., different seeds of FC
# --- Var-NotIn, the strings to represent the variables to be excluded, e.g., "Buckner2011"
# --- fmdl, the full filename of the statistical models, fiter (for Post Hoc test), and effects of interest
# --- fdir_in, the directory to the statistical outputs & model estimations
# --- fdata, the fullname of the Data_cleaned.csv which has all cleaned data
# --- finfo, the fullname of the Data_cleaned_info.csv which has the atlas labels
# --- fcorr, the methods to correct for multiple comparisons, default is "fdr" 
#.       (can also be "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", and "none")
# --- fthr, threshold of corrected p values, default = 0.05
# --- Ntop, number of the most significant top-n results, sorted by p-values
# --- fdir_o, the directory for outputs
# Output
# --- atlas-based full picture of statistical outputs (T-values, 
#        only significant findings are shown, i.e., corrected p-value < 0.05)
# --- tables of statistical outputs, sorted by p-values
# --- plots of the most representative findings (most significant)
DS_stat_vis <- function(
    Var_In = c("FC_0_100_Left-Amygdala", "FC_0_100_Right-Amygdala", "FC_0_100_Left-Hippocampus", "FC_0_100_Right-Hippocampus"),
    Var_NotIn = "Buckner2011", 
    fmdl = file.path("..","Processes","Statistical_Models.csv"), 
    fdir_in = file.path("..","Results","DS","stats"), 
    fdata = file.path("..","Processes","Data_cleaned.csv"),
    finfo = file.path("..","Processes","Data_cleaned_info.csv"),
    fcorr = "fdr", 
    fthr= 0.05,
    Ntop = 5, 
    fdir_o = file.path("..","Results","DS","vis")
){
  ## Load statistical models & add more details (columns)
  df_mdl <- fmdl %>% fread() %>% mutate(PCorrMethod = fcorr, Pthr = fthr)
  
  # ## expand the dataframe of statistical models
  # # mainly for the filter & effects of interest
  # df_mdl <- df_mdl %>% apply(1, myfun_expand_row) %>% do.call(rbind, .)
  
  ## add columns of variables of interest & no interest
  # they should NOT be expanded at this step
  df_mdl$Interest <- Var_In %>% paste(collapse = ";")
  df_mdl$NoInterest <- Var_NotIn %>% paste(collapse = ";")
  
  # load cleaned data
  df_data <- fdata %>% fread() %>% as.data.frame()
  df_data[df_data==""] <- NA # replace "" with NA, especially for categorical variables using characters as levels
  
  # # re-organize data structure to facilitate the analyses on data from INFILE (individual data files)
  # df_in_data     <- df_data %>% select(starts_with("INFILE_"))  # dataframe of individual data filenames
  # df_not_in_data <- df_data %>% select(!starts_with("INFILE_")) # dataframe of data not stored in individual data files
  # colname_not_in <- df_not_in_data %>% colnames() # get the name of the data NOT from individual files to re-organize Data_cleaned_info.csv
  # 
  # load data info
  df_info <- finfo %>% fread() %>% as.data.frame()
  
  # # re-organize data_info structure 
  # colname_in <- setdiff(df_info$VarName, colname_not_in)
  # df_info <- df_info[match(c(colname_not_in, colname_in), df_info$VarName), ]
  # 
  ## get the list of dataframe for visulization across all models, and also
  # --- generate the excel files of the top-n significant results (pos + neg)
  # --- generate the figures showing the overall maps of significanct results
  # --- generate the figures of plots of the most significant results (pos + neg)
  list_all <- df_mdl %>% apply(1, myfun_df, fdir_in=fdir_in, Ntop=Ntop, fthr=fthr, df_data=df_data, df_info=df_info, fdir_o=fdir_o)
 
  # return
  cat("\n All tables, maps, & plots were completed and saved in the vis folder!!!\nPlease check them carefully!!!")
  return(list_all)


}

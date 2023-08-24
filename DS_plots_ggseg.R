### Plots of brain atlas

## Load packages
if (!require("pacman")) {
  install.packages("pacman") # make sure that pacman is installed
}

pacman::p_load(ggseg,        # atlas-based brain maps, both cortical & subcortical (e.g. aseg)
               ggsegSchaefer,# Schaefer's atlas, e.g. schaefer17_400
               
               tidyverse,    # data manipulaions
               data.table,   # quickly read & write data files
               
               ggpubr        # plot multiple panels
)
               

# Input
# --- fname, filename of the statistical outputs, e.g., 'Statistics_lmer(y ~ Sex + Age + (1_SITE), data=.)_posthoc_PTSD_only'
# --- fterm, the effect of interest, e.g. 'curr_ptsd_dxPTSD'
# --- fdf0_info, filename of the info of cleaned data: class, domains
# --- fmask, filename of the statistical outputs as an inclusive mask, e.g., 'Statistics_lmer(y ~ curr_ptsd_dx x Sex + Age + (1_SITE), data=.)'
# --- fmask_term, the effect of the mask, e.g. 'curr_ptsd_dxPTSD:SexFemale'
# --- fdir, the directory of the final outputs (Figures)
# Output
# --- brain maps of T-values (statistics)
# --- list of top-5 target regions per seed showing significance
DS_plots_ggseg <-
  function(fname,
           fterm = 'curr_ptsd_dxPTSD',
           fdf0_info,
           fmask = NULL,
           fmask_term = NULL,
           fdir = NULL) {
    
    # # set the size of margins
    # par(mar=rep(5, 4)) 
    
    if(!is.null(fdir)){
      # make the directory of results if it does not exist
      if (!dir.exists(fdir)) {dir.create(fdir, recursive=TRUE)}
    }
    
    
    
    # load the table of results
    df_stats <- 
      fname %>% fread()
    
    # load data info
    df0_info <-
      fdf0_info %>% fread() %>% as.data.frame()
    
    # domain1 unique variables
    Vdom1 <-
      df0_info$VarDomain1 %>% unique() # uniuqe domain1 variables
    idx   <-
      !Vdom1 %in% c('Demographic and clinical', 'GROUP', '') # not these values
    Vdom1 <- Vdom1[idx]
    
    # domain2 unique variables
    Vdom2 <-
      df0_info$VarDomain2 %>% unique() # uniuqe domain1 variables
    idx   <- !Vdom2 %in% c('') # not these values
    Vdom2 <- Vdom2[idx]
    
    # load the data of the make file
    if (!is.null(fmask)){
      # load it if the mask file exists
      df_stats_mask <- fmask %>% fread()
      # effect of interest (significant interaction)
      df_mask <- df_stats_mask[term == fmask_term,]
      for (Vname in Vdom1) {
        # p adjustment for each unique value in domain1, e.g., "DFC" and "SFC"
        df_mask[startsWith(yname, Vname), 'adj_p'] <-
          df_mask[startsWith(yname, Vname), 'p.value'] %>%
          unlist() %>% p.adjust('fdr')
      }
      # vector of mask (TRUE=in the mask, FALSE=out of the mask)
      df_mask <- df_mask %>% mutate(inclusive = ifelse(adj_p < 0.05, TRUE, FALSE))
      df_mask <- df_mask %>% select(yname, inclusive)
    }
    
    
    # select the effect of interest & add a column of adj_p (adjust p for DFC and SFC, respectively)
    df <- df_stats[term == fterm,]
    # apply mask if the mask exists
    if(exists("df_mask")){
      # only keep the features that are significant in the inclusive mask
      df<- df %>% merge(df_mask, by="yname") %>% filter(inclusive)
    }
    # correction for multiple comparisons per domain1 variable, e.g., "DFC" and "SFC"
    for (Vname in Vdom1) {
      df[startsWith(yname, Vname), 'adj_p'] <-
        df[startsWith(yname, Vname), 'p.value'] %>%
        unlist() %>% p.adjust('fdr')
    }
    
    
    # merge results & atlas
    df_atlas <- df0_info %>%
      select(c('VarName', 'Atlas_Label', 'Atlas_R', 'Atlas_A', 'Atlas_S'))
    df <-
      df %>% merge(df_atlas, by.x = 'yname', by.y = 'VarName') %>%
      rename_at('Atlas_Label', ~ 'label')
    
    
    # upper & lower limits of significant findings (non-significant results are NOT shown)
    ylim <- df %>% filter(adj_p < 0.05) %>%
      select(statistic) %>% abs() %>% max()
    
    print(paste('Colorbar range:',round(-1*ylim,3), 'to' , round(ylim,3)))
    
    # set non-significant variables as NA (i.e., do NOT show them)
    df <- df %>% mutate(statistic = ifelse(adj_p < 0.5, statistic, NA))
    
    # make plots for each domain2 unique variable
    plt_cortex <- NULL
    plt_sub    <- NULL
    
    # dataframe of top a few regions
    df_top_desc <- NULL
    df_top_asce <- NULL
    for (i in seq(length(Vdom2))) {
      # rbind of the top a few regions
      df_top_desc <- df %>% filter(startsWith(yname, Vdom2[[i]])) %>% 
        arrange(desc(statistic)) %>% 
        .[1:5,] %>% mutate(seed=Vdom2[[i]]) %>% 
        select(c("seed","label", "statistic", "Atlas_R", "Atlas_A", "Atlas_S")) %>% 
        rbind(df_top_desc, .)
      df_top_asce <- df %>% filter(startsWith(yname, Vdom2[[i]])) %>% 
        arrange(statistic) %>% 
        .[1:5,] %>% mutate(seed=Vdom2[[i]]) %>% 
        select(c("seed","label", "statistic", "Atlas_R", "Atlas_A", "Atlas_S")) %>% 
        rbind(df_top_asce, .)

      # full path & name of the statistics output file
      # descending order
      fname1 <- fname %>% basename() %>% 
        tools::file_path_sans_ext() %>% paste0('_desc_top5.csv')
      fout <- file.path(fdir, fname1)
      # write statistical outputs
      print(df_top_desc)
      print(fout)
      # df_top_desc %>% fwrite(fout)
      # ascending order
      fname1 <- fname %>% basename() %>% 
        tools::file_path_sans_ext() %>% paste0('_asce_top5.csv')
      fout <- file.path(fdir, fname1)
      # write statistical outputs
      # df_top_asce %>% fwrite(fout)
      
      print(i)
      # Cortical plots
      plt_cortex[[i]] <-
        df %>% filter(grepl(Vdom2[[i]], yname)) %>%
        ggseg(
          atlas = schaefer17_400,
          mapping = aes(fill = statistic),
          position = 'stacked'
        ) +
        scale_fill_distiller(
          type = 'div',
          palette = 'RdBu',
          direction = -1,
          limits = c(-1 * ylim, ylim),
          name = "T-value"
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none"
        ) + 
        ylab(paste0("",str_extract(Vdom2[[i]], "(?<=_)[^_]+$")))
      
      # Subcortical plots
      plt_sub[[i]]   <-
        df %>% filter(grepl(Vdom2[[i]], yname)) %>%
        ggseg(
          atlas = 'aseg',
          mapping = aes(fill = statistic),
          color = 'white',
          position = 'dispersed',
          size = 0.5
        ) +
        scale_fill_distiller(
          type = 'div',
          palette = 'RdBu',
          direction = -1,
          limits = c(-1 * ylim, ylim),
          name = "T-value"
        ) +
        theme(
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          legend.position = "none"
          )# + coord_fixed(1)
      
      # # cortical + subcortical plots
      # plt_map[[i]]   <- ggarrange(plt_cortex,
      #                             plt_sub,
      #                             # font.label=list(size=15, face="bold"),
      #                             # common.legend = T,
      #                             labels = Vdom2[[i]]
      #                             )
      # print(plt_map)
    }
    
    
    plt_cortex_DFC <- ggarrange(plt_cortex[[1]], plt_cortex[[2]], plt_cortex[[3]], plt_cortex[[4]], ncol=1
                                , common.legend=T, legend='none')
    plt_sub_DFC <- ggarrange(plt_sub[[1]], plt_sub[[2]], plt_sub[[3]], plt_sub[[4]], ncol=1
                             , common.legend=T, legend='none'
                             )
    plt_cortex_SFC <- ggarrange(plt_cortex[[5]], plt_cortex[[6]], plt_cortex[[7]], plt_cortex[[8]], ncol=1
                                , common.legend=T, legend='none')
    plt_sub_SFC <- ggarrange(plt_sub[[5]], plt_sub[[6]], plt_sub[[7]], plt_sub[[8]], ncol=1
                             , common.legend=T, legend='none'
                             )

    # merge all plots

    plt_all <- ggarrange(plt_cortex_DFC, plt_sub_DFC, plt_cortex_SFC, plt_sub_SFC,
                         nrow=1, 
                         # labels = "AUTO",
                         common.legend=T, legend='bottom')
    
    # plt_all <- ggarrange(
    #   plt_cortex[[1]], plt_cortex[[2]], plt_cortex[[3]], plt_cortex[[4]],
    #   plt_sub[[1]], plt_sub[[2]], plt_sub[[3]], plt_sub[[4]], 
    #   plt_cortex[[5]], plt_cortex[[6]], plt_cortex[[7]], plt_cortex[[8]],
    #   plt_sub[[5]], plt_sub[[6]], plt_sub[[7]], plt_sub[[8]], 
    #                      # nrow=4, 
    #   labels = NULL
    #                      # common.legend=T 
    #   # legend='bottom'
    #   )
    
    
    
    return(plt_all)
    
    
  }
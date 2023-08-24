### functions to make table 1

## Load packages
if (!require("pacman")) {
  install.packages("pacman") # make sure that pacman is installed
}

pacman::p_load(tidyverse,    # data manipulaions
               flextable,    # for making tables (italic etc.)
               table1        # to make Table1
)




## function to test (an F test) homogeneity of variances between two groups
ds_EqualVar2 <- function(y, g){
  ifelse(var.test(y ~ g)$p.value > .05, TRUE, FALSE) %>% return()  
  # non-sig = equal variance
}

## function for Two sample t test if equal variances, otherwise Welch' t test
ds_ConTest2 <- function(y, g) {
  flag <- ds_EqualVar2(y, g)
  # Two sample t test if equal variances, otherwise Welch' t test
  mdl <- t.test(y ~ g, var.equal = flag, na.action = na.omit)
  c(paste0(
    "t(",
    round(mdl$parameter, 1),
    # df
    ") = ",
    round(-1 * mdl$statistic, 3),
    # t value, grp2-grp1, so -1*
    ", p = ",
    round(mdl$p.value, 3)           # t(df) = t-val, p = p-val
  )) %>% return()
}

# function to exchange 2 the locations of 2 strings in a sentence
# for post hoc tests outputs in the below "ds_CatTest()"
ds_exchange_strings <- function(sentence) {
  # split the sentence by the split symbol ':'
  split_sentence <- strsplit(sentence, ":")
  
  # extract the two strings before and after the split symbol
  string1 <- trimws(split_sentence[[1]][1])
  string2 <- trimws(split_sentence[[1]][2])
  
  # combine the two strings in reverse order with a hyphen in between
  paste0(string2, ':', string1) %>% return()
}


## function for categorical variables comparisons between two groups
ds_CatTest <- function(y, g) {
  # DO NOT worry about missing values because table(y,g) excludes NA automatically
  if (any(table(y, g) < 5)) {
    # Fisher’s exact test (when N of any cell < 5)
    mdl <- fisher.test(table(y, g))
    grp_sig <- mdl$p.value # p val
    ptxt <- paste0("Fisher’s exact test, p = ",
                   round(grp_sig, 3))    # Fisher’s exact test, p = p-val)
    
    # Post hoc tests
    if (grp_sig < .05) {
      mdl_post <-
        pairwiseNominalIndependence(table(g, y), method = 'bonferroni')
      for (i in seq(dim(mdl_post)[[1]])) {
        rname <- mdl_post[[i, 'Comparison']] %>% ds_exchange_strings()
        ptxt <- ptxt %>% paste0(.,
                                '\n',
                                rname,
                                ", p = ",
                                round(mdl_post[i, 'p.adj.Fisher'], 3))
      }
    }
  }
  else {
    # chi-squared test of independence (when N of any cell >= 5)
    mdl <- chisq.test(table(y, g))
    grp_sig <- mdl$p.value # p val
    ptxt <- paste0(
      "χ2(",
      round(mdl$parameter, 1),
      # df
      ", ",
      sum(table(y, g)),
      # N, sample size
      ") = ",
      round(mdl$statistic, 3),
      # χ2 value
      ", p = ",
      round(mdl$p.value, 3)    # χ2(df, N) = chi2-val, p = p-val
    )
    
    # Post hoc tests
    if (grp_sig < .05) {
      mdl_post <-
        pairwiseNominalIndependence(table(g, y), method = 'bonferroni')
      for (i in seq(dim(mdl_post)[[1]])) {
        rname <- mdl_post[[i, 'Comparison']] %>% ds_exchange_strings()
        ptxt <- ptxt %>% paste0(.,
                                '\n',
                                rname,
                                ", p = ",
                                round(mdl_post[i, 'p.adj.Chisq'], 3))
      }
    }
    
  }
  return(ptxt)
}


## Test each group for normality using the Shapiro-Wilk Normality Tests
ds_NormVar3 <- function(y, g){
  df1 <- data.frame(y = y, g = g) %>% 
    group_by(g) %>% 
    summarise(pval = shapiro.test(y)$p.value)
  return(any(df1$pval < 0.05)) # if any group is not normally distributed (p<.05)
}

## function for 3 samples comparisons (any group not normally distributed): 
#  Kruskal test 
#  Dunn test for Post Hoc analyses 
ds_ConTest3_NoNorm <- function(y, g){
  # kruskal test if any group is not normally distributed
  mdl <- kruskal.test(y ~ g)
  grp_sig <- mdl$p.value # significance of group effect
  ptxt <-
    c(paste0(
      "Kruskal Test, H(",
      round(mdl$parameter, 1),
      # df
      ") = ",
      round(mdl$statistic, 3),
      # statistics value
      ", p = ",
      round(mdl$p.value, 3)   # H(df) = Kruskal-val, p = p-val
    ))
  
  # Dunn test for multiple comparisons (adj_p values) only for sig kruskal.test
  if (grp_sig < 0.05) {
    mdl_post <- dunnTest(y ~ g, method = "bonferroni") %>% .$res
    for (i in seq(dim(mdl_post)[[1]])) {
      rname <- mdl_post[[i, 'Comparison']]
      ptxt <- ptxt %>% paste0(.,
                              '\n',
                              rname,
                              ", p = ",
                              round(mdl_post[i, 'P.adj'], 3))
    }
  }
  return(ptxt)
}

##  Levene's Test for Homogeneity of Variance (center = median)
ds_EqualVar3 <- function(y, g) {
  # non-sig = equal variance
  ifelse(leveneTest(y ~ g)[1, 'Pr(>F)'] > .05, TRUE, FALSE) %>% return()
}

## function for 3 samples comparisons (no violation of normal distribution): 
#  One-Way ANOVA if equal variances, otherwise Welch’s ANOVA
#  Tukey's test for Post Hoc analyses
ds_ConTest3_Norm <- function(y, g) {
  flag <- ds_EqualVar3(y, g) #  Levene's Test for Homogeneity of Variance
  mdl <- oneway.test(y ~ g, var.equal = flag, na.action = na.omit)
  grp_sig <- mdl$p.value # significance of group effect
  ptxt <-
    c(paste0(
      "F(",
      round(mdl$parameter[[1]], 2),
      # df1
      ", ",
      round(mdl$parameter[[2]], 2),
      # df2
      ") = ",
      round(mdl$statistic, 3),
      # F value
      ", p = ",
      round(mdl$p.value, 3)   # F(df1, df2) = F-val, p = p-val
    ))
  
  
  # Tukey's test for multiple comparisons (adj_p values) only for significant ANOVA
  if (grp_sig < 0.05) {
    mdl_post <-
      TukeyHSD(aov(y ~ g), conf.level = .95) %>% .$g %>% unlist()
    for (i in seq(dim(mdl_post)[[1]])) {
      rname <- rownames(mdl_post)[[i]]
      ptxt <- ptxt %>% paste0(.,
                              '\n',
                              rname,
                              ", p = ",
                              round(mdl_post[i, 4], 3))
    }
  }
  return(ptxt)
}

## function to get the texts of statistics (e.g. t/χ2, df, and p)
ptxt <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x) # dependent variable
  g <- factor(rep(1:length(x), 
                  times = sapply(x, length))) # grouping variable
  
  if (length(unique(g)) == 1) {
    # No group comparisons if there is only one group!
    ptxt <-
      'One Group Only!' # one group only, no comparison(s) between groups
    
  }
  else if (length(unique(g)) == 2) {
    # Two groups
    if (is.numeric(y)) {
      # Two sample t test if equal variances, otherwise Welch' t test
      ptxt <- ds_ConTest2(y, g)
      
    }
    else {
      # For categorical variables
      ptxt <- ds_CatTest(y, g)
    }
    
  }
  else {
    # Three or more groups
    
    if (is.numeric(y)) {
      # For numeric variables
      # Test each group for normality using the Shapiro-Wilk Normality Tests
      flag <- ds_NormVar3(y, g)
      
      if (flag) {
        # kruskal.test if any group violates normality
        ptxt <- ds_ConTest3_NoNorm(y, g)
      }
      else{
        # One-Way ANOVA if equal variances, otherwise Welch’s ANOVA
        ptxt <- ds_ConTest3_Norm(y, g)
      }
      
      
    }
    else {
      # For categorical variables
      ptxt <- ds_CatTest(y, g)
    }
    
  }
  
  return(ptxt)
}

### function to make Table 1
##  including both descriptive & statistical outputs
## Input:
## --- df, dataframe contains variables of interest (continuous or categorical)
## --- fmodel, a string of model for table1 function, e.g. "~ Age  + Sex + PTSDlifeDx + DepSev | Group", Group is grouping variable
## --- fnote, the footnote, default: "None"
DS_table1 <- function(df, fmodel, fnote = "None") {
  ## my render: how to display descriptive statistics
  # continuous variables
  my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits = 3),
         # c("Mean (SD)" = sprintf("%s &plusmn; %s", MEAN, SD))) #
         c("Mean (SD)" = sprintf("%s ± %s", MEAN, SD)))
  }
  # categorical variables
  my.render.cat <- function(x) {
    c('',sapply(stats.default(x), function(y)
      with(y,
           sprintf(
             "%d (%0.0f %%)", FREQ, PCT
           ))))
  }
  
  ## Table 1
  df %>% table1(
    eval(parse(text = fmodel)),
    data = .,
    overall = F,
    extra.col = list(`Statistics` = ptxt),
    render.continuous = my.render.cont,
    render.categorical = my.render.cat,
    footnote = fnote
  )
  
}
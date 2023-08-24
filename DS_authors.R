### A function to put authors' information into a Word document

## Load packages
if (!require("pacman")) {install.packages("pacman")} # make sure that pacman is installed

 pacman::p_load(
  tidyverse,    # data management
  rlang,        # for !! rlang::sym() function to cite character as column names in mutate()
  readxl,       # Read xls and xlsx files

  officer,      # add images, tables and text into Word documents from R
  officedown   # format MS Word documents produced by R Markdown documents
 )


# Input
# --- fin, the full path of the .xlsx file containing all authors' info
# --- fsheet, sheet name of in the .xlsx file
# --- fnames, vector of column names of first/middle/last names
# --- faffs, vector of column names of affiliations
# --- fcontr, vector of column name of author contributions
# --- fackn,  vector of column name of acknowledgement/ funding info
# --- fcoi,   vector of column name of conflict of interest (COI)
# --- fout_title_page, full file path (.docx) of title, running title, author list, affiliations, corr author
# --- fout_ackn_page,  full file path (.docx) of author contribution, acknowledgement (funding) & conflict of interest
# Output
# --- a .docx file containing the info of author(s) list & affiliation(s) list
# --- a .docx file containing the info of author(s) contributions & acknowledgement
DS_authors <- function(fin, 
                       fsheet = 'Sheet1', 
                       fnames = c('First Name', 'Middle I.', 'Last Name'),
                       faffs = c('Primary Institution', 'Secondary Institution', 'Additional Institutions 1',
                                 'Additional Institutions 2', 'Additional Institutions 3'),
                       fcontr = "Author Contribution",
                       fackn  = "Funding to Acknowledge",
                       fcoi   = "COI:",
                       fout_title_page = 'DSdoc_Title.docx',
                       fout_ackn_page  = 'DSdoc_AuthorContribution_Acknowledgment_COI.docx'){
 
  # load .xlsx file
  df0 <- read_excel(file.path(fin), sheet = fsheet)
  
  ## author fullname (add a new column 'fullname')
  df <- df0 %>% dplyr::mutate(
    ID         = row_number(),
    firstname  = !!rlang::sym(fnames[[1]]),
    middlename = !!rlang::sym(fnames[[2]]),
    lastname   = !!rlang::sym(fnames[[3]]),
    fullname   = paste0(firstname,
                        ' ',
                        ifelse(
                          !is.na(middlename),
                          paste0(middlename, ' '),
                          ''
                        ),
                        lastname),
    auth_contr = !!rlang::sym(fcontr),
    fund_ackn  = !!rlang::sym(fackn),
    conf_inter = !!rlang::sym(fcoi)
  )
  
  
  # wide to long table of affiliations
  df1 <- df %>% 
    dplyr::select(any_of(c('ID', 'fullname', faffs))) %>% 
    pivot_longer(cols = !c('ID', 'fullname'), values_to = "Affiliations")
  
  # unique affiliations & id for each affiliation
  df2 <- df1 %>% dplyr::select(Affiliations) %>% unique() %>% drop_na() %>% dplyr::mutate(affid = row_number()) %>% select(c('affid', 'Affiliations'))
  
  # final author & affiliation info
  df3 <-  df2 %>% merge(df1, by = 'Affiliations') %>% select(c('ID', 'fullname', 'affid')) %>% arrange(ID, affid)
  
  # concatenate affiliation id & remove duplicated rows
  df4 <- df3 %>% group_by(ID) %>% 
    dplyr::mutate(fullname = fullname,
           afid_all = paste(affid, collapse = " ")) %>% 
    distinct(ID, .keep_all = T) %>% 
    select(c('ID', 'fullname', 'afid_all'))
  
  ## information to write to .docx
  # font paramters for Word doc
  t_red <- fp_text(color = 'red')
  t_sup <- fp_text(vertical.align = 'superscript')
  t_sub <- fp_text(vertical.align = 'subscript')
  t_italy <- fp_text(italic = TRUE)
  t_bold <- fp_text(bold = TRUE)
  
  # title info
  f_title    <- fpar(ftext("[Title: Please insert your title here]", t_red))
  f_runtitle <- fpar(ftext("[Running Title: Please insert your running title here]", t_red))
  
  # author(s) info
  f_authors_txt <- 'fpar('
  for (i in seq(dim(df4)[[1]])){
    f_authors_txt <- f_authors_txt %>% 
      paste("df4$fullname[[",i,"]],", "ftext(df4$afid_all[[",i,"]], t_sup)") %>% 
      paste(ifelse(i != dim(df4)[[1]],   ", ', ',",   ")"))
  }
  f_authors    <-  eval(parse(text=f_authors_txt))
  
  # corresponding author(s) info
  f_corresponding <- fpar(
    ftext("[Please insert Corresponding Author info here]", t_red)
  )
  
  # author contribution
  
  
  # acknowledgement to funding & persons
  
  
  # conflict of interest
  
  
  ######### Author Page ################# 
  # officer: title, running title, & author list
  # template <- system.file(package = "officer","doc_examples", "landscape.docx")
  my_doc <- read_docx() %>% 
    body_add_fpar(f_title, "centered") %>% 
    body_add_par("\n", "centered") %>%
    body_add_fpar(f_runtitle, "centered") %>% 
    body_add_par("\n\n", "centered") %>%
    body_add_fpar(f_authors, 'Normal') %>%
    body_add_par("\n\n", "centered") 
  
  # officer: affiliations
  for (i in seq(dim(df2)[[1]])) {
    my_doc <- my_doc %>%
      body_add_fpar(
        fpar(ftext(df2$affid[[i]], t_sup), ' ', df2$Affiliations[[i]]), 'Normal')
  }
  
  # officer: corresponding author(s)
  my_doc <- my_doc %>%
    body_add_par("\n\n", "centered") %>%
    body_add_fpar(fpar(ftext("Corresponding Author(s):", t_bold))) %>% 
    # body_add_par("Corresponding Author(s):", "Normal") %>%
    body_add_fpar(f_corresponding, 'Normal')
  
  # Output to a .docx file (Title Page)
  fdir<- fout_title_page %>% dirname()
  if (!dir.exists(fdir)) {dir.create(fdir, recursive=TRUE)}
  print(my_doc, target = fout_title_page)
 ############################################### 
  
  
 ### Author contribution, acknowledgements, conflict of interest
  
  ## initializing a docx
  my_doc <- read_docx()
  
  
  ## officer: author contribution
  #df <- df %>% drop_na()
  my_doc <- my_doc %>% 
    body_add_fpar(fpar(ftext("Author Contribution Statement", t_bold)))
  for (i in seq(dim(df)[[1]])) {
    my_doc <- my_doc %>%
      body_add_fpar(
        fpar(ftext(df$fullname[[i]], t_bold), ': ', df$auth_contr[[i]]), 'Normal')
  }
  
  
  ## officer: acknowledgement & funding
  my_doc <- my_doc %>% 
    body_add_par("\n\n", "Normal") %>% 
    body_add_fpar(fpar(ftext("Acknowledgments", t_bold)))
  
  # remove meaningless and duplicated records
  vstr <- df %>% 
    dplyr::filter(!fund_ackn %in% c('na', 'NA', 'see above', NA)) %>% 
    .$fund_ackn %>% unique()
  # remove ".", ";", & " " at the end of each record
  vstr <- vstr %>% gsub("[;\\. ]+$", "", .)
  # add ";" at the end of each record, concatenate all records, split the string, and
  # remove duplicates
  vstr <- vstr %>%
    paste(collapse = "; ") %>% 
    strsplit(";")
  # concatenate records
  vstr <- vstr %>% unlist() %>% paste(collapse = ";")
  # add a sentence at the end
  vstr <- paste0(vstr, ". The views expressed in this article are those of the authors and do not necessarily reflect the position or policy of any funding sources listed here.")
  # write
  my_doc <- my_doc %>% 
    body_add_par(vstr, "Normal")
  
  
  # officer: conflict of interest
  my_doc <- my_doc %>% 
    body_add_par("\n\n", "Normal") %>% 
    body_add_fpar(fpar(ftext("Conflict of Interest", t_bold)))
  
  # remove meaningless and duplicated records
  vstr <- df %>% 
    dplyr::filter(!conf_inter %in% c('none', 'na', 'NA', NA)) %>% 
    .$conf_inter %>% unique()
  # remove ".", ";", & " " at the end of each record
  vstr <- vstr %>% gsub("[;\\. ]+$", "", .)
  # add "." at the end of each record, concatenate all records
  vstr <- vstr %>% paste(collapse = ". ")
  # add a sentence at the end
  vstr <- paste0(vstr, ". All other authors have no conflicts of interest to declare.")
  # write
  my_doc <- my_doc %>% 
    body_add_par(vstr, "Normal")
  
  
  ## Output to a .docx file (Acknowledgment Page)
  fdir<- fout_ackn_page %>% dirname()
  if (!dir.exists(fdir)) {dir.create(fdir, recursive=TRUE)}
  print(my_doc, target = fout_ackn_page)
  
}

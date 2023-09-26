### Copy and paste files with specific strings while preserving the folder structure

## Load packages
if (!require("pacman"))
  install.packages("pacman") # make sure that pacman is installed

pacman::p_load(tidyverse,    # data management
               fs    # to handle file system operations in a platform-indepdent way
)


# Input
# --- fdir_in, path to source directory, 
#.     e.g., "/Volumes/dusom_morey/Data/Lab/new_halfpipe/Outputs/atlas_conn"
# --- fdir_out, path to destination directory, 
#.     e.g., "../Data/BIAC_Server"
# --- fstrings, the specific strings to match, 
#.     e.g. "task-rest_feature-corrMatrix_atlas-schaefer2011Combined_timeseries.tsv", 
#.          or c("string1","string2"), default=NULL
# Output
# --- files with specific strings while preserving the folder structure in the 
#     destination directory
DS_copy <- function(fdir_in, fdir_out, fstrings=NULL){
  # Recursively search for files in the source directory (time consuming!!!)
  files <- fdir_in %>% fs::dir_ls(recurse=T)
  
  # Filter the files based on the specific strings
  if (!is.null(fstrings)){
    matching_files <- files[grepl(paste(fstrings, collapse="|"), files)]
  } else {
    print("Error: the specific strings are NULL!!!")
  }
  
  # Create the corresponding directories in the destination directory
  dirs <- matching_files %>% dirname()
  fs::path(fdir_out, dirs) %>% fs::dir_create()
  
  # Copy the matching files to the destination directory
  print("Copying files!!! Please be patient...")
  matching_files %>% fs::file_copy(
    fs::path(fdir_out, matching_files)
  )
  print("Files copied!!! Please double check the files!")
}
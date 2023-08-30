# Deep_Simple
An R package to simplify the draft of academic research papers.

Now is for internal testing only.

Please download the package and save the files in a folder named "DS" which is in the same directory as your main R Markdown file.

The organization of your files and data looks like below:

Your/path/to/the/project/Scripts/SDL_main.RMD                          # The main R markdown file to do most of the analyses
                                /SDL_TS.RMD                            # The R markdown file to copy the time series files to your own computer
Your/path/to/the/project/Data/ts/                                      # The folder to contain the time series files
                             /Brain_Atlas_Schaefer400_FreeSurfer17.csv # Brain atlas's labels and coordinates
Your/path/to/the/project/Scripts/DS/DS_authors.R                       # R script to make a draft of the author list, affiliations, acknowledgments, author contributions, and conflict of interest
                                   /DS_net_FC.R                        # R script to calculate static or dynamic functional connections
                                   /DS_plots_ggseg.R                   # R script to plot at-las-based brain maps
                                   /DS_stat.R                          # R script for mass univariate analyses
                                   /DS_table1.R                        # R script to make the draft of Table 1 (demographic & clinical info)
Your/path/to/the/project/Results/FC/FC_Matrix_TriNetwork/              # The folder to contain the FC matrix (either static or dynamic FC)
                                   /Stats/                             # The folder to contain statistical outputs
                                 

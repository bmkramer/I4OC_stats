 
#install.packages("tidyverse")
library(tidyverse)

#define function to count number of publishers with coverage above set threshold
getThreshold <- function(file){
  
  file_summary <- file %>%
    summarize(count_current = sum(as.numeric(count_current_type > counts_current_threshold)),
              count_backfile = sum(as.numeric(count_backfile_type > counts_backfile_threshold)),
              refs_current = sum(as.numeric(references_current_type > references_current_threshold & count_current_type >  counts_current_threshold)),
              refs_backfile = sum(as.numeric(references_backfile_type > references_backfile_threshold & count_backfile_type >  counts_backfile_threshold))
    ) %>%
    mutate(perc_current = round(refs_current / count_current, 3),
           perc_backfile = round(refs_backfile / count_backfile, 3))
}  


#------------------------------------------------------------------------------

#set date to date of data collection
date <- "2022-06-07"
#set output directory
path <- file.path("data",date) 

#set variables
##set threshold for counts of dois
counts_current_threshold <- 100
counts_backfile_threshold <- 100
##set threshold for percentage of references
references_current_threshold <- 0.75
references_backfile_threshold <- 0.25

#get counts
type = "all"
filename <- paste0(path,"/crossref_member_references_", type, "_", date,".csv")
file <- read_csv(filename)

file_summary <- getThreshold(file)
view(file_summary)

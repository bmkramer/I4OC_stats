#this script uses the rcrossref package to interface with the Crossref API 
#to get information on coverage of references per member ID
#Crossref REST API information: https://github.com/CrossRef/rest-api-doc

#use rcrossref
#vignette: https://cran.r-project.org/web/packages/rcrossref/rcrossref.pdf 

#install.packages("tidyverse")
#install.packages("rcrossref")
library(tidyverse)
library(rcrossref)

#set email in Renviron
#file.edit("~/.Renviron")
#add email address to be shared with Crossref:
#crossref_email = name@example.com
#save the file and restart your R session
  
#Use low level API as this includes abstract coverage per type
#set parse = FALSE to get JSON, parse = TRUE to get list output
getCrossref_low <- function(offset){
  res <- cr_members_(offset = offset,
                     limit = 1000,
                     parse = TRUE) %>%
  
  .$message %>%
  .$items
  
  return(res)
}

#add progress bar to function
getCrossref_low_progress <- function(offset){
  pb$tick()$print()
  result <- getCrossref_low(offset)
  
  return(result)
}

#extract relevant variables with pluck
#also consider (from ?pluck):
#The map() functions use pluck() by default to retrieve multiple values from a list:
# map(x, list(2, "elt")) is equivalent to map(x, pluck, 2, "elt")

extractData_all <- function(x){
  data <- tibble(
    id = map_dbl(x, "id"),
    primary_name = map_chr(x, "primary-name"),
    count_current_type = map_dbl(x, 
                                 list("counts",
                                      "current-dois"),
                                 .default = 0),
    count_backfile_type = map_dbl(x, 
                                 list("counts",
                                      "backfile-dois"),
                                 .default = 0),
    deposits_references_current = map_lgl(x,
                                         list("flags",
                                              "deposits-references-current"),
                                         .default = NA),
    deposits_references_backfile = map_lgl(x,
                                          list("flags",
                                               "deposits-references-backfile"),
                                          .default = NA),
    references_current_type = map(x,
                                  list("coverage",
                                       "references-current"),
                                  .default = 0),
    references_backfile_type = map(x,
                                 list("coverage",
                                      "references-backfile"),
                                 .default = 0)) %>%
    #keep only members with (current) output of type
    #filter(count_current_type > 0) %>%
    #convert reference coverage into numerical, then percentage
    mutate(references_current_type = as.double(references_current_type),
           references_backfile_type = as.double(references_backfile_type)) %>%
    #arrange in descending order of count
    arrange(desc(count_current_type))
  
  return(data)
}



extractData_type <- function(x, type){
  data <- tibble(
    id = map_dbl(x, "id"),
    primary_name = map_chr(x, "primary-name"),
    count_current_type = map_dbl(x, 
                                 list("counts-type",
                                      "current",
                                      type),
                                 .default = 0),
    count_backfile_type = map_dbl(x, 
                                 list("counts-type",
                                      "backfile",
                                      type),
                                 .default = 0),
    deposits_references_current = map_lgl(x,
                                         list("flags",
                                              "deposits-references-current"),
                                         .default = NA),
    deposits_references_backfile = map_lgl(x,
                                          list("flags",
                                               "deposits-references-backfile"),
                                          .default = NA),
    references_current_type = map(x,
                                  list("coverage-type",
                                       "current", 
                                       type,
                                       "references"),
                                  .default = 0),
    references_backfile_type = map(x,
                                 list("coverage-type",
                                      "backfile", 
                                      type,
                                      "references"),
                                 .default = 0)) %>%
    #keep only members with (current) output of type
    #filter(count_current_type > 0) %>%
    ##convert abstract coverage into numerical
    mutate(references_current_type = as.double(references_current_type),
           references_backfile_type = as.double(references_backfile_type)) %>%
    #arrange in descending order of count
    arrange(desc(count_current_type))
  
  return(data)
  }

#define function to write to csv
toFile <- function(type, data, path){
  filename <- paste0(path,"/crossref_member_references_", type, "_", date,".csv")
  write_csv(data, filename)
}


#------------------------------------------------------------------------------

#set date
date <- Sys.Date()
#create output directory
path <- file.path("data",date) 
dir.create(path)

#set cutoff for percentage of references
cutoff_current <- 0.75
cutoff_backfile <- 0.25

#get number of members
res <- cr_members(limit=0)
total <- res$meta$total_results

#set vector of offset values
c <- seq(0, total, by=1000)

#set parameter for progress bar
pb <- progress_estimated(length(c))

#get API results, flatten into 1 list
res <- map(c, getCrossref_low_progress) %>%
  flatten()

#extract data for different types, write each to file
type = "all"
data <- extractData_all(res)
toFile(type, data, path)

type = "journal-article"
data <- extractData_type(res, type)
toFile(type, data, path)

type = "posted-content"
data <- extractData_type(res, type)
toFile(type, data, path)

type = "book-chapter"
data <- extractData_type(res, type)
toFile(type, data, path)

type = "proceedings-article"
data <- extractData_type(res, type)
toFile(type, data, path)

type = "proceedings"
data <- extractData_type(res, type)
toFile(type, data, path)


#aggregate counts per type


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = T,
  results = "hide")


## ---- eval=FALSE--------------------------------------------------------------
#  
#  # To update the R package in your R environment you may first need to remove it,
#  # and use the exit command quit() to terminate the current R session.
#  # Use the script below to install the R package.
#  devtools::install_github(
#    repo = "https://github.com/maelstrom-research/harmonizRLegacy",
#    auth_token = "ghp_xxx",
#    force = TRUE)
#  
#  library(harmonizRLegacy)
#  
#  # additional packages needed in the demonstration
#  library(tidyverse)
#  library(opalr)
#  
#  # please make sure that those libraries are installed on your R environment:
#  #
#  # DT,         # tools,        # plotly,
#  # bookdown,   # utils,        # readr,
#  # dplyr,      # viridis,      # readxl,
#  # fs,         # xlsx,         # rlang,
#  # ggplot2,    # getPass,      # stringr,
#  # grDevices,  # knitr,        # tibble,
#  # janitor,    # car,          # tidyr,
#  # opalr,      # lubridate     # tidytext,
#  
#  harmonizR_help()
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # use case 1: create folder without versioning
#  harmo_env_create(project = "DEMO")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  o <- opal.login(username = 'administrator',
#                  password = 'password',
#                  url = 'https://opal-demo.maelstrom-research.org/')
#  
#  opal_files_pull(o, from = "/home/administrator/DEMO/data_processing_elements", to = "DEMO")
#  opal_files_pull(o, from = "/home/administrator/DEMO/dataschema",               to = "DEMO")
#  opal_files_pull(o, from = "/home/administrator/DEMO/study_specific_datasets",  to = "DEMO")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # create index of files that are in a  folder
#  index_DEMO <- file_index_create(folder = "DEMO")
#  
#  # search index for files matching a specific file path query
#  file_index_search(index = index_DEMO, file_path = "study_specific_datasets")
#  
#  # read the files
#  file_index_read(index = index_DEMO, file_path = "study_specific_datasets")
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # use case 1: report of demo dataset TOKYO
#  study_visual_report(
#    dataset = study_TOKYO,
#    data_dict = dd_TOKYO_format_maelstrom_tagged,
#    to = "DEMO/reports/TOKYO")
#  
#  # use case 2: report of demo dataset TOKYO, grouped by gndr
#  study_visual_report(
#    dataset = study_TOKYO,
#    data_dict = dd_TOKYO_format_maelstrom_tagged,
#    to = "DEMO/reports/TOKYO_gndr",group_by = "gndr",out = "ggplot2")
#  
#  # re-index your files to include new files created
#  index_DEMO <- file_index_create(folder = "DEMO")
#  
#  # read the bookdown
#  file_index_read(index_DEMO,file_path = "DEMO/reports/TOKYO_gndr/docs/index.html")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # Read the files
#  file_index_read(index_DEMO, file_path = "study_specific_datasets")
#  file_index_read(index_DEMO, file_path = "dataschema")
#  file_index_read(index_DEMO, file_path = "data_processing_elements")
#  
#  # Example 1: run the process of harmonization with data processing elements containing errors
#  harmo_process(
#    dataschema = DEMO_dataschema,
#    data_proc_elem = `DEMO_data_processing_elements - with error`,
#    output_harmo_folder = "DEMO/harmonized_datasets",
#    name_report = "harmo_report")
#  
#  # Example 2: use the function harmo_summary() to access the errors.
#  # In your environment, you will find a report attached to harmonization process.
#  harmo_summary(harmo_report)
#  rm(harmo_report)
#  
#  # After correction, re-run the process of harmonization with the corrected data
#  # processing elements.
#  harmo_process(
#    dataschema = DEMO_dataschema,
#    data_proc_elem = `DEMO_data_processing_elements - final`,
#    name_report = "final_report",
#    output_harmo_folder = "DEMO/harmonized_datasets")
#  
#  # re-index your files to include new files created
#  index_DEMO <- file_index_create(folder = "DEMO")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # pool your data in one tibble
#  harmonized_study_DEMO_table_DEMO <-
#    harmonized_study_MELBOURNE_table_MELBOURNE %>%
#    bind_rows(harmonized_study_PARIS_table_PARIS) %>%
#    bind_rows(harmonized_study_TOKYO_table_TOKYO)
#  
#  # use case 1: report of harmonized_study_DEMO_table_DEMO.
#  harmo_visual_report(
#    harmonized_dataset = harmonized_study_DEMO_table_DEMO,
#    dataschema = DEMO_dataschema,
#    data_proc_elem = `DEMO_data_processing_elements - work in progress`,
#    to = "DEMO/reports/harmonized_DEMO"
#    # group_by = "adm_study"
#  )
#  
#  # re-index your files to include new files created
#  index_DEMO <- file_index_create(folder = "DEMO")
#  
#  # read the bookdown (or open the file named 'index.html' in your browser)
#  file_index_read(index_DEMO,file_path = "DEMO/reports/harmonized_DEMO/docs/index.html")
#  


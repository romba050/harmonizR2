#' Generate a visual report for a study-specific dataset
#'
#' This function generates a visual report for a study-specific dataset in an html
#' bookdown document, showing descriptive statistics for each study-specific variable
#' to facilitate assessment of input data. Statistics and figures are generated
#' according to their valueTypes. The variable valueTypes is automatically detected
#' from the dataset and its data dictionary. The output is editable (using plotly
#' library) or static (using ggplot2 library).
#'
#' Must provide dataset and data dictionary in the Maelstrom Research formats.
#' If the data dictionary is not provided, a basic one is being created during the process.
#'
#' @param dataset A character string or tibble R object identifying the input dataset
#' (in the Maelstrom Research formats). Dataset in Maelstrom Research format has
#' the entity (usually participant) ID as the first column.
#' @param data_dict A tibble identifying the data dictionary (in the Maelstrom Research formats) associated
#' with the dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param to A character string identifying the folder path where the bookdown report
#' will be saved.
#' @param out parameter that specifies the graphical outputs expected in the report:
#' can be either 'ggplot2' or 'plotly'. gglot2 renders static plots, plotly dynamic plots.
#' @param .keep_files Boolean value to say whether to keep the R-markdown files.
#'
#' @return A bookdown folder containing files in the specified output folder
#' (current folder/docs by default). To open the file in browser, open "index.html"
#'
#' @examples
#' \dontrun{
#'
#' # Create index of files in one folder and read the files
#' index_DEMO <- file_index_create(folder = "DEMO")
#' file_index_read(index = index_DEMO, file_name = "study_TOKYO")
#' file_index_read(index = index_DEMO, file_name = "dd_TOKYO")
#'
#' # use case 1: report of demo dataset TOKYO
#' study_visual_report(
#'   dataset = study_TOKYO,
#'   data_dict = dd_TOKYO_format_maelstrom_tagged,
#'   to = "DEMO/reports/TOKYO")
#'
#' # use case 2: report of demo dataset TOKYO, grouped by gndr
#' study_visual_report(
#'   dataset = study_TOKYO,
#'   data_dict = dd_TOKYO_format_maelstrom_tagged,
#'   to = "DEMO/reports/TOKYO_gndr",group_by = "gndr",out = "ggplot2")
#'
#'# re-index files to include new files created
#' index_DEMO <- file_index_create(folder = "DEMO")
#'
#' # read the book down
#' file_index_read(index_DEMO,file_path = "DEMO/reports/TOKYO_gndr/docs/index.html")
#' }
#'
#' @import dplyr bookdown utils rlang readr stringr grDevices fs janitor DT
#' @export
study_visual_report <- function(
  dataset,
  data_dict,
  group_by = NULL,
  to = paste0("reports/"),
  out = "ggplot2",
  .keep_files = TRUE){

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset }

  data_dict_name <- if(class(data_dict)[1] == "character") {data_dict}else{as.character(substitute(data_dict)) }
  data_dict      <- if(class(data_dict)[1] == "character") { parceval(data_dict) }else{ data_dict }

  if(is_empty(data_dict)){data_dict <- data_dict_create(dataset)}

  data_dict$Variables  <- data_dict$Variables %>% filter(table == dataset_name)
  data_dict$Categories <- data_dict$Categories %>% filter(table == dataset_name)

  if(!"summary_1" %in% (data_dict$Variables %>% names)){

    data_dict <- data_dict %>%
      identify_visual_type(data_dict = ., dataset = dataset) %>%
      identify_plot_type(data_dict = ., dataset = dataset, group_by = group_by, out = out)
  }

  if(nrow(data_dict$Variables) == 0){

    return(message(
"[Error]: the name of your dataset has not been found in your data dictionary. Please verify
the name of your dataset in your datadictionary (column 'name' in 'Variables' sheet)
and reprocess."))
  }

  # global data
  ## dataset must have ID in first column

  # silently_run(count_tag <- count_tag(data_dict))
  all_na_column <- get_all_na_cols(dataset)
  nb_unique_participants <- dataset %>% select(1) %>% unique %>% nrow()

  template_visual_report(to)
  save(to,data_dict,group_by ,file = paste0(file = paste0(to,"/temp_bookdown_report/bookdown_report.RData")))

  ## markdown writing elements

  ##### HEADER ##########

  paste0(
    '# About the study dataset {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(harmonizRLegacy)
load(file = paste0(file = paste0("',getwd(),"/",to,'/temp_bookdown_report/bookdown_report.RData")))

```
--------------------------------------------------------------------------------


**Name of the study**: ',data_dict$Variables$project %>% unique,'

**Name of the study dataset**: ',data_dict$Variables$table %>% unique,'

**Number of unique participants**: ',dataset %>% nrow,'

**Number of variables (including id column)**: ',dataset %>% ncol,'

**Variables where observations are all NA**: ',all_na_column %>% toString(),'

## Variables

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(
  data_dict$Variables %>%
    select(-viz_type, -code_dd, -plot_1, -plot_2, -plot_3, -plot_4, -summary_1) %>%
    filter(name != "',dataset[1] %>% names,'") %>%
    remove_empty("cols") %>%
    mutate(name = paste0("<a href=\\"./var",index,".html\\" >",name,"</a>")),
  options = list(scrollX = TRUE),rownames = FALSE,escape = FALSE)

```
--------------------------------------------------------------------------------

## Categories

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(data_dict$Categories %>% remove_empty("cols"),
  options = list(scrollX = TRUE),rownames = FALSE)

```
--------------------------------------------------------------------------------

') %>% write_lines(.,file = paste0(to,"/temp_bookdown_report/file/bookdown-template-main/index.Rmd"), append = TRUE)


  # ## Areas of information
  #
  # ```{r echo = FALSE, message = FALSE, warning = FALSE}
  #
  # # plot_bar("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  # # plot_pie("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  #
  # ```
  #

  ##### CONTENT ##########

  increment <-   paste0(rep(0,((nrow(data_dict$Variables)) %>% nchar)) %>% paste(collapse = ""))

  for(i in 1:nrow(data_dict$Variables)){

    rmd_file_name <-
      paste0(to,"/temp_bookdown_report/file/bookdown-template-main/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             data_dict$Variables$name[i],".Rmd")
    file.create(rmd_file_name)

    paste0(
      "# ",data_dict$Variables$name[i],"{.unnumbered #var",i,"}\n\n") %>%

      paste0("\n**VARIABLE CHARACTERISTICS**\n") %>%

      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r echo = FALSE, message = FALSE, warning = FALSE, knitr.figure = TRUE}\n",
        "datatable(
   data_dict$Variables %>%
     filter(name == '",data_dict$Variables$name[i],"') %>%
     select(-viz_type, -code_dd, -plot_1, -plot_2, -plot_3, -plot_4, -summary_1) %>%
     gather %>% filter(!is.na(value)) %>%
     mutate(key = paste0('<b>' , key, '</b>')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = TRUE),
   rownames = FALSE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        "\n```\n") %>%

      paste0("\n</div>\n\n") %>%
      paste0(
        ifelse(data_dict$Categories %>% filter(variable == data_dict$Variables$name[i]) %>% nrow > 0,
               paste0("\n* **Categories**: ","\n\n") %>%
                 paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
                 paste0(
                   "\n```{r echo = FALSE, message = FALSE, warning = FALSE}\n",
                   "datatable(
   data_dict$Categories %>% filter(variable == '",data_dict$Variables$name[i],"'),
   options = list(scrollX = TRUE),rownames = FALSE)",
                   "\n```\n") %>%
                 paste0("\n</div>\n\n")
               ,"")) %>%
      paste0("\n--------------------------------------------------------------------------------\n") %>%
      paste0("\n**SUMMARY STATISTICS**\n") %>%
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0("\n```{r echo = FALSE, message = FALSE, warning = FALSE, knitr.figure = TRUE}\n\n",data_dict$Variables$summary_1[i],"\n\n```\n") %>%
      paste0("\n</div>\n\n") %>%
      paste0("\n--------------------------------------------------------------------------------\n") %>%
      paste0("\n**VISUAL REPRESENTATION**\n") %>%

      paste0("\n```{r, figures-plot12-",i,", fig.show='hold',fig.align = 'center', echo = FALSE, message = FALSE, warning = FALSE}\n",
             "\n","try({",data_dict$Variables$plot_1[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_2[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_3[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_4[i],"}, silent = TRUE)","\n",
             "\n```\n") %>%
      paste0("\n") %>%

      write_lines(.,file = rmd_file_name, append = FALSE)
  }

  wd <- getwd()
  graphics.off()

  setwd(paste0(wd,"/",to,"/temp_bookdown_report/file/bookdown-template-main/"))
  silently_run(file.remove(list.files() %>% str_subset(dataset[1] %>% names)))
  try({render_book(paste0(wd,"/",to,"/temp_bookdown_report/file/bookdown-template-main/index.Rmd"))},silent = FALSE)
  setwd(wd)

  if(dir.exists(paste0(to,"/docs"))){  dir_delete(paste0(to,"/docs"))}
  dir_copy(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/docs"),
           paste0(to,"/docs"))

  if(.keep_files == FALSE){dir_delete(paste0(to,"/temp_bookdown_report/"))}

  browseURL(paste0(to,"/docs/index.html"))


  return(message(paste0("\n\nTo edit your file, open: ",
                        to,"/docs/index.html in your browser (Compatibility tested on Chrome and Mozilla)\n\n")))

}

#' Generate a visual report for a harmonized dataset
#'
#' This function generates a visual report for an harmonized dataset in in an html
#' bookdown document, showing descriptive statistics for each harmonized variable
#' to facilitate assessment of input data. Statistics and figures are generated
#' according to their valueTypes. The variable valueTypes is automatically detected
#' from the dataset and its data dictionary. The output is editable (using plotly
#' library) or static (using ggplot2 library).
#'
#' Must provide a harmonized dataset and DataSchema in the Maelstrom Research formats.
#' The DataSchema is a tibble identifying the core data to be generated
#'
#' @param dataschema A tibble identifying the DataSchema to use for harmonization
#' @param data_proc_elem A character string or tibble R object identifying the
#' data processing elements file
#' @param harmonized_dataset A character string or tibble identifying the input dataset.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column. Default is “adm_study” (following the Maelstrom Reaseach naming standard).
#' @param to A character string identifying the folder path where the bookdown report
#' will be saved.
#' @param out parameter that specifies the graphical outputs expected in the report:
#' can be either 'ggplot2' or 'plotly'. gglot2 renders static plots, plotly dynamic plots.
#' @param .keep_files Boolean value to say whether to keep the R-markdown files.
#'
#' @return A bookdown folder containing files in the specified output folder
#' (current folder/docs by default). To open the file in browser, open "index.html"
#'
#' @examples
#' \dontrun{
#' # Read the files. Make sure the harmonization has been processed.
#' index_DEMO <- file_index_create(folder = "DEMO")
#' file_index_read(index = index_DEMO, file_path = "dataschema")
#' file_index_search(index_DEMO, file_path = "data_processing_elements")
#' file_index_read(index_DEMO, file_path = "harmonized_datasets")
#'
#' # pool data in one tibble
#' harmonized_study_DEMO_table_DEMO <-
#'   harmonized_study_MELBOURNE_table_MELBOURNE %>%
#'   bind_rows(harmonized_study_PARIS_table_PARIS) %>%
#'   bind_rows(harmonized_study_TOKYO_table_TOKYO)
#'
#' # use case 1: report of harmonized_study_DEMO_table_DEMO.
#' harmo_visual_report(
#'   harmonized_dataset = harmonized_study_DEMO_table_DEMO,
#'   dataschema = DEMO_dataschema,
#'   data_proc_elem = `DEMO_data_processing_elements - work in progress`,
#'   to = "DEMO/reports/harmonized_DEMO"
#'   # group_by = "adm_study"
#' )
#'
#' # re-index files to include new files created
#' index_DEMO <- file_index_create(folder = "DEMO")
#'
#' # read the book down (or open in browser the file named 'index.html')
#' file_index_read(index_DEMO,file_path = "DEMO/reports/harmonized_DEMO/docs/index.html")
#'
#' }
#'
#' @import dplyr
#' @export
harmo_visual_report <- function(
  dataschema,
  data_proc_elem = NULL,
  harmonized_dataset,
  group_by = "adm_study",
  to = paste0("report/"),
  out = "plotly",
  .keep_files = FALSE){


  harmonized_dataset_name <-
    if(class(harmonized_dataset)[1] == "character") {harmonized_dataset
    }else{as.character(substitute(harmonized_dataset)) }
  harmonized_dataset      <-
    if(class(harmonized_dataset)[1] == "character") { parceval(harmonized_dataset)
    }else{ harmonized_dataset}

  dataschema_name <- if(class(dataschema)[1] == "character") {dataschema}else{as.character(substitute(dataschema)) }
  dataschema      <- if(class(dataschema)[1] == "character") { parceval(dataschema) }else{ dataschema }

  dataschema$Variables <- dataschema$Variables %>% mutate(table = "dataset")
  dataschema$Categories <- dataschema$Categories %>% mutate(table = "dataset")

  dataset <-
    if(is.null(group_by)){harmonized_dataset}else{
      harmonized_dataset %>%
        mutate_at(.vars = group_by, .funs = ~as.character(.)) %>%
        bind_rows(harmonized_dataset %>% mutate_at(.vars = group_by, .funs = ~"TOTAL"))}

  study_visual_report(data_dict = dataschema, dataset = dataset, group_by = group_by,to = to,.keep_files = .keep_files, out = out)

}


#' Identify visual type of a variable based on valueType
#'
#' This helper function analyses the content of a dataset and it data dictionary
#' to extract the type of visualization to generate in a report. This function can
#' be used to manually personalize the report parameters.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param data_dict tibble of the data dictionary (automatically generated if not provided)
#'
#' @return A list of two tibbles which makes up the data dictionary in Maelstrom Research format
#' where a column 'viz_type' has been added to the datadictionary provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: viz type of iris dataset
#' library(tidyverse)
#' identify_visual_type(dataset = iris) %>% .$Variables %>% select(name,viz_type)
#' }
#'
#' @import readr dplyr tibble
#' @export
identify_visual_type <- function(dataset, data_dict = data_dict_create(dataset)){

  if(is.null(dataset)){
    dataset <-
      read_csv("\n", col_names = data_dict$Variables %>% pull(name) %>% unique,show_col_types = FALSE) %>%
      add_row()}

  try({
    var_names_cat_dd <-
      data_dict$Categories %>%
      select(variable, code = name) %>% unique %>%
      group_by(variable) %>%
      summarise(code_dd = paste(code,collapse = "','")) %>%
      mutate(code_dd = paste0("c('NA','",code_dd,"')")) %>%
      filter(!is.na(variable))

    if(nrow(var_names_cat_dd) == 0){
      data_dict$Variables <- data_dict$Variables %>%
        rename(variable = name) %>%
        filter(variable %in% (dataset %>% names)) %>%
        mutate(viz_type = valueType) %>%
        rename(name = variable)
    }else{

      name_var <- dataset[dataset %>% names() %in% (var_names_cat_dd$variable)] %>% names
      var_name_cat_dataset <- tibble(variable = as.character(),code_dataset = as.character())
      for(i in name_var){
        var_name_cat_dataset <-
          add_row(
            var_name_cat_dataset,
            dataset[i] %>% unique %>% pull(.) %>% toString(.) %>%
              str_replace_all(", ","','") %>% paste0("c('",.,"')") %>%
              as_tibble() %>% mutate(variable = i) %>%
              select(variable, code_dataset = value))
      }

      to_eval <-
        var_name_cat_dataset %>% inner_join(var_names_cat_dd) %>%
        mutate(to_eval = paste0("all(",code_dataset," %in% ",code_dd,")"))

      to_eval <-
        to_eval %>% rowwise %>% mutate(to_eval = parceval(to_eval)) %>%
        mutate(viz_type = ifelse(to_eval == TRUE,"categorical","dual")) %>%
        select(variable,viz_type)

      data_dict$Variables <-
        data_dict$Variables %>% rename(variable = name) %>%
        left_join(.,to_eval) %>%
        filter(variable %in% (dataset %>% names)) %>%
        mutate(viz_type = ifelse(is.na(viz_type), valueType,viz_type)) %>%
        rename(name = variable)
    }

  },silent = TRUE)


  return(data_dict)

}

#' Generate R script for plots based on the 'viz_type' of the variable
#'
#' This helper function uses the visual type attributed to a variable in a data dictionary
#' to generate R script to generate plots in a report. This function can be used to
#' manually personalize the report parameters. The plots can use an additional variable
#' to group each variable shown by the grouping variable.
#'
#' The user must run identify_visual_type first to run this function.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param data_dict tibble of the data dictionary (automatically generated if not provided)
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param out parameter that specifies the graphical outputs expected in the report:
#' can be either 'ggplot2' or 'plotly'. gglot2 renders static plots, plotly dynamic plots.
#'
#' @return A list of two tibbles which make up the data dictionary in Maelstrom Research
#' format where columns plots and summary have been added to the data dictionary
#' provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: plot R stripts for iris variables.
#' data_dict_create(iris, categories = "Species") %>%
#' identify_visual_type(data_dict = ., dataset = iris) %>%
#'   identify_plot_type(data_dict = ., dataset = iris) %>% .$Variables %>%
#'   select(name,viz_type, contains("plot"),contains("summary"))
#' }
#'
#' @import dplyr ggplot2
#' @export
identify_plot_type <- function(dataset = NULL, data_dict, group_by = NULL, out = "plotly"){

  if(! "viz_type" %in% colnames(data_dict$Variables)){
    data_dict$Variables <- data_dict$Variables %>% mutate(viz_type = valueType)
  }

  data_dict$Variables <- data_dict$Variables %>%
    left_join(data_dict$Categories %>%
                select(name = variable, code = name, missing) %>%
                filter(missing == 1) %>%
                group_by(name) %>%
                summarise(code_dd = paste(code,collapse = "','")) %>%
                mutate(code_dd = paste0("c('",code_dd,"')")) %>%
                filter(!is.na(name))) %>%
    mutate(code_dd = replace_na(code_dd,"c()"))

  group_by <- ifelse(is.null(group_by),'NULL', paste0("'",group_by,"'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_1 = case_when(
        viz_type == "text"                              ~ paste0('plot_main_word(dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "decimal"                           ~ paste0('plot_box(      dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "integer"                           ~ paste0('plot_box(      dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "date"                              ~ paste0('plot_date(     dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,' , time = "year")'),

        viz_type == "dual"  & valueType == "text"       ~ paste0('plot_main_word(dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "dual"  & valueType == "decimal"    ~ paste0('plot_density(  dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "dual"  & valueType == "integer"    ~ paste0('plot_box(      dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "dual"  & valueType == "date"       ~ paste0('plot_date(     dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,' , time = "year")') ,
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_2 = case_when(
        viz_type == "decimal"                           ~ paste0('plot_density(  dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "integer"                           ~ paste0('plot_histogram(dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "categorical"                       ~ paste0('plot_bar(      dataset = dataset,col = "',name,'"                                 , out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "dual"  & valueType == "decimal"    ~ paste0('plot_box(      dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        viz_type == "dual"  & valueType == "integer"    ~ paste0('plot_histogram(dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')') ,
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_3 = case_when(
        viz_type == "categorical"                       ~ paste0('plot_pie(      dataset = dataset,col = "',name,'" ,                                 out = "',out,'-code", group_by = ',group_by,')'),
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(plot_4 =                              paste0('plot_pie_valid_value(   dataset = dataset,col = "',name,'" , missing_values = "',code_dd,'", out = "',out,'-code", group_by = ',group_by,')'))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      summary_1 = case_when(
        viz_type == "text"                              ~ paste0('summary_text(     dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "decimal"                           ~ paste0('summary_numerical(dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "integer"                           ~ paste0('summary_numerical(dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "date"                              ~ paste0('summary_text(     dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "categorical"                       ~ paste0('summary_category( dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),

        viz_type == "dual"  & valueType == "text"       ~ paste0('summary_text(     dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "dual"  & valueType == "decimal"    ~ paste0('summary_numerical(dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "dual"  & valueType == "integer"    ~ paste0('summary_numerical(dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        viz_type == "dual"  & valueType == "date"       ~ paste0('summary_text(     dataset = dataset,col = "',name,'", missing_values = "',code_dd,'", out = "DT-code", group_by = ',group_by,')'),
        TRUE                      ~ NA_character_))
  # this_dd <<- data_dict

  for (i in 1:length(data_dict$Variables$index)) {
    data_dict$Variables$plot_1[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_1[i])))
    data_dict$Variables$plot_2[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_2[i])))
    data_dict$Variables$plot_3[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_3[i])))
    data_dict$Variables$plot_4[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_4[i])))
    data_dict$Variables$summary_1[i] <- eval(parse(text = str_squish(data_dict$Variables$summary_1[i])))
  }

  return(data_dict)

}

#' Draw bar plot of one (possibly grouped) open-text column in a dataset
#'
#' This function draws a bar plot of the values of open text column. This
#' plot shows the x-th first most cited words in a column having open text content using
#' tidytext library.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param max integer specifying the x-th first most cited words
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return bar plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_main_word()
#'
#' # Example 2: words contains in Species
#' plot_main_word(dataset = dataset,col = "Species",out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2 tidytext
#' @export
plot_main_word        <- function(dataset = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()', max = 10,     out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                              ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                             ,"\n",
    "  filter(!(",col," %in% ",missing_values," | is.na(",col,"))) %>% "              ,"\n",
    "  group_by(",group_by,") %>%"                                                    ,"\n",
    "  mutate(",col," = as.character(",col,")) %>%"                                   ,"\n",
    "  unnest_tokens(output = word, input = ",col,") %>%"                             ,"\n",
    "  anti_join(tidytext::stop_words) %>%"                                           ,"\n",
    "  count(word, sort = TRUE) %>%"                                                  ,"\n",
    "  mutate(word = reorder(word, n)) %>%"                                           ,"\n",
    "  slice(1:min(",max,",nrow(.))) "                                                     )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                         ,"\n",
      "  ggplot(aes(word, n)) +"                                                           ,"\n",
      "  geom_col() +"                                                                     ,"\n",
      "  coord_flip() +"                                                                   ,"\n",
      "  labs(x = 'Word', y = ' Count', title = 'Frequent words in ",col,"') +"            ,"\n",
      "  geom_text(aes(label = n), hjust = 1.2, colour = 'white') +"                       ,"\n",
      "  theme(plot.title = element_text(hjust = 0.5),"                                    ,"\n",
      "       axis.title.x = element_text(face='bold', colour='darkblue', size = 12),"     ,"\n",
      "       axis.title.y = element_text(face='bold', colour='darkblue', size = 12))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                                 ,"\n",
      "  ggplot(aes(word, n)) +"                                                        ,"\n",
      "  geom_col() +"                                                                  ,"\n",
      "  coord_flip() +"                                                                ,"\n",
      "  labs(x = 'Word', y = ' Count', title = 'Frequent words in ",col,"') +"         ,"\n",
      "  geom_text(aes(label = n), hjust = 1.2, colour = 'white') +"                    ,"\n",
      "  theme(plot.title = element_text(hjust = 0.5),"                                 ,"\n",
      "       axis.title.x = element_text(face='bold', colour='darkblue', size = 12),"  ,"\n",
      "       axis.title.y = element_text(face='bold', colour='darkblue', size = 12))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw histogram of one (possibly grouped) column in a dataset
#'
#' This function draws a histogram plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return hist plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_histogram()
#'
#' # Example 2: graph of Petal.Length
#' plot_histogram(dataset = dataset,col = "Petal.Length",out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @export
plot_histogram        <- function(dataset = "airquality", col = "Ozone", filter = 'c()', negate = FALSE, missing_values = 'c()',out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                             ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  ))  "                                     )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                               ,"\n",
      "  ggplot(aes(x = ",col,ifelse(!is.null(group_by),paste0(", fill = ",group_by),""),")) +"  ,"\n",
      "  geom_histogram(color = '#e9ecef',alpha = 0.9, stat = 'count') +"                        ,"\n",
      "  ggtitle('distribution of ",col,"') +"                         ,"\n",
      "  theme(plot.title = element_text(size = 15))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                                         ,"\n",
      "  ggplot(aes(x = ",col,ifelse(!is.null(group_by),paste0(", fill = ",group_by),""),")) +"  ,"\n",
      "  geom_histogram(color = '#e9ecef',alpha = 0.9, stat = 'count') +"                        ,"\n",
      "  ggtitle('distribution of ",col,"') +"                         ,"\n",
      "  theme(plot.title = element_text(size = 15))",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) "                   )
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw box plot of one (possibly grouped) column in a dataset
#'
#' This function draws a box plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return box plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_box()
#'
#' # Example 2: graph of Petal.Length
#' plot_box(dataset = dataset,col = "Petal.Length",out = "ggplot2")
#'
#' }
#'
#' @import dplyr tibble ggplot2
#' @export
plot_box              <- function(dataset = "airquality", col = "Month", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                         ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                         ,"\n",
    "  add_column(participants = 'participants')"                                      )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                    ,"\n",
      "  ggplot(aes(x = ",group_by,", y = ",col,", fill = ",group_by,")) +"         ,"\n",
      "  geom_boxplot() + "                                                         ,"\n",
      "  coord_flip() +"                                                            ,"\n",
      "  theme(legend.position = 'none',plot.title = element_text(size=11)) +"      ,"\n",
      "  ggtitle('Box plot representation of ",col,"') +"                           ,"\n",
      "  ylab('') +"                                                                ,"\n",
      "  xlab('')"                                                                       )
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                                     ,"\n",
      "  ggplot(aes(x = ",group_by,", y = ",col,", fill = ",group_by,")) +"         ,"\n",
      "  geom_boxplot() + "                                                         ,"\n",
      "  coord_flip() +"                                                            ,"\n",
      "  theme(legend.position = 'none',plot.title = element_text(size=11)) +"      ,"\n",
      "  ggtitle('Box plot representation of ",col,"') +"                           ,"\n",
      "  ylab('') +"                                                                ,"\n",
      "  xlab('') )"                                                                     )
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw lollipop plot of one (possibly grouped) time-related column in a dataset
#'
#' This function draws a lollipop plot of the values of time related column.
#' the 'time' parameter uses lubridate synthax to specify the period of time to consider.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param time parameter following lubridate synthaxe to specify the period of time
#' to consider. Can be ymd, mdy, year, months, etc. See lubridate documentation.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return lollipop plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_date()
#'
#' # Example 2: graph of
#'
#' dataset = read_csv_any_formats("study_TOKYO.csv")
#' plot_date(dataset, col = "dob", out = "ggplot2",time = "year")
#' plot_date(dataset, col = "dob", out = "ggplot2",time = "month")
#'
#' }
#'
#' @import dplyr ggplot2 lubridate
#' @export
plot_date             <- function(dataset = "airquality", col = "Day", filter = 'c()', negate = FALSE, missing_values = 'c()', time = "day", out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                      ,"\n",
    "  mutate(",col," = ",time,"(as.Date(",col,"))) %>%"                ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                     ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                     ,"\n",
    "  group_by(",col,",",group_by,") %>%"                                    ,"\n",
    "  tally "                                                                     )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                              ,"\n",
      "  ggplot(aes(x = ",col,", y = n", ifelse(!is.null(group_by),paste0(", color = ",group_by),""),")) +" ,"\n",
      "  geom_segment("                                                         ,"\n",
      "  aes(x = ",col,", xend = ",col,", y = 0, yend = n), color = 'grey') +"  ,"\n",
      "  geom_point(size=4) +"                                                  ,"\n",
      "  theme("                                                                ,"\n",
      "    panel.grid.major.x = element_blank(),"                               ,"\n",
      "    panel.border = element_blank(),"                                     ,"\n",
      "    axis.ticks.x = element_blank()) +"                                   ,"\n",
      "  xlab('distribution of dates - in ",time,"') +"                         ,"\n",
      "  ylab('Number of participants') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                            ,"\n",
      "  ggplot(aes(x = ",col,", y = n", ifelse(!is.null(group_by),paste0(", color = ",group_by),""),")) +" ,"\n",
      "  geom_segment("                                                            ,"\n",
      "  aes(x = ",col,", xend = ",col,", y = 0, yend = n), color = 'grey') +"     ,"\n",
      "  geom_point(size=4) +"                                                     ,"\n",
      "  theme("                                                                   ,"\n",
      "    panel.grid.major.x = element_blank(),"                                  ,"\n",
      "    panel.border = element_blank(),"                                        ,"\n",
      "    axis.ticks.x = element_blank()) +"                                      ,"\n",
      "  xlab('distribution of dates - in ",time,"') +"                            ,"\n",
      "  ylab('Number of participants') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," )"      )
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw bar plot of one (possibly grouped) column in a dataset
#'
#' This function draws a bar plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return bar plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_bar()
#'
#' # Example 2: graph of Species
#' plot_bar(dataset = dataset,col = "Species",out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2 viridis
#' @export
plot_bar              <- function(dataset = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                       ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                      ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                      ,"\n",
    "  mutate(",col," = ",col," %>% as.character ) "                                )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                             ,"\n",
      "  ggplot(aes(x = ",col,", fill =  ",col," )) + "                        ,"\n",
      "  geom_bar() + "                                                        ,"\n",
      "  scale_fill_viridis(discrete = TRUE) + "                               ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                        ,"\n",
      "  ggplot(aes(x = ",col,", fill =  ",col," )) + "        ,"\n",
      "  geom_bar() + "                                        ,"\n",
      "  scale_fill_viridis(discrete = TRUE) + "               ,"\n",
      "  theme(legend.position = 'right')",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw density plot of one (possibly grouped) column in a dataset
#'
#' This function draws a density line plot of the values of a column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return density plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_density()
#'
#' # Example 2: graph of Petal.Length
#' plot_density(dataset = dataset,col = "Petal.Length",out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2
#' @export
plot_density          <- function(dataset = "iris", col = "Sepal.Length", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                                                        ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                                                       ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) "                                                            )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                                                                ,"\n",
      "  ggplot(aes(x = ",col,ifelse(!is.null(group_by),paste0(", fill = ",group_by),""),")) +"                   ,"\n",
      "  geom_density( color = '#e9ecef'", ifelse(!is.null(group_by),"",", fill = '#69b3a2'"),", alpha = 0.8) + " ,"\n",
      "  theme(legend.position = 'right') " ,
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),""))
  }

  if(str_detect(out,"plotly")){
    plot <- paste0(
      "plotly::ggplotly(", plot," %>% "                                               ,"\n",
      "  ggplot(aes(x = ",col,ifelse(!is.null(group_by),paste0(", fill = ",group_by),""),")) +"  ,"\n",
      "  geom_density( color = '#e9ecef'", ifelse(!is.null(group_by),"",", fill = '#69b3a2'"),", alpha = 0.8) + " ,"\n",
      "  theme(legend.position = 'right')",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~",group_by,")"),"")," ) ")

  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw pie chart of one (possibly grouped) column in a dataset
#'
#' This function draws a pie plot of the values of column.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return pie plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_pie()
#'
#' # Example 2: graph of Species
#' plot_pie(dataset = dataset,col = "Species",out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2 viridis
#' @export
plot_pie              <- function(dataset = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = 'c()',               out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  filter(!(",col," %in% ",missing_values,"  )) %>% "                  ,"\n",
    "  mutate(",col," = ",col," %>% as.character ) %>%  "                  ,"\n",
    "  mutate(group_by = ",group_by,") %>% "                               ,"\n",
    "  group_by(",col,",group_by) %>% "                                    ,"\n",
    "  tally "                                                                  )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                           ,"\n",
      "  ggplot(aes(x = '', y = n, fill = ",col,")) +"                       ,"\n",
      "  geom_bar(stat='identity', width = 1, position = position_fill()) + "  ,"\n",
      "  coord_polar('y', start=0) + "                                       ,"\n",
      "  theme_void() + "                                                    ,"\n",
      "  scale_fill_viridis(discrete = TRUE) + "                             ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~group_by)"),""))

  }

  if(str_detect(out,"plotly")){

    count_category = c(0:(eval(parse(text = str_squish(plot))) %>% pull(group_by) %>% unique %>% length))

    plot <- paste0(
      "plotly::plot_ly() %>%"                                                    ,"\n",
      paste(
        paste0("plotly::add_pie(data = ",plot," %>% filter(group_by == \nunique(",plot," %>% pull(group_by)) %>% .[",count_category + 1,"])",",
                             labels = ~",col,",
                             text = ~group_by,
                             values = ~n,
                             domain = list(row = 0, column = ",count_category,")) %>%"),
        collapse = "\n")  ,"\n",
      "       plotly::layout(title = 'Pie Plots with Subplots', showlegend = TRUE,
             grid=list(rows=1, columns=",max(count_category)+1,"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) ")
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Draw pie chart of one (possibly grouped) column in a dataset (valid, non-valid and missing values)
#'
#' This function draws a pie plot of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either
#' 'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
#' gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
#' (usable directly with eval/parse functions) and cat provides indented code in the
#' console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return pie plot object
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' plot_pie_valid_value()
#'
#' # Example 2: graph of Species (virginica is associated to missing values for the
#' # purpose of example)
#' plot_pie_valid_value(dataset = dataset,col = "Species",missing_values = "'virginica'" , out = "ggplot2")
#'
#' }
#'
#' @import dplyr ggplot2 viridis
#' @export
plot_pie_valid_value  <- function(dataset = "iris", col = "Species", filter = 'c()', negate = FALSE, missing_values = "'versicolor'", out = "ggplot2", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")
  #
  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  plot <- paste0(
    dataset_name," %>% "                                                     ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                    ,"\n",
    "  mutate(",col," = ",col," %>% as.character )  %>% "                    ,"\n",
    "  mutate( "                                                             ,"\n",
    "    ",col," = case_when("                                               ,"\n",
    "      is.na(",col,")                     ~ 'missing value',"            ,"\n",
    "      !(",col," %in% ",missing_values,") ~ 'valid value', "             ,"\n",
    "      TRUE                               ~ 'not valid value')) %>% "    ,"\n",
    "  mutate(group_by = ",group_by,")  %>% "                                ,"\n",
    "  group_by(",col,",group_by)               %>% "                        ,"\n",
    "  tally "                                                                )

  if(str_detect(out,"ggplot2")){
    plot <- paste0(
      plot," %>% "                                                           ,"\n",
      "  ggplot(aes(x = '', y = n, fill = ",col,")) +"                       ,"\n",
      "  geom_bar(stat='identity', width = 1, position = position_fill()) + "  ,"\n",
      "  coord_polar('y', start=0) + "                                       ,"\n",
      "  theme_void() + "                                                    ,"\n",
      "  scale_fill_viridis(discrete = TRUE) + "                             ,"\n",
      "  theme(legend.position = 'right') ",
      ifelse(!is.null(group_by),paste0("+ \n  facet_wrap(~group_by)"),""))

  }


  if(str_detect(out,"plotly")){

    count_category = c(0:(eval(parse(text = str_squish(plot))) %>% pull(group_by) %>% unique %>% length))

    plot <- paste0(
      "plotly::plot_ly() %>%","\n",
      paste(
        paste0("plotly::add_pie(data = ",plot," %>% filter(group_by == \nunique(",plot," %>% pull(group_by)) %>% .[",count_category + 1,"])",",
                             labels = ~",col,",
                             text = ~group_by,
                             values = ~n,
                             domain = list(row = 0, column = ",count_category,")) %>%"),
        collapse = "\n")  ,"\n",
      "       plotly::layout(title = 'Pie Plots with Subplots', showlegend = TRUE,
             grid=list(rows=1, columns=",max(count_category)+1,"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) ")
  }

  if(str_detect(out,"code"))                  { return(plot)
  }else if(str_detect(out,"cat"))             { return(plot %>% cat)
  }else if(out == "plotly" | out == "ggplot2"){ return(plot %>% parceval)
  }else                                       { return(message("Valide 'out' attributes are 'ggplot2', 'plotly',",
                                                               "'ggplot2-code', 'plotly-code',",
                                                               "'ggplot2-cat', 'plotly-cat'"))}
}


#' Create summary table of one (possibly grouped) text-type column in a dataset
#'
#' This function creates a datatable of the values of a column with separate valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_text()
#'
#' # Example 2: summary table of Species
#' summary_text(dataset = iris,col = "Species", out = "DT")
#'
#' }
#'
#' @import dplyr DT janitor
#' @export
summary_text          <- function(dataset = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  summary <- paste0(
    dataset_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  filter(!(",col," %in% ",missing_values,")) %>% "                    ,"\n",
    "  mutate(",col," = as.character(",col,")) %>% "                       ,"\n",
    "  group_by(",col,",",group_by,") %>% count %>% "                      ,"\n",
    "  select(2,3,1) %>% mutate(",col," = replace_na(",col,",'-')) %>% "   ,"\n",
    "  rename(`Content` = 1, `Number of answers` = 2) %>% "                ,"\n",
    "  arrange(`Content`,desc(`Number of answers`)) %>% "                  ,"\n",
    "  mutate(`Content` = na_if(`Content`,'')) %>%"                        ,"\n",
    "    remove_empty(which = 'cols') %>%"                                 ,"\n",
    "  datatable( "                                                        ,"\n",
    "    class = 'cell-border stripe', rownames = FALSE,"                  ,"\n",
    "    filter = 'top', editable = FALSE, extensions = 'Buttons', "       ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv') ))"                  )

  if(str_detect(out,"code"))     { return(summary)
  }else if(str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}


#' Create summary table of one (possibly grouped) numerical-type column in a dataset
#'
#' This function creates datatable of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string specifying a column of interest
#' @param filter A character string specifying the values to filter. (equivalent of 'values in')
#' This determines which values should be retained. It can be applied to both grouped
#' and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. These
#' values will not be excluded from counting - but will be displayed separately from valid values.
#'
#' @param dataset A character string or tibble
#' @param col A character string of a column of interest
#' @param filter A character string to subset the rows, applying the expressions in ...
#' to the column values to determine which rows should be retained. It can be applied
#' to both grouped and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. Those
#' values will not be exclud from counting, but will be separated from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_numerical()
#'
#' # Example 2: summary table of Petal.Length
#' summary_numerical(dataset = iris,col = "Petal.Length", out = "DT")
#'
#' }
#'
#' @import dplyr tidyr DT
#' @export
summary_numerical     <- function(dataset = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  summary <- paste0(
    dataset_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  group_by(",group_by,") %>% "                                        ,"\n",
    "  select(",col,") %>% "                                               ,"\n",
    "  rename(group = 1) %>% "                                             ,"\n",
    "  mutate(group = ifelse(group == '', ' ', group)) %>% "               ,"\n",
    "  summarise( "                                                        ,"\n",
    "    nbr.val = sum(!is.na(",col,")), "                                 ,"\n",
    "    nbr.na  = sum(is.na(",col,")), "                                  ,"\n",
    "    min = min(",col,", na.rm = TRUE), "                               ,"\n",
    "    max = max(",col,", na.rm = TRUE), "                               ,"\n",
    "    range = max - min, "                                              ,"\n",
    "    median = median(",col,", na.rm = TRUE), "                         ,"\n",
    "    mean = mean(",col,", na.rm = TRUE), "                             ,"\n",
    "    std.dev = sd(",col,", na.rm = TRUE)) %>% "                        ,"\n",
    "  pivot_longer(!group) %>% "                                          ,"\n",
    "  pivot_wider(names_from = group, "                                   ,"\n",
    "  names_glue = '{group}<br>(all answers)') %>%"                       ,"\n",
    " full_join(     "                                                     ,"\n",
    dataset_name," %>% "                                                   ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                  ,"\n",
    "  filter(!(",col," %in% ",missing_values,"| is.na(",col,"))) %>% "    ,"\n",
    "  group_by(",group_by,") %>% "                                        ,"\n",
    "  select(",col,") %>% "                                               ,"\n",
    "  rename(group = 1) %>% "                                             ,"\n",
    "  mutate(group = ifelse(group == '', ' ', group)) %>% "               ,"\n",
    "  summarise( "                                                        ,"\n",
    "    nbr.val = sum(!is.na(",col,")), "                                 ,"\n",
    "    nbr.na  = sum(is.na(",col,")), "                                  ,"\n",
    "    min = min(",col,", na.rm = TRUE), "                               ,"\n",
    "    max = max(",col,", na.rm = TRUE), "                               ,"\n",
    "    range = max - min, "                                              ,"\n",
    "    median = median(",col,", na.rm = TRUE), "                         ,"\n",
    "    mean = mean(",col,", na.rm = TRUE), "                             ,"\n",
    "    std.dev = sd(",col,", na.rm = TRUE)) %>% "                        ,"\n",
    "  pivot_longer(!group) %>% "                                          ,"\n",
    "  pivot_wider(names_from = group, "                                   ,"\n",
    "  names_glue = '{group}<br>(only valid answers)')) %>%"               ,"\n",
    "  select(' ' = name,order(colnames(.))) %>% "                         ,"\n",
    "  mutate_at(.vars = -1,~round(.,2)) %>% "                             ,"\n",
    "  datatable( "                                                        ,"\n",
    "    class = 'cell-border stripe', rownames = TRUE,"                   ,"\n",
    "    editable = FALSE, extensions = 'Buttons',"                        ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Brtip', buttons = c('csv')), escape = FALSE )"   )

  if(str_detect(out,"code"))     { return(summary)
  }else if(str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}


#' Create summary table of one (possibly grouped) category-type column in a dataset
#'
#' This function creates datatable of the values of a column separating valid,
#' non-valid and missing values.
#' Missing values can be given as input to non-valid and valid values separately, or
#' grouped by another column. The output can be editable (using plotly library) or static
#' (using ggplot2 library). The R-code is also editable for coding recycling purpose.
#' The user can download the content of the datatable in csv format.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param col A character string of a column of interest
#' @param filter A character string to subset the rows, applying the expressions in ...
#' to the column values to determine which rows should be retained. It can be applied
#' to both grouped and ungrouped data.
#' @param negate If TRUE, return non-matching elements.
#' @param missing_values Vector listing values to exclude from valid values. Those
#' values will not be exclud from counting, but will be separated from valid values.
#' @param out parameter that specifies the output expected: can be either 'DT', 'DT-code' and 'DT-cat'.
#' DT renders a datatable using DT library, code gives the code in a string (usable
#' directly with eval/parse functions) and cat provides indented code in the console.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#'
#' @return A datatable (editable) object or a R script in a character string to create it.
#'
#' @examples
#' \dontrun{
#' # Example 1: cat output generated as a template when no argument provided
#' summary_category()
#'
#' # Example 2: summary table of Petal.Length
#' summary_category(dataset = iris,col = "Species", out = "DT")
#'
#' }
#'
#' @import dplyr DT janitor stringr tibble
#' @export
summary_category      <- function(dataset = "iris", col = "col", filter = 'c()', negate = FALSE, missing_values = 'c()', out = "DT-cat", group_by = NULL){

  group_by <- ifelse(is.null(group_by) | toString(group_by) == col,"\'\'",group_by)
  negate <- ifelse(negate == TRUE | (filter == 'c()' & negate == FALSE),"!","")

  dataset_name <- if(class(dataset)[1] == "character") {dataset}else{as.character(substitute(dataset)) }
  dataset      <- if(class(dataset)[1] == "character") { parceval(dataset) }else{ dataset}

  summary <- paste0(
    dataset_name," %>% "                                                        ,"\n",
    "  filter(",negate,"(",col," %in% ",filter, ")) %>% "                       ,"\n",
    "  group_by(",col,", group_by = ",group_by,") %>% count %>% "               ,"\n",
    "  add_column(prop_no_mis = NA_real_) %>% "                                 ,"\n",
    "  group_by(group_by) %>% "                                                 ,"\n",
    "  select(group_by, everything()) %>% "                                     ,"\n",
    "  mutate(prop_no_mis = paste0(round(n/sum(n), digits = 2)*100,'%')) %>%"   ,"\n",
    "  full_join( "                                                             ,"\n",
    dataset_name," %>% "                                                        ,"\n",
    "    filter(",negate,"(",col," %in% ",filter, ")) %>% "                     ,"\n",
    "    filter(!(",col," %in% ",missing_values," | is.na(",col,"))) %>% "      ,"\n",
    "      group_by(",col,", group_by = ",group_by,") %>% count %>% "           ,"\n",
    "      add_column(prop_tot = NA_real_) %>% "                                ,"\n",
    "      group_by(group_by) %>% "                                             ,"\n",
    "      select(group_by, everything()) %>% "                                 ,"\n",
    "      mutate(prop_tot = paste0(round(n/sum(n), digits = 2)*100,'%')))%>% " ,"\n",
    "  mutate(prop_tot = replace_na(prop_tot,'-')) %>% "                        ,"\n",
    "  rename(`Grouping variable` = 1, "                                        ,"\n",
    "          `Category code` = 2, "                                           ,"\n",
    "          `Number of answers` = 3 ,"                                       ,"\n",
    "          `Proportion - all` = 4 , "                                       ,"\n",
    "          `Proportion - valid values` = 5) %>% "                           ,"\n",
    "  select(1,2,3,4,5) %>% "                                                  ,"\n",
    "  mutate(`Grouping variable` = na_if(`Grouping variable`,'')) %>% "        ,"\n",
    "  arrange(`Grouping variable`,`Category code`) %>% "                       ,"\n",
    "    remove_empty('cols') %>% "                                             ,"\n",
    "  datatable( "                                                             ,"\n",
    "    class = 'cell-border stripe', rownames = FALSE,"                       ,"\n",
    "    filter = 'top', editable = FALSE, extensions = 'Buttons', "            ,"\n",
    "    options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv') ))"       )

  if(str_detect(out,"code"))     { return(summary)
  }else if(str_detect(out,"cat")){ return(summary %>% cat)
  }else if(out == "DT")          { return(summary %>% parceval)
  }else                          { return(message("Valide 'out' attributes are 'DT', 'DT-code','DT-cat'"))}

}


#' Create a bookdown template for the visual report
#'
#' This helper function creates a template for the visual report bookdown. This
#' template is taken from the following link:
#' https://github.com/jtr13/bookdown-template/archive/refs/heads/master.zip
#' folder
#'
#' @param to A character string of a path where the bookdown report will be placed
#'
#' @return a folder containing all files (Rmd, yml, docs, ...) to generate bookdown report
#'
#' @examples
#' \dontrun{
#' # Example 1: create a folder containing template
#'
#' template_visual_report("template")
#'
#' }
#'
#' @import dplyr fs utils readr
#' @export
template_visual_report    <- function(to){

  try({unlink(paste0(to,"/temp_bookdown_report/"), recursive = TRUE)},silent = TRUE)
  dir_create(paste0(to,"/temp_bookdown_report"))

  download.file("https://github.com/jtr13/bookdown-template/archive/refs/heads/master.zip", paste0(to,"/temp_bookdown_report/file.zip"))
  unzip(paste0(to,"/temp_bookdown_report/file.zip"),exdir = paste0(to,"/temp_bookdown_report/file"))
  file.remove(paste0(to,"/temp_bookdown_report/file.zip"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/02-tears.Rmd"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/03-race.Rmd"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/README.md"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/docs/index.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/docs/the-pool-of-tears.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/docs/a-caucus-race-and-a-long-tale.html"))
  file.remove(paste0(to,"/temp_bookdown_report/file/bookdown-template-main/docs/search_index.json"))

  paste0(
    'book_filename: "bookdownproj"
output_dir: docs
delete_merged_file: true
edit: https://github.com/YOUR GITHUB USERNAME/YOUR REPO NAME/edit/main/%s
view: https://github.com/YOUR GITHUB USERNAME/YOUR REPO NAME/blob/main/%s
language:
  ui:
    chapter_name: ""

') %>% write_lines(.,file = paste0(to,"/temp_bookdown_report/file/bookdown-template-main/_bookdown.yml"), append = FALSE)


  paste0(
    'body{ /* Normal  */
      font-size: 14px;
  }
td {  /* Table  */
  font-size: 12px;
}
h1.title {
  font-size: 28px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 22px;
  color: DarkBlue;
}
h2 { /* Header 2 */
    font-size: 14px;
  color: DarkBlue;
}
h3 { /* Header 3 */
  font-size: 28px;
  color: green;
}
h4 { /* Header 4 */
  font-size: 12px;
  font-style: italic;
  color: DarkRed;
}
code.r{ /* Code block */
    font-size: 14px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 14px;
}
.center {
   width: 70%;
   margin-right: auto;
}
.twoColomns {
   width: 90%;
   display: flex;
   margin-right: auto;
}

.twoColomns {
   width: 90%;
   display: flex;
   margin-right: auto;
}

') %>% write_lines(.,file = paste0(to,"/temp_bookdown_report/file/bookdown-template-main/style.css"), append = FALSE)

  paste0(
    '---
title: "ASSESSMENT STUDY DATASET"
author: "maelstrom-research.org"
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
---

') %>% write_lines(.,file = paste0(to,"/temp_bookdown_report/file/bookdown-template-main/index.Rmd"), append = FALSE)

}


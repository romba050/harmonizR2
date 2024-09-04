#' Extract columns that are all 'NA' from a dataset
#'
#' This helper function extracts the names of the columns in a dataset having NA
#' values for all observations.
#'
#' @param dataset A character string or tibble of the input dataset
#'
#' @return A vector string indicating either that the dataset does not have empty
#' columns or the names of the empty columns.
#'
#' @examples
#' \dontrun{
#' # Example 1: All columns have observation
#' get_all_na_cols(iris)
#'
#' # Example 2: One column doesn't have any observations
#' get_all_na_cols(iris %>% mutate(new_col = NA))
#' }
#'
#' @import dplyr janitor
get_all_na_cols <- function(dataset){

  # identify columns containing all NA's
  test <-
    dataset %>% summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(),names_to = "name_var",values_to = "condition") %>%
    filter(condition == 0) %>%
    mutate(
      condition = "[INFO] - Empty column")

  return(test)
}


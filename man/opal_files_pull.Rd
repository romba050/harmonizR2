% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07-mlstr_opal_functions.R
\name{opal_files_pull}
\alias{opal_files_pull}
\title{Download files from an Opal environment into the R environment}
\usage{
opal_files_pull(
  opal = opal.login(url = getPass::getPass("Enter the url: "), username =
    getPass::getPass("Enter the username: "), password =
    getPass::getPass("Enter the password: ")),
  from,
  to = paste0(getwd(), "/opal_files")
)
}
\arguments{
\item{opal}{Opal login attributes (asked on prompt if this field is empty)}

\item{from}{A character string of a path where the files will be taken from in R}

\item{to}{A character string of a path where the files will be placed to in Opal}
}
\value{
Folder(s) containing files coming from Opal in user R environment.
}
\description{
These Opal functions are wrappers of functions present in opalr. For further
information, please refer to Opal documentation. https://www.obiba.org/opalr/index.html
}
\details{
The user must be allowed to interact with their Opal
}
\examples{
\dontrun{

# use case 1: download all files from a project folder ("home/project/") or a user's folder ("home/administrator/").
opal_files_pull(
  opal = o,
  from = "/home/administrator/DEMO/data_processing_elements",
  to = "DEMO")

# use case 2: download specific file from an Opal folder and rename it.
opal_files_pull(
  opal = o,
  from = "/home/administrator/DEMO/dataschema/DEMO_dataschema.xlsx",
  to = "DEMO/dataschema/MY_dataschema.xlsx")

}

}

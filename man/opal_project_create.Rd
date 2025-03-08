% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07-mlstr_opal_functions.R
\name{opal_project_create}
\alias{opal_project_create}
\title{Create a project in an Opal environment}
\usage{
opal_project_create(
  opal = opal.login(url = getPass::getPass("Enter the url: "), username =
    getPass::getPass("Enter the username: "), password =
    getPass::getPass("Enter the password: ")),
  project,
  tag = NULL,
  ...
)
}
\arguments{
\item{opal}{Opal login attributes (asked on prompt if empty)}

\item{project}{A character string to name the project in Opal}

\item{tag}{A character string to provide a tag for the Opal project}

\item{...}{Additional parameters. See opal.project_create()}
}
\value{
A project in an Opal environment. If the project already exists, it won't
it will remain as it is, and no new project is created. The errors provided are
associated to the handler or the read/write permissions to Opal.

The user must have adequate credentials to interact with their Opal environment.
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

# use case 1: create a project in Opal
opal_project_create(project = "DEMO" ,tag = "DEMO")

}

}

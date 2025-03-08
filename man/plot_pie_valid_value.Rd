% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-harmo_visualize.R
\name{plot_pie_valid_value}
\alias{plot_pie_valid_value}
\title{Draw pie chart of one (possibly grouped) column in a dataset (valid, non-valid and missing values)}
\usage{
plot_pie_valid_value(
  dataset = "iris",
  col = "Species",
  filter = "c()",
  negate = FALSE,
  missing_values = "'versicolor'",
  out = "ggplot2",
  group_by = NULL
)
}
\arguments{
\item{dataset}{A character string or tibble specifying the input dataset}

\item{col}{A character string specifying a column of interest}

\item{filter}{A character string specifying the values to filter. (equivalent of 'values in')
This determines which values should be retained. It can be applied to both grouped
and ungrouped data.}

\item{negate}{If TRUE, return non-matching elements.}

\item{missing_values}{Vector listing values to exclude from valid values. These
values will not be excluded from counting - but will be displayed separately from valid values.}

\item{out}{parameter that specifies the output expected: can be either
'ggplot2', 'plotly','ggplot2-code', 'plotly-code','ggplot2-cat' or 'plotly-cat'.
gglot2 renders a static plot, plotly a dynamic plot, code gives the code in a string
(usable directly with eval/parse functions) and cat provides indented code in the
console.}

\item{group_by}{A character string of one column in the dataset that can be
taken as a grouping column. The visual element will be grouped and displayed
by this column.}
}
\value{
pie plot object
}
\description{
This function draws a pie plot of the values of a column separating valid,
non-valid and missing values.
Missing values can be given as input to non-valid and valid values separately, or
grouped by another column. The output can be editable (using plotly library) or static
(using ggplot2 library). The R-code is also editable for coding recycling purpose.
}
\examples{
\dontrun{
# Example 1: cat output generated as a template when no argument provided
plot_pie_valid_value()

# Example 2: graph of Species (virginica is associated to missing values for the
# purpose of example)
plot_pie_valid_value(dataset = dataset,col = "Species",missing_values = "'virginica'" , out = "ggplot2")

}

}

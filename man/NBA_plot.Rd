% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs.R
\name{nba_plot}
\alias{nba_plot}
\title{NBA plot function}
\usage{
nba_plot(
  DF,
  GROUPS,
  COLS,
  CHRT = c("bar", "donut"),
  NUM = FALSE,
  LAB,
  GRP = FALSE,
  SAVE = NULL,
  SCALE_TEXT = 1
)
}
\arguments{
\item{DF}{The data frame that contains the information}

\item{GROUPS}{The categorized variables (ecosystem functional group, taxa, etc)}

\item{COLS}{The categories to describe the variables (protection level, threat status, condition, etc). You can use any tidyselect method to select these columns e.g. 2:4/ Endangered:Vulnerable/ c(Well Protected,Moderately Protected,Poorly Protected,Not Protected) etc}

\item{CHRT}{A choice of either "bar" or "donut" plot}

\item{NUM}{A choice to show numbers in the plot, False to show no numbers}

\item{LAB}{The x axis label of the plot}

\item{GRP}{A choice of whether or not to plot the donut graphs by group, TRUE will plot a donut plot for each group.}

\item{SAVE}{The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.}

\item{SCALE_TEXT}{scale the sizes of the plot text to fit your intended output. currently set at 1 as default. If you want to save it to 8 by 6 cm, set it to 0.5.}
}
\value{
Returns a plot
}
\description{
A function to create horizontal bar plots and donut plots.
The function expects that there is a coloumn of groups (e.g. ecosystem function groups,
taxa etc), with one
group value per row, and then several columns of the protection level,
threat status, or condition of the groups with values representing either the number
or percentage/ extent of groups within each category. These columns should be named
according the to conventions in the nbaR::NBA_categories example list.
}
\details{
Please look at the example datasets NBA_example_thr_plot and
NBA_example_pro_plot to see the correct
structure for the data. Please note that both of these datasets have the
same structure, whether it will be used to make a bar or donut plot is
irrelevant.
This function will plot the data as either a bar or donut plot
depending on what you require. You can also decide if you want the donut
plot to be split by ecosystem functional group or not (e.g one donut plot per
functional group) and choose ifyou want the number of ecosystems to be
displayed within the plot.

The name of the groups column is irrelevant, but the categories must be
spelled correctly (there is a list of the standard spellings/ cases
of NBA categories named nbaR::NBA_categories in this package,
which can be accessed for reference).
}
\examples{

bar_plot <- nba_plot(NBA_example_thr_data,
`OVERALL types`,
2:5,
CHRT = "bar",
NUM = TRUE,
LAB = "Percentage of ecosystem types",
SAVE = NULL)

bar_plot

donut_plot <- nba_plot(NBA_example_pro_data,
`OVERALL types`,
2:5,
CHRT = "donut",
NUM = TRUE,
LAB = "Percentage of ecosystem types",
GRP = FALSE,
SAVE = NULL)

donut_plot

}

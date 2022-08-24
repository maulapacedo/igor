#init.R
#
#Example R code to install packages if not already installed 

my_packages <- c("shiny", "data.table", "dplyr", "ggplot2", "plotly", "shinyWidgets", "shinydashboard", 
                 "readr", "lubridate", "DT", "gt", "htmlTable", "kableExtra", "magrittr", "skimr",
                 "gtsummary", "xlsx", "devtools", "rJava", "xlsxjars", "textshaping", "ragg")


install_if_missing <- function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

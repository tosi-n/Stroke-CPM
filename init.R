#
# init.R
# Stroke-CPM
#
# Created by Tosin-Dairo on 28/03/2020
# MIT License
#

my_packages = c("rmarkdown","httr", "reticulate", "tidyverse", "mice", "MASS", "sjPlot", "dataPreparation", "DMwR", "randomForest", "caret", "pROC", "ROCR", "ResourceSelection","devtools")


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
# install pool from Github
invisible(devtools::install_github("rstudio/pool"))
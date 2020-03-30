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

library(reticulate)
library(mice)
library(tidyverse)
library(MASS)
library(sjPlot)
library(dataPreparation)
library(DMwR)
library(randomForest)  
library(caret)
library(pROC)
library(ROCR)
library(ResourceSelection)
use_python("/Volumes/Loopdisk/Dev/PyDsc/env/bin/python3", required = TRUE)
d <- read.csv("/Volumes/Loopdisk/Stroke-CPM/data/train_2v.csv")
reticulate::py_config()
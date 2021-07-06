# libraries and path ------------------------------------------------------
library(reshape)
library(dplyr)
library(lubridate)
library(stats)
library(tseries)
library(rugarch)
library(fpp)
library(forecast)
library(car)
library(nortest)
require(graphics)
library(mFilter)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(randomForest)
library(readxl)

Local = "~/Projects/R/time-series"
setwd(Local)


# Datasets ----------------------------------------------------------------
tox = read.csv("Datasets/toxina.csv",
               encoding = "UTF-8" )

clim = read.csv2("Datasets/dados_clima.csv",
                encoding = "UTF-8")


clim = cbind(clim[,1],apply(clim[,-1],
           2,
           as.numeric))


# Exploratory analysis  --------------------------------------------------



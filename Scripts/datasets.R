# Libraries and Data sets ----------------------------------------------------------------


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


#working directory
setwd("D:/Documents/Projects/time-series")


# Hydrobiologic  ----------------------------------------------------------

hydroB = read_excel("Datasets/Relatório de análises hidrobiológicas 2019.xlsx")
unique(hydroB$`Ponto de amostragem`)

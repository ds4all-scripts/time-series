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

#Toxins data

# Microcystin

# remove "<"
a = str_remove(as.character(tox$MICROCISTINAS),
            "<")

# replace "," by "."
b = str_replace(a,",",".")

# replace "LQ" by "0.15"
c = str_replace(b,"LQ","0.15")
c = as.numeric(c)

mc = data.frame("Date" = as.Date(tox[,2]),
                "Tox" = c)

mc = mc[!is.na(mc$Tox),]

mc$Date = as.Date(mc$Date)

mc$Year = year(mc$Date)

mc$Month = month(mc$Date)

mc$Day = day(mc$Date)

mc$point = ifelse(mc$Day>15,2,1)

f=data.frame()
for (y in unique(mc$Year)) {
  dt = mc[mc$Year == y,]
  cat("\n Year: ", y,"\n")
  for (m in unique(dt$Month)) {
    et = dt[dt$Month == m,]
    cat("\n Month: ", m, "\n")
    print(et)
    cat("\nRbind...\n\n")
    a = tapply(et[et$point==1,2],
               et[et$point ==1,6],
               max)
    b = tapply(et[et$point !=1,2],
               et[et$point != 1,6],
               max)

    c = data.frame("Date"= str_c(y,
                                 m,
                                 "01",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(a),
                                 NA,a)
    )
    d = data.frame("Date"= str_c(y,
                                 m,
                                 "15",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(b),
                                 NA,b)
    )
    print(rbind (c,d))
    f = rbind (f,c,d)
  }
}

mc.q = f

# Saxitoxin

# remove "<"
a = str_remove(as.character(tox$SAXITOXINAS),
               "<")

# replace "," by "."
b = str_replace(a,",",".")

# replace "LQ" by "0.15"
c = str_replace(b,"LQ","0.02")
c = as.numeric(c)

stx = data.frame("Date" = as.Date(tox[,2]),
                "Tox" = c)

stx = stx[!is.na(stx$Tox),]

stx$Date = as.Date(stx$Date)

stx$Year = year(stx$Date)

stx$Month = month(stx$Date)

stx$Day = day(stx$Date)

stx$point = ifelse(stx$Day>15,2,1)

f=data.frame()
for (y in unique(stx$Year)) {
  dt = stx[stx$Year == y,]
  cat("\n Year: ", y,"\n")
  for (m in unique(dt$Month)) {
    et = dt[dt$Month == m,]
    cat("\n Month: ", m, "\n")
    print(et)
    cat("\nRbind...\n\n")
    a = tapply(et[et$point==1,2],
               et[et$point ==1,6],
               max)
    b = tapply(et[et$point !=1,2],
               et[et$point != 1,6],
               max)

    c = data.frame("Date"= str_c(y,
                                 m,
                                 "01",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(a),
                                 NA,a)
    )
    d = data.frame("Date"= str_c(y,
                                 m,
                                 "15",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(b),
                                 NA,b)
    )
    print(rbind (c,d))
    f = rbind (f,c,d)
  }
}

stx.q = f

plot(
  y = stx.q$Tox,
  x = as.Date(stx.q$Date),
  ylab = "STX",
  xlab = "Date",
  type = "l",
  las = 2
)

# Cylindrospermopsin

# remove "<"
a = str_remove(as.character(tox$CILINDROSPERMOPSINA),
               "<")

# replace "," by "."
b = str_replace(a,",",".")

# replace "LQ" by "0.05"
c = str_replace(b,"LQ","0.05")
c = as.numeric(c)

cyn = data.frame("Date" = as.Date(tox[,2]),
                 "Tox" = c)

cyn = cyn[!is.na(cyn$Tox),]

cyn$Date = as.Date(cyn$Date)

cyn$Year = year(cyn$Date)

cyn$Month = month(cyn$Date)

cyn$Day = day(cyn$Date)

cyn$point = ifelse(cyn$Day>15,2,1)

f=data.frame()
for (y in unique(cyn$Year)) {
  dt = cyn[cyn$Year == y,]
  cat("\n Year: ", y,"\n")
  for (m in unique(dt$Month)) {
    et = dt[dt$Month == m,]
    cat("\n Month: ", m, "\n")
    print(et)
    cat("\nRbind...\n\n")
    a = tapply(et[et$point==1,2],
               et[et$point ==1,6],
               max)
    b = tapply(et[et$point !=1,2],
               et[et$point != 1,6],
               max)

    c = data.frame("Date"= str_c(y,
                                 m,
                                 "01",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(a),
                                  NA,a)
    )
    d = data.frame("Date"= str_c(y,
                                 m,
                                 "15",
                                 sep = "-"),
                   "Tox" = ifelse(is.logical(b),
                                  NA,b)
    )
    print(rbind (c,d))
    f = rbind (f,c,d)
  }
}

cyn.q = f

plot(
  y = cyn.q$Tox,
  x = as.Date(cyn.q$Date),
  ylab = "cyn",
  xlab = "Date",
  type = "l",
  las = 2
)
# # Drafting  -------------------------------------------------------------





a = tapply(mc[mc$Year == 2011 &
            mc$Month == 7 &
            mc$point == 1, 2],
       mc[mc$Year == 2011 &
            mc$Month == 7  &
            mc$point == 1, 6],
       max)

b = tapply(mc[mc$Year == 2011 &
                mc$Month == 7 &
                mc$point != 1, 2]
           , mc[mc$Year == 2011 &
                  mc$Month == 7 &
                  mc$point != 1, 6], max)
a = rbind(ifelse(is.logical(a),NA, a),
  ifelse(is.logical(b), NA, b))


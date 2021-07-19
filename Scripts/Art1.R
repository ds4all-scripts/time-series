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


# Analysis ----------------------------------------------------------------

ggplot(toxins,aes(x = as.Date(Date), y = Tox, group = type))+
  geom_line(aes(colour = type))+
  geom_point(shape = 16 , size =1. )+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 0),
        legend.title=element_blank(),
        legend.position = "top",
        #legend.position = c(.05,.95),
        legend.direction = "horizontal")+
  ylab(expression(paste(mu,g,L)^-1))+
  xlab("")

start_Date = min(as.Date(toxins$Date))
end_Date = max(as.Date(toxins$Date))

#Visualizacao simplificada
a = ggplot(toxins, aes(x = as.Date(Date),y = Tox, group = type)) +
  geom_line(aes(color = type), size = 1) +
  scale_x_date(limits = c(start_Date, end_Date))+
  geom_hline(yintercept =1, linetype = 2)+
  theme_cowplot()+
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +

  xlab("") +
  ylab(expression(paste(mu,g,L)^-1));a




# O que se pode notar no gráfico:
# Não há tendencia (aumento ou reducao);
# Eventos ciclicos;
# Padroes sazonais em cada ciclo.
library(urca)
library(seasonal)

a = ggseasonplot(toxins$Tox, polar = F)+
  ggtitle("")+
  geom_point(size = 2)+
  theme_bw()+
  geom_vline(xintercept = c(.4,.7),
             linetype = 2)+
  facet_grid(~year, scales = "free")+
  theme(axis.text.x = element_text(size = rel(.75),
                                   angle = 90))+
  guides(colour = "none")
library(RColorBrewer)

b = ggsubseriesplot(Turb)+ ggtitle("")+
  ylab("Máximas mensais de turbidez (uT)")+
  theme_cowplot()+
  geom_vline(xintercept = c(6,9),
             linetype = 2)+
  geom_point(size = 2)+
  annotate("text", x = c(3,7.5,11),
           y = c(15.5),
           label = c("bold(\"Período de Alta\")",
                     "bold(\"Período de Baixa\")",
                     "bold(\"Período de Alta\")"),parse = TRUE)

plot_grid(b,a,nrow = 2,labels = "AUTO")
#Analisando 2015 a 2017
TURB = TURB[as.Date(TURB$Date)>= start_Date
            & as.Date(TURB$Date ) <= end_Date,]
b = ggplot(data = TURB,
           aes(x = Date,
               y = Turb.max)) +
  geom_line(color = "red", size = 1) +
  stat_smooth( color = "blue",
               fill = "yellow",
               method = "loess" )+
  theme_bw();b
# Observar que a variavel Turb.max:
# Nao apresenta natureza monotonica ( crescente, descrescente ou constante)
# Forte dependencia (correlação) com o tempo

plot_grid(a,b,nrow = 1, labels = "AUTO")



# Decomposição  -----------------------------------------------------------

# Suavizacao exponencial sazonal de Holt-Winters

# Suavização Exponecial Holt
# Series que apresentam tendencia linear
# modelos aditivos e multiplicativos apresentam formas diferentes
# Interpretar gamma = se prox. de 1, valores recente explicam
# melhor a sazonalidade da serie, no contrario (gamma prox. zero),
# valores mais antigos fornecem melhor explicacao.


# Visualização das funções de autocorreção (FAC) e autocorrelação
# parcial (FACP)

par(mfrow = c(2,1))
acf(Turb, main = "(a)",ylab = "FAC",xlab = "",lag.max = 50)
pacf(Turb,main = "(b)",ylab = "FACP",lag.max = 50)

#Decomposição
dec1 = decompose(Turb)
plot(dec1)

#Visualização apenas da tendência
dec1$trend
plot(dec1$trend)



#Modelo aditivo
Ajuste_Turb = HoltWinters(Turb)
Ajuste_Turb
par(mfrow = c(1,1))
plot(Turb,
     ylab = "Valores observados/Ajustados",
     xlab = "Tempo")
lines(fitted(Ajuste_Des)[,1], col = "red", lwd = 2)
legend("top",
       c("Serie original","Suavizaçao Aditiva de Holt-Winters"),
       col = c("black","red"),
       lwd = c(1,2),
       bty = "n")

# Prever 1 ano
prev_Ad_des = forecast::forecast(Ajuste_Des,
                                 h = 12,
                                 level = c(80,95))


# Modelo multiplicativo

Ajuste_Turb1 = HoltWinters(Turb,
                           seasonal = "multiplicative")
prev_Ad_des1 = forecast::forecast(Ajuste_Turb1,
                                  h = 12,
                                  level = c(80,95))
lines(fitted(Ajuste_Des)[,1], col = "red", lwd = 2)

plot(prev_Ad_des,
     ylab = "Previções Desemprego [IC 95%]",
     xlab = "Tempo")
lines(prev_Ad_des1)

train = window(Turb,start = c(2008,2),end = c(2015,4))
test = window(Turb,start = c(2015,5))
fit1 <- hw(train,h = 24,seasonal="additive")
fit2 <- hw(train,h = 60,seasonal="multiplicative")
autoplot(Turb) +
  theme_cowplot()+
  autolayer(fit1, series="Aditivo",
            PI=FALSE) +
  autolayer(fit2, series="Multiplicatico",
            PI=FALSE) +
  geom_vline(aes(xintercept = 2017.3),
             linetype = 2,size = 1)+
  geom_vline(aes(xintercept = 2015.4),
             linetype = 2,size = 1)+
  geom_hline(aes(yintercept = 5 ),
             linetype = 2,size = 1)+
  xlab("Year") +
  ylab("Valor máximo de Turbidez mensal") +
  annotate("text", x = c(2011.6,2016.6,2018.5),
           y = c(15,15,15),
           label = c("bold(\"Treino\")",
                     "bold(\"Teste\")",
                     "bold(\"Prognóstico\")"),parse = TRUE)+

  guides(colour=guide_legend(title="Modelos de Holt-Winters"))


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

mc.q$type = "mc"
stx.q$type = "stx"
cyn.q$type = "cyn"

toxins = rbind(mc.q, stx.q, cyn.q)

write.csv(toxins,file = "Datasets/AllToxins.csv",
          row.names =F,
          col.names = T)

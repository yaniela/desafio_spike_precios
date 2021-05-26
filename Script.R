
#1.Datos: Precipitaciones, Indicadores Económicos Banco Centra

precip <- read.csv("precipitaciones.csv", na.strings=c("NA","#DIV/0!", ""))
banc<-read.csv("banco_central.csv",dec=".", na.strings=c("NA","#DIV/0!", ""))


#2.Análisis de datos. Creación de variables 
## Realiza un análisis exploratorio de la base de datos, ¿Qué puedes decir de los datos, 
##sus distribuciones, valores faltantes,otros? ¿Hay algo que te llame la atención? 

# Estructura del dataframe precip
str(precip)

# Primeras 6 filas
head(precip)

## Distribucion de las precip

library(tidyr)
library(ggplot2)
# or `library(tidyverse)

df<-precip[,-1]

df %>% gather() %>% head()

ggplot(gather(df), aes(value)) + 
  geom_histogram(bins = 75)  + 
  facet_wrap(~key, scales = 'free_x')+xlab("Precipitaciones en mm") 

ggplot(gather(df), aes(y=value, x=key)) + xlab("Regiones") + ylab("Precipitaciones en mm")+ 
geom_boxplot()




# Instalar el paquete Hmisc si es preciso 
if(!is.installed('Hmisc'))  
  install.packages('Hmisc')
library('Hmisc')
describe(precip)

#Se observan 9 variables con 496 filas. Sin valores NA, aqui se incluyen las celdas con "", "NA" y "#DIV/0!". 
#El preiodo de fechas va desde el 1/1/1979 hasta el 1/4/2020 Se observa los valores medios de precipitaciones en cada region, 
# siendola región de los Rios donde más llueve. 

## cantidad de filas duplicadas en precip son 0
sum(duplicated(precip)==TRUE)

# Estructura del dataframe banc

str(banc)

## En el datset banco el imacec es un valor de tipo cadena, y los valores de precio son de tipo numerico. 

describe(banc)

#Se observan 85 variables con 614 observaciones. Existen dos filas duplicadas, la gráfica 1 muestra varias columnas con valores NA.  



#Cambiar a numerico el PIB que esta en millones

indexPib<-grep("PIB",colnames(banc), fixed = TRUE)
indexPrecio<-grep("Precio",colnames(banc), fixed = TRUE)
indexImacec<-grep("Imacec",colnames(banc), fixed = TRUE)

 for(i in 1:length(indexPib)){
   banc[,indexPib[i]]<-gsub("[:alpha:]","",banc[,indexPib[i]],fixed=TRUE)
   banc[,indexPib[i]]<-as.numeric(gsub(".","",banc[,indexPib[i]],fixed=TRUE))
   banc[,indexPib[i]]<-banc[,indexPib[i]]/1e6

 }

for(i in 1:length(indexPrecio)){
  banc[,indexPrecio[i]]<-gsub("[:alpha:]","",banc[,indexPrecio[i]],fixed=TRUE)
  banc[,indexPrecio[i]]<-as.numeric(banc[,indexPrecio[i]])
}

for(i in 1:length(indexImacec)){
  banc[,indexImacec[i]]<-gsub("[:alpha:]","",banc[,indexImacec[i]],fixed=TRUE)
  banc[,indexImacec[i]]<-as.numeric(gsub(".","",banc[,indexImacec[i]],fixed=TRUE))
  banc[,indexImacec[i]]<-banc[,indexImacec[i]]/1e6
}

#falta cambiar el impuesto, los indices y la ocupacion

for(i in 38:39){
  banc[,i]<-gsub("[:alpha:]","",banc[,i],fixed=TRUE)
  banc[,i]<-as.numeric(gsub(".","",banc[,i],fixed=TRUE))
  banc[,i]<-banc[,i]/1e6
}

for(i in 52:74){
  banc[,i]<-gsub("[:alpha:]","",banc[,i],fixed=TRUE)
  banc[,i]<-as.numeric(gsub(".","",banc[,i],fixed=TRUE))
  banc[,i]<-banc[,i]/1e6
}

for(i in 75:84){
  banc[,i]<-gsub("[:alpha:]","",banc[,i],fixed=TRUE)
  banc[,i]<-as.numeric(gsub(".","",banc[,i],fixed=TRUE))
  banc[,i]<-banc[,i]/1e6
}


missing_values_by_columns<-apply(is.na(banc), 2, mean) 
df<-as.data.frame(missing_values_by_columns)
df$names<-row.names(df)
plot3 <-ggplot(data = df, aes(x = missing_values_by_columns, y =names , fill = missing_values_by_columns)) + geom_bar(width =0.7, stat = "identity") + scale_fill_gradient(low = "yellow", high = "red") +theme(axis.text.x = element_text(angle = 90, hjust = 1))

## cantidad de filas duplicadas en banc son solo 2
sum(duplicated(banc)==TRUE)

 # distribucion del dataset

df<-banc[,c(2:10)]

ggplot(gather(df), aes(value)) + 
  geom_histogram(bins = 75)  + 
  facet_wrap(~key, scales = 'free_x')


#Realiza una limpieza de datos para que las series de tiempo no tengan duplicados 
#ni valores incorrectos para ello no elimino las filas con valores NA por que pueden servir para crear los modelos
# solo revisaré los datos incorrectos y transformaré los tipos de datos de la fecha a Date. 

banc<-banc[!duplicated(banc),] ##Eliminando filas duplicadas

unique(precip$date)

precip$date<-as.Date(precip$date)
banc$Periodo<-as.Date(banc$Periodo)

#Revisando si existe alguna fila con fecha incorrecta en banc para eliminarla y existe 1 
sum(is.na(banc$Periodo)==TRUE)
banc<-banc[!is.na(banc$Periodo),]

#En precip todas las fechas son correctas
sum(is.na(precip$date)==TRUE)

#3. Visualizacion
#Crea una función que permita graficar series históricas de precipitaciones para un rango 
#de fechas determinado. Para esto la función debe recibir como argumentos el nombre de una región, 
#fecha de inicio y fecha de término (asegúrate de verificar en tu función que tanto el nombre de la 
#región como las fechas ingresadas existan en el dataset)

#Usa esta función para graficar las precipitaciones para la Región Libertador General Bernardo 
#O'Higgins y para la Región Metropolitana entre las fechas 2000-01-01 y 2020-01-01.o¿ Qué observas 
#con respecto a estacionalidades y tendencias?


library(stringr)
library(ggplot2)
library(lattice)
library(zoo)
library(xts)
library(dygraphs)

#Version 1(Esta no me muestra la tendencia ni la estacionalidad)

plotSerieByRegion <- function(region, inicDate, endDate) {
  
 
  inicDate<-as.Date(inicDate)
  endDate<-as.Date(endDate)
  
  index=vector("integer", length(region))
  pos=1
  
  for ( i in 1:length(region)){ 
    
    region[i]<- iconv(region[i],to="ASCII//TRANSLIT")# eliminar tilde
    region[i]<-str_replace_all(region[i], "[^[:alnum:]]", "_")
    
    if(grepl("Libertador",region[i], fixed = TRUE))  region[i]="Libertador_Gral__Bernardo_O_Higgins"
    if(grepl("Metropolitana",region[i], fixed = TRUE)) region[i]="Metropolitana_de_Santiago"
    
    if(length(grep(region[i], colnames(precip), fixed = TRUE))){
      index[pos]=grep(region[i],colnames(precip), fixed = TRUE)
      pos=pos+1
      
    }
    else{ return( cat("La region: " , region[i] , " no es válida"))
         
      }
  }
 
  d1<-precip[precip$date >= inicDate & precip$date <=endDate,c(1,index)]
 
  
  d1xts=xts(x =d1[,-1], order.by = d1[,1] )
 
}

serieTemp<-plotSerieByRegion(c("Maule",  "Biobio"), "2000-01-01", "2020-01-01")
dygraph(serieTemp)

#Las dos regiones tienen la misma tendencia en los mismos periodos estacionales, la única diferencia es que en una llueve mas que otra. 


#Version 2 (Con la clase ts para series de tiempo)

plotSerieByRegion <- function(region, inicDate, endDate) {
  
  inicDate<-as.Date(inicDate)
  endDate<-as.Date(endDate)
  
  if(is.null(inicDate)||is.null(endDate)){
    return( cat("Una de las fechas es invalida"))
  }
  
  
  region<- iconv(region,to="ASCII//TRANSLIT")# eliminar tilde
  region<-str_replace_all(region, "[^[:alnum:]]", "_")
  
  if(grepl("Libertador",region, fixed = TRUE))  region="Libertador_Gral__Bernardo_O_Higgins"
  if(grepl("Metropolitana",region, fixed = TRUE)) region="Metropolitana_de_Santiago"
    
  if(length(grep(region, colnames(precip), fixed = TRUE)))
      index=grep(region,colnames(precip), fixed = TRUE)
    
   else{ return( cat("La region: " , region , " no es válida"))
      
    }
  
  #Falta revisar que las fechas existan en el dataset
  
  precip_Sort<-precip[order(precip$date),] 
  precipRegions<-precip_Sort[,index]
  
  preciptimeseries <- ts(precipRegions, frequency=12, start=c(format(inicDate, "%Y") ,format(inicDate, "%m")), end=c(format(endDate, "%Y") ,format(endDate, "%m")))
  #preciptimeseriescomponents <- decompose(preciptimeseries)
  
  
}

par(mfrow = c(3, 2),  mar = c(2.2, 2.2, 1, 1), cex = 0.8)
serieTemp1<-plotSerieByRegion(c("Metropolitana"), "2000-01-01", "2020-01-01")
serieTemp2<-plotSerieByRegion(c("Libertador"), "2000-01-01", "2020-01-01")

plot.ts(serieTemp1, axes=F, main="Región Metropolitana")
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot

plot.ts(serieTemp2, axes=F,main="Región Libertador")
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot

trendSerie1 <- decompose(serieTemp1)
trendSerie2 <- decompose(serieTemp2)

plot.ts(trendSerie1$trend, axes=F,main="Tendencia: Región Metropolitana")
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot
plot.ts(trendSerie2$trend, axes=F,main="Tendencia: Región Libertador")
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot


plot.ts(trendSerie1$seasonal, axes=F,main="Estacionalidad: Región Metropolitana")
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot
plot.ts(trendSerie2$seasonal, axes=F,main="Estacionalidad: Región Libertador") 
axis(2) # plot the y axis
axis(1, at=seq(from=2000,to=2020, by=1.0) )
box() # and the box around the plot

#Crea  una  función  que,  para  una  región,  grafique  múltiples  series  de  tiempo mensuales
#de precipitaciones, donde cada serie de tiempo corresponda a un año. La función debe recibir como
#argumento una lista con los años que queremos graficar (2000,  2005,..)  y  el  nombre  de  la  región.  
#El  eje  X  debe  indicar  los  meses  (enero, febrero, etc...).
#•Usa esta función para graficar las precipitaciones 
#para la Región del Maule durante los años 1982, 1992, 2002, 2012 y 2019.o¿Qué puedes concluir de estos gráficos?

#Version 1 Creo un dataframe con este formato donde cada columna es un año:

#   Mes       1982        1992       2002       2012       2019
#   01  10.568819   0.3242845   4.370566   5.590691  15.183145
#   02  13.127745  20.0532612  47.604684  47.591331   2.803785
#   03  28.017354  35.0655824 120.568952   1.184659   1.779509
#   04  28.904933 107.3808402  50.337413  27.407246   8.875549
#

precip_Sort<-precip[order(precip$date),] 

region="Maule"

if(length(grep(region, colnames(precip), fixed = TRUE))){
  index=grep(region,colnames(precip), fixed = TRUE)
} else cat("La region: " , region , " no es válida")
  
years=c("1982", "1992", "2002", "2012", "2019")
df<-split(precip_Sort, format(as.Date(precip_Sort$date), "%Y")) #Devuelve una lista de dataframes por años

df2<-df[years]  #Creo un nuevo dataframe con los años entrados por el usuario

#creo mi dataframe final

df3<-setNames(data.frame(sort(format(as.Date(df2[[1]][,1]), "%m"))), c("Mes"))

for(i in 1:length(years)){
  
df3[years[i]]<-df2[[i]][,index]

}

df3

# Tengo que poner obligado una fecha y no me cuadra, tendria que modificar la etiqueta.
df3$Mes<- seq(from = as.Date("2017-01-01"), to = as.Date("2017-12-01"), by = 'month')
d2xts=xts(x =df3[,-1], order.by = df3[,1] )

#plot(d2xts, legend.loc = "topright", yaxis.right=FALSE)

dygraph(d2xts ) 
#


##Version 2
#         date     Region    year  
# 1982-01-01   1.48317647   1982   
# 1982-02-01   0.89415032   1982   
# 1982-03-01   9.11508497   1982   
# 1982-04-01   0.00000000   1982
# 1982-05-01  65.24232024   1982
# 1982-06-01 101.62241173   1982
# 1982-07-01  64.15089544   1982

plotSerieByYear<-function(region, years){
  
  precip_Sort<-precip[order(precip$date),] 
  if(length(grep(region, colnames(precip), fixed = TRUE))){
    index=grep(region,colnames(precip), fixed = TRUE)
  } else cat("La region: " , region , " no es válida")
  
  df<-precip_Sort[format(as.Date(precip_Sort$date), "%Y")  %in% years,c(1,index)]
  df$Year<-as.numeric(format(df$date, "%Y"))
  df$Month<-format(df$date, "%m")
  
  qplot(Month, df[,2], data = df, geom = "line", group = Year, ylab = str_c("Region ", region)) + 
    facet_grid(df$Year ~ ., scale = "free_y")
  
  
  
}

plotSerieByYear("Maule",c("1982", "1992", "2002", "2012", "2019") )

#Crea una función que permita visualizar dos series históricas de PIB para un rango de
#fechas determinado. Para esto la función debe recibir como input el nombre de cada #serie,
#fecha de inicio y fecha de término.

# Grafica las series de tiempo del PIB agropecuario y silvícola y la del PIB de Servicios
#financieros desde el 2013-01-01 hasta la fecha más reciente en que haya datos.
#o ¿Qué puedes decir de cada serie en particular?
 # o ¿Hay alguna relación entre estas dos series?

plotSeriesPIB<-function(name1="silvicola",name2="financieros", inicDate="2013-01-01", endDate="2020-04-01"){
  
  # validando los argumentos
  
  banc_Sort<-banc[order(banc$Periodo),] 
  if(length(grep(name1, colnames(banc_Sort), fixed = TRUE))){
    index1=grep(name1, colnames(banc_Sort), fixed = TRUE)
  } else cat("El pib: " ,name1 , " no es válido")
  
  if(length(grep(name2, colnames(banc_Sort), fixed = TRUE))){
    index2=grep(name2, colnames(banc_Sort), fixed = TRUE)
  }
  
  inicDate<-as.Date(inicDate)
  endDate<-as.Date(endDate)
  
  # seleccionando las columnas del dataframe
  
  df<-banc_Sort[banc_Sort$Periodo>=inicDate & banc_Sort$Periodo<=endDate,c(1,index1, index2)]
  df[,2]<-as.numeric(df[,2])
  df[,3]<-as.numeric(df[,3])
  df %>% replace(is.na(.), 0)
  
  serieTemp1 <- ts(df[,2], frequency=12, start=c(format(inicDate, "%Y") ,format(inicDate, "%m")), end=c(format(endDate, "%Y") ,format(endDate, "%m")))
  serieTemp2 <- ts(df[,3], frequency=12, start=c(format(inicDate, "%Y") ,format(inicDate, "%m")), end=c(format(endDate, "%Y") ,format(endDate, "%m")))
  
  par(mfrow = c(3, 2))
  plot.ts(serieTemp1, axes=F, main="PIB agropecuario y silvícola")
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  
  plot.ts(serieTemp2, axes=F,main=" PIB de Servicios financiero")
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  
  trendSerie1 <- decompose(serieTemp1)
  trendSerie2 <- decompose(serieTemp2)
  
  plot.ts(trendSerie1$trend, axes=F,main="Tendencia: PIB agropecuario y silvícola")
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  plot.ts(trendSerie2$trend, axes=F,main="Tendencia: PIB de Servicios financiero")
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  
  plot.ts(trendSerie1$seasonal, axes=F,main="Estacionalidad:PIB agropecuario y silvícola")
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  plot.ts(trendSerie2$seasonal, axes=F,main="Estacionalidad: PIB de Servicios financiero") 
  axis(2) # plot the y axis
  axis(1, at=seq(from=2013,to=2020, by=1.0) )
  box() # and the box around the plot
  

}

plotSeriesPIB("silvicola", "financieros", "2013-01-01", "2020-04-01")

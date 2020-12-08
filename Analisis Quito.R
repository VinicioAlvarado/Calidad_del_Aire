library(tidyverse)
library(magrittr)
library(readxl)

#Cargar Datos CO(mg/m3), NO2(ug/m3), O3(ug/m3), PM2,5 (ug/m3) PM10 (ug/m3), SO2(ug/m3), 
# Meteor: Vel m/s, Dir(º), Temp(ºC), Hr(%), Lluvia(mm), Radiación (W/m2)

DIR <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/DIR.xlsx")
DIR
View(DIR)

Velo <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/VEL.xlsx")
View(Velo)
TMP <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/TMP.xlsx")
View(TMP)
HR <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/HUM.xlsx")
View(HR)
PPM <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/LLU.xlsx")
View(PPM)
RS <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/RS.xlsx")
View(RS)
CO <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/CO.xlsx")
View(CO)
NO2 <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/NO2.xlsx")
View(NO2) 
O3 <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/O3.xlsx")
View(O3)
PM25 <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/PM25.xlsx")
View(PM25)
SO2 <- read_xlsx("~/Documents/Doctorado/Contaminación Aire/Datos/SO2.xlsx")
View(SO2)

#### Arreglar colunma inicial separar fecha de hora ####
library(lubridate)
DIR %>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " ", convert = TRUE )
DIR %>% 
  parse_date()

Velo%>% 
  separate("Fecha \\ Unidad", into= c("Fecha", "Hora"), sep= " " )

TMP%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

HR%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

PPM%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

RS%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

CO %>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )
names(CO)

NO2%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

O3%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

PM25%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )

SO2%>% 
  separate("FECHA \\ UNIDAD", into= c("Fecha", "Hora"), sep= " " )



#### Filtrado y ordenamiento de datos, Renombrar columnas ####

#Para dirección viento

DIR$Cotocollao <- NULL #Elimino
DIR$Guamaní <- NULL #Elimino
DIR$LosChillos <- NULL #Elimino
DIR$SanAntonio <- NULL #Elimino
DIR$Tumbaco <- NULL #Elimino
DIR <-  DIR %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")

#Para humedad relativa

HR$Carapungo <- NULL #Elimino
HR$Cotocollao <- NULL #Elimino
HR$Guamaní <- NULL #Elimino
HR$LosChillos <- NULL #Elimino
HR$SanAntonio <- NULL #Elimino
HR$Tumbaco <- NULL #Elimino
HR <-  HR %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
View(HR)


#Para Velocidad viento

Velo$Carapungo <- NULL #Elimino
Velo$Cotocollao <- NULL #Elimino
Velo$Guamani <- NULL #Elimino
Velo$LosChillos <- NULL #Elimino
Velo$SanAntonio <- NULL #Elimino
Velo$Tumbaco <- NULL #Elimino
Velo <-  Velo %>% 
  rename("Fecha"= "Fecha \\ Unidad")
Velo
#Para temperatura media

TMP$Carapungo <- NULL #Elimino
TMP$Cotocollao <- NULL #Elimino
TMP$Guamaní <- NULL #Elimino
TMP$LosChillos <- NULL #Elimino
TMP$SanAntonio <- NULL #Elimino
TMP$Tumbaco <- NULL #Elimino
TMP$...11<- NULL #Elimino
TMP$...12<- NULL #Elimino
TMP$...13<- NULL #Elimino
TMP$...14<- NULL #Elimino
TMP <-  TMP %>% 
  rename("Fecha"= "Fecha \\ Unidad")
TMP

#Para lluvia o precipitación

PPM$Carapungo <- NULL #Elimino
PPM$Cotocollao <- NULL #Elimino
PPM$Guamaní <- NULL #Elimino
PPM$LosChillos <- NULL #Elimino
PPM$`San Antonio` <- NULL #Elimino
PPM$Tumbaco <- NULL #Elimino

PPM <-  PPM %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")


#Para Radiación Solar

RS$Carapungo <- NULL #Elimino
RS$Cotocollao <- NULL #Elimino
RS$Guamaní <- NULL #Elimino
RS$LosChillos <- NULL #Elimino
RS$SanAntonio <- NULL #Elimino
RS$Tumbaco <- NULL #Elimino

RS <-  RS %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
RS

#Para CO

CO$CARAPUNGO <- NULL #Elimino
CO$COTOCOLLAO <- NULL #Elimino
CO$GUAMANI<- NULL #Elimino
CO$`LOS CHILLOS` <- NULL #Elimino
CO$TUMBACO <- NULL #Elimino
CO$CONDADO<- NULL #Elimino
CO$TURUBAMBA<- NULL #Elimino
CO$CHILLOGALLO<- NULL #Elimino

CO <-  CO %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
CO

#Para NO2

NO2$CARAPUNGO<- NULL #Elimino
NO2$COTOCOLLAO <- NULL #Elimino
NO2$GUAMANI<- NULL #Elimino
NO2$`LOS CHILLOS`<- NULL #Elimino
NO2$TUMBACO<- NULL #Elimino
NO2$CONDADO <- NULL #Elimino
NO2$TURUBAMBA <- NULL #Elimino
NO2$CHILLOGALLO <- NULL #Elimino
NO2 <-  NO2 %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
NO2

#Para O3

O3$Carapungo <- NULL #Elimino
O3$Cotocollao <- NULL #Elimino
O3$Guamaní <- NULL #Elimino
O3$LosChillos <- NULL #Elimino
O3$`SAN ANTONIO` <- NULL #Elimino
O3$Tumbaco <- NULL #Elimino

O3 <-  O3 %>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
O3

#Para PM25

PM25$CARAPUNGO <- NULL #Elimino
PM25$COTOCOLLAO <- NULL #Elimino
PM25$GUAMANI <- NULL #Elimino
PM25$`LOS CHILLOS` <- NULL #Elimino
PM25$`SAN ANTONIO` <- NULL #Elimino
PM25$TUMBACO<- NULL #Elimino
PM25$CONDADO<- NULL #Elimino
PM25$TURUBAMBA<- NULL #Elimino
PM25$CHILLOGALLO<- NULL #Elimino

PM25 <-  PM25%>% 
  rename("Fecha"= "FECHA \\ UNIDAD")
PM25

#Para SO2

SO2$CARAPUNGO <- NULL #Elimino
SO2$COTOCOLLAO <- NULL #Elimino
SO2$GUAMANI <- NULL #Elimino
SO2$`LOS CHILLOS` <- NULL #Elimino
SO2$TUMBACO <- NULL #Elimino
SO2$CONDADO <- NULL #Elimino
SO2$TURUBAMBA <- NULL #Elimino
SO2$CHILLOGALLO <- NULL #Elimino

SO2 <-  SO2 %>% 
  rename("Fecha"= "Fecha \\ Unidad")
SO2

#### Arreglo de datos ####

DIR <- DIR %>% 
  gather(Sector, Dir_wind,"Belisario","Centro","Camal")
View(DIR)

#Para velocidad``

Velo <- Velo %>% 
  gather(Sector, Vel_wind, "Belisario","Centro","Camal")
View(Velo)

#Para temperatura``

TMP <- TMP %>% 
  gather(Sector, Temperatura, "Belisario","Centro","Camal")
View(TMP)

#Para Humedad relativa``

HR <- HR %>% 
  gather(Sector, HumRel, "Belisario","Centro","Camal")
View(HR)

#Para Precipitación

PPM <- PPM %>% 
  gather(Sector, Lluvia, "Belisario","Centro","Camal")
View(PPM)

#Para Radiación Solar

RS <- RS %>% 
  gather(Sector, RadSol, "Belisario","Centro","Camal")
View(RS)

#Para CO

CO <- CO %>% 
  gather(Sector, CO, "Belisario","Centro","Camal")
View(CO)

#Para NO2

NO2 <- NO2 %>% 
  gather(Sector, NO2, "Belisario","Centro","Camal")
View(NO2)

#Para O3

O3 <- O3 %>% 
  gather(Sector, O3, "Belisario","Centro","Camal")
View(O3)

#Para PM25

PM25 <- PM25 %>% 
  gather(Sector, PM25, "Belisario","Centro","Camal")
View(PM25)

#Para SO2

SO2 <- SO2 %>% 
  gather(Sector, SO2, "Belisario","Centro","Camal")
View(SO2)



#### Unión de data frames ####
library(tidyverse)
library(dplyr)

Prueba <- CO %>% 
  inner_join(NO2,by=c("Fecha","Sector"))


AireQuito <- Prueba %>% 
  inner_join(O3,by=c("Fecha","Sector"))

AireQuito <- AireQuito %>% 
  inner_join(PM25,by=c("Fecha","Sector"))
AireQuito

AireQuito <- AireQuito %>% 
  inner_join(SO2,by=c("Fecha","Sector"))
AireQuito

AireQuito <- AireQuito %>% 
  inner_join(DIR,by=c("Fecha","Sector"))

AireQuito <- AireQuito %>% 
  inner_join(Velo,by=c("Fecha","Sector"))

AireQuito <- AireQuito %>% 
  inner_join(TMP,by=c("Fecha","Sector"))


AireQuito <- AireQuito %>% 
  inner_join(HR,by=c("Fecha","Sector"))


AireQuito <- AireQuito %>% 
  inner_join(PPM,by=c("Fecha","Sector"))


AireQuito <- AireQuito %>% 
  inner_join(RS,by=c("Fecha","Sector"))
AireQuito

colnames(AireQuito)

#Exportar a Excel

library(rio)
export(AireQuito, "AireQuito.xlsx")








library(readxl)
dane1 <- read_excel("Tobacco_consumption_HLTH2022-2.xlsx",sheet = "T1",)
OdsetekPalacy <- dane1[, 1:2]
OdsetekPalacy <- OdsetekPalacy[order(OdsetekPalacy[[1]]), ]
dane2 <- read_excel("Europe_Population.xlsx")
PopulacjaKraju<- dane2[, 1:2]
PopulacjaKraju<- PopulacjaKraju[order(PopulacjaKraju[[1]]), ]


library(readxl)
dane1 <- read_excel("Tobacco_consumption_HLTH2022-2.xlsx",sheet = "T1",)
OdsetekPalacy <- dane1[, 1:2]
OdsetekPalacy <- OdsetekPalacy[order(OdsetekPalacy[[1]]), ]
dane2 <- read_excel("Europe_Population.xlsx")
PopulacjaKraju<- dane2[, 1:2]
PopulacjaKraju<- PopulacjaKraju[order(PopulacjaKraju[[1]]), ]
dane3 <- read_excel("europe_gdp_per_capita_2019.xlsx")
PKBKraju <- dane3[, 1:2]
dane4 <- read_excel("Direct-taxes.xlsx", skip = 5, col_names = FALSE, sheet="Table 13")
AkcyzaPKB <- dane4[, c(1,5)] #akcyza jako porcent pkb kraju
AkcyzaPKB <- AkcyzaPKB[1:(nrow(AkcyzaPKB) - 6), ]
Kraje=intersect(OdesetekPalcy$Country, )s


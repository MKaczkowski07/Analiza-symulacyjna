
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

colnames(AkcyzaPKB) <- c("Country", "ProcentPKB")
Kraje <- intersect(
  intersect(OdsetekPalacy$Country, PopulacjaKraju$Country),
  intersect(PKBKraju$Country, AkcyzaPKB$Country)
)

Kraje <- Reduce(intersect, list(
  OdsetekPalacy$Country,
  PopulacjaKraju$Country,
  PKBKraju$Country,
  AkcyzaPKB$Country
))


OdsetekPalacy<-OdsetekPalacy[OdsetekPalacy$Country %in% Kraje, ]
PopulacjaKraju <- PopulacjaKraju[PopulacjaKraju$Country %in% Kraje, ]
PKBKraju <- PKBKraju[PKBKraju$Country %in% Kraje, ]
AkcyzaPKB <- AkcyzaPKB[AkcyzaPKB$Country %in% Kraje, ]

OdsetekPalacy <- OdsetekPalacy[order(OdsetekPalacy$Country), ]
PopulacjaKraju <- PopulacjaKraju[order(PopulacjaKraju$Country), ]
PKBKraju <- PKBKraju[order(PKBKraju$Country), ]
AkcyzaPKB <- AkcyzaPKB[order(AkcyzaPKB$Country), ]

rm(dane1)
rm(dane2)
rm(dane3)
rm(dane4)

DanePolaczone <- data.frame(
  Country = OdsetekPalacy$Country,
  OdsetekPalacy = OdsetekPalacy[[2]],
  Populacja = PopulacjaKraju[[2]],
  PKB_per_capita = PKBKraju[[2]],
  Akcyza_procent_PKB = AkcyzaPKB[[2]]
)


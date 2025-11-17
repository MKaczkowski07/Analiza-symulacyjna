library(readxl)
library(dplyr)
library(ggplot2)
<<<<<<< HEAD
library(GGally)             #4
library(rnaturalearth)      #5
library(rnaturalearthdata)  #5
=======
library(GGally)
library(rnaturalearth)
library(rnaturalearthdata)
>>>>>>> 9eec4d198af78984b03c37d63bbb6bb8f014ef35

# ---- 1. Wczytanie danych ----
dane1 <- read_excel("Tobacco_consumption_HLTH2022-2.xlsx", sheet = "T1")
OdsetekPalacy <- dane1[, 1:2] %>%
  arrange(.[[1]]) %>%
  rename(Country = 1, Smokers = 2)

dane2 <- read_excel("Europe_Population.xlsx")
PopulacjaKraju <- dane2[, 1:2] %>%
  arrange(.[[1]]) %>%
  rename(Country = 1, Population = 2)

dane3 <- read_excel("europe_gdp_per_capita_2019.xlsx")
PKBKraju <- dane3[, 1:2] %>%
  rename(Country = 1, GDP_per_capita = 2)

dane4 <- read_excel("Direct-taxes.xlsx", skip = 5, col_names = FALSE, sheet = "Table 13")
AkcyzaPKB <- dane4[, c(1, 5)]
AkcyzaPKB <- AkcyzaPKB[1:(nrow(AkcyzaPKB) - 6), ]
names(AkcyzaPKB) <- c("Country", "Excise_share_GDP")

dane5 <- read_excel("hlth_sha11_hf$defaultview_spreadsheet.xlsx", sheet="Sheet 1", col_names = FALSE, skip = 18)
WydatkiMedyczne <- dane5[, c(1,10)] %>%
  rename(Country = 1, Wydatki_na_medycyne = 2)

library(rvest)
library(dplyr)
url <- "https://taxfoundation.org/data/all/eu/cigarette-tax-europe-2019/"
page <- read_html(url)
dane6 <- page %>% html_table(fill = TRUE)
length(dane6)
Akcyza <- dane6[[1]]
Akcyza <- Akcyza[-c(1:3, nrow(Akcyza)), ]
Akcyza <- Akcyza[,c(1:2)]
names(Akcyza) <- c("Country", "Akcyza")
Akcyza$Country <- sub(" .*", "", Akcyza$Country)
Akcyza$Akcyza <- gsub("[^0-9.]", "", Akcyza$Akcyza)
Akcyza$Akcyza <- as.numeric(Akcyza$Akcyza)




# ---- 2. Ujednolicenie krajów i połączenie ----
kraje <- Reduce(intersect, list(OdsetekPalacy$Country, PopulacjaKraju$Country, PKBKraju$Country, Akcyza$Country))

dane_final <- OdsetekPalacy %>%
  filter(Country %in% kraje) %>%
  left_join(PopulacjaKraju, by = "Country") %>%
  left_join(PKBKraju, by = "Country") %>%
  left_join(Akcyza, by = "Country") %>%
  left_join(WydatkiMedyczne, by = "Country")


head(dane_final)
rm(dane1,dane2,dane3,dane4,dane5,dane6)
# ==============================================================================

#1. Korelacja: akcyza – PKB per capita
cor1 <- cor(dane_final$Akcyza, dane_final$GDP_per_capita, use="complete.obs")
cat("Korelacja (akcyza vs PKB per capita):", round(cor1, 3), "\n")

ggplot(dane_final, aes(x = GDP_per_capita, y = Akcyza)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste0("Akcyza a PKB per capita (r = ", round(cor1, 2), ")"),
       x = "PKB per capita (USD)", y = "Udział akcyzy w PKB (%)") +
  theme_minimal()


#2. Korelacja: akcyza – odsetek palaczy
ggplot(dane_final, aes(x = Akcyza, y = Smokers)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Czy akcyza wpływa na poziom palenia?",
       x = "Udział akcyzy w PKB (%)",
       y = "Odsetek palących (%)")

#3. Korelacja: PKB per capita – odsetek palaczy
cor3 <- cor(dane_final$GDP_per_capita, dane_final$Smokers, use="complete.obs")
cat("Korelacja (PKB per capita vs palenie):", round(cor3, 3), "\n")

ggplot(dane_final, aes(x = GDP_per_capita, y = Smokers)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste0("Zamożność a palenie (r = ", round(cor3, 2), ")"),
       x = "PKB per capita (USD)", y = "Odsetek palących (%)") +
  theme_minimal()

#4 Korelacja Wydatki na medycyne na obywatela a odsetek palaczy
dane_final$Wydatki_na_medycyne <- as.numeric(dane_final$Wydatki_na_medycyne)
cor4 <- cor((dane_final$Wydatki_na_medycyne/dane_final$Population), dane_final$Smokers, use = "complete.obs")
cor4

#5 Korelacja Wydatki na medycyne na obywatela a odsetek palaczy
dane_final$Wydatki_na_medycyne <- as.numeric(dane_final$Wydatki_na_medycyne)
cor5 <- cor(dane_final$GDP_per_capita,(dane_final$Wydatki_na_medycyne/dane_final$Population), use = "complete.obs")
cor5


dane_final$Wydatki_na_medycyne <- as.numeric(dane_final$Wydatki_na_medycyne)
cor5 <- cor(dane_final$GDP_per_capita,(dane_final$Wydatki_na_medycyne/dane_final$Population), use = "complete.obs")
cor5
cor2 <- cor(dane_final$Akcyza, dane_final$Smokers, use = "complete.obs")

# Zbiorczo
wyniki_korelacji <- data.frame(
  Zmienna_X = c("Akcyza", "Akcyza", "PKB per capita"),
  Zmienna_Y = c("PKB per capita", "Odsetek palących", "Odsetek palących"),
  Korelacja = round(c(cor1, cor2, cor3), 3)
)
print(wyniki_korelacji)


#.4 Anliza wieloraka
model <- lm(Smokers ~ GDP_per_capita + Akcyza, data = dane_final)
summary(model)

<<<<<<< HEAD
ggpairs(dane_final[, c("Smokers","GDP_per_capita","Excise_share_GDP")])
=======
ggpairs(dane_final[, c("Smokers","GDP_per_capita","Akcyza")])
>>>>>>> 9eec4d198af78984b03c37d63bbb6bb8f014ef35


#5
mapa <- ne_countries(continent = "Europe", returnclass = "sf")

dane_map <- merge(mapa, dane_final, by.x="name", by.y="Country", all.x=FALSE)
ggplot(dane_map) +
  geom_sf(aes(fill = Smokers)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Odsetek palących w krajach Europy", fill = "% palących")


#5A Mapa odsetka palących:
dane_map2 <- merge(mapa, dane_final, by.x = "name", by.y = "Country")

#5B Mapa PKB per capita
ggplot(dane_map2) +
  geom_sf(aes(fill = GDP_per_capita)) +
  scale_fill_viridis_c(option = "B", trans = "log") +
  labs(title = "PKB per capita w Europie", fill = "PKB per capita (log)") +
  theme_minimal()

#5C Mapa różnice regionalne akcyzy
ggplot(dane_map2) +
  geom_sf(aes(fill = Akcyza)) +
  scale_fill_viridis_c(option = "A") +
  labs(title = "Udział akcyzy w PKB (%)", fill = "Akcyza % PKB") +
  theme_minimal()


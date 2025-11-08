
library(readxl)
dane1 <- read_excel("Tobacco_consumption_HLTH2022-2.xlsx",sheet = "T1",)
OdsetekPalacy <- dane1[, 1:2]



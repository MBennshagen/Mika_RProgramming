install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("glm2")

library(dplyr)
library(ggplot2)
library(readxl)
library(glm2)

sökväg_transport <- "nationell_arstatistik.xlsx"
sökväg_scb <- "SCB_procent.xlsx"

data_transport <- read_excel(sökväg_transport, sheet = "omkomna_data")
data_scb <- read_excel(sökväg_scb, sheet = "scb")


#Linjär regressionsmodell - Transportstyrelsen
modell_transport <- lm(omkomna ~ år, data = data_transport)
summary(modell_transport)
#Linjär regressionsmodell - SCB
modell_scb <- lm(riskkonsumenter ~ år, data = data_scb)
summary(modell_scb)

#Logistisk regression - Transportstyrelsen
modell_transport_logistik <- glm(omkomna_binär ~ år, data = data_transport, family = binomial)
summary(modell_transport_logistik)
#Logistisk regression - SCB
modell_scb_logistik <- glm(riskkonsumenter_binär ~ år, data = data_scb, family = binomial)
summary(modell_scb_logistik)

library(readr)
library(dplyr)
library(ps)

source("preparacion_base_datos.R")

####### VERIFICAR CORTE #######
table(eventos$state,useNA = "ifany")
table(eventos$event_type, useNA = "ifany")
table(eventos$year, useNA = "ifany")

eventos <- eventos %>%
  filter(event_type %in% c("Hail", "Thundereventos Wind"),
         state %in% c("TEXAS", "OKLAHOMA", "KANSAS"),
         year >= 2018, year <= 2023) %>%
  registrar("Recorte de tipo, estado y años", "debería coincidir con el paquete")


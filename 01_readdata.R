pacman::pc_load("arrow","googlesheets4","dplyr",
                    "tidyverse","lubridate")

### ID Equivalance
lk <- "https://docs.google.com/spreadsheets/d/1195oJCW_rj1jxiGj2Pb4o8VKSZ717HA7E1GhYoVwlqI/edit?usp=sharing"
ID <- range_read(lk, sheet = "Equivalencia_ID")

### Read precios
f <- read_parquet("SIPSA_Precios.parquet")
f$fechaCaptura <- as.Date(f$fechaCaptura)
f <- f[,c("ciudad","fechaCaptura","producto","precioPromedio")]
f$mes_año <- floor_date(f$fechaCaptura, "month")

# Calcular el promedio mensual de la variable
I <- merge(f,ID,by.y="SIPSA_PRECIOS",by.x="producto")

sipsa_mensual <- I %>% group_by(ciudad,TCAC_ID,mes_año) %>%
                    summarise(promedio = mean(precioPromedio, na.rm = TRUE))







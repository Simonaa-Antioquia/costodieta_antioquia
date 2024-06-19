## Check data
source("aux_head.R")

# <--- Open SIPSA --->
    sipsa <- fix_sipsa_uni(readRDS("output/sipsa_dieta_antioquia.rds"))  %>%
                left_join(IPC) %>%
                mutate(precio_cons=(precio/ipc)*100)

# <--- CoCA ---> Only un balor
    ds_food <- create_data_food(1,2023,sipsa)$dt_mean_r
    data(EER, package = "Foodprice")
    CoCA(data = ds_food, EER=EER)

# Manual Check
    

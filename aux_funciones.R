##### Update data in google sheets
########################################

limpiar_texto <- function(cadena) {
  # Eliminar espacios dobles
  cadena <- str_replace_all(cadena, " {2,}", " ")

  # Convertir a minúsculas
  cadena <- tolower(cadena)

  # Eliminar espacios al principio y al final de la cadena
  cadena <- str_trim(cadena)

  return(cadena)
}

##### Create Bases de datos - por tipo markap
########################################

create_food_input <- function(Markup,dt,TCAC_dt) {

    ### Merge informacion de alimentos
    dt$Food <- dt$alimento
    dt <- dt[!duplicated(dt$TCAC),]
    dt <- dt %>% inner_join(TCAC_dt)
      
    ### Merge markup
    dt <- merge(dt,Markup,by=c("Group","Subgroup"))
    dt <- dt[!duplicated(dt$TCAC),] ## CHECK why repeat
    dt <- dt[!is.na(dt$Serving),]

    ## Precios Corrientes --> precio x kg * markup * 0.1 (conversion a 100g)
    dt_min_c <- dt %>% mutate(Price_100g=precio_corr*(1+(markup_min/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving)) %>%
                       drop_na(Price_100g)
    dt_mean_c <- dt %>% mutate(Price_100g=precio_corr*(1+(markup_mean/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving)) %>%
                        drop_na(Price_100g)
    dt_max_c <- dt %>% mutate(Price_100g=precio_corr*(1+(markup_max/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving)) %>%
                        drop_na(Price_100g)

    ## Precios Reales
    dt_min_r <- dt %>%  mutate(Price_100g=precio_cons*(1+(markup_min/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving)) %>%
                        drop_na(Price_100g)
    dt_mean_r <- dt %>% mutate(Price_100g=precio_cons*(1+(markup_mean/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving)) %>%
                        drop_na(Price_100g)
    dt_max_r <- dt %>% mutate(Price_100g=precio_cons*(1+(markup_max/100))*0.1,
                              Price_serving=Price_100g*(Serving_g/Serving))%>%
                        drop_na(Price_100g)

    ## Keep structue
    dt_min_c=dt_min_c[,names(data_example)]
    dt_mean_c=dt_mean_c[,names(data_example)]
    dt_max_c=dt_max_c[,names(data_example)]
    dt_min_r=dt_min_r[,names(data_example)]
    dt_mean_r=dt_mean_r[,names(data_example)]
    dt_max_r=dt_max_r[,names(data_example)]

    return(list(dt_min_c = dt_min_c, dt_mean_c = dt_mean_c, dt_max_c = dt_max_c, dt_min_r = dt_min_r, dt_mean_r = dt_mean_r, dt_max_r = dt_max_r))
}


##### Fix - SIPSA - Unidades diferentes
########################################

fix_sipsa_uni <- function(sipsa){
  
    ### fix 
    sipsa <- sipsa %>% mutate(precio=ifelse(
                # (p1) Aceite vegetal mezcla: PrecioSIPSA*(100/920)
                SIPSA_P_ID %in% c("P1","P6","P5"),(precio*(100/920))/0.1,
                # Huevo A: PrecioSIPSA*(100/50)
                ifelse(SIPSA_P_ID %in% c("P126","P88","P122","P123"),(precio*(100/50))/0.1,
                # Huevo AA: PrecioSIPSA*(100/60)
                ifelse(SIPSA_P_ID %in% c("P125","P146"),(precio*(100/60))/0.1,
                # Huevo AAA: PrecioSIPSA*(100/67)
                ifelse(SIPSA_P_ID %in% c("P124","P121","P124"),(precio*(100/67))/0.1,
                # Otros
                precio)))))

    ### return 
    return(sipsa)
}


#### CoCA 01/03 -- cost day of Caloric Adequacy
########################################

create_CoCA_year_m <- function(mt,an,sipsa) {

  dt <- sipsa %>% filter(ano==an & mes==mt) 
  dt <- dt[,c("SIPSA_P_ID","alimento","precio","precio_cons")]
  dt <- merge(dt,sipsa_p_c,by="SIPSA_P_ID")
  ### make sure we only have one food
  dt <- dt %>% group_by(TCAC,alimento) %>% 
    summarise(precio_corr=mean(precio),
              precio_cons=mean(precio_cons))
  
  data_food <- create_food_input(Markup,dt,TCAC_dt)
  
  ds <- create_CoCA(data_food)
  
  ds$year <- an
  ds$mt <- mt
  
  return(ds)
}


##### CoCA 02/03 -- cost day of Caloric Adequacy
########################################

create_CoCA <- function(data_food) {

  nams_p <- c("cons_min","cons_max","cons_mean","corr_min","corr_max","corr_min")
  data(EER, package = "Foodprice")
  
  ds <- do.call(rbind,lapply(nams_p,get_CoCA,data_food=data_food,EER=EER))
  
  return(ds)
}  
  
##### CoCA 03/03 --  Calculate Coca
########################################

get_CoCA <- function(i,data_food,EER) {
  nams_ds <- list("cons_min"="dt_min_r",
                  "cons_max"="dt_max_r",
                  "cons_mean"="dt_mean_r",
                  "corr_min"="dt_min_c",
                  "corr_max"="dt_max_c",
                  "corr_mean"="dt_mean_c")
  ds_nm <- eval(parse(text=paste0("nams_ds$",i)))
  d <- eval(parse(text=paste0("data_food$",ds_nm)))
  out <- CoCA(data = d, EER=EER)$cost
  out$price_type <- i
  return(out)
}
  # Precios corrientes: min(dt_min_c), mean(dt_mean_c),max(dt_max_c)
  # Precios reales: min(dt_min_r), mean(dt_mean_r),max(dt_max_r)

########################################
##### CoNA 01/03 -- cost day of Caloric Adequacy
########################################

create_CoNA_year_m <- function(mt,an,sipsa) {
  
  dt <- sipsa %>% filter(ano==an & mes==mt) 
  dt <- dt[,c("SIPSA_P_ID","alimento","precio","precio_cons")]
  dt <- merge(dt,sipsa_p_c,by="SIPSA_P_ID")
  ### make sure we only have one food
  dt <- dt %>% group_by(TCAC,alimento) %>% 
    summarise(precio_corr=mean(precio),
              precio_cons=mean(precio_cons))
  
  data_food <- create_food_input(Markup,dt,TCAC_dt)
  
  ds <- create_CoNA(data_food)
  
  ds$year <- an
  ds$mt <- mt
  
  return(ds)
}

########################################
##### CoNA 02/03 -- cost day of Caloric Adequacy
########################################

create_CoNA <- function(data_food) {
  
  nams_p <- c("cons_min","cons_max","cons_mean","corr_min","corr_max","corr_min")
  data(EER_LL, package = "Foodprice")
  data(UL, package = "Foodprice")
  
  ds <- do.call(rbind,lapply(nams_p,get_CoNA,data_food=data_food,
                             EER_LL=EER_LL,UL=UL))
  
  return(ds)
}  

########################################
##### CoNA 03/03 --  Calculate Coca
########################################

get_CoNA <- function(i,data_food,EER_LL,UL) {
  nams_ds <- list("cons_min"="dt_min_r",
                  "cons_max"="dt_max_r",
                  "cons_mean"="dt_mean_r",
                  "corr_min"="dt_min_c",
                  "corr_max"="dt_max_c",
                  "corr_mean"="dt_mean_c")
  ds_nm <- eval(parse(text=paste0("nams_ds$",i)))
  d <- eval(parse(text=paste0("data_food$",ds_nm)))
  out <- CoNA(data = d ,EER_LL=EER_LL,UL=UL)$cost
    
  out$price_type <- i
  return(out)
}
# Precios corrientes: min(dt_min_c), mean(dt_mean_c),max(dt_max_c)
# Precios reales: min(dt_min_r), mean(dt_mean_r),max(dt_max_r)
  

########################################
##### CoRD 01/03 -- cost day of Caloric Adequacy
########################################

create_CoRD_year_m <- function(mt,an,sipsa) {
  
  dt <- sipsa %>% filter(ano==an & mes==mt) 
  dt <- dt[,c("SIPSA_P_ID","alimento","precio","precio_cons")]
  dt <- merge(dt,sipsa_p_c,by="SIPSA_P_ID")
  ### make sure we only have one food
  dt <- dt %>% group_by(TCAC,alimento) %>% 
    summarise(precio_corr=mean(precio),
              precio_cons=mean(precio_cons))
  
  data_food <- create_food_input(Markup,dt,TCAC_dt)
  
  ds <- create_CoRD(data_food)
  
  ds$year <- an
  ds$mt <- mt
  
  return(ds)
}

########################################
##### CoRD 02/03 -- cost day of Caloric Adequacy
########################################

create_CoRD <- function(data_food) {
  
  nams_p <- c("cons_min","cons_max","cons_mean","corr_min","corr_max","corr_mean")
  data(diverse, package = "Foodprice")
  data(serv, package = "Foodprice")
  
  ds <- do.call(rbind,lapply(nams_p,get_CoRD,data_food=data_food,
                             diverse =diverse,serv = serv))
  
  return(ds)
}  

########################################
##### CoRD 03/03 --  Calculate Coca
########################################

get_CoRD <- function(i,data_food,diverse,serv) {
  nams_ds <- list("cons_min"="dt_min_r",
                  "cons_max"="dt_max_r",
                  "cons_mean"="dt_mean_r",
                  "corr_min"="dt_min_c",
                  "corr_max"="dt_max_c",
                  "corr_mean"="dt_mean_c")
  ds_nm <- eval(parse(text=paste0("nams_ds$",i)))
  d <- eval(parse(text=paste0("data_food$",ds_nm)))
  out <- CoRD(data = d ,diverse = diverse ,serv = serv)$cost
  
  out$price_type <- i
  return(out)
}
# Precios corrientes: min(dt_min_c), mean(dt_mean_c),max(dt_max_c)
# Precios reales: min(dt_min_r), mean(dt_mean_r),max(dt_max_r)

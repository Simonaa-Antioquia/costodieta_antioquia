## Check data
source("aux_head.R")

# <--- Open SIPSA --->
    sipsa <- fix_sipsa_uni(readRDS("output/sipsa_dieta_antioquia.rds"))  %>%
                left_join(IPC) %>%
                mutate(precio_cons=(precio/ipc)*100)

# <--- CoCA ---> Only un balor
    data <- create_data_food(1,2023,sipsa)$dt_mean_r
    data(EER, package = "Foodprice")
    CoCA(data = data, EER=EER)

# <--- Manual Check --->
    EER_S <- EER[EER$Sex==0 & EER$Age=="14 a 18 años",]

      CoCA = lp(direction = "min",
                objective.in = Precio,
                const.mat = Coef.Restriq,
                const.dir = c("="),
                const.rhs = Limitaciones[1],
                compute.sens = TRUE)
    # ---Asignación de vectores al modelo
    Precio = data$Price_100g
    Food=data$Food
    Age=EER_S$Age
    
    # MAtriz de coef de restricción al modelo (ENERGIA)
    Coef.Restriq = matrix(as.vector(data$Energy), ncol = length(Food))
    
    # Vector de limitaciones del modelo
    Limitaciones=EER_S$Energy

     # Vector de limitaciones del modelo
    Limitaciones=EER_S$Energy
    
    # ---Optimización
     CoCA = lpSolve::lp(direction = "min",
                    objective.in = Precio,
                    const.mat = Coef.Restriq,
                    const.dir = c("="),
                    const.rhs = Limitaciones[1])
      
    CoCA$solution

      linprog::solveLP(  cvec = Precio,
                bvec = Limitaciones[1],
                Amat = Coef.Restriq,
                maximum = FALSE,
                const.dir = c("="))


            

# Now run
results <- lp(direction = "max",
objective.in = f.obj,
const.mat = f.con,
const.dir = f.dir,
const.rhs = f.rhs,
all.int = T
)

solveLP(cvec = f.obj,
bvec = f.rhs,
Amat = f.con,
maximum = TRUE,
const.dir = f.dir)
# Laboratorio 2 Análisis de Datos

# Integrantes: 
# Angel Avendaño 
# Rodrigo Escobar


# Importar paquetes.
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(ez)){
  install.packages("ez",dependencies = TRUE)
  require(ez)
}
if(!require(nlme)){
  install.packages("nlme",dependencies = TRUE)
  require(nlme)
}
if(!require(emmeans)){
  install.packages("emmeans",dependencies = TRUE)
  require(emmeans)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if (!require(boot)){
  install.packages("boot", dependencies = TRUE )
  require (boot)
}
library(dplyr)
library(tidyverse)
library(pROC)
library(caret)
library(leaps)
library(car)
library(mice)
library(MASS)
library(caret)

# Importar datos
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# Copia de la BD original
poblacion2 <- poblacion

# Se verifica la existencia de Nan´s
sum(is.na(poblacion))

############## REDUCCIÓN DE DIMENSIONALIDAD ############## 

# Conteo de variable pdays
sum(poblacion$pdays == '999')

# Se elimina la variable pdays
poblacion2 <- dplyr::select(poblacion2, -pdays)

# Se cuentan las apariciones de la categoria noexistent en poutcome
sum(poblacion$poutcome == 'nonexistent')


############## MANEJO DE MISSINGS ############## 

# Saber la cantidad de unknown en un vector de cada variable
# NOTA: SON TODOS CATEGÓRICOS
unknown_count <- colSums(poblacion2 == 'unknown')
unknown_count

# 330 en job
# 80 en marital
# 1731 en education
# 8597 default 
# 990 housing
# 990 en loan

# Se quitan los unknowns dado el tamaño de la data y el poco efecto que estos tienen al realizar
# cluster por k-means
poblacion2 <- subset(poblacion2, poblacion2$job != "unknown")
poblacion2 <- subset(poblacion2, poblacion2$marital != "unknown")
poblacion2 <- subset(poblacion2, poblacion2$education != "unknown")
poblacion2 <- subset(poblacion2, poblacion2$default != "unknown")
poblacion2 <- subset(poblacion2, poblacion2$housing != "unknown")
poblacion2 <- subset(poblacion2, poblacion2$loan != "unknown")


################################## PRUEBAS CHI CUADRADO ##################################
library(stats)

# Se obtiene la lista de nombres de las variables categoricas en el dataframe

cat_vars <-  c('job','marital','education','housing',
               'loan','contact','previous','poutcome','y')

# Se obtiene la lista de nombres de las variables numéricas en el dataframe
var_cont <- c('duration',"cons.price.idx","cons.conf.idx","euribor3m",'emp.var.rate')

# Se crea un ciclo para comparar cada par de variables con la prueba chi cuadrado
chi_test <- function(data,nombres_variables ){
  
                  for (i in 1:(length(nombres_variables)-1)) {
                    for (j in (i+1):length(nombres_variables)) {
                      variable1 <- nombres_variables[i]
                      variable2 <- nombres_variables[j]
                      
                      # Realizar la prueba chi cuadrado entre las variables
                      resultado_chi <- chisq.test(data[[variable1]], data[[variable2]])
                      
                      # Imprimir el nombre de las variables si el valor p es muy bajo (por ejemplo, menor que 0.05)
                      if (resultado_chi$p.value < 0.05) {
                        cat(variable1, "-", variable2, "pvalue: ", resultado_chi$p.value, "\n")
                      }
                    }
                  }
                }

# Se extran solo las variables categóricas del dataframe.
poblacion_cat <- poblacion2[cat_vars]

# Se realiza la prueba chi cuadrado al dataframe con solo variables categóricas.
chi_test(poblacion_cat,cat_vars)


# Muestreo aleatorio estratificado para generar clusters en k-means
# se utiliza jobs dado que se elimina la categoria illiterate debido a que es muy poco representativa en lada
# data, por lo tanto tiene una probabilidad de ocurrencia en el muestro bajo p(illiterate)=0.0003607977 .
# Lo mismo para las categorias 6 y 7 de la variable previous
prop.table(table(poblacion2$education))
poblacion2 <- subset(poblacion2, poblacion2$education != "illiterate")
poblacion2 <- subset(poblacion2, poblacion2$previous != 5)
poblacion2 <- subset(poblacion2, poblacion2$previous != 6)
poblacion2 <- subset(poblacion2, poblacion2$previous != 7)
poblacion2 <- subset(poblacion2, poblacion2$emp.var.rate != -0.2)

# nrow(poblacion2)
# colSums(poblacion2 == 'unknown')


################ GENERACIÓN DE MUESTRAS ################ 
# Muestreo al 5 % de los datos estatificados
set.seed(133)  
strat_sample <- poblacion2 %>%
                  group_by(loan) %>%
                  sample_frac(size=.05)
# nrow(strat_sample)

# Al comparar la data obtenida por el muestreo aleatorio estratificado y la original
# podemos apreciar que la porporciones de las  variables categoricas seleccionadas 
# se mantiene y por lo tanto, el muestreo es exitoso.

for (i in 1:(length(cat_vars))){
  variable <- cat_vars[i]
  print(paste("variable:", variable))
  print(abs(prop.table(table(strat_sample[[variable]])) - prop.table(table(poblacion2[[variable]]))))
}

# https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwj-lISnobz_AhUlIrkGHS32AYQQFnoECBgQAQ&url=https%3A%2F%2Fwww.kdd.org%2Fkdd2016%2Fpapers%2Ffiles%2Frpp0427-dos-reisA.pdf&usg=AOvVaw06HqzbQcTfTW7bEVuP0w9W

# En el caso de las variables continuas se aprecia que su distribucion no fue altamente alterada
# en ninguna de ellas, teniendo KS poco significativos, exepto por el caso de la variable duration
# que a pesar de tener un ks significativo, este es considerado muy bajo (0.09)

# Se transforman las variables continuas a integer
strat_sample2 <- strat_sample[c(var_cont,cat_vars)]

# Se convierten las variables continuas en integer
for (i in 1:(length(c(var_cont)))){
  variable <- var_cont[i]
  print(variable)
  strat_sample2[[variable]] <- as.integer(strat_sample2[[variable]])
}

# Se convierten las variables categoricas en numericas
for (i in 1:(length(cat_vars))){
  variable <- cat_vars[i]
  print(variable)
  strat_sample2[[variable]] <- as.integer(factor(strat_sample2[[variable]], levels =unique(strat_sample2[[variable]])))
}

# Se transforman las variables continuas en integer en poblacion2
for (i in 1:(length(c(var_cont)))){
  variable <- var_cont[i]
  print(variable)
  poblacion2[[variable]] <- as.integer(poblacion2[[variable]])
}


# Se realiza el calculo del KS
for (i in 1:(length(var_cont))){
  variable <- var_cont[i]
  print(paste("variable:", variable))
  print(ks.test(strat_sample2[[variable]],poblacion2[[variable]]))
  }

################ OBTENER K ÓPTIMO ################

# Se transforman las variables categóricas a binarias para evitar el warning
strat_sample2$housing <- factor(strat_sample2$housing)
strat_sample2$loan <- factor(strat_sample2$loan)
strat_sample2$contact <- factor(strat_sample2$contact)
strat_sample2$y <- factor(strat_sample2$y)

# Se calcula la distancia entre los datos con la distancia de gower
library(cluster)
distancia <- daisy(strat_sample2, metric = "gower")

######################## CALCULAR NUMERO DE CLUSTERES con el método silhouette ##########################
# Se calcula el ancho de silhouette para varios valores de k con PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(distancia,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Se grafica silhouette a lo ancho
plot(1:10, sil_width,
     xlab = "Cantidad de clusters",
     ylab = "Ancho de Silhouette")
lines(1:10, sil_width)

################## SEGMENTACIÓN EN 7 CLÚSTER ###########################
set.seed(3312)
library(Rtsne)
pam_fit <- pam(distancia,
               diss = TRUE,
               k = 3)
tsne_obj <- Rtsne(distancia, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = strat_sample2$job)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))





####################### Lab 3 #################################

###############################################################

library(arules)
library(igraph)
library(tidygraph)



# Generar las reglas de asociación
rules <- apriori(strat_sample2, parameter = list(support = 0.01, confidence = 0.5))

# Convertir las reglas en un grafo con las reglas como nodos
g <- associations2igraph(rules)

# Mostrar el grafo de reglas
plot(g)

# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)

# Mostrar el tidygraph
print(tg)

# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(rules)

# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)

# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)

# Guardar las reglas como un grafo en formato graphml
saveAsGraph(rules, "rules.graphml")

# Eliminar el archivo graphml generado
unlink("rules.graphml")























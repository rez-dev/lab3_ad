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
poblacion <- read.csv2("bank-additional-full.csv", encoding="utf8")

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

# Al comparar la data obtenida por el muestreo aleatorio estratificado y la original
# podemos apreciar que la porporciones de las  variables categoricas seleccionadas 
# se mantiene y por lo tanto, el muestreo es exitoso.

# for (i in 1:(length(cat_vars))){
#   variable <- cat_vars[i]
#   print(paste("variable:", variable))
#   print(abs(prop.table(table(strat_sample[[variable]])) - prop.table(table(poblacion2[[variable]]))))
# }
####################### Lab 3 #################################

###############################################################

library(arulesViz)
library(igraph)
library(tidygraph)

# Generar las reglas de asociación
rules <- apriori(poblacion2,parameter = list(supp=0.001, conf=0.8, minlen=3),
                 appearance =list(default='lhs', rhs='y=yes'),
                 control=list(verbose=FALSE))

inspect(rules[1:5])

rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:5])

subrules <- head(rules,5)
plot(subrules, method='graph', interactive=FALSE)



# # Convertir las reglas en un grafo con las reglas como nodos
# g <- associations2igraph(rules)
# 
# # Mostrar el grafo de reglas
# plot(g)
# 
# # Convertir el grafo en un tidygraph
# tg <- as_tbl_graph(g)
# 
# # Mostrar el tidygraph
# print(tg)
# 
# # Obtener los conjuntos de elementos generadores de las reglas
# itemsets <- generatingItemsets(rules)
# 
# # Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
# g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# 
# # Mostrar el grafo de conjuntos de elementos
# plot(g_itemsets, layout = layout_in_circle)
# 
# # Guardar las reglas como un grafo en formato graphml
# saveAsGraph(rules, "rules.graphml")
# 
# # Eliminar el archivo graphml generado
# unlink("rules.graphml")























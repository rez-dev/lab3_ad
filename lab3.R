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

####################### Lab 3 #################################

###############################################################

library(arulesViz)
library(igraph)
library(tidygraph)

str(strat_sample)

strat_sample$age <- cut(strat_sample$age, breaks = c(12, 30, 50, 70, Inf), labels = c('young', 'middle_age', 'senior_age', 'old'))
table(strat_sample$age)

strat_sample$job <- factor(strat_sample$job)
strat_sample$marital <- factor(strat_sample$marital)
strat_sample$education <- factor(strat_sample$education)
strat_sample$default <- factor(strat_sample$default)
strat_sample$housing <- factor(strat_sample$housing)
strat_sample$loan <- factor(strat_sample$loan)
strat_sample$contact <- factor(strat_sample$contact)
strat_sample$month <- factor(strat_sample$month)
strat_sample$day_of_week <- factor(strat_sample$day_of_week)
strat_sample$poutcome <- factor(strat_sample$poutcome)
strat_sample$y <- factor(strat_sample$y)


strat_sample$default <- dplyr::recode(strat_sample$default, no='default=no')
strat_sample$housing <- dplyr::recode(strat_sample$housing, no='housing=no',yes='housing=yes')
strat_sample$loan <- dplyr::recode(strat_sample$loan, no='loan=no',yes='loan=yes')


# Otra parte: discretizar para duration
max_duration <- max(strat_sample$duration)
min_duration <- min(strat_sample$duration)
bins <- 3
width <- (max_duration - min_duration)/bins
strat_sample$duration <- cut(strat_sample$duration, breaks = seq(min_duration, max_duration, width))
table(strat_sample$duration)
######################

# Discretizar para campaing
boxplot(strat_sample$campaign)
min_campaign <- min(strat_sample$campaign)
max_campaign <- max(strat_sample$campaign)
bins <- 3
width <- (max_campaign - min_campaign)/bins
strat_sample$campaign <- cut(strat_sample$campaign, breaks = seq(min_campaign, max_campaign, width))
table(strat_sample$campaign)
###################################

strat_sampler <- strat_sample
strat_sampler$emp.var.rate <- as.numeric(strat_sampler$emp.var.rate)
strat_sampler$cons.price.idx <- as.numeric(strat_sampler$cons.price.idx)
strat_sampler$cons.conf.idx <- as.numeric(strat_sampler$cons.conf.idx)
strat_sampler$euribor3m <- as.numeric(strat_sampler$euribor3m)
strat_sampler$nr.employed <- as.numeric(strat_sampler$nr.employed)


head(strat_sampler,10)
str(strat_sampler)
strat_sampler$previous <- factor(strat_sampler$previous)


##################################################

# cut the emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m and nr.employed into 3 equal bins
bins <- 3
#emp.var.rate
max_emp.var.rate <- max(strat_sampler$emp.var.rate)
min_emp.var.rate <- min(strat_sampler$emp.var.rate)
width <- (max_emp.var.rate - min_emp.var.rate)/bins
strat_sampler$emp.var.rate <- cut(strat_sampler$emp.var.rate, breaks= seq(min_emp.var.rate,max_emp.var.rate,width))

#cons.price.idx
max_cons.price.idx <- max(strat_sampler$cons.price.idx)
min_cons.price.idx <- min(strat_sampler$cons.price.idx)
width <- (max_cons.price.idx - min_cons.price.idx)/bins
strat_sampler$cons.price.idx <- cut(strat_sampler$cons.price.idx, breaks= seq(min_cons.price.idx,max_cons.price.idx,width))

#cons.conf.idx
max_cons.conf.idx <- max(strat_sampler$cons.conf.idx)
min_cons.conf.idx <- min(strat_sampler$cons.conf.idx)
width <- (max_cons.conf.idx - min_cons.conf.idx)/bins
strat_sampler$cons.conf.idx <- cut(strat_sampler$cons.conf.idx, breaks= seq(min_cons.conf.idx,max_cons.conf.idx,width))

# euribor3m
max_euribor3m <- max(strat_sampler$euribor3m)
min_euribor3m <- min(strat_sampler$euribor3m)
width <- (max_euribor3m - min_euribor3m)/bins
strat_sampler$euribor3m <- cut(strat_sampler$euribor3m, breaks= seq(min_euribor3m,max_euribor3m,width))

# nr.employed
max_nr.employed <- max(strat_sampler$nr.employed)
min_nr.employed <- min(strat_sampler$nr.employed)
width <- (max_nr.employed - min_nr.employed)/bins
strat_sampler$nr.employed <- cut(strat_sampler$nr.employed, breaks= seq(min_nr.employed,max_nr.employed,width))


# target variable : y
strat_sampler$y <- dplyr::recode(strat_sampler$y,yes='target=yes', no='target=no')

str(strat_sampler)

Final <- data.frame()
Final <- strat_sampler
str(Final)
###################################################



# Generar las reglas de asociación
rules <- apriori(strat_sampler,parameter = list(supp=0.001, conf=0.8, minlen=3),
                 appearance =list(default='lhs', rhs='y=target=yes'),
                 control=list(verbose=FALSE))


##################################################################
################## EJECUTAR SOLO POR SECCIONES ###################
##################################################################


################ Ordenar por confianza ###########################
inspect(rules[1:6])
rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:6])
subrules <- head(rules,6)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
#######################################################

################# Ordenar por soporte ##################
# sort rules b support
rules <- sort(rules, by='support', decreasing=TRUE)
inspect(rules[1:6])
subrules <- head(rules,6)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
#######################################################


################## Ordenar por lift ###################
# sort rules by lift
rules <- sort(rules, by='lift', decreasing=TRUE)
inspect(rules[1:6])
subrules <- head(rules,5)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
#######################################################


######################################################
################ Evaluar caso "no" ###################
######################################################


rules <- apriori(Tester, parameter=list(supp=0.001, conf=0.8, minlen=3),
                 appearance=list(default='lhs', rhs='y=target=no'),
                 control=list(verbose=FALSE))
summary(rules)

############ Ordenar por confianza ###################
# sort rules by confidence
rules <- sort(rules, by='confidence', decreasing=TRUE)
inspect(rules[1:6])
subrules <- head(rules,6)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
######################################################


########### Ordenar por soporte #########################
rules <- sort(rules, by='support', decreasing=TRUE)
inspect(rules[1:6])
subrules <- head(rules,6)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
########################################################

############ Oredenar por lift #########################
rules <- sort(rules, by='lift', decreasing=TRUE)
inspect(rules[1:6])
subrules <- head(rules,6)
plot(subrules, method='graph', interactive=FALSE)
# Proceso para generar el grafo 
g <- associations2igraph(subrules)
plot(g)
# Convertir el grafo en un tidygraph
tg <- as_tbl_graph(g)
# Mostrar el tidygraph
print(tg)
# Obtener los conjuntos de elementos generadores de las reglas
itemsets <- generatingItemsets(subrules)
# Convertir los conjuntos de elementos en un grafo con los conjuntos como aristas
g_itemsets <- associations2igraph(itemsets, associationsAsNodes = FALSE)
# Mostrar el grafo de conjuntos de elementos
plot(g_itemsets, layout = layout_in_circle)
# Guardar las reglas como un grafo en formato graphml
saveAsGraph(subrules, "subrules.graphml")
# Eliminar el archivo graphml generado
unlink("subrules.graphml")
########################################################





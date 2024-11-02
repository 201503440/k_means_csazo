library(arules)
library(dplyr)

data <- read.csv('C:\\Users\\Documents\\Censo2018\\db_csv_\\MIGRACION_BDP.csv', sep = ",")

reglas <- apriori(data, parameter = list(support=0.2, confidence=0.5))

inspect(reglas[0:100])

# Tarea K-means

cluster <- kmeans(data, centers=3)

library(ggplot2)

ggplot(data, aes(x=COD_MUNICIPIO, y=NUM_VIVIENDA, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=COD_MUNICIPIO, y= NUM_VIVIENDA), color="black", size=4, shape=17)+
  labs(title="COD_MUNICIPIO VRS NUM_VIVIENDA")+
  theme_minimal()

ggplot(data, aes(x=PEI3, y=PEI4, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=PEI3, y= PEI4), color="black", size=4, shape=17)+
  labs(title="PEI3 VRS PEI4")+
  theme_minimal()

ggplot(data, aes(x=PEI3, y=PEI5, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=PEI3, y= PEI5), color="black", size=4, shape=17)+
  labs(title="PEI3 VRS PEI5")+
  theme_minimal()

ggplot(data, aes(x=PEI4, y=PEI5, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=PEI4, y= PEI5), color="black", size=4, shape=17)+
  labs(title="PEI4 (Edad) VRS PEI5 (AñoSeFue)")+
  theme_minimal()

ggplot(data, aes(x=COD_MUNICIPIO, y=PEI5, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=COD_MUNICIPIO, y= PEI5), color="black", size=4, shape=17)+
  labs(title="COD_MUNICIPIO VRS PEI5")+
  theme_minimal()

ggplot(data, aes(x=COD_MUNICIPIO, y=PEI4, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=COD_MUNICIPIO, y= PEI4), color="black", size=4, shape=17)+
  labs(title="COD_MUNICIPIO VRS PEI4")+
  theme_minimal()

ggplot(data, aes(x=AREA, y=NUM_HOGAR, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=AREA, y= NUM_HOGAR), color="black", size=4, shape=17)+
  labs(title="AREA VRS NUM_HOGAR")+
  theme_minimal()

ggplot(data, aes(x=COD_MUNICIPIO, y=PEI3, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=COD_MUNICIPIO, y= PEI3), color="black", size=4, shape=17)+
  labs(title="COD_MUNICIPIO VRS PEI3")+
  theme_minimal()

ggplot(data, aes(x=AREA, y=NUM_VIVIENDA, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=AREA, y= NUM_VIVIENDA), color="black", size=4, shape=17)+
  labs(title="AREA VRS NUM_VIVIENDA")+
  theme_minimal()

ggplot(data, aes(x=AREA, y=PEI3, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_path(data=as.data.frame(cluster$centers), aes(x=AREA, y= PEI3), color="black", size=4, shape=17)+
  labs(title="AREA VRS SEXO")+
  theme_minimal()
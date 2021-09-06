

######################LIMPIEZA BASES SISTEMA DE INFORMACIÓN CULTURAL

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr) #leer csv

#Descarcar bases de datos de 
#https://sic.cultura.gob.mx/datos.php

#Seleccionar directorio de trabajo
#(donde esten guardadas todas las bases)
#setwd("E:/Mapeo cultural")


#creamos una lista de las diferentes bases de datos sobre espacios culturales
c_escultural<-c("auditorio", "biblioteca", "casa_artesania", "centro_cultural", "centro_desarrollo_indigena", "comp_cine",
                  "comp_cine", "galeria", "impresos", "institucion_cultural", "institucion_cultural_mun", "libreria", "museo",
                    "otra_bib", "teatro")



#bucle for para abrir todas las bases al mismo tiempo
for (i in c_escultural){
  nam<- paste("x_", i, sep="") #creamos los nombres de las bases de datos
  assign(nam, read_csv(paste("0_", i, "_directorio.csv", sep=""), #assing es una función que permite asignarle un valor a un objeto
                       locale = locale(encoding = "ISO-8859-1"))) #local encodig= ISO... corrige los acentos
}


View(x_biblioteca)


#Contar el numero de observaciones por entidad para cada base
for (i in c_escultural){
  y= i 
  nom<-paste(i,"_ent", sep="")
  base<-get(paste("x_",i, sep=""))
  assign(nom, base %>%
           count(estado_id)
  )
}
remove(base)


#Lista de estados con nombres
Estados<-x_auditorio %>%
            count(estado_id,nom_ent)
Estados<-Estados[,1:2]


#Nombrar variables de los distintos dataframe
names(auditorio_ent)[2]= "auditorios"
names(biblioteca_ent)[2]= "bibliotecas"
names(casa_artesania_ent)[2]= "casas  artesania"
names(centro_cultural_ent)[2]= "centros culturales"
names(centro_desarrollo_indigena_ent)[2]= "centros indigena"
names(comp_cine_ent)[2]= "comp cines"
names(galeria_ent)[2]= "galerias"
names(impresos_ent)[2]= "impresos"
names(institucion_cultural_ent)[2]= "inst cultural ent"
names(institucion_cultural_mun_ent)[2]= "inst cultural mun"
names(libreria_ent)[2]= "librerias"
names(museo_ent)[2]= "museos"
names(otra_bib_ent)[2]= "otras bib"
names(teatro_ent)[2]= "teatros"

#Extraer unicamente id de estados
Total_estados <- auditorio_ent[,1]

#Primera union de estados con auditorios
merge_cult <- merge(Total_estados, auditorio_ent, by= c("estado_id"))

#union de las demás bases
for (i in c_escultural){
  base<-get(paste(i,"_ent", sep=""))
  merge_cult <- merge(merge_cult, base, by= c("estado_id"), all.x=TRUE, all.y=FALSE)
}

#Agregar nombres de los estados
merge_cult <- merge(merge_cult, Estados, by= c("estado_id"))

#Ordenar variables de merge
merge_cult<- merge_cult[,c(-3)]
merge_cult<- merge_cult[,c(1,17,2:16)]


#Base obtenida de github
#Abrimos base de internet 2020
internet_censo2020 <- read_csv("viviendas_internet_censo2020.csv", 
                                locale = locale(encoding = "ISO-8859-1"))

#Extraer nombre de la entidad
internet_censo2020$nom_ent<-substring(internet_censo2020$`Entidad federativa`, first = 4)

#Extraer id de estados
internet_censo2020$estado_id<-as.integer(substring(internet_censo2020$`Entidad federativa`, first = 1, last=3))
names(internet_censo2020)

#Ordenar dataframe de internet
internet_censo2020<-internet_censo2020[,c(-1)]
internet_censo2020<-internet_censo2020[,c(10,11,1:9)]
internet_ent<-internet_censo2020[,c(2,4,5,8,9)]

#Unir merge cultrual con internet
merge_cult <- merge(merge_cult, internet_ent, by= c("estado_id"), all.x=TRUE, all.y=FALSE)

#transformar a ceros
merge_cult <- mutate_at(merge_cult, c(3:21), ~replace(., is.na(.),0))


#Guardar csv
write.csv(merge_cult,"E:/Mapeo cultural/Merge_cultural.csv",row.names = TRUE)




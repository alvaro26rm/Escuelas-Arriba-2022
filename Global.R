# Escuelas Arriba 2021
# Armado de base de datos

# Nota: Esta base servirá para nuestro ShinyApp más delante

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)


# Elimino todos los objetos
rm(list=ls())

# Fijo directorio
setwd("C:/Users/alvaro.romero/Escritorio/EA/EA")

# Cargo base de establecimientos a nivel nacional
eadata <- read_delim("data/matricula.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# De la base nacional, quedarme solamente con los niveles que abarca el programa
eadata <- eadata[which(eadata$COD_ENSE2==2 |
                         eadata$COD_ENSE2==4 |
                         eadata$COD_ENSE2==5 |
                         eadata$COD_ENSE2==7 ), ]

# Nos quedamos con los cursos y niveles que necesitamos
eadata <- eadata[which(eadata$COD_GRADO==3 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==4 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==5 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==6 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==7 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==8 & eadata$COD_ENSE==110 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==310 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==310 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==410 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==410 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==510 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==510 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==610 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==610 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==710 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==710 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==810 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==810 |
                         eadata$COD_GRADO==1 & eadata$COD_ENSE==910 |
                         eadata$COD_GRADO==2 & eadata$COD_ENSE==910 |
                         eadata$COD_ENSE==211 |
                         eadata$COD_ENSE==212 |
                         eadata$COD_ENSE==213 |
                         eadata$COD_ENSE==214 |
                         eadata$COD_ENSE==215 |
                         eadata$COD_ENSE==216 |
                         eadata$COD_ENSE==217 |
                         eadata$COD_ENSE==299
), ]

# Crear variable que sea la suma de estudiantes de cada nivel
eadata <- eadata %>%
  group_by(RBD, COD_ENSE2, COD_GRADO)  %>%
  mutate(TOTAL_ALU_NIVEL=sum(N_ALU))

# Nos quedamos con las variables de interés
eadata <- eadata[ ,c(2:14,39)]

# Quitamos los duplicados de las escuelas
eadata <- eadata %>% distinct(RBD, TOTAL_ALU_NIVEL, .keep_all = TRUE)

# Creamos varibale que contiene total matrícula por colegio
eadata <- eadata %>%
  group_by(RBD) %>%
  mutate(TOTAL_MAT = sum(TOTAL_ALU_NIVEL))

# Nos quedamos solamente con una fila de establecimiento
eadata <- eadata %>% distinct(RBD, TOTAL_MAT, .keep_all = TRUE)

# Eliminamos variable de matricula por nivel
eadata <- eadata[ ,c(1:13,15)]

# Cargo base de sostenedores
easostenedor <- read_delim("data/directorio.csv",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nos quedamos con las variables de interés
easostenedor <- easostenedor[ ,c(2,5,6,20)]

# Unir sostenedor con eadata
eadata <- merge(eadata, easostenedor,
                by="RBD")

# Cargamos base de sostenedores con nombre
nomsostenedor <- read_delim("data/sostenedores.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Me quedo con las variables de interés
nomsostenedor <- nomsostenedor[ ,c(3,4,5)]

# Cambio el nombre de la variable RUT_SOST para que coincida con la otra
nomsostenedor <- rename(nomsostenedor, RUT_SOSTENEDOR = RUT_SOST)

# Unir nomsostenedor con eadata
eadata <- merge(eadata, nomsostenedor,
                by=c("MRUN", "RUT_SOSTENEDOR"))

# Cargamos la base ubicación
ubicacion <- read_csv("data/ubicacion.csv")

# Nos quedamos con las variables de interés
ubicacion <- ubicacion[ ,c(2,17,18)]

# Corregimos los nombres de las variables
ubicacion <- rename(ubicacion, RBD = `RBD,N,19,11`)
ubicacion <- rename(ubicacion, LATITUD = `LATITUD,N,19,11`)
ubicacion <- rename(ubicacion, LONGITUD = `LONGITUD,N,19,11`)

# Unimos ubicaciÃ³n con eadata
eadata <- merge(eadata, ubicacion,
                by="RBD")

# Cargamos la base de escuelas arriba
inscritos <- read_xlsx("data/inscritos.xlsx")

# Nos quedamos con las variables de interés
inscritos <- inscritos[ ,c(3,18)]

# Unimos la base inscritos con eadata
eadata <- merge(eadata, inscritos,
                by="RBD")

# Arreglamos el nombre de la variable EA_2021
eadata <- rename(eadata, EA_2021 = `EA 2021`)

# Quitar objetos que no necesito
rm(easostenedor, inscritos, nomsostenedor, ubicacion)

# Cambiar nombres de la variable NOM_REG_RBD_A Y EA_2021
eadata <- mutate(eadata, REGIÓN = ifelse(NOM_REG_RBD_A == "ANTOF", "ANTOFAGASTA",
                                  ifelse(NOM_REG_RBD_A == "ATCMA", "ATACAMA",
                                  ifelse(NOM_REG_RBD_A == "ARAUC", "LA ARAUCANÍA",
                                  ifelse(NOM_REG_RBD_A == "AYP", "ARICA Y PARINACOTA",
                                  ifelse(NOM_REG_RBD_A == "AYSEN", "AYSÉN",
                                  ifelse(NOM_REG_RBD_A == "BBIO", "BIOBÍO",
                                  ifelse(NOM_REG_RBD_A == "COQ", "COQUIMBO",
                                  ifelse(NOM_REG_RBD_A == "LAGOS", "LOS LAGOS",
                                  ifelse(NOM_REG_RBD_A == "LGBO", "O'HIGGINS",
                                  ifelse(NOM_REG_RBD_A == "MAG", "MAGALLANES",
                                  ifelse(NOM_REG_RBD_A == "MAULE", "MAULE",       
                                  ifelse(NOM_REG_RBD_A == "NUBLE", "ÑUBLE",
                                  ifelse(NOM_REG_RBD_A == "RIOS", "LOS RÍOS",
                                  ifelse(NOM_REG_RBD_A == "RM", "METROPOLITANA",
                                  ifelse(NOM_REG_RBD_A == "TPCA", "TARAPACÁ",
                                  ifelse(NOM_REG_RBD_A == "VALPO", "VALPARAÍSO", "F")))))))))))))))))

eadata$EA_2021 <- toupper(eadata$EA_2021)



eadata$NOM_REG_RBD_A <- NULL

attach(eadata)

eadata <- data.frame(RBD, NOM_RBD, REGIÓN , NOM_COM_RBD, NOM_DEPROV_RBD, NOMBRE_SOST, RURAL_RBD, TOTAL_MAT, CONVENIO_PIE, LATITUD, LONGITUD, EA_2021  )

detach(eadata)

# Cambiamos nombre a las variables
eadata <- rename(eadata, ESTABLECIMIENTO = NOM_RBD)
eadata <- rename(eadata, COMUNA = NOM_COM_RBD)
eadata <- rename(eadata, DEPROV = NOM_DEPROV_RBD)
eadata <- rename(eadata, RURAL = RURAL_RBD)
eadata <- rename(eadata, MATRÍCULA = TOTAL_MAT)
eadata <- rename(eadata, PIE = CONVENIO_PIE)
eadata <- rename(eadata, LATITUD = LATITUD)
eadata <- rename(eadata, LONGITUD = LONGITUD)
eadata <- rename(eadata, SOSTENEDOR = NOMBRE_SOST)

save.image(file="data/eadata.RData")

# Escuelas Arriba 2021
# Armado de base de datos

# Nota: Esta base servirá para nuestra ShinyApp más adelante

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)

# Cargo base de establecimientos a nivel nacional
eadata <- read_delim("data/matricula.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nos quedamos con las variables de interés
eadata <- eadata[ ,c(2,4,6,7,9,12,14)]

# Cargamos la base de escuelas arriba
inscritos <- read_xlsx("data/inscritos.xlsx")

# Nos quedamos con las variables de interés
inscritos <- inscritos[ ,c(4,8,9,19,20)]

# Unimos la base inscritos con eadata
eadata <- merge(eadata, inscritos,
                by="RBD")

# Cargamos la base ubicación
ubicacion <- read_csv("data/ubicacion.csv")

# Nos quedamos con las variables de interés
ubicacion <- ubicacion[ ,c(2,17,18)]

# Corregimos los nombres de las variables
ubicacion <- rename(ubicacion, RBD = `RBD,N,19,11`)
ubicacion <- rename(ubicacion, LATITUD = `LATITUD,N,19,11`)
ubicacion <- rename(ubicacion, LONGITUD = `LONGITUD,N,19,11`)

# Unimos ubicación con eadata
eadata <- merge(eadata, ubicacion,
                by="RBD", all = TRUE)

# Quitamos los NA
eadata <- eadata[complete.cases(eadata[ , 9]),]

# Cargamos la base ive
ive <- read_delim("data/ive.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

# Unimos ive con eadata
eadata <- merge(eadata, ive,
                by="RBD", all = TRUE)

# Quitamos los NA
eadata <- eadata[complete.cases(eadata[ , 9]),]

# Arreglamos el nombre de la variable EA_2021 e IVE
eadata <- rename(eadata, EA_2021 = `EA 2021`)
eadata <- rename(eadata, IVE_BÁSICA= `IVE BÁSICA 2021`)
eadata <- rename(eadata, IVE_MEDIA= `IVE MEDIA 2021`)
eadata <- rename(eadata, NIVEL_EA= `Nivel EA`)
eadata <- rename(eadata, SOSTENEDOR = Sostenedor)
eadata <- rename(eadata, CORREO_ENCARGADO = `Correo encargado pl`)

# Quitar objetos que no necesito
rm(inscritos, ubicacion, ive)

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

# Modificamos variables rural y dependencia
eadata <- mutate(eadata, RURAL_RBD = ifelse(RURAL_RBD == "1", "RURAL", "URBANO"))
eadata <- mutate(eadata, COD_DEPE2 = ifelse(COD_DEPE2 == "1", "MUNICIPAL",
                                            ifelse(COD_DEPE2 == "2", "PARTICULAR SUBVENCIONADO",
                                                   ifelse(COD_DEPE2 == "3", "PARTICULAR PAGADO",
                                                          ifelse(COD_DEPE2 == "4", "CORPORACIÓN DE ADMINISTRACIÓN DELEGADA",
                                                                 ifelse(COD_DEPE2 == "5", "SERVICIO LOCAL DE EDUCACIÓN", "F"))))))

# Eliminamos variable NOM_REG_RBD_A
eadata$NOM_REG_RBD_A <- NULL

eadata <- mutate(eadata, NIVEL = ifelse(NIVEL_EA == "Educación Básica", "BÁSICA",
                                        ifelse(NIVEL_EA == "0", "NA",
                                        ifelse(NIVEL_EA == "ambas", "AMBOS",
                                               ifelse(NIVEL_EA== "Educación Básica y Educación Media", "AMBOS",
                                                      ifelse(NIVEL_EA=="Educación Media", "MEDIA",
                                                             ifelse(NIVEL_EA=="educacion_basica", "BÁSICA",
                                                                    ifelse(NIVEL_EA=="educacion_media", "MEDIA", "F"))))))))
# Eliminamos variable NIVEL_EA
eadata$NIVEL_EA <- NULL

# Cambiamos nombre a las variables
eadata <- rename(eadata, ESTABLECIMIENTO = NOM_RBD)
eadata <- rename(eadata, COMUNA = NOM_COM_RBD)
eadata <- rename(eadata, DEPROV = NOM_DEPROV_RBD)
eadata <- rename(eadata, RURAL = RURAL_RBD)
eadata <- rename(eadata, LATITUD = LATITUD)
eadata <- rename(eadata, LONGITUD = LONGITUD)
eadata <- rename(eadata, ANTIGUO = EA_2021)
eadata <- rename(eadata, DEPENDENCIA = COD_DEPE2)

# Cambiarle el nombre de antiguo a nuevo
eadata <- mutate(eadata, NUEVA = ifelse(ANTIGUO == "si", "NO",
                                                ifelse(ANTIGUO =="no", "SI", "F")))

eadata$ANTIGUO <- NULL

# Ordenamos las variables
attach(eadata)
eadata <- data.frame(RBD, ESTABLECIMIENTO, REGIÓN , COMUNA, DEPROV, DEPENDENCIA, NUEVA, RURAL, SOSTENEDOR,CORREO_ENCARGADO, NIVEL, IVE_BÁSICA, IVE_MEDIA, LATITUD, LONGITUD)
detach(eadata)

# Cargamos base del PIE
pie <- read_delim("data/pie.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

# Nos quedamos con algunas variables
pie <- pie[ ,c(2,20)]

# Unimos eadata con pie
eadata <- merge(eadata, pie,
                by="RBD", all=TRUE)

# Quitamos los NA 
eadata <- eadata[complete.cases(eadata[ , 7]),]

# Cambiamos nombre observaciones PIE
eadata <- mutate(eadata, CONVENIO_PIE = ifelse(CONVENIO_PIE == "1", "SI", "NO"))
eadata <- rename(eadata, PIE = CONVENIO_PIE)

# Cargamos la base de matrícula por escuela
mat <- read_xlsx("data/curso.xlsx")

# Crear variable que sea la suma de estudiantes de cada nivel
mat <- mat %>%
  group_by(RBD)  %>%
  mutate(MATRÍCULA=sum(CANTIDAD_ALUMNOS))

# Nos quedamos con las variables importantes
mat <- mat[ c(2,15)]

# Quitamos repetidos
mat <- mat %>% distinct(RBD, MATRÍCULA, .keep_all = TRUE)

# Unimos mat con eadata
eadata <- merge(eadata, mat,
                by="RBD", all = TRUE)

# Eliminamos NA
eadata <- eadata[complete.cases(eadata[ , 7]),]

# Quitamos mat y pie
rm(mat, pie)

# Colocamos "SIN INFORMACIÓN" EN IVE VACÍOS
eadata <- eadata %>% replace_na(list(IVE_BÁSICA = 'SIN INFORMACIÓN', IVE_MEDIA = 'SIN INFORMACIÓN')) 

# Agregamos latitud y longitud a obsservación que falta
eadata <- eadata %>% replace_na(list(LATITUD = '-33.40387', LONGITUD = '-70.53654')) 

# Convertir latitud y longitud en numérica
eadata$LATITUD <- as.numeric(eadata$LATITUD)
eadata$LONGITUD <- as.numeric(eadata$LONGITUD)

# Cargamos la base participación
participa <- read_xlsx("data/participacion.xlsx")

# Unimos eadata con participa
eadata <- merge(eadata, participa, 
                by="RBD", all = TRUE)

# Eliminamos NA
eadata <- eadata[complete.cases(eadata[ , 7]),]

# Cargo la base de categoría
categoria <- read_xlsx("data/categoria.xlsx")

# Unimos eadata con participa
eadata <- merge(eadata, categoria, 
                by="RBD", all = TRUE)

# Eliminamos NA
eadata <- eadata[complete.cases(eadata[ , 7]),]

# Cambiamos nombre de variable categoria de desempeño
eadata <- rename(eadata, CATEGORÍA = `Categoría Desempeño 2019`)

eadata <- mutate(eadata, CATEGORÍA = ifelse(CATEGORÍA == "ALTO", "ALTO",
                                            ifelse(CATEGORÍA == "INSUFICIENTE", "INSUFICIENTE",
                                                   ifelse(CATEGORÍA == "MEDIO", "MEDIO",
                                                          ifelse(CATEGORÍA == "MEDIO-BAJO","MEDIO-BAJO",
                                                                 ifelse(CATEGORÍA == "MEDIO-BAJO (NUEVO)", "MEDIO-BAJO",
                                                                        ifelse(CATEGORÍA == "S/O * S/O", "SIN CATEGORÍA",
                                                                               ifelse(CATEGORÍA == "SIN CATEGORIA: BAJA MATRICULA", "SIN CATEGORÍA",
                                                                                      ifelse(CATEGORÍA == "SIN CATEGORIA: FALTA DE INFORMACIÓN", "SIN CATEGORÍA", "F")))))))))

# Modifico variable NUEVA
eadata <- mutate(eadata, NUEVA = ifelse(PARTICIPACIÓN == "NUEVO", "SI", "NO"))

# Quitamos objetvos
rm(categoria, participa)


# Guardar base
save.image("data/eadata.RData")



write_xlsx(eadata, "data.xlsx")


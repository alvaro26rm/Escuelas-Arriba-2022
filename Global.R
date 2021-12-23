# Escuelas Arriba 2021
# Armado de base de datos

# Nota: Esta base servirá para nuestra ShinyApp más adelante

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)


# ------- ESCUELAS -------#

# Cargo base de establecimientos a nivel nacional
escuela <- read_delim("data/matricula.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Nos quedamos con las variables de interés
escuela <- escuela[ ,c(2,4,6,7,9,12,14)]

# Cambiar nombres de la variable NOM_REG_RBD_A Y EA_2021
escuela <- mutate(escuela, REGIÓN = ifelse(NOM_REG_RBD_A == "ANTOF", "ANTOFAGASTA",
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

# Eliminamos variable NOM_REG_RBD_A
escuela$NOM_REG_RBD_A <- NULL

# Modificamos variables rural y dependencia
escuela <- mutate(escuela, RURAL_RBD = ifelse(RURAL_RBD == "1", "SI", "NO"))
escuela <- mutate(escuela, COD_DEPE2 = ifelse(COD_DEPE2 == "1", "MUNICIPAL",
                                       ifelse(COD_DEPE2 == "2", "PARTICULAR SUBVENCIONADO",
                                       ifelse(COD_DEPE2 == "3", "PARTICULAR PAGADO",
                                       ifelse(COD_DEPE2 == "4", "CORPORACIÓN DE ADMINISTRACIÓN DELEGADA",
                                       ifelse(COD_DEPE2 == "5", "SERVICIO LOCAL DE EDUCACIÓN", "F"))))))

# Cambiamos nombre a las variables
escuela <- rename(escuela, ESTABLECIMIENTO = NOM_RBD)
escuela <- rename(escuela, COMUNA = NOM_COM_RBD)
escuela <- rename(escuela, DEPROV = NOM_DEPROV_RBD)
escuela <- rename(escuela, RURAL = RURAL_RBD)
escuela <- rename(escuela, DEPENDENCIA = COD_DEPE2)




# ------- INSCRITOS -------#

# Cargamos la base de escuelas arriba
inscritos <- read_xlsx("data/inscritos.xlsx")

# Nos quedamos con las variables de interés
inscritos <- inscritos[ ,c(4,8,10,12,17,19,20)]

# Arreglamos el nombre de la variable EA_2021 e IVE
inscritos <- rename(inscritos, NIVEL_EA= `Nivel EA`)
inscritos <- rename(inscritos, SOSTENEDOR = Sostenedor)
inscritos <- rename(inscritos, NOMBRE_DIRECTOR = `Nombre Director`)
inscritos <- rename(inscritos, CORREO_DIRECTOR = `Correo director`)
inscritos <- rename(inscritos, NOMBRE_ENCARGADO = `Nombre encargado pl`)
inscritos <- rename(inscritos, CORREO_ENCARGADO = `Correo encargado pl`)

inscritos <- mutate(inscritos, NIVEL = ifelse(NIVEL_EA == "Educación Básica", "BÁSICA",
                                       ifelse(NIVEL_EA == "0", "NA",
                                       ifelse(NIVEL_EA == "ambas", "AMBOS",
                                       ifelse(NIVEL_EA == "Educación Básica y Educación Media", "AMBOS",
                                       ifelse(NIVEL_EA == "Educación Media", "MEDIA",
                                       ifelse(NIVEL_EA == "educacion_basica", "BÁSICA",
                                       ifelse(NIVEL_EA == "educacion_media", "MEDIA", "F"))))))))

# Eliminamos la variable NIVEL_EA
inscritos$NIVEL_EA <- NULL

# ------- UBICACIÓN -------#

# Cargamos la base ubicación
ubicacion <- read_delim("data/ubicacion.csv", 
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE)

# Corregimos las variable de longitud y latitud
ubicacion$LATITUD <- ubicacion$LATITUD / 100000
ubicacion$LONGITUD <- ubicacion$LONGITUD / 100000


# Nos quedamos con las variables de interés
ubicacion <- ubicacion[ ,c(2,18,19)]





# ------- IVE -------#

# Cargamos la base ive
ive <- read_delim("data/ive.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

ive <- rename(ive, IVE_BÁSICA= `IVE BÁSICA 2021`)
ive <- rename(ive, IVE_MEDIA= `IVE MEDIA 2021`)

# Colocamos "SIN INFORMACIÓN" EN IVE VACÍOS
ive <- ive %>% replace_na(list(IVE_BÁSICA = 'SIN INFORMACIÓN', IVE_MEDIA = 'SIN INFORMACIÓN')) 



# ------- PIE -------#

# Cargamos base del PIE
pie <- read_delim("data/pie.csv", delim = ";", 
                  escape_double = FALSE, trim_ws = TRUE)

# Nos quedamos con algunas variables
pie <- pie[ ,c(2,20)]

# Cambiamos nombre observaciones PIE
pie <- mutate(pie, CONVENIO_PIE = ifelse(CONVENIO_PIE == "1", "SI", "NO"))
pie <- rename(pie, PIE = CONVENIO_PIE)




# ------- MATRÍCULA -------#

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



# ------- PARTICIPA -------#

# Cargamos la base participación
participa <- read_xlsx("data/participacion.xlsx")

# Creo la variable NUEVA
participa <- mutate(participa, NUEVA = "NO")




# ------- CATEGORÍA -------#

# Cargo la base de categoría
categoria <- read_xlsx("data/categoria.xlsx")

# Cambiamos nombre de variable categoria de desempeño
categoria <- rename(categoria, CATEGORÍA = `Categoría Desempeño 2019`)

categoria <- mutate(categoria, CATEGORÍA = ifelse(CATEGORÍA == "ALTO", "ALTO",
                                           ifelse(CATEGORÍA == "INSUFICIENTE", "INSUFICIENTE",
                                           ifelse(CATEGORÍA == "MEDIO", "MEDIO",
                                           ifelse(CATEGORÍA == "MEDIO-BAJO","MEDIO-BAJO",
                                           ifelse(CATEGORÍA == "MEDIO-BAJO (NUEVO)", "MEDIO-BAJO",
                                           ifelse(CATEGORÍA == "S/O * S/O", "SIN CATEGORÍA",
                                           ifelse(CATEGORÍA == "SIN CATEGORIA: BAJA MATRICULA", "SIN CATEGORÍA",
                                           ifelse(CATEGORÍA == "SIN CATEGORIA: FALTA DE INFORMACIÓN", "SIN CATEGORÍA", "F")))))))))

# ------- UNIENDO BASES -------#

# Unimos inscritos con categoria
eadata <- merge(inscritos, categoria, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(categoria, inscritos)

# Unimos eadata con escuela
eadata <- merge(eadata, escuela, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(escuela)

# Unimos eadata con ive
eadata <- merge(eadata, ive, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(ive)

# Unimos eadata con mat
eadata <- merge(eadata, mat, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(mat)

# Unimos eadata con participa
eadata <- merge(eadata, participa, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

eadata$NUEVA <- eadata$NUEVA %>% replace_na('SI')

eadata$PARTICIPA <- eadata$PARTICIPA %>% replace_na('NUEVA')

rm(participa)


# Unimos eadata con pie
eadata <- merge(eadata, pie, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(pie)


# Unimos eadata con ubicación
eadata <- merge(eadata, ubicacion, 
                by="RBD", all = TRUE)

eadata <- eadata[complete.cases(eadata[ , 7]),]

rm(ubicacion)


# Ordenamos las variables
attach(eadata)
eadata <- data.frame(RBD, ESTABLECIMIENTO, REGIÓN, DEPROV, COMUNA, DEPENDENCIA, SOSTENEDOR, CATEGORÍA, NIVEL, RURAL, PARTICIPA, NUEVA, PIE, MATRÍCULA, IVE_BÁSICA, IVE_MEDIA, NOMBRE_DIRECTOR, CORREO_DIRECTOR, NOMBRE_ENCARGADO, CORREO_ENCARGADO, LATITUD, LONGITUD)
detach(eadata)


# Guardar base
save.image("data/eadata.RData")

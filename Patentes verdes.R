# ------------------------------------------------------------------------------------------------------------------------
## LIBRERIAS
# ------------------------------------------------------------------------------------------------------------------------
#install.packages("data.table")
library(data.table)
library(tidyverse)
library(openxlsx)
library(stringr)
library(readxl)
library(dplyr)

# ------------------------------------------------------------------------------------------------------------------------
## CARGA DE DATOS
# ------------------------------------------------------------------------------------------------------------------------

patentes <- (file.choose())
WIPO <- read_xlsx('Lista clases verdes WIPO-EU.xlsx', sheet = 'verdes OMPI', col_names = TRUE) #Lista clases verdes de la WIPO
EU <- read_xlsx('Lista clases verdes WIPO-EU.xlsx', sheet = 'verdes EPO', col_names = TRUE) #Lista clases verdes de la EU

# ------------------------------------------------------------------------------------------------------------------------
## CÓDIGO
# ------------------------------------------------------------------------------------------------------------------------

#**\\\LIMPIEZA DE COLUMNAS A DIAGNOSTICAR\\\**
#*DEJAR LAS COLUMNAS DE '(IPC) `, "(CPC)","IPC Clases", #*"CPC clases","Total green inventions"
#*CON VALORES NA PARA LOS TEXTOS Y 0 PARA LOS NUM Y QUE ASÍ NO ALTERE EL RESULTADO
patentes$`(IPC) ` <- 0
patentes$`(CPC)` <- 0
patentes$`IPC Clases` <- NA
patentes$`CPC clases` <- NA
patentes$`Total green inventions`<- 0


patentesCPC <- patentes%>%
  select(-`(IPC) `,-`IPC Clases`)

patentesIPC <- patentes%>%
  select(-`(CPC)`,-`CPC clases`)


#**\\\CLASIFICACIÓN DE WIPO CON SEPARADOR , \\\**
#*\\ WIPO \\*
#*\ IPC \*
#*FUNCIÓN DE ASIGNACIÓN, CHECA AMBAS BASES Y SI COINCIDEN PONE 1 Y EXTRAE EL VALOR COINCIDENTE EN OTRA COLUMNA*
ipcWIPO <- function(patentesIPC, WIPO) {
  patentesIPC$`(IPC) ` <- 0
  
  for (i in 1:nrow(patentesIPC)) {
    IPC <- str_split(patentesIPC$`CIP - IPC`[i], "; ")[[1]]
    
    # verificar si algún valor de `CIP - IPC` coincide con algún valor de WIPO
    if (!any(is.na(IPC)) && any(str_detect(IPC, paste(WIPO$TOPIC, collapse = "|")))) {
      patentesIPC$`(IPC) `[i] <- 1
      
      # obtener los valores de WIPO que coinciden con los valores de `CIP - IPC`
      IPC_Clases <- IPC[str_detect(IPC, paste(WIPO$TOPIC, collapse = "|"))]
      patentesIPC$'IPC Clases'[i] <- paste(IPC_Clases, collapse = "; ")
    }
  }
  
  return(patentesIPC)
}


#*\ CPC \*
#*FUNCIÓN DE ASIGNACIÓN, CHECA AMBAS BASES Y SI COINCIDEN PONE 1 Y EXTRAE EL VALOR COINCIDENTE EN OTRA COLUMNA*
cpcWIPO <- function(patentesCPC, WIPO) {
  patentesCPC$`(CPC)` <- 0
  
  for (i in 1:nrow(patentesCPC)) {
    CPC <- str_split(patentesCPC$`CPC - PATENTSCOPE` [i], "; ")[[1]]
    
    # verificar si algún valor de `CPC - PATENTSCOPE` coincide con algún valor de WIPO
    if (!any(is.na(CPC)) && any(str_detect(CPC, paste(WIPO$TOPIC, collapse = "|")))) {
      patentesCPC$`(CPC)`[i] <- 1
      
      # obtener los valores de WIPO que coinciden con los valores de `CPC - PATENTSCOPE`
      CPC_Clases <- CPC[str_detect(CPC, paste(WIPO$TOPIC, collapse = "|"))]
      patentesCPC$`CPC clases` [i] <- paste(CPC_Clases, collapse = "; ")
    }
  }
  
  return(patentesCPC)
}

#CLASIFICACIÓN WIPO IPC Y CPC
patentesWIPO1 <- ipcWIPO(patentesIPC, WIPO)
patentesWIPO2 <- cpcWIPO(patentesCPC, WIPO)

patentesWIPO <- merge(patentesWIPO1, patentesWIPO2, all= TRUE)

# WIPO TOTAL VERDES
patentesWIPO$`Total green inventions` <- ifelse(patentesWIPO$`(CPC)` =="1" | patentesWIPO$`(IPC) ` == "1", 1, 0)

#Reordenar
patentesWIPO <- patentesWIPO%>%
  select("Patente", "Año concesión", "Inventores", "Solicitante", "Tipo persona. Moral ó Física", "Fecha de presentación", 
         "Solo Hombres", "Solo Mujeres", "mixto", "Hombres", "Mujeres", "Estado", "Número de contacto de el o los inventores", 
         "Correo de contacto de los inventores", "Nombre o nombres de los apoderados", "Número de contacto de los apoderados", 
         "Correo de contacto de los apoderados", "CIP - IPC", "CPC - PATENTSCOPE", `(IPC) `, "(CPC)", "IPC Clases", "CPC clases", 
         "Total green inventions", "Nacionalidad", "Mujeres extranjeras", "Hombres extranjeros", "Mujeres y hombres extranjeros", 
         "Inteligencia Artificial", "Cockburn et al. IPC", "Cockburn et al. Keywords", "EU-JRC (keywords)", "Fujii and Managi", 
         "OECD STI - IPC", "Resumen")

#**\\\CLASIFICACIÓN DE EU CON SEPARADOR , \\\**
#*\\ EU \\*
#*\ IPC \*
#*FUNCIÓN DE ASIGNACIÓN, CHECA AMBAS BASES Y SI COINCIDEN PONE 1 Y EXTRAE EL VALOR COINCIDENTE EN OTRA COLUMNA*
ipcEU <- function(patentesIPC, EU) {
  patentesIPC$`(IPC) ` <- 0
  
  for (i in 1:nrow(patentesIPC)) {
    IPC <- str_split(patentesIPC$`CIP - IPC`[i], "; ")[[1]]
    
    # verificar si algún valor de `CIP - IPC` coincide con algún valor de EU
    if (!any(is.na(IPC)) && any(str_detect(IPC, paste(EU$TOPIC, collapse = "|")))) {
      patentesIPC$`(IPC) `[i] <- 1
      
      # obtener los valores de EU que coinciden con los valores de `CIP - IPC`
      IPC_Clases <- IPC[str_detect(IPC, paste(EU$TOPIC, collapse = "|"))]
      patentesIPC$'IPC Clases'[i] <- paste(IPC_Clases, collapse = "; ")
    }
  }
  
  return(patentesIPC)
}


#*\ CPC \*
#*FUNCIÓN DE ASIGNACIÓN, CHECA AMBAS BASES Y SI COINCIDEN PONE 1 Y EXTRAE EL VALOR COINCIDENTE EN OTRA COLUMNA*
cpcEU <- function(patentesCPC, EU) {
  patentesCPC$`(CPC)` <- 0
  
  for (i in 1:nrow(patentesCPC)) {
    CPC <- str_split(patentesCPC$`CPC - PATENTSCOPE` [i], "; ")[[1]]
    
    # verificar si algún valor de `CPC - PATENTSCOPE` coincide con algún valor de EU
    if (!any(is.na(CPC)) && any(str_detect(CPC, paste(EU$TOPIC, collapse = "|")))) {
      patentesCPC$`(CPC)`[i] <- 1
      
      # obtener los valores de EU que coinciden con los valores de `CPC - PATENTSCOPE`
      CPC_Clases <- CPC[str_detect(CPC, paste(EU$TOPIC, collapse = "|"))]
      patentesCPC$`CPC clases` [i] <- paste(CPC_Clases, collapse = "; ")
    }
  }
  
  return(patentesCPC)
}

#CLASIFICACIÓN EU IPC Y CPC
patentesEU1 <- ipcEU(patentesIPC, EU)
patentesEU2 <- cpcEU(patentesCPC, EU)

patentesEU <- merge(patentesEU1, patentesEU2, all= TRUE)


# EU TOTAL VERDES
patentesEU$`Total green inventions` <- ifelse(patentesEU$`(CPC)` == "1" | patentesEU$`(IPC)` == "1", 1, 0)


#Reordenar
patentesEU1 <- patentesEU%>%
  select("Patente", "Año concesión", "Inventores", "Solicitante", "Tipo persona. Moral ó Física", "Fecha de presentación", 
         "Solo Hombres", "Solo Mujeres", "mixto", "Hombres", "Mujeres", "Estado", "Número de contacto de el o los inventores", 
         "Correo de contacto de los inventores", "Nombre o nombres de los apoderados", "Número de contacto de los apoderados", 
         "Correo de contacto de los apoderados", "CIP - IPC", "CPC - PATENTSCOPE", `(IPC) `, "(CPC)", "IPC Clases", "CPC clases", 
         "Total green inventions", "Nacionalidad", "Mujeres extranjeras", "Hombres extranjeros", "Mujeres y hombres extranjeros", 
         "Inteligencia Artificial", "Cockburn et al. IPC", "Cockburn et al. Keywords", "EU-JRC (keywords)", "Fujii and Managi", 
         "OECD STI - IPC", "Resumen")

patentesEU <- patentesEU%>%
  select("Patente", "CIP - IPC", "CPC - PATENTSCOPE", `(IPC) `, "(CPC)", "IPC Clases", "CPC clases", 
         "Total green inventions")

patentesEU <- patentesEU%>%
  mutate(`Solo Hombres` = as.numeric(`Solo Hombres`),
         `Solo Mujeres` = as.numeric(`Solo Mujeres`))

patentesWIPO <- patentesWIPO%>%
  mutate(`Solo Hombres` = as.numeric(`Solo Hombres`),
         `Solo Mujeres` = as.numeric(`Solo Mujeres`))

# ------------------------------------------------------------------------------------------------------------------------
## INTERSECCIÓN
# ------------------------------------------------------------------------------------------------------------------------

## MERGE DE LOS DF
# Renombrar las columnas seleccionadas del segundo DataFrame (df2) con el sufijo "-EPO"
cols_to_rename <- setdiff(names(patentesEU), "Patente")
patentesEU2 <- patentesEU %>%
  rename_at(vars(all_of(cols_to_rename)), ~paste0(., "-EPO"))

# Realizar el merge basado en la columna "Patente" y conservar todas las columnas
conglomerado <- patentesWIPO %>%
  left_join(patentesEU, by = "Patente")


## INTERSECCIÓN

# Obtener los valores de las columnas en cada DataFrame
values_wipi <- WIPO$TOPIC
values_eu <- EU$TOPIC

# Identificar valores duplicados presentes en ambos DataFrames
duplicated_values <- intersect(values_wipi, values_eu)


# Crear una lista con los valores duplicados presentes en ambos DataFrames
if (length(duplicated_values) > 0) {
  duplicated_list <- as.list(duplicated_values)
  print(duplicated_list)
} else {
  print("No hay valores duplicados en ambos DataFrames.")
}


# Función para extraer los valores coincidentes con la lista
extract_values <- function(values, duplicated_list) {
  matching_values <- sapply(values, function(val) {
    val <- strsplit(as.character(val), ";", fixed = TRUE)[[1]]
    matching <- val[val %in% duplicated_list]
    if (length(matching) > 0) {
      paste(matching, collapse = ";")
    } else {
      NA
    }
  })
  return(matching_values)
}

# Crear columna en df3 con valores coincidentes de "IPC Clases"
conglomerado$Coincidentes_IPC <- extract_values(conglomerado$`CIP - IPC.x`, duplicated_values)

# Crear columna en df3 con valores coincidentes de "CPC Clases"
conglomerado$Coincidentes_CPC <- extract_values(conglomerado$`CPC - PATENTSCOPE.x`, duplicated_values)

# Crear columna "Intersección" en df3 con valor 1 si hay coincidencias y 0 si ambas son NA
conglomerado$Intersección <- ifelse(!is.na(conglomerado$Coincidentes_IPC) | !is.na(conglomerado$Coincidentes_CPC), 1, 0)


# Cambiar los nombres de las columnas que contienen ".x" al final por "-OMPI"
colnames(conglomerado)[grep("\\.x$", colnames(conglomerado))] <- gsub("\\.x$", "-OMPI", colnames(conglomerado)[grep("\\.x$", colnames(conglomerado))])

# Cambiar los nombres de las columnas que contienen ".y" al final por "-EPO"
colnames(conglomerado)[grep("\\.y$", colnames(conglomerado))] <- gsub("\\.y$", "-EPO", colnames(conglomerado)[grep("\\.y$", colnames(conglomerado))])



conglomerado$Verdes <- ifelse(conglomerado$`Total green inventions-OMPI` == 1 | conglomerado$`Total green inventions-EPO`==1, 1, 0)


# Crear un nuevo dataframe que contenga solo las columnas seleccionadas
Conglomerado <- conglomerado[, c("Patente", "Año concesión", "Inventores", "Solicitante",
                                 "Tipo persona. Moral ó Física", "Fecha de presentación",
                                 "Solo Hombres", "Solo Mujeres", "mixto", "Hombres",
                                 "Mujeres", "Estado", "Número de contacto de el o los inventores",
                                 "Correo de contacto de los inventores", "Nombre o nombres de los apoderados",
                                 "Número de contacto de los apoderados", "Correo de contacto de los apoderados",
                                 "CIP - IPC-OMPI", "CPC - PATENTSCOPE-OMPI", "(IPC) -OMPI", "(CPC)-OMPI",
                                 "IPC Clases-OMPI", "CPC clases-OMPI", "Total green inventions-OMPI",
                                 "(IPC) -EPO", "(CPC)-EPO", "IPC Clases-EPO", "CPC clases-EPO", 
                                 "Total green inventions-EPO", "Verdes",
                                 "Coincidentes_IPC","Coincidentes_CPC", "Intersección", 
                                 "Nacionalidad", "Mujeres extranjeras", "Hombres extranjeros",
                                 "Mujeres y hombres extranjeros", "Inteligencia Artificial",
                                 "Cockburn et al. IPC", "Cockburn et al. Keywords", "EU-JRC (keywords)",
                                 "Fujii and Managi", "OECD STI - IPC", "Resumen")]

# ------------------------------------------------------------------------------------------------------------------------
## DESCARGA
# ------------------------------------------------------------------------------------------------------------------------

write.xlsx(patentesWIPO,"PatentesVerdesWIPO.xlsx", rowNames = TRUE)
write.xlsx(patentesEU,"PatentesVerdesEU.xlsx", rowNames = TRUE)
write.xlsx(Conglomerado,"Conglomerado.xlsx", rowNames = TRUE)
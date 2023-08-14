# ------------------------------------------------------------------------------------------------------------------------
## LIBRERIAS
# ------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(readxl)
library(stringr)
library(genero)
library(openxlsx)

# ------------------------------------------------------------------------------------------------------------------------
## CARGA DE DATOS
# ------------------------------------------------------------------------------------------------------------------------

datos<-read_excel(file.choose())
wipo<-read.csv("WIPO.csv")

# ------------------------------------------------------------------------------------------------------------------------
## CÓDIGO
# ------------------------------------------------------------------------------------------------------------------------

## Estandarización lista de wipo
wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)

## Definición de funciones
limpieza<-function(columna) {
  columna <- iconv(columna, to = "ASCII//TRANSLIT") #Mantiene los nombres sin caracteres especiales
  columna<-gsub("\\[.*?\\]", "", columna)
  columna<-toupper(columna)
  delimitador <- "[;]"  # Expresión regular que incluye coma y punto y coma
  col <- (str_count(columna, delimitador) + 1)  # Guardamos el número total de nombres
  limpio <- data.frame(str_split_fixed(columna, delimitador, max(col, na.rm = TRUE)))
  
  separar<-function(x){
    ifelse(is.na(x),"Missing", str_extract(x, "\\b\\w+\\b"))
  } #Nos permitirá extraer el primer nombre en caso de querer evaluar su genero
  
  limpio<-data.frame(sapply(limpio,FUN=separar))
  limpio[is.na(limpio)]<-"Missing" #Reemplazamos NA con Missing
  
  return(limpio)
} #Nos permite limpiar y separar los nombres en diferentes columnas

generoWIPO<- function(nombre) {
  indice <- match(nombre, wipo$name)
  resultado <- ifelse(nombre %in% c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED"), "neutral", NA)
  resultado[!is.na(indice)] <- wipo$gender[indice[!is.na(indice)]]
  resultado<-data.frame(resultado)
  return(resultado)
} #Asignación de género según base de datos WIPO

generossni <- function(col1, col2) {
  resultado <- vector("character", length = length(col1))
  for (i in 1:length(col1)) {
    if (is.na(col1[i]) & is.na(col2[i])) {
      resultado[i] <- NA
    }
    else if (col1[i]=="Missing" & col2[i]=="Missing"){
      resultado["Missing"]
    }
    else if (is.na(col2[i])) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col2[i]=="Missing"){
      resultado[i]<-as.character(col1[i])
    }
    else if (col1[i]=="Missing"){
      resultado[i]<-as.character(col2[i])
    }
    else if (col1[i] == col2[i]) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == "neutral") {
      resultado[i] <- as.character(col1[i])
    }
    else {
      resultado[i] <- "checar"
    }
  }
  return(resultado)
} #Permitirá quedarnos con un único genero

contar_hombres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "male"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
} #Permite contar cuantos hombres existen 

contar_mujeres <- function(df) {
  Total <- apply(df, 1, function(row) {
    nombres <- row[row == "female"]  # Obtener solo los nombres en la fila
    num_nombres <- length(nombres)    # Contar el número de nombres
    return(num_nombres)               # Devolver el número de nombres
  })
  return(Total)
} #Permite contar cuantas mujeres existen

## Ejecución
nombres<-limpieza(datos$Inventores)
col<-ncol(nombres)
#nombres[9,3:6]<-"Missing"
#nombres[11,7:10]<-"Missing"
#nombres[13:14,5:8]<-"Missing"

#Aplicamos la funcion "generoWIPO" 
wipo_gen<-data.frame(sapply(nombres, generoWIPO))
colnames(wipo_gen)<-paste("GenW_",1:col) #Para un mejor entendimiento cambiamos los nombres de las columnas
wipo_gen[is.na(wipo_gen)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

#Aplicamos la funcion genero de la libreria genero
gen_results<-data.frame(lapply(nombres,genero))
colnames(gen_results)<-paste("GenG_",1:col) #Para un mejor entendimiento cambiamos los nombres de las columnas
gen_results[is.na(gen_results)]<-"Missing" #Cambiamos los NA's por Missing para poder aplicar las funciones después

#Creamos un nuevo df donde se encontrará el genero final
genero<-data.frame(mapply(generossni,wipo_gen,gen_results)) 
#genero[3,1]<-"male"

datos1 <- data.frame(matrix(ncol =0 , nrow = nrow(datos)))

#Asignamos el conteo de hombres al df original
datos1$Hombres<-contar_hombres(genero) 

#Asignamos el conteo de mujeres al df original
datos1$Mujeres<-contar_mujeres(genero) 

#Clasificamos en Solo.Hombres, Solo.Mujeres o mixto
for (i in 1:nrow(datos1)) {
  if (datos1$Hombres[i] == 0) {
    datos1$mixto[i] <- 0
    datos1$`Solo Hombres`[i] <- 0
    datos1$`Solo Mujeres`[i] <- 1
  } else if (datos1$Mujeres[i] == 0) {
    datos1$mixto[i] <- 0
    datos1$`Solo Hombres`[i] <- 1
    datos1$`Solo Mujeres`[i] <- 0
  } else {
    datos1$mixto[i] <- 1
    datos1$`Solo Hombres`[i] <- 0
    datos1$`Solo Mujeres`[i] <- 0
  }
} 


datos2<-datos%>% select(7:11)

datos2[1,2,3,4,5]<-as.numeric(datos2[1,2,3,4,5])

nombres_columnas <- colnames(datos1)
datos1$comparacion <- TRUE

for (columna in nombres_columnas) {
  resultado <- datos1[columna] == datos2[columna]
  datos1$comparacion <- datos1$comparacion & resultado
}

datos1$Patente<-datos$Patente
datos3<-datos1%>%filter(comparacion==FALSE)


for (i in 1:nrow(datos)) {
  if (datos$Hombres[i] == 0) {
    datos$mixto[i] <- 0
    datos$`Solo Hombres`[i] <- 0
    datos$`Solo Mujeres`[i] <- 1
  } else if (datos$Mujeres[i] == 0) {
    datos$mixto[i] <- 0
    datos$`Solo Hombres`[i] <- 1
    datos$`Solo Mujeres`[i] <- 0
  } else {
    datos$mixto[i] <- 1
    datos$`Solo Hombres`[i] <- 0
    datos$`Solo Mujeres`[i] <- 0
  }
}

# ------------------------------------------------------------------------------------------------------------------------
## DESCARGA
# ------------------------------------------------------------------------------------------------------------------------

write.xlsx(datos,"Título_del_archivo.xlsx")
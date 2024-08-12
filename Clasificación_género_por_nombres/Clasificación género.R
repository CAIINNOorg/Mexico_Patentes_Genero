# ------------------------------------------------------------------------------------------------------------------------
## LIBRERIAS
# ------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(readxl)
library(stringr)
library(genero)
library(gender)
library(openxlsx)


# ------------------------------------------------------------------------------------------------------------------------
## CARGA DE DATOS
# ------------------------------------------------------------------------------------------------------------------------
#LECTURA DE BASE DE DATOS 
d2<-read_excel('Datos.xlsx') #Base de Datos PATENTES

datos <- d2 %>% 
  select("Patente", "Año concesión", "Inventores") %>% 
  filter(`Año concesión` %in% c('2017','2018','2019','2020','2021','2022'))

d2 <- d2[,!names(d2) %in% c('...1')]


wipo<- read.csv('WIPO.csv',  sep = ",") #Base de Datos WIPO

#PREPARACION DE LA BASE DE DATOS 
#WIPO
#Intercambiamos las letras por los generos para que coincidan con la funcion genero 
wipo$gender <- gsub("M", "male", wipo$gender) 
wipo$gender <- gsub("F", "female", wipo$gender)

#OTORGADAS 

# Función para acomodar los nombres completos en cada celda de una columna específica
separar_nombres_completos <- function(df, columna) {
  df[[columna]] <- sapply(df[[columna]], function(x) {
    if (grepl(";", x)) {
      nombres <- unlist(strsplit(x, ";"))
      nombres <- sapply(nombres, function(nombre) {
        if (grepl(",", nombre)) {
          partes <- unlist(strsplit(nombre, ","))
          nombre <- paste0(partes[2], " ", partes[1])
        }
        return(nombre)
      })
      return(paste(nombres, collapse = "; "))
    } else {
      if (grepl(",", x)) {
        partes <- unlist(strsplit(x, ","))
        x <- paste0(partes[2], " ", partes[1])
      }
      return(x)
    }
  })
  return(df)
}

# ordenar los nombres completos
datos <- separar_nombres_completos(datos, "Inventores")


## Separamos todos los nombres incluidos en la columna Inventores que estan separados por ; y un espacio para que esten separados unicamente por ;
datos$Inventores <- gsub("; ", ";", datos$Inventores, fixed = TRUE) #esto se realiza para que después no tengamos columnas vacías al momento de crear una nueva base de datos
# Reemplazar valores nulos con cadena vacía
#datos$Inventores[is.na(datos$Inventores)] <- ""

# Calcular el número máximo de valores separados por punto y coma
max_cols <- max(str_count(datos$Inventores, ";")) + 1

## Creamos una nueva base de datos en donde únicamente tendremos los nombres y cada uno de ellos estará en una columna
nombres_separados <- str_split_fixed(datos$Inventores, ";", max_cols)
## Convertimos los nombres a mayúsculas
nombres_separados <- toupper(nombres_separados)
## Asignamos los nombres a las nuevas columnas
colnames(nombres_separados) <- paste0("Nombre_", 1:max_cols)

## Unir los nombres separados a los datos originales para extraer las columnas en donde realizaremos el conteo
datos1 <- cbind(datos, nombres_separados)
datos_c<-datos1%>%dplyr::select(1,4:26)


limpiar_columnas <- function(datos, columnas) {
  for (i in columnas) {
    datos[[i]] <- sapply(datos[[i]], function(x) {
      if (is.na(x)) {
        return("")
      } else {
        palabras <- unlist(strsplit(x, "\\s+")) # Dividir la cadena en palabras
        primera_palabra <- palabras[which.max(nchar(palabras) > 0)] # Encontrar la primera palabra que no sea un espacio en blanco
        return(toString(primera_palabra)) # Convertir la primera palabra en una cadena y devolverla
      }
    })
  }
  return(datos)
}

columnas1 <- colnames(datos_c)[2:24] #NOTA: PARA QUE FUNCIONE EL LÍMITE DESPUÉS DEL : DENTRO DEL CORCHETE DEBE SER EL NÚMERO MÁXIMO DE COLUMNAS DE NOMBRE QUE HAY

datos_c <- limpiar_columnas(datos_c,columnas1)


datos_c[datos_c=="MA."]<-"MARIA" #por ultimo se identifico que MA existia en varias observaciones, por lo que lo reemplazamos por MARIA

# CLASIFICACION DE GENERO

##GENERO WIPO
# crea un vector con los nombres de las columnas que contienen los nombres

#Funcion de asignación de genero
genesni <- function(nombre) {
  indice <- match(nombre, wipo$name)
  resultado <- rep(NA, length(nombre))  # Inicializa el vector resultado con NA
  
  resultado[!is.na(indice)] <- wipo$gender[indice[!is.na(indice)]]
  
  # Aplicar condiciones adicionales
  resultado <- ifelse(nombre %in% c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED", "CHEUK", "XIMIN"), "neutral", resultado)
  resultado[nombre %in% c("FRANCIS", "GIOVANNY", "SANTOS", "GAL", "EDEN")] <- "male"
  resultado[nombre %in% c("KELSEY", "PAZ", "REMEDIOS", "ROSE")] <- "female"
  
  return(resultado)
}

# Aplicar la función a varias columnas del dataframe usando sapply
columnas <- colnames(datos_c)[2:24]
resultados_wipo <- sapply(datos_c[columnas], genesni) #resultados WIPO

wipo_results <-data.frame(resultados_wipo)
colnames(wipo_results)<-paste0("Genero1_",1:22) #Dar un nuevo nombre a las columnas que contiene el genero


## GENERO FUNCIÓN LIBRERÍA GENERO 

nombres<-datos_c[,2:24] #NOTA: PARA QUE FUNCIONE EL LÍMITE DESPUÉS DEL : DENTRO DEL CORCHETE DEBE SER EL NÚMERO MÁXIMO DE COLUMNAS DE NOMBRE QUE HAY. DEBE SER IGUAL QUE EL DE LA LÍNEA 98
nombres[nombres==""] <-NA 
nombres[is.na(nombres)]<-"Missing" #Al poder colocar "Missing" la funcion genero podrá correr el código de manera correcta y no influenciarse por los posibles NA's

# Aplicamos la funcion de la libreria genero
gen_results<-data.frame(lapply(nombres,genero))

#Comparacion WIPO vs funcion genero
generossni <- function(col1, col2) {
  resultado <- vector("character", length = length(col1))
  for (i in 1:length(col1)) {
    if (is.na(col1[i]) & is.na(col2[i])) {
      resultado[i] <- NA
    }
    else if (is.na(col1[i])) {
      resultado[i] <- as.character(col2[i])
    }
    else if (is.na(col2[i])) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == col2[i]) {
      resultado[i] <- as.character(col1[i])
    }
    else if (col1[i] == "neutral") {
      resultado[i] <- as.character(col1[i])
    }
    else {
      resultado[i] <- as.character(col1[i])
    }
  }
  return(resultado)
}

#En este data frame guardaremos el genero final
resultados_gen <- data.frame(matrix(ncol = 25, nrow = 2830))

# CON EL VALOR DE ncol SE APLICA MISMA LÓGICA QUE EL LÍMITE DE LA LÍNEA 98 Y 135

#Establecemos el genero final
for (i in 1:23) {
  col_df1 <- wipo_results[, i]
  col_df2 <- gen_results[, i]
  resultados <- generossni(col_df1, col_df2)
  resultados_gen[, i] <- resultados
}

colnames(resultados_gen)<-paste("Gen_",1:25) #Cambiamos el nombre de las columnas

# CON EL VALOR DESPUÉS DEL : SE APLICA MISMA LÓGICA QUE EL LÍMITE DE LA LÍNEA 98 Y 135

# De nuestros resultados final hacemos una ultima conversión para poder hacer el conteo posterior
resultados_gen[resultados_gen=="male"] <-"M"
resultados_gen[resultados_gen=="female"] <-"F"

#Conteo total de Total de Hombres y Total de Mujeres
datos_c$Hombres <- rowSums(sapply(resultados_gen, grepl, pattern = "M"))
datos_c$Mujeres <- rowSums(sapply(resultados_gen, grepl, pattern = "F"))

#Asignamos valores de 0 y 1 para poder hacer realizar la clasificación de mejor manera
datos45 <- resultados_gen
datos45[datos45 == "M"] <- 1
datos45[datos45 == "F"] <- 0
datos45[datos45 == "neutral"] <- NA

datos45 <- datos45 %>% 
  mutate(
    "Solo Hombres" = ifelse(rowSums(!is.na(.)) == 0, 0, apply(., 1, function(x) ifelse(sum(x == 1, na.rm = TRUE) == sum(!is.na(x)), 1, 0))),
    "Solo Mujeres" = ifelse(rowSums(!is.na(.)) == 0, 0, apply(., 1, function(x) ifelse(sum(x == 0, na.rm = TRUE) == sum(!is.na(x)), 1, 0))),
    "Mixto" = ifelse(rowSums(!is.na(.)) == 0, 0, apply(., 1, function(x) ifelse(sum(x == 1, na.rm = TRUE) > 0 & sum(x == 0, na.rm = TRUE) > 0, 1, 0)))
  )

datos_c <- cbind(datos_c, datos45[, 26:28])  # Añadir las columnas de datos45 a datos_c

datos <- cbind(datos,datos_c[, 25:29])  # Añadir las columnas de datos_c a datos

### COMPROBACIÓN DE RESULTADOS
contar_y_sumar <- function(df, inicio_col_letras, fin_col_letras, df_genero, col_hombres, col_mujeres) {
  # Crear la columna "CountNames" que cuenta los datos con letras en un rango de columnas
  df$CountNames <- rowSums(sapply(df[inicio_col_letras:fin_col_letras], function(x) grepl("[A-Za-z]", x)))
  
  # Crear la columna "SumMF" que suma el valor en las columnas de "Hombres" y "Mujeres" de otro df para la misma fila
  df$SumMF <- df_genero[[col_hombres]] + df_genero[[col_mujeres]]
  
  return(df)
}

df_resultado <- contar_y_sumar(datos_c, 2, 24, datos, "Hombres", "Mujeres")

df_resultado <- df_resultado %>% 
  mutate(diferencia = CountNames-SumMF)

# Vector con los nombres que quieres excluir
nombres_excluir <- c("GUADALUPE", "YUNUEN", "TAYDE", "ROSARIO", "ALYED", "CHEUK", "XIMIN")

#cuando toda la clasificación esté bien, este df debe tener 0 registros
df_checar <- df_resultado %>% 
  filter(diferencia > 0) %>%
  filter(rowSums(sapply(., function(x) x %in% nombres_excluir)) == 0)

#cuando toda la clasificación esté bien, este df debe tener los registros de todas las patentes con nombres neutros
df_checar_neutros <- df_resultado %>% 
  filter(diferencia > 0) 


# tras obtener los datos distintos, la corrección se hace de forma manual

###### DESCARGA
# dl = datos limpios
dl <- d2 %>% 
  filter(`Año concesión` %in% c('2017','2018','2019','2020','2021','2022'))

dl$`Solo Hombres` <- datos$`Solo Hombres`
dl$`Solo Mujeres` <- datos$`Solo Mujeres`
dl$mixto <- datos$Mixto
dl$Hombres <- datos$Hombres
dl$Mujeres <- datos$Mujeres


# TRAS DESCARGAR EL ARCHIVO SE DEBE ABRIR PARA BUSCAR MANUALMENTE LAS PATENTES QUE TENGAN 0 TANTO EN
# HOMBRES COMO MUJERES, ESAS SON LAS PATENTES CUYO ÚNICO INVENTOR TIENE NOMBRE NEUTRAL POR LO QUE
# DEBE BUSCARSE EN GOGOLE PARA CLASIFICARLO MANUALMENTE EN EL EXCEL
write.xlsx(dl, "DatosG.xlsx", rowNames = TRUE, colNames = TRUE) #Exportamos

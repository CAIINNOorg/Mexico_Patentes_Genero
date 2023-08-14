# Patentes_Verdes

### Documentos clave

**Código Clasificación Patentes Verdes**
: Script de R que contiene todo el código para realizar la clasificación de listas de patentes verdes (IPC y CPC) según OMPI (WIPO) y EPO (EU).

**Lista clases verdes WIPO-EU**
: listas con los códigos de patentes clasificadas como verdes según según OMPI (WIPO) y EPO (EU).

### Metodología

Este código es un programa escrito en lenguaje R que clasifica las patentes verdes de una base de datos según su similitud con la lista de clasificación de patentes verdes sacada de la WIPO (Organización Mundial de la Propiedad Intelectual) y la UE (Unión Europea).

La forma en la que se obtuvieron las listas de patentes verdes según cada institución fue por medio de un repositorio, hecho a mano dentro de Excel, en donde se registraron los códigos de patentes considerados como verdes para cada una. Teniendo esas listas, estas se homologaron para que coincidieran con el formato que se usó en la base que sería diagnosticada.

Para realizar la clasificación de las patentes verdes de la base de datos se siguieron los pasos descritos a continuación:

* Tras pasar un proceso de limpieza de datos, en primer instancia el código selecciona dos subconjuntos de la base de datos original de patentes que serán utilizadas para cada clasificación: una consulta que selecciona todas las columnas, con excepción de las columnas “IPC” e “IPC Clases”, y que se utilizará para hacer la clasificación de IPC; y otra con todas las columnas con excepción de “CPC” y “CPC clases” pues se utilizará para la clasificación de CPC.
* Luego, define dos funciones (ipcWIPO y cpcWIPO) que se encargan de buscar las patentes que coincidan en términos de IPC o CPC entre las bases de datos de patentes y las de clasificaciones de la WIPO. Si se encuentran coincidencias en las listas, las patentes se etiquetan con un valor de 1 y se extrae el valor coincidente en otra columna. 
* A continuación, se unen las dos tablas resultantes en una sola y agrega una columna que indica si una patente se clasifica como una invención verde (verdadera o falsa) según los criterios del WIPO. Posteriormente, repite el proceso para hacer la clasificación según la lista de EU con las funciones (ipcEU y cpcEU).
* Una vez identificadas las patentes verdes para cada institución, se unen ambos dataframes. De esta forma se podrá identificar, por medio de una columna dicotómica si, si una patente es verde o no ya sea por una, por la otra o por ambas instituciones.
* Posteriormente el código se encargará de identificar las patentes cuya clase fue clasificada como verde por ambas instituciones extrayendo dicha clase.
* Finalmente, se orderará ese dataframe y podrá descargarse junto con los anteriores.

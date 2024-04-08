rm(list = ls())

library(RSQLite)
library(DBI)
library(data.table)
library(openxlsx)
library(ggplot2)

## función para dejar mayusculas, corregir tildes y errores de ortografía
cambiarTildes <- function(x){
	palabras <- data.frame(tilde = c("Á", "É", "Í", "Ó", "Ú", "Ñ", "\\s+"),
		                SinTilde = c("A", "E", "I", "O", "U", "N", " "))
    y  <- toupper(x)
   for(i in 1:nrow(palabras)){
     y <- gsub(palabras[i, "tilde"], palabras[i, "SinTilde"], y)
     y <- gsub("[^A-Z]", "", y)
    }
	return(y)
}

## Establecer directorio de trabajo 
setwd("C:/Users/Asus/Documents/Leonardo/Trabajos/Adres")

## Leer las bases de datos
Municipios <- data.table(read.xlsx(file.path("Municipios", "Municipios.xlsx")))
Prestadores <- data.table(read.xlsx(file.path("Prestadores", "Prestadores.xlsx"))) # Se cambio la extensión del archivo por problemas de lectura

# Corregir errores de campos
Municipios[, Departamento := cambiarTildes(Departamento)]
Municipios[, Municipio := cambiarTildes(Municipio)]
Prestadores[, depa_nombre := cambiarTildes(depa_nombre)]
Prestadores[, muni_nombre := cambiarTildes(muni_nombre)]

# Cargar las bases a SQLite

BasePrueba <- dbConnect(SQLite(), dbname = "BasePrueba.sqlite")
dbWriteTable(BasePrueba, "Municipios", Municipios)
dbWriteTable(BasePrueba, "Prestadores", Prestadores)

# Consulta 

# Contar la cantidad de prestadores por municipio y ver la cantidad de metros cuadrados que debería cubrir
# SQL

inicio <- Sys.time()

conteoPrestadores <- dbGetQuery(BasePrueba, "SELECT 
    M.departamento,
    M.municipio,
    COUNT(DISTINCT P.nombre_prestador) AS cantidad_prestadores,
    ((M.Superficie)/COUNT(DISTINCT P.nombre_prestador)) AS tasa_prestadores
FROM 
    Municipios M
JOIN 
    Prestadores P ON M.departamento = P.depa_nombre AND M.Municipio = P.muni_nombre
GROUP BY 
    M.departamento,
    M.municipio;")

fin <- Sys.time()

tiempoSQL <- fin - inicio

# R
inicio <- Sys.time()

Base <- merge(Prestadores, Municipios, 
	by.x = c("depa_nombre", "muni_nombre"),
	by.y = c("Departamento", "Municipio")) 

conteoPrestadoresR <- unique(Base[, .(cantidadPrestadores = .N, tasa_prestadores = Superficie/.N), 
	by = c("depa_nombre", "muni_nombre")])

fin <- Sys.time()
tiempoR <- fin - inicio

cat("Tiempo SQL: ", tiempoSQL, "\n Tiempo R: ", tiempoR)

conteoPrestadoresDeparR <- unique(Base[, .(cantidadPrestadores = .N), 
	by = "depa_nombre"])

# Cantidad de prestadores por departamento
conteoPrestadoresDeparR[order(cantidadPrestadores), ]

# Cantidad de prestadores por depar
NPrestadores <- unique(Base[, .(.N, Poblacion), by = c("depa_nombre", "muni_nombre")])[, NPrestadores := Poblacion/N]

NPrestadores[order(NPrestadores), ]

# Análisis descriptivo

# Medidas resumen de variables numéricas
summary(Municipios[, c("Superficie", "Poblacion", "Irural")])

# Conteos de variables categóricas
Municipios[, table(Departamento)] #Cantidad de Municipios por departamento
Municipios[, table(Region)] #Cantidad de Municipios por region
conteoPob <- Municipios[, .(Poblacion = sum(Poblacion)), by = "Region"]

# Grafico de población por región

ggplot(conteoPob) +
	geom_col(aes(x = Region, y = Poblacion/1000000, fill = Region)) + 
	geom_text(aes(x = Region, y = Poblacion/1000000, label = Poblacion),
		size = 3.5, 
        vjust = 0, show.legend = F) + 
	guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = 45 ,hjust = 0.9, size = 10)) + 
    labs(y = "Población en Millones", 
    	 x = "")

# Conteo de prestadores habilitados
Prestadores[, table(habilitado, useNA = "ifany")]

# Conteo de clpr_nombre
Prestadores[, table(clpr_nombre, useNA = "ifany")]
Prestadores[, table(clpr_codigo, useNA = "ifany")] # Código

# 

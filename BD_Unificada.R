# BASE DE DATOS UNIFICADA

## Leer daatos ----
economiatd<- read.csv2("C:\\Users\\Yerazo\\Documents\\Documents\\SIGMAP\\Proyecto\\TerriData_Economia.txt", 
                       header = TRUE, encoding="UTF-8", dec = ".")
indicadores<- read.csv2("C:\\Users\\Yerazo\\Documents\\Documents\\SIGMAP\\Proyecto\\Indicadores-Corrupcion-Consolidado.csv",
                        header = TRUE, encoding="UTF-8")

## Ajustes ----
## Economía ----
### Ajustar los nombres de la base de datos de economía
economiatd<- economiatd %>%
  rename(cod_departamento = "Código.Departamento", 
         departamento = "Departamento", 
         cod_entidad = "Código.Entidad", 
         entidad = "Entidad", 
         dimension = "Dimensión", 
         subcategoria = "Subcategoría", 
         indicador = "Indicador", 
         dato_numerico = "Dato.Numérico", 
         dato_cualitativo = "Dato.Cualitativo", 
         anno = "Año", 
         mes = "Mes", 
         fuente = "Fuente", 
         unidad_medida = "Unidad.de.Medida")
### Filtros
economiatd<- economiatd[!is.na(economiatd$dato_numerico),]
economiatd<- economiatd %>% filter(anno >=2014)
economiatd<- economiatd %>% filter(departamento != "Colombia")
### Generar una nueva columna con el nombre del departamento-municipio
economiatd<- unite(economiatd, depto_mun, c("departamento", "entidad"), sep = "-", remove = FALSE)
### Pasar a minúsculas 
economiatd$depto_mun<-str_to_lower(string = economiatd$depto_mun)
### Eliminar tildes
economiatd$depto_mun<-chartr('áéíóú', 'aeiou', economiatd$depto_mun)


## Indicadores ----
### Ajustar nombres de algunas entidades territoriales para que coincidad en ambas BDs
indicadores<-as.data.table(indicadores)
indicadores[departamento_entidad=="Bogotá D.C." , departamento_entidad:="Bogotá"]
indicadores[departamento_entidad=="Bogotá" , municipio_entidad:="Bogotá"]
indicadores[departamento_entidad=="Norte De Santander", departamento_entidad:="Norte de Santander"]
### Generar una nueva columna con el nombre del departamento-municipio
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)
### Pasar a minúsculas y eliminar tildes
indicadores$depto_mun<-str_to_lower(string = indicadores$depto_mun)
### Eliminar tildes
indicadores$depto_mun<-chartr('áéíóú', 'aeiou', indicadores$depto_mun)


## Emparejamiento ---- 
### Conjunto de dato para ver cuántos municipios distintos hay (748)
municipios_general <- indicadores %>% dplyr::select(depto_mun) %>% distinct() %>% rename(indicadores_depto_mun=depto_mun)

### Algoritmo de emparejamiento
for(i in 1: nrow(municipios_general)){   
  # metodo estadistico
  valor_z <- stringdist(as.character(
    municipios_general[i, "indicadores_depto_mun"]), 
    as.character(economiatd$depto_mun), 
    method = "jw", 
    p = 0.1)
  
  posicion <- which.min(valor_z)
  
  # Emparejar valores de codigo dto y mun según el grado de similitud
  if(valor_z[posicion] < 0.15){
    municipios_general[i, "valor_emparejamiento"] <- valor_z[posicion]
    municipios_general[i, "depto_mun_economiatd"] <- economiatd[posicion, "depto_mun"]
    municipios_general[i, "cod_entidad"] <- as.character(economiatd[posicion, "cod_entidad"])
  } 
  else{
    municipios_general[i, "valor_emparejamiento"] <- 1
    municipios_general[i, "depto_mun_economiatd"] <- "ninguno"
    municipios_general[i, "cod_entidad"] <- "ninguno"
  }
}

print(paste("Existen", nrow(municipios_general %>% 
                              filter(valor_emparejamiento > 0.00)),"registros críticos" , sep = " "))

## Ajuste manual de registros críticos
### Reemplazar los datos de indicadores como los nombres de economiatd
### (se hace así para que no corran algunas líneas de código y no tengan que crear la tabla municipios_general)
### caso especial: No existe en economíatd Sucre-Sincé , Bolívar-Santa Rosa de Lima, 
indicadores[depto_mun=="Cauca-Paez/Belalcazar" , municipio_entidad:="Paéz"]
indicadores[depto_mun=="Boyacá-Chíquiza (San Pedro de Iguaque)" , municipio_entidad:="Chíquiza"]
indicadores[depto_mun=="Cesar-Manaure Balcon del Cesar" , municipio_entidad:="Manaure"]
indicadores[depto_mun=="Cauca-López de Micay" , municipio_entidad:="López"]
indicadores[depto_mun=="Huila-El Pital" , municipio_entidad:="Pital"]
indicadores[depto_mun=="Antioquia-Carolina del Príncipe" , municipio_entidad:="Carolina"]
indicadores[depto_mun=="Nariño-Cuaspud/Carlosama" , municipio_entidad:="Cuaspud"]
indicadores[depto_mun=="Norte de Santander-San José de Cúcuta" , municipio_entidad:="Cúcuta"]
indicadores[depto_mun=="Tolima-El Espinal" , municipio_entidad:="Espinal"]
indicadores[depto_mun=="Putumayo-San Miguel (La Dorada)" , municipio_entidad:="San Miguel"]
indicadores[depto_mun=="Antioquia-El Peñol" , municipio_entidad:="Peñol"]
indicadores[depto_mun=="Putumayo-San Miguel de Mocoa" , municipio_entidad:="Mocoa"]
indicadores[depto_mun=="Sucre-Toluviejo" , municipio_entidad:="Tolú Viejo"]
indicadores[depto_mun=="Boyacá-Gameza" , municipio_entidad:="Gámeza"]
indicadores[depto_mun=="Bolívar-San Estanislao de Kostka" , municipio_entidad:="San Estanislao"]
indicadores[depto_mun=="Nariño-Roberto Payán/San José" , municipio_entidad:="Roberto Payán"]
indicadores[depto_mun=="Putumayo-Valle del Guamuez/La Hormiga" , municipio_entidad:="Valle del Guamuez"]
indicadores[depto_mun=="Tolima-San Sebastian de Mariquita" , municipio_entidad:="San Sebastián de Mariquita"]
indicadores[depto_mun=="Cundinamarca-Caqueza" , municipio_entidad:="Cáqueza"]
indicadores[depto_mun=="Tolima-El Guamo" , municipio_entidad:="Guamo"]
indicadores[depto_mun=="Tolima-El Líbano" , municipio_entidad:="Líbano"]
indicadores[depto_mun=="Sucre-Coloso" , municipio_entidad:="Colosó"]
indicadores[depto_mun=="Cauca-Toribio" , municipio_entidad:="Toribío"]
indicadores[depto_mun=="Nariño-San Andres de Tumaco" , municipio_entidad:="San Andrés de Tumaco"]
indicadores[depto_mun=="Antioquia-San Pedro de los Milagros" , municipio_entidad:="San Pedro de Los Milagros"]
indicadores[depto_mun=="Antioquia-Anza" , municipio_entidad:="Anzá"]
indicadores[depto_mun=="Nariño-Consaca" , municipio_entidad:="Consacá"]
indicadores[depto_mun=="Quindío-Calarca" , municipio_entidad:="Calarcá"]
indicadores[depto_mun=="Antioquia-Yolombo" , municipio_entidad:="Yolombó"]
indicadores[depto_mun=="Santander-Lebríja" , municipio_entidad:="Lebrija"]
indicadores[depto_mun=="Cundinamarca-Fomeque" , municipio_entidad:="Fómeque"]
indicadores[depto_mun=="Valle del Cauca-Jamundi" , municipio_entidad:="Jamundí"]
indicadores[depto_mun=="Norte de Santander-Abrego" , municipio_entidad:="Ábrego"]
indicadores[depto_mun=="Atlántico-Campo de la Cruz" , municipio_entidad:="Campo de La Cruz"]
indicadores[depto_mun=="Antioquia-San Pedro de Uraba" , municipio_entidad:="San Pedro de Urabá"]
indicadores[depto_mun=="La Guajira-La Jagua Del Pilar" , municipio_entidad:="La Jagua del Pilar"]
indicadores[depto_mun=="Caquetá-Belén de Los Andaquies" , municipio_entidad:="Belén de Los Andaquíes"]
indicadores[depto_mun=="Magdalena-Sabanas de San Angel" , municipio_entidad:="Sabanas de San Ángel"]
indicadores[depto_mun=="Antioquia-San Andrés de Cuerquia" , municipio_entidad:="San Andrés de Cuerquía"]
indicadores[depto_mun=="Antioquia-San José de la Montaña" , municipio_entidad:="San José de La Montaña"]

### Reemplazr la columna con el nombre del departamento-municipio para volver a correr el for
indicadores$depto_mun<-NULL
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)


## Extraer datos ---- 
### Departamentos
### Separar datos por departamentos y pasar los indicadores a columnas
Economia_depto <- economiatd %>% 
  filter( subcategoria=="PIB" | subcategoria=="PIB por grandes ramas de actividad económica" | 
            subcategoria=="Porcentaje del PIB por grandes ramas de actividad económica", 
          departamento == entidad, departamento != "Colombia", departamento!= "Bogotá") %>% 
  group_by(depto_mun, cod_departamento, departamento, cod_entidad, entidad, dimension,indicador) %>%
  summarise(dato=mean(dato_numerico))  %>% pivot_wider(names_from = indicador, values_from=dato)
### Unir indicadores general con los indicadores departamentales (recordar problema con San Andrés y Bogotá)
indicadores_BD<- merge(indicadores, Economia_depto, by.x= "departamento_entidad", by.y= "departamento", all.x=TRUE)
### Eliminar columnas inncesarias después del merge
indicadores_BD$depto_mun.y<- NULL
indicadores_BD$dimension<- NULL
indicadores_BD$cod_departamento<-NULL
indicadores_BD$cod_entidad<-NULL
indicadores_BD$entidad<-NULL

### Municipios
### Separar datos por municipios y pasar los indicadores a columnas
Economia_municipio <- economiatd %>%
         filter( economiatd$depto_mun %in% indicadores$depto_mun & subcategoria=="Valor agregado - Base 2015") %>% 
        group_by(depto_mun,cod_entidad, indicador) %>% summarise(dato=mean(dato_numerico)) %>% 
        pivot_wider(names_from = indicador, values_from=dato)

### Unir indicadores_BD general con los indicadores municipio (recordar problema con San Andrés y Bogotá)
indicadores_BD<- merge(indicadores_BD, Economia_municipio, by.x="depto_mun.x", by.y="depto_mun",all.x= FALSE,  all.y=TRUE)
### Se dejaron 746 municipios***

### Cambiar los NAs por la media de su columna
indicadores_BD<-as.data.frame(indicadores_BD)
columnas_numericas <- which(sapply(indicadores_BD, is.numeric))
cols_mean <- rep(NA, ncol(indicadores_BD))
cols_mean[columnas_numericas] <- colMeans(indicadores_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  indicadores_BD[is.na(indicadores_BD[,x]), x] <- cols_mean[x]
}


write.csv(indicadores_BD, file = "indicadores_BD.csv", row.names = TRUE)

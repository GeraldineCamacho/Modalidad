# BASE DE DATOS UNIFICADA 
# ECONOMÍA E INDICADORES ----
## Economia TERRIDATA ----
### Leer daatos
economiatd<- read.csv2("TerriData_Economia.txt", 
                       header = TRUE, 
                       encoding="UTF-8", dec = ".")

indicadores<- read.csv2("Indicadores-Corrupcion-Consolidado.csv",
                        header = TRUE, 
                        encoding="UTF-8")

### Ajustes 
### Economía 
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

### Indicadores
### Ajustar nombres de algunas entidades territoriales para que coincidad en ambas BDs
indicadores<-as.data.table(indicadores)
indicadores[departamento_entidad=="Bogotá D.C." , departamento_entidad:="Bogotá"]
indicadores[departamento_entidad=="Bogotá" , municipio_entidad:="Bogotá"]
indicadores[departamento_entidad=="Norte De Santander", departamento_entidad:="Norte de Santander"]
### Generar una nueva columna con el nombre del departamento-municipio
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)

### Ajuste manual de registros críticos
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

### Reemplazr la columna con el nombre del departamento-municipio
indicadores$depto_mun<-NULL
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)

### Extraer datos 
### Departamentos
### Separar datos por departamentos media
Economia_depto_m <- economiatd %>% 
  filter( subcategoria == "PIB" |  
            subcategoria == "Porcentaje del PIB por grandes ramas de actividad económica" &
            departamento == entidad, 
          departamento != "Colombia", 
          departamento!= "Bogotá") %>% 
  group_by(cod_departamento, departamento,indicador) %>%
  summarise(dato = mean(dato_numerico))  

### Reemplazar los nombres de los indicadores de la media
Economia_depto_m<-as.data.table(Economia_depto_m)
Economia_depto_m[indicador == "% Actividades de servicios sociales, comunales y personales" , 
                 indicador := "inec_%actividades_serviciossociales_comunales_personales_dto_m"]
Economia_depto_m[indicador == "% Agricultura, ganadería, caza, silvicultura y pesca" , 
                 indicador := "inec_%agricultura_ganaderia_caza_silvicultura_peza_dto_m"]
Economia_depto_m[indicador == "% Comercio, reparación, restaurantes y hoteles" , 
                 indicador := "inec_%comercio_reparacion_restaurantes_hoteles_dto_m"]
Economia_depto_m[indicador == "% Construcción" , 
                 indicador := "inec_%construccion_dto_m"]
Economia_depto_m[indicador == "% Establecimientos financieros, inmobiliarias y servicios a empresas" , 
                 indicador := "inec_%establecimientos_finan_inmob_servaempresas_dto_m"]
Economia_depto_m[indicador == "% Explotación de minas y canteras" , 
                 indicador := "inec_%explotacion_minas_canteras_dto_m"]
Economia_depto_m[indicador == "% Impuestos" , 
                 indicador := "inec_%impuestos_dto_m"]
Economia_depto_m[indicador == "% Industria manufacturera" , 
                 indicador := "inec_%industria_manufact_dto_m"]
Economia_depto_m[indicador == "% Suministro de electricidad, gas y agua" , 
                 indicador := "inec_%suministro_electricidad_gas_agua_dto_m"]
Economia_depto_m[indicador == "% Transporte, almacenamiento y comunicaciones" , 
                 indicador := "inec_%transp_almac_comunicaciones_dto_m"]
Economia_depto_m[indicador == "PIB" , 
                 indicador := "inec_PIB_dto_m"]
Economia_depto_m[indicador == "PIB per cápita" , 
                 indicador := "inec_PIB_percapita_dto_m"]
Economia_depto_m[indicador == "PIB per cápita como porcentaje del promedio nacional" , 
                 indicador := "inec_PIB_percapita_%prom_nacional_dto_m"]
Economia_depto_m[indicador == "Porcentaje de contribución al PIB nacional" , 
                 indicador := "inec_%contribucion_alPIBnacional_dto_m"]

### Pasar los indicadores de la media a columnas
Economia_depto_m <- Economia_depto_m %>% 
  pivot_wider(names_from = indicador, values_from=dato)

### Separar datos por departamentos desviación estándar
Economia_depto_sd <- economiatd %>% 
  filter( subcategoria == "PIB" |  
            subcategoria == "Porcentaje del PIB por grandes ramas de actividad económica" &
            departamento == entidad, 
          departamento != "Colombia", 
          departamento!= "Bogotá") %>% 
  group_by(departamento,indicador) %>%
  summarise(dato = sd(dato_numerico))  

### Reemplazar los nombres de los indicadores de la media
Economia_depto_sd<-as.data.table(Economia_depto_sd)
Economia_depto_sd[indicador == "% Actividades de servicios sociales, comunales y personales" , 
                  indicador := "inec_%actividades_serviciossociales_comunales_personales_dto_sd"]
Economia_depto_sd[indicador == "% Agricultura, ganadería, caza, silvicultura y pesca" , 
                  indicador := "inec_%agricultura_ganaderia_caza_silvicultura_peza_dto_sd"]
Economia_depto_sd[indicador == "% Comercio, reparación, restaurantes y hoteles" , 
                  indicador := "inec_%comercio_reparacion_restaurantes_hoteles_dto_sd"]
Economia_depto_sd[indicador == "% Construcción" , 
                  indicador := "inec_%construccion_dto_sd"]
Economia_depto_sd[indicador == "% Establecimientos financieros, inmobiliarias y servicios a empresas" , 
                  indicador := "inec_%establecimientos_finan_inmob_servaempresas_dto_sd"]
Economia_depto_sd[indicador == "% Explotación de minas y canteras" , 
                  indicador := "inec_%explotacion_minas_canteras_dto_sd"]
Economia_depto_sd[indicador == "% Impuestos" , 
                  indicador := "inec_%impuestos_dto_sd"]
Economia_depto_sd[indicador == "% Industria manufacturera" , 
                  indicador := "inec_%industria_manufact_dto_sd"]
Economia_depto_sd[indicador == "% Suministro de electricidad, gas y agua" , 
                  indicador := "inec_%suministro_electricidad_gas_agua_dto_sd"]
Economia_depto_sd[indicador == "% Transporte, almacenamiento y comunicaciones" , 
                  indicador := "inec_%transp_almac_comunicaciones_dto_sd"]
Economia_depto_sd[indicador == "PIB" , 
                  indicador := "inec_PIB_dto_sd"]
Economia_depto_sd[indicador == "PIB per cápita" , 
                  indicador := "inec_PIB_percapita_dto_sd"]
Economia_depto_sd[indicador == "PIB per cápita como porcentaje del promedio nacional" , 
                  indicador := "inec_PIB_percapita_%prom_nacional_dto_sd"]
Economia_depto_sd[indicador == "Porcentaje de contribución al PIB nacional" , 
                  indicador := "inec_%contribucion_alPIBnacional_dto_sd"]

### Pasar los indicadores de la media a columnas
Economia_depto_sd <- Economia_depto_sd %>% 
  pivot_wider(names_from = indicador, values_from=dato)

### Base de datos de Economías con los indicadores por media y sd
Economia_depto <- merge(Economia_depto_m, Economia_depto_sd, by = "departamento")

### Unir indicadores general con los indicadores departamentales (recordar problema con San Andrés y Bogotá)
indicadores_BD<- merge(indicadores, Economia_depto, by.x= "departamento_entidad", by.y= "departamento", all.x=TRUE)

### Municipios
### Municipios que están en ambas DBs
economiatdmun <- economiatd %>% filter(economiatd$depto_mun %in% indicadores$depto_mun)
### Separar datos por municipios media
Economia_municipio_m <- economiatdmun %>%
  filter(   indicador == "Participación del valor agregado municipal en el departamental" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades primarias" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades secundarias" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades terciarias" |
              indicador == "Valor agregado" |
              indicador == "Valor agregado per cápita" |
              indicador == "Valor agregado per cápita como porcentaje del promedio nacional" ) %>% 
  group_by(depto_mun,cod_entidad, indicador) %>% 
  summarise(dato=mean(dato_numerico)) 

### Reemplazar los nombres de los indicadores de la media
Economia_municipio_m<-as.data.table(Economia_municipio_m)
Economia_municipio_m[indicador == "Participación del valor agregado municipal en el departamental" , 
                     indicador := "inec_particip_vagregado_muni_en_deptal_mun_m"]
Economia_municipio_m[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades primarias" , 
                     indicador := "inec_%vagregado_actividades_primarias_mun_m"]
Economia_municipio_m[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades secundarias" , 
                     indicador := "inec_%vagregado_actividades_secundarias_mun_m"]
Economia_municipio_m[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades terciarias" , 
                     indicador := "inec_%vagregado_actividades_terciarias_mun_m"]
Economia_municipio_m[indicador == "Valor agregado" , 
                     indicador := "inec_valor_agregado_mun_m"]
Economia_municipio_m[indicador == "Valor agregado per cápita" , 
                     indicador := "inec_valor_agregado_percapita_mun_m"]
Economia_municipio_m[indicador == "Valor agregado per cápita como porcentaje del promedio nacional" , 
                     indicador := "inec_valor_agregado_percapita_como%prom_nacional_mun_m"]

### Pasar los indicadores de la media a columnas
Economia_municipio_m <- Economia_municipio_m %>% 
  pivot_wider(names_from = indicador, values_from=dato)

### Separar datos por municipios desviacion estándar
Economia_municipio_sd <- economiatdmun %>%
  filter(   indicador == "Participación del valor agregado municipal en el departamental" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades primarias" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades secundarias" |
              indicador == "Porcentaje del valor agregado por actividades económicas - Actividades terciarias" |
              indicador == "Valor agregado" |
              indicador == "Valor agregado per cápita" |
              indicador == "Valor agregado per cápita como porcentaje del promedio nacional" ) %>% 
  group_by(cod_entidad, indicador) %>% 
  summarise(dato=sd(dato_numerico)) 

### Reemplazar los nombres de los indicadores de la media
Economia_municipio_sd<-as.data.table(Economia_municipio_sd)
Economia_municipio_sd[indicador == "Participación del valor agregado municipal en el departamental" , 
                      indicador := "inec_particip_vagregado_muni_en_deptal_mun_sd"]
Economia_municipio_sd[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades primarias" , 
                      indicador := "inec_%vagregado_actividades_primarias_mun_sd"]
Economia_municipio_sd[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades secundarias" , 
                      indicador := "inec_%vagregado_actividades_secundarias_mun_sd"]
Economia_municipio_sd[indicador == "Porcentaje del valor agregado por actividades económicas - Actividades terciarias" , 
                      indicador := "inec_%vagregado_actividades_terciarias_mun_sd"]
Economia_municipio_sd[indicador == "Valor agregado" , 
                      indicador := "inec_valor_agregado_mun_sd"]
Economia_municipio_sd[indicador == "Valor agregado per cápita" , 
                      indicador := "inec_valor_agregado_percapita_mun_sd"]
Economia_municipio_sd[indicador == "Valor agregado per cápita como porcentaje del promedio nacional" , 
                      indicador := "inec_valor_agregado_percapita_como%prom_nacional_mun_sd"]

### Pasar los indicadores de la media a columnas
Economia_municipio_sd <- Economia_municipio_sd %>% 
  pivot_wider(names_from = indicador, values_from=dato)

### Base de datos de Economías con los indicadores por media y sd
Economia_municipio <- merge(Economia_municipio_m, Economia_municipio_sd, by = "cod_entidad")

### Unir indicadores_BD general con los indicadores municipio (recordar problema con San Andrés y Bogotá)
indicadores_BD<- merge(indicadores_BD, Economia_municipio, by="depto_mun",all.x= FALSE,  all.y=TRUE)
### Se dejaron 746 municipios***

### Odernar el nombre de las columnas
nombres <- c("nombre_entidad", "nit_entidad" , "orden_entidad", 
             "departamento_entidad", "cod_departamento",
             "municipio_entidad", "depto_mun","cod_entidad")
indicadores_BD_O1 <- indicadores_BD %>% dplyr::select(!nombres)
nombres_BD <- c(nombres, names(indicadores_BD_O1))
indicadores_BD <- indicadores_BD %>% dplyr::select(nombres_BD)

### Cambiar los NAs por la media de su columna
indicadores_BD<-as.data.frame(indicadores_BD)
columnas_numericas <- which(sapply(indicadores_BD, is.numeric))
cols_mean <- rep(NA, ncol(indicadores_BD))
cols_mean[columnas_numericas] <- colMeans(indicadores_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  indicadores_BD[is.na(indicadores_BD[,x]), x] <- cols_mean[x]
}

write.csv(indicadores_BD, file = "indicadores_BD.csv", row.names = TRUE)

# UNIR BASES DE DATOS RESTANTES  ----

## MDM (Medición del Desempeño Municipal) ----
### Leer datos
library(readxl)
MDM <- read_excel("Documents/SIGMAP/Proyecto/Resultados_2018_MDM.xlsx")

### Ajustar nombres
nombres_mdm <- c("cod_entidad", "depto" , "municipio" , "grupo_cap_iniciales",
                 "inmdm_gestion_mov_recursos_mun", "inmdm_gestion_ejec_recursos_mun",
                 "inmdm_gestion_ord_territorial_mun", "inmdm_gobno_abierto_transparencia_mun",
                 "inmdm_puntaje_gestion_mun", "puesto_gestion_grupo_capacidades",
                 "inmdm_res_educacion_mun", "inmdm_res_salud_mun", "inmdm_res_servicios_mun",
                 "inmdm_res_seguridad_mun", "ind_puntaje_res_2017_mun", "ind_ppuntaje_res_2018_mun",
                 "inmdm_ajuste_por_resultados_mun", "puesto_resultados_grupo_capacidades", 
                 "inmdm_puntaje_mdm_2018", "puesto_mdm_grupo_capacidades", "clasificacion")
names(MDM) <- nombres_mdm

### Seleccionar datos
MDM <- MDM %>% dplyr::select(cod_entidad, inmdm_gestion_mov_recursos_mun, inmdm_gestion_ejec_recursos_mun,
                             inmdm_gestion_ord_territorial_mun, inmdm_gobno_abierto_transparencia_mun, 
                             inmdm_puntaje_gestion_mun,ind_ppuntaje_res_2018_mun, inmdm_puntaje_mdm_2018)

### Agregar MDM a la base de datos de los indicadores
indicadores_BD <- merge(indicadores_BD, MDM, by = "cod_entidad", all.x = TRUE, all.y = FALSE)

write.csv(indicadores_BD, file = "indicadores_BD.csv", row.names = TRUE)

## Educacion de TERRIDATA ----
### Lectura de datos
library(readr)
educacion_BD <- read_csv("Documents/SIGMAP/Proyecto/eleccionGlobal_educacion.csv")

### Tratamiento de datos
educacion_BD <- educacion_BD %>% filter(educacion_BD$cod_entidad %in% indicadores_BD$cod_entidad)
educacion_BD$cod_departamento <- NULL
educacion_BD$ine_porcentAsistencia_mun_sd <- NULL
educacion_BD$ine_tasaAnalfabetismo_mun_sd <- NULL

### Cambiar los NAs por la media de su columna
educacion_BD<-as.data.frame(educacion_BD)
columnas_numericas <- which(sapply(educacion_BD, is.numeric))
cols_mean <- rep(NaN, ncol(educacion_BD))
cols_mean[columnas_numericas] <- colMeans(educacion_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  educacion_BD[is.na(educacion_BD[,x]), x] <- cols_mean[x]
}

### Probar si existen nulos
sapply(educacion_BD, function(x) sum(is.na(x)))

### Agregar educacion a indicadores_bd
indicadores_BD <- merge(indicadores_BD, educacion_BD, by = "cod_entidad", all.x = TRUE, all.y = FALSE)

## financiero TERRIDATA ----
financiero_BD <- read_csv("Documents/SIGMAP/Proyecto/ind_financiero_total.csv")

### Tratamiento de datos
financiero_BD <- financiero_BD %>% filter(financiero_BD$codigo_entidad %in% indicadores_BD$cod_entidad)
financiero_BD$codigo_departamento <- NULL

### Probar si existen nulos
sapply(financiero_BD, function(x) sum(is.na(x)))

### Cambiar los NAs por la media de su columna
financiero_BD<-as.data.frame(financiero_BD)
columnas_numericas <- which(sapply(financiero_BD, is.numeric))
cols_mean <- rep(NaN, ncol(financiero_BD))
cols_mean[columnas_numericas] <- colMeans(financiero_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  financiero_BD[is.na(financiero_BD[,x]), x] <- cols_mean[x]
}

### Probar si existen nulos
sapply(financiero_BD, function(x) sum(is.na(x)))

### Agregar financiero a indicadores_bd
indicadores_BD <- merge(indicadores_BD, financiero_BD, 
                        by.x = "cod_entidad", by.y = "codigo_entidad", 
                        all.x = TRUE, all.y = FALSE)

## Salud TERRIDATA
salud_BD <- read_csv("Documents/SIGMAP/Proyecto/Salud_Global_indicadores.csv")

### Tratamiento de datos
salud_BD$cod_departamento <- NULL

### Probar si existen nulos
sapply(salud_BD, function(x) sum(is.na(x)))

### Cambiar los NAs por la media de su columna
salud_BD<-as.data.frame(salud_BD)
columnas_numericas <- which(sapply(salud_BD, is.numeric))
cols_mean <- rep(NaN, ncol(salud_BD))
cols_mean[columnas_numericas] <- colMeans(salud_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  salud_BD[is.na(salud_BD[,x]), x] <- cols_mean[x]
}

### Probar si existen nulos
sapply(salud_BD, function(x) sum(is.na(x)))

### Agregar salud a indicadores_bd
indicadores_BD <- merge(indicadores_BD, salud_BD, by = "cod_entidad", all.x = TRUE, all.y = FALSE)

### Probar si existen nulos
sapply(indicadores_BD, function(x) sum(is.na(x)))

### Cambiar los NAs por la media de su columna
indicadores_BD<-as.data.frame(indicadores_BD)
columnas_numericas <- which(sapply(indicadores_BD, is.numeric))
cols_mean <- rep(NA, ncol(indicadores_BD))
cols_mean[columnas_numericas] <- colMeans(indicadores_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  indicadores_BD[is.na(indicadores_BD[,x]), x] <- cols_mean[x]
}

write.csv(indicadores_BD, file = "indicadores_BD.csv", row.names = TRUE)

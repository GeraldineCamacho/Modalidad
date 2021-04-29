# Indicadores 

## 1. Librerias ----
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.align = "center")
library(tidyverse)
library(dplyr)
library(vroom)

## 2. Carga de conjunto de datos ----
#SECOP <- read_csv(file = "secop_i_ips_limpia.csv", locale = locale(encoding = "UTF-8"))


SECOP <- read_delim("SECOP_IPS_LIMPIA_2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#filas = 1190792
#filas 1018186

#NOTA: No se tiene codigo de entidad y depto
## Tener en cuenta:
# No se puede calcular las de SECOP II:
# 4.1 Promedio de oferentes por proceso de selección 
# 4.8 Índice de participación de oferentes.
#Junte por NIT entidad

## 3. Limpieza----


# nombre_entidad	nit_entidad	orden_entidad	departamento_entidad	municipio_entidad total_contratos	valor_total	valor_mediana	valor_promedio	valor_desviacion	perc_adic_valor	perc_adic_tiempo	perc_contr_directa_num	perc_contr_direct_val	HHI_cant	HHI_val	ID_cant	ID_val	ganadoras	IC4K_cant	IC4K_val	ind_riesgo_corrupcion

#Se aplica el siguiente filtro:
###- Contratos de mas de 1 millones.
###- Entidades con mínimo 3 contratos

# Quitar contratos con valor menor a 1 millon 
SECOP_I <- SECOP %>% 
  filter(cuantia_contrato >= 1e6)

# Consultar las entidades con menos de 3 contratos 
entidades_del <- SECOP_I %>% 
  group_by(nombre_entidad) %>% 
  summarise(contratos = n()) %>% 
  filter(contratos < 3)

# Quitar las entidades con menos de 3 contratos
SECOP_I <- SECOP_I %>% 
  filter(!(nombre_entidad %in% entidades_del$nombre_entidad))

# Cargar datos
#contratos <- SECOP_I %>% select(nombre_entidad, nit_entidad, orden_entidad, departamento_entidad, municipio_entidad) %>% distinct()

## 4. Indicadores----
## 4. Indicadores----
### 4.1 Grupo1----
Indicadores <- SECOP_I %>%
  group_by(nombre_entidad, nit_entidad, orden_entidad, departamento_entidad, municipio_entidad) %>% 
  summarise(indba_total_cont	 = n(), 
            indba_inicial = sum(cuantia_contrato, na.rm = TRUE),
            indba_valora_total = sum(valor_total_con_adiciones, na.rm = TRUE), 
            indba_valora_median	= median(valor_total_con_adiciones, na.rm = TRUE),
            indba_valora_prom = mean(valor_total_con_adiciones, na.rm = TRUE),
            indba_valora_desv = sd(valor_total_con_adiciones, na.rm = TRUE), .groups = 'drop')

#Cantidad 

temp <- SECOP_I %>%
filter(cuantia_contrato > 0, !is.na(cuantia_contrato)) %>% 
  group_by(nombre_entidad, anno_firma) %>% 
  summarise(dato_numerico = n(), .groups = 'drop') %>% 
  mutate(indicador = "indba_total_cont") %>% 
  unite(indicador_annio, c("indicador", "anno_firma"), sep = "_", remove = TRUE)

temp1 <- temp %>% 
  pivot_wider(names_from = indicador_annio, values_from = dato_numerico) 

Indicadores<- merge(Indicadores,
                          temp1,
                          by.x= "nombre_entidad",
                          by.y= "nombre_entidad",
                          all.x=TRUE)

rm(temp)
rm(temp1)
# Valor total con adiciones

temp <- SECOP_I %>%
  filter(cuantia_contrato > 0, !is.na(cuantia_contrato)) %>% 
  group_by(nombre_entidad, anno_firma) %>% 
  summarise(dato_numerico = sum(cuantia_contrato, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(indicador = "indba_valora_prom") %>% 
  unite(indicador_annio, c("indicador", "anno_firma"), sep = "_", remove = TRUE)

temp1 <- temp %>% 
  pivot_wider(names_from = indicador_annio, values_from = dato_numerico) 

Indicadores<- merge(Indicadores,
                    temp1,
                    by.x= "nombre_entidad",
                    by.y= "nombre_entidad",
                    all.x=TRUE)

rm(temp)
rm(temp1)

#Valor total con adiciones

temp <- SECOP_I %>%
  filter(cuantia_contrato > 0, !is.na(cuantia_contrato)) %>% 
  group_by(nombre_entidad, anno_firma) %>% 
  summarise(dato_numerico = mean(valor_total_con_adiciones, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(indicador = "indba_valor_total") %>% 
  unite(indicador_annio, c("indicador", "anno_firma"), sep = "_", remove = TRUE)

temp1 <- temp %>% 
  pivot_wider(names_from = indicador_annio, values_from = dato_numerico) 

Indicadores<- merge(Indicadores,
                    temp1,
                    by.x= "nombre_entidad",
                    by.y= "nombre_entidad",
                    all.x=TRUE)

rm(temp)
rm(temp1)


### 4.2 GRUPO2 perc_adic_valor----
temp <- SECOP_I %>%
  filter(cuantia_contrato > 0, !is.na(cuantia_contrato)) %>% 
  group_by(nombre_entidad, anno_firma) %>% 
  summarise(dato_numerico = mean((valor_adiciones/cuantia_contrato)), .groups = 'drop') %>% 
  mutate(indicador = "indba_perc_val_adic") %>% 
  unite(indicador_annio, c("indicador", "anno_firma"), sep = "_", remove = TRUE)

  temp1 <- temp %>% 
  pivot_wider(names_from = indicador_annio, values_from = dato_numerico) 
  
  Indicadores<- merge(Indicadores,
                      temp1,
                      by.x= "nombre_entidad",
                      by.y= "nombre_entidad",
                      all.x=TRUE)
  
  rm(temp)
  rm(temp1)
  
  temp <- SECOP_I %>%
    filter(cuantia_contrato > 0, !is.na(cuantia_contrato)) %>% 
    group_by(nombre_entidad) %>% 
    summarise(indba_perc_val_adic = mean((valor_adiciones/cuantia_contrato)), .groups = 'drop')
  
  # Agrupar los indicadores en una sola tabla
  Indicadores <- Indicadores %>% 
    left_join(y = temp, by = "nombre_entidad") %>% 
    mutate(indba_perc_val_adic = ifelse(is.na(indba_perc_val_adic), 0, indba_perc_val_adic))
  rm(temp)
  

### 4.3 GRUPO3 perc_adic_tiempo ----
temp <- SECOP_I %>%
  group_by(nombre_entidad, anno_firma) %>%
  summarise( dato_numerico = sum(( tiempo_adiciones_dias >= "1" || tiempo_adiciones_meses >= "1")/n(),na.rm = TRUE)*100) %>% 
  mutate(indicador = "indba_perc_tiem_adic") %>% 
    unite(indicador_annio, c("indicador", "anno_firma"), sep = "_", remove = TRUE)
  
  temp1 <- temp %>% 
    pivot_wider(names_from = indicador_annio, values_from = dato_numerico) 
  
  Indicadores<- merge(Indicadores,
                      temp1,
                      by.x= "nombre_entidad",
                      by.y= "nombre_entidad",
                      all.x=TRUE)
  
  temp <- SECOP_I %>%
    group_by(nombre_entidad) %>%
    summarise( indba_perc_tiem_adic = sum(( tiempo_adiciones_dias >= "1" || tiempo_adiciones_meses >= "1")/n(),na.rm = TRUE)*100)
  
  # Agrupar los indicadores en una sola tabla
  Indicadores <- Indicadores %>% 
    left_join(y = temp, by = "nombre_entidad")
  rm(temp)

##Indicadores para la Falta de Competencia ----

  # 4.2 Porcentaje de procedimientos que utilizaron adjudicación direct
  
  # Calcula el porcentaje de contratación en numero de contratos
  temp <- SECOP_I %>% 
    group_by(nombre_entidad) %>% 
    summarise(indba_perc_cdirec_num = sum((tipo_proceso == "Contratación Directa (Ley 1150 de 2007)")/n(),na.rm = TRUE)*100)
  
  # Agrupar los indicadores en una sola tabla
  Indicadores <- Indicadores %>% 
    left_join(y = temp, by = "nombre_entidad") %>% 
    mutate(indba_perc_cdirec_num = ifelse(is.na(indba_perc_cdirec_num), 0, indba_perc_cdirec_num))
  rm(temp)

  # 4.2 Porcentaje de procedimientos que utilizaron adjudicación directa o regimen especial
  
  # Calcula el porcentaje de contratación en numero de contratos
  temp <- SECOP_I %>% 
    group_by(nombre_entidad) %>% 
    summarise(indba_perc_cerrada_num = sum((tipo_proceso == "Contratación Directa (Ley 1150 de 2007)" | tipo_proceso == "Régimen Especial")/n(),na.rm = TRUE)*100)
  
  # Agrupar los indicadores en una sola tabla
  Indicadores <- Indicadores %>% 
    left_join(y = temp, by = "nombre_entidad") %>% 
    mutate(indba_perc_cerrada_num = ifelse(is.na(indba_perc_cerrada_num), 0, indba_perc_cerrada_num))
  rm(temp)
  
# 4.3 Porcentaje del valor de procedimientos que utilizaron adjudicación directa o regimen especial     

# Calcula el porcentaje de contratación con respecto al valor de los contratos
temp <- SECOP_I %>% 
  group_by(nombre_entidad) %>% 
  filter(tipo_proceso == "Contratación Directa (Ley 1150 de 2007)" |tipo_proceso == "Régimen Especial") %>%
  summarise(valor_cerrada = sum(valor_total_con_adiciones))

temp1 <- SECOP_I %>% 
  group_by(nombre_entidad) %>% 
  summarise(valor_total = sum(valor_total_con_adiciones))

temp <- temp %>% 
  left_join(y = temp1, by = "nombre_entidad") 
rm(temp1)

temp <- temp %>% 
  mutate(indba_perc_cerrada_val = (valor_cerrada/valor_total)*100) %>% 
  select(nombre_entidad, indba_perc_cerrada_val)

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")%>% 
  mutate(indba_perc_cerrada_val = ifelse(is.na(indba_perc_cerrada_val), 0, indba_perc_cerrada_val))
rm(temp)

# Calcula el porcentaje de contratación con respecto al valor de los contratos
temp <- SECOP_I %>% 
  group_by(nombre_entidad, tipo_proceso) %>% 
  summarise(valor_contratos = sum(valor_total_con_adiciones)) %>% 
  group_by(nombre_entidad) %>% 
  mutate(valor_total = sum(valor_contratos),
         perc_valor = (valor_contratos/valor_total)*100) %>% 
  filter(tipo_proceso == "Contratación Directa (Ley 1150 de 2007)") %>% 
  rename(indba_perc_direc_val = "perc_valor") %>% 
  select(nombre_entidad, indba_perc_direc_val) 

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad") %>% 
  mutate(indba_perc_direc_val = ifelse(is.na(indba_perc_direc_val), 0, indba_perc_direc_val))
rm(temp)


# 4.4 y 4.5 Indice de concentración de contratos HHI

temp<- SECOP_I %>% 
  group_by(nombre_entidad, id_contratista) %>% 
  summarise(cant_contratos = n(),
            val_contratos = sum(valor_total_con_adiciones), na.rm = TRUE) %>% 
  group_by(nombre_entidad) %>% 
  mutate(total_contratos_ent = sum(cant_contratos),
         total_val_contratos_ent = sum(val_contratos),
         si_cant = (cant_contratos/total_contratos_ent)*100,
         si2_cant = si_cant^2,
         si_val = (val_contratos/total_val_contratos_ent)*100,
         si2_val = si_val^2) %>% 
  group_by(nombre_entidad) %>% 
  summarise(indba_HHI_cant = (sum(si2_cant)/10000)*100,
            indba_HHI_val = (sum(si2_val)/10000)*100)

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")
rm(temp)

# 4.6 y 4.7 Indice de diversidad - ID
#El indice no se puede aplicar a entidades con 1 solo contrato

temp <- SECOP_I %>% 
  group_by(nombre_entidad, id_contratista) %>% 
  summarise(nj_cant = n(),
            nj_val = sum(valor_total_con_adiciones),
            sub_cant = nj_cant*(nj_cant - 1),
            sub_val = nj_val*(nj_val - 1), na.rm = TRUE) %>% 
  group_by(nombre_entidad) %>% 
  summarise(N_cant = sum(nj_cant),
            N_val = sum(nj_val),
            indba_ID_cant = (sum(sub_cant)/(N_cant*(N_cant - 1))*100),
            indba_ID_val = (sum(sub_val)/(N_val*(N_val - 1)))*100, na.rm = TRUE) %>% 
  select(nombre_entidad, indba_ID_cant, indba_ID_val)

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")

rm(temp)

# 4.9 Número de empresas ganadoras diferentes por cada 100 contratos

# Calcular el indicador de diversidad
temp <- SECOP_I %>% 
  group_by(nombre_entidad) %>% 
  summarise(n_contratistas_dif = n_distinct(id_contratista),
            n_contratos = n(),
            ganadoras = (n_contratistas_dif/n_contratos)*100) %>% 
  mutate(indba_ganadoras = 100-ganadoras) %>% 
  select(nombre_entidad, indba_ganadoras)  

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")
rm(temp)

#4.10y 4.11 Índice de concentración de las cuatro empresas con mayor numero y valor de contratos - IC4k

# Indice para el numero de contratos
temp <- SECOP_I %>% 
  group_by(nombre_entidad, id_contratista) %>% 
  summarise(nj = n()) %>% 
  group_by(nombre_entidad) %>% 
  mutate(Ni = sum(nj),
         Pi = nj/Ni) %>% 
  top_n(n = 4, wt = nj) %>% 
  summarise(indba_IC4K_cant = sum(Pi)*100)

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")
rm(temp)

# Valor 

# Indice para el valor de los contratos
temp <- SECOP_I %>% 
  group_by(nombre_entidad, id_contratista) %>% 
  summarise(nj = sum(valor_total_con_adiciones)) %>% 
  group_by(nombre_entidad) %>% 
  mutate(Ni = sum(nj),
         Pi = nj/Ni) %>% 
  top_n(n = 4, wt = nj) %>% 
  summarise(indba_IC4K_val = sum(Pi)*100)

# Agrupar los indicadores en una sola tabla
Indicadores <- Indicadores %>% 
  left_join(y = temp, by = "nombre_entidad")
rm(temp)


##Indicadores de Violaciones o anomalías en los procesos de compra ----



#5. Indice de corrupción ----

#El indice de corrupción para cada entidad se calcula como la media aritmética de los indices de corrupción estimados anteriormente. Esto se hace basado en el estudio de Zuleta & Caro (2019).

#Indicadores <- Indicadores %>% mutate(ind_riesgo_corrupcion = apply(Indicadores[,-c(1,2,3,4,5,6,7,8,9,10,11,14,15,16,17)],1, mean, na.rm = TRUE))

##################REVISAR EN CADA CORRIDA
Indicadores <- Indicadores %>%
  mutate(ind_riesgo_corrupcion = apply(Indicadores[, c(36,43,48,49,50,51,52,53,54)],1, mean, na.rm = TRUE))


### Guardar en Archivo csv 
#write.csv(Indicadores, file = "Indicadores.csv", row.names = FALSE)
vroom_write(Indicadores, path = "Indicadores_Final_Anno.csv", delim = ";")

SECOP_Prueba <- read_delim("Indicadores_Final.csv", ";", escape_double = FALSE, trim_ws = TRUE)

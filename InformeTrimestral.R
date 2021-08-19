# El objetivo de este script es crear un dashboar de consumos trimestrales de la empresa Multilimpiezas S.L.
# Objetivos específicos.
# 1. Conocer el consumo en el trimestre presente.
# 2. Compararlo con el consumo del trimestre equivalente del año anterior. Teniendo en cuenta las horas trabajadas.
# 3. Hacer un dashboard donde se comparen ambos valores trimestrales por productos escogidos.
# 4. Mostrar si se han cumplido o no los objetivos.
# 5. Incorporar elementos de automatización al script para solamente ingresar el trimestre y dejar el programa haga el resto. 

# Preliminares: Cargar los datos faltantes:
## Como todavía no se editan los datos directamente en la base correspondiente, ellos deben traerse del archivo cvs
## correspondiente. 

# Objetivo 1: Conocer el consumo del trimestre presente. Producto: tabla con los consumos de este trimestre y el 
# correspondiente del año pasado.

## Verificar que codigoDesechos este actualizado. Si no, actualizar. Se sabe si está actualizado cuando el número de entradas
## en el google sheets coincide con la base de datos MongoDB.
# 0. Funciones y generales

######  VARIABLES INICIALES ###############
anual <- 2021
trimestre <- 2

#### VARIABLES DE ESTE AÑO ##################################################
diaInicioTrimestre <- NULL
diaFinTrimestre <- NULL

if(trimestre==1){
    diaInicioTrimestre <- as.Date(paste0(anual,"-01-01"))
    diaFinTrimestre <- as.Date(paste0(anual,"-03-31"))
  } else if(trimestre==2){
    diaInicioTrimestre <- as.Date(paste0(anual,"-04-01"))
    diaFinTrimestre <- as.Date(paste0(anual,"-06-30"))
  } else if(trimestre==3){
    diaInicioTrimestre <- as.Date(paste0(anual,"-07-01"))
    diaFinTrimestre <- as.Date(paste0(anual,"-09-30"))
  } else {
    diaInicioTrimestre <- as.Date(paste0(anual,"-10-01"))
    diaFinTrimestre <- as.Date(paste0(anual,"-12-31"))
  }
  
diaAnteriorTrimestre <- as.Date(diaInicioTrimestre)-1  
diaSiguienteTrimestre <- as.Date(diaFinTrimestre)+1



#### VARIABLES DEL AÑO PASADO ##################################################
AAdiaInicioTrimestre <- NULL
AAdiaFinTrimestre <- NULL

if(trimestre==1){
  AAdiaInicioTrimestre <- as.Date(paste0(anual-1,"-01-01"))
  AAdiaFinTrimestre <- as.Date(paste0(anual-1,"-03-31"))
} else if(trimestre==2){
  AAdiaInicioTrimestre <- as.Date(paste0(anual-1,"-04-01"))
  AAdiaFinTrimestre <- as.Date(paste0(anual-1,"-06-30"))
} else if(trimestre==3){
  AAdiaInicioTrimestre <- as.Date(paste0(anual-1,"-07-01"))
  AAdiaFinTrimestre <- as.Date(paste0(anual-1,"-09-30"))
} else {
  AAdiaInicioTrimestre <- as.Date(paste0(anual-1,"-10-01"))
  AAdiaFinTrimestre <- as.Date(paste0(anual-1,"-12-31"))
}

AAdiaAnteriorTrimestre <- as.Date(AAdiaInicioTrimestre)-1  
AAdiaSiguienteTrimestre <- as.Date(AAdiaFinTrimestre)+1

######  FUNCIONES ######################

swr = function(string, nwrap=12) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

elDirectorio <- "informeTrimestral"

## Descargar codigoDesechos
{ 
  library(mongolite)
  nombre <- "codigoDesechos"
  options(mongodb = list(
    "host" = "qem.rodsc.mongodb.net",
    "username" = "DBQEM",
    "password" = "dbqem"
  ))
  databaseName="ISO"
  collectionName <- nombre
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  db$find()
  assign(nombre,db$find())
}

## Luego de hecha esta verificación, procedemos a cargar los datos necesarios para este objetivo 1 y para el objetivo 2.
## 1. Traemos los datos de la base de Datos MongoDB

{ 
  library(mongolite)
  nombre <- "pedidosConCodigo"
  options(mongodb = list(
    "host" = "qem.rodsc.mongodb.net",
    "username" = "DBQEM",
    "password" = "dbqem"
  ))
  databaseName="ISO"
  collectionName <- nombre
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  db$find()
  assign(nombre,db$find())
}

## 1.1. Joint de datos de consumo con sus descripciones
library(tidyverse)
pedidosConCodigo$FacturaFecha <- as.Date(pedidosConCodigo$FacturaFecha, format="%d-%m-%Y")
class(pedidosConCodigo$FacturaFecha)
informeTrimestral <- pedidosConCodigo %>% 
  dplyr::filter((FacturaFecha > AAdiaAnteriorTrimestre & FacturaFecha < AAdiaSiguienteTrimestre)|(FacturaFecha > diaAnteriorTrimestre & FacturaFecha < diaSiguienteTrimestre)) %>% 
  select(Trazabilidad,FacturaFecha, starts_with("S"),starts_with("O")) %>% 
  pivot_longer(cols = starts_with("S")| starts_with("O"), names_to="Producto", values_to="Cantidad") %>% 
  mutate(FacturaMes = as.numeric(format(FacturaFecha,"%m"))) %>% 
  mutate(Trimestre = ifelse(FacturaMes < 4, 1,
                            ifelse(FacturaMes < 7, 2,
                                   ifelse(FacturaMes < 10, 3,4)))) %>%
  mutate(Anual = as.numeric(format(FacturaFecha, "%Y"))) %>% 
  group_by(Producto,Trimestre,Anual) %>% 
  summarise(Total=sum(Cantidad, na.rm = TRUE)) %>% 
  #pivot_wider(names_from = Anual, values_from=Total) %>% 
  left_join(.,codigoDesechos[c("Codex","DescriptioSpeciei","MensuraePonderum","PondusKg","NomenGeneris")], by=c("Producto"="Codex")) %>% 
  mutate(Total = ifelse(is.na(PondusKg),Total,Total*PondusKg)) %>% 
  ungroup() %>% 
  select(c(1,3,5,4,6,8))
colnames(informeTrimestral) <- c("Codex","Anualidad","Descripcion","Consumo","Unidad","NomenGeneris")



rm(pedidosConCodigo)
rm(codigoDesechos)

# 2. Compararlo con el consumo del trimestre equivalente del año anterior. Teniendo en cuenta las horas trabajadas.
# Producto Tabla con las horas trabajadas en los trimestres a comparar.

## 2.1. Descargar los datos de tiempoTrabajadores
{ 
  library(mongolite)
  nombre <- "tiempoTrabajadores"
  options(mongodb = list(
    "host" = "qem.rodsc.mongodb.net",
    "username" = "DBQEM",
    "password" = "dbqem"
  ))
  databaseName="ISO"
  collectionName <- nombre
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  db$find()
  assign(nombre,db$find())
}

## 2.2. Seleccionar los trimestres y calcular las horas trabajadas
## 2.2.1. Seleccionar las columnas Entra y Sale
pruebaTiempo <- tiempoTrabajadores[c("Entra","Sale","HorasSemana")]
#rm(tiempoTrabajadores)
pruebaTiempo$Entra <- as.Date(pruebaTiempo$Entra, format = "%d-%m-%Y",origin = "1970-01-01")
pruebaTiempo$Sale <- as.Date(pruebaTiempo$Sale, format = "%d-%m-%Y",origin = "1970-01-01")
pruebaTiempo$Sale <- ifelse(is.na(pruebaTiempo$Sale),Sys.Date(),pruebaTiempo$Sale)
pruebaTiempo$Sale <- as.Date(pruebaTiempo$Sale, origin = "1970-01-01", format = "%Y-%m-%d")
pruebaTiempo <- pruebaTiempo[!is.na(pruebaTiempo$Entra),]

## 2.2.2. Determinar las fechas 
pruebaTiempo$InicioTrimestre0 <- as.Date(ifelse(pruebaTiempo$Entra<=AAdiaInicioTrimestre,AAdiaInicioTrimestre,NA),origin = "1970-01-01")
pruebaTiempo$FinTrimestre0 <- as.Date(ifelse(pruebaTiempo$Sale< AAdiaSiguienteTrimestre, pruebaTiempo$Sale,AAdiaFinTrimestre),origin = "1970-01-01")
pruebaTiempo$date_diff_T0 <- difftime(pruebaTiempo$FinTrimestre0,pruebaTiempo$InicioTrimestre0, units = "weeks" )
pruebaTiempo$InicioTrimestre1 <- as.Date(ifelse(pruebaTiempo$Entra<=diaInicioTrimestre , diaInicioTrimestre,NA),origin = "1970-01-01")
pruebaTiempo$date_diff_T0 <- ifelse(pruebaTiempo$date_diff_T0<=0,NA,pruebaTiempo$date_diff_T0)
pruebaTiempo$FinTrimestre1 <- as.Date(ifelse(pruebaTiempo$Sale<as.Date(diaSiguienteTrimestre, origin = "1970-01-01"),as.Date(pruebaTiempo$Sale, origin = "1970-01-01"),as.Date(diaFinTrimestre, origin = "1970-01-01")), origin = "1970-01-01")
pruebaTiempo$date_diff_T1 <- difftime(pruebaTiempo$FinTrimestre1,pruebaTiempo$InicioTrimestre1, units = "weeks" )
pruebaTiempo$date_diff_T1 <- ifelse(pruebaTiempo$date_diff_T1<=0,NA,pruebaTiempo$date_diff_T1)

## 2.2.3. Determinar las horas trabajadas
pruebaTiempo$Horas_T0 <- pruebaTiempo$date_diff_T0*pruebaTiempo$HorasSemana
pruebaTiempo$Horas_T1 <- pruebaTiempo$date_diff_T1*pruebaTiempo$HorasSemana
horasTrabajadasT0 <- sum(pruebaTiempo$Horas_T0, na.rm = TRUE)
horasTrabajadasT1 <- sum(pruebaTiempo$Horas_T1, na.rm = TRUE)

## 2.2.4. Asignar las horas trabajadas a cada año
informeTrimestral$Horas <- ifelse(informeTrimestral$Anualidad==min(informeTrimestral$Anualidad),horasTrabajadasT0,horasTrabajadasT1)

## 2.2.5. Consumo por cada 10 horas de trabajo
informeTrimestral$ConsumoJornada <- round((informeTrimestral$Consumo/informeTrimestral$Horas)*8,3)

## Objetivo 3. Generar un dashboard.
## Producto: un gráfico en ggplot2

# ### Grafico Bolsas
# ## 3.1. Crear tabla de códigos únicos
tablaCodigos <- informeTrimestral %>%
  mutate(CodexPrimus=substring(Codex,1,3)) %>%
  select(CodexPrimus,NomenGeneris,Unidad) %>%
  distinct()
#codificador <- levels(factor(substr(informeTrimestral$Codex,1,3)))
annus <- levels(as.factor(informeTrimestral$Anualidad))
informeTrimestral$Descripcion2 <- swr(informeTrimestral$Descripcion)
# 
# for (i in 1:nrow(tablaCodigos)){   
#   ElTrimestre <- informeTrimestral %>% 
#     dplyr::filter(substr(Codex,1,3) == tablaCodigos$CodexPrimus[i]) 
# 
#   graficoTrimestre <- ggplot(ElTrimestre, aes(x=factor(Anualidad) , y=ConsumoJornada, group = Descripcion, fill=factor(Anualidad), label=ConsumoJornada))+
#     geom_col()+
#     geom_text(color="black", fontface = "bold", size = 4, hjust = 0.6)+
#     labs(title=paste0("Consumo de: ",tablaCodigos$NomenGeneris[i]," (",tablaCodigos$Unidad[i],")" ),
#          subtitle =  paste0("Segundo Trimestre ", min(annus), " y ", max(annus)),
#          y = "Consumo por jornada (8 horas)")+
#     scale_fill_manual("legend", values = c("2020" = "#f6d099", "2021" = "#c76d39"))+
#     facet_grid(rows = vars(Descripcion), scales = "free")+
#     coord_flip()+
#     theme(legend.position = "none",
#           axis.title.y = element_blank(),
#           axis.text = element_text(color="black", face="bold"),
#           axis.title.x = element_text(color="black", face="bold"),
#           plot.background = element_rect(fill = "#c8d2a3"),
#           panel.background = element_rect(fill = "#91a368"),
#           panel.grid.major.y = element_line(color = "#91a368"),
#           panel.grid.major.x = element_line(color = "#c8d2a3"),
#           strip.background =element_rect(fill= "#c8d2a3"),
#           strip.text = element_text(colour = 'black', face = "bold"),
#           strip.text.y = element_text(angle = 0, size=8))
#   ggsave(paste0(elDirectorio,"/",tablaCodigos$CodexPrimus[i],".png"),graficoTrimestre,width = 12, height = (nrow(ElTrimestre)*0.375)+5, units = "cm", device = "png")
# }
  
productoConsumido <- "Bolsa 115 * 150"

ElTrimestre <- informeTrimestral %>% 
  dplyr::filter(Descripcion == productoConsumido) 

unidad <- ElTrimestre$Unidad[1]

graficoTrimestre <- ggplot(ElTrimestre, aes(x=factor(Anualidad) , y=ConsumoJornada, group = Descripcion, fill=factor(Anualidad), label=ConsumoJornada))+
  geom_col()+
  geom_text(color="black", fontface = "bold", size = 4, hjust = 0.6)+
  labs(title=paste0("Consumo de: ", productoConsumido," (",unidad,")" ),
       subtitle =  paste0("Segundo Trimestre ", min(annus), " y ", max(annus)),
       y = "Consumo por jornada (8 horas)")+
  scale_fill_manual("legend", values = c("2020" = "#f6d099", "2021" = "#c76d39"))+
  #facet_grid(rows = vars(Descripcion), scales = "free")+
  coord_flip()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(color="black", face="bold"),
        axis.title.x = element_text(color="black", face="bold"),
        plot.background = element_rect(fill = "#c8d2a3"),
        panel.background = element_rect(fill = "#91a368"),
        panel.grid.major.y = element_line(color = "#91a368"),
        panel.grid.major.x = element_line(color = "#c8d2a3"),
        strip.background =element_rect(fill= "#c8d2a3"),
        strip.text = element_text(colour = 'black', face = "bold"),
        strip.text.y = element_text(angle = 0, size=8))

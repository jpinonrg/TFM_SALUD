# time serie by groups
library(ggalt)

ts_change <- function(data,indicador,ylabtitle,ymin,ymax,unidades,sexo){
data <- data[indic_he==indicador]
if (unidades =='YR'){
  data <- data[!grep("PC",indic_he)]
  unidades <- "Años"
}else{
  data <- data[grep("PC",indic_he)]
  unidades <- "%"
}
# Quitamos EU28
data <- data[geo !='EU28']
data[is.na(pais),pais:='UE27_2020']
# Desde 2004 a 2019
data <- data[as.character(variable) >= '2004']
if (sexo =='F'){sexo2 <- 'Mujeres'}else{sexo2 <- 'Hombres'}
data <- data[sex==sexo]
ggplot(data[variable==ymin], 
       aes(y = reorder(data[variable==ymin]$pais, data[variable==ymin]$value),#ymin o solo geo
           x = data[variable==ymin]$value,
           xend = data[variable==ymax]$value)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = ifelse(data[variable==ymin]$pais != 'UE27_2020', "grey", "black"), 
                colour_x = "blue", 
                colour_xend = "red") +
  #theme_minimal() + 
#  labs(x = (paste0("Cambio ","(",unidades,")")),
  labs(x = unidades,
       y = "")+ggtitle(label=paste0(ylabtitle," en ",tolower(sexo2)),subtitle=paste(ymin,ymax,sep="-"))+theme(plot.title = element_text(hjust = 0.5),plot.subtitle=element_text(hjust = 0.5))
}

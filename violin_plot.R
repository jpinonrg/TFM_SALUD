################
# violin_plot.R
# Función para realizar gráficos de violín
###############

library(ggplot2)
library(dplyr)
library(forcats)
library(viridis)

violin_plot <- function(data,indicador,ylabtitle,ymin,ymax,unidades){
  data <- data[grep(indicador,indic_he)]
  if (unidades =='YR'){
    data <- data[!grep("PC",indic_he)]
    unidades <- "años"
  }else{
    data <- data[grep("PC",indic_he)]
    unidades <- "%"
  }
  data$indic_he <- as.factor(data$indic_he)
  # Nombres indicadores
  if (indicador =='LE'){
    data$indic_he2 <- gsub("LE_","Esperanza de vida:",data$indic_he)
    w <- 1.4
  }else{
    data$indic_he2 <- gsub("HLY_","Años de vida saludables:",data$indic_he)
    w <- 0.9
  }
  #Seleccionamos solo sexo masculino y femenino
  data <- data[sex!='T'] 
  # Consideramos los datos por país (No para el conjunto de la UE)
  data <- data[!is.na(pais)]
  #data <- data[!grep('EU',geo)]
  #data <- data[!grep('EA',geo)]
  # Solo consideramos 2004 a 2019
  data <- data[as.character(variable) >= '2004']
  # Agrupamos y ploteamos
  data %>%
    mutate(ind = fct_reorder(indic_he, value)) %>%
    mutate(ind = factor(indic_he, levels=c(levels(data$indic_he)))) %>%
    ggplot(aes(fill=sex, y=value, x=ind)) + 
    geom_violin(position = position_dodge(0.9), alpha=0.5, outlier.colour="transparent",width=w)+
    geom_boxplot(aes(color = sex), width = 0.15,position = position_dodge(0.9),outlier.colour=NA,color="grey", alpha=0.2)+
    #scale_fill_viridis(discrete=T, name="") + theme_bw()+
    xlab("") + scale_x_discrete(labels=unique(data$indic_he2))+
    ylab(paste0(ylabtitle, " (",unidades,")")) +
    ylim(ymin,ymax)+scale_fill_manual(name = "Sexo",values = c("#FFA500", "#008000"))+theme(axis.text.x = element_text(angle = 60, hjust=1))
 }
library(treemapify)
treemap_plot <- function(data,indicador,grupo,cat,sexo){
  # indicator
  data <- data[Indicator==indicador]
  if (grupo=='TOTAL'  & sexo=='all'){
    print('Análisis por sexo')
    temp <- data[age=="TOTAL" & isced11=='TOTAL' & variable=='EU27_2020' & sex !="T"]
  }
  if (grupo=='isced11' & sexo=='all'){
    print('Análisis por nivel educativo y sexo')
    temp <- data[age=="TOTAL" & isced11!='TOTAL' & variable=='EU27_2020' & sex!="T"]
  }
  if (grupo=='isced11' & sexo!='all'){
    print('Análisis por nivel educativo')
    temp <- data[age=="TOTAL" & isced11!='TOTAL' & variable=='EU27_2020' & sex==sexo]
  }
  if (grupo=='deg_urb' & sexo=='all'){
    print('Análisis por grado urbanismo y sexo')
    temp <- data[age=="TOTAL" & deg_urb!='TOTAL' & variable=='EU27_2020' & sex!="T"]
  }
  if (grupo=='deg_urb' & sexo!='all'){
    print('Análisis por grado urbanismo')
    temp <- data[age=="TOTAL" & deg_urb!='TOTAL' & variable=='EU27_2020' & sex==sexo]
  }
  if (cat=='accident'){cat2 <- 'Accidente'}
  if (cat=='hlthcare'){cat2 <- 'Gravedad del accidente'}
  if (cat=='isced11'){cat2 <- 'Nivel educativo'}
  if (cat=='deg_urb'){cat2 <- 'Grado de urbanismo'}
  if (sexo=='all'){
  ggplot(temp, aes(area = value,
                       fill = get(cat),
                       label = paste0(sex,"-",value," %"),
                       subgroup = get(cat),subgroup2=sex)) + geom_treemap()+
    geom_treemap_subgroup_border(colour = "white") +
    geom_treemap_subgroup2_border(colour = "white",size=2) +
    geom_treemap_text(fontface = "italic",
                      colour = ifelse(temp$sex != 'F', "white", "black"),
                      place = "centre",
                      grow = F,
                      reflow = T) +labs(fill=cat2)
  }else{
    ggplot(temp, aes(area = value,
                     fill = get(cat),
                     label = paste0(get(grupo),"-",value," %"),
                     subgroup = get(cat),subgroup2=get(grupo))) + geom_treemap()+
      geom_treemap_subgroup_border(colour = "white") +
      geom_treemap_subgroup2_border(colour = "white",size=2) +
      geom_treemap_text(fontface = "italic",
                        colour = "white",
                        place = "centre",
                        grow = F,
                        reflow = T) +labs(fill=cat2)
  }
}
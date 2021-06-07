################
# 01
# Script para leer y limpiar los datos
###############


#--------------------
# Cargamos librerías
#--------------------
library(data.table)
library(splitstackshape) #cSplit
library(dplyr)
library(DataExplorer)

#--------------------
# Directorios
#--------------------
bindir <- 'D:/UOC_MASTER/TFM/Salud/PAC4/Raw/'
outdir <- 'D:/UOC_MASTER/TFM/Salud/PAC4/src/HealthStatus/data/'

#--------------------
# Programa principal
#--------------------
setwd(bindir)
list_categories <- list()
folders <- list.files()
for (folder in folders){
  setwd(bindir)
  print(paste0('Leyendo indicador: ',folder))
  setwd(paste0(bindir,folder))
  files <- list.files(pattern='.tsv$')
  list_files <- list()
  for (file in files){
    print(file)
    file.dt <- fread(file,header=T)
    #Separamos las variables de la primera columna
    colsind <- unlist(tstrsplit(names(file.dt)[1],","))
    file.dt <- cSplit(file.dt, 1, ",")
    cols <- names(file.dt[,!grepl("time",names(file.dt)),with=F])
    # FLAGS
    #Ponemos NA donde haya 'u' o 'c'
    for (col in cols){
      file.dt[grep('u',get(col)),(col):=NA,]
      file.dt[grep('c',get(col)),(col):=NA,]
    }
    file.dt[, (cols):= lapply(.SD, function(x) (tstrsplit(x," "))[[1]]), .SDcols=cols]
    file.dt[, (cols):= lapply(.SD, as.numeric), .SDcols=cols]
    setnames(file.dt,names(file.dt)[grepl("time",names(file.dt))],colsind)
    file.dt <- melt(file.dt,id=colsind)
    # Escogemos geo o time según proceda
    if (colsind[grep("geo",colsind)]=='time\\geo'){
      setnames(file.dt,"time\\geo","time")
    }else if(colsind[grep("geo",colsind)]=='geo\\time'){
      setnames(file.dt,"geo\\time","geo")
    }
    ## Porcentaje de valores perdidos
    sapply(file.dt, function(x) (sum(is.na(x))/length(x))*100)
    #Añadimos categoría
    file.dt[,Category:=as.factor(folder)]
    #Añadimos tipo de indicador
    file.dt[,Indicator:=as.factor(strsplit(file,".tsv")[[1]][1])]
    list_files[[file]] <- file.dt
  }
  list_categories[[folder]] <- rbindlist(list_files,use.names=T,fill=T)
}


#Guardamos por categoría general
hly <- list_categories$Healthy_life_years
absw <- list_categories$Absence_from_work_due_to_health_problems
ifa <- list_categories$Injuries_from_accidents
ifa <- ifa[unit=='PC']
fal <- list_categories$Functional_and_activity_limitations
sph <- list_categories$`Self-perceived_healt_and_well_being`
srcm <- list_categories$`Self-reported_chronic_morbidity`
saveRDS(hly,paste0(outdir,"hly.rds"))
saveRDS(absw,paste0(outdir,"absw.rds"))
saveRDS(ifa,paste0(outdir,"ifa.rds"))
saveRDS(fal,paste0(outdir,"fal.rds"))
saveRDS(sph,paste0(outdir,"sph.rds"))
saveRDS(srcm,paste0(outdir,"srcm.rds"))

####
# Formateamos fichero serie temporal de años de vida saludables
# Años 1996-2003
unique(hly[Indicator=="hlth_hlye_h"]$indic_he)
hly[grep('F_',indic_he),sex:='F']
hly[grep('M_',indic_he),sex:='M']
hly[grep('PC',indic_he),unit:='PC']
hly[!grep('PC',indic_he),unit:='YR']
hly$indic_he <- gsub(".*0_LE","LE_0",hly$indic_he)
hly$indic_he <- gsub(".*65_LE","LE_65",hly$indic_he)
hly$indic_he <- gsub(".*0_DFLEPC","HLY_PC_0",hly$indic_he)
hly$indic_he <- gsub(".*65_DFLEPC","HLY_PC_65",hly$indic_he)
hly$indic_he <- gsub(".*0_DFLE","HLY_0",hly$indic_he)
hly$indic_he <- gsub(".*65_DFLE","HLY_65",hly$indic_he)

#Añado nombre de países
paises <- fread('D:/UOC_MASTER/TFM/Salud/PAC4/src/pib_capita.txt',header=F,encoding = 'UTF-8')
paises$V3 <- NULL
colnames(paises) <-c('geo','pais')
saveRDS(paises,paste0(outdir,"paises.rds"))
#Paises
hly <- merge(hly,paises,by='geo',all.x=T)
saveRDS(hly,paste0(outdir,"hly_ts.rds"))
colnames(paises) <-c('variable','pais')
ifa <- merge(ifa,paises,by='variable',all.x=T)
saveRDS(ifa,paste0(outdir,"ifa.rds"))
absw <- merge(absw,paises,by='variable',all.x=T)
saveRDS(absw,paste0(outdir,"absw.rds"))
sph <- merge(sph,paises,by='variable',all.x=T)
saveRDS(sph,paste0(outdir,"sph.rds"))


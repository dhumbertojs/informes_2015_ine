#librerias y directorios####
library(tabulizer)
library(dplyr)
library(stringr)
library(readxl)
library(beepr)

inp <- "/home/dhjs/Documentos/R_projects/informes_2015_ine/bases"
out <- "/home/dhjs/Documentos/R_projects/informes_2015_ine/"

names <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.OT.EFE", "AP.OT.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.ESP", "AP.SIM.ESP", "AP.MIL.EFE", "AP.SIM.EFE", "REND.FINC", "TRANS.NOFED", "OTROS", "FIN.PUB.INDEP", "TOTAL")

beep(5)

#PAN####
pan <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/01_PAN_Anexo.pdf"

pan.egr <- extract_tables(pan, 1:5)
#View(pan.egr)

x <- 1
ab <- data.frame()

for(x in seq(1:5)) {
  tempo <- as.data.frame(pan.egr[x])
  tempo <- tempo[-(1:4),]
  ab <- bind_rows(ab, tempo)
  rm(tempo)
}

ab <- ab[-nrow(ab), -1]

pan.ing <- ab

colnames(pan.ing) <- names

pan.ing <- pan.ing %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
    PARTY = "PAN"
  )
beep(2)
#PRI####
pri <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/02_PRI_Anexos.pdf"

pri.egr <- extract_tables(pri, 3)
#View(pri.egr)

#x <- 1
#ab <- data.frame()

#for(x in seq(1:2)) {
  tempo <- as.data.frame(pri.egr)
  tempo <- tempo[-(1:4),-(1:2)]
  #ab <- bind_rows(ab, tempo)
  #rm(tempo)
#}

ab <- tempo[-nrow(tempo),]

pri.ing <- ab

pri.ing <- pri.ing[, -4]

pri.ing <- pri.ing %>% 
  mutate_all(as.character)
  
pri.ing <- pri.ing %>% 
  mutate(
    AP.CEN.EFE = 0, 
    AP.CEN.ESP = 0,
    
    AP.OT.EFE = sapply(strsplit(X7, " "), "[", 1), 
    AP.OT.ESP = sapply(strsplit(X7, " "), "[", 2), 
    
    AP.CAN.EFE = sapply(strsplit(X8, " "), "[", 1), 
    AP.CAN.ESP = sapply(strsplit(X8, " "), "[", 2),
    
    AP.MIL.EFE = 0, 
    AP.MIL.ESP = 0, 
    
    AP.SIM.EFE = sapply(strsplit(X10, " "), "[", 1), 
    AP.SIM.ESP = sapply(strsplit(X10, " "), "[", 2),
    
    REND.FINC = 0,
    TRANS.NOFED = 0, 
    OTROS = 0,
    FIN.PUB.INDEP = 0
  )  %>% 
  rename(ENTIDAD = X3, DISTRITO = X4, NOMBRE.CANDIDATO = X5, TOTAL = X13) %>% 
  select(names)

pri.ing <- pri.ing %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
    PARTY = "PRI"
  )
beep(2)
#PRD####
prd <- read_excel(paste(inp, "prd.xlsx", sep = "/"), col_names = F)

colnames(prd) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.OT.EFE", "AP.OT.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.EFE", "AP.MIL.ESP", "AP.SIM.EFE", "AP.SIM.ESP", "REND.FINC", "TRANS.NOFED", "OTROS", "TOTAL")

prd <- prd %>% 
  mutate(
    DISTRITO = sapply(strsplit(DISTRITO, "-"), "[", 1),
  ) %>% 
  filter(AP.CEN.EFE != "COALICIÓN DE IZQUIERDA PROGRESISTA" & !is.na(AP.CEN.EFE)) %>% 
  mutate(AP.CEN.EFE = as.numeric(AP.CEN.EFE))

#prd <- read.csv(paste(inp, "tabula-3_PRD_Anexo.csv", sep = "/"), stringsAsFactors = F)

# prd <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/3_PRD_Anexo.pdf"
# 
# prd.egr <- extract_tables(prd, 4:8)
# #View(prd.egr)
# 
# x <- 1
# ab <- data.frame()
# 
# for(x in seq(4:8)) {
#   tempo <- as.data.frame(prd.egr[x])
#   tempo <- tempo[-(1:6),]
#   ab <- bind_rows(ab, tempo)
#   rm(tempo)
# }
# 
# ab <- ab %>% 
#   mutate_all(as.character) %>% 
#   filter(X1 != "") %>% 
#   select(-c(1,4))
# 
# prd.ing <- ab
# 
# prd.ing <- prd.ing %>% 
#   mutate(
#     DISTRITO = sapply(strsplit(X3, "-"), "[", 1),
#     NOMBRE.CANDIDATO = sapply(strsplit(X3, " "), "[", 2),
#     
#     AP.CEN.EFE = sapply(strsplit(X5, " "), "[", 1),  
#     AP.CEN.ESP = sapply(strsplit(X5, " "), "[", 2),  
#     
#     AP.OT.EFE = sapply(strsplit(X6, " "), "[", 1),
#     AP.OT.ESP = sapply(strsplit(X6, " "), "[", 1),
#     
#     AP.CAN.EFE = sapply(strsplit(X6, " "), "[", 1),
#     
#     AP.MIL.EFE = sapply(strsplit(X8, " "), "[", 1),
#     AP.MIL.ESP = sapply(strsplit(X8, " "), "[", 2),
#     
#     AP.SIM.EFE = sapply(strsplit(X9, " "), "[", 1),
#     AP.SIM.ESP = sapply(strsplit(X9, " "), "[", 2),
#     
#     FIN.PUB.INDEP = 0
#   ) %>% 
#   rename(ENTIDAD = X2, AP.CAN.ESP = X7, REND.FINC = X10, TRANS.NOFED = X11, OTROS = X12, TOTAL = X13) %>% 
#   select(names)
# 
# prd.ing <- prd.ing %>% 
#   mutate(
#     AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
#     AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
#     AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
#     AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
#     AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
#     AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
#     AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
#     AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
#     AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
#     AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
#     REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
#     TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
#     OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
#     FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
#     TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
#     PARTY = "PRD"
#   )
beep(2)
#PVEM####
pvem <- read.csv(paste(inp, "tabula-05_PVEM_Anexos.csv", sep = "/"), stringsAsFactors = F)
pvem <- pvem[-c(1:2, nrow(pvem)), -c(1,4)]
pvem <- pvem %>% select(c(2:6, 8:16, 21:24))
colnames(pvem) <- names

pvem <- pvem %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$-]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$-]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$-]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$-]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$-]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$-]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$-]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$-]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$-]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$-]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$-]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$-]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$-]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$-]", "", FIN.PUB.INDEP)), 
    TOTAL = as.numeric(gsub("[,$-]", "", TOTAL)),
    PARTY = "PVEM"
    )

#pvem <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/05_PVEM_Anexos.pdf"

#pvem.egr <- extract_tables(pvem, 1)
#View(pvem.egr)

#x <- 1
#ab <- data.frame()

#for(x in seq(1:5)) {
  #tempo <- as.data.frame(pvem.egr)
  #tempo <- tempo[-(1:4),]
  #ab <- bind_rows(ab, tempo)
  #rm(tempo)
#}

#ab <- tempo[-nrow(tempo), -1]

#pvem.ing <- ab

#colnames(pvem.ing) <- names

#pvem.ing <- pvem.ing %>% 
  #mutate(
    #AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    #AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    #AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    #AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    #AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    #AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    #AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    #AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    #AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    #AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    #REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    #TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    #OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    #FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    #TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
    #PARTY = "PVEM"
  #)


beep(2)
#MC####
mc <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/06_MC_Anexos.pdf"

mc.egr <- extract_tables(mc, 1:6)
#View(mc.egr)

x <- 1
ab <- data.frame()

for(x in seq(1:14)) {
  tempo <- as.data.frame(mc.egr[x])
  tempo <- tempo[-(1:4),]
  ab <- bind_rows(ab, tempo)
  rm(tempo)
}

ab <- ab %>% 
  mutate_all(as.character) %>% 
  filter(X1 != "")

ab <- ab %>% 
  rename(ENTIDAD = X1, DISTRITO = X2, NOMBRE.CANDIDATO = X3, AP.CEN.EFE = X5, AP.CEN.ESP = X6, AP.OT.EFE = X8, AP.OT.ESP = X9, AP.CAN.EFE = X11, AP.CAN.ESP = X12, AP.MIL.ESP = X14, AP.SIM.ESP = X15, AP.MIL.EFE = X16, AP.SIM.EFE = X17, REND.FINC = X19, TRANS.NOFED = X20, OTROS = X21, FIN.PUB.INDEP = X22, TOTAL = X23) %>% 
  select(names)

mc.ing <- ab

mc.ing <- mc.ing %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL))
  )

mc.ing <- mc.ing %>% 
  group_by(ENTIDAD, DISTRITO, NOMBRE.CANDIDATO) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(PARTY = "MC")

#da <- bind_rows(pan.ing, pri.ing, prd.ing, mc.ing)

#write.csv(da, paste(out, "tablas_2015.csv", sep = "/"), row.names = F)

beep(2)
#Morena####
morena <- read.csv(paste(inp, "tabula-08_MORENA_Anexos.csv", sep = "/"), stringsAsFactors = F)
colnames(morena) <- names

morena <- morena %>% 
  filter(DISTRITO != "") %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL))
  )
# morena <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/08_MORENA_Anexos.pdf"
# 
# morena.egr <- extract_tables(morena, 1:7)
# #View(morena.egr)
# 
# #x <- 1
# ab <- data.frame()
# 
# #for(x in seq(1:7)) {
#   #tempo <- as.data.frame(morena.egr[x])
#   #tempo <- tempo[-(1:6),]
#   #ab <- bind_rows(ab, tempo)
#   #rm(tempo)
# #}
# 
#   tempo <- as.data.frame(morena.egr[1])
#   tempo <- tempo[-(1:6),]
#   try <- tempo %>% 
#     mutate_all(as.character) %>% 
#     mutate(
#       AP.OT.EFE = sapply(strsplit(X9, " "), "[", 1), 
#       AP.OT.ESP = sapply(strsplit(X9, " "), "[", 2),
#       
#       AP.CAN.EFE = sapply(strsplit(X10, " "), "[", 1), 
#       AP.CAN.ESP1 = sapply(strsplit(X10, " "), "[", 2),
#       AP.CAN.ESP2 = sapply(strsplit(X11, " "), "[", 1),
#       #AP.CAN.ESP = 
#       
#       AP.MIL.ESP = sapply(strsplit(X11, " "), "[", 1),
#       AP.SIM.ESP = sapply(strsplit(X11, " "), "[", 2), 
#       AP.MIL.EFE = sapply(strsplit(X11, " "), "[", 3), 
#       AP.SIM.EFE = sapply(strsplit(X11, " "), "[", 4)
#     ) %>% 
#     rename(ENTIDAD = X2, DISTRITO = X3, NOMBRE.CANDIDATO = X4, AP.CEN.EFE = X5, AP.CEN.ESP = X7, 
#            #AP.OT.EFE, AP.OT.ESP, X9
#            #AP.CAN.EFE, AP.CAN.ESP, X10
#            #AP.MIL.ESP, AP.SIM.ESP, AP.MIL.EFE, AP.SIM.EFE, X11
#            REND.FINC = X12, TRANS.NOFED = X13, OTROS = X14, FIN.PUB.INDEP = X15, TOTAL = X16) #%>% select(names)
#   
# #  ENTIDAD, DISTRITO, NOMBRE.CANDIDATO, AP.CEN.EFE, AP.CEN.ESP, AP.OT.EFE, AP.OT.ESP, AP.CAN.EFE, AP.CAN.ESP, AP.MIL.ESP, AP.SIM.ESP, AP.MIL.EFE, AP.SIM.EFE, REND.FINC, TRANS.NOFED, OTROS, FIN.PUB.INDEP, TOTAL
#   
#   tempo <- as.data.frame(morena.egr[2])
#   tempo <- tempo[-(1:6),]
#   
#   tempo <- as.data.frame(morena.egr[3])
#   tempo <- tempo[-(1:6),]
#   
#   tempo <- as.data.frame(morena.egr[4])
#   tempo <- tempo[-(1:6),]
#   
#   tempo <- as.data.frame(morena.egr[5])
#   tempo <- tempo[-(1:6),]
#   
#   tempo <- as.data.frame(morena.egr[6])
#   tempo <- tempo[-(1:6),]
#   
#   tempo <- as.data.frame(morena.egr[7])
#   tempo <- tempo[-(1:6),]
#   
#   ab <- bind_rows(ab, tempo)  
#   
#   
# try <- ab %>% 
#   filter(X1 != "") %>% 
#   select(-1)
# 
# morena.ing <- ab
# 
# colnames(morena.ing) <- names
# 
# morena.ing <- morena.ing %>% 
#   mutate(
#     AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
#     AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
#     AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
#     AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
#     AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
#     AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
#     AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
#     AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
#     AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
#     AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
#     REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
#     TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
#     OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
#     FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
#     TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
#     PARTY = "Morena"
#   )
beep(2)
#Humanista####
humanista <- read.csv(paste(inp, "tabula-09_PH_Anexos.csv", sep = "/"), header = F, stringsAsFactors = F)

colnames(humanista) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.OT.EFE", "AP.OT.ESP", "AP.CAN.EFE", "AP.CAN.ESP",  "AP.MIL.EFE", "AP.MIL.ESP", "AP.SIM.EFE", "AP.SIM.ESP", "TRANS.NOFED", "TOTAL")

humanista <- humanista %>% 
  mutate(
    DISTRITO = as.character(DISTRITO),
         AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
         AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
         AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
         AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
         AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
         AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
         AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
         AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
         AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
         AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
         TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
         TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
         PARTY = "Humanista"
       )

# humanista <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/09_PH_Anexos.pdf"
# 
# humanista.egr <- extract_tables(humanista, 1:4)
# #View(humanista.egr)
# 
# x <- 1
# ab <- data.frame()
# 
# for(x in seq(1:4)) {
#   tempo <- as.data.frame(humanista.egr[x])
#   tempo <- tempo[-(1:4),]
#   ab <- bind_rows(ab, tempo)
#   rm(tempo)
# }
# ab <- ab[-(1:4),]
# ab <- ab[-nrow(ab), -1]
# 
# humanista.ing <- ab
# 
# colnames(humanista.ing) <- names
# 
# humanista.ing <- humanista.ing %>% 
#   mutate(
#     AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
#     AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
#     AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
#     AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
#     AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
#     AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
#     AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
#     AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
#     AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
#     AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
#     REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
#     TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
#     OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
#     FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
#     TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
#     PARTY = "Humanista"
#   )
beep(2)
#PES####
pes <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/10_ES_Anexos.pdf"

pes.egr <- extract_tables(pes, 1:5)
#View(pes.egr)

x <- 1
ab <- data.frame()

for(x in seq(1:5)) {
  tempo <- as.data.frame(pes.egr[x])
  tempo <- tempo[-(1:3),]
  ab <- bind_rows(ab, tempo)
  rm(tempo)
}

ab <- ab[-nrow(ab), -1]

pes.ing <- ab

pes.ing$DISTRITO <- sapply(str_split(pes.ing$X3, " "), "[", 1)

pes.ing <- pes.ing %>% select(X2, DISTRITO, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18)

colnames(pes.ing) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.OT.EFE", "AP.OT.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.EFE", "AP.MIL.ESP", "AP.SIM.EFE", "AP.SIM.ESP", "REND.FINC", "TRANS.NOFED", "OTROS", "TOTAL")

pes.ing <- pes.ing %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
    PARTY = "PES"
  )
beep(2)
#PRI_PVEM####
pri.pvem <- read.csv(paste(inp, "tabula-11_COA_PRI_PVEM_Anexos.csv", sep = "/"), header = F, stringsAsFactors = F)

pri.pvem <- pri.pvem %>% select(c(3:7, 9:10, 12, 14:15, 17))

colnames(pri.pvem) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.ESP", "AP.SIM.EFE", "AP.SIM.ESP", "TOTAL")

pri.pvem <- pri.pvem %>% 
  mutate(
         AP.CEN.EFE = as.numeric(gsub("[,$LAOI]", "", AP.CEN.EFE)), 
         AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
         AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
         AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
         AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
         AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
         AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
         TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
         PARTY = "PRI.PVEM"
       )

# pri.pvem <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/11_COA_PRI_PVEM_Anexos.pdf"
# 
# pri.pvem.egr <- extract_tables(pri.pvem, 15:20)
# #View(pri.pvem.egr)
# 
# x <- 1
# ab <- data.frame()
# 
# for(x in seq(15:20)) {
#   tempo <- as.data.frame(pri.pvem.egr[x])
#   tempo <- tempo[-(1:4),]
#   ab <- bind_rows(ab, tempo)
#   rm(tempo)
# }
# 
# ab <- ab[-nrow(ab), -1]
# 
# pri.pvem.ing <- ab
# 
# colnames(pri.pvem.ing) <- names
# 
# pri.pvem.ing <- pri.pvem.ing %>% 
#   mutate(
#     AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
#     AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
#     AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
#     AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
#     AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
#     AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
#     AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
#     AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
#     AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
#     AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
#     REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
#     TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
#     OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
#     FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
#     TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
#     PARTY = "PRI.PVEM"
#   )
beep(2)
#PRD_PT####

prd.pt.1 <- read.csv(paste(inp, "tabula-012_COA_PRD_PT_Anexos.csv", sep = "/"), header = F, stringsAsFactors = F)
prd.pt.1 <- prd.pt.1[-1,]
#prd.pt.1 <- prd.pt.1 %>% mutate_all(as.character)
prd.pt.2 <- read.csv(paste(inp, "tabula-012_COA_PRD_PT_Anexos (1).csv", sep = "/"), header = F, stringsAsFactors = F)
#prd.pt.2 <- prd.pt.2 %>% mutate_all(as.character)
prd.pt.3 <- read.csv(paste(inp, "tabula-012_COA_PRD_PT_Anexos (2).csv", sep = "/"), header = F, stringsAsFactors = F)
#prd.pt.3 <- prd.pt.3 %>% mutate_all(as.character)

prd.pt <- bind_rows(prd.pt.1, prd.pt.2, prd.pt.3)

colnames(prd.pt) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.EFE", "AP.MIL.ESP", "AP.SIM.EFE", "AP.SIM.ESP", "REND.FINC", "OTROS", "TOTAL")

prd.pt <- prd.pt %>% 
  mutate(
    DISTRITO = sapply(strsplit(DISTRITO, "-"), "[", 1), 
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
         AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
         AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
         AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
         AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
         AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
         AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
         AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
         REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
         OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
         TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
         PARTY = "PRD.PT"
       )

# prd.pt <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/012_COA_PRD_PT_Anexos.pdf"
# 
# prd.pt.egr <- extract_tables(prd.pt, 1:3)
# #View(prd.pt.egr)
# 
# x <- 1
# ab <- data.frame()
# 
# for(x in seq(1:3)) {
#   tempo <- as.data.frame(prd.pt.egr[x])
#   tempo <- tempo[-(1:4),]
#   ab <- bind_rows(ab, tempo)
#   rm(tempo)
# }
# 
# ab <- ab[-nrow(ab), -1]
# 
# prd.pt.ing <- ab
# 
# colnames(prd.pt.ing) <- names
# 
# prd.pt.ing <- prd.pt.ing %>% 
#   mutate(
#     AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
#     AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
#     AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
#     AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
#     AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
#     AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
#     AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
#     AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
#     AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
#     AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
#     REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
#     TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
#     OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
#     FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
#     TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
#     PARTY = "PRD.PT"
#   )
beep(2)
#Independientes####
indep <- "https://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/UF/UF-PP/IC-Fiscalizacion/IC-2015/Dictamenes-CampanaFederal/13_INDEPENDIENTES_Anexos.pdf"

indep.egr <- extract_tables(indep, 1)
#View(indep.egr)

#x <- 1
#ab <- data.frame()

#for(x in seq(1:5)) {
  tempo <- as.data.frame(indep.egr)
  tempo <- tempo[-(1:4),]
  #ab <- bind_rows(ab, tempo)
  #rm(tempo)
#}

ab <- tempo[-nrow(tempo), -1]

indep.ing <- ab

colnames(indep.ing) <- c("ENTIDAD", "DISTRITO", "NOMBRE.CANDIDATO", "AP.CEN.EFE", "AP.CEN.ESP", "AP.OT.EFE", "AP.OT.ESP", "AP.CAN.EFE", "AP.CAN.ESP", "AP.MIL.ESP", "AP.SIM.ESP", "AP.MIL.EFE", "AP.SIM.EFE", "REND.FINC", "TRANS.NOFED", "OTROS", "FIN.PUB.INDEP", "NO.REPORT", "TOTAL")

indep.ing <- indep.ing %>% 
  mutate(
    AP.CEN.EFE = as.numeric(gsub("[,$]", "", AP.CEN.EFE)), 
    AP.CEN.ESP = as.numeric(gsub("[,$]", "", AP.CEN.ESP)), 
    AP.OT.EFE = as.numeric(gsub("[,$]", "", AP.OT.EFE)), 
    AP.OT.ESP = as.numeric(gsub("[,$]", "", AP.OT.ESP)), 
    AP.CAN.EFE = as.numeric(gsub("[,$]", "", AP.CAN.EFE)), 
    AP.CAN.ESP = as.numeric(gsub("[,$]", "", AP.CAN.ESP)), 
    AP.MIL.ESP = as.numeric(gsub("[,$]", "", AP.MIL.ESP)), 
    AP.SIM.ESP = as.numeric(gsub("[,$]", "", AP.SIM.ESP)), 
    AP.MIL.EFE = as.numeric(gsub("[,$]", "", AP.MIL.EFE)), 
    AP.SIM.EFE = as.numeric(gsub("[,$]", "", AP.SIM.EFE)), 
    REND.FINC = as.numeric(gsub("[,$]", "", REND.FINC)), 
    TRANS.NOFED = as.numeric(gsub("[,$]", "", TRANS.NOFED)), 
    OTROS = as.numeric(gsub("[,$]", "", OTROS)), 
    FIN.PUB.INDEP = as.numeric(gsub("[,$]", "", FIN.PUB.INDEP)), 
    NO.REPORT = as.numeric(gsub("[,$]", "", NO.REPORT)), 
    TOTAL = as.numeric(gsub("[,$]", "", TOTAL)),
    PARTY = "indep"
  )

# Exportando --------------------------------------------------------------

fin <- bind_rows(pan.ing, pri.ing, prd, pvem, mc.ing, morena, humanista, pes.ing, pri.pvem, prd.pt, indep.ing)
write.csv(fin, paste(out, "tablas_2015.csv", sep = "/"), row.names = F)
beep(8)
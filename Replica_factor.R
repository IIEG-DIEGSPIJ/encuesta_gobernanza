library(dplyr)
library(tidyr)
library(readr)
library(stringr)

##Empiezo
##Seleccione el directorio sobre el que trabajará:
setwd("C:/Encuesta Estatal de Gobernanza/Replicación del factor de expansión")
EEG_2020 <- read.csv("EEG_2020.csv") 
EEG_2020 <- EEG_2020 %>% select(-wf2) 
# select(-c(ID_gob, Grado_num2))

#Variables de referencia para hacer el factor de expansión
Gobernanz <- EEG_2020 %>% 
  mutate(Trabaja = ifelse(Trabaja!=1,No_trabaja, Trabaja)) %>% 
  select(Sexo, Grupo, Grado, Trabaja, Personas_hogar, 
         Ing_hogar, Municipio, Clave_mun) %>% 
  mutate(Intercensal = 0,
         ID_gob = 1:n())

### Manejo de la encuesta intercensal para que sus variables sean equiparables a las de Gobernanza
Intercensal <- read.csv("TR_PERSONA14.csv")
Viv_int <- read.csv("TR_VIVIENDA14.csv")

Viv_int <- Viv_int %>% 
  select(ID_VIV, UPM, NUMPERS, INGTRHOG)
Intercensal <- Intercensal %>% 
  select(ID_VIV, UPM, SEXO, EDAD, NIVACAD, CONACT, 
         NOM_MUN, MUN)

Union_inter <- inner_join(x = Intercensal, y = Viv_int, by = c("ID_VIV", "UPM"))
Union_inter <- Union_inter %>% 
  mutate(NIVACAD1 = ifelse(NIVACAD==0, 1, ifelse(NIVACAD==1, 1,
                                                 ifelse(NIVACAD==2, 2, ifelse(NIVACAD==3, 3,
                                                                              ifelse(NIVACAD==4, 4, ifelse(NIVACAD==5, 5,
                                                                                                           ifelse(NIVACAD==6, 2, ifelse(NIVACAD==7, 3,
                                                                                                                                        ifelse(NIVACAD==8, 4, ifelse(NIVACAD==9, 3,
                                                                                                                                                                     ifelse(NIVACAD==10, 5, ifelse(NIVACAD==11, 5,
                                                                                                                                                                                                   ifelse(NIVACAD==12, 6, ifelse(NIVACAD==13, 6,
                                                                                                                                                                                                                                 ifelse(NIVACAD==14, 6,
                                                                                                                                                                                                                                        ifelse(NIVACAD==99, 7, NA))))))))))))))))) %>% 
  mutate(INGTRHOG1 = ifelse(INGTRHOG<3696.60, 1,
                            ifelse((INGTRHOG>=3696.60 & INGTRHOG<7393.20), 2,
                                   ifelse((INGTRHOG>=7393.20 & INGTRHOG<11089.80), 3,
                                          ifelse((INGTRHOG>=11089.80 & INGTRHOG<18483.00), 4,
                                                 ifelse((INGTRHOG>=18483.00 & INGTRHOG<25876.20), 5,
                                                        ifelse((INGTRHOG>=25876.20 & INGTRHOG<36966.00), 6,
                                                               ifelse((INGTRHOG>=36966.00 & INGTRHOG<55449.00), 7,
                                                                      ifelse((INGTRHOG>=55449.00 & INGTRHOG<999997), 8, 
                                                                             ifelse((INGTRHOG==999997), 1,
                                                                                    ifelse((INGTRHOG>=999998), 8,
                                                                                           ifelse((INGTRHOG>=999999), 9,
                                                                                                  ifelse(is.na(INGTRHOG), 9, 
                                                                                                         NA))))))))))))) %>% 
  rename(Sexo = SEXO,
         Edad = EDAD,
         Grado = NIVACAD1,
         Trabaja = CONACT,
         Personas_hogar = NUMPERS,
         Ing_hogar = INGTRHOG1,
         Municipio = NOM_MUN,
         Clave_mun = MUN) %>% 
  mutate(Trabaja = ifelse(Trabaja==10, 1, ifelse(Trabaja==11, 1,
                                                 ifelse(Trabaja==12, 1, ifelse(Trabaja==13, 1,
                                                                               ifelse(Trabaja==14, 1, ifelse(Trabaja==15, 1,
                                                                                                             ifelse(Trabaja==16, 1, ifelse(is.na(Trabaja), NA,
                                                                                                                                           ifelse(Trabaja==20, 3, ifelse(Trabaja==31, 4,
                                                                                                                                                                         ifelse(Trabaja==32, 5, ifelse(Trabaja==33, 6,
                                                                                                                                                                                                       2))))))))))))) %>% 
  mutate(Sexo = ifelse(Sexo==3, 2, 1),
         Edad = ifelse(Edad>110,NA,Edad)) %>% 
  filter(Edad>=18,
         Edad<60) %>% 
  mutate(Grupo = ifelse(Edad < 31, 1, ifelse(Edad < 46, 2, ifelse(
    Edad < 65, 3, ifelse(Edad < 76, 4, 5))))) %>% 
  select(Sexo, Grupo, Grado, Trabaja, Personas_hogar, 
         Ing_hogar, Municipio, Clave_mun) %>% 
  mutate(Intercensal = 1,
         ID_gob = 1001:(1000+n()))

#Unión de la Encuesta de Gobernanza con la Intercensal y añado texto a las variables
Gobernanza_Intercensal <- rbind(Gobernanz, Union_inter)
Gobernanza_Intercensal <- Gobernanza_Intercensal %>% 
  mutate(Ing_hogar = as.character(ifelse(is.na(Ing_hogar), 9, Ing_hogar))) %>% 
  mutate(Sexo = replace(Sexo, str_detect(Sexo, "1"), "Hombre"),
         Sexo = replace(Sexo, str_detect(Sexo, "2"), "Mujer"),
         Grado = replace(Grado, str_detect(Grado, "1"), "Ninguno"),
         Grado = replace(Grado, str_detect(Grado, "2"), "Primaria"),
         Grado = replace(Grado, str_detect(Grado, "3"), "Secundaria"),
         Grado = replace(Grado, str_detect(Grado, "4"), "Preparatoria / Bachillerato"),
         Grado = replace(Grado, str_detect(Grado, "5"), "Licenciatura"),
         Grado = replace(Grado, str_detect(Grado, "6"), "Postgrado"),
         Grado = replace(Grado, str_detect(Grado, "7"), "No sabe / no responde"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "1"), "Sí"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "2"), "No"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "3"), "Busca trabajo"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "4"), "Estudiante"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "5"), "Jubilado / pensionado"),
         Trabaja = replace(Trabaja, str_detect(Trabaja, "6"), "Quehaceres del hogar")) %>% 
  mutate(Ing_hogar = case_when(Ing_hogar=="1" ~ "Menos de 1 SM",
                               Ing_hogar=="2" ~ "De 1 a 2 SM",
                               Ing_hogar=="3" ~ "De 2 a 3 SM",
                               Ing_hogar=="4" ~ "De 3 a 5 SM",
                               Ing_hogar=="5" ~ "De 5 a 7 SM",
                               Ing_hogar=="6" ~ "De 7 a 10 SM",
                               Ing_hogar=="7" ~ "De 10 a 15 SM",
                               Ing_hogar=="8" ~ "Más de 15 SM",
                               Ing_hogar=="9" ~ "No sabe / no responde"))

####Inicio del proceso para obtener el factor de expansión
###Cambio de nombre a la base para un manejo más cómodo
dt_a <- Gobernanza_Intercensal
prop_usada <- mean(dt_a$Grado[dt_a$Intercensal==1]=="Licenciatura", na.rm = TRUE) +
  mean(dt_a$Grado[dt_a$Intercensal==1]=="Postgrado", na.rm = TRUE)

dt_a <- dt_a %>% 
  filter(Grado == "Licenciatura" |  Grado == "Postgrado",
         Municipio %in% unique(dt_a$Municipio[dt_a$Intercensal==0])) %>% 
  drop_na(-c(Clave_mun))

dt1 <- dt_a %>% 
  as_tibble() %>%
  select(-c(Grupo, Grado, Municipio, Clave_mun, ID_gob)) %>%
  drop_na()

# Modelo logit
logit <- glm(Intercensal ~ ., data = dt1, family = "binomial")
probs <- predict(logit, type = "response")
##Para revisar los resultados del modelo logit:
# summary(logit)

# Factor de ajuste:
wf <- 1/(1-probs[dt_a$Intercensal==0])
sum(wf)

# Población municipal:
conapo1 <- read_rds("Poblacion.rds")
conapo <- conapo1 %>% 
  gather(Año, Poblacion, 7:ncol(conapo1)) %>% 
  as_tibble() %>%
  filter(Año == "X2020", 
         Grupos.de.EdAmbosd != "0-14",
         Grupos.de.EdAmbosd != "65+",
         Grupos.de.EdAmbosd != "H",
         Grupos.de.EdAmbosd != "M",
         Grupos.de.EdAmbosd != "Ambos") %>% 
  mutate(Clave.Municipio = Clave.Municipio-14000) %>% 
  filter(Clave.Municipio %in% dt_a$Clave_mun[dt_a$Intercensal==0])
pob <- round(sum(conapo$Poblacion)*prop_usada)

# Factores de ajuste:
wf2 <- round(wf*pob/sum(wf))

##Para que la base original tenga el factor de expansión
Factor_ex <- cbind(EEG_2020,wf2)

##Se guarda en 
write_rds(Factor_ex, "Resultado_final/EEG_2020.rds")
write.csv(Factor_ex, "Resultado_final/EEG_2020.csv", )

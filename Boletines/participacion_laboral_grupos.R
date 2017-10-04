#-------------------------------------------------------------------------------
# fuerza de trabajo y tasa de participacion de Nuble
#-------------------------------------------------------------------------------
#
setwd("C:\\Users\\Usuario\\Documents\\scripts R\\")

n = length(info)

fuerza_trabajo_nuble = lapply(1:n, function(i) svytotal(~I(cae_general=="Ocupado" | 
                                                             cae_general=="Desocupado" | 
                                                             cae_general=="Busca Trabajo Primera Vez"), 
                                                        subset(info[[i]],prov==84), multicore= TRUE, 
                                                        drop_empty_groups = FALSE, na_rm=TRUE))

fuerza = unlist (lapply(fuerza_trabajo_nuble,'[',2))
std_error_fuerza = unlist(lapply(fuerza_trabajo_nuble, SE))[seq(2,2*n,2)]



tasa_participacion_nuble =lapply(1:n, function(i) svyratio(~I(cae_general=="Ocupado" | 
                                                                cae_general=="Desocupado" | 
                                                                cae_general=="Busca Trabajo Primera Vez"), 
                                                           denominator=~I(edad>=15), subset(info[[i]],prov==84), 
                                                           multicore= TRUE, drop_empty_groups = FALSE, na_rm=TRUE))

tasa = unlist (lapply(tasa_participacion_nuble,'[',1))
std_error_tasa = unlist(lapply(tasa_participacion_nuble, SE))

data = cbind(fuerza, std_error_fuerza, tasa, std_error_tasa)
rownames(data) = NULL

write.csv(data, "tasa_participacion.csv")

tasa_participacion_nuble_sexo =lapply(1:n, function(i) svyby(~I(cae_general=="Ocupado" | 
                                                                cae_general=="Desocupado" | 
                                                                cae_general=="Busca Trabajo Primera Vez"), by=~sexo,
                                                           denominator=~I(edad>=15), subset(info[[i]],prov==84), svyratio,
                                                           multicore= TRUE, drop_empty_groups = FALSE, na_rm=TRUE)) %>% rbindlist()
names(tasa_participacion_nuble_sexo) = c("sexo", "tasa_sexo", "std_error_sexo")
tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, cv = (std_error_tasa/tasa_sexo)*100)

frecuencia  = lapply(1:n, function(i) group_by(subset(info[[i]]$variables, prov==84),
                     ocupado = I(cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez"),sexo) %>%
                       count() %>% filter(ocupado==TRUE)) %>% rbindlist()

tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, frecuencia = frecuencia$n)

write.csv(tasa_participacion_nuble_sexo, "tasa_participacion_sexo.csv")

tasa_participacion_nuble_sexo =lapply(1:n, function(i) svyby(~I(cae_general=="Ocupado" | 
                                                                  cae_general=="Desocupado" | 
                                                                  cae_general=="Busca Trabajo Primera Vez"), by=~sexo,
                                                             denominator=~I(edad>=15), subset(info[[i]],prov==84 & edad>=15 & edad<=29), svyratio,
                                                             multicore= TRUE, drop_empty_groups = FALSE, na_rm=TRUE)) %>% rbindlist()
names(tasa_participacion_nuble_sexo) = c("sexo", "tasa_sexo", "std_error_sexo")
tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, cv = (std_error_tasa/tasa_sexo)*100)

frecuencia  = lapply(1:n, function(i) group_by(subset(info[[i]]$variables, prov==84 & edad>=15 & edad<=29),
                                               ocupado = I(cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez"),sexo) %>%
                       count() %>% filter(ocupado==TRUE)) %>% rbindlist()

tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, frecuencia = frecuencia$n)

write.csv(tasa_participacion_nuble_sexo, "tasa_participacion_sexo_jovenes.csv")

tasa_participacion_nuble_sexo =lapply(1:n, function(i) svyby(~I(cae_general=="Ocupado" | 
                                                                  cae_general=="Desocupado" | 
                                                                  cae_general=="Busca Trabajo Primera Vez"), by=~sexo,
                                                             denominator=~I(edad>=15), subset(info[[i]],prov==84 & edad>29), svyratio,
                                                             multicore= TRUE, drop_empty_groups = FALSE, na_rm=TRUE)) %>% rbindlist()
names(tasa_participacion_nuble_sexo) = c("sexo", "tasa_sexo", "std_error_sexo")
tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, cv = (std_error_tasa/tasa_sexo)*100)

frecuencia  = lapply(1:n, function(i) group_by(subset(info[[i]]$variables, prov==84 & edad>=30),
                                               ocupado = I(cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez"),sexo) %>%
                       count() %>% filter(ocupado==TRUE)) %>% rbindlist()

tasa_participacion_nuble_sexo = mutate(tasa_participacion_nuble_sexo, frecuencia = frecuencia$n)

write.csv(tasa_participacion_nuble_sexo, "tasa_participacion_sexo_adultos.csv")


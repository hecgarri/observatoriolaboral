iure = lapply(1:length(info), function(x) svyby(~I(b10==1 | b10==2), by=~sector, 
            design = subset(info[[x]], prov_e==84), svytotal, na.rm=TRUE, na.rm.all = TRUE) %>% 
                filter(sector=="Agropecuario-Silvicola")) %>% rbindlist() %>%
  select(sector,`I(b10 == 1 | b10 == 2)TRUE`, `se.I(b10 == 1 | b10 == 2)TRUE`) %>%
  mutate(cv = `se.I(b10 == 1 | b10 == 2)TRUE`/`I(b10 == 1 | b10 == 2)TRUE`) 


ocup_agro = lapply(1:(length(info)), function(x) svyby(~I(cae_general=="Ocupado"), by=~sector, 
              design = subset(info[[x]], prov_e==84), svytotal, na.rm=TRUE, na.rm.all = TRUE) %>% 
                filter(sector=="Agropecuario-Silvicola")) %>% rbindlist() 


trabajadores = cbind(iure, ocup_agro)
write.csv(iure, "trabajadores y temporeros agro.csv")


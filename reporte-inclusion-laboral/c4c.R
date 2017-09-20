##############################################################################
# Cuadro 4. Brecha de género en ingresos para trabajadores dependientes,
# ingresos de los dependientes según sexo y nivel educacional de Ñuble, 2015.
##############################################################################

diseño$variables$ingreso_hora = diseño$variables$yoprCor/(diseño$variables$o10*4)

ingreso_educ = svyby(~ingreso_hora, design = subset(diseño, provincia==84),
                     by=~nivel_educ+sexo, svymean,na.rm.all = TRUE, na.rm=TRUE) %>% 
  mutate(cv = se/ingreso_hora, ingreso_hora=round(ingreso_hora,0))

temp = xtabs(~nivel_educ+sexo, data=subset(diseño$variables, provincia==84)) %>% data.frame() 

ingreso_educ$frecuencia = temp$Freq


ingreso_sexo = svyby(~ingreso_hora, design = subset(diseño, provincia==84),
                     by=~sexo, svymean,na.rm.all = TRUE, na.rm=TRUE)

write.csv(ingreso_educ, "brecha_genero.csv")

#################################################
# Contrastes de Hipótesis 
#################################################
diseño$variables = mutate(diseño$variables, nivel_educ = ifelse(educ==0 | educ==1,
                              "Básica incompleta o menor",
                              ifelse(educ==2, "Básica completa", 
                              ifelse(educ==3 | educ==4, "Básica completa", 
                              ifelse(educ==5 | educ==6, "Media completa", 
                              ifelse(educ==7, "Media completa", 
                              ifelse(educ==8, "Técnico Nivel Superior Completo", 
                              ifelse(educ==9, "Media completa", 
                              ifelse(educ==10, "Profesional Completo o más", 
                              ifelse(educ==11, "Profesional Completo o más", 
                              ifelse(educ==12, "Profesional Completo o más", NA)))))))))))
                              
niveles = names(table(diseño$variables$nivel_educ))

options(survey.lonely.psu = "certainty")
diseños = lapply(niveles,function(x) svydesign(id = ~varunit,
         strata = ~varstrat, weights = ~expr, nest = TRUE,
         data = subset(diseño$variables, provincia==84 & nivel_educ==x)))

contraste_ = list()
for (i in 1:length(diseños)){
  contraste_[[i]] = svyttest(ingreso_hora~sexo, diseños[[i]])
}


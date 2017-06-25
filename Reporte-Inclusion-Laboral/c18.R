###################################################################################
#Cuadro 18. Población perteneciente a pueblos indígenas de la <Región de...>, 2015
####################################################################################

diseño$variables = mutate(diseño$variables,
                          indigena =  ifelse(r3!=10 & r3!=99, "Indigena",
                                      ifelse(r3==10, "No indigena",NA)))

pert_indigena = svytotal(~I(indigena=="Indigena"), diseño, na.rm=TRUE, multicore = TRUE)

pert_indigena.provincia = svyby(~I(indigena=="Indigena"), by=~provincia, diseño, svytotal, na.rm=TRUE, multicore = TRUE)


write.csv(pert_indigena, "indigena_region.csv", fileEncoding = "WINDOWS-1252")
write.csv(pert_indigena.provincia, "indigena_provincia", fileEncoding = "WINDOWS-1252")


setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo")


#Grupos 

#https://www.facebook.com/groups/ventasytrabajo/?ref=br_rs

#https://www.facebook.com/groups/758569620901263/?ref=br_rs

#https://www.facebook.com/groups/340574436122303/?ref=br_rs

#https://www.facebook.com/groups/264643917279452/

#https://www.facebook.com/groups/331566843583567/?ref=br_rs

#https://www.facebook.com/Trabajos-Octava-Region-326618694129317/

#https://www.facebook.com/Radio-%C3%91uble-La-Sintonia-Grande-251084981663158/


#Páginas 

#https://www.facebook.com/omil.chillanviejo?ref=br_rs
#https://www.facebook.com/omil.yungay?ref=br_rs
#https://www.facebook.com/omil.bulnes?ref=br_rs
#https://www.facebook.com/omil.chillan/
# https://www.facebook.com/sencebiobio/?ref=br_rs&hc_ref=SEARCH

if (!require(devtools)) install.packages("devtools")
if (!require(Rfacebook)) install_github("pablobarbera/Rfacebook/Rfacebook")

my_oauth = fbOAuth(app_id = "300012520434375", app_secret = "d3d57c77ca1fc005ea49f1c4377cb21c", extended_permissions = TRUE)

save(my_oauth, file="my_oauth")

load("my_oauth")


# VENTAS Y TRABAJO CHILLÁN
group1 = getGroup(group_id = 1413941602178262, token = my_oauth, n=Inf)

x = " Facebook VENTAS Y TRABAJO CHILLÁN"
write.csv(group1, paste0(Sys.Date(), x, ".csv"))

# Persa Chillan Online 
searchGroup('persachillanonline', token = my_oauth)
group2 = getGroup(group_id = 426649290827068, token = my_oauth, n=100000)

x = " Persa Chillan Online"
write.csv(group2, paste0(Sys.Date(), x, ".csv"))


anuncios = group2 %>% filter(str_detect(message, 'trabajo|empleo|personal|trabajar|currículum|
                                      curriculum|pega|chofer|trabaje|maestro|temporero|cocinero|
                                        vendedor|peluquera| part time|carpintero|gasfiter| aseo'))



searchGroup('DATOS DE TRABAJO EN CHILLAN', token = my_oauth)
group3 = getGroup(group_id = 241293439569652, token = my_oauth)


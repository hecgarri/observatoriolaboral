
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Facebook")


# token generated here: https://developers.facebook.com/tools/explorer 
# El token es temporal, hay que redirigirse a la dirección cada vez. 


token = "EAACEdEose0cBADoLQugjih9alc0YZB3zDVCPfEAWvgmYcHqkeekmGKKTqc4dqQZCxbF52EP4UFoa9PqXnUOt9RIOCIDk3fLZBj6hZCA7KEOA3PIQrBYQCwZB7bXEiBhXA07Pt6hAV93tGXqERV13hGQZBiXZC3JU7R4xt2jqlDdsho7RuiJBf8uAdDh3B9875gZD"


me <- getUsers("me", token, private_info=TRUE)

group3 = getGroup(group_id = 241293439569652, token = token, n=Inf)

write.csv(group3, paste0((Sys.Date()-1)," Datos de trabajo en Chillan.csv"))


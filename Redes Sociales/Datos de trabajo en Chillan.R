
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Redes Sociales")


# token generated here: https://developers.facebook.com/tools/explorer 
# El token es temporal, hay que redirigirse a la dirección cada vez. 
if (!require(Rfacebook)) devtools::install_github("pablobarbera/Rfacebook/Rfacebook")

token = "EAACEdEose0cBADgKUdFCfJogYzZCm6bB8mV0J6ZARAV8sXUThO7uRDDSwZCZAuspynfOVXuDkZCWZA3mtxO8EluIX37vfX5gtgfVeEkalGZAM9mEu6H6zbz4gPaDMpR0ZBPu44B9G1elWniD1QoDnS34knkvUNJDm0TzSEyvhwhZCZC2mZCnwEHDnLhEfxXpVjTcXoZD"


me <- getUsers("me", token, private_info=TRUE)

group3 = getGroup(group_id = 241293439569652, token = token, n=Inf)

write.csv(group3, paste0((Sys.Date()-1)," Datos de trabajo en Chillan.csv"))


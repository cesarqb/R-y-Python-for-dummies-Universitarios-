#Diferentes paquetes dependiendo del tipo de base de datos a
#conectarse desde R:
#MySQL =??? RMySQL
#PostgresSQL =??? RPostgresSQL
#Oracle Database =??? ROracle
#Para interactuar con la base de datos se necesita el paquete DBI , el cual funciona como una interfaz


library(DBI) # Cargar el paquete DBI
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "datamini_class",
                 host = "dataminingperu.com",
                 user = "datamini_class",
                 password = "DXv=XqnQDK?z")

dbListTables(con)#Listar las tablas existentes en la base de datos


#. Importar los clientes mayores de 88 a~nos de sexo femenino.
#Mostrar la Edad, Estado Civil y Sexo
dbGetQuery(con, "SELECT EDAD, ESTADO_CIVIL, SEXO FROM Reduc_BD_CLIENTE
WHERE EDAD > 88 AND SEXO = 'F'")



#Archivos planos en la Web
#

publicidad <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
head(publicidad)




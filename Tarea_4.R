#Paquetes a instalar en caso de que no esten
install.packages('arules')
install.packages('arulesViz')

library(arules)
library(arulesViz)

#cargamos la data desde los csv
ejemplo <- read.csv("ejemplo.csv", header = TRUE)
periodico <- read.csv("periodico.csv", header = TRUE)

#elimino la columna que no hace falta
periodico <- periodico[ ,2:5]
periodico

#separo las columnas
ID <- periodico[ ,1]
entry <- periodico[ ,2]
exit <- periodico[ ,3]
articles <- periodico[ ,4]



texto <- entry[1]
texto


??strptime()

texto_columnas = data.frame(unlist(texto_split))


data(ejemplo)

for(i in periodico){
  fecha <- periodico$entry[i]
}

fecha[2]

#Este es nuestro elemento inicial
texto=c("Elemento","de","tabla")

#Tenemos que generar un data frame con con las palabras
#que componen este vector
texto_split = strsplit(texto, split=" ")
texto_columnas = data.frame(unlist(texto_split))


var <- texto_columnas$unlist.texto_split

var[1]
var[2]
var[3]

for(i in texto_split){
  
  fecha_entry[i] <- 
  hora_entry[i] <- 
  fecha_exit[i] <- 
  hora_exit[i] <- 
}









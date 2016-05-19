#Deportes 1 del 1-9 
#Politica 2  del 10-18
#Variedades 3 del 19-27
#Internacional 4 del 28-36
#Nacionales 5 del 37-45
#Sucesos 6 del 46-54
#Comunidad 7 del 55-63
#Negocios 8 del 64-72
#Opinion 9 del 73-81

#Paquetes a instalar en caso de que no esten
#install.packages("plyr")
#install.packages('arules')
#install.packages('arulesViz')

library(arules)
library(arulesViz)
library(plyr)

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

#??strptime()

texto_columnas = data.frame(unlist(texto_split))

#Este es nuestro elemento inicial
texto=c("Elemento","de","tabla")

#Tenemos que generar un data frame con con las palabras
#que componen este vector
texto_split = strsplit(texto, split=" ")
texto_columnas = data.frame(unlist(texto_split))


var <- texto_columnas$unlist.texto_split

salida= data.frame(X=periodico$X,
                   ID=periodico$ID,
                   entry=periodico$entry,
                   exit=periodico$exit,
                   articulos=periodico$articles)
salida$articles='1'
salida$tiempo='1'

matriz=matrix(data = 0, nrow = nrow(salida), ncol = 81)
matriz=as.data.frame(matriz)
#por extension agregamos todos los posibles arreglos disponible
colnames(matriz)= c("deportes/articulo1","deportes/articulo2","deportes/articulo3","deportes/articulo4","deportes/articulo5","deportes/articulo6","deportes/articulo7","deportes/articulo8","deportes/articulo9","politica/articulo1","politica/articulo2","politica/articulo3","politica/articulo4","politica/articulo5","politica/articulo6","politica/articulo7","politica/articulo8","politica/articulo9",
                    "variedades/articulo1","variedades/articulo2","variedades/articulo3","variedades/articulo4","variedades/articulo5","variedades/articulo6","variedades/articulo7","variedades/articulo8","variedades/articulo9","internacional/articulo1","internacional/articulo2","internacional/articulo3","internacional/articulo4","internacional/articulo5","internacional/articulo6","internacional/articulo7","internacional/articulo8","internacional/articulo9",
                    "nacionales/articulo1","nacionales/articulo2","nacionales/articulo3","nacionales/articulo4","nacionales/articulo5","nacionales/articulo6","nacionales/articulo7","nacionales/articulo8","nacionales/articulo9","sucesos/articulo1","sucesos/articulo2","sucesos/articulo3","sucesos/articulo4","sucesos/articulo5","sucesos/articulo6","sucesos/articulo7","sucesos/articulo8","sucesos/articulo9",
                    "comunidad/articulo1","comunidad/articulo2","comunidad/articulo3","comunidad/articulo4","comunidad/articulo5","comunidad/articulo6","comunidad/articulo7","comunidad/articulo8","comunidad/articulo9","negocios/articulo1","negocios/articulo2","negocios/articulo3","negocios/articulo4","negocios/articulo5","negocios/articulo6","negocios/articulo7","negocios/articulo8","negocios/articulo9","opinion/articulo1","opinion/articulo2","opinion/articulo3","opinion/articulo4","opinion/articulo5","opinion/articulo6","opinion/articulo7","opinion/articulo8","opinion/articulo9")
transaccion_bot=c()

#Recorremos todas las filas del set de datos de los articulos de periodicos
for (i in 1:nrow(periodico)) {
  
  articulo=''
  #Tomamos el articulo y lo modificamos
  articulo=periodico$articles[i]
  #Pasamos todo a string ya que tiene números
  articulo=toString(articulo)
  
  #obtenemos el numero del item del articulo
  numero=unlist(articulo)
  numero=strsplit(numero, "[^0-9]+") #segun el número que posee
  numero= unlist(numero)
  numero= as.numeric(numero)
  numero=na.omit(numero)
  numero=unique(numero)
  
  
  for (k in 1:length(numero)) {
    
    #si contiene solo un numero agregamos la descripción con el número
    if (numero[k]>=1 & numero[k]<=9) {
          articulo=paste(articulo,"deportes/articulo",numero[k],",")
          nombre=paste("deportes/articulo",numero[k])
          nombre=gsub(" ","",nombre) 
          indice=grep(nombre, colnames(matriz))
          matriz[i,indice]=1
          
    }else 
    if (numero[k]>=10 & numero[k]<=18) {
          
          #llevamos a caracter
          segundo_numero = as.character(numero[k])
          #separamos el numero del item
          segundo_numero = strsplit(segundo_numero, "")
          #lo convertimos a numero
          segundo_numero = as.numeric(segundo_numero[[1]])
          #lo sumamos
          segundo_numero = sum(segundo_numero)
          
      if (segundo_numero>9) {
          #separamos al numero del resto del string
          segundo_numero = as.character(segundo_numero)
          segundo_numero = strsplit(segundo_numero, "")
          segundo_numero = as.numeric(segundo_numero[[1]])
          #realizamos la suma de los numeros
          segundo_numero = sum(segundo_numero)
      }
          
      articulo=paste(articulo,"politica/articulo",segundo_numero,",")
      nombre=paste("politica/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=19 & numero[k]<=27) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"variedades/articulo",segundo_numero,",")
      nombre=paste("variedades/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=28 & numero[k]<=36) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"internacional/articulo",segundo_numero,",")
      nombre=paste("internacional/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=37 & numero[k]<=45) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"nacionales/articulo",segundo_numero,",")
      nombre=paste("nacionales/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=46 & numero[k]<=54) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"sucesos/articulo",segundo_numero,",")
      nombre=paste("sucesos/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=55 & numero[k]<=63) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"comunidad/articulo",segundo_numero,",")
      nombre=paste("comunidad/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=64 & numero[k]<=72) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos con el primero
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"negocios/articulo",segundo_numero,",")
      nombre=paste("negocios/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }else 
    if (numero[k]>=73 & numero[k]<=81) {
      
      #llevamos a caracter
      segundo_numero = as.character(numero[k])
      #separamos el numero del item
      segundo_numero = strsplit(segundo_numero, "")
      #lo convertimos a numero
      segundo_numero = as.numeric(segundo_numero[[1]])
      #lo sumamos con el primero
      segundo_numero = sum(segundo_numero)
      
      if (segundo_numero>9) {
        
        #separamos al numero del resto del string
        segundo_numero = as.character(segundo_numero)
        segundo_numero = strsplit(segundo_numero, "")
        segundo_numero = as.numeric(segundo_numero[[1]])
        #realizamos la suma de los numeros
        segundo_numero = sum(segundo_numero)
        
      }
      
      articulo=paste(articulo,"opinion/articulo",segundo_numero,",")
      nombre=paste("opinion/articulo",segundo_numero)
      nombre=gsub(" ","",nombre)
      indice=grep(nombre, colnames(matriz))
      matriz[i,indice]=1
      
    }
  }
  
  articulo=gsub(" ","",articulo) 
  articulo=substr(articulo,1,nchar(articulo)-1)
  salida$articles[i]=articulo
  
  entry = strftime(periodico$entry[i], format="%H:%M:%S")
  entry=as.POSIXct(entry,format="%H:%M:%S")
  exit = strftime(periodico$exit[i], format="%H:%M:%S")
  exit=as.POSIXct(exit,format="%H:%M:%S")
  resta = difftime(exit,entry,units="secs")
  
  if (resta<20) {
    
    transaccion_bot=c(transaccion_bot, i)
    
  }
  
  salida$tiempo[i]=resta
  
}

salida=salida[-transaccion_bot,]
matriz=matriz[-transaccion_bot,]


#nuevo set de datos de salida
datos_de_salida = salida
datos_de_salida$X=NULL
datos_de_salida$entry=NULL
datos_de_salida$exit=NULL
datos_de_salida$articulos=NULL
datos_de_salida$articles=NULL

datos_de_salida$tiempo=as.numeric(datos_de_salida$tiempo)

arr=arrange(datos_de_salida, tiempo)   
datos_de_salida=datos_de_salida[ order(datos_de_salida$tiempo), ] 

    #Visitas con mayor tiempo de estadia
            visitas_con_mayor=head(datos_de_salida,10)
    #Visitas con menor tiempo de estadia
            visitas_con_menor=tail(datos_de_salida,10)
    #transacciones con mayor numero de apariciones en el dataset
            tab=table(datos_de_salida$ID)
            sort=sort(tab,decreasing = T)
            apariciones=head(sort,10)



            
            
            
            
            
            
            
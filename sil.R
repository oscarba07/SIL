# Paqueteria --------------------------------------------------------------
# Carga de paquetería necesaria
packs <- c('xml2','rvest','stringr','tidyr','dplyr','purrr','plyr','ggplot2',
           'plotly')

for (p in packs) {
  if (!require(p, character.only = T)) {
    install.packages(p)
    library(p, character.only = T)
  }
}

# Descarga de datos -------------------------------------------------------
# Preparacion
colnames <- c('No.','tipo.de.asunto','denom','clasif','origen',
              'fecha.presentacion','presentada.por','p.politico','legislatura',
              'turnado.a','estatus','tema')

# Crear funcion para leer html
scrap.sil <- function(url){
  response <- read_html(url)
  tbls <- html_nodes(response, 'table')
  rm(response)
  closeAllConnections()
  inic <- tbls[[6]] 
  
  # Extraer links para objeto de iniciativas
  links <- inic %>% html_elements('a') %>% html_attr('onclick')
  links <- links[grep('ContenidoAsuntos',links)]  
  links <- str_extract(links,'/Librerias/pp_ContenidoAsuntos\\.php\\?SID=&Clave=\\d*')
  links <- paste0('http://sil.gobernacion.gob.mx', links)
  
  # Crear tabla
  inic <- inic %>% html_table(fill = TRUE)
  inic <- inic[-c(1:5),]
  names(inic) <-  colnames
  
  # Asignacion de objeto de iniciativa
  obj <- sapply(links,FUN = inic_obj) %>% unname()
  inic$objeto <- obj
  return(inic)
}

# Crear funciion para extraer objeto de iniciativas
inic_obj <- function(v){
  h <- read_html(v)
  h <- html_nodes(h, 'table')[[3]] %>% html_table()
  obj <- h$X1[[4]]
  rm(h)
  closeAllConnections()
  return(obj)
}

# Definicion de URL
url <- 'http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=79598c75d82eba56a1b26f87e0831ee8&Reg=6594&Paginas=15&pagina=2'
tot <- 6594
url <- sub('Paginas=\\d+', paste0('Paginas=',min(tot,999)), url)
url <- sub('pagina=2', 'pagina=1', url)
# Determinar total de paginas de resultados
n.pag <-  ceiling(tot/min(tot,999))
# Definir vector de urls
urls <- vector(length = n.pag)
for (i in 1:n.pag) {
    urls[i] <- sub('pagina=1', paste0('pagina=',i), url)
  }

  
inic <- lapply(urls, FUN=scrap.sil) %>% bind_rows()

# Limpieza de datos -------------------------------------------------------
# Split temas
t <- str_split(inic$tema,'\\d.-')
# Subset para eliminar blanks
t <- lapply(t, FUN= function(x) subset(x,x!='')) 
temas <- t %>% unlist() %>% unique() %>% str_replace_all(pattern = ' ','\\.')
# Funcion para convertir a dataframes con dummies
todf <- function(x){
  x.names <- x
  x <- matrix(1,nrow=1,ncol=length(x),dimnames=list('row',x.names)) %>% 
    data.frame()
}
# Convertir a dataframe con dummies
tdummy_df <- sapply(t, todf) %>% bind_rows() %>% replace(is.na(.),0)

# Unir a dataframe
inic <- cbind(inic,tdummy_df)

# Limpiar casos donde la iniciativa es presentada por un partido politico
inic[which(inic$p.politico=='-'),'p.politico'] <- inic[which(inic$p.politico=='-'),
                                                     "presentada.por"]
# Cambiar 'Congreso de...' por 'Congreso estatal'
inic[str_detect(inic$p.politico,'Congreso de'),'p.politico'] <- "Congreso estatal"

# Renombrar Organo de Gobierno
inic[str_detect(inic$p.politico,'Órgano de'),'p.politico'] <- "Órgano de Gobierno"

# Analisis de datos -------------------------------------------------------
# Numero de iniciativas por partido politico
tot.pp <- inic %>% group_by(p.politico) %>% summarise(n=n())
# Grafico
cols <- c('Morena'='#600c0c','PAN'='#00308f','PRD'='#ffcc01','PRI'='#006600',
          'MC'='#f77821','PVEM'='#50b646','PT'='#da251c', 'PES'='#592a7b',
          'Ejecutivo Federal'='#c8b64f','Congreso de Michoacán'='#691549')

p <- ggplot(tot.pp, aes(x=n,y=p.politico, fill=p.politico)) +
  scale_fill_manual(values=cols) +
  geom_bar(stat="identity", colour="white") + guides(fill="none") + 
  labs(title='Numero de iniciativas totales por partido politico',
       x='Numero de iniciativas',y='Partido politico') +
  scale_x_continuous(breaks=seq(0,max(tot.pp$n)+20,20))
ggplotly(p)

# Numero de iniciativas por tema y partido politico
inic.pp <- aggregate(inic[,temas], by = list(inic$p.politico), FUN = sum)
# Formato Base de datos
inic.pp <- inic.pp %>% pivot_longer(cols = temas, values_to = 'n.inic') %>% 
  arrange(desc(n.inic))
# Grafico
p <- ggplot(inic.pp, aes(x=n.inic,y=name, fill=Group.1)) +
  scale_fill_manual(values=cols) +
  geom_bar(stat="identity", colour="white") + 
  labs(title='Numero de iniciativas por tema',
       x='Numero de iniciativas',y='Tema',fill='Partido politico') +
  scale_x_continuous(breaks=seq(0,sum(inic.pp$n.inic)+20,20))
ggplotly(p)


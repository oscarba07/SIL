# Carga de paquetería necesaria
packs <- c('xml2','rvest','stringr','tidyr','dplyr','purrr','plyr','ggplot2',
           'plotly')

for (p in packs) {
  if (!require(p, character.only = T)) {
    install.packages(p)
    library(p, character.only = T)
  }
}

# Definicion de URL
url <- 'http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=334c274a03d349cf633b61b01cc8feab&Reg=466&Paginas=15&pagina=2'
tot <- 50
url <- sub('Paginas=\\d+', paste0('Paginas=',tot), url)
url <- sub('pagina=2', 'pagina=1', url)


# Lectura de tabla
colnames <- c('No.','tipo.de.asunto','denom','clasif','origen',
              'fecha.presentacion','presentada.por','p.politico','legislatura',
              'turnado.a','estatus','tema')
response <- read_html(url)
#download.file(url, destfile = "scrapedpage.html",method='libcurl', quiet=TRUE)
#response <- read_html('SIL.html')
tbls <- html_nodes(response, 'table')
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

# Crear funciion para extraer objeto de iniciativas
inic_obj <- function(v){
  #download.file(v, destfile = "inic_obj.html",method='libcurl', quiet=TRUE)
  #h <- read_html("inic_obj.html")
  h <- read_html(v)
  h <- html_nodes(h, 'table')[[3]] %>% html_table()
  obj <- h$X1[[4]]
  return(obj)
}

# Asignacion de objeto de iniciativa
obj <- sapply(links,FUN = inic_obj) %>% unname()
inic$objeto <- obj

head(inic)

# Split temas
t <- str_split(inic$tema,'\\d.-')
# Subset para eliminar blanks
t <- lapply(t, FUN= function(x) subset(x,x!='')) 
temas <- t %>% unlist() %>% unique() %>% str_replace_all(pattern = ' ','\\.')
# Funcion para convertir a dataframes con dummies
todf <- function(x){
  x.names <- x
  x <- matrix(1,nrow=1,ncol=length(x),dimnames=list('row',x.names)) %>% data.frame()
}
# Convertir a dataframe con dummies
tdummy_df <- sapply(t, todf) %>% bind_rows() %>% replace(is.na(.),0)

# Unir a dataframe
inic <- cbind(inic,tdummy_df)

# Limpiar casos donde la iniciativa es presentada por un partido politico
inic[which(inic$p.politico=='-'),'p.politico'] <- inic[which(inic$p.politico=='-'),
                                                     "presentada.por"]

# Numero de iniciativas por tema y partido politico
inic.pp <- aggregate(inic[,temas], by = list(inic$p.politico), FUN = sum)
# Formato Base de datos
inic.pp <- inic.pp %>% pivot_longer(cols = temas, values_to = 'n.inic')  
# Grafico
cols <- c('Morena'='#600c0c','PAN'='#00308f','PRD'='#ffcc01','PRI'='#006600',
          'MC'='#f77821','PVEM'='#50b646','PT'='#da251c',
          'Ejecutivo Federal'='#c8b64f','Congreso de Michoacán'='#691549')
p <- ggplot(inic.pp, aes(x=n.inic,y=name, fill=Group.1)) +
  scale_fill_manual(values=cols) +
  geom_bar(stat="identity", colour="white") + 
  labs(x='Numero de iniciativas',y='Tema',fill='Partido politico') +
  scale_x_continuous(breaks=1:50)
ggplotly(p)


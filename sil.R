url <- 'http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=8a35e4f0f8df2e4837b3a5295a3df854&Reg=460&Paginas=15&pagina=2'
tot <- 460
url <- sub('Paginas=\\d+', paste0('Paginas=',tot), url)
url <- sub('pagina=2', 'pagina=1', url)

# Carga de paquetería necesaria
packs <- c('xml2','rvest','stringr','tidyr','dplyr','purrr')

for (p in packs) {
  if (!require(p, character.only = T)) {
    install.packages(p)
    library(p, character.only = T)
  }
}

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

inic$tema.n <- t
inic <- inic %>% mutate(n = map_dbl(.x=tema.n,.f=length))

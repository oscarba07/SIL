# Carga de paquetería necesaria
packs <- c('xml2','rvest','stringr')

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
download.file(url, destfile = "scrapedpage.html",method='libcurl', quiet=TRUE)
response <- read_html('SIL.html')
tbls <- html_nodes(response, 'table')
inic <- tbls[[6]] 

# Extraer links para objeto de iniciativas
links <- inic %>% html_elements('a') %>% html_attr('onclick')
links <- links[grep('ContenidoAsuntos',links)]  
links <- str_extract(links,'/Librerias/pp_ContenidoAsuntos\\.php\\?SID=&Clave=\\d*')
links <- paste0('http://sil.gobernacion.gob.mx', links)

inic_obj <- function(v){
  #download.file(v, destfile = "inic_obj.html",method='libcurl', quiet=TRUE)
  h <- read_html("inic_obj.html")
  h <- read_html(v)
  h <- html_nodes(h, 'table')[[3]] %>% html_nodes(xpath='/html/body/table[3]/tbody/tr[4]/td/text()')
}

# Crear tabla
inic <- inic %>% html_table(fill = TRUE)
inic <- inic[-c(1:5),]
names(inic) <-  colnames


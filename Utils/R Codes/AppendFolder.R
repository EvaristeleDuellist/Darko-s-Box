# Carga en un solo dataset todos los archivos de cierta extension dentro de una caperta especificada

Cargar.Datos<-function (carpeta, extension="csv", exhaustivo=F, Fuente=T, n_ultimos=0, ordenado=T, ausentes=getOption("datatable.na.strings","NA"), 
                        separador="auto", dec=".", quote="\"", header="auto", clases=NULL){
    
    require("dplyr"); require("data.table"); require("qdapRegex")
  
    if(ordenado) file.list <- paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = F))
    else file.list <- sort(paste0(carpeta,"/",list.files(carpeta, pattern = paste0("+.",extension), recursive = F)))
    m=length(file.list)
    n=ifelse((n_ultimos>=m| n_ultimos<1),1, m-(n_ultimos-1))
    
    file.list<-file.list[n:length(file.list)]
    
    print("Importando:: ")
    print(file.list)
    Union <- do.call("bind_rows",
                     lapply(file.list, FUN = function(file) {
                         fread(file, sep=separador, dec=dec, quote=quote, header = header,
                               na.strings = ausentes,
                               col.names = names(fread(file.list[1], nrows = 1)),
                               colClasses = clases
                         ) %>% mutate(Source=as.character(rm_between(gsub(fold, "", file), "/", ".", extract=TRUE)[[1]]))
                       }
                     )
    )
    if(!Fuente)
      Union<-Union %>% select(-Source)
    return(Union)
}

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(x))
  if(espacios){
    x<-gsub("\\W\\s","",iconv(x,to="ASCII//TRANSLIT"))
    } else {
    x<-gsub("\\W","",iconv(x,to="ASCII//TRANSLIT"))
    }
}

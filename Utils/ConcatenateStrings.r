# Une cadenas de caracteres con la opcion de eliminar del resultado los NAs

Unir.Cadenas <- function(..., sep = " ", collapse = NULL, na.rm = F) {
    if (!na.rm)
        paste(..., sep = sep, collapse = collapse)
    else
        if (na.rm == T) {
            paste.na <- function(x, sep) {
                x <- gsub("^\\s+|\\s+$", "", x)
                ret <- paste(na.omit(x), collapse = sep)
                is.na(ret) <- ret == ""
                return(ret)
            }
            df <- data.frame(..., stringsAsFactors = F)
            ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
            
            if (is.null(collapse))
                ret
            else {
                paste.na(ret, sep = collapse)
            }
        }
}

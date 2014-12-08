require(jsonlite)
# Pasamos el URL de API endpoint, y la API developer key
# por defecto sólo retorna los datos, sin metadata
get_data <- function (api_url, api_key, include_metadata=FALSE) {
    api_data <- fromJSON(paste0(api_url, "?auth_key=", api_key))
    data <- extract_dataframe(api_data)
    metadata <- list()
    if (include_metadata==TRUE) {
        metadata <- api_data[c("id", "title", "description", "user",
                               "tags", "created_at", "source", "link")]
        # fTimestamp en milisegundos (aparentemente)
        metadata["timestamp"] <- api_data$result$fTimestamp / 1000
    }
    return(list(metadata=metadata, data=data))
}

# Hace el trabajo de convertir los datos de un formato secuencial
# a una data.frame de R
extract_dataframe <- function(api_data) {
    header <- subset(api_data$result$fArray, fHeader==TRUE)$fStr
    values <- subset(api_data$result$fArray, is.na(fHeader))$fStr
    header_len <- api_data$result$fCols
    df <- as.data.frame(
        matrix(values,
               ncol=header_len,
               byrow = TRUE),
        stringsAsFactors=FALSE)
    colnames(df) <- header
    return(df)
}
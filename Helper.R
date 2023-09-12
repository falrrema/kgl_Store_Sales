####################################################
# Funciones R para el apoyo analítico y desarrollo #
####################################################

# Estas son funciones que apoyan de forma accesoria los proceso mensuales en multiples partes del codigo.
# No necesariamente se catalogan.
# Muchas de estas funciones son generalizables a funciones compartidas del equipo de analytics.
# No obstante, dado lo critico de los procesos mensuales estas no pueden ubicarse en sitios compartidos
# y deben estan en un sitio seguro lejos de las modificaciones de terceros.
options(repos = "https://cran.dcc.uchile.cl/")
options(scipen = 9999)
Sys.setenv(PATH = paste(Sys.getenv("PATH"),":/opt/teradata/client/17.00/bin"))

# Criticos de uso habitual ------------------------------------------------
# Para el blablabla
bla <- function(...) {
  writeLines(paste(..., sep = " "))
}

fun_print <- function(df) {
  df %>% print(n = 100)
}

# Paquetes e instalacion de requerimientos --------------------------------
# Installation
install_these_packages <- function(listPackages) {
  new.packages <- listPackages[!(listPackages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
}

# Connections
# connect_libs <- function(loadLibrary = TRUE) {
#   packages <- c("RODBC")
#   install_these_packages(packages)
#   
#   if (loadLibrary) invisible(capture.output(lapply(packages, function(t) library(t, character.only = TRUE))))
# }

# Data wrangling, manipulation and processing
datawrang_libs <- function(loadLibrary = TRUE) {
  packages <- c("plotly", "stringr", "lubridate", "pbapply", "ggrepel", "ggthemes", "data.table", "lubridate", "readxl", "tm", "writexl", "sparklyr", "tidyverse") # dbplyr
  install_these_packages(packages)
  
  
  if (loadLibrary) invisible(capture.output(lapply(packages, function(t) library(t, character.only = TRUE))))
}

# MLlibs
ml_libs <- function(loadLibrary = TRUE) {
  packages <- c("tidymodels", "modeltime")
  install_these_packages(packages)
  if (loadLibrary) invisible(capture.output(lapply(packages, function(t) library(t, character.only = TRUE))))
}

# Conexion Bases de Datos -------------------------------------------------
# Conexion TD using RODBC
# te pide la clave con una ventana desplegable.
td_connect <- function() {
  if (!exists("usuario", envir = globalenv()) & !exists("pass", envir = globalenv())) {
    usuario <<- rstudioapi::askForPassword("Ingresa tu usuario:")
    pass <<- rstudioapi::askForPassword("PASSWORD:")
    
    setting_conexion <- sprintf("Driver=Teradata;DBCName=teraprod.bci.cl;uid=%s;pwd=%s;MechanismName =ldap",
                                usuario = usuario,
                                pass = pass)
    tera <<- odbcDriverConnect(setting_conexion, believeNRows = FALSE,
                               rows_at_time = 1, DBMSencoding = "UTF-8", colQuote = NULL)
  } else {
    setting_conexion <- sprintf("Driver=Teradata;DBCName=teraprod.bci.cl;uid=%s;pwd=%s;MechanismName =ldap",
                                usuario = usuario,
                                pass = pass)
    tera <<- odbcDriverConnect(setting_conexion, believeNRows = FALSE,
                               rows_at_time = 1, DBMSencoding = "UTF-8", colQuote = NULL)
  }
}

# Verifica si objeto Tera existe sino lo crea y lo deja en el entorno global
global_td <- function(verbose = TRUE) {
  if (exists("tera", envir = globalenv())) {
    if (!(-1L) %in% tera) {
      check <- invisible(try(RODBC::odbcQuery(tera, paste("sel * from bcimkt.ae_check_connection"))))
      if (class(check) == "try-error" | check == -1) {
        message("Objeto 'tera' ha caducado o tiene errores... eliminando y rehaciendo")
        rm(tera, envir = globalenv()) # elimina el objeto que esta caducado
        td_connect()
      } else {
        if (verbose) bla("Connection OK\n")
      }
    } else {
      message("Credenciales Erroneas, solicitando nuevamente usuario y contraseña...\n")
      rm(list = c("tera", "usuario", "pass"), envir = globalenv()) # elimina el objeto que esta caducado
      td_connect()
    }
  } else {
    td_connect()
  }
}

td_close <- function(conn) {
  close(conn)
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[grepl("tera", objs)], pos = ".GlobalEnv") # Borrar todo menos tera
}


# Asesina bases, elimina todas el nombre de tablas que se les pasen
db_killer <- function(databases, tera) {
  check_tempusu <- grepl("edw_tempusu", databases)
  
  if (sum(check_tempusu) == length(databases)) {
    bla("Eliminando las siguientes tablas:")
    
    sapply(databases, function(t) {
      cat("\n", t)
      kill_check <- sqlQuery(tera, paste("drop table", t))
      
      if (any(grepl("error", tolower(kill_check)))) {
        cat("... Not Found!")
      } else {
        cat("... killed!")
      }
    })
    bla("")
    bla("OK!")
  } else {
    not_tempusu <- paste0(databases[!check_tempusu], "\n")
    
    stop("Se esta solicitando la eliminacion de bases no EDW_TEMPUSU: ", not_tempusu)
  }
}

# Leer una base del delta lake a tibble
delta_to_tibble <- function(path) {
  df <- SparkR::read.df(path, source="delta")
  df_local <- SparkR::collect(df)
  as_tibble(df_local)
}

# Ejecucion queries -------------------------------------------------------
# Arma el titulo del proceso en el log
print_log <- function(nombre_proceso) {
  relleno <- paste0(rep("#", nchar(nombre_proceso)), collapse = "")
  cat(paste0("#############################################################", relleno), sep = "\n")
  cat(paste("#######                 Proceso",  nombre_proceso, "                     #######"), sep = "\n", append = TRUE)
  cat(paste0("#############################################################", relleno), sep = "\n", append = TRUE)
}

# Esta funcion es crucial en la automatizacion de los procesos mensuales, porque actualiza los parametros
# Genera una tabla bcimkt, con los parametros de añomes y fecha actual
# todos los modelos descargan esta tabla para setear el anomes de referencia
parametros_procesos <- function(what_time, tera, prueba = FALSE) {
  if (prueba == TRUE) {
    tbl <- "bcimkt.ae_Parametros_Procesos_Prueba"
  } else {
    tbl <- "bcimkt.ae_Parametros_Procesos"
  }
  
  is_table <- odbcQuery(tera, paste("sel * from", tbl))
  
  if (is_table == 1) {
    is_today <- what_time == sqlQuery(tera, paste("sel fecha_ejec from ", tbl))[[1]]
    
    is_today <- ifelse(length(is_today) == 0, FALSE, is_today)
  } else {
    is_today <- FALSE
  }
  
  if (!is_today) {
    drop <- sqlDrop(tera, sqtable = tbl, errors = FALSE)
    query <- paste("Q1-Creando Tabla Parametros mes# create table", tbl,
                   "(ANOMES_REF Int ,FECHA_REF_MESES Int ,
                   ANO_REF Int ,MES_REF Int ,
                   FECHA_REF_DIA Date ,
                   MESES_ANT_PROP Int,
                   FECHA_EJEC Date ) primary index (ANOMES_REF);
                   
                   # Q2-Insertando parametros#
                   insert into", tbl, "values ( 201xxx ,
                   cast(substr(cast( 201xxx as varchar(6)),1,4) as integer)*12+cast(substr(cast( 201xxx as varchar(6)),5,2) as integer) ,
                   cast(substr(cast( 201xxx as varchar(6)),1,4) as integer) ,
                   cast(substr(cast( 201xxx as varchar(6)),5,2) as integer) ,
                   cast(substr(cast( 201xxx as varchar(6)),1,4)||'-'||substr(cast( 201xxx as varchar(6)),5,2)||'-01' as date) ,13 ,
                   'what_time');
                   
                   # Q3-Indexes#
                   create index id_fec_ref_mes (fecha_ref_meses) on ", tbl,
                   ";create index id_ano (ano_ref) on ", tbl,
                   ";create index id_mes (mes_ref) on ", tbl,
                   ";create index id_dia (fecha_ref_dia) on ", tbl,
                   ";create index id_meses_ant (MESES_ANT_PROP) on ", tbl)
    
    what_time <- as.Date(what_time)
    anomes <- format(as.Date(what_time), format = "%Y%m")
    anomes_ref <- mes_to_anomes(anomes_to_mes(anomes) - 1)
    query <- gsub("201xxx", anomes_ref, query) # reemplazando año mes
    query <-  gsub("what_time", what_time, query) # reemplazando fecha de ingreso
    qt <- query_formater(query, title = "#")
    res <- query_loader(qt, tera)
    
    if (sum(res$status != "OK") > 0) {
      message("\nERROR. En la creacion de parametros")
    } else {
      message("\nProceso completado: Parametros ")
    }
  } else {
    message("\nParametros actualizado a la fecha")
  }
}

# Rompe un string de query en llamadas independientes separadas por ';' (se puede especificar)
query_breaker <- function(q, sep = ";") {
  q <- unlist(strsplit(q, split = sep))
  q <- trimws(tm::stripWhitespace(q))
  q
}

# Ejecutor de query
query_do <- function(connect_obj, query, as_is = FALSE) {
  query <- tm::stripWhitespace(query)
  dbCall <- sqlQuery(connect_obj, query,
                     believeNRows = FALSE,
                     stringsAsFactors = FALSE,
                     rows_at_time = 1,
                     as.is = as_is)
  dbCall <- tibble::as_tibble(dbCall)
  names(dbCall) <- tolower(names(dbCall))
  return(dbCall)
}

# Esta funcion permite realizar llamado y descargar tablas creadas en Teradata o Winsdat2.
# Maneja adecuadamente las credenciales, Por base busca si existe el objeto 'tera' o 'wins' para la conexion,
# si no lo encuentra lo crea con las credenciales 'usuario' y 'pass',
# si no los encuentra los pide con ua ventana desplegable.
# database: character, recibe la base de dato que se conectara, la otra es 'winsdat'
db_pull <- function(connect_obj, query, as_is = FALSE) {
  if(.Platform$OS.type == "windows") {
    options(dec = ",")
  }
  
  check_select <- grepl("^sel", tolower(query))
  
  if (check_select == TRUE) {
    query_do(connect_obj, query, as_is)
  } else {
    query <- paste("select * from", query)
    query_do(connect_obj, query, as_is)
  }
}

# Formatea la query que se lea de texto, maneja los comentarios y corta si tienes titulos, si no tiene, los crea
query_formater <- function(query, title = "#", sep = ";") {
  query <- gsub("--.*", "", query) # elimina todos los comentarios
  query <- paste(query, collapse = " ") # colapsa la query y la consolida en una
  qb <- query_breaker(query, sep = title) # si se tiene titulo con un separador especial se corta aca
  qb <- trimws(qb[!nchar(qb) == 0]) # se eliminan las queries vac?as por el corte
  
  check_title <- any(grepl("^q", tolower(qb)))
  if (check_title) {
    qt <- tibble::tibble(id = stringr::str_to_title(qb[grepl("^q", tolower(qb))]),
                         queries = qb[!grepl("^q", tolower(qb))])
  } else {
    qb <- query_breaker(qb, sep = sep)
    qt <- tibble::tibble(id =  stringr::str_to_title(paste("query", 1:length(qb))), queries = qb)
  }
  return(qt)
}

# Recibe una tabla de queries a ejecutar en teradata sin traer tablas a R
# query_table$id: id de la query que se esta subiendo
# query_table$queries: la query correspondiente
query_loader <- function(query_table, tera, title = "#", query_separator = ";") {
  
  check_qt <- is.data.frame(query_table) | data.table::is.data.table(query_table)
  
  if (!check_qt) {
    query_table <- query_formater(query_table, title = title, sep = query_separator)
  }
  
  tbl <- tibble::tibble(id =  character(0), queries = character(0),
                        status = character(0), timeSeg = numeric(0))
  
  bla("")
  for (t in 1:nrow(query_table)) {
    t1 <- Sys.time()
    qb <- query_breaker(query_table$queries[t], sep = query_separator)
    
    cat("\nEjecutando", query_table$id[t], "...")
    ql <- lapply(qb, function(t) {
      check_drop <- grepl("^drop table", tolower(t)) > 0 & !(grepl("create table", tolower(t)) > 0)
      if (check_drop) {
        ifelse(any(grepl("error", tolower(sqlQuery(tera, t)))), 0, "drop")
      } else {
        sqlQuery(tera, t, believeNRows = FALSE, rows_at_time = 1, stringsAsFactors = FALSE)
      }
    })
    
    droped <- sum(grepl("drop", tolower(ql)))
    t2 <- Sys.time()
    
    if(any(grepl("error", tolower(ql)))) {
      cat("Failed!")
      
      if (droped > 0) {
        message(paste("\nDropped", droped, "tables! from:", query_table$id[t]))
      }
      
      tbl <- rbind(tbl,
                   tibble::tibble(id =  query_table$id[t],
                                  queries = query_table$queries[t],
                                  status = "FAILED", timeSeg = as.numeric(difftime(t2, t1, units = "secs"))))
      return(tbl)
    } else {
      cat("Listo!")
      
      if (droped > 0) {
        message(paste("\nDropped", droped, "tables! from:", query_table$id[t]))
      }
      
      tbl <- rbind(tbl,
                   tibble::tibble(id =  query_table$id[t],
                                  queries = query_table$queries[t],
                                  status = "OK", timeSeg = as.numeric(difftime(t2, t1, units = "secs"))))
    }
  }
  return(tbl)
}

# Capturador de bases, sirve para rescatar todas las bases de una query
bases <- function(query, which_base = "edw_tempusu") {
  which_base <- paste0("^", toupper(which_base))
  query_paste <- paste0(query, collapse = " ")
  query_paste <- gsub(")", " )", query_paste)
  query_paste <- gsub("\"", " \"", query_paste)
  words <- unlist(stringr::str_split(query_paste, " "))
  db <- words[grepl(which_base, toupper(words))]
  db <- cleanText(db, preservePunct = c("_", "\\."), removeNum = FALSE, lowercase = FALSE)
  db <- db[!duplicated(db)]
  db
}

query_ready <- function(file, anomes, UF, dolar) {
  query <- readLines(file)
  if(!missing(anomes)) query <- gsub("201xxx", anomes, query) # reemplazando año mes si esta
  qt <- query_formater(query, title = "#")
  db <- bases(qt$queries)
  return(list(query_table = qt, databases = db))
}

# Funcion que facilita la lectura de las queries impresas en consolas
query_print <- function(x) {
  primer <- stringr::word(toupper(x), 1)
  x <- gsub("SELECT", "\nSELECT", x)
  x <- gsub("WHEN", "\n\tWHEN", x)
  x <- gsub("FROM", "\nFROM", x)
  x <- gsub("LEFT", "\nLEFT", x)
  x <- gsub("INNER", "\nINNER", x)
  x <- gsub("UNION", "\nUNION\n", x)
  x <- gsub("WHERE", "\nWHERE", x)
  x <- gsub("GROUP", "\nGROUP", x)
  x <- gsub(" AND ", "\n\tAND ", x)
  x <- gsub(" OR ", "\n\tOR ", x)
  x <- gsub(",", "\n\t,", x)
  x <- gsub("CASE \n\tWHEN", "CASE WHEN", x)
  
  
  if (primer == 'CREATE') {
    x <- sub("\\(", "(\n\t", x)
    x <- gsub("PRIMARY", "\nPRIMARY", x)
    bla("")
    cat(x)
  } else {
    bla("")
    cat(x)
  }
}

# Funcion que facilita el procesamiento de la query de su lectura antes de ejecutarla
query_process <- function(file_query, file = TRUE) {
  if (file) {
    query <- readLines(file_query)
  } else {
    query <- file_query
  }
  
  query <- toupper(query)
  query <- gsub("--.*", "", query) # elimina todos los comentarios
  query <- gsub("\\.IF .*", "", query) # elimina todos los IF ERROR
  query <- gsub("\\.SET.*", "", query) # elimina todos .SET
  query <- gsub("\\.QUIT.*", "", query) # elimina todos .QUIT
  query <- gsub("QUIT.*", "", query) # elimina todos QUIT
  query <- gsub("/\\*.*\\*/", "", query) # elimina todos brackets comentados
  query <- paste(query, collapse = " ") # colapsa la query y la consolida en una
  query <- gsub("/\\*.*?\\*/", "", query) # elimina todos brackets comentados
  query <- gsub("\\s+"," ",query)
  qp <- query_breaker(query, sep = ";") # si se tiene titulo con un separador especial se corta aca
  qp <- trimws(qp[!nchar(qp) == 0]) # se eliminan las queries vacias por el corte
  qp <- qp[!grepl("^SEL", qp)] # se sacan todas las queries que sean un simple SELECT
  return(qp)
}

# Función ejecutor de query universal desde un vector preprocesado
query_trigger <- function(query_vector) {
  map_chr(query_vector, function(x){
    primer <- stringr::word(x, 1)
    bla("")
    cat('EJECUTANDO:', stringr::word(x, 1, 3), "...")
    qexec <- odbcQuery(tera, x)
    
    if (primer == 'DROP') {
      if (qexec == -1) {
        cat("NO EXISTE PERO OK!")
        return(x)
      } else {
        cat("OK!")
        return(x)
      }
    } else if (qexec == -1) {
      err <- odbcGetErrMsg(tera)
      if (primer == 'DELETE' & length(err) == 1) {
        message("NO HAY DATOS PARA EL PERIODO OK!")
        return(x)
      } else if (primer == 'INSERT' & length(err) == 1) {
        message("\n\nNO SE INSERTARON DATOS... OJO!")
        return(x)
      } else {
        cat("FAIL!")
        bla("\nMENSAJE DE ERROR:\n")
        print(err)
        bla("\nQUERY DE ERROR:")
        query_print(x)
        bla("")
        stop("Fallo la ejecucion de una query")
      }
    } else {
      cat("OK!")
      return(x)
    }
  })
}


# Funciones accesorias ----------------------------------------------------
restart <- function() {
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[!grepl("usuario|pass|pass|seteo|restart", objs)], pos = ".GlobalEnv")
}

# A partir de una variable del estilo "Anomes" (Año*100+Mes) crea una variable del estilo "Mes" (Año*12+Mes)
anomes_to_mes <-function(variable_anomes){
  mes_ref<-as.numeric(substr(as.character(variable_anomes),1,4))*12+as.numeric(substr(as.character(variable_anomes),5,6))
  return(mes_ref)
}

# Lo contario a lo anterior
mes_to_anomes <-function(variable_mes){
  anomes_ref<-ifelse(variable_mes%%12 == 0, (variable_mes%/%12 - 1)*100 + 12, (variable_mes%/%12)*100+(variable_mes%%12))
  return(anomes_ref)
}

# Funcion utilitaria de cleanText() que permite preservar selectivamente signos de puntuaci?n.
removeMostPunctuation <<- function (text, preserveWhich = NULL) { # The functionality is base in tagging, removing and detagging.
  if (!is.null(preserveWhich)) {
    for (i in 1:length(preserveWhich)) {
      replacement <- paste("000", i, sep = "")
      text <- gsub(preserveWhich[i], replacement, text)
    }
    text <- tm::removePunctuation(text)
    for (i in 1:length(preserveWhich)) {
      replacement <- paste("000", i, sep = "")
      text <- gsub(replacement, preserveWhich[i], text)
    }
  } else {
    text <- tm::removePunctuation(text)
  }
  return(text)
}

# Esta funcion recibe un texto (string) de una columna y dependiendo de los parametros que se les pasa hace:
# - Elimina acentuaci?n o caracteres extraños (encode = T)
# - Convierte a minuscula todo el texto (lowercase = T)
# - Elimina todo tipo de signos de puntuacion a menos que se le especifique cual preservar (preservePunct = NA, si quieres mantener todos = 'all')
# - Remueve todo character numerico (removeNumbers = T)
# - Es posible palabras espec?ficas si se les pasa como un vector (ejemplo: remover los stopwords)
# - Para facilitar la transformaci?n de nombres de columnas a un formato definido se puede poner este parametro en TRUE (ejemplo: nombre de columna -> nombreDeColumna)

cleanText <- function(whateverText, columnNames = F, removeNum = T, encode = T, lowercase = T, preservePunct = NULL, stemming = F, removeExtraWords = NULL, language = "english") {
  # iconv() inates accentuation and strange characters. Accentuation are turned to apostrophes.
  if (encode == T) {
    # whateverText <- iconv(whateverText, to = "UTF-8")
    whateverText <- iconv(whateverText, "UTF-8", "ASCII//TRANSLIT", sub="")
  }
  if (!is.null(preservePunct)) {
    if (!"all" %in% preservePunct) { #  Calling 'removeMostPunctuation' function if not "all"
      whateverText <- removeMostPunctuation(whateverText, preserveWhich = preservePunct)
    }
  } else {
    whateverText <- tm::removePunctuation(whateverText)
  }
  if (lowercase == T) {
    whateverText <- tolower(whateverText) # lower case function
  }
  if (removeNum == T) {
    whateverText <- tm::removeNumbers(whateverText) # remove numbers
  }
  if (!is.null(removeExtraWords)) {
    whateverText <-
      tm::removeWords(whateverText, removeExtraWords)
  }
  if (stemming == T) {
    whateverText <- tm::stemDocument(whateverText, language = language)
  }
  whateverText <- stripWhitespace(whateverText) # Trim extra whitespace between.
  whateverText <- trimws(whateverText) # Trim extra whitespace at the beginning and the end.
  if (columnNames == T) {
    whateverText <- sapply(whateverText, function(t) {
      str = unlist(strsplit(t, split = " "))
      if (length(str) >= 3) {
        
        isSmall = !(nchar(str) <= 3)
        isNumber = grepl("[[:digit:]]",str)
        smallorNumber = (isSmall | isNumber)
        str = str[smallorNumber]
      }
      str[1] = tolower(str[1])
      str = paste0(str, collapse = "_")
    })
  }
  return(whateverText)
}

# formatea y limpia los nombres de columna de una tabla
col_clean <- function(df) { # funcion rápida para limpiar nombre columnas
  names(df) <- cleanText(names(df), columnNames = TRUE, removeNum = FALSE)
  return(df)
}

# Determina el numero y el % de datos duplicados por columnas en un data frame. Retorna la tabla.
col_duplicated <<- function(data) {
  data <- data.frame(data)
  namesColumn <- names(data)
  listDuplicated <- sapply(namesColumn, function(t) sum(duplicated(data[t])))
  tableDuplicated <- data.frame(columns = names(listDuplicated), duplicates = listDuplicated,
                                percent = round(listDuplicated/nrow(data),4), row.names = NULL, stringsAsFactors = F)
  return(tableDuplicated)
}

# para cambiar todas las columnas a 0 si hay NA
coalesce_0 <- function(x) {
  x <- as.numeric(x)
  dplyr::coalesce(x, 0)
}

# Funcion rapida de pasar de fecha a anomes
date_to_anomes <- function(date) {
  date <- format(date, format = "%Y%m")
  date
}

# Transformador logaritmico, se le suma uno por los casos de los ceros
logaritmizador <- function(x) {
  log10(x + 1)
}

# Transformador exponencial (inverso logaritmizador)
exponenciador <- function(x) {
  10^(x) - 1
}

# Entrega una tabla que cuantifica cuantos NA hay por columna y el % con respecto al total
# Autor: Fabián
table_na <- function(data) {
  data <- data.frame(data)
  names_column <- names(data)
  list_na <- sapply(names_column, function(t) sum(is.na(data[t])))
  missing_data <- data.frame(column = names(data), numberNA = list_na, row.names = NULL, stringsAsFactors = F)
  missing_data$percent <- round(missing_data$numberNA/nrow(data)*100, 4)
  missing_dataOrder <- missing_data[order(missing_data$numberNA, decreasing = T),]
  missing_dataOrder <- tibble::as_tibble(missing_dataOrder)
  return(missing_dataOrder)
}


removeColumns_na <- function(data, upperBound = 0) {
  col_ini <- ncol(data)
  missing_data <- table_na(data)
  columns_for_elimination <- missing_data$column[which(missing_data$percent >= upperBound)] # column names
  columns_for_elimination <- which(names(data) %in% columns_for_elimination) # column numbers in the dataset
  data[, c(columns_for_elimination)] <- list(NULL)
  col_fin <- ncol(data)
  bla("\nSe eliminaron un total de", col_ini - col_fin, "columnas")
  return(data)
}

# Función db_push ---------------------------------------------------------
# Revisa si el número es entero independiente de lo que diga la columna de un df por ejemplo
is.entero <- function(x) {
  if (is.numeric(x)) {
    x <- na.omit(x)
    ifelse(sum(round(x) != x) > 0, FALSE, TRUE)
  } else {
    FALSE
  }
}

# Este es el debe push tradicional y sirve para cuando es menos de 1000 datos
db_push_basico <- function(db, nombre_tabla, verbose = FALSE) {
  invisible(global_td())
  # Estimando el tiempo de carga
  if (nrow(db) > 200) {
    n_filas <- 200
    bla("Estimando tiempo de carga...")
    sqlDrop(tera, "edw_tempusu.ae_prueba_de_carga", errors = FALSE)
    
    t1 <- Sys.time()
    sqlSave(tera, db[1:200,], "edw_tempusu.ae_prueba_de_carga", rownames = FALSE, fast = FALSE)
    t2 <- Sys.time()
    
    time_diff <- as.numeric(difftime(t2, t1, units = "secs")) # tiempo en segundo que tardo
    tasa_de_carga_filas <- time_diff/n_filas
    tiempo_carga_min <- (tasa_de_carga_filas * nrow(db) / 60) %>% round
    bla("Listo! la carga tardara aproximadamente", tiempo_carga_min, "minutos")
  }
  
  bla("Comenzando carga...")
  sqlSave(tera, db, nombre_tabla, rownames = FALSE, fast = FALSE, verbose = verbose)
  bla("Completado!\n")
}

# Este es el nuevo db_push que lo hace a través de fastload
db_push_fastload <- function(df, nombre_tabla, usuario, pass, primary_index = NULL, default_size = 100) {
  bla("Tamano de data calfica para POWER FASTLOAD! Encendiendo motores...")
  if(.Platform$OS.type == "windows") {
    tmp_dir <- tempdir() # Creamos un directorio temporal
    tmp_dir <- paste0(tmp_dir, "\\")
    dec <- "."
  } else {
    tmp_dir <- tempdir() # Creamos un directorio temporal
    tmp_dir <- paste0(tmp_dir, "/")
    #dec <- options()$dec
  }
  
  dest <- paste0(tmp_dir, "tabla_subir.txt") # donde guardaremos la data a subir
  df <- as_tibble(df)
  # setDT(df) # pasando formato data.table para mayor eficiencia datos masivos
  
  bla("Formateando las columnas para subir con FASTLOAD...")
  date_cols <- map_lgl(df, lubridate::is.Date) # verificando si hay fechas
  num_cols <- map_lgl(df, function(k) class(k) == "numeric") # verificando si hay numericos
  lgl_cols <- map_lgl(df, is.logical) # verificando si hay logicos
  fac_cols <- map_lgl(df, is.factor) # verificando si hay factores
  
  if (sum(date_cols) > 0) {
    # Convertimos todas las columnas de fechas en caracter
    df <- mutate_if(df, lubridate::is.Date, as.character)
  }
  
  if (sum(num_cols) > 0) {
    # Redondeamos los decimales hasta 8 cifras significativas
    df <- mutate_if(df, num_cols, function(k) round(k, 5))
  }
  
  if (sum(lgl_cols) > 0) {
    # Convertimos todas las columnas de fechas en caracter
    df <- mutate_if(df, is.logical, as.character)
  }
  
  if (sum(fac_cols) > 0) {
    # Lanzamos un STOP para este tipo de variable
    # Que la arregle el usuario
    stop("\nTu tabla contiene variables del tipo FACTOR, por favor corregir a NUMERICO o CHARACTER,
             las variables son: ", paste0(names(df)[map_lgl(df, is.factor)], collapse = ", "))
  }
  
  bla("Almacenando temporalmente la tabla para su carga...")
  
  if(.Platform$OS.type == "windows") {
    fwrite(df, dest, sep = "|", quote = FALSE, na = "", row.names = FALSE,
           col.names = FALSE, showProgress = FALSE, dec = dec)
    
  } else {
    fwrite(df, dest, sep = "|", quote = FALSE, na = "", row.names = FALSE,
           col.names = FALSE, showProgress = FALSE)
  }
  
  # Formateando columnas con descripcion de tamaño
  if(nrow(df) > 100000) {
    bla("Tabla de carga es enorme, aplicando criterios de optimizacion de carga....")
    size_sample <- max(nrow(df)*0.1, 100000)
    df <- sample_n(df, size_sample)
    size <- default_size
  } else {
    
    size <- sapply(df, function(t) max(nchar(t, keepNA = FALSE), na.rm = TRUE))
  }
  
  col_desc <- data.table(col = names(df), tipo = sapply(df, class), size = size) %>%
    mutate(size = ifelse(is.infinite(size), 1, size),
           tipo = ifelse(tipo == "numeric", "float",
                         ifelse(tipo == "integer", tipo, paste0("varchar(", size, ")"))),
           type = toupper(tipo))
  
  # Creando el cuerpo del create
  col_unite_create <- col_desc %>%
    unite(col_type, col, type, sep = " ")
  
  # Crear el cuerpo del define
  col_unite_define <- col_desc %>%
    mutate(type = paste0("(VARCHAR(", size, "))")) %>%
    unite(col_type, col, type, sep = " ")
  
  # Creando el fastload script
  bla("Creando el fastload script...")
  if(is.null(primary_index)) {
    primary_index <- names(df)[1]
    bla("No se definio un PRIMARY INDEX, se ocupara la primera columna:", toupper(primary_index),
        "como PRIMARY INDEX...")
  }
  
  logmech <- ".LOGMECH ldap;" # Se agrega que el mecanismo de login es LDAP
  logon <- paste0(".LOGON teraprod.bci.cl/", usuario, ",", pass, ";") # Importate que haya usuario y pass
  drop_e1 <- "DROP TABLE edw_tempusu.ae_tabla_errA;"
  drop_e2 <- "DROP TABLE edw_tempusu.ae_tabla_errB;"
  drop_tbl <- paste("DROP TABLE", nombre_tabla,";")
  body_create <- paste0(col_unite_create$col_type, collapse = ", ")
  create_table <- paste("CREATE TABLE", nombre_tabla, "(", body_create, ") PRIMARY INDEX(", primary_index, ");")
  begin <- paste("BEGIN LOADING", nombre_tabla)
  error_file <- paste("ERRORFILES edw_tempusu.ae_tabla_errA, edw_tempusu.ae_tabla_errB;")
  set_record <- 'SET RECORD VARTEXT  "|";'
  body_define <- paste0(col_unite_define$col_type, collapse = ", ")
  define <- paste("DEFINE", body_define)
  file_dest <- paste("FILE =", dest, ";")
  ins_format <- paste0(":", col_desc$col, collapse = ",")
  insert <- paste("INSERT INTO", nombre_tabla, "values(", ins_format, ");")
  end <- "END LOADING;"
  logoff <- "LOGOFF;"
  quit <- ".QUIT"
  
  # Juntando todo el script
  doc <- c(logmech, logon, drop_e1, drop_e2, drop_tbl, create_table, begin, error_file,
           set_record, define, file_dest, insert, end, logoff, quit)
  
  # Guardando el script en el temporal
  script_dest <- "script_subida.txt"
  write_lines(doc, paste0(tmp_dir, script_dest))
  
  # Activando fastload
  bla("Comenzando FASTLOAD...\n")
  
  if (.Platform$OS.type == "windows") {
    shell(paste0("fastload < ", paste0(tmp_dir, script_dest)))
  } else {
    system(paste0("fastload < ", paste0(tmp_dir, script_dest)))
  }
  
  unlink(tmp_dir) # eliminando el directorio temporal
  bla("\nCompletada la carga masiva!\n")
}

# Esta es la función de entrada, decide según el tamaño de la data si lo hace
# por db_push tradicional (n_datos < 1000) o db_push fastload
db_push <- function(df, nombre_tabla, primary_index = NULL, default_size = 100) {
  # Revisa si está el usuario y contraseña en el global
  if (!exists("usuario", envir = globalenv()) | !exists("pass", envir = globalenv())) {
    usuario <<- rstudioapi::askForPassword("Ingresa tu usuario:")
    pass <<- rstudioapi::askForPassword("PASSWORD:")
  }
  
  # Si es menos de 1000 datos se ocupa el método tradicional de carga
  if (nrow(df) <= 1000) {
    bla("Usando metodo tradicional de carga de dato...")
    db_push_basico(df, nombre_tabla)
    
  } else { # Si va por fastload
    db_push_fastload(df, nombre_tabla, primary_index = primary_index, default_size = default_size,
                     usuario = usuario, pass = pass)
  }
}

# Funciones de Transformación ---------------------------------------------
minMax_scaler <- function(x) {
  # Deja todo entre 0 y 1, cuidado con los outliers!
  (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

robust_scaler <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

# Funciones de evaluación de modelos --------------------------------------
lift_tbl <- function(predicciones, target, por = 0.1) {
  pred_obj <- data.frame(pred = predicciones, target = target)
  rankings <- pred_obj %>% mutate(rank_probs = trunc(rank(pred))) %>%
    arrange(desc(pred)) %>%
    mutate(r = 1:nrow(pred_obj))
  
  deciles <- quantile(rankings$r, probs = seq(0, 1, por))
  
  rankings$deciles <- cut(rankings$r, breaks = deciles, labels = names(deciles)[-1], include.lowest = TRUE)
  wrn <- sum(pred_obj$target)/nrow(pred_obj)*100
  
  rankings %>%
    group_by(deciles) %>%
    summarise(n = n(),
              n_win = sum(target),
              wr = sum(target)/n()*100) %>%
    mutate(wr_natural = wrn,
           lift = wr/wrn)
}


lift_deciles <- function(predicciones, target, por = 0.1) {
  pred_obj <- data.frame(pred = predicciones, target = target)
  rankings <- pred_obj %>% mutate(rank_probs = trunc(rank(pred))) %>%
    arrange(desc(pred)) %>%
    mutate(r = 1:nrow(pred_obj))
  
  deciles <- quantile(rankings$r, probs = seq(0, 1, por))
  
  rankings$deciles <- cut(rankings$r, breaks = deciles, labels = names(deciles)[-1], include.lowest = TRUE)
  rankings %>% group_by(deciles) %>%
    summarise(cortes_prob = min(as.numeric(pred)))
  
}

lift_metricas <- function(predicciones, target, por = 0.1) {
  rankings <- lift_tbl(predicciones, target, por)
  deciles <- lift_deciles(predicciones, target, por)
  rankings %>% 
    left_join(deciles, by = "deciles")
}

lift_curve <- function(predicciones, target, por = 0.1) {
  tablon_lift <- lift_metricas(predicciones, target, por)
  wrn <- unique(tablon_lift$wr_natural)
  
  tablon_lift %>% 
    ggplot(aes(x = deciles, wr, group = 1, label = round(lift, 2))) +
    geom_point() + geom_line() +
    xlab("Deciles") + ylab("Winrate") +
    geom_hline(aes(yintercept = wrn), linetype = "dotted") +
    ggrepel::geom_text_repel() + ggtitle("Lift Curve") +
    ggplot2::annotate("text", x = 8, y = wrn*1.1, label = paste("WinRate natural =", round(wrn, 2), "%"))
}

get_prob_reales <- function(preds_mod, tasa_sobremuestra, tasa_natural) {
  # Ecuacion: Prob_Real = (Prob_Mod * r0 * p1)/((Prob_Mod * r0 * p1) + ((1-Prob_Mod)*r1*p0))
  # Sea:
  # r0: Tasa ceros sobremuestreo
  # r1: Tasa unos sobremuestreo
  # p0: Tasa ceros muestra original
  # p1: Tasa unos muestra original
  r0 <-  1 - tasa_sobremuestra
  r1 <- tasa_sobremuestra
  p0 <- 1 - tasa_natural
  p1 <- tasa_natural
  (preds_mod * r0 * p1)/((preds_mod * r0 * p1) + ((1-preds_mod)*r1*p0))
}

lift_sencillo <- function(data, prob, target, por = 0.1) {
  lift_curve(data[[prob]], data[[target]], por = por) +
    ggthemes::theme_few()
}

lift_tbl_sencillo <- function(data, prob, target, por = 0.1) {
  lift_tbl(predicciones = data[[prob]], target = data[[target]], por = por) 
}

lift_del_sencillo <- function(data, prob, target, por = 0.1) {
  lift_deciles(predicciones = data[[prob]], target = data[[target]], por = por) 
}

lift_graph <- function(tablon_lift) {
  tablon_lift %>% 
    ggplot(aes(x = deciles, wr, group = 1, label = round(lift, 2))) +
    geom_point() + geom_line() +
    xlab("Deciles") + ylab("Winrate") +
    geom_hline(aes(yintercept = wr_natural), linetype = "dotted") +
    ggrepel::geom_text_repel() + ggtitle("Lift Curve") +
    facet_wrap(~segmento, scales = "free") +
    geom_text(aes(x = 8, y = wr_natural*1.1, label = paste("WinRate natural =", round(wr_natural, 2), "%"))) +
    theme_bw()
}

eval_mod <- function(df) {
  tibble(anomes_ref = unique(df$anomes_ref),
         auc = AUC(y_pred = df$propension, y_true = df$target),
         gini = Gini(y_pred = df$propension, y_true = df$target)) %>% 
    mutate_at(vars(auc, gini), function(x) round(x, 2))
}

eval_mod_group <- function(df, groups) {
  out <- df %>% 
    group_by_at(groups) %>% 
    summarise(n = n(),
              auc = AUC(y_pred = propension, y_true = target),
              gini = Gini(y_pred = propension, y_true = target)) %>% 
    ungroup()
  
  if (length(groups) > 1) {
    out %>% 
      select(-n, -gini) %>% 
      spread(groups[2], auc) %>% 
      print(n = 100)
  }
  return(out)
}

lift_segmento <- function(df, segmento) {
  segmento <- enquo(segmento)
  df %>% 
    group_split(!!segmento) %>% 
    map(lift_tbl_sencillo, prob = "propension", target = "target") %>% 
    map2_df(df %>%
              group_keys(!!segmento) %>% 
              pull(!!segmento), function(x, y) {
                x$segmento <- y
                x
              }) %>% 
    lift_graph()
}

rmsle <- function(data, truth, estimate) {
  truth <- data$truth
  estimate <- data$estimate
  
  error <- log1p(estimate) - log1p(truth)
  sqrt(mean(error^2, na.rm = TRUE))
}

# Define the RMSLE metric
# Does not accept negative values!!
rmsle_impl <- function(truth, estimate, case_weights = NULL) {
  error <- log1p(estimate) - log1p(truth)
  sqrt(mean(error^2))
}

rmsle_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  rmsle_impl(truth, estimate, case_weights = case_weights)
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}

rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")
rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, 
                             case_weights = NULL, ...) {
  numeric_metric_summarizer(
    name = "rmsle",
    fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}




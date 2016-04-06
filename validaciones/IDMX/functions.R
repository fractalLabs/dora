###################################################
## This script provides several tools which enable
## the automatization of the processes of data
## integrity verification.
###################################################


###################################################
## Libraries
###################################################
## Manipulate URLS
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(RCurl))
## Manipulate dates
suppressPackageStartupMessages(library(lubridate))
## Manipulate strings
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(stringdist))
## Manipulate data
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(DT))
## Graphics
suppressPackageStartupMessages(library(ggplot2))
## Read in data
suppressPackageStartupMessages(library(gdata))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(foreign))
## Display dashboard
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shiny))


###################################################
##---------------------------------
## Functions
##---------------------------------
###################################################


###################################################
##---------------------------------
## most_simil
##---------------------------------
###################################################
most_simil <- function(char1, char2){
    ##-------------------------------------------------------------------
    ## Identifies similiarites between characters using several metrics
    ## OSA (Restricted Damerau-Levenshtein): insertions, erasures,
    ## replacements and transpositions. Each substring can be edited at
    ## most once.
    ## LV (Levenshtein): insertions, erasures and replacements.
    ## DL (FULL Damerau-Levenshtein): insertions, erasures,
    ## replacements and transpositions (less or equal than Levenshtein).
    ## LCS: longest common substring
    ##-------------------------------------------------------------------

    max_str_length <- max(str_length(char1), str_length(char2))
    max_unique_length <- max(
        length(unique(str_split(char1, "")[[1]])),
        length(unique(str_split(char2, "")[[1]]))
    )
    ## Set all metrics between  0 and 1.
    res <-     list(
        osa       = 1 - stringdist(char1, char2, method = "osa") / max_str_length,
        lv        = 1 - stringdist(char1, char2, method = "lv")  / max_str_length,
        dl        = 1 - stringdist(char1, char2, method = "dl")  / max_str_length,
        lcs       = 1 - stringdist(char1, char2, method = "lcs") / max_str_length,
        cosine    = max(1 - stringdist(char1, char2, method = "cosine", q = 3), 0),
        jaccard   = max(1 - stringdist(char1, char2, method = "jaccard", q = 3), 0)
    )
    max(unlist(res))
}

###################################################
##---------------------------------
## most_simil_mult
##---------------------------------
###################################################
most_simil_mult <- function(char, bag){
    ##--------------------------------------------------------------
    ## Returns a list containing the following elements:
    ## 1.- Maximum similarity between char & bag.
    ## 2.- The character within bag that reached such index.
    ## 3.- The index of such character.
    ##--------------------------------------------------------------

    ## First comparison, using jaccard similarity.
    simil <- laply(
        bag,
        function(t)t <- (
            1 - stringdist(
                    char,
                    t,
                    method = "jaccard",
                    q = 1
                )
        )
    )
    indexes <- which(simil == max(simil))
    res_1   <-
    list(simil = max(simil),
         char  = unique(bag[indexes]))

    ## Second comparison, using a mix of similarities.
    simil <- laply(res_1[[2]],
                  function(t)t <- most_simil(char, t))
    index <- which(simil == max(simil))[1]
    char  <- res_1[[2]][index]
    index <- indexes[index]
    res   <-
        list(simil = max(simil),
             index = index,
             char  = char)
    ## Results.
    res
}

###################################################
###################################################
##################### DATES #######################
###################################################
###################################################

## Base with standard dates.
date_base <- seq(as.Date("1995-01-01"),
                today(),
                "days")

###################################################
##---------------------------------
## transform_month
##---------------------------------
###################################################
transform_month <- function(month){
    bag_months <- list( "01" = c("Enero", "ene."),
                       "02" = c("Febrero", "feb."),
                       "03" = c("Marzo", "mar."),
                       "04" = c("Abril", "abr."),
                       "05" = c("Mayo", "may."),
                       "06" = c("Junio", "jun."),
                       "07" = c("Julio", "jul."),
                       "08" = c("Agosto", "ag."),
                       "09" = c("Septiembre", "sep."),
                       "10" = c("Octubre", "oct."),
                       "11" = c("Noviembre", "nov."),
                      "12"  = c("Diciembre","dic.")
                      )
    ## Bag of months
    off_months <- unlist(bag_months)
    ## Get most similar date
    most_like <- most_simil_mult(month, off_months)$char
    ## Get index
    clave <- names(bag_months)[laply(
                     bag_months,
                     function(t){ t <- most_like %in% t})]
    list("Mes" = clave, "Nombre" = bag_months[clave][[1]][1])
}

###################################################
##---------------------------------
## date_pre_proc
##---------------------------------
###################################################
date_pre_proc <- function(date){
    if(str_detect(date,"([a-z]|[A-Z])")){
        month <- transform_month(date)$Mes
        date  <- str_replace(date, "([a-z]|[A-Z])", month)
        date  <- str_replace_all(date, "([a-z]|[A-Z])", "")
    }
    date <- str_split(date, "([[:punct:]]|[[:space:]])")[[1]]
    date <- date[order(date, str_length(date), decreasing = TRUE)]
    date <- date[str_length(date) > 0]
    list("date1" = paste0(date,collapse = "-"),
         "date2" = paste0(date[c(1, 3, 2)], collapse = "-"))
}

###################################################
##---------------------------------
## transform.date
##---------------------------------
###################################################
transform.date <- function(date, dates = date_base){
    date <- date_pre_proc(date)
    list("possible_date1" =
             head(most_simil_mult(date$date1, dates)$char, 1),
         "possible_date2" =
             head(most_simil_mult(date$date2, dates)$char, 1)
         )
}


####################################################
################### Test DATES #####################
####################################################
## transform.date("10 de enero 2015")
## transform.date("12/dic/2014")
## transform.date("12/15/2004")


####################################################
####################################################
################ FEDERAL ENTITIES ##################
####################################################
####################################################


entities <- list(
    "01" = "Aguascalientes",
    "02" = "Baja California",
    "03" = "Baja California Sur",
    "04" = "Campeche",
    "05" = c("Coahuila de Zaragoza","Coahuila"),
    "06" = "Colima",
    "07" = "Chiapas",
    "08" = "Chihuahua",
    "09" = "Distrito Federal",
    "10" = "Durango",
    "11" = "Guanajuato",
    "12" = "Guerrero",
    "13" = "Hidalgo",
    "14" = "Jalisco",
    "15" = "México",
    "16" = c("Michoacán de Ocampo", "Michoacán"),
    "17" = "Morelons",
    "18" = "Nayarit",
    "19" = "Nuevo León",
    "20" = "Oaxaca",
    "21" = "Puebla",
    "22" = "Querétaro",
    "23" = "Quintana Roo",
    "24" = "San Luis Potosí",
    "25" = "Sinaloa",
    "26" = "Sonora",
    "27" = "Tabasco",
    "28" = "Tamaulipas",
    "29" = "Tlaxcala",
    "30" = c("Veracruz de Ignacio de la Llave","Veracruz"),
    "31" = "Yucatán",
    "32" = "Zacatecas"
)

full_ent <- read.csv("./data/coords.csv",
    #"http://raw.githubusercontent.com/lromang/IDMX/master/data/coords.csv",
    stringsAsFactors = FALSE,
    header = TRUE,
    colClasses = rep("character", 6),
    encoding = "UTF-8"
)

###################################################
##---------------------------------
## transform.all
##---------------------------------
###################################################
transform.all <- function(entity, mun = NA,
                         loc = NA, bag_entities = entities,
                         full_bag = full_ent){
    ##--------------------------------------------------------------
    ## Recibe una entidad federativa y opcionalment un municipio y una localidad
    ## los transforma a su clave INEGI  y nombre más probable,
    ## dada esa entidad federativa.
    ## IN
    ## entity: Entidad federativa la cual se quiere transformar.
    ## mun: Municipio el cual se quiere transformar.
    ## loc: Localidad la cual se quiere transformar.
    ## OUT
    ## data.frame con la clasificación correspondiente
    ##--------------------------------------------------------------
    mun_name <- NA
    mun_key  <- NA
    loc_name <- NA
    loc_key  <- NA
    off_entities  <- tolower(unlist(bag_entities))
    ## Obtener la entidad que se parece más.
    most_like     <- most_simil_mult(tolower(entity), off_entities)$char
    ## Obtener índice entidad.
    clave         <- names(bag_entities)[
                             laply(bag_entities,
                                   function(t){t <-
                                                   most_like %in%
                                                   tolower(t)})]
    ## Obtener municipio al que se parece más, en caso de que haya.
    if(!is.na(mun)){
        filt_mun      <- dplyr::filter(full_bag, ENTIDAD == clave)
        most_like_mun <- most_simil_mult(tolower(mun),
                                        unique(filt_mun$NOM_MUN))$index
        ## Obtener clave y nombre municipio
        mun_name <- unique(filt_mun$NOM_MUN)[most_like_mun]
        mun_key  <- filt_mun$MUN[filt_mun$NOM_MUN == mun_name][1]
        ## Obtener localidad a la que se parece más, en caso de que haya.
        if(!is.na(loc)){
            filt_mun_loc  <- dplyr::filter(filt_mun, MUN == mun_key)
            most_like_loc <- most_simil_mult(tolower(loc),
                                            unique(filt_mun_loc$NOM_LOC))$index
            ## Obtener clave y nombre municipio
            loc_name <- unique(filt_mun_loc$NOM_LOC)[most_like_loc]
            loc_key  <- filt_mun_loc$LOC[filt_mun_loc$NOM_LOC == loc_name][1]
        }
    }
    ## Regresar resultados
    data.frame("entidad"         = bag_entities[clave][[1]][1],
               "clave_entidad"   = clave,
               "municipio"       = mun_name,
               "clave_municipio" = mun_key,
               "localidad"       = loc_name,
               "clave_localidad" = loc_key
               )
}


###################################################
##---------------------------------
## transform.all
##---------------------------------
###################################################
transform.all.col <- function(entity_array, mun = NA,
                         loc = NA, bag_entities = entities,
                         full_bag = full_ent){
    ##--------------------------------------------------------------
    ## Recibe una entidad federativa y opcionalment un municipio y una localidad
    ## los transforma a su clave INEGI  y nombre más probable,
    ## dada esa entidad federativa.
    ## IN
    ## entity: Entidad federativa la cual se quiere transformar.
    ## mun: Municipio el cual se quiere transformar.
    ## loc: Localidad la cual se quiere transformar.
    ## OUT
    ## data.frame con la clasificación correspondiente
    ##--------------------------------------------------------------
    ids <- c()
    for(i in 1:length(entity_array)){
        ids <- rbind(ids, transform.all(entity_array[i], mun[i], loc[i],
                                       bag_entities = entities,
                                       full_bag = full_ent))
    }
    ids
}


###################################################
##---------------------------------
## iden.entity
##---------------------------------
###################################################
ident.entity <- function(col, class = "ent", thresh = .5, pres = .05){
    if(class == "ent"){
        test_set <- unlist(entities)
    }else if(class == "mun"){
        mun      <- unique(full_ent$NOM_MUN)
        test_set <- mun[
            sample(
                length(mun),
                length(mun) * pres)
        ]
    }else if(class == "loc"){
        loc      <- unique(full_ent$NOM_LOC)
        test_set <- loc[
            sample(
                length(loc),
                length(loc) * pres)
            ]
    }else{
        test_set <- date_base[
            sample(
                length(date_base),
                length(date_base) * pres)
            ]
    }
    samp      <- sample(length(col), min(.3*length(col), 5))
    col_test  <- col[samp]
    if(class != "date"){
        sim_index <- max(laply(col_test,
                              function(t) t <- most_simil_mult(tolower(t),
                                                              tolower(test_set))$simil))
    }else{
        sim_index <- max(laply(col_test,
                              function(t) t <- most_simil_mult(date_pre_proc(t)[[1]],
                                                              test_set)$simil))
    }
    sim_index > thresh
}

###################################################
##---------------------------------
## run.a.test
##---------------------------------
###################################################
run.a.test <- function(data){
    result <- list()
    ents <- apply(data, 2, function(t) t <- ident.entity(t, "ent"))

    ## muns <- apply(data, 2, function(t) t <- ident.entity(t, "mun"))
    ## locs <- apply(data, 2, function(t) t <- ident.entity(t, "loc"))
    ## date <- apply(data, 2, function(t) t <- ident.entity(t, "date"))
    mssg <- paste(
        paste0("<h4>Se corrió un análisis de validación sobre la base y se  ",
               "detectaron posibles errores  en los tipos de datos: </h4>"),
          "<h3>Nombres de estados: </h3>",
          paste0("cols = ", paste(which(ents == TRUE), collapse = ",")),
          "<h3>Fechas: </h3>",
       ##   paste0("cols = ", paste(which(date == TRUE), collapse = ",")),
          sep = "<br/>"
    )
    cols_ents  <- which(ents == TRUE)
    ## cols_dates <- which(date == TRUE)
    result[[1]] <- mssg
    result[[2]] <- cols_ents
    ## result[[3]] <- cols_dates
    result
}




####################################################
############# Test FEDERAL ENTITIES ################
## transform.all("CAMPECHE")
## transform.all("mchcan")
## transform.all("oxc", "sn. agst. chayu")
## transform.all("oxc", "sn. agst. chayu", "col. sn. flipe")

####################################################
####################################################
################# NAMED ENTITIES ###################
####################################################
####################################################


###################################################
##---------------------------------
## iden_names
##---------------------------------
###################################################
iden_names <- function(text, only_names = FALSE){
    names <- ""
    if(only_names == TRUE){
        text  <- str_replace(text,"^.{1}",tolower(str_sub(text,1,1)))
    }
    text  <- str_replace_all(text, "[[:punct:]]+[[:space:]]*[A-Z][a-z]{1,4}","_")
    text  <- str_replace_all(text, "[[:punct:]]+.{1,5}\n+.{1,5}[A-Z]","_")
    regex <- paste0("( *[[:upper:]]]+[[:alpha:]]{3,} *[[:upper:]]+[[:alpha:]]{3,} ",
                   "*[[:upper:]]+[[:alpha:]]{3,} *| *[[:upper:]]+[[:alpha:]]{3,}",
                   " *[[:upper:]]+[[:alpha:]]{3,} *| *[[:upper:]]+[[:alpha:]]{3,} *)")
    names <- str_match_all(text, regex)
    if(length(names[[1]]) > 0){
    names <- names[[1]][str_length(names[[1]][,1])>=3,1]
    #name  <- names[which(max(str_length(names)) == str_length(names))[1]]
    names  <- unique(str_trim(names))
    }
    names
}

###################################################
##---------------------------------
## names_in_col
##---------------------------------
###################################################
names_in_col <- function(col, thres = 0){
    result <- list()
    entit  <- llply(col, function(t)t <- iden_names(t))
    result[[1]] <- laply(entit, function(t)t <- length(t) > thres)
    result[[2]] <- sum(result[[1]])
    result
}

###################################################
##---------------------------------
## ident_cols_names
##---------------------------------
###################################################
ident_cols_names <- function(data){
    apply(data, 2, function(t)t <- names_in_col(t)[[2]]) / nrow(data)
}


###################################################
##---------------------------------
## get_words_wiki
##---------------------------------
###################################################
get_words_wiki <- function(url){
    ## Obtiene todas las ligas dentro de la pagina proporcionada.
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas.
    ## OUT
    ## arreglo que contiene todas las ligas de la pagina
    result <- list()
    page   <- getURL(url)
    tree   <- htmlParse(page)
    links  <- xpathApply(tree,
                        path = "//a",
                        fun  = xmlGetAttr,
                        name = "href")
    links     <- unlist(links)
    regex     <- "Spanish_adjectives&pagefrom=.{2,}"
    next_page <- links[
        str_detect(links,regex)][1]
    links       <- links[str_detect(links, "/wiki/[a-z]+")]
    words       <- str_replace(links, "/wiki/", "")
##    result[[1]] <- paste0("https://en.wiktionary.org/",next_page)
    result[[2]] <- words
    result
}


###################################################
##---------------------------------
## get_words_number
##---------------------------------
###################################################
get_words_number <- function(url, number){
    ## Obtiene todas las ligas de una pagina que es parte de un conjunto enumerado
    ## de paginas
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas
    ## number: el número de la pagina.
    ## OUT
    ## arreglo que contiene todas las ligas de la pagina
    base.url <- paste0(url,"?page=",number)
    links    <- get_words_wiki(base.url)
    links
}

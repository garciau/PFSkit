#' ---
#' title: "simple contextual indexes from PFS
#' authors: "Jesús Ortiz Bejar, Data Scientist at Knotion"
#' date: "2019-09-26"
#' Based on the work of Alfonso Garcia
#' ---

#' ## Objectives
#' ### Construct  a function to calculate PISA contextual INDEX:
#' * MATHEFF,
#' * SCIEEFF,
#' * INSTMOT,
#' * INSTSCIE,
#' * DISCLIM,
#' * DISCLIMA,
#' * DISCLISCI,
#' * STUDREL"
#' ## Input
#' * Input 1: dataframe with context questionaire answers data

#' ## Output


library(readxl)
library(TAM)
library(tidyverse)

# function to transforming params Conquest Format into TAM compatible anchor values format
# and
get_params<-function(indice='ALL'){

  dfindices <- read_excel(path = 'data/Contextual_Indices_params.xlsx',
                          sheet = 'INDICES' )
  if (indice!='ALL'){
     indices <- c(indice)
   }else{
     indices<-dfindices$indices[dfindices$is_simple==1]
   }
  params=list()
  for (indice in indices){
   tmpdf<- read_excel(path = 'data/Contextual_Indices_params.xlsx',
                      sheet = indice )

   if("alpha" %in% colnames(tmpdf)){
     anchor_values <- tmpdf %>%
       select(-c(Item))%>%
       mutate(row = row_number()) %>%
       rowwise() %>%
       mutate_at(vars(contains('d_')),
                 funs(plus = beta - .)) %>%
       ungroup() %>%
       select(row, contains('plus')) %>%
       gather(type, anchorValues, d_1_plus:d_3_plus) %>%
       arrange(row)%>%
       pull(anchorValues)
   }else{
     if(!("tau_3" %in% colnames(tmpdf))){
       tmpdf$tau_3 <- -(tmpdf$tau_1 + tmpdf$tau_2)
     }
     anchor_values <- tmpdf %>%
     select(-c(Item))%>%
     mutate(row = row_number()) %>%
     rowwise() %>%
     mutate_at(vars(contains('tau')),
              funs(plus_delta = . + Delta)) %>%
     ungroup() %>%
     select(row, contains('plus_delta')) %>%
     gather(type, anchorValues, tau_1_plus_delta:tau_3_plus_delta) %>%
     arrange(row)%>%
     pull(anchorValues)
   }
   params[[indice]]=list("anchor_values"= anchor_values,
                         "items"= tmpdf$Item,
                         "mean"= dfindices$mean[which(dfindices$indices==indice)],
                         "sd"= dfindices$sd[which(dfindices$indices==indice)],
                         "recoding"= dfindices$recoding[which(dfindices$indices==indice)],
                         "PISA" = dfindices$PISA[which(dfindices$indices==indice)]
                         )
  }
   return(params)
}

apply_model<-function(data,indice='ALL'){
  indicesdf<-data%>%select(stidstd)
  param_index = get_params(indice)
  for(modelo in names(param_index) ){
    indice = param_index[[modelo]]
    if (indice$PISA%%100<10){
      sep="_0"
    }else{
      sep="_"
    }
    itemselect = paste(indice$items,indice$PISA%%100,sep = sep)
    if(indice$recoding=='inverse'){
      indice_q <- data %>%
        select(stidstd,itemselect) %>%
        rowwise() %>%
        mutate_at(itemselect,
                funs(recode = case_when(
                  . == 1 ~ 3,
                  . == 2 ~ 2,
                  . == 3 ~ 1,
                  . == 4 ~ 0,
                  TRUE ~ as.numeric(NA)
                ))) %>%
        ungroup() %>%
        select(stidstd, contains('recode'))
    }else if(indice$recoding=='shifting'){
      #print(data %>%select(stidstd,itemselect))
      indice_q <- data %>%
        select(stidstd,itemselect) %>%
        rowwise() %>%
        mutate_at(itemselect,
                  funs(recode = case_when(
                    . == 0 ~ as.numeric(NA),
                    . == 1 ~ 0,
                    . == 2 ~ 1,
                    . == 3 ~ 2,
                    . == 4 ~ 3,
                    TRUE ~ as.numeric(NA)
                  ))) %>%
        ungroup() %>%
        select(stidstd, contains('recode'))
    }
    #Combine into a matrix, TAM INPUT format
    TAM_anchorValues <-
      unname(cbind(1:(length(indice$anchor_values)),indice$anchor_values ))
    if(modelo=='DISCLISCI'){
      model <-
        tam.mml.2pl(indice_q %>%
                      select(contains('recode')),
                      irtmodel = "GPCM",
                      verbose = FALSE)
    }else{
    model <-
      tam(indice_q %>%
            select(contains('recode')) ,
          xsi.fixed = TAM_anchorValues,
          verbose = FALSE)
    }
    wle <- tam.wle(model)
    indicesdf<-cbind(indicesdf,
                     (wle$theta - indice$mean) / indice$sd)
  }
  #final computing

  newnames<-c('stidstd',names(param_index))
  print(newnames)
  print(colnames(indicesdf))
  colnames(indicesdf)<-newnames

  return(indicesdf)
}


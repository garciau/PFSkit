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
shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}
map_B.fixed<-function(categories,alphas){
  n<-sum(categories)
  B.fixed <- matrix(0,n,4)
  fin<-0
  inicio<-1
  print(categories)
  for(q in 1:length(categories)){
    fin<-fin+categories[q]
    B.fixed[inicio:fin,1]=q
    B.fixed[inicio:fin,2]=1:categories[q]
    B.fixed[inicio:fin,3]=1
    B.fixed[inicio:fin,4]=(0:(categories[q]-1))*alphas[q]
    inicio<-inicio+categories[q]
  }
  return(B.fixed)
}
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
     taus <- (tmpdf%>%select(contains('d_')))*(-1*tmpdf$alpha)
     #taus <- (tmpdf%>%select(contains('d_')))*(-1)
     anchor_values <- c(tmpdf$beta*tmpdf$alpha,as.vector(t(taus)))
     #anchor_values <- c(tmpdf$beta,as.vector(t(taus)))
     anchor_values_m<-na.omit(anchor_values)
     anchor_values<-anchor_values[!is.na(anchor_values)]
     categories<- tmpdf %>%
       select(contains('d_'))%>%
       apply(1,function(x) ifelse(length(which(!is.na(x)))==0,
                                  2,
                                  length(which(!is.na(x)))+2))
       B.fixed<-map_B.fixed(categories,tmpdf$alpha)
       params[[indice]]=list("anchor_values"= anchor_values,
                              "B.fixed" = B.fixed,
                             "items"= tmpdf$Item,
                             "mean"= dfindices$mean[which(dfindices$indices==indice)],
                             "sd"= dfindices$sd[which(dfindices$indices==indice)],
                             "recoding"= dfindices$recoding[which(dfindices$indices==indice)],
                             "PISA" = dfindices$PISA[which(dfindices$indices==indice)],
                             "xsi.fixed"=cbind(1:length(anchor_values),anchor_values)
       )
   }else{
     #if(!("tau_3" %in% colnames(tmpdf))){
     #   tmpdf$tau_3 <- -(tmpdf$tau_1 + tmpdf$tau_2)
     #}
     taus <- (tmpdf%>%select(contains('tau')))
     anchor_values <- c(tmpdf$Delta,as.vector(t(taus)))
     anchor_values_m<-na.omit(anchor_values)
     anchor_values<-anchor_values[!is.na(anchor_values)]
     params[[indice]]=list("anchor_values"= anchor_values,
                           "items"= tmpdf$Item,
                           "mean"= dfindices$mean[which(dfindices$indices==indice)],
                           "sd"= dfindices$sd[which(dfindices$indices==indice)],
                           "recoding"= dfindices$recoding[which(dfindices$indices==indice)],
                           "PISA" = dfindices$PISA[which(dfindices$indices==indice)]
     )
   }
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
    if(modelo=='DISCLISCICQ'){
      print(TAM_anchorValues)
      print(indice$B.fixed)
      A1 <- TAM::.A.PCM2( resp=indice_q %>%
                            select(contains('recode')))
      model <-tam.mml.2pl(indice_q %>%
                      select(contains('recode')),
                      irtmodel = "GPCM",
                      xsi.fixed = (TAM_anchorValues),
                      B.fixed = indice$B.fixed,
                      A=A1,
                      verbose = FALSE)

    }else{
    A1 <- TAM::.A.PCM2( resp=indice_q %>%
                            select(contains('recode')))
    model <-
      tam(indice_q %>%
            select(contains('recode')) ,
          irtmodel = "PCM2",
          xsi.fixed = TAM_anchorValues,
          A=A1,
          verbose = FALSE)
    }
    print(modelo)
    wle <- tam.wle(model,Msteps = 200)
    indicesdf<-cbind(indicesdf,
                     (wle$theta - indice$mean) / indice$sd)
  }
  #final computing

  newnames<-c('stidstd',names(param_index))
  colnames(indicesdf)<-newnames
  return(indicesdf)
}


data2<-read_csv('data/input_pfs_context_mex_2018.csv')
data<-read_excel('data/output_StdQ_golddataset.xlsx')
dataproc<-apply_model(data2)
compar<-inner_join(dataproc,
                   data %>% select(stidstd,
                                   DISCLIMA,
                                   DISCLIM,
                                   DISCLIMS,
                                   INSTMOT,
                                   MATHEFF,
                                   STUDREL,
                                   SCIEEFF,
                                   INSTSCIE) ,
                   by=c("stidstd"="stidstd"))%>%
  mutate(err=abs(DISCLIMS-DISCLISCICQ))
compar<-compar[,order(colnames(compar))]

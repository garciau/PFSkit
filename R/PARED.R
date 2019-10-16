coder <- function(x) {
  if (length(which(x == 1)) == 0) {
    return(NA)
  } else {
    return(min(which(x == 1)))
  }
}
get_PARED <- function(questionaire) {
  mex_ISCED_to_years <-
    data.frame(
      level = c("0", "1", "2", "3A", "3B", "3C", "4", "5A", "5B", "6"),
      years = c(3, 6, 9, 12, 12, 12, 12, 14, 16, 16)
    )
  #Converting ISCO-08 CODES to ISCED levels
  mex_ST005_ST007_to_ISCED <-
    data.frame(ans_code = c(1, 2, 3, 4, 5),
               isced = c("3A", "3B","2", "1", '0'))
  mex_ST006_ST008_to_ISCED <- data.frame(ans_code = c(1, 2, 3, 4),
                                         isced = c("6", "5B", "5A", "4"))

  #selecting relevant data from PFS Student contextual Questionaire
  Q_ISCED <- questionaire %>%
    select(
      stidstd,
      contains('ST005'),
      contains('ST006'),
      contains('ST007'),
      contains('ST008')
    )
  #Items 15-ST006 & 15-ST008, need a different coding approach because of its answer structure.
  #We need to choose the highest answer possible to code it as the ISCED for that
  Q_ISCED_006 <- Q_ISCED %>%
    select(stidstd, contains('ST006'))
  Q_ISCED_008 <- Q_ISCED %>%
    select(stidstd, contains('ST008'))


  #Applying coding function to Items 15-ST006 & 15-ST008
  Q_ISCED_006$`ST006_coded` <-
    apply(Q_ISCED_006 %>% select(contains('ST006')),
          MARGIN = 1,
          FUN = coder)
  Q_ISCED_008$`ST008_coded` <-
    apply(Q_ISCED_008 %>% select(contains('ST008')),
          MARGIN = 1,
          FUN = coder)

  #Adding newly coded Values and removing old ones
  Q_ISCED <- Q_ISCED %>%
    left_join(Q_ISCED_006 %>%
                select(stidstd, `ST006_coded`), by = "stidstd") %>%
    left_join(Q_ISCED_008 %>%
                select(stidstd, `ST008_coded`), by = "stidstd") %>%
    select(-contains('15-ST006'), -contains('15-ST008'))

  #obtaining MISCEDa,MISCEDb,FISCEDa,FISCEDb.
  #a) Referes to lower educational levels
  #b) Refers to higher education level
  Q_ISCED <- Q_ISCED %>%
    left_join(mex_ST005_ST007_to_ISCED, by = c("ST005Q01_15" = "ans_code")) %>%
    rename(MISCEDa = isced) %>%
    left_join(mex_ST005_ST007_to_ISCED, by = c("ST007Q01_15" = "ans_code")) %>%
    rename(FISCEDa = isced) %>%
    left_join(mex_ST006_ST008_to_ISCED, by = c("ST006_coded" = "ans_code")) %>%
    rename(MISCEDb = isced) %>%
    left_join(mex_ST006_ST008_to_ISCED, by = c("ST008_coded" = "ans_code")) %>%
    rename(FISCEDb = isced) %>%
    select(-contains('ST00'))

  isced_to_int=list("0"=0,"1"=1,"2"=2,"3B"=3,"3C"=3,"3A"=4,"4"=4,"5A"=5,"5B"=6,"6"=6)

  #Computing years of education for MISCED and FISCED variables
  PARED <- Q_ISCED %>%
    left_join(mex_ISCED_to_years, by = c("MISCEDa" = "level")) %>% rename(MISCEDa_years =
                                                                            years) %>%
    left_join(mex_ISCED_to_years, by = c("FISCEDa" = "level")) %>% rename(FISCEDa_years =
                                                                            years) %>%
    left_join(mex_ISCED_to_years, by = c("MISCEDb" = "level")) %>% rename(MISCEDb_years =
                                                                            years) %>%
    left_join(mex_ISCED_to_years, by = c("FISCEDb" = "level")) %>% rename(FISCEDb_years =
                                                                            years) %>%
    #select(stidstd,contains('years')) %>%
    #selecting highest educational level for each parent
    rowwise() %>% mutate(
      PAREDm = max(MISCEDa_years, MISCEDb_years, na.rm = TRUE),
      PAREDf = max(FISCEDa_years, FISCEDb_years, na.rm = TRUE)
    ) %>%
    #select(stidstd,contains('PARED')) %>%
    #selecting highest educational from parents
    rowwise() %>% mutate(PARED = max(PAREDm, PAREDf, na.rm = TRUE)) %>% mutate(
      FISCED = case_when(is.na(FISCEDb) ~ FISCEDa,
                         TRUE ~ FISCEDb),
      MISCED = case_when(is.na(MISCEDb) ~ MISCEDa,
                         TRUE ~ MISCEDb)
     )#%>%mutate(FISCED=ifelse(is.null(isced_to_int[[FISCED]]),9, isced_to_int[[FISCED]]),
    #            MISCED=ifelse(is.null(isced_to_int[[MISCED]]),9, isced_to_int[[MISCED]]))

  #it returns the PARED simple variable

  PARED[mapply(is.infinite, PARED)] <- 99
  return(PARED)
}


datin<-get_PARED(data2)%>%
  select(stidstd,PARED,FISCED,MISCED)%>%
  inner_join(data%>%
               select(stidstd,PARED,FISCED,MISCED),by = c("stidstd"="stidstd"))

datin<-datin[,order(colnames(datin))]%>%#mutate(diffP=PARED.x-PARED.y,
                                               #diffM=MISCED.x-MISCED.y,
                                               #diffF=FISCED.x-FISCED.y)%>%
  filter(MISCED.x==4)%>%
  select(stidstd)

datin<-data%>%
 filter(stidstd %in% datin$stidstd)%>%
select(stidstd,
       contains('ST005'),
       contains('ST006'),
       contains('ST007'),
       contains('ST008')
       )

get_HOMEPOS <- function(questionaire) {
  h_params <- get_params('HOMEPOSCQ')
  # HOMEPOS parsing
  ## ¿Hay en tu casa las cosas siguientes?
  q_15ST011 <- questionaire %>%
    select(stidstd, contains('ST011')) %>%
    rowwise() %>%
    mutate_at(vars(contains('ST011')),
              funs(TA = case_when(. == 1 ~ 1, # option 1 is yes (1)
                                  . == 2 ~ 0, # option 2 is no (0)
                                  TRUE ~ as.numeric(NA))) # other responses coded as NA
    ) %>%
    select(stidstd, contains('TA'))

  ## ¿Cuántas de las siguientes cosas hay en tu casa? - excepto TV y smartphone
  q_15ST012 <- questionaire %>%
    select(stidstd, contains('ST012')) %>%
    rowwise() %>% #recoding
    mutate_at(vars(contains('ST012')),
              funs(TA = case_when(
                . == 1 ~ 0, # ninguno
                . == 2 ~ 1, # uno
                . == 3 ~ 2, # dos
                . == 4 ~ 3, # tres o más
                TRUE ~ as.numeric(NA) # other responses coded as NA
              ))) %>%
    select(stidstd, contains('TA'))

  ## ¿Cuántos libros hay en tu casa?
  q_15ST013 <- questionaire %>%
    select(stidstd, contains('ST013')) %>%
    rowwise() %>% #recoding
    mutate_at(vars(contains('ST013')),
              funs(`ST013TA_15` = case_when(
                . == 1 ~ 0, # de 0 a 10 libros
                . == 2 ~ 1, # de 11 a 25 libros
                . == 3 ~ 2, # de 26 a 100 libros
                . == 4 ~ 3, # de 101 a 200 libros
                . == 5 ~ 4, # de 201 a 500 libros
                . == 6 ~ 5, # más de 500 libros
                TRUE ~ as.numeric(NA))) # any other responses coded as NA
    ) %>%
    select(stidstd, contains('TA'))

  # Merging all recoded components of HOMEPOS
  HOMEPOSrecoded <- q_15ST011 %>% # ¿Hay en tu casa las cosas siguientes?
    # left_join(q_15ST012_0_3) %>% ## TV y smartphone
    left_join(q_15ST012) %>%  ## all other posessions
    left_join(q_15ST013) # books
  HOMEPOSrecoded<-HOMEPOSrecoded[rowSums(is.na(HOMEPOSrecoded%>%select(-stidstd))) != ncol(HOMEPOSrecoded%>%select(-stidstd)), ]

  # still missing the anchoring values to have it comparable to PISA.
  # uses 1491/1498 persons, excluding all-NA responses of 7 persons.
  # Refer to:
  # sum(rowSums(sapply(X = HOMEPOSrecoded, is.na)) == ncol(HOMEPOSrecoded) - 1)
  # Loading PISA difficulty values into XSI fixed
  xsi_fixed <- h_params$HOMEPOS$xsi.fixed
  #xsi_fixed <- unname(cbind(1:(length( xsi_fixed)), xsi_fixed ))

  # Loading PISA item slope into b_fixed
  b_fixed<-h_params$HOMEPOS$B.fixed
  print(xsi_fixed)
  print(b_fixed)
  A1 <- TAM::.A.PCM2( resp=HOMEPOSrecoded%>%
                        select(-stidstd))
  # Creating a Generalized Partial Credit Model with anchoring values from international data of PISA
  HOMEPOS_model <- TAM::tam.mml.2pl(resp = HOMEPOSrecoded %>%
                                      select(-stidstd),
                                     irtmodel = "GPCM",
                                     pid = HOMEPOSrecoded %>%
                                     pull(stidstd),
                                     xsi.fixed = xsi_fixed,
                                     B.fixed = b_fixed,
                                     A=A1,
                                     verbose = FALSE
                                     )
  #Obtaining HOMEPOS Values for all students
  print(HOMEPOS_model$xsi)
  HOMEPOS_WLE<-TAM::tam.wle(HOMEPOS_model,Msteps=200)
  print(length(HOMEPOS_WLE$theta))
  #creating a dataframe that will contain HOMEPOS values
  HOMEPOS_idx<-cbind(HOMEPOSrecoded %>% select(stidstd),HOMEPOS_WLE)
  HOMEPOS_idx$pid<-NULL
  return(HOMEPOS_idx %>% select(stidstd,HOMEPOS = theta )%>%
           mutate(HOMEPOS=(HOMEPOS-h_params$HOMEPOSCQ$mean)/h_params$HOMEPOSCQ$sd))

}
dataproc<-get_HOMEPOS(data2)
compar<-inner_join(dataproc %>%
                     select(stidstd,HOMEPOS),
                   data %>%
                     select(stidstd,HOMEPOS),
                   by=c("stidstd"="stidstd"))%>%
  mutate(diff=abs(HOMEPOS.x-HOMEPOS.y))
compar

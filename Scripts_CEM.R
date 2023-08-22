# Installation of packages ---------------------------------------------


# Chargement des librairies ----------------------------------------------------

  pacman::p_load(
                 tidyverse,
                 janitor,
                 remotes,
                 rio,
                 readxl,
                 linelist,
                 here)


# Database Import --------------------------------------------------------------


  cem <- rio::import(here("data","cem_updates.xlsx"))
  
  cem <- cem %>% 
          clean_variable_names()
  
# Data cleaning and others -----------------------------------------------------
  
## Filtering the date from the  "2022-11-17" till up to date -------------------
    
  cem_d0 <- cem %>% 
    clean_variable_names() %>% 
    filter(period == "d0") %>% 
    mutate(across(.cols = contains("date"), .fns = as.Date),
           nom = str_c(identification_surname,identification_othernames,sep = " ")) %>% 
    filter(recordate >= "2022-11-01")
  
  
  cem_autres <- cem %>% 
    clean_variable_names()%>% 
    mutate(across(.cols = contains("date"), .fns = as.Date),
           nom = str_c(identification_surname,identification_othernames,sep = " ")) %>% 
    filter(recordate >= "2022-11-01") 
  
  
 ## Selecting the key variables containing the infor;ations of the participants-
  
  cem_d0 <- cem_d0 %>%
    select(identification_caseid,identification_state,identification_district,nom,identification_age,
           identification_subjsex,vaccination_vaccdate)
  
  cdc <- cem_autres %>% 
    group_by(identification_caseid,period,identification_state,identification_age,identification_subjsex,nom) %>% 
    summarise( n = n())
  

  cdc <- cdc %>% 
    pivot_wider(names_from = period,
                values_from = n)
  
  
   
  cdc <- cdc %>% 
    left_join(cem_d0, by = c("identification_caseid","identification_state","nom"))
  
  
  
  cdc<- cdc %>% 
    select(identification_caseid,identification_state,identification_district,nom,identification_age.x,
           identification_subjsex.x,vaccination_vaccdate,d0,d1,d3,d7,d15,d30) %>% 
           arrange(nom)
  
    
  
  ## Add the date frame for the names ------------------------------------------
  
  test <- cem_autres %>%
    mutate(nom = identification_enroleur) %>% 
    group_by(identification_caseid,period,nom) %>% 
    summarise(n = n())
  
  
  
  test <- test %>% 
    pivot_wider(names_from = period,
                values_from = nom)
  
  test <- test %>% 
           mutate(across(.cols = everything(),.fns = as.character)) %>% 
            select(identification_caseid,d0,d1,d3,d7,d15,d30)
  
  
testeur<- as.data.frame(test)


## Add a data frame for the date of follow up-----------------------------------

periodes <-  c("d1","d3","d7","d15","d30","d42")

dates <- cem_autres %>% 
            select(identification_caseid,period, recordate) %>% 
            filter(period %in% periodes) %>% 
            mutate(recordate =  as.character(recordate))

  dates <- dates %>% 
  pivot_wider(names_from = period,
              values_from = recordate,
              names_repair = "unique")
  
  
  dates <- dates %>% 
    mutate(across(.cols = c(d1:d30),.fns = as.character)) %>% 
    select(identification_caseid,d1,d3,d7,d15,d30)
  
  
# Export the dataset -----------------------------------------------------------


export(testeur,  
       file=here::here("Outputs/testeur.xlsx"))


export(cdc,  
       file=here::here("Outputs/cdc.xlsx"))

  
  
export(dates,  
       file=here::here("Outputs/dates.xlsx"))



# Retrait de valeurs en double -------------------------------------------------

## Doublons de la periode d0 ---------------------------------------------------

enrollement <- cem %>% 
                 filter(period == "d0")

t1 <- enrollement %>% distinct(identification_caseid, .keep_all = TRUE) 

## Doublons periode d1 ---------------------------------------------------------

jour1 <- cem %>% 
  filter(period == "d1")

jounn <- jour1 %>% distinct(identification_caseid, .keep_all = TRUE)


## Doublons periode d3 ---------------------------------------------------------

 jour3 <-  cem %>% 
            filter(period == "d3")

 day3 <- jour3 %>% distinct(identification_caseid, .keep_all = TRUE)

## Doublons  periode d7 --------------------------------------------------------
 
   
 jour7 <-  cem %>% 
   filter(period == "d7")
 
 day7 <- jour7 %>% distinct(identification_caseid, .keep_all = TRUE)


## Doublons periode d15 --------------------------------------------------------
 
 jour15 <-  cem %>% 
   filter(period == "d15")
 
 day15 <- jour15 %>% distinct(identification_caseid, .keep_all = TRUE)

## Doublons periode d30 --------------------------------------------------------
 
  
 jour30 <-  cem %>% 
   filter(period == "d30")
 
 day30 <- jour30 %>% distinct(identification_caseid, .keep_all = TRUE)
 

## Merge de d0 et d1  ----------------------------------------------------------
 
     fichier <- union_all(t1,jounn) %>% 
                union_all(day3) %>% 
                union_all(day7) %>% 
                union_all(day15) %>% 
                union_all(day30)
 
   fichier_do <- fichier %>% 
                   filter(period ==  "d0")
 
 
 ## Analyse deu suivi ----------------------------------------------------------
 
   fichier <- fichier %>% 
     clean_variable_names()%>% 
     mutate(across(.cols = contains("date"), .fns = as.Date),
            nom = str_c(identification_surname,identification_othernames,sep = " ")) %>% 
     filter(recordate >= "2022-11-01") 
   
   fichier_autres <- fichier %>% 
     group_by(identification_caseid,period,identification_state,identification_age,identification_subjsex,nom) %>% 
     summarise( n = n())
   
   fichier_autres <- fichier_autres %>% 
     pivot_wider(names_from = period,
                 values_from = n)
   
    fichier_autres <- fichier_autres %>% 
                        select(identification_caseid,identification_state,d0,d1,d3,d7,d15,d30)
    
## Tri des valeurs dont 1 est est partout ---------------------------------    
    
      suivi_ok <- fichier_autres %>% 
                     filter(d0 ==  1 & d1 == 1 & d3 ==1 & d7==1 & d15 == 1)
    
       sans_d0 <- fichier_autres %>% 
                    filter(is.na(d0))
       avec_d0 <- fichier_autres %>% 
         filter(!is.na(d0))
       
       avec_d0_id <- avec_d0 %>% 
                      select(identification_caseid)
       
       id_select <- as.data.frame(avec_d0$identification_caseid) 
       
       id_select <- id_select %>% 
                      rename(identification_caseid=`avec_d0$identification_caseid`) %>% 
                      mutate(identification_caseid = as.vector(identification_caseid))
       
       vector <- as.vector(id_select$identification_caseid)
                         
        bd_id <- fichier %>% 
                   filter(identification_caseid %in% vector)
 
# Fichier de sortie ------------------------------------------------------------

 export(bd_id,  
        file=here::here("Outputs/doublonsnettoye).xlsx"))
   







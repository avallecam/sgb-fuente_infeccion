library(tidyverse)
library(magrittr)
library(readxl)
library(avallecam)

rute <- "<path-to-local-denominator-data>/"
list.files(path = rute)

denomi_pe <- read_rds(paste0(rute,"data/pop_pe-year_dept_age_std_sex-2000_2020.rds")) %>% 
  mutate(sexo=str_to_title(sexo)) #%>% count(sexo)

denomi_pe %>% 
  filter(year==2019) %>% 
  group_by(departamento) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  filter(is_in(departamento,c("CAJAMARCA","LIMA","PIURA","LAMBAYEQUE","JUNIN","CALLAO"))) %>% 
  summarise(pop=sum(pop)) %>% 
  mutate(cases=700,
         n_exposed=1000*cases,
         prop_exposed=1-(n_exposed/pop))

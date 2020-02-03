
# pendientes --------------------------------------------------------------

#' fechas ojo cómo está?
#' frecuencia de consumo reacategorizazda?
#' 
# packages ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(janitor)
library(compareGroups)
library(avallecam)

# identify file -----------------------------------------------------------

kobo_file <- "kobo-raw/afpj8GZMya3pyaRRz5Pwwg_2019_12_17_15_22_00.xlsx"
excel_format(path = kobo_file)

# import + cleaning -------------------------------------------------------

sgb <- read_excel(path = kobo_file) %>% 
  clean_names() %>% 
  #corregir ingresos
  mutate(introduction_id=str_replace_all(introduction_id,"-",""),
         introduction_id=case_when(
           introduction_id=="CA0403" & introduction_case_or_control=="control" ~ "CO0403",
           introduction_id=="CA0404" & introduction_case_or_control=="control" ~ "CO0404",
           TRUE~introduction_id
         ),
         introduction_case_or_control=fct_relevel(introduction_case_or_control,"control")) %>%
  #cambiar tipo de variable
  mutate_if(.predicate = is.logical,.funs = as.factor) %>% 
  #cambiar orden dentro de variable
  mutate(alimentos_consumo_frequ_cheese=fct_relevel(alimentos_consumo_frequ_cheese,"one_twoq","three_fourq"),
         alimentos_consumo_frequ_corte=fct_relevel(alimentos_consumo_frequ_corte,"one_twoq","three_fourq")) %>% 
  #re-categorization
  mutate(alimentos_consumo_frequ_freshchik_new=fct_collapse(alimentos_consumo_frequ_freshchik,
                                                        "three_moreq"=c("three_fourq","five_moreq")),
         alimentos_consumo_frequ_freshchik_new=fct_relevel(alimentos_consumo_frequ_freshchik_new,"one_twoq")) %>% 
  mutate(alimentos_consumo_mercado1_new=case_when(
    str_detect(alimentos_consumo_mercado1,"queque|QUEQUE")~"mercado_moshoqueque",
    str_detect(alimentos_consumo_mercado1,"modelo|Modelo|MODELO")~"mercado_modelo",
    str_detect(alimentos_consumo_mercado1,"lambayeque|Lambayeque|LAMBAYEQUE")~"mercado_lambayeque",
    str_detect(alimentos_consumo_mercado1,"inca|Inca|INCA")~"mercado_inca_lavictoria",
    TRUE~"mercado_otro"
  )) %>% 
  mutate(alimentos_consumo_mercado2_new=case_when(
    str_detect(alimentos_consumo_mercado2,"queque|QUEQUE")~"mercado_moshoqueque",
    str_detect(alimentos_consumo_mercado2,"modelo|Modelo|MODELO")~"mercado_modelo",
    str_detect(alimentos_consumo_mercado2,"lambayeque|Lambayeque|LAMBAYEQUE")~"mercado_lambayeque",
    str_detect(alimentos_consumo_mercado2,"inca|Inca|INCA")~"mercado_inca_lavictoria",
    TRUE~"mercado_otro"
  )) %>% 
  mutate(alimentos_consumo_mercado3_new=case_when(
    str_detect(alimentos_consumo_mercado3,"queque|QUEQUE")~"mercado_moshoqueque",
    str_detect(alimentos_consumo_mercado3,"modelo|Modelo|MODELO")~"mercado_modelo",
    str_detect(alimentos_consumo_mercado3,"lambayeque|Lambayeque|LAMBAYEQUE")~"mercado_lambayeque",
    str_detect(alimentos_consumo_mercado3,"inca|Inca|INCA")~"mercado_inca_lavictoria",
    TRUE~"mercado_otro"
  )) %>% 
  #retiro temporal por no tener control
  filter(introduction_id!="CA0402")
sgb %>% glimpse()

# evaluate cleaning -------------------------------------------------------

#non-duplicates
sgb %>% 
  count(introduction_id,sort = T) #%>% print_inf()
sgb %>% 
  count(introduction_id,introduction_case_or_control) %>% 
  arrange(introduction_id) %>% 
  print_inf()
# sgb %>% 
#   count(introduction_id,submission_time,sort = T) %>% 
#   arrange(submission_time) %>% 
#   print_inf()

#pairing
sgb %>% count(introduction_case_or_control)
sgb %>% 
  count(introduction_id,introduction_case_or_control) %>% 
  arrange(introduction_id) %>% 
  mutate(separate_id=str_replace(introduction_id,"(..)(.+)","\\2")) %>% 
  #select(introduction_case_or_control,separate_id) %>%
  arrange(separate_id) %>% 
  print_inf()
  # pivot_wider(names_from = introduction_case_or_control,
  #             values_from = separate_id)
# # mujer de 54 anhos
# sgb %>% 
#   filter(introduction_id=="CA0402") %>% 
#   glimpse()

# describe ----------------------------------------------------------------

sgb %>% 
  select(-starts_with("consent_")) %>% 
  compareGroups(formula = introduction_case_or_control~.,data = .,max.xlev = 100) %>% 
  createTable(show.ratio = T,show.n = T,show.all = T,digits = 1,sd.type = 2) %>% 
  export2xls("table/tab01-all_variables.xls")


# draft-zone --------------------------------------------------------------

# sgb %>% 
#   select(alimentos_consumo_mercado3) %>% #count(alimentos_consumo_mercado3)
#   mutate(alimentos_consumo_mercado3_new=case_when(
#     str_detect(alimentos_consumo_mercado3,"queque|QUEQUE")~"mercado_moshoqueque",
#     str_detect(alimentos_consumo_mercado3,"modelo|Modelo|MODELO")~"mercado_modelo",
#     str_detect(alimentos_consumo_mercado3,"lambayeque|Lambayeque|LAMBAYEQUE")~"mercado_lambayeque",
#     str_detect(alimentos_consumo_mercado3,"inca|Inca|INCA")~"mercado_inca_lavictoria",
#     TRUE~"mercado_otro"
#   )) %>% 
#   count(alimentos_consumo_mercado3,alimentos_consumo_mercado3_new) %>% 
#   print_inf()



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
library(survival)
library(magrittr)

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
  filter(introduction_id!="CA0402") %>% 
  
  #definicion de pares
  mutate(separate_id=str_replace(introduction_id,"(..)(.+)","\\2")) %>% 
  #select(introduction_case_or_control,separate_id) %>%
  arrange(separate_id) %>% 
  mutate(separate_id_fct=as.factor(separate_id),
         separate_id_num=as.numeric(separate_id_fct)) %>% 
  
  #adaptar outcome a formato dicotomico
  mutate(outcome=as.numeric(introduction_case_or_control)-1) 

sgb %>% glimpse()
sgb %>% count(introduction_case_or_control,outcome)

sgb %>% haven::write_dta("data/sgb-01.dta")

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
  count(separate_id,separate_id_fct,separate_id_num)

# # mujer de 54 anhos
# sgb %>% 
#   filter(introduction_id=="CA0402") %>% 
#   glimpse()

# describe ----------------------------------------------------------------

sgb %>% 
  select(-starts_with("consent_"),-starts_with("separate_")) %>% 
  compareGroups(formula = introduction_case_or_control~.,data = .,max.xlev = 100) %>% 
  createTable(show.ratio = T,show.n = T,show.all = T,digits = 1,sd.type = 2) %>% 
  export2xls("table/tab01-all_variables.xls")


# paired analysis ----------------------------------------------------

#' Matched case-control r comparegroups
#' https://www.rdocumentation.org/packages/epiR/versions/1.0-4/topics/epi.ccsize
#' https://datascienceplus.com/how-to-use-r-for-matching-samples-propensity-score/
#' https://stackoverflow.com/questions/35574269/matching-cases-to-controls-without-replacement-in-r
#' https://stat.ethz.ch/R-manual/R-devel/library/survival/html/clogit.html
#' https://rpubs.com/kaz_yos/case-control1
#' https://rdrr.io/rforge/Epi/man/ccwc.html
#' 
#' library(survival)
#' example("clogit")

# _test model outputs --------------------------------------------------------------

#sgb %>% glimpse()
#sgb %>% pull(outcome) %>% mean()

glm(outcome ~ alimentos_consumo_milkalim, 
    data = sgb, 
    family = binomial(link = "logit"),
    na.action = na.exclude) %>% 
  epi_tidymodel_or()

glm(outcome ~ alimentos_consumo_milkalim+introduction_age+introduction_sex, 
    data = sgb, 
    family = binomial(link = "logit"),
    na.action = na.exclude) %>% 
  epi_tidymodel_or()

#no hay convergencia
clogit(formula = outcome ~ alimentos_consumo_milkalim+strata(separate_id_num), 
       data = sgb, 
       na.action = na.exclude) %>% 
  epi_tidymodel_or()

# _new strategy -----------------------------------------------------------

#' el modelo de regresion no llegó a convergir en la estimación de parámetros
#' el motivo puede haberse dado por el pequeño tamaño de muestra
#' una alternativa conservadora es realizar modelos de regresión 
#' ajustando por los confusores de edad y sexo con los cuales se diseño el pareamiento
#' estrategia:
#' - realizar un modelo nulo con edad y sexo
#' - mapear este modelo inicial hacia todas las covariables
#' - generar la nueva tabla con las nuevas estimaciones de OR ajustando por edad y sexo 


# _01 null model ----------------------------------------------------------

glm_null <- glm(outcome ~ introduction_age+introduction_sex, 
                data = sgb, 
                family = binomial(link = "logit"),
                na.action = na.exclude)
glm_null %>% epi_tidymodel_or()


# _02_update model --------------------------------------------------------

simple_models <- sgb %>%  
  select(
    # introduction_sex,
    # introduction_age,
    introduction_education,
    clinical_eda,
    clinical_eda_familia,
    clinical_ira,
    clinical_viaje,
    alergias_dietas_dieta,
    alergias_dietas_alimento_diet,
    alergias_dietas_alimento_diet_mariscos,
    alergias_dietas_alimento_diet_milk,
    alergias_dietas_alimento_diet_other2,
    alimentos_fuera_restaurant,
    alimentos_fuera_tipo_rest_brasa,
    alimentos_fuera_tipo_rest_menu,
    alimentos_fuera_tipo_rest_chifa,
    alimentos_fuera_tipo_rest_criollo,
    alimentos_fuera_tipo_rest_ambulante,
    alimentos_fuera_tipo_rest_food_market,
    alimentos_fuera_tipo_rest_soda_sand,
    #alimentos_fuera_tipo_rest_evento,
    alimentos_fuera_tipo_rest_fast_food,
    alimentos_fuera_tipo_rest_cebiche,
    alimentos_fuera_tipo_rest_work_cafeteria,
    alimentos_fuera_tipo_rest_other3,
    #alimentos_fuera_freq_rest2,
    alimentos_fuera_freq_rest3,
    alimentos_consumo_frequ_freshchik,
    alimentos_consumo_froz_chicken,
    #alimentos_consumo_frequ_frozchik,
    alimentos_consumo_liver,
    alimentos_consumo_frequ_liver,
    alimentos_consumo_almacenamiento,
    alimentos_consumo_almacenamiento_refrig,
    alimentos_consumo_almacenamiento_tempo,
    alimentos_consumo_almacenamiento_noalmace,
    alimentos_consumo_markets_market1,
    alimentos_consumo_markets_market2,
    alimentos_consumo_markets_market3,
    alimentos_consumo_markets_other4,
    alimentos_consumo_brandchik,
    alimentos_consumo_milk_can,
    alimentos_consumo_volmilkcan,
    alimentos_consumo_milk_can1,
    alimentos_consumo_volmilkcan1,
    alimentos_consumo_milk_can2,
    alimentos_consumo_volmilkcan2,
    alimentos_consumo_milk_can3,
    alimentos_consumo_beb,
    alimentos_consumo_bebfreq,
    alimentos_consumo_milkalim,
    alimentos_consumo_milkalimfreq,
    alimentos_consumo_milkcom,
    alimentos_consumo_milkcomfreq,
    alimentos_consumo_milk_box,
    alimentos_consumo_milk_box1,
    alimentos_consumo_milk_box2,
    alimentos_consumo_milk_past,
    alimentos_consumo_milk_past1,
    #alimentos_consumo_volmilkpast2,
    #alimentos_consumo_volmilkpow,
    #alimentos_consumo_volmilkpow1,
    #alimentos_consumo_volmilkpow2,
    #alimentos_consumo_volmilkpow3,
    #alimentos_consumo_milk_raw,
    #alimentos_consumo_volmilkraw,
    #alimentos_consumo_hervor,
    alimentos_consumo_openmilk,
    alimentos_consumo_openmilk_tap,
    alimentos_consumo_openmilk_hueco,
    alimentos_consumo_openmilk_abrefacil,
    #alimentos_consumo_aguadil,
    #alimentos_consumo_aguadil_herv,
    #alimentos_consumo_aguadil_cruda,
    alimentos_consumo_dilucion,
    alimentos_consumo_almacenmilk,
    alimentos_consumo_almacenmilk_refrig,
    alimentos_consumo_almacenmilk_tempo,
    alimentos_consumo_almacenmilk_noalmace,
    alimentos_consumo_almacenmilk_noaplica,
    alimentos_consumo_almacecan,
    #alimentos_consumo_milktimeamb,
    alimentos_consumo_milktimerefr,
    alimentos_consumo_milkmarket_market1,
    alimentos_consumo_milkmarket_market2,
    alimentos_consumo_milkmarket_market3,
    alimentos_consumo_milkmarket_other4,
    alimentos_consumo_fresh_cheese,
    alimentos_consumo_frequ_cheese,
    #alimentos_consumo_chee_gloria,
    #alimentos_consumo_frequ_cheeglo,
    #alimentos_consumo_chee_bonle,
    #alimentos_consumo_frequ_cheebon,
    alimentos_consumo_chee_lai,
    #alimentos_consumo_frequ_cheelai,
    #alimentos_consumo_chee_other,
    #alimentos_consumo_otro3,
    #alimentos_consumo_frequ_cheeother,
    alimentos_consumo_chee_corte,
    alimentos_consumo_frequ_corte,
    alimentos_consumo_chee_art,
    alimentos_consumo_frequ_art,
    alimentos_consumo_cheecom,
    alimentos_consumo_cheecom2,
    alimentos_consumo_cheemarket,
    alimentos_consumo_cheemarket_market1,
    alimentos_consumo_cheemarket_market3,
    alimentos_consumo_cheemarket_other4,
    #alimentos_consumo_supermercado3,
    alimentos_consumo_almacenchee_refrig,
    #alimentos_consumo_almacenchee_tempo,
    alimentos_consumo_almacenchee_noalmace,
    #alimentos_consumo_almacenchee_noaplica,
    #alimentos_consumo_note_yogurt,
    alimentos_consumo_yogurt,
    alimentos_consumo_volyogurt,
    alimentos_consumo_yogloria,
    alimentos_consumo_volgloria,
    #alimentos_consumo_volyoleit,
    alimentos_consumo_yolaive,
    alimentos_consumo_yogurt_other,
    alimentos_consumo_almacenyog,
    alimentos_consumo_almacenyog_refrig,
    alimentos_consumo_almacenyog_tempo,
    alimentos_consumo_almacenyog_noalmace,
    alimentos_consumo_almacenyog_noaplica,
    alimentos_consumo_yogmarket_market1,
    alimentos_consumo_yogmarket_market2,
    alimentos_consumo_yogmarket_market3,
    alimentos_consumo_yogmarket_other4,
    alimentos_consumo_fish,
    alimentos_consumo_frequ_fish,
    alimentos_consumo_moluscos,
    alimentos_consumo_almacenfish,
    alimentos_consumo_almacenfish_refrig,
    alimentos_consumo_almacenfish_noalmace,
    alimentos_consumo_almacenfish_noaplica,
    alimentos_consumo_fishmarket,
    alimentos_consumo_fishmarket_market1,
    alimentos_consumo_fishmarket_other4,
    #alimentos_consumo_supermercado5,
    #alimentos_consumo_bodega5,
    #alimentos_consumo_note_frutveg,
    alimentos_consumo_tomato,
    alimentos_consumo_freqtom,
    alimentos_consumo_onion,
    alimentos_consumo_freqoni,
    alimentos_consumo_avocado,
    alimentos_consumo_freqavo,
    alimentos_consumo_banana,
    alimentos_consumo_freqban,
    alimentos_consumo_vegother,
    alimentos_consumo_freqvegother,
    alimentos_consumo_fruitother,
    alimentos_consumo_freqfruitother,
    # alimentos_consumo_vegfruitmarket_market1,
    # alimentos_consumo_vegfruitmarket_market3,
    # #alimentos_consumo_vegfruitmarket_other4,
    # #alimentos_consumo_supermercado6,
    # alimentos_consumo_bodega6,
    # #alimentos_consumo_obs_vegfruit,
    # #alimentos_consumo_note_water,
    alimentos_consumo_raw_water,
    alimentos_consumo_freqrawwater,
    alimentos_consumo_teeth,
    alimentos_consumo_freqteeth,
    #alimentos_consumo_freqboil,
    alimentos_consumo_treated,
    alimentos_consumo_freqtreated,
    # alimentos_consumo_bottle,
    alimentos_consumo_freqbottle,
    # alimentos_consumo_watercarlos,
    # alimentos_consumo_watermateo,
    # alimentos_consumo_freqmateo,
    # alimentos_consumo_waterluis,
    # alimentos_consumo_freqluis,
    # alimentos_consumo_waterother,
    # alimentos_consumo_otro12,
    # alimentos_consumo_freqwatother,
    alimentos_preparar_refrigeracion,
    #alimentos_preparar_lava_verduras,
    alimentos_preparar_agua_cruda,
    #alimentos_preparar_mercado_sep,
    #alimentos_preparar_refri_sep,
    alimentos_preparar_comida_tabla,
    alimentos_preparar_pollo_tipo_sopas,
    alimentos_preparar_pollo_tipo_estofado,
    alimentos_preparar_pollo_tipo_parrilla,
    alimentos_preparar_pollo_tipo_frito,
    alimentos_preparar_pollo_tipo_otro,
    #alimentos_preparar_envasado_previo_note,
    #alimentos_preparar_milkvencida,
    #alimentos_preparar_envasado_previo_yogurt,
    animales_hogar_dogs,
    #animales_hogar_agedogs,
    #animales_hogar_agedogs_age1,
    #animales_hogar_agedogs_age2,
    animales_hogar_hens,
    animales_hogar_cows,
    casa_caracteristicas_aguapot,
    casa_caracteristicas_desag,
    #casa_caracteristicas_desagopt,
    productos_gloriabluebig,
    productos_gloriaredbig,
    productos_gloriayellowbig,
    #productos_gloriagreenbig,
    #productos_glorialightblubig,
    productos_gloriabluesmall,
    productos_gloriaredsmall,
    productos_gloriayellowsmall,
    productos_idealcrem,
    productos_idealaman,
    productos_ideallight,
    productos_ppvida,
    #productos_cajabonent,
    #productos_cajalaient,
    #productos_cajalaini,
    #productos_cajalalight,
    #productos_frescgloch,
    #productos_yogulai,
    productos_yoguglo,
    alimentos_consumo_frequ_freshchik_new,
    alimentos_consumo_mercado1_new,
    alimentos_consumo_mercado2_new,
    alimentos_consumo_mercado3_new,
    
    #seleccionadas
    clinical_eda,
    clinical_ira,
    alimentos_fuera_restaurant,
    alimentos_consumo_fresh_chicken,
    alimentos_consumo_frequ_freshchik_new,
    alimentos_consumo_almacenamiento,
    alimentos_consumo_markets_market3,
    alimentos_consumo_mercado1_new,
    alimentos_consumo_otro1,
    alimentos_consumo_milk_can,
    alimentos_consumo_milk_can1,
    alimentos_consumo_milk_can2,
    alimentos_consumo_milk_box,
    alimentos_consumo_milk_past,
    productos_gloriabluebig,
    productos_gloriabluesmall,
    alimentos_consumo_almacenmilk_refrig,
    alimentos_consumo_almacenmilk_noalmace,
    alimentos_consumo_milktimerefr,
    alimentos_consumo_milkmarket_market1,
    alimentos_consumo_milkmarket_market3,
    alimentos_consumo_mercado2_new,
    alimentos_consumo_fresh_cheese,
    alimentos_consumo_frequ_cheese,
    alimentos_consumo_chee_corte,
    alimentos_consumo_frequ_corte,
    alimentos_consumo_cheemarket,
    alimentos_consumo_mercado3_new,
    alimentos_consumo_almacenchee,
    alimentos_consumo_almacenchee_refrig
    
  ) %>% 
  colnames() %>% 
  enframe(name = NULL) %>% 
  # filter(!value %in% c("outcome","nueva_clasif",
  #                      "codigo_participante","codigo","p05")) %>% 
  mutate(variable=map(value,sym),
         simple_rawm=map(variable,epi_tidymodel_up,reference_model=glm_null),
         simple_tidy=map(simple_rawm,epi_tidymodel_or)
  ) %>% 
  unnest(cols = c(simple_tidy)) %>% 
  filter(term!="(Intercept)") %>% 
  filter(term!="introduction_age") %>% 
  filter(term!="introduction_sexmale") %>% 
  select(-value,-variable,-simple_rawm)

simple_models %>% print_inf()

punto_corte_valor_p <- 0.2
simple_models %>% 
  filter(p.value<punto_corte_valor_p) %>% 
  arrange(p.value) %>% 
  arrange(desc(or)) %>% 
  print_inf()

simple_models %>% writexl::write_xlsx("table/tab01-all_variables-age_sex_adjusted.xlsx")

simple_models %>% 
  filter(p.value<punto_corte_valor_p) %>% 
  arrange(p.value) %>% 
  arrange(desc(or)) %>% 
  writexl::write_xlsx("table/tab01-all_variables-age_sex_adjusted-filter.xlsx")

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

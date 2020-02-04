
use "data/sgb-01.dta"
descr
br

codebook alimentos_consumo_milkalim
encode alimentos_consumo_milkalim, gen(alimentos_consumo_milkalim_fct)
codebook alimentos_consumo_milkalim_fct

clogit outcome alimentos_consumo_milkalim_fct, group(separate_id_num) or

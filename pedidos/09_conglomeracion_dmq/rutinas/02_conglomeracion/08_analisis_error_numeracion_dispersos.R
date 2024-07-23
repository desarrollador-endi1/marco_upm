rm(list = ls())

new <- readRDS("pedidos/09_conglomeracion_dmq/man_sec_upm_final_dmq.rds") |> 
  select(man_sec, id_upm1 = id_upm)
old <- readRDS("productos/02_conglomeracion/man_sec_upm_final_dmq_control.rds") |> 
  select(man_sec, id_upm2 = id_upm)

apoyo <- full_join(new, old, by = "man_sec") |> 
  mutate(control = id_upm1 != id_upm2)

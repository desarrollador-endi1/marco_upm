lol <- upm_ciu %>% 
  mutate(canton = substr(id_upm, 1, 4)) %>% 
  group_by(canton, area) %>% 
  count() %>% 
  filter(canton %in% c("0101", "0601", "0701", "0901", "1101", "1308", "1701",
                       "1801", "2301", "0801"))

lol1 <- upm_ciu %>% 
  mutate(canton = substr(id_upm, 1, 4),
         dominio = case_when(canton %in% c("0101", "0601", "0701", "0901", 
                                           "1101", "1308", "1701", "1801", 
                                           "2301", "0801") & area == 1 ~ "991",
                             T ~ paste0(provincia, area))) %>% 
  group_by(provincia, dominio) %>% 
  count() %>% 
  filter(provincia %in% c("01", "06", "07", "09", "11", "13", "17", "18", "23",
                          "08"))

write.table(lol, "clipboard")

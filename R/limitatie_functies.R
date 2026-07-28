# library(tidyverse)
# library(HHSKwkl)
# 
# theme_set(hhskthema())
# 
# fys_chem <- data_online("fys_chem.rds")
# parameters <- data_online("parameters.rds")
# meetpunten <- data_online("meetpunten.rds")
# 
# 
# fys_chem |> 
#   filter(mp == "S_0067", year(datum) > 2000) 
  


plot_limitatie_jaar <- function(data, mp) {
  
  data |> 
    filter(parnr %in% c(2, 4, 6, 24)) |> 
    select(-parnr, -eenheid) |> 
    summarise(waarde = first(waarde), detectiegrens = first(detectiegrens), .by  = c(mp, datum, par)) |> 
    pivot_wider(names_from = par, values_from = c(waarde, detectiegrens)) |> 
    mutate(`C-limitatie` = waarde_HCO3 < 85,
           `N-limitatie` = detectiegrens_NH4 == "<" & detectiegrens_sNO3NO2 == "<",
           `P-limitatie` = detectiegrens_PO4 == "<") |> 
    mutate(across(.cols = c(`N-limitatie`, `P-limitatie`), .fns = \(x) ifelse(is.na(x), FALSE, TRUE))) |>
    select(-contains("detectiegrens"), -contains("waarde")) |> 
    pivot_longer(cols = contains("limitatie"), names_to = "type", values_to = "limitatie") |> 
    mutate(datum_kol = format(datum , "%Y")) |>
    group_by(mp, datum_kol, type) |>
    summarise(limitatie = any(limitatie)) |>
    ungroup() |>
    ggplot(aes(type, fct_rev(datum_kol), fill = limitatie)) + 
    geom_tile(colour = grijs) +
    scale_fill_manual(values = c("TRUE" = blauw, "FALSE" = NA), labels = 
                        function(x) case_when(is.na(x) ~ "Geen data", x == TRUE ~ "Limitatie", x == FALSE ~ "Geen limitatie")) +
    scale_x_discrete(position = "top") +
    labs(title = "Treedt er groeilimitatie op?",
         subtitle = mp,
         x = "",
         y = "") +
    theme(panel.grid.major = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom") 
  
  
}

plot_limitatie_maand <- function(data, mp) {
  
  data |> 
    filter(parnr %in% c(2, 4, 6, 24)) |> 
    select(-parnr, -eenheid) |> 
    summarise(waarde = first(waarde), detectiegrens = first(detectiegrens), .by  = c(mp, datum, par)) |> 
    pivot_wider(names_from = par, values_from = c(waarde, detectiegrens)) |> 
    mutate(`C-limitatie` = waarde_HCO3 < 85,
           `N-limitatie` = detectiegrens_NH4 == "<" & detectiegrens_sNO3NO2 == "<",
           `P-limitatie` = detectiegrens_PO4 == "<") |> 
    mutate(across(.cols = c(`N-limitatie`, `P-limitatie`), .fns = \(x) ifelse(is.na(x), FALSE, TRUE))) |>
    select(-contains("detectiegrens"), -contains("waarde")) |> 
    pivot_longer(cols = contains("limitatie"), names_to = "type", values_to = "limitatie") |> 
    mutate(datum_kol = format(datum , "%Y-%m")) |>
    group_by(mp, datum_kol, type) |>
    summarise(limitatie = any(limitatie)) |>
    ungroup() |>
    ggplot(aes(type, fct_rev(datum_kol), fill = limitatie)) + 
    geom_tile(colour = grijs) +
    scale_fill_manual(values = c("TRUE" = blauw, "FALSE" = NA), labels = 
                        function(x) case_when(is.na(x) ~ "Geen data", x == TRUE ~ "Limitatie", x == FALSE ~ "Geen limitatie")) +
    scale_x_discrete(position = "top") +
    labs(title = "Treedt er groeilimitatie op?",
         subtitle = mp,
         x = "",
         y = "") +
    theme(panel.grid.major = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom") 
  
  
}

# fys_chem |> 
#   filter(mp == "S_0067", year(datum) > 2020) |> 
#   plot_limitatie_jaar(mp = "S_0067")
# 
# fys_chem |> 
#   filter(mp == "S_0067", year(datum) > 2020) |> 
#   plot_limitatie_maand(mp = "S_0067")
#   
#   


library(tidyverse)
library(googledrive)
library(janitor)

datasus_url <- 
    "https://github.com/pabsantos/datasus-rda/raw/main/rda/datasus-sim-2022.rda"

load(datasus_url)
load("data/prf-sinistros-2022.rda")
load("data/renaest-sinistros-2022.rda")

cod_norte_fluminense <- str_sub(c(
    "3301009",
    "3300936",
    "3301157", 
    "3301405", 
    "3302403",
    "3304151",
    "3304805",
    "3304755",
    "3305000"
), 1, 6)


# Óbitos ------------------------------------------------------------------

tab_obitos <- datasus_sim |> 
    filter(
        ano_ocorrencia %in% seq(2017, 2021, 1),
        cod_municipio %in% cod_norte_fluminense
    ) |> 
    count(nome_municipio, ano_ocorrencia) |> 
    pivot_wider(
        names_from = ano_ocorrencia,
        values_from = n,
        values_fill = 0
    ) |> 
    adorn_totals(name = "Mesorregião Norte Fluminense") |>
    filter(nome_municipio %in% c(
        "Campos dos Goytacazes", "São João da Barra",
        "Mesorregião Norte Fluminense"
    ))

# Sinistros ---------------------------------------------------------------



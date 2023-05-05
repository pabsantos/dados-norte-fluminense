library(tidyverse)
library(googledrive)
library(janitor)
library(lubridate)
library(writexl)

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


df_municipios <- tibble(
    codigo_ibge = c(
        "3301009",
        "3300936",
        "3301157", 
        "3301405", 
        "3302403",
        "3304151",
        "3304805",
        "3304755",
        "3305000"
    ),
    nome_municipio = c(
        "Campos dos Goytacazes",
        "Carapebus",
        "Cardoso Moreira",
        "Conceição de Macabu",
        "Macaé",
        "Quissamã",
        "São Fidélis",
        "São Francisco de Itabapoana",
        "São João da Barra"
    )
)

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

tab_prf <- prf_sinistros |>
    mutate(ano_ocorrencia = year(data_inversa)) |> 
    filter(
        uf == "RJ",
        ano_ocorrencia > 2016,
        municipio %in% c(
            "CAMPOS DOS GOYTACAZES", "CARAPEBUS", "CARDOSO MOREIRA",
            "CONCEICAO DE MACABU", "MACAE", "QUISSAMA", "SAO FIDELIS", 
            "SAO FRANCISCO DE ITABAPOANA", "SAO JOAO DA BARRA"
        )
    ) |> 
    count(municipio, ano_ocorrencia) |> 
    pivot_wider(names_from = ano_ocorrencia, values_from = n, values_fill = 0) |> 
    adorn_totals(name = "Mesorregião Norte Fluminense") |> 
    filter(municipio %in% c(
        "CAMPOS DOS GOYTACAZES", "SAO JOAO DA BARRA",
        "Mesorregião Norte Fluminense"
    ))

tab_renaest <- renaest_sinistros |>
    mutate(codigo_ibge = as.character(codigo_ibge)) |> 
    filter(
        ano_acidente > 2016,
        codigo_ibge %in% c(
            "3301009",
            "3300936",
            "3301157", 
            "3301405", 
            "3302403",
            "3304151",
            "3304805",
            "3304755",
            "3305000"
        )
    ) |> 
    left_join(df_municipios, by = "codigo_ibge") |> 
    count(nome_municipio, ano_acidente) |>
    pivot_wider(names_from = ano_acidente, values_from = n) |> 
    adorn_totals(name = "Mesorregião Norte Fluminense") |>
    filter(nome_municipio %in% c(
        "Campos dos Goytacazes", "São João da Barra",
        "Mesorregião Norte Fluminense"
    ))

# export ------------------------------------------------------------------

list_tab <- list(
    tab_obitos,
    tab_prf,
    tab_renaest
)

list_path <- paste0(
    "table/",
    c("tab_obitos.xlsx", "tab_prf.xlsx", "tab_renaest.xlsx")
)

map2(list_tab, list_path, write_xlsx)

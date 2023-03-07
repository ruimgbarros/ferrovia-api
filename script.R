library(tidyverse)
library(glue)
library(lubridate)
library(jsonlite)
library(stringr)
library(googlesheets4)

update <- .POSIXct(Sys.time(), "GMT")
update <- glue('{format(update, "%a %b %d %Y %X")} GMT')
updated_pt_text <- glue('{day(Sys.time())} de {month(Sys.time(), label = TRUE, abbr = FALSE, locale="pt_PT")} de {year(Sys.time())}')

data_sheets <- read_sheet("https://docs.google.com/spreadsheets/d/1I_9kfSX8Qa8--Zn2g0E8qE5x5oq6cc9kfBKmDJ1pzSc/edit#gid=1", sheet = 'BaseDeDados', skip = 1)


info_trocos <- data_sheets %>%
  select(ID,mapa, Linha, Troço,Quilómetros, Estado, informacao_estado, breve_descricao, descricao,
         preco_contratual, valor_real,prazo_inicio_construcao, prazo_previsto_fim_construcao, prazo_fim_obra,
         data_projecto, data_inicio_obra, data_abertura_linha,
         avaliacao_prazos, empreiteiro,
         foto1, foto2, foto3, foto4, foto5, foto6, autores_fotos)

#Calcular atrasos
total_atrasos <- info_trocos %>%
  mutate(date_t = Sys.Date()) %>%
  mutate(date_fim_fake = coalesce(data_abertura_linha, date_t)) %>%
  mutate(atraso = difftime(date_fim_fake,prazo_fim_obra))

total_atrasos <- sum(total_atrasos$atraso, na.rm = T)
total_atrasos <- as.numeric(total_atrasos)

#Calcular gasoleo
start <- 18250000
start_date <- ymd("2023/03/06")
days <- difftime(Sys.Date(), start_date)
days <- as.numeric(days)
sum <- days * 14237

#total gasto
total_gasto <- read_sheet("https://docs.google.com/spreadsheets/d/1I_9kfSX8Qa8--Zn2g0E8qE5x5oq6cc9kfBKmDJ1pzSc/edit#gid=1", sheet = 'TotalGasto', skip = 0) %>%
  filter(data_update == max(data_update))




data <- list(
  update = list(
    update_number = update,
    update_text = updated_pt_text
  ),
  big_numbers = list(
    valor_gasto = 9999999,
    t_atrasos = total_atrasos,
    t_litros = start + sum,
  ),
  info_trocos = info_trocos
)

data <- data %>% toJSON(pretty = FALSE, auto_unbox = TRUE, na = "null")
write(data, "data.json")

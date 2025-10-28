## Carregando o banco de dados
data_set <- readxl::read_xlsx("data-raw/Dados Climaticos TCC Neucy - Organizados.xlsx") |>
  janitor::clean_names() |>
  dplyr::rename(data = dia) |>
  dplyr::mutate(
    dia = lubridate::day(data),
    mes = lubridate::month(data),
    ano = lubridate::year(data),
    dia_juliano = lubridate::yday(data)
  ) |>
    dplyr::relocate(data, ano, mes, dia, dia_juliano) |>
  dplyr::group_by(ano, mes) |>
  dplyr::mutate(
    preci_acumulada = cumsum(precipitacao)
  ) |>
  dplyr::ungroup()

# Resumo rápido
dplyr::glimpse(data_set)

# Salvando os dados
readr::write_rds(data_set,"data/dados-climaticos.rds")

## Carregando o banco de dados
data_set_2 <- readxl::read_xlsx("data-raw/Dados TCC Neucy - Organizados.xlsx") |>
  janitor::clean_names() |>
  dplyr::mutate(
    epoca = forcats::as_factor(epoca)
  )
  # dplyr::rename(data = dia) |>
  # dplyr::mutate(
  #   dia = lubridate::day(data),
  #   mes = lubridate::month(data),
  #   ano = lubridate::year(data),
  #   dia_juliano = lubridate::yday(data)
  # ) |>
  # dplyr::relocate(data, ano, mes, dia, dia_juliano) |>
  # dplyr::group_by(ano, mes) |>
  # dplyr::mutate(
  #   preci_acumulada = cumsum(precipitacao)
  # ) |>
  # dplyr::ungroup()

# Resumo rápido
dplyr::glimpse(data_set_2)

# Salvando os dados
readr::write_rds(data_set_2,"data/dados-tcc-neucy.rds")

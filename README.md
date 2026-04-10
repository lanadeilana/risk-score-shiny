# Risk Score — Shiny App

Aplicação Shiny para scoring de risco de abandono em plataformas educacionais, desenvolvida como projeto final de pós-graduação em Data Science.

## Sobre

O app calcula um score de risco por usuário com base em features comportamentais da primeira semana de uso, permitindo priorizar ações de retenção antes que o abandono aconteça.

## Tecnologias

- R + Shiny
- tidyverse, lubridate, DT, scales

## Como rodar

1. Clone o repositório
2. Adicione os arquivos de dados na mesma pasta (não incluídos por confidencialidade)
3. Abra `app_ajustado_VF.R` no RStudio e clique em **Run App**

## Dependências

```r
install.packages(c("shiny", "tidyverse", "lubridate", "scales", "DT"))
```

## Dados

Os dados utilizados não estão disponíveis neste repositório por questões de confidencialidade.

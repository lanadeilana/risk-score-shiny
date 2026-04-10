# AprendiZAP — Score de Risco de Abandono

Aplicação Shiny desenvolvida como projeto final do **PADS 2026 (Programa Avançado em Data Science e Decisão — Insper)**, em parceria com a **Fundação 1Bi**.

## Sobre o projeto

O objetivo foi construir um score de risco de churn para professores da plataforma AprendiZAP, permitindo priorizar ações de retenção antes que o abandono aconteça.

### Metodologia

- **Janela de features:** D+0 a D+6 (primeira semana de uso)
- **Janela de target:** D+7 a D+30 (churn = zero eventos no período)
- **Score:** modelo de pesos por features comportamentais (sessões, dias ativos, uso de recursos)
- **Avaliação:** Lift@Top20%, Precision, Recall e análise de sensibilidade dos pesos

### Features utilizadas

| Feature | Descrição |
|---|---|
| `num_sessoes_7d` | Número de sessões na primeira semana |
| `teve_segundo_acesso` | Se o professor retornou após o primeiro acesso |
| `dias_ativos_7d` | Dias distintos com atividade na primeira semana |
| `num_aulas_distintas_7d` | Aulas acessadas na primeira semana |
| `usou_mari_ia` | Se interagiu com a assistente de IA |
| `usou_formacao` | Se acessou trilhas de formação |

### Resultado

O score identificou churners com **lift superior a 1,5× a taxa base** ao focar no Top 20% de maior risco.

## Como rodar

1. Clone o repositório
2. Adicione os arquivos de dados na mesma pasta (não incluídos por confidencialidade):
   - `features_final.csv`
   - `fct_teachers_entries.csv`
   - `dim_teachers.csv`
3. Abra `app_ajustado_VF.R` no RStudio e clique em **Run App**

## Dependências R

```r
install.packages(c("shiny", "tidyverse", "lubridate", "scales", "DT"))
```

## Nota sobre os dados

Os dados utilizados neste projeto são propriedade da **Fundação 1Bi** e não estão disponíveis neste repositório por questões de confidencialidade.

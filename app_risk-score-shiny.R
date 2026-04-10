library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)

cor_azul    <- "#1B2A4A"
cor_laranja <- "#E8603C"
cor_teal    <- "#028090"
cor_bg      <- "#F5F0E8"

# ══════════════════════════════════════════════════════════════════
# CARREGAMENTO — roda uma vez ao iniciar o app
# Coloque app.R, features_final.csv, fct_teachers_entries.csv
# e dim_teachers.csv na mesma pasta
# ══════════════════════════════════════════════════════════════════

DATA_PATH <- dirname(rstudioapi::getSourceEditorContext()$path)

message("Carregando features_final...")
features_final <- read_csv(file.path(DATA_PATH, "features_final.csv"), show_col_types = FALSE)
taxa_base <- mean(features_final$churn, na.rm = TRUE)
message("Features: ", nrow(features_final), " professores | Churn: ", round(taxa_base * 100, 1), "%")

# Heatmap de retencao
message("Calculando retencao por coorte...")
fct_teachers_entries <- read_csv(file.path(DATA_PATH, "fct_teachers_entries.csv"), show_col_types = FALSE)
dim_teachers <- read_csv(file.path(DATA_PATH, "dim_teachers.csv"), show_col_types = FALSE) %>%
  filter(!is.na(currentstage), currentstage != "")

entries <- fct_teachers_entries %>%
  filter(!is.na(data_inicio)) %>%
  semi_join(dim_teachers, by = "unique_id") %>%
  mutate(data_inicio_date = as.Date(data_inicio))

first_session <- entries %>%
  group_by(unique_id) %>%
  summarise(FirstSessionDate = min(data_inicio_date), .groups = "drop")

coorte_trim <- first_session %>%
  mutate(coorte = floor_date(FirstSessionDate, "quarter")) %>%
  group_by(coorte) %>% filter(n() >= 1000) %>% ungroup()

tamanho_coorte <- coorte_trim %>% count(coorte, name = "cohort_size")

retencao_calc <- function(dias) {
  coorte_trim %>%
    left_join(entries %>% select(unique_id, data_inicio_date), by = "unique_id") %>%
    filter(data_inicio_date > FirstSessionDate + dias) %>%
    distinct(coorte, unique_id) %>%
    count(coorte, name = "retidos") %>%
    left_join(tamanho_coorte, by = "coorte") %>%
    mutate(retencao = retidos / cohort_size, janela = paste0(dias, " dias"))
}

retencao <- bind_rows(retencao_calc(30), retencao_calc(60), retencao_calc(90)) %>%
  filter(coorte >= "2022-01-01", coorte < floor_date(Sys.Date() - days(90), "quarter")) %>%
  mutate(janela = factor(janela, levels = c("30 dias", "60 dias", "90 dias")))

# Funcao de score
calcular_score <- function(df, w1=3, w2=2, w3=2, w4=1, w5=1, w6=1) {
  df %>% mutate(
    score_risco =
      if_else(num_sessoes_7d <= 1,         as.integer(w1), 0L) +
      if_else(teve_segundo_acesso == 0,    as.integer(w2), 0L) +
      if_else(dias_ativos_7d <= 1,         as.integer(w3), 0L) +
      if_else(num_aulas_distintas_7d == 0, as.integer(w4), 0L) +
      if_else(usou_mari_ia == 0,           as.integer(w5), 0L) +
      if_else(usou_formacao == 0,          as.integer(w6), 0L),
    faixa_risco = factor(case_when(
      score_risco >= 7 ~ "Alto",
      score_risco >= 3 ~ "Médio",
      TRUE             ~ "Baixo"
    ), levels = c("Alto", "Médio", "Baixo")),
    acao_recomendada = case_when(
      faixa_risco == "Alto"  ~ "Contato ativo + trilha de boas-vindas",
      faixa_risco == "Médio" ~ "Notificação com conteúdo personalizado",
      TRUE                   ~ "Monitoramento padrão"
    )
  )
}

# Helper: calcula lift, ganho e recall dado um df com score
calc_metricas <- function(df_score) {
  curva     <- df_score %>% arrange(desc(score_risco))
  top20_idx <- ceiling(nrow(curva) * 0.20)
  prec      <- mean(curva$churn[1:top20_idx])
  list(
    lift_top20 = prec / taxa_base,
    prec_top20 = prec,
    ganho      = prec - taxa_base,
    recall     = sum(curva$churn[1:top20_idx]) / sum(df_score$churn),
    top20_idx  = top20_idx
  )
}

base_score_global <- calcular_score(features_final)
m_global          <- calc_metricas(base_score_global)
message("App pronto.")

# ══════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════
ui <- fluidPage(
  title = "Risk Score App",
  tags$head(tags$style(HTML(paste0("
    body{background-color:", cor_bg, ";font-family:'Segoe UI',sans-serif;}
    .nav-tabs{border-bottom:2px solid ", cor_azul, ";}
    .nav-tabs>li.active>a{border-top:3px solid ", cor_laranja, ";color:", cor_azul, ";font-weight:bold;background:white;border-bottom:none;}
    .nav-tabs>li>a{color:#555;}
    .btn-primary{background-color:", cor_teal, ";border-color:", cor_teal, ";}
    .btn-primary:hover{background-color:", cor_azul, ";border-color:", cor_azul, ";}
    .value-box{background:white;border-radius:6px;padding:16px 20px;border-left:4px solid ", cor_teal, ";margin-bottom:12px;box-shadow:0 1px 3px rgba(0,0,0,.06);}
    .value-box .vb-value{font-size:2rem;font-weight:700;color:", cor_azul, ";}
    .value-box .vb-label{font-size:.82rem;color:#666;margin-top:4px;}
    .value-box .vb-sub{font-size:.75rem;color:", cor_teal, ";margin-top:3px;font-style:italic;}
    .value-box.danger{border-left-color:", cor_laranja, ";}
    .value-box.danger .vb-value{color:", cor_laranja, ";}
    .value-box.success{border-left-color:#52b788;}
    .value-box.highlight{border-left-color:", cor_azul, ";}
    h4.section-title{color:", cor_azul, ";font-size:1rem;font-weight:600;border-bottom:1px solid #ddd;padding-bottom:6px;margin-top:24px;margin-bottom:16px;}
    .sidebar-panel{background:white;border-radius:6px;padding:20px;box-shadow:0 1px 3px rgba(0,0,0,.06);}
    .header-bar{background-color:", cor_azul, ";color:white;padding:18px 28px;border-radius:6px;margin-bottom:24px;}
    .header-bar h3{margin:0;font-size:1.25rem;font-weight:600;}
    .header-bar p{margin:6px 0 0;font-size:.82rem;opacity:.75;}
  ")))),

  div(class="header-bar",
    h3("Score de Risco de Abandono"),
    p("PADS 2026 · Ilana García · Izabelle Silva · Julia Navarro · Lívia Bertoni")
  ),

  tabsetPanel(id="tabs",

    # ── ABA 1: DASHBOARD ──────────────────────────────────────────────────────
    tabPanel("Dashboard", br(),

      # Linha 1: volume
      fluidRow(
        column(3, div(class="value-box danger",
          div(class="vb-value", percent(taxa_base, accuracy=0.1)),
          div(class="vb-label", "Taxa base de churn (D+7 a D+30)"))),
        column(3, div(class="value-box",
          div(class="vb-value", comma(nrow(features_final))),
          div(class="vb-label", "Professores analisados"))),
        column(3, div(class="value-box danger",
          div(class="vb-value", comma(sum(base_score_global$churn, na.rm=TRUE))),
          div(class="vb-label", "Professores em churn"))),
        column(3, div(class="value-box",
          div(class="vb-value", comma(sum(base_score_global$faixa_risco=="Alto"))),
          div(class="vb-label", "Professores em risco alto")))
      ),

      # Linha 2: métricas de negócio (NOVAS)
      fluidRow(
        column(3, div(class="value-box highlight",
          div(class="vb-value", paste0(round(m_global$lift_top20, 2), "×")),
          div(class="vb-label", "Lift@Top 20%"),
          div(class="vb-sub",  "vs. abordagem aleatória"))),
        column(3, div(class="value-box highlight",
          div(class="vb-value", percent(m_global$prec_top20, accuracy=0.1)),
          div(class="vb-label", "Precision@Top 20%"),
          div(class="vb-sub",  "churners no grupo priorizado"))),
        column(3, div(class="value-box success",
          div(class="vb-value", paste0("+", percent(m_global$ganho, accuracy=0.1))),
          div(class="vb-label", "Ganho incremental"),
          div(class="vb-sub",  "pp acima do aleatório"))),
        column(3, div(class="value-box success",
          div(class="vb-value", percent(m_global$recall, accuracy=0.1)),
          div(class="vb-label", "Recall@Top 20%"),
          div(class="vb-sub",  "dos churners capturados")))
      ),

      fluidRow(
        column(6, h4(class="section-title","Retenção por Coorte Trimestral"),
               plotOutput("plot_heatmap", height="420px")),
        column(6, h4(class="section-title","Curva de Precisão Acumulada"),
               plotOutput("plot_curva", height="420px"))
      ),

      # Gráficos de negócio (NOVOS)
      fluidRow(
        column(6, h4(class="section-title","Ganho Incremental vs. Abordagem Aleatória"),
               plotOutput("plot_ganho", height="280px")),
        column(6, h4(class="section-title","Cobertura do Churn — Recall@Top 20%"),
               plotOutput("plot_recall", height="280px"))
      )
    ),

    # ── ABA 2: TABELA ─────────────────────────────────────────────────────────
    tabPanel("Tabela de Priorização", br(),
      fluidRow(
        column(3, div(class="sidebar-panel",
          h4(class="section-title","Filtros"),
          checkboxGroupInput("filtro_faixa","Faixa de risco:",
            choices=c("Alto","Médio","Baixo"), selected=c("Alto","Médio","Baixo")),
          hr(),
          sliderInput("filtro_score","Score mínimo:", min=0, max=10, value=0, step=1),
          hr(),
          downloadButton("exportar_csv","Exportar CSV", class="btn-primary", style="width:100%")
        )),
        column(9,
          h4(class="section-title", textOutput("label_tabela", inline=TRUE)),
          DTOutput("tabela_priorizacao"))
      )
    ),

    # ── ABA 3: ROBUSTEZ ───────────────────────────────────────────────────────
    tabPanel("Robustez", br(),
      fluidRow(
        column(4, div(class="sidebar-panel",
          h4(class="section-title","Ajuste os Pesos"),
          p(style="color:#666;font-size:.85rem;",
            "Varie os pesos e veja o impacto nas métricas em tempo real."),
          hr(),
          sliderInput("w1","Sessão única (base: 3)",  min=1, max=5, value=3),
          sliderInput("w2","Sem 2º acesso (base: 2)", min=1, max=4, value=2),
          sliderInput("w3","Dia único (base: 2)",     min=1, max=4, value=2),
          sliderInput("w4","Sem conteúdo (base: 1)",  min=0, max=3, value=1),
          sliderInput("w5","Sem IA interativa (base: 1)",   min=0, max=3, value=1),
          sliderInput("w6","Sem formação (base: 1)",  min=0, max=3, value=1),
          hr(),
          div(class="value-box success",
            div(class="vb-value", textOutput("lift_atual", inline=TRUE)),
            div(class="vb-label","Lift@Top20% com pesos atuais")),
          div(class="value-box highlight",
            div(class="vb-value", textOutput("ganho_atual", inline=TRUE)),
            div(class="vb-label","Ganho incremental atual")),
          div(class="value-box",
            div(class="vb-value", textOutput("recall_atual", inline=TRUE)),
            div(class="vb-label","Recall@Top20% atual"))
        )),
        column(8,
          h4(class="section-title","Sensibilidade ±1 Ponto por Peso"),
          plotOutput("plot_sensibilidade", height="420px"), br(),
          h4(class="section-title","Taxa de Churn por Faixa de Risco"),
          plotOutput("plot_faixas", height="300px"))
      )
    ),

    # ── ABA 4: SOBRE ──────────────────────────────────────────────────────────
    tabPanel("Sobre", br(),
      fluidRow(column(8, offset=2,
        div(style="background:white;border-radius:6px;padding:32px;box-shadow:0 1px 3px rgba(0,0,0,.06);",
          h4(class="section-title","Definições de Negócio"),
          tags$ul(
            tags$li(strong("Churn:"), " sem sessão entre D+7 e D+30 após o primeiro acesso"),
            tags$li(strong("Features:"), " comportamento em D+0 a D+6"),
            tags$li(strong("Elegibilidade:"), " mínimo 60 dias de histórico"),
            tags$li(strong("Âncora:"), " primeiro acesso real, não data de cadastro")
          ),
          h4(class="section-title","Métricas em linguagem de negócio"),
          tags$ul(
            tags$li(strong("Lift@Top20%:"), " o score é X× melhor que selecionar professores aleatoriamente"),
            tags$li(strong("Precision@Top20%:"), " de cada 100 professores priorizados, X% são churners reais"),
            tags$li(strong("Ganho incremental:"), " focar no Top 20% captura X pp a mais que uma abordagem aleatória"),
            tags$li(strong("Recall@Top20%:"), " X% de todos os churners da base estão no grupo priorizado")
          ),
          h4(class="section-title","Critérios do Score"),
          tags$table(style="width:100%;border-collapse:collapse;",
            tags$tr(style=paste0("background:",cor_azul,";color:white;"),
              tags$th(style="padding:8px;","Critério"),
              tags$th(style="padding:8px;text-align:center;","Peso"),
              tags$th(style="padding:8px;","Hipótese")),
            tags$tr(tags$td(style="padding:8px;","Apenas 1 sessão na semana"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","3"),
                    tags$td(style="padding:8px;","H1 — acesso pontual")),
            tags$tr(style="background:#f9f9f9;",
                    tags$td(style="padding:8px;","Sem segundo acesso"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","2"),
                    tags$td(style="padding:8px;","H2 — sem recorrência")),
            tags$tr(tags$td(style="padding:8px;","Menos de 2 dias ativos"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","2"),
                    tags$td(style="padding:8px;","H2 — concentração temporal")),
            tags$tr(style="background:#f9f9f9;",
                    tags$td(style="padding:8px;","Nenhuma aula distinta"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","1"),
                    tags$td(style="padding:8px;","H2 — sem exploração")),
            tags$tr(tags$td(style="padding:8px;","Sem uso da IA interativa"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","1"),
                    tags$td(style="padding:8px;","H3 — sem recurso interativo")),
            tags$tr(style="background:#f9f9f9;",
                    tags$td(style="padding:8px;","Sem uso de formação"),
                    tags$td(style="padding:8px;text-align:center;font-weight:bold;","1"),
                    tags$td(style="padding:8px;","H3 — sem adesão à trilha"))
          ),
          h4(class="section-title","Como usar"),
          tags$ol(
            tags$li("Atualize o features_final.csv com dados novos"),
            tags$li("Reabra o app no RStudio"),
            tags$li("Filtre por 'Alto' na aba Tabela de Priorização"),
            tags$li("Exporte o CSV e encaminhe para o time de CS")
          )
        )
      ))
    )
  )
)

# ══════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  base_reativa <- reactive({
    calcular_score(features_final,
                   w1=input$w1, w2=input$w2, w3=input$w3,
                   w4=input$w4, w5=input$w5, w6=input$w6)
  })

  metricas_reativas <- reactive({
    calc_metricas(base_reativa())
  })

  tabela_filtrada <- reactive({
    base_score_global %>%
      filter(faixa_risco %in% input$filtro_faixa, score_risco >= input$filtro_score) %>%
      arrange(desc(score_risco)) %>%
      select(
        Professor         = unique_id,
        `Sessões D0-D6`   = num_sessoes_7d,
        `Dias ativos`     = dias_ativos_7d,
        `2o acesso`       = teve_segundo_acesso,
        `Aulas distintas` = num_aulas_distintas_7d,
        `IA interativa`         = usou_mari_ia,
        `Formacao`        = usou_formacao,
        Score             = score_risco,
        Risco             = faixa_risco,
        Acao              = acao_recomendada
      )
  })

  output$label_tabela <- renderText({ paste0(comma(nrow(tabela_filtrada())), " professores") })

  output$tabela_priorizacao <- renderDT({
    df <- tabela_filtrada()
    datatable(df,
      options=list(pageLength=15, scrollX=TRUE,
        language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json")),
      rownames=FALSE, filter="top") %>%
      formatStyle("Risco",
        backgroundColor=styleEqual(c("Alto","Médio","Baixo"), c("#FFF0F0","#FFFBEA","#F0FFF4")),
        fontWeight="bold") %>%
      formatStyle("Score",
        background=styleColorBar(c(0,10), cor_teal),
        backgroundSize="98% 60%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })

  output$exportar_csv <- downloadHandler(
    filename=function() paste0("aprendizap_risco_", Sys.Date(), ".csv"),
    content=function(file) write_csv(tabela_filtrada(), file)
  )

  # ── Heatmap retenção ────────────────────────────────────────────────────────
  output$plot_heatmap <- renderPlot({
    retencao %>% mutate(coorte_num=as.numeric(coorte)) %>%
      ggplot(aes(x=janela, y=coorte_num, fill=retencao)) +
      geom_tile(color="white", linewidth=0.5) +
      geom_text(aes(label=percent(retencao, accuracy=1)), size=3, color="white", fontface="bold") +
      scale_y_reverse(
        breaks=as.numeric(seq(as.Date("2022-01-01"), as.Date("2025-10-01"), by="quarter")),
        labels=format(seq(as.Date("2022-01-01"), as.Date("2025-10-01"), by="quarter"), "%b %Y")) +
      scale_fill_gradientn(colours=c("#B56576","#E76F51","#F4A261","#FFCAD4","#FFE5EC"),
        trans="sqrt", labels=percent, name="Retenção") +
      labs(title="Retenção por coorte trimestral",
           subtitle="% que retornou após 30, 60 e 90 dias do primeiro acesso", x=NULL, y=NULL) +
      theme_minimal(base_size=12) +
      theme(panel.grid=element_blank(), legend.position="right",
            plot.subtitle=element_text(color="gray40", size=10))
  })

  # ── Curva de precisão (com recall anotado) ───────────────────────────────────
  output$plot_curva <- renderPlot({
    m     <- m_global
    curva <- base_score_global %>% arrange(desc(score_risco)) %>%
      mutate(rank_pct=row_number()/n()*100, precision=cumsum(churn)/row_number())

    ggplot(curva, aes(x=rank_pct, y=precision)) +
      geom_line(color=cor_azul, linewidth=1.2) +
      geom_hline(yintercept=taxa_base, linetype="dashed", color=cor_laranja, linewidth=0.8) +
      geom_vline(xintercept=20, linetype="dotted", color=cor_teal, linewidth=0.8) +
      annotate("text", x=22, y=min(m$prec_top20+0.05, 0.95),
               label=paste0("Top 20%\nLift: ", round(m$lift_top20,2), "×\nRecall: ", percent(m$recall,0.1)),
               color=cor_teal, hjust=0, size=3.5) +
      annotate("text", x=70, y=taxa_base+0.03,
               label=paste0("Taxa base: ", percent(taxa_base, accuracy=0.1)),
               color=cor_laranja, size=3.5) +
      scale_x_continuous(labels=function(x) paste0(x,"%")) +
      scale_y_continuous(labels=percent_format(), limits=c(0,1)) +
      labs(title="Curva de Precisão Acumulada",
           subtitle="Proporção de churners capturados ao priorizar pelo score",
           x="% de professores (score decrescente)", y="Precisão acumulada") +
      theme_minimal(base_size=12) +
      theme(plot.subtitle=element_text(color="gray40", size=10))
  })

  # ── Ganho incremental (NOVO) ─────────────────────────────────────────────────
  output$plot_ganho <- renderPlot({
    m <- m_global
    tibble(
      label = c("Aleatório\n(taxa base)", "Score\n(Top 20%)"),
      valor = c(taxa_base, m$prec_top20),
      tipo  = c("base", "modelo")
    ) %>%
      ggplot(aes(x=label, y=valor, fill=tipo)) +
      geom_col(width=0.45) +
      geom_text(aes(label=percent(valor, 0.1)), vjust=-0.4, fontface="bold", size=5) +
      annotate("text", x=1.5, y=(taxa_base+m$prec_top20)/2+0.04,
               label=paste0("+", percent(m$ganho, 0.1)),
               color=cor_laranja, fontface="bold", size=5) +
      scale_fill_manual(values=c("base"="#A8C5C9","modelo"=cor_azul)) +
      scale_y_continuous(labels=percent_format(), limits=c(0,1.15)) +
      labs(title="Ganho incremental vs. abordagem aleatória",
           subtitle="Ao focar no Top 20%, identificamos mais churners que sem o score",
           x=NULL, y="Precisão") +
      theme_minimal(base_size=12) +
      theme(legend.position="none", plot.subtitle=element_text(color="gray40", size=10))
  })

  # ── Recall / cobertura (NOVO) ────────────────────────────────────────────────
  output$plot_recall <- renderPlot({
    m              <- m_global
    total_churners <- sum(features_final$churn)
    churners_top20 <- round(m$recall * total_churners)

    tibble(
      grupo = c("Capturados\n(Top 20%)", "Fora do\nradar"),
      n     = c(churners_top20, total_churners - churners_top20),
      pct   = c(m$recall, 1 - m$recall)
    ) %>%
      ggplot(aes(x="", y=pct, fill=grupo)) +
      geom_col(width=0.4) +
      geom_text(aes(label=paste0(percent(pct, 0.1), "\n(", comma(n), " prof.)")),
                position=position_stack(vjust=0.5),
                color="white", fontface="bold", size=4.5) +
      scale_fill_manual(values=c("Capturados\n(Top 20%)"=cor_azul,
                                  "Fora do\nradar"="#A8C5C9")) +
      coord_flip() +
      scale_y_continuous(labels=percent_format()) +
      labs(title="Cobertura do churn — Recall@Top 20%",
           subtitle="Quanto do problema de churn está no grupo priorizado",
           x=NULL, y=NULL, fill=NULL) +
      theme_minimal(base_size=12) +
      theme(axis.text.y=element_blank(), legend.position="right",
            plot.subtitle=element_text(color="gray40", size=10))
  })

  # ── Lift, ganho e recall reativos (aba Robustez) ─────────────────────────────
  output$lift_atual <- renderText({
    paste0(round(metricas_reativas()$lift_top20, 2), "×")
  })

  output$ganho_atual <- renderText({
    paste0("+", percent(metricas_reativas()$ganho, accuracy=0.1))
  })

  output$recall_atual <- renderText({
    percent(metricas_reativas()$recall, accuracy=0.1)
  })

  # ── Sensibilidade ─────────────────────────────────────────────────────────────
  output$plot_sensibilidade <- renderPlot({
    W <- c(input$w1, input$w2, input$w3, input$w4, input$w5, input$w6)
    calc_lift_local <- function(dw, idx) {
      w_tmp <- W; w_tmp[idx] <- w_tmp[idx] + dw
      df_tmp <- calcular_score(features_final, w_tmp[1],w_tmp[2],w_tmp[3],
                               w_tmp[4],w_tmp[5],w_tmp[6]) %>% arrange(desc(score_risco))
      top20 <- ceiling(nrow(df_tmp)*0.20)
      mean(df_tmp$churn[1:top20])/taxa_base
    }
    labels_peso <- c("sessao unica","sem 2o acesso","dia unico","sem conteudo","sem IA interativa","sem formacao")
    m_atual <- metricas_reativas()
    variacoes <- tibble(
      variacao   = c("Base", paste0(labels_peso," -1"), paste0(labels_peso," +1")),
      lift_top20 = c(
        m_atual$lift_top20,
        map2_dbl(rep(-1,6), 1:6, calc_lift_local),
        map2_dbl(rep(+1,6), 1:6, calc_lift_local)),
      tipo = c("Base", rep("-1",6), rep("+1",6))
    )
    ggplot(variacoes, aes(x=reorder(variacao,lift_top20), y=lift_top20, fill=tipo)) +
      geom_col(width=0.7) +
      geom_hline(yintercept=1.5, linetype="dashed", color=cor_laranja, linewidth=0.8) +
      geom_text(aes(label=round(lift_top20,2)), hjust=-0.2, size=3.5) +
      coord_flip() +
      scale_fill_manual(values=c("Base"=cor_azul,"-1"="#E07070","+1"=cor_teal)) +
      scale_y_continuous(limits=c(0, max(variacoes$lift_top20)*1.15)) +
      labs(title="Sensibilidade dos pesos — Lift@Top20%",
           subtitle="Variacao de +/-1 ponto em cada peso individualmente",
           x=NULL, y="Lift vs. taxa base", fill="Variacao",
           caption="Linha tracejada = criterio minimo (1,5x)") +
      theme_minimal(base_size=12) +
      theme(plot.subtitle=element_text(color="gray40",size=10))
  })

  # ── Churn por faixa (aba Robustez) ───────────────────────────────────────────
  output$plot_faixas <- renderPlot({
    val <- base_reativa() %>%
      group_by(faixa_risco) %>%
      summarise(taxa_churn=mean(churn), n=n(), .groups="drop")
    ggplot(val, aes(x=faixa_risco, y=taxa_churn, fill=faixa_risco)) +
      geom_col(width=0.5) +
      geom_hline(yintercept=taxa_base, linetype="dashed", color=cor_laranja, linewidth=0.8) +
      geom_text(aes(label=percent(taxa_churn,accuracy=0.1)), vjust=-0.5, fontface="bold", size=4) +
      scale_fill_manual(values=c("Alto"=cor_azul,"Médio"=cor_teal,"Baixo"="#A8C5C9")) +
      scale_y_continuous(labels=percent_format(), limits=c(0,1.1)) +
      labs(title="Taxa de churn por faixa de risco", x=NULL, y="Taxa de churn") +
      theme_minimal(base_size=12) + theme(legend.position="none")
  })
}

shinyApp(ui=ui, server=server)

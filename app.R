library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(sf)
library(DT)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(shinyjs)
library(bsicons)
library(osrm)
library(openrouteservice)

# MUDAR A CHAVE DA API EM ---> ORS_API_KEY

# ==============================================================================
# 1. CARREGAMENTO DE DADOS
# ==============================================================================

dados_mapa <- readxl::read_excel("Detalhes Escolas Quantitativos.xlsx") %>%
  rename(
    NM_REGIONAL = `COORD. REGIONAL`,
    NM_MUNICIPIO = `MUNICÍPIO`,
    CD_ESCOLA = `CÓDIGO ESCOLA`,
    NM_ESCOLA = `ESCOLA`
  ) %>%
  mutate(ESCOLA_BUSCA = paste0(NM_ESCOLA, " (Cód: ", CD_ESCOLA, ")"))

dados_mapa <- dados_mapa %>%
  mutate(
    LATITUDE = case_when(
      CD_ESCOLA == 52106195 ~ -16.561577453050006,
      TRUE ~ LATITUDE
    ),
    LONGITUDE = case_when(
      CD_ESCOLA == 52106195 ~ -49.38875445351771,
      TRUE ~ LONGITUDE
    )
  )

alunos_quantitativo <- readxl::read_excel(
  "Quantitativo Alunos Por Turma.xlsx"
) %>%
  select(
    `COD. ESCOLA`,
    LOCALIZAÇÃO,
    CARACTERÍSTICA,
    TURNO,
    `TIPO DE MEDIAÇÃO PEDAGÓGICA`,
    `ENSINO MODALIDADE`,
    `CÓD. COMPOSIÇÃO`,
    COMPOSIÇÃO,
    `CÓD. TURMA`,
    TURMA,
    SÉRIE,
    `LOCAL DE FUNCIONAMENTO DIFERENCIADO`,
    `QTDE. ALUNOS FREQUENTES`,
    `QTDE. ALUNOS TRANSFERIDOS`,
    `QTDE. INFREQUÊNCIA ESCOLAR`,
    `QTDE. ALUNOS FALECIDOS`
  ) %>%
  rename(CD_ESCOLA = `COD. ESCOLA`) %>%
  mutate(CHAVE_TURMA = paste(CD_ESCOLA, `CÓD. TURMA`))

dados_mapa <- dados_mapa %>% left_join(alunos_quantitativo, by = "CD_ESCOLA")

escola_estrutura <- readxl::read_excel("Escola Estrutura.xlsx") %>%
  select(
    `Código Escola`,
    Característica,
    Convênio,
    `Qtde. de Salas Ativas`,
    `Local de Funcionamento`,
    Logradouro,
    `Natureza de Ocupação`,
    `Gestor(a)`,
    Telefone,
    `E-mail`
  ) %>%
  rename(CD_ESCOLA = `Código Escola`) %>%
  mutate(
    TIPO_DA_ESCOLA = case_when(
      Característica %in%
        c("Escola Militar", "Escola Militar/Integral") ~ "Escola Militar",
      Característica %in%
        c("NAEE", "Escola Especial com Escolarização") ~ "Escola Especial",
      Característica %in% c("Não se aplica") ~ "Escola Padrão",
      Característica %in%
        c(
          "Tempo Integral Misto - 7h e parcial",
          "Tempo Integral - duplo 7h",
          "Tempo Integral - 9h",
          "Tempo Integral - 7h",
          "Escola Tempo Integral"
        ) ~ "Escola Integral"
    )
  )

dados_mapa <- dados_mapa %>% left_join(escola_estrutura, by = "CD_ESCOLA")

conselhos_esc <- readxl::read_excel("Detalhes Conselhos Escolares.xlsx") %>%
  select(`Código escola`, Conselho, Cnpj) %>%
  rename(CD_ESCOLA = `Código escola`)

dados_mapa <- dados_mapa %>% left_join(conselhos_esc, by = "CD_ESCOLA")

# --- RESUMOS PARA O TOOLTIP ---
resumo_turnos <- dados_mapa %>%
  filter(!is.na(TURNO)) %>%
  group_by(CD_ESCOLA, TURNO) %>%
  summarise(
    QTD_TURMAS = n_distinct(CHAVE_TURMA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CD_ESCOLA) %>%
  summarise(
    TURMAS_POR_TURNO = paste(TURNO, QTD_TURMAS, sep = ": ", collapse = " | ")
  )

resumo_series <- dados_mapa %>%
  filter(!is.na(TURNO) & !is.na(SÉRIE)) %>%
  group_by(CD_ESCOLA, TURNO, SÉRIE) %>%
  summarise(
    ALUNOS_SERIE = sum(`QTDE. ALUNOS FREQUENTES`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(TXT_SERIE = paste0(SÉRIE, " (", ALUNOS_SERIE, ")")) %>%
  group_by(CD_ESCOLA, TURNO) %>%
  summarise(
    TXT_TURNO = paste0(
      "<b>",
      first(TURNO),
      "</b>: ",
      paste(TXT_SERIE, collapse = ", ")
    ),
    .groups = "drop"
  ) %>%
  group_by(CD_ESCOLA) %>%
  summarise(MATRICULAS_SERIE_TURNO = paste(TXT_TURNO, collapse = "<br>"))

resumo_composicoes <- dados_mapa %>%
  filter(!is.na(TURNO) & !is.na(COMPOSIÇÃO)) %>%
  group_by(CD_ESCOLA, TURNO, COMPOSIÇÃO) %>%
  summarise(
    QTD_TURMAS_COMP = n_distinct(`CHAVE_TURMA`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TXT_COMP = paste0("• ", COMPOSIÇÃO, " (", QTD_TURMAS_COMP, " turmas)")
  ) %>%
  group_by(CD_ESCOLA, TURNO) %>%
  summarise(
    TXT_TURNO_COMP = paste0(
      "<b style='color:#4f8ef7;'>",
      first(TURNO),
      "</b><br>",
      paste(TXT_COMP, collapse = "<br>")
    ),
    .groups = "drop"
  ) %>%
  group_by(CD_ESCOLA) %>%
  summarise(COMPOSICOES_TURNO = paste(TXT_TURNO_COMP, collapse = "<br><br>"))

resumo_caracteristica <- dados_mapa %>%
  filter(!is.na(CARACTERÍSTICA.y)) %>%
  group_by(CD_ESCOLA, CARACTERÍSTICA.y) %>%
  summarise(
    QTD_TURMAS_CARAC = n_distinct(`CHAVE_TURMA`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TXT_CARAC = paste0(
      "• ",
      CARACTERÍSTICA.y,
      ": ",
      QTD_TURMAS_CARAC,
      " turmas"
    )
  ) %>%
  group_by(CD_ESCOLA) %>%
  summarise(TURMAS_CARACTERISTICA = paste(TXT_CARAC, collapse = "<br>"))

# NOVO: Resumo de Turmas por Modalidade
resumo_modalidades <- dados_mapa %>%
  filter(!is.na(`ENSINO MODALIDADE`)) %>%
  group_by(CD_ESCOLA, `ENSINO MODALIDADE`) %>%
  summarise(
    QTD_TURMAS_MOD = n_distinct(`CHAVE_TURMA`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TXT_MOD = paste0(`ENSINO MODALIDADE`, " (", QTD_TURMAS_MOD, " turmas)")
  ) %>%
  group_by(CD_ESCOLA) %>%
  summarise(TURMAS_MODALIDADE = paste(TXT_MOD, collapse = " | "))

# NOVO: Resumo de Alunos Únicos (Para não inflar números nos joins)
resumo_alunos <- dados_mapa %>%
  distinct(CD_ESCOLA, CHAVE_TURMA, .keep_all = TRUE) %>%
  group_by(CD_ESCOLA) %>%
  summarise(
    ALUNOS_FREQUENTES_TOTAL = sum(`QTDE. ALUNOS FREQUENTES`, na.rm = TRUE),
    ALUNOS_TRANSFERIDOS_TOTAL = sum(`QTDE. ALUNOS TRANSFERIDOS`, na.rm = TRUE),
    ALUNOS_INFREQUENCIA_TOTAL = sum(`QTDE. INFREQUÊNCIA ESCOLAR`, na.rm = TRUE),
    ALUNOS_FALECIDOS_TOTAL = sum(`QTDE. ALUNOS FALECIDOS`, na.rm = TRUE),
    .groups = "drop"
  )

# --- BASE ÚNICA (Para o Mapa) ---
dados_escolas_unicas <- dados_mapa %>%
  group_by(CD_ESCOLA) %>%
  summarise(
    NM_REGIONAL = first(NM_REGIONAL),
    NM_MUNICIPIO = first(NM_MUNICIPIO),
    NM_ESCOLA = first(NM_ESCOLA),
    ESCOLA_BUSCA = first(ESCOLA_BUSCA),
    LATITUDE = first(LATITUDE),
    LONGITUDE = first(LONGITUDE),
    DEP_ADMINISTRATIVA = first(`DEP. ADMINISTRATIVA`),
    CONVENIO = first(Convênio),
    ENDERECO = first(ENDEREÇO),
    LOGRADOURO = first(Logradouro),
    TELEFONE = first(Telefone),
    EMAIL = first(`E-mail`),
    GESTOR = first(`Gestor(a)`),
    TIPO = first(TIPO_DA_ESCOLA),
    NATUREZA_OCUPACAO = first(`Natureza de Ocupação`),
    SALAS_ATIVAS = first(`QTDE. DE SALAS ATIVAS`),
    CAPACIDADE = first(CAPACIDADE),
    TURMAS_EF = first(`QTDE. DE TURMAS ENS. FUNDAMENTAL`),
    TURMAS_EM = first(`QTDE. DE TURMAS ENS. MÉDIO`),
    TURMAS_EJA = first(`QTDE. DE TURMAS EJA`),
    LOCAL_FUNCIONAMENTO = first(`Local de Funcionamento`),
    CONSELHO = first(Conselho),
    CNPJ = first(Cnpj),
    PROF_EFETIVOS = first(`QTDE. DE PROFESSORES EFETIVOS`),
    PROF_TEMP = first(`QTDE. DE PROFESSORES TEMPORÁRIOS`),
    SERV_EFETIVOS = first(`QTDE. DE SERVIDORES ADM. EFETIVOS`),
    SERV_TEMP = first(`QTDE. DE SERVIDORES ADM. TEMPORÁRIOS`),
    .groups = "drop"
  ) %>%
  left_join(resumo_turnos, by = "CD_ESCOLA") %>%
  left_join(resumo_series, by = "CD_ESCOLA") %>%
  left_join(resumo_composicoes, by = "CD_ESCOLA") %>%
  left_join(resumo_caracteristica, by = "CD_ESCOLA") %>%
  left_join(resumo_modalidades, by = "CD_ESCOLA") %>%
  left_join(resumo_alunos, by = "CD_ESCOLA")

# --- LISTAS PARA OS FILTROS ---
lista_turnos <- sort(na.omit(unique(dados_mapa$TURNO)))
lista_modalidades <- sort(na.omit(unique(dados_mapa$`ENSINO MODALIDADE`)))
lista_composicoes <- sort(na.omit(unique(dados_mapa$COMPOSIÇÃO)))
lista_tipos <- sort(na.omit(unique(dados_escolas_unicas$TIPO)))
lista_regionais <- sort(unique(dados_mapa$NM_REGIONAL))

objetos_mapa <- readRDS("dados_shape.rds")
shape_estado <- objetos_mapa$estado %>% st_transform(4326)
shape_munis <- objetos_mapa$municipios %>% st_transform(4326)

tabela_regionais <- dados_mapa %>%
  distinct(NM_MUNICIPIO, NM_REGIONAL) %>%
  mutate(
    chave = stringi::stri_trans_general(toupper(NM_MUNICIPIO), "Latin-ASCII")
  ) %>%
  distinct(chave, .keep_all = TRUE)

shape_munis_enrich <- shape_munis %>%
  mutate(
    chave = stringi::stri_trans_general(toupper(name_muni), "Latin-ASCII")
  ) %>%
  select(-any_of(c("NM_REGIONAL", "NM_MUNICIPIO"))) %>%
  left_join(tabela_regionais, by = "chave")

# ==============================================================================
# 2. CSS REDESENHADO — TEMA PREMIUM GOVERNO/GEO
# ==============================================================================
css_dinamico <- "
@import url('https://fonts.googleapis.com/css2?family=Sora:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500&display=swap');
:root { --speed: 0.25s; --radius-sm: 6px; --radius-md: 10px; --radius-lg: 16px; --radius-xl: 22px; --shadow-sm: 0 1px 3px rgba(0,0,0,.08), 0 1px 2px rgba(0,0,0,.05); --shadow-md: 0 4px 16px rgba(0,0,0,.10); --shadow-lg: 0 12px 40px rgba(0,0,0,.16); }
body.light-mode { --bg-app: #f0f4fa; --bg-sidebar: #ffffff; --bg-card: #ffffff; --bg-input: #f5f7fb; --bg-input-focus: #eef2ff; --border: #dde3ef; --border-focus: #4f8ef7; --text-1: #0d1b3e; --text-2: #4a5578; --text-3: #8994b0; --accent-1: #1a56db; --accent-2: #0ea5e9; --accent-3: #10b981; --accent-warn: #f59e0b; --accent-danger:#ef4444; --accent-purple:#7c3aed; --nav-bg: #0d1b3e; --nav-text: #e2eaff; --nav-active: #4f8ef7; --kpi-1: linear-gradient(135deg, #1a56db 0%, #4f8ef7 100%); --kpi-2: linear-gradient(135deg, #0891b2 0%, #0ea5e9 100%); --kpi-3: linear-gradient(135deg, #059669 0%, #10b981 100%); --sb-track: #e8edf7; --sb-thumb: #bbc5de; --sb-hover: #4f8ef7; }
body.dark-mode { --bg-app: #080f1e; --bg-sidebar: #0e1831; --bg-card: #111d38; --bg-input: #0a1225; --bg-input-focus: #0d1a36; --border: rgba(79,142,247,.18); --border-focus: #4f8ef7; --text-1: #e8eeff; --text-2: #8ba1cc; --text-3: #4a5e84; --accent-1: #4f8ef7; --accent-2: #22d3ee; --accent-3: #34d399; --accent-warn: #fbbf24; --accent-danger:#f87171; --accent-purple:#a78bfa; --nav-bg: #060e1d; --nav-text: #8ba1cc; --nav-active: #4f8ef7; --kpi-1: linear-gradient(135deg, #1a3d7a 0%, #1a56db 100%); --kpi-2: linear-gradient(135deg, #0a3550 0%, #0891b2 100%); --kpi-3: linear-gradient(135deg, #064e3b 0%, #059669 100%); --sb-track: #0a1225; --sb-thumb: #1e3460; --sb-hover: #4f8ef7; }
* { box-sizing: border-box; }
body { background-color: var(--bg-app) !important; color: var(--text-1) !important; font-family: 'Sora', sans-serif !important; font-size: 14px; line-height: 1.6; transition: background-color var(--speed), color var(--speed); -webkit-font-smoothing: antialiased; }
* { scrollbar-width: thin; scrollbar-color: var(--sb-thumb) var(--sb-track); }
*::-webkit-scrollbar { width: 6px; height: 6px; }
*::-webkit-scrollbar-track { background: var(--sb-track); }
*::-webkit-scrollbar-thumb { background: var(--sb-thumb); border-radius: 99px; }
*::-webkit-scrollbar-thumb:hover { background: var(--sb-hover); }
.navbar { background: var(--nav-bg) !important; border-bottom: 1px solid rgba(79,142,247,.2) !important; box-shadow: 0 2px 20px rgba(0,0,0,.3) !important; padding: 0 24px !important; min-height: 56px; backdrop-filter: blur(12px); }
.navbar-brand { color: #ffffff !important; font-family: 'Sora', sans-serif !important; font-weight: 800 !important; font-size: 1.05rem !important; letter-spacing: .5px; display: flex; align-items: center; gap: 10px; }
.navbar-brand::before { content: ''; width: 28px; height: 28px; background: url('logo_estado.png') center/contain no-repeat; border-radius: 7px; flex-shrink: 0; display: inline-block; }
.navbar .nav-link { color: var(--nav-text) !important; font-weight: 500 !important; font-size: .85rem !important; letter-spacing: .3px; padding: 6px 14px !important; border-radius: var(--radius-sm); transition: all .2s; margin: 0 2px; }
.navbar .nav-link:hover { color: #fff !important; background: rgba(79,142,247,.15) !important; }
.navbar .nav-link.active { color: #ffffff !important; background: rgba(79,142,247,.25) !important; border-bottom: 2px solid var(--accent-2) !important; }
.bslib-sidebar-layout > .sidebar { background: var(--bg-sidebar) !important; border-right: 1px solid var(--border) !important; padding: 20px 16px !important; transition: background var(--speed); }
.sidebar-section-label { font-size: .7rem; font-weight: 700; text-transform: uppercase; letter-spacing: 1.2px; color: var(--text-3) !important; margin: 16px 0 8px; padding: 0 2px; display: flex; align-items: center; gap: 6px; }
.sidebar-section-label::after { content: ''; flex: 1; height: 1px; background: var(--border); }
.control-label, label, .shiny-input-container label { color: var(--text-2) !important; font-size: .78rem !important; font-weight: 600 !important; letter-spacing: .2px; margin-bottom: 4px !important; }
h1, h2, h3, h4, h5, h6 { color: var(--text-1) !important; font-family: 'Sora', sans-serif !important; }
.form-control, .selectize-input, .bootstrap-select .dropdown-toggle, .btn-default { background: var(--bg-input) !important; color: var(--text-1) !important; border: 1.5px solid var(--border) !important; border-radius: var(--radius-md) !important; font-family: 'Sora', sans-serif !important; font-size: .83rem !important; transition: border-color .2s, box-shadow .2s, background .2s; padding: 7px 12px !important; box-shadow: none !important; }
.form-control:focus, .selectize-input.focus, .selectize-input:focus { background: var(--bg-input-focus) !important; border-color: var(--border-focus) !important; box-shadow: 0 0 0 3px rgba(79,142,247,.15) !important; outline: none !important; }
.selectize-dropdown { background: var(--bg-card) !important; border: 1.5px solid var(--border) !important; border-radius: var(--radius-md) !important; box-shadow: var(--shadow-lg) !important; overflow: hidden; }
.selectize-dropdown-content .option { color: var(--text-1) !important; font-size: .83rem !important; padding: 7px 12px !important; transition: background .15s; }
.selectize-dropdown-content .option:hover, .selectize-dropdown-content .option.active { background: rgba(79,142,247,.15) !important; color: var(--accent-1) !important; }
.bootstrap-select .dropdown-menu { background: var(--bg-card) !important; border: 1.5px solid var(--border) !important; border-radius: var(--radius-md) !important; box-shadow: var(--shadow-lg) !important; padding: 4px !important; }
.bootstrap-select .dropdown-item, .bootstrap-select .dropdown-item a, .filter-option-inner-inner { color: var(--text-1) !important; font-size: .83rem !important; }
.bootstrap-select .dropdown-item:hover, .bootstrap-select .dropdown-item.active { background: rgba(79,142,247,.12) !important; color: var(--accent-1) !important; }
.bootstrap-select .dropdown-item .text,
.bootstrap-select .filter-option-inner-inner {white-space: normal !important;word-break: break-word !important;line-height: 1.4 !important;
padding-right: 15px !important; /* Dá um respiro para o ícone de 'check' */}
.bootstrap-select .dropdown-item {display: flex;align-items: center;}
.bootstrap-select .bs-searchbox input { background: var(--bg-input) !important; border: 1.5px solid var(--border) !important; border-radius: var(--radius-sm) !important; color: var(--text-1) !important; }
.btn { font-family: 'Sora', sans-serif !important; font-weight: 600 !important; font-size: .83rem !important; border-radius: var(--radius-md) !important; padding: 8px 16px !important; letter-spacing: .2px; transition: all .2s !important; border: none !important; }
.btn-primary { background: linear-gradient(135deg, #1a56db, #4f8ef7) !important; color: #fff !important; box-shadow: 0 4px 12px rgba(26,86,219,.3) !important; }
.btn-primary:hover { background: linear-gradient(135deg, #1545b8, #3a7ef0) !important; box-shadow: 0 6px 18px rgba(26,86,219,.45) !important; transform: translateY(-1px); }
.btn-secondary { background: var(--bg-input) !important; color: var(--text-2) !important; border: 1.5px solid var(--border) !important; }
.btn-secondary:hover { background: var(--bg-input-focus) !important; color: var(--accent-1) !important; border-color: var(--border-focus) !important; }
.btn:active { transform: translateY(0) !important; }
.bslib-value-box { border: none !important; border-radius: var(--radius-lg) !important; overflow: hidden; box-shadow: var(--shadow-md) !important; transition: transform .2s, box-shadow .2s; position: relative; }
.bslib-value-box:hover { transform: translateY(-2px); box-shadow: var(--shadow-lg) !important; }
.bslib-value-box.bg-primary, .bslib-value-box[class*='primary'] { background: var(--kpi-1) !important; }
.bslib-value-box.bg-info, .bslib-value-box[class*='info'] { background: var(--kpi-2) !important; }
.bslib-value-box.bg-secondary, .bslib-value-box[class*='secondary'] { background: var(--kpi-3) !important; }
.bslib-value-box .value-box-title { color: rgba(255,255,255,.75) !important; font-size: .72rem !important; font-weight: 600 !important; text-transform: uppercase; letter-spacing: .8px; }
.bslib-value-box .value-box-value { color: #ffffff !important; font-size: 1.9rem !important; font-weight: 800 !important; font-family: 'JetBrains Mono', monospace !important; line-height: 1.1; }
.bslib-value-box .value-box-showcase svg, .bslib-value-box .value-box-showcase .bi { fill: rgba(255,255,255,.35) !important; color: rgba(255,255,255,.35) !important; }
.bslib-value-box::after { content: ''; position: absolute; inset: 0; background: url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"60\" height=\"60\"><circle cx=\"30\" cy=\"30\" r=\"28\" fill=\"none\" stroke=\"rgba(255,255,255,.06)\" stroke-width=\"1\"/></svg>') repeat; pointer-events: none; opacity: .6; }
.card, .bslib-card { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-radius: var(--radius-lg) !important; box-shadow: var(--shadow-sm) !important; color: var(--text-1) !important; transition: background var(--speed), border-color var(--speed); }
.card-header { background: transparent !important; border-bottom: 1px solid var(--border) !important; color: var(--text-1) !important; font-weight: 700 !important; font-size: .85rem !important; letter-spacing: .2px; padding: 14px 18px !important; }
.nav-pills .nav-link { color: var(--text-2) !important; background: transparent !important; border-radius: var(--radius-sm) !important; font-size: .82rem !important; font-weight: 600 !important; padding: 6px 14px !important; transition: all .2s; }
.nav-pills .nav-link:hover { color: var(--accent-1) !important; background: rgba(79,142,247,.08) !important; }
.nav-pills .nav-link.active { background: rgba(79,142,247,.18) !important; color: var(--accent-1) !important; box-shadow: 0 0 0 1.5px rgba(79,142,247,.35) !important; }
.dataTables_wrapper { color: var(--text-1) !important; }
.dataTables_wrapper .dataTables_length select, .dataTables_wrapper .dataTables_filter input { background: var(--bg-input) !important; color: var(--text-1) !important; border: 1px solid var(--border) !important; border-radius: var(--radius-sm) !important; }
.dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--text-2) !important; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current { background: var(--accent-1) !important; color: #fff !important; border-radius: var(--radius-sm) !important; border: none !important; }
table.dataTable thead th { background: var(--bg-input) !important; color: var(--text-2) !important; font-size: .76rem !important; font-weight: 700 !important; text-transform: uppercase; letter-spacing: .6px; border-bottom: 2px solid var(--border) !important; padding: 10px 12px !important; }
table.dataTable tbody tr { background: transparent !important; color: var(--text-1) !important; }
table.dataTable tbody tr:nth-child(even) { background: rgba(79,142,247,.04) !important; }
table.dataTable tbody tr:hover { background: rgba(79,142,247,.1) !important; }
table.dataTable td { padding: 9px 12px !important; font-size: .83rem !important; border-bottom: 1px solid var(--border) !important; }
.dark-mode-toggle-box { background: var(--bg-input) !important; border: 1.5px solid var(--border) !important; border-radius: var(--radius-md) !important; padding: 10px 14px !important; display: flex; align-items: center; justify-content: space-between; margin-top: 4px; }
.dark-mode-toggle-box .control-label { font-size: .8rem !important; color: var(--text-2) !important; margin: 0 !important; }
.material-switch > input[type='checkbox']:checked + label::before { background: linear-gradient(135deg, #1a56db, #4f8ef7) !important; }
.btn-group-justified .btn, .btn-group .btn[class*='btn-primary'] { background: linear-gradient(135deg, #1a56db, #4f8ef7) !important; border: none !important; }
.btn-group .btn-primary:not(.active) { background: #2d6fd4 !important; color: #ffffff !important; border-color: #2d6fd4 !important; }
hr.sidebar-hr { border: none !important; height: 1px !important; background: var(--border) !important; margin: 14px 0 !important; opacity: 1 !important; }
.leaflet-container { border-radius: var(--radius-lg) !important; font-family: 'Sora', sans-serif !important; }
.leaflet-popup-content-wrapper { border-radius: var(--radius-md) !important; box-shadow: var(--shadow-lg) !important; padding: 0 !important; overflow: hidden; border: 1px solid #dde3ef; }
.leaflet-popup-content { margin: 0 !important; }
.leaflet-popup-tip { display: none !important; }
.leaflet-control-zoom a { background: var(--bg-card) !important; color: var(--text-1) !important; border-color: var(--border) !important; }
.shiny-notification { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-radius: var(--radius-md) !important; color: var(--text-1) !important; box-shadow: var(--shadow-lg) !important; font-family: 'Sora', sans-serif !important; }
.nav-item-info { color: var(--nav-text) !important; font-size: .78rem !important; padding: 0 8px; opacity: .7; display: flex; align-items: center; gap: 5px; }
.leaflet-tooltip { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-radius: var(--radius-sm) !important; color: var(--text-1) !important; font-family: 'Sora', sans-serif !important; font-size: .79rem !important; font-weight: 600 !important; box-shadow: var(--shadow-md) !important; }
@keyframes pulse-ring { 0% { transform: scale(.9); opacity: .6; } 70% { transform: scale(1.4); opacity: 0; } 100% { transform: scale(1.4); opacity: 0; } }
@media (max-width: 768px) { .bslib-value-box .value-box-value { font-size: 1.4rem !important; } }..bootstrap-select.dropup .dropdown-menu,
.bootstrap-select .dropdown-menu {
    top: 100% !important;
    bottom: auto !important;
    margin-top: 6px !important;
    margin-bottom: 0 !important;
    transform: none !important;
}
.bootstrap-select .bs-searchbox,
.bootstrap-select .bs-actionsbox {
    background: var(--bg-card) !important;
    padding-bottom: 6px;
}.bootstrap-select.dropup .dropdown-toggle::after,
.bootstrap-select .dropdown-toggle::after {
    border-top: .3em solid !important;
    border-right: .3em solid transparent !important;
    border-bottom: 0 !important;
    border-left: .3em solid transparent !important;
    transform: rotate(0deg) !important;
    transition: transform 0.2s ease-in-out !important;
}.bootstrap-select .dropdown-toggle[aria-expanded=true]::after {
    transform: rotate(180deg) !important;
}
"

# ==============================================================================
# 3. UI
# ==============================================================================
ui <- page_navbar(
  title = div(
    style = "font-family: 'Sora', sans-serif; font-weight: 800; letter-spacing: .5px; font-size: 1rem;",
    "SEDUC GO",
    tags$span(
      style = "font-weight: 300; opacity: .6; margin-left: 6px;",
      "/ Logística e Dados"
    )
  ),
  theme = bs_theme(
    version = 5,
    primary = "#1a56db",
    base_font = font_google("Sora"),
    heading_font = font_google("Sora"),
    code_font = font_google("JetBrains Mono")
  ),
  id = "abas_principais",
  bg = "#0d1b3e",

  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"
    ),
    tags$style(HTML(css_dinamico)),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('toggle_theme_js', function(mode) {
        if (mode === 'dark') {
          document.body.classList.remove('light-mode');
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
          document.body.classList.add('light-mode');
        }
      });
      $(document).ready(function() {
        document.body.classList.add('light-mode');
      });
    "
    ))
  ),
  useShinyjs(),

  # ============================================================
  # ABA 1: EXPLORADOR DE ESCOLAS
  # ============================================================
  nav_panel(
    "Explorador de Escolas",
    icon = icon("map-location-dot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340, # <--- AQUI VOCÊ CONTROLA A LARGURA DA BARRA!

        tags$div(
          class = "sidebar-section-label",
          tags$i(class = "bi bi-geo-alt-fill"),
          "Filtros Geográficos"
        ),
        pickerInput(
          "exp_regional",
          "Regional",
          choices = lista_regionais,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Todas as Regionais",
            `dropupAuto` = FALSE
          )
        ),
        pickerInput(
          "exp_municipio",
          "Município",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Todos os Municípios",
            `dropupAuto` = FALSE
          )
        ),
        selectizeInput(
          "exp_escola",
          "Buscar Escola",
          choices = NULL,
          options = list(placeholder = "Nome ou código da escola...")
        ),

        tags$div(
          class = "sidebar-section-label",
          tags$i(class = "bi bi-mortarboard-fill"),
          "Filtros Pedagógicos"
        ),
        pickerInput(
          "exp_tipo",
          "Tipo de Escola",
          choices = lista_tipos,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `none-selected-text` = "Todos os Tipos",
            `dropupAuto` = FALSE,
            `container` = "body",
            `size` = 4
          )
        ),
        pickerInput(
          "exp_turno",
          "Turno",
          choices = lista_turnos,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `none-selected-text` = "Todos os Turnos",
            `dropupAuto` = FALSE,
            `container` = "body",
            `size` = 4
          )
        ),
        pickerInput(
          "exp_modalidade",
          "Modalidade",
          choices = lista_modalidades,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `none-selected-text` = "Todas as Modalidades",
            `dropupAuto` = FALSE,
            `size` = 4
          )
        ),
        pickerInput(
          "exp_composicao",
          "Composição de Turma",
          choices = lista_composicoes,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Todas",
            `dropupAuto` = FALSE,
            `size` = 1,
            `container` = "body"
          )
        ),

        actionButton(
          "btn_limpar_exp",
          "Limpar Filtros",
          icon = icon("eraser"),
          class = "btn-secondary w-100",
          style = "margin-top: 10px;"
        ),

        tags$div(
          class = "sidebar-section-label",
          style = "margin-top: 18px;",
          tags$i(class = "bi bi-palette"),
          "Aparência"
        ),
        div(
          class = "dark-mode-toggle-box",
          materialSwitch(
            inputId = "dark_mode_1",
            label = "Modo Escuro",
            value = FALSE,
            status = "primary",
            right = TRUE,
            width = "100%"
          )
        )
      ),

      div(
        style = "padding: 12px; height: calc(100vh - 80px); overflow-y: hidden; display: flex; flex-direction: column; gap: 12px;",
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "Escolas Encontradas",
            value = textOutput("kpi_exp_escolas"),
            showcase = bsicons::bs_icon("building", size = "2.2rem"),
            theme = "primary"
          ),
          value_box(
            title = "Turmas Filtradas",
            value = textOutput("kpi_exp_turmas"),
            showcase = bsicons::bs_icon("people-fill", size = "2.2rem"),
            theme = "info"
          ),
          value_box(
            title = "Alunos Frequentes",
            value = textOutput("kpi_exp_alunos"),
            showcase = bsicons::bs_icon("mortarboard-fill", size = "2.2rem"),
            theme = "secondary"
          )
        ),
        div(
          style = "flex-grow: 1; border-radius: 16px; overflow: hidden; border: 1px solid var(--border); box-shadow: var(--shadow-md);",
          leafletOutput("mapa_explorador", height = "100%")
        )
      )
    )
  ),

  # ============================================================
  # ABA 2: ROTEIRIZADOR
  # ============================================================
  nav_panel(
    "Roteirizador Escolar",
    icon = icon("route"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340, # <--- AQUI VOCÊ CONTROLA A LARGURA DA BARRA!

        tags$div(
          class = "sidebar-section-label",
          tags$i(class = "bi bi-funnel-fill"),
          "1. Filtrar Lista"
        ),
        pickerInput(
          "rot_regional",
          "Regional",
          choices = lista_regionais,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Todas as Regionais"
          )
        ),
        pickerInput(
          "rot_municipio",
          "Município",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `none-selected-text` = "Todos os Municípios"
          )
        ),

        tags$div(
          class = "sidebar-section-label",
          tags$i(class = "bi bi-signpost-split-fill"),
          "2. Configurar Rota"
        ),
        selectizeInput(
          "rot_origem",
          "Escola de Origem",
          choices = c("", sort(dados_escolas_unicas$ESCOLA_BUSCA)),
          options = list(placeholder = "Selecione a origem...")
        ),
        selectizeInput(
          "rot_destino",
          "Escola de Destino",
          choices = c("", sort(dados_escolas_unicas$ESCOLA_BUSCA)),
          options = list(placeholder = "Selecione o destino...")
        ),
        radioGroupButtons(
          "rot_modo",
          "Modo de Transporte",
          choices = c("🚗 Carro" = "car", "🚶 A Pé" = "foot"),
          status = "primary",
          justified = TRUE
        ),

        actionButton(
          "btn_calcular_rot",
          "Traçar Rota",
          icon = icon("route"),
          class = "btn-primary w-100",
          style = "margin-top: 12px; font-size: .92rem; padding: 10px !important;"
        ),
        actionButton(
          "btn_limpar_rot",
          "Limpar Rota",
          icon = icon("eraser"),
          class = "btn-secondary w-100",
          style = "margin-top: 8px;"
        ),

        tags$hr(class = "sidebar-hr"),
        tags$div(
          class = "sidebar-section-label",
          tags$i(class = "bi bi-radar"),
          "3. Escolas no Raio"
        ),
        selectizeInput(
          "rot_escola_base",
          "Escola de Referência",
          choices = c("", sort(dados_escolas_unicas$ESCOLA_BUSCA)),
          options = list(placeholder = "Selecione a escola base...")
        ),
        sliderInput(
          "rot_raio",
          "Raio de Busca (km)",
          min = 1,
          max = 50,
          value = 5,
          step = 1,
          post = " km"
        ),
        actionButton(
          "btn_buscar_raio",
          "Buscar no Raio",
          icon = icon("search"),
          class = "btn-primary w-100",
          style = "margin-top: 8px; font-size: .92rem;"
        ),

        tags$div(
          class = "sidebar-section-label",
          style = "margin-top: 18px;",
          tags$i(class = "bi bi-palette"),
          "Aparência"
        ),
        div(
          class = "dark-mode-toggle-box",
          materialSwitch(
            inputId = "dark_mode_2",
            label = "Modo Escuro",
            value = FALSE,
            status = "primary",
            right = TRUE,
            width = "100%"
          )
        )
      ), # fim da sidebar

      div(
        style = "padding: 12px; display: flex; flex-direction: column; gap: 12px;",
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "Distância Total",
            value = textOutput("kpi_rot_dist"),
            showcase = bsicons::bs_icon("signpost-split-fill", size = "2.2rem"),
            theme = "primary"
          ),
          value_box(
            title = "Tempo Estimado",
            value = textOutput("kpi_rot_temp"),
            showcase = bsicons::bs_icon("stopwatch-fill", size = "2.2rem"),
            theme = "info"
          ),
          value_box(
            title = "Modo de Transporte",
            value = textOutput("kpi_rot_modo"),
            showcase = bsicons::bs_icon("car-front-fill", size = "2.2rem"),
            theme = "secondary"
          )
        ),
        div(
          style = "border: 1px solid var(--border); border-radius: var(--radius-lg); overflow: hidden; box-shadow: var(--shadow-md);",
          navset_card_pill(
            nav_panel(
              "Mapa",
              icon = icon("map"),
              leafletOutput("mapa_roteirizador", height = "680px")
            ),
            nav_panel(
              "Histórico de Rotas",
              icon = icon("table"),
              div(
                style = "padding: 16px; height: 680px; overflow-y: auto;",
                DTOutput("tabela_rotas")
              )
            )
          )
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$span(
      style = "color: rgba(255,255,255,.45); font-size: .78rem; padding: 0 8px; display: flex; align-items: center; gap: 5px; cursor: default;",
      tags$i(class = "bi bi-bar-chart-line-fill"),
      "Coord. de Estatística e Dados"
    )
  )
)

# ==============================================================================
# 4. SERVER
# ==============================================================================
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  gerar_tooltip <- function(df) {
    sprintf(
      "<div style='font-family: Sora, sans-serif; font-size: 12px; min-width: 420px; max-width: 520px; max-height: 420px; overflow-y: auto; padding-right: 8px;'>
         <div style='background: linear-gradient(135deg,#1a56db,#0ea5e9); color:#fff; padding:12px 14px; margin:-0px; border-radius:0;'>
           <div style='font-weight: 800; font-size: 13.5px; line-height:1.3;'>%s</div>
           <div style='font-size: 10.5px; opacity:.8; margin-top:2px; font-family: JetBrains Mono, monospace;'>Cód: %s</div>
         </div>
         <div style='padding: 12px 14px; display: grid; grid-template-columns: 1fr; gap: 5px; line-height: 1.5; color:#1e293b;'>
           <div><b>Regional:</b> %s &nbsp;|&nbsp; <b>Município:</b> %s</div>
           <div><b>Endereço:</b> %s</div>
           <div><b>Gestor(a):</b> %s &nbsp;|&nbsp; <b>Tel:</b> %s</div>
           <div><b>Natureza/Local:</b> %s — %s</div>

           <hr style='margin:6px 0; border-color:#e2e8f0;'>
           <div style='font-weight:700; font-size:11px; text-transform:uppercase; letter-spacing:.6px; color:#64748b;'>Recursos Humanos</div>
           <div style='display:grid; grid-template-columns:1fr 1fr; gap:6px; font-size:11px; background:#f0f4fa; padding:8px; border-radius:6px;'>
             <div>👨‍🏫 <b>Professores</b><br>Efetivos: <b>%s</b> &nbsp; Temp: <b>%s</b></div>
             <div>👔 <b>Servidores</b><br>Efetivos: <b>%s</b> &nbsp; Temp: <b>%s</b></div>
           </div>

           <hr style='margin:6px 0; border-color:#e2e8f0;'>
           <div style='font-weight:700; font-size:11px; text-transform:uppercase; letter-spacing:.6px; color:#64748b;'>Informações Pedagógicas</div>
           <div style='background:#f0f4fa; padding:8px; border-radius:6px;'>
             <div><b>Tipo:</b> %s &nbsp;|&nbsp; <b>Convênio:</b> %s</div>
             <div><b>Capacidade:</b> %s vagas &nbsp;|&nbsp; <b>Salas Ativas:</b> %s</div>
             <div style='color: #1a56db; font-weight: bold;'>Alunos Frequentes: %s</div>
             <div><b>Modalidade(s):</b> %s</div>
             <div><b>Turmas:</b> EF: %s &nbsp; EM: %s &nbsp; EJA: %s</div>
             <div><b>Por Turno:</b> %s</div>
           </div>

           <hr style='margin:6px 0; border-color:#e2e8f0;'>
           <div style='font-weight:700; font-size:11px; text-transform:uppercase; letter-spacing:.6px; color:#64748b;'>Turmas por Característica</div>
           <div style='font-size:11px; background:#f8fafc; padding:8px; border-radius:6px; line-height:1.6;'>%s</div>

           <hr style='margin:6px 0; border-color:#e2e8f0;'>
           <div style='font-weight:700; font-size:11px; text-transform:uppercase; letter-spacing:.6px; color:#64748b;'>Composição por Turno</div>
           <div style='font-size:11px; background:#f8fafc; padding:8px; border-radius:6px; line-height:1.6;'>%s</div>
         </div>
       </div>",
      df$NM_ESCOLA,
      df$CD_ESCOLA,
      df$NM_REGIONAL,
      df$NM_MUNICIPIO,
      ifelse(is.na(df$LOGRADOURO), "N/I", df$LOGRADOURO),
      ifelse(is.na(df$GESTOR), "N/I", df$GESTOR),
      ifelse(is.na(df$TELEFONE), "N/I", df$TELEFONE),
      df$NATUREZA_OCUPACAO,
      df$LOCAL_FUNCIONAMENTO,
      ifelse(is.na(df$PROF_EFETIVOS), 0, df$PROF_EFETIVOS),
      ifelse(is.na(df$PROF_TEMP), 0, df$PROF_TEMP),
      ifelse(is.na(df$SERV_EFETIVOS), 0, df$SERV_EFETIVOS),
      ifelse(is.na(df$SERV_TEMP), 0, df$SERV_TEMP),
      ifelse(is.na(df$TIPO), "N/I", df$TIPO),
      ifelse(is.na(df$CONVENIO), "Não Possui", df$CONVENIO),
      ifelse(is.na(df$CAPACIDADE), "N/I", df$CAPACIDADE),
      df$SALAS_ATIVAS,
      ifelse(is.na(df$ALUNOS_FREQUENTES_TOTAL), 0, df$ALUNOS_FREQUENTES_TOTAL),
      ifelse(is.na(df$TURMAS_MODALIDADE), "N/I", df$TURMAS_MODALIDADE),
      ifelse(is.na(df$TURMAS_EF), 0, df$TURMAS_EF),
      ifelse(is.na(df$TURMAS_EM), 0, df$TURMAS_EM),
      ifelse(is.na(df$TURMAS_EJA), 0, df$TURMAS_EJA),
      ifelse(is.na(df$TURMAS_POR_TURNO), "N/I", df$TURMAS_POR_TURNO),
      ifelse(
        is.na(df$TURMAS_CARACTERISTICA),
        "Não informado",
        df$TURMAS_CARACTERISTICA
      ),
      ifelse(is.na(df$COMPOSICOES_TURNO), "Não informado", df$COMPOSICOES_TURNO)
    ) %>%
      lapply(htmltools::HTML)
  }

  # --- SINCRONIZAÇÃO DO MODO ESCURO ---
  observeEvent(input$dark_mode_1, {
    updateMaterialSwitch(session, "dark_mode_2", value = input$dark_mode_1)
  })
  observeEvent(input$dark_mode_2, {
    updateMaterialSwitch(session, "dark_mode_1", value = input$dark_mode_2)
  })

  observeEvent(input$dark_mode_1, {
    modo <- if (input$dark_mode_1) "dark" else "light"
    session$sendCustomMessage("toggle_theme_js", modo)
    tile <- if (modo == "dark") {
      providers$CartoDB.DarkMatter
    } else {
      providers$CartoDB.Positron
    }
    leafletProxy("mapa_explorador") %>% clearTiles() %>% addProviderTiles(tile)
    leafletProxy("mapa_roteirizador") %>%
      clearTiles() %>%
      addProviderTiles(tile)
  })

  # --- MAPAS INICIAIS ---
  output$mapa_explorador <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -53.5, lat1 = -19.5, lng2 = -45.5, lat2 = -12.5) %>%
      htmlwidgets::onRender(
        "
        function(el, x) {
          this.createPane('polygons'); this.getPane('polygons').style.zIndex = 400;
          this.createPane('markers');  this.getPane('markers').style.zIndex  = 600;
        }
      "
      )
  })

  output$mapa_roteirizador <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -53.5, lat1 = -19.5, lng2 = -45.5, lat2 = -12.5) %>%
      htmlwidgets::onRender(
        "
        function(el, x) {
          this.createPane('polygons'); this.getPane('polygons').style.zIndex = 400;
          this.createPane('rotas');    this.getPane('rotas').style.zIndex    = 500;
          this.createPane('markers');  this.getPane('markers').style.zIndex  = 600;
        }
      "
      )
  })
  outputOptions(output, "mapa_roteirizador", suspendWhenHidden = FALSE)

  # ============================================================
  # LÓGICA: EXPLORADOR (ABA 1)
  # ============================================================
  observe({
    munis <- if (
      is.null(input$exp_regional) || length(input$exp_regional) == 0
    ) {
      sort(unique(dados_escolas_unicas$NM_MUNICIPIO))
    } else {
      sort(unique(dados_escolas_unicas$NM_MUNICIPIO[
        dados_escolas_unicas$NM_REGIONAL %in% input$exp_regional
      ]))
    }

    updatePickerInput(
      session,
      "exp_municipio",
      choices = munis,
      selected = input$exp_municipio,
      choicesOpt = list(style = rep("white-space: normal;", length(munis)))
    )
  })

  dados_explorador_filtrados <- reactive({
    df <- dados_mapa
    if (!is.null(input$exp_regional) && length(input$exp_regional) > 0) {
      df <- df %>% filter(NM_REGIONAL %in% input$exp_regional)
    }
    if (!is.null(input$exp_municipio) && length(input$exp_municipio) > 0) {
      df <- df %>% filter(NM_MUNICIPIO %in% input$exp_municipio)
    }
    if (!is.null(input$exp_escola) && input$exp_escola != "") {
      df <- df %>% filter(ESCOLA_BUSCA == input$exp_escola)
    }
    if (!is.null(input$exp_tipo) && length(input$exp_tipo) > 0) {
      df <- df %>% filter(TIPO_DA_ESCOLA %in% input$exp_tipo)
    }
    if (!is.null(input$exp_turno) && length(input$exp_turno) > 0) {
      df <- df %>% filter(TURNO %in% input$exp_turno)
    }
    if (!is.null(input$exp_modalidade) && length(input$exp_modalidade) > 0) {
      df <- df %>% filter(`ENSINO MODALIDADE` %in% input$exp_modalidade)
    }
    if (!is.null(input$exp_composicao) && length(input$exp_composicao) > 0) {
      df <- df %>% filter(COMPOSIÇÃO %in% input$exp_composicao)
    }
    df
  })

  observe({
    df <- dados_explorador_filtrados()
    escolas_validas <- if (nrow(df) > 0) {
      sort(unique(df$ESCOLA_BUSCA))
    } else {
      character(0)
    }
    updateSelectizeInput(
      session,
      "exp_escola",
      choices = c("", escolas_validas),
      selected = input$exp_escola
    )
  })

  # KPIs CORRIGIDOS (Calculados com Distinct para não inflar valores se a base possuir quebras)
  output$kpi_exp_escolas <- renderText({
    df <- dados_explorador_filtrados()
    if (nrow(df) == 0) {
      return("0")
    }
    format(n_distinct(df$CD_ESCOLA), big.mark = ".")
  })

  output$kpi_exp_turmas <- renderText({
    df <- dados_explorador_filtrados()
    if (nrow(df) == 0) {
      return("0")
    }
    format(n_distinct(df$`CHAVE_TURMA`, na.rm = TRUE), big.mark = ".")
  })

  output$kpi_exp_alunos <- renderText({
    df <- dados_explorador_filtrados()
    if (nrow(df) == 0) {
      return("0")
    }
    # Agrupa por turma primeiro, garantindo que alunos de uma mesma turma não sejam somados duplicados!
    alunos_unicos <- df %>% distinct(CHAVE_TURMA, .keep_all = TRUE)
    format(
      sum(alunos_unicos$`QTDE. ALUNOS FREQUENTES`, na.rm = TRUE),
      big.mark = ".",
      decimal.mark = ","
    )
  })

  # Polígonos – Explorador
  observe({
    proxy_exp <- leafletProxy("mapa_explorador") %>% clearGroup("poligonos")

    if (
      (is.null(input$exp_regional) || length(input$exp_regional) == 0) &&
        (is.null(input$exp_municipio) || length(input$exp_municipio) == 0)
    ) {
      proxy_exp %>%
        addPolygons(
          data = shape_estado,
          group = "poligonos",
          fillColor = "#1a56db",
          color = "#0ea5e9",
          weight = 1.5,
          fillOpacity = 0.06,
          options = pathOptions(pane = "polygons")
        )
    } else {
      # MÁGICA: Identifica os municípios das escolas que passaram pelos filtros pedagógicos/geográficos
      munis_filtrados <- unique(dados_explorador_filtrados()$NM_MUNICIPIO)
      chaves_validas <- stringi::stri_trans_general(
        toupper(munis_filtrados),
        "Latin-ASCII"
      )

      # Filtra o shapefile baseado nesses municípios reais
      munis_sf <- shape_munis_enrich %>% filter(chave %in% chaves_validas)

      if (nrow(munis_sf) > 0) {
        bbox <- st_bbox(munis_sf)
        proxy_exp %>%
          flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
          addPolygons(
            data = munis_sf,
            group = "poligonos",
            fillColor = "#1a56db",
            color = "#0ea5e9",
            weight = 2,
            fillOpacity = 0.15,
            label = ~name_muni,
            options = pathOptions(pane = "polygons")
          )
      }
    }
  })

  # Marcadores – Explorador
  observe({
    df_filtrado <- dados_explorador_filtrados()
    proxy_exp <- leafletProxy("mapa_explorador") %>% clearGroup("escolas")
    if (nrow(df_filtrado) == 0) {
      return()
    }

    df <- dados_escolas_unicas %>%
      filter(CD_ESCOLA %in% unique(df_filtrado$CD_ESCOLA)) %>%
      mutate(
        COR_MARCADOR = case_when(
          !is.na(CONVENIO) &
            toupper(CONVENIO) != "NÃO POSSUI" &
            CONVENIO != "-" ~ "#f59e0b",
          TIPO == "Escola Militar" ~ "#10b981",
          TIPO == "Escola Especial" ~ "#3b82f6",
          TIPO == "Escola Integral" ~ "#ef4444",
          TRUE ~ "#1a56db"
        )
      )

    proxy_exp %>%
      addCircleMarkers(
        data = df,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 6,
        color = "#ffffff",
        weight = 1.5,
        fillColor = ~COR_MARCADOR,
        fillOpacity = 0.92,
        group = "escolas",
        layerId = ~ESCOLA_BUSCA,
        label = ~NM_ESCOLA,
        popup = gerar_tooltip(df),
        popupOptions = popupOptions(maxWidth = 520),
        options = pathOptions(pane = "markers")
      ) %>%
      removeControl("legenda_cores") %>%
      addLegend(
        position = "bottomright",
        colors = c("#f59e0b", "#10b981", "#3b82f6", "#ef4444", "#1a56db"),
        labels = c(
          "Com Convênio",
          "Escola Militar",
          "Escola Especial",
          "Escola Integral",
          "Escola Padrão"
        ),
        title = "Classificação",
        opacity = 0.9,
        layerId = "legenda_cores"
      )
  })

  observeEvent(input$exp_escola, {
    if (input$exp_escola != "") {
      escola <- dados_escolas_unicas %>%
        filter(ESCOLA_BUSCA == input$exp_escola) %>%
        head(1)
      if (nrow(escola) > 0) {
        leafletProxy("mapa_explorador") %>%
          flyTo(lng = escola$LONGITUDE, lat = escola$LATITUDE, zoom = 16)
      }
    }
  })

  observeEvent(input$btn_limpar_exp, {
    updatePickerInput(session, "exp_regional", selected = character(0))
    updatePickerInput(session, "exp_municipio", selected = character(0))
    updateSelectizeInput(session, "exp_escola", selected = "")
    updatePickerInput(session, "exp_tipo", selected = character(0))
    updatePickerInput(session, "exp_turno", selected = character(0))
    updatePickerInput(session, "exp_modalidade", selected = character(0))
    updatePickerInput(session, "exp_composicao", selected = character(0))
    leafletProxy("mapa_explorador") %>%
      setView(lng = -49.5, lat = -16.0, zoom = 6)
  })

  # ============================================================
  # LÓGICA: ROTEIRIZADOR (ABA 2)
  # ============================================================
  observe({
    munis <- if (
      is.null(input$rot_regional) || length(input$rot_regional) == 0
    ) {
      sort(unique(dados_escolas_unicas$NM_MUNICIPIO))
    } else {
      sort(unique(dados_escolas_unicas$NM_MUNICIPIO[
        dados_escolas_unicas$NM_REGIONAL %in% input$rot_regional
      ]))
    }

    updatePickerInput(
      session,
      "rot_municipio",
      choices = munis,
      selected = input$rot_municipio
    )
  })

  dados_roteirizador_filtrados <- reactive({
    df <- dados_escolas_unicas
    if (!is.null(input$rot_regional) && length(input$rot_regional) > 0) {
      df <- df %>% filter(NM_REGIONAL %in% input$rot_regional)
    }
    if (!is.null(input$rot_municipio) && length(input$rot_municipio) > 0) {
      df <- df %>% filter(NM_MUNICIPIO %in% input$rot_municipio)
    }
    df
  })

  observe({
    df <- dados_roteirizador_filtrados()
    escolas_validas <- if (nrow(df) > 0) {
      sort(unique(df$ESCOLA_BUSCA))
    } else {
      character(0)
    }

    sel_origem <- isolate(input$rot_origem)
    sel_destino <- isolate(input$rot_destino)
    sel_base <- isolate(input$rot_escola_base) # Pega o que estava selecionado no raio

    novo_origem <- if (
      !is.null(sel_origem) && sel_origem %in% escolas_validas
    ) {
      sel_origem
    } else {
      ""
    }
    novo_destino <- if (
      !is.null(sel_destino) && sel_destino %in% escolas_validas
    ) {
      sel_destino
    } else {
      ""
    }
    novo_base <- if (!is.null(sel_base) && sel_base %in% escolas_validas) {
      sel_base
    } else {
      ""
    }

    updateSelectizeInput(
      session,
      "rot_origem",
      choices = c("", escolas_validas),
      selected = novo_origem
    )
    updateSelectizeInput(
      session,
      "rot_destino",
      choices = c("", escolas_validas),
      selected = novo_destino
    )

    # Agora a Escola do Raio obedece os filtros da CRE e Município!
    updateSelectizeInput(
      session,
      "rot_escola_base",
      choices = c("", escolas_validas),
      selected = novo_base
    )
  })

  # Polígonos – Roteirizador
  observe({
    proxy_rot <- leafletProxy("mapa_roteirizador") %>% clearGroup("poligonos")

    if (
      (is.null(input$rot_regional) || length(input$rot_regional) == 0) &&
        (is.null(input$rot_municipio) || length(input$rot_municipio) == 0)
    ) {
      proxy_rot %>%
        addPolygons(
          data = shape_estado,
          group = "poligonos",
          fillColor = "#1a56db",
          color = "#0ea5e9",
          weight = 1.5,
          fillOpacity = 0.06,
          options = pathOptions(pane = "polygons")
        )
    } else {
      munis_filtrados <- unique(dados_roteirizador_filtrados()$NM_MUNICIPIO)
      chaves_validas <- stringi::stri_trans_general(
        toupper(munis_filtrados),
        "Latin-ASCII"
      )

      munis_sf <- shape_munis_enrich %>% filter(chave %in% chaves_validas)

      if (nrow(munis_sf) > 0) {
        bbox <- st_bbox(munis_sf)
        proxy_rot %>%
          flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
          addPolygons(
            data = munis_sf,
            group = "poligonos",
            fillColor = "#1a56db",
            color = "#0ea5e9",
            weight = 2,
            fillOpacity = 0.15,
            label = ~name_muni,
            options = pathOptions(pane = "polygons")
          )
      }
    }
  })

  # Marcadores – Roteirizador
  observe({
    df <- dados_roteirizador_filtrados()
    proxy_rot <- leafletProxy("mapa_roteirizador") %>% clearGroup("escolas")
    if (nrow(df) == 0) {
      return()
    }
    df <- df %>%
      mutate(
        COR_MARCADOR = case_when(
          !is.na(CONVENIO) &
            toupper(CONVENIO) != "NÃO POSSUI" &
            CONVENIO != "-" ~ "#f59e0b",
          TIPO == "Escola Militar" ~ "#10b981",
          TIPO == "Escola Especial" ~ "#3b82f6",
          TIPO == "Escola Integral" ~ "#ef4444",
          TRUE ~ "#1a56db"
        )
      )
    proxy_rot %>%
      addCircleMarkers(
        data = df,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 6,
        color = "#ffffff",
        weight = 1.5,
        fillColor = ~COR_MARCADOR,
        fillOpacity = 0.92,
        group = "escolas",
        layerId = ~ESCOLA_BUSCA,
        label = ~NM_ESCOLA,
        popup = gerar_tooltip(df),
        popupOptions = popupOptions(maxWidth = 520),
        options = pathOptions(pane = "markers")
      )
  })

  # --- SISTEMA DE ROTAS ---
  resultado_rota <- reactiveVal(NULL)
  historico_rotas <- reactiveVal(data.frame(
    Origem = character(),
    Destino = character(),
    `Distância (km)` = numeric(),
    `Tempo (min)` = numeric(),
    Modo = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))

  observeEvent(input$btn_calcular_rot, {
    if (
      is.null(input$rot_origem) ||
        input$rot_origem == "" ||
        is.null(input$rot_destino) ||
        input$rot_destino == ""
    ) {
      showNotification("⚠️ Selecione a origem e o destino.", type = "warning")
      return()
    }
    if (input$rot_origem == input$rot_destino) {
      showNotification("❌ Origem e destino são iguais!", type = "error")
      return()
    }
    coords_origem <- dados_escolas_unicas %>%
      filter(ESCOLA_BUSCA == input$rot_origem) %>%
      select(LONGITUDE, LATITUDE) %>%
      head(1)
    coords_destino <- dados_escolas_unicas %>%
      filter(ESCOLA_BUSCA == input$rot_destino) %>%
      select(LONGITUDE, LATITUDE) %>%
      head(1)
    if (nrow(coords_origem) == 0 || nrow(coords_destino) == 0) {
      showNotification("Coordenadas não encontradas.", type = "error")
      return()
    }
    id_notif <- showNotification(
      "🗺️ Calculando rota...",
      duration = NULL,
      type = "message"
    )
    tryCatch(
      {
        # 1. FORÇA A CHAVE NO SISTEMA ANTES DO CÁLCULO (Isso resolve o 403 no Shiny!)
        Sys.setenv(
          ORS_API_KEY = "eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjJiODQxYWI2OGY3ZjQ5YzM4YzNkNDc2YjhlOGUzN2M5IiwiaCI6Im11cm11cjY0In0="
        )

        # 2. Converte a opção do seu botão para a linguagem do ORS
        modo_ors <- ifelse(
          input$rot_modo == "car",
          "driving-car",
          "foot-walking"
        )

        # 3. Extrai APENAS os números (evita que o Shiny mande formato de tabela pra API)
        lon_ori <- as.numeric(coords_origem$LONGITUDE)
        lat_ori <- as.numeric(coords_origem$LATITUDE)
        lon_des <- as.numeric(coords_destino$LONGITUDE)
        lat_des <- as.numeric(coords_destino$LATITUDE)

        coordenadas <- list(c(lon_ori, lat_ori), c(lon_des, lat_des))

        # 4. Faz o cálculo rápido retornando o formato espacial (sf)
        rota_ors <- ors_directions(
          coordenadas,
          profile = modo_ors,
          output = "sf"
        )

        # Salva o resultado para o mapa desenhar
        resultado_rota(rota_ors)

        # 5. O ORS retorna os valores agregados dentro da coluna 'summary'
        dist_km <- round(rota_ors$summary[[1]]$distance / 1000, 2)
        tempo_min <- round(rota_ors$summary[[1]]$duration / 60, 0)

        novo_reg <- data.frame(
          Origem = input$rot_origem,
          Destino = input$rot_destino,
          `Distância (km)` = dist_km,
          `Tempo (min)` = tempo_min,
          Modo = ifelse(input$rot_modo == "car", "🚗 Carro", "🚶 A Pé"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        historico_rotas(rbind(novo_reg, historico_rotas()))
      },
      error = function(e) {
        showNotification(
          paste("Erro ao calcular rota:", conditionMessage(e)),
          type = "error"
        )
      },
      finally = {
        removeNotification(id_notif)
      }
    )
  })

  observeEvent(input$btn_limpar_rot, {
    updatePickerInput(session, "rot_regional", selected = character(0))
    updatePickerInput(session, "rot_municipio", selected = character(0))
    updateSelectizeInput(session, "rot_origem", selected = "")
    updateSelectizeInput(session, "rot_destino", selected = "")

    # Nova linha para limpar o input de raio também:
    updateSelectizeInput(session, "rot_escola_base", selected = "")

    resultado_rota(NULL)

    # Atualizado para apagar os desenhos do raio também:
    leafletProxy("mapa_roteirizador") %>%
      clearGroup("rota") %>%
      clearGroup("raio_busca") %>%
      clearGroup("escolas_destaque") %>%
      setView(lng = -49.5, lat = -16.0, zoom = 6)
  })

  observeEvent(input$btn_buscar_raio, {
    if (is.null(input$rot_escola_base) || input$rot_escola_base == "") {
      showNotification(
        "⚠️ Selecione uma escola de referência para a busca.",
        type = "warning"
      )
      return()
    }

    escola_base <- dados_escolas_unicas %>%
      filter(ESCOLA_BUSCA == input$rot_escola_base) %>%
      head(1)
    if (nrow(escola_base) == 0) {
      return()
    }

    base_sf <- st_as_sf(
      escola_base,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = 4326,
      remove = FALSE
    )
    escolas_sf <- st_as_sf(
      dados_escolas_unicas,
      coords = c("LONGITUDE", "LATITUDE"),
      crs = 4326,
      remove = FALSE
    )

    distancias <- st_distance(escolas_sf, base_sf)
    raio_metros <- input$rot_raio * 1000

    escolas_no_raio <- escolas_sf[as.numeric(distancias) <= raio_metros, ]
    bbox <- st_bbox(escolas_no_raio)

    leafletProxy("mapa_roteirizador") %>%
      clearGroup("raio_busca") %>%
      clearGroup("escolas_destaque") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
      addCircles(
        lng = escola_base$LONGITUDE,
        lat = escola_base$LATITUDE,
        radius = raio_metros,
        weight = 2,
        color = "#1a56db",
        fillColor = "#4f8ef7",
        fillOpacity = 0.15,
        group = "raio_busca"
      ) %>%
      addCircleMarkers(
        data = escolas_no_raio,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 8,
        color = "#ffffff",
        weight = 2,
        fillColor = "#f59e0b",
        fillOpacity = 1,
        group = "escolas_destaque",
        layerId = ~ paste0("raio_", ESCOLA_BUSCA),
        label = ~NM_ESCOLA,
        popup = gerar_tooltip(escolas_no_raio),
        popupOptions = popupOptions(maxWidth = 520),
        options = pathOptions(pane = "markers")
      ) %>%
      addAwesomeMarkers(
        lng = escola_base$LONGITUDE,
        lat = escola_base$LATITUDE,
        icon = awesomeIcons(
          icon = "star",
          library = "fa",
          markerColor = "blue"
        ),
        label = paste("BASE:", escola_base$NM_ESCOLA),
        group = "raio_busca",
        options = pathOptions(pane = "markers")
      )

    qtd_encontrada <- nrow(escolas_no_raio) - 1
    showNotification(
      sprintf(
        "%d escolas encontradas num raio de %d km.",
        qtd_encontrada,
        input$rot_raio
      ),
      type = "message"
    )
  })

  output$kpi_rot_dist <- renderText({
    if (is.null(resultado_rota())) {
      return("—")
    } else {
      dist_metros <- as.numeric(resultado_rota()$summary[[1]]$distance)
      dist_km <- round(dist_metros / 1000, 1)
      return(paste0(dist_km, " km"))
    }
  })

  output$kpi_rot_temp <- renderText({
    if (is.null(resultado_rota())) {
      return("—")
    } else {
      tempo_segundos <- as.numeric(resultado_rota()$summary[[1]]$duration)
      t <- round(tempo_segundos / 60, 0)

      if (t < 60) {
        return(paste0(t, " min"))
      } else {
        return(paste0(floor(t / 60), "h ", t %% 60, "m"))
      }
    }
  })
  output$kpi_rot_modo <- renderText({
    ifelse(input$rot_modo == "car", "Carro", "A Pé")
  })

  observeEvent(resultado_rota(), {
    req(resultado_rota())
    rota <- resultado_rota()
    coords <- st_coordinates(rota)
    bbox <- st_bbox(rota)
    leafletProxy("mapa_roteirizador") %>%
      clearGroup("rota") %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
      addPolylines(
        data = rota,
        color = "#0ea5e9",
        weight = 5,
        opacity = .95,
        group = "rota",
        options = pathOptions(pane = "rotas")
      ) %>%
      addAwesomeMarkers(
        lng = coords[1, 1],
        lat = coords[1, 2],
        icon = awesomeIcons(
          icon = "play",
          library = "fa",
          markerColor = "green"
        ),
        popup = input$rot_origem,
        group = "rota",
        options = pathOptions(pane = "markers")
      ) %>%
      addAwesomeMarkers(
        lng = coords[nrow(coords), 1],
        lat = coords[nrow(coords), 2],
        icon = awesomeIcons(
          icon = "flag-checkered",
          library = "fa",
          markerColor = "red"
        ),
        popup = input$rot_destino,
        group = "rota",
        options = pathOptions(pane = "markers")
      )
  })

  output$tabela_rotas <- renderDT({
    datatable(
      historico_rotas(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tp",
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
        )
      ),
      class = "display nowrap"
    )
  })
}

shinyApp(ui, server)

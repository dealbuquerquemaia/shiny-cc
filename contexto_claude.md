# Contexto: Painel de Rastreamento de Câncer do Colo do Útero (shiny-cc)

Use este documento para entender o projeto antes de discutir melhorias ou mudanças. Quando decidirmos implementar algo, as alterações de código são feitas em outra sessão (Claude Code / terminal).

---

## O que é o projeto

**shiny-cc** é um dashboard em R Shiny para planejamento de rastreamento de câncer do colo do útero (CCU). Dado um país/região, cobertura-alvo, faixa etária e método de rastreamento (HPV ou citologia), ele modela o fluxo completo do rastreamento e estima a necessidade anual de procedimentos e recursos.

Contexto institucional: ferramenta desenvolvida no âmbito do projeto ConeCta-SP / IARC / FSP-USP.

---

## Abas do painel

| Aba | Conteúdo |
|-----|---------|
| **Summary** | Métricas-chave: mulheres rastreadas, procedimentos necessários (testes, colposcopias, biópsias, EZTs, tratamentos) e seguimento |
| **Capacity** | Compara produção real SUS/SIA 2024 com necessidade estimada (Brasil) |
| **Epidemiology** | Dados GLOBOCAN de incidência/mortalidade para o país selecionado |
| **Equipment & HR needs** | Colposcópios, colposcopistas, citopatologistas e patologistas necessários |
| **Pathway** | Diagrama de fluxo (DiagrammeR) com volumes em cada nó |
| **About** | Informações do projeto |

---

## Sidebar (filtros globais)

A sidebar é um painel lateral com accordion, contendo:

### Population
- Seletor de país/população (GLOBOCAN)
- Fonte da população: GLOBOCAN ou manual
- **Brasil**: escolha entre população total (IBGE) ou dependente do SUS (IBGE − ANS)
- **Filtros geográficos Brasil** (em cascata): Estado (UF) → Macrorregião de Saúde → Região de Saúde → Município
- Referência geográfica do SIA: local de atendimento ou residência

### Screening protocol
- Método: HPV ou Citologia
- Faixa etária alvo (From / To)
- **Fonte de parâmetros HPV**: preset fechado (ex: PREVENTIVO – Indaiatuba) ou Customize
  - Em Customize: painel completo de positividades (prevalência HPV, resultado citologia reflexa, colposcopia, biópsia, seguimento)
- **Fonte de parâmetros Citologia**: preset fechado (ex: INCA 2019, SISCAN 2022–2024) ou Customize
  - SISCAN tem vinculação geográfica automática: se 1 UF selecionada → usa dado daquela UF; sem UF ou múltiplas UFs → usa Brasil
  - Em Customize: painel completo de positividades

### Screening coverage
- Slider 0–100%

### Resources
- Unidade de capacidade (Dia / Semana / Mês / Ano) com conversão automática dos inputs
- Capacidade anual: colposcópio, colposcopista (20h/sem), citopatologista, patologista (20h/sem)

---

## Modelos de cálculo

### HPV (rastreamento a cada 5 anos)
Fluxo: HPV test → HPV 16/18+ vai direto para colposcopia; outros HR-HPV+ fazem citologia reflexa → se positiva, colposcopia → biópsia → EZT (NIC2+) ou alta complexidade (câncer). Seguimento com HPV após tratamento (6 e 18 meses).

### Citologia (rastreamento a cada 3 anos)
Fluxo: Pap smear → ASCH+ vai para colposcopia; outras alterações podem ir para colposcopia; negativo → seguimento. Colposcopia → biópsia → EZT ou alta complexidade. Seguimento com citologia e colposcopia pós-EZT.

---

## Stack técnica

- **R Shiny** (UI modular com `moduleServer`)
- **data.table** (sem dplyr em nenhum lugar)
- Dados carregados de arquivos `.rds` na inicialização (`app.R`)
- Módulos independentes: cada aba é um módulo com `_ui` e `_server`
- Filtros globais via `input_global()` reativo passado para todos os módulos

---

## Fontes de dados

| Objeto | Origem | Conteúdo |
|--------|--------|---------|
| `df_cc_completo` | GLOBOCAN 2022 | Incidência/mortalidade + população por país e faixa etária |
| `df_cc_taxas` | GLOBOCAN 2022 | Taxas de incidência/mortalidade |
| `pop_municipio_regional` | IBGE / ANS | População feminina por município, faixa etária, macrorregião e região de saúde |
| `sia_cc_resumo` | SUS/SIA 2024 | Produção real de citologias, colposcopias, biópsias e EZTs por município |
| `regional_sus_map` | IBGE | Mapeamento município → região de saúde → macrorregião → UF |
| `cito_presets` | INCA 2019 + SISCAN 2022–2024 | Parâmetros de positividade da citologia por fonte e UF |

---

## Parâmetros de citologia disponíveis por fonte

### INCA 2019 (nacional, fixo)
Todos os 15 parâmetros do modelo (volume de exames, resultados, colposcopia, biópsia, desfechos).

### SISCAN 2022–2024 (por UF)
4 parâmetros derivados variam por UF:
- `unsatisfactory_pct` — % exames insatisfatórios
- `res_asch_pct` — % ASCH+ entre satisfatórios
- `res_other_pct` — % outras alterações entre satisfatórios
- `res_neg_pct` — % negativos entre satisfatórios

Os outros 11 parâmetros (encaminhamento para colposcopia, positividade da colposcopia, desfechos da biópsia) usam INCA 2019 como fallback — serão atualizados quando houver dados disponíveis por UF.

---

## Estrutura de presets (extensível)

### HPV — `HPV_PRESETS` (em `R/00_constants_cc.R`)
```r
HPV_PRESETS <- list(
  indaiatuba = list(label = "PREVENTIVO - Indaiatuba", params = HPV_DEFAULTS)
  # adicionar: pernambuco = list(label = "...", params = list(...))
)
```

### Citologia — `CITO_PRESETS_META` + `data/cito_presets.rds`
- Metadados em `00_constants_cc.R`
- Dados em `data/cito_presets.rds` (gerado por `data-raw/07_prepare_cito_presets.R`)
- Para adicionar nova fonte: atualizar o script de `data-raw` e regenerar o `.rds`
- Os módulos não precisam ser alterados — consomem apenas `input_global()`

---

## Decisões de design relevantes

- **Filtros globais na sidebar**: todos os módulos recebem `input_global()` — nenhum módulo tem seus próprios filtros de seleção geográfica ou protocolo
- **Preset vs. Customize**: ao selecionar um preset, os inputs numéricos são preenchidos automaticamente (mas permanecem editáveis se o usuário mudar para Customize)
- **SISCAN automático**: ao trocar de UF com SISCAN ativo, os parâmetros de citologia atualizam automaticamente. Múltiplas UFs → fallback para Brasil
- **Capacidade anual**: inputs de capacidade de recursos são sempre convertidos para base anual antes de passar para o engine
- **Brasil vs. mundo**: módulos detectam `is_brazil` internamente para ativar funcionalidades específicas (filtros subnacionais, dados SUS/SIA)

---

## O que ainda não está implementado / possíveis melhorias a discutir

- Parâmetros SISCAN de colposcopia e biópsia por UF (dados não disponíveis ainda)
- Presets HPV para outros municípios/estados além de Indaiatuba
- Exportação de resultados (PDF, Excel)
- Comparação entre dois cenários lado a lado
- Uso de algum algoritmo de IA para identificar os outliers em capacidade vs necessidade
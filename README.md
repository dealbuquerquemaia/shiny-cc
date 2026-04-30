# shiny-cc — Cervical Cancer Screening Planning Dashboard

Dashboard Shiny para planejamento de rastreamento de câncer do colo do útero (CCU). Dado país/região, cobertura, faixa etária e método (HPV ou citologia), estima a necessidade anual de testes, colposcopias, biópsias, tratamentos, seguimento, e recursos humanos/equipamentos.

Projeto desenvolvido no âmbito do **ConeCta-SP / IARC / FSP-USP**.

> **Status da documentação:** este README está sendo reconstruído arquivo-por-arquivo. Consulte `docs/INVENTORY.md` para o inventário completo de funções e variáveis. A seção **"Arquivos"** abaixo é preenchida conforme cada arquivo é processado.

---

## Principais funcionalidades (abas)

- **Summary** — KPIs consolidados do cenário (rastreadas, testes, colposcopias, biópsias, EZTs, seguimento)
- **Compare** — dois cenários lado a lado
- **Pathway** — diagrama do fluxo de rastreamento com contagens (DiagrammeR)
- **Epidemiology** — dados GLOBOCAN do país selecionado
- **Capacity** — comparação produção SUS/SIA 2024 × necessidade estimada (Brasil)
- **Equipment & HR needs** — colposcópios, colposcopistas, citopato/patologistas necessários
- **Peer Analysis** — comparação com países similares
- **Maps** — camadas geográficas (município/região de saúde/macrorregião/UF)
- **Detailed table** — tabela granular
- **About** — créditos e contexto

---

## Estrutura do projeto

```
shiny-cc/
├── app.R                    # Entry point (carga de dados, UI, server)
├── R/                       # Módulos e utilitários
│   ├── 00_constants_cc.R    # Constantes: HPV_PRESETS, HPV_DEFAULTS, CITO_PRESETS_META
│   ├── 01_utils_cc.R        # Helpers (formatação, operadores)
│   ├── 02_engine_capacity_cc.R  # Engine de cálculo (cc_engine_*)
│   ├── 10_mod_filters_cc.R  # Sidebar — filtros globais → input_global()
│   ├── 11_mod_resumo_geral.R     # aba Summary
│   ├── 12_mod_resumo_pais.R      # aba Epidemiology
│   ├── 14_mod_equipamentos.R     # aba Equipment & HR needs
│   ├── 15_mod_fluxo.R            # aba Pathway
│   ├── 16_mod_capacidade.R       # aba Capacity
│   ├── 17_mod_compare.R          # aba Compare
│   ├── 18_mod_maps.R             # aba Maps
│   ├── 19_mod_peers.R            # aba Peer Analysis
│   ├── 20_mod_tabela_detalhada.R # aba Detailed table
│   └── 99_mod_sobre.R            # aba About
├── data/                    # .rds consumidos pelo app (saídas do ETL)
├── data-raw/                # Scripts ETL (pré-processam data/)
│   ├── 01_prepare_cc.R      # globais GLOBOCAN
│   ├── 02_prepare_BR.R      # populações IBGE/ANS
│   ├── 03_prepare_SUS.R     # produção SIA 2024
│   ├── 04_checks_cc.R       # checagens
│   ├── 05_glossary_cc.R     # glossário
│   ├── 06_make_all_cc.R     # pipeline completo
│   ├── 07_prepare_cito_presets.R # presets de citologia (INCA, SISCAN)
│   ├── 08_prepare_geometrias.R   # shapefiles (geobr → sf → rds)
│   └── 09_prepare_peers.R        # tabela de países peers
├── docs/
│   ├── INVENTORY.md         # Tabelas de funções + variáveis (gerado incrementalmente)
│   └── dicionario.md        # Dicionário de dados (mínimo — a consolidar)
├── tests/
├── www/                     # CSS, JS, logos, template de PDF
└── rsconnect/               # config de deploy
```

---

## Arquivos (descrições detalhadas)

> Preenchido arquivo-por-arquivo. Cada seção abaixo é atualizada ao processar o arquivo correspondente.

### `app.R` — Entry point
<!-- DOC:app.R:START -->
Carrega pacotes, lê todos os `.rds` de `data/`, faz `source()` de todos os módulos, define a UI (`navbarPage` com 10 abas + sidebar customizada) e o `server` que:

1. Instancia o módulo de filtros (`mod_filters_cc_server`) e propaga o reativo `input_global` para todos os demais módulos.
2. Chama o servidor de cada módulo com os dataframes necessários.
3. Implementa o export de PDF (`btn_download_pdf`) — reroda o engine (`cc_engine_run`), monta uma lista `data` com KPIs, equipamentos, epidemiologia, capacidade e "assumptions", renderiza `www/report_template.Rmd` e converte pra PDF via `pagedown::chrome_print`.

**Dados carregados (todos via `readRDS("data/…")`):**
`df_cc_completo`, `df_cc_taxas`, `df_dim_country`, `df_dim_age`, `df_dim_type`, `df_dim_year`, `pop_municipio_faixas`, `pop_municipio_faixas_total_sus`, `pop_municipio_regional`, `regional_sus_map`, `sia_cc_resumo` (origem: `sus_proc_resumo.rds`), `cito_presets`, `peers_data`, `geo_municipios`, `geo_regioes_saude`, `geo_macrorregioes`, `geo_estados`.

**Módulos invocados no `server`:**
`mod_filters_cc_server` (produz `input_global`), `mod_resumo_geral_server`, `mod_fluxo_server`, `mod_resumo_pais_server`, `mod_equipment_server`, `mod_sobre_server`, `peers_server`, `mod_maps_server`, `mod_compare_server`, `mod_capacity_server`, `mod_detailed_table_server`.

**Helpers internos ao PDF (linhas 322–360):** `.fmt_safe`, `.fmt_rate_safe`, `.val_or`, `.num1`, `.cap1`, `.req_n`, `.demand_cito`, `.demand_patol`. Alguns duplicam helpers que também existem em outros módulos — ver pendências em `docs/INVENTORY.md`.

**UI notável:** injeta JS jQuery para (a) toggle do sidebar via `#cc_sidebar_toggle` e `#cc_hamburger`, (b) acordeão `.cc-section-header`, (c) abertura automática do painel de "Customize" quando o usuário troca pra Custom, (d) inicialização de tooltips Bootstrap.
<!-- DOC:app.R:END -->

### `R/00_constants_cc.R` — Constantes e presets
<!-- DOC:00_constants_cc.R:START -->
Constantes globais, carregadas antes de todos os módulos. Define:

- **Ordenações canônicas:** `AGE_ORDER` (18 faixas etárias GLOBOCAN), `TYPE_ORDER` (Incidence/Mortality), `SEX_LABELS`.
- **Paleta institucional:** `CC_COLORS` (lista nomeada de hex) e `PALETTE_MAIN` (vetor de 10 cores para séries em gráficos). Cores verde-azuladas alinhadas ao CSS em `www/style.css`.
- **Parâmetros HPV:** `HPV_DEFAULTS` (15 campos em %) — valores-padrão do modelo de rastreamento com HPV (origem: PREVENTIVO/Indaiatuba). Campos: `p16_18`, `poutros`, `pneg`, `cito_out_*`, `colpo16_*`, `colpoout_*`, `b16_*`, `bo_*`, `hpv_followup_pos_pct`.
- **Presets HPV:** `HPV_PRESETS` (lista extensível) — para adicionar uma nova fonte, acrescentar entrada com `label` + `params`.
- **Parâmetros Citologia:** `CITO_DEFAULTS` (16 campos em %) — cobre nós 1 (realização), 2 (resultado), 3 (colposcopia) e desfechos de biópsia por braço (HSIL/ASC-H vs outras alterações).
- **Metadados de presets de Citologia:** `CITO_PRESETS_META` — apenas `label` e flag `por_uf`; os valores numéricos ficam em `data/cito_presets.rds`.
- **Tooltips:** `cc_TOOLTIPS` — estrutura `$<aba>$<subgrupo>$<campo>` consumida pelos módulos. Chaves atuais: `resumo_geral_ccu` (common/hpv/cytology), `capacidade_ccu`, `equipamentos_ccu`.

Como estender: para novo preset HPV, editar `HPV_PRESETS` aqui. Para novo preset Citologia, atualizar `data-raw/07_prepare_cito_presets.R`, regenerar o `.rds`, e adicionar entrada em `CITO_PRESETS_META`.
<!-- DOC:00_constants_cc.R:END -->

### `R/01_utils_cc.R` — Utilitários
<!-- DOC:01_utils_cc.R:START -->
Utilitários de uso geral carregados cedo (depois de `00_constants_cc.R`). Organizados em 4 blocos:

**1. Parser de faixas etárias** — `age_band_min()` e `age_band_max()` convertem rótulos GLOBOCAN ("50-54", "85+") em inteiros. "85+" tem máximo convencionado em 120. Aceitam vetores, NA ou string vazia sem quebrar.

**2. Formatação numérica (padrão IARC)** — `fmt_int()` (inteiros com vírgula de milhar, ex. `12,345`) e `fmt_rate()` (valores com casas decimais, ex. `12,345.6`). Ambos devolvem `"–"` (en-dash) quando a entrada é NULL, NA ou vetor todo-NA. Vírgula = milhar, ponto = decimal (notação internacional, alinhada à IARC). **Duplicados em `app.R`** como `.fmt_safe`/`.fmt_rate_safe` — ver pendências em `docs/INVENTORY.md`.

**3. Tooltip Bootstrap e operador `%||%`** — `cc_with_tt(tag, tooltip, placement)` acrescenta atributos `title`, `data-toggle`, `data-placement` e `data-container` na tag; a ativação efetiva é feita pelo JS injetado em `app.R`. `%||%` é o operador null-coalescing (`a %||% b` → `a` se não-NULL, senão `b`), usado amplamente nos módulos para dar default a inputs opcionais.

**4. Diagrama de fluxo (aba Pathway)** — helpers que produzem um card HTML com nós absolutamente posicionados e conectores SVG (`viewBox` 1200×780, escalável via CSS):
  - `.NODE_PROPS` — paleta institucional dos 5 tipos de nó (n0 branco → n4 verde escuro para desfechos graves).
  - `.pnode(cls, left, top, label, count, pct)` — gera um `<div.node>` com posição/estilo por classe.
  - `.pfmt()`, `.ppct(num, denom, label)` — formatação de contagem e "X% of <label>" (retorna NULL se denom 0/NA).
  - `.wrap_pathway()` — envelope com título, `.pathway-scalable-wrapper` e rodapé de caption.
  - **`render_pathway_hpv(f, geo_label, caption)`** — fluxo HPV de 5 fases (Screening → HPV result → Reflex cytology → Colposcopy → Biopsy). `f` é a lista de contagens do engine.
  - **`render_pathway_cytology(f, geo_label, caption)`** — fluxo Citologia de 5 fases (Screening → Cytology result → Diagnostic cytology → Colposcopy → Biopsy).

Consumidas por `R/15_mod_fluxo.R` (Pathway) e indiretamente pelo export de PDF.
<!-- DOC:01_utils_cc.R:END -->

### `R/02_engine_capacity_cc.R` — Engine de cálculo
<!-- DOC:02_engine_capacity_cc.R:START -->
Núcleo computacional do app. Carregado cedo e consumido por praticamente todos os módulos (via `cc_engine_run`) e pelo export de PDF em `app.R`. Função pura sem dependência de Shiny.

**Pipeline (ordem de chamada dentro de `cc_engine_run`):**

1. `cc_engine_settings(...)` — normaliza inputs do usuário em uma `cfg` (list). Aplica defaults de `HPV_DEFAULTS` e `CITO_DEFAULTS` quando o parâmetro chega NULL/NA. Todos os % ficam em 0–100 aqui (a divisão por 100 acontece em (3)).
2. `cc_pop_in_range(...)` — dispatch de 3 modos para produzir `pop_selected`:
   - `pop_mode = "other"` → usa `custom_pop` (entrada manual).
   - Brasil → `cc_br_pop_in_range()` agrega `pop_total`/`pop_sus` por filtros geográficos.
   - GLOBOCAN → `cc_pop_by_age()` + intersecção com `[age_min, age_max]`.
3. `cc_workup_metrics(N_rastreada, cfg, eligible)` — dispatch por `cfg$screen_method`:
   - `"hpv"` → delega para `modelo_hpv()` (seção 11 do arquivo).
   - `"cytology"` → modelo inline: volume de Paps `((E/3) + E*ft) * (1+uns)`, resultados em 3 categorias (ASC-H+, outras, negativos), colposcopia por braço, biópsia por positividade, desfechos (NIC2/3 → EZT, câncer → alta complexidade, NIC1/neg → follow-up), follow-up pós-colpo/biópsia/EZT (6 citologias + 2 colposcopias por EZT).
4. `cc_hr_metrics(metrics, cfg)` — converte demanda de procedimentos em nº de recursos (colposcópios, colposcopistas, citopatologistas, patologistas) dividindo pela capacidade anual (`cfg$cap_*` ou fallback `BASE_ANO`). Usa totais "all-in" (`cytologies_total_all`, `colposcopies_total_all`) quando existem.

**Funções auxiliares:**

- `cc_engine_summary_dt(res)` — achata o resultado em data.table de 1 linha para cenários "side-by-side".
- `cc_capacity_from_sia(sia_cc_resumo, ano_cmp_ref)` — soma produção SUS/SIA por categoria. Aceita (a) dataset novo agregado por `categoria` + `total_all`, ou (b) dataset antigo com `ano_cmp` + `PA_PROC_ID` (SIGTAP). Filtros geográficos devem ser aplicados antes pelo chamador.
- `mod_capacity_compare(engine_res, sia_cap)` — cruza necessidade × realizado em 5 categorias (citologia, colposcopia, biópsia, anatomo, tratamento); entrega `ratio_realized_needed` e `coverage_percent`. Consumido pela aba Capacity.
- `modelo_hpv(N, ...)` — função pura do fluxo clínico HPV (3 braços: 16/18+, outros HR+, negativo), retorna data.table de 1 linha com contagens de cada nó.

**Dependências implícitas:** `HPV_DEFAULTS`, `CITO_DEFAULTS`, `BASE_ANO` (de `00_constants_cc.R`); `age_band_min()`, `age_band_max()` (de `01_utils_cc.R`).

**Observação:** HPV e Citologia seguem intervalos diferentes (5 e 3 anos respectivamente). O cálculo de `screened_per_year` em `cc_engine_run()` e o `n_cyt_screen` em `cc_workup_metrics()` agora delegam ao helper único `cc_workup_volumes(eligible, params)` (TASK-04), que também é consumido por `data-raw/09_prepare_peers.R`.
<!-- DOC:02_engine_capacity_cc.R:END -->

### `R/10_mod_filters_cc.R` — Módulo de filtros (sidebar)
<!-- DOC:10_mod_filters_cc.R:START -->
Módulo que constrói a sidebar inteira e produz o reactive `input_global()` — fonte única de verdade para todos os outros módulos. Nenhum outro módulo expõe seletores de país, UF, faixa etária, cobertura, método ou capacidade; todos consomem este objeto.

**UI — accordion com 4 seções:**

1. **Population** (abre por default) — país (`pais_sel`), fonte de população (`pop_mode`: GLOBOCAN/WPP ou manual), e se país = Brasil, mostra `br_pop_tipo` (Total IBGE vs SUS-dependente), `sia_geo_ref` (local de atendimento vs residência) e a cascata de 4 seletizeInputs geográficos (`filt_uf`, `filt_macro`, `filt_reg`, `filt_mun`) + botão *Clear filters*.
2. **Screening protocol** — método (`screen_method` = hpv/cytology), faixa etária alvo, fonte de parâmetros (preset vs Customize) e, quando Customize, os painéis expansíveis de 16 parâmetros do HPV ou da Citologia. Cada preset HPV vem de `HPV_PRESETS` (`00_constants_cc.R`); presets de citologia vêm de `CITO_PRESETS_META` + `data/cito_presets.rds`.
3. **Screening coverage** — slider 0–100% (`coverage`).
4. **Resources** — unidade de capacidade (`cap_unidade` ∈ {dia, semana, mes, ano}) + 4 inputs numéricos de capacidade bruta (colposcópio, colposcopista, citopatologista, patologista). O server anualiza tudo antes de entregar.

**Server — 6 blocos:**

1. **Validação + auto-balance HPV** (`sum_check`, `lock_pair`, `lock_triplet`). Mantém a soma = 100% quando "Lock proportions" está ativo, identificando qual campo foi editado via `rv_lock$prev`. Erros de soma viram mensagens no `params_alert`.
2. **Presets** — observers para HPV (1 fonte) e Citologia (fonte + UF). A função interna `load_cito_preset()` resolve por UF: `por_uf=TRUE` + 1 UF selecionada → usa dados daquela UF; 0 ou N UFs → fallback `"brasil"`.
3. **Validação + auto-balance Citologia** (`lock_triplet_cito`) — espelho do bloco 1 com estado separado (`rv_lock_cito`) para os trios ASCH/other/neg e os dois trios de biópsia (HSIL arm e other arm).
4. **Unidade de capacidade** — multiplicadores relativos a ano: Dia×240, Semana×48, Mês×12. Converte os 4 inputs ao mudar de unidade (mantém sempre o valor "mostrado" consistente com a unidade escolhida).
5. **Filtros geográficos Brasil** — monta `dt_br` (data.table distinct de UF/macro/reg/mun) e 4 `observeEvent`s para a cascata. Troca de país para fora do Brasil → limpa seletores. Botão "Clear filters" zera as seleções mas mantém as opções disponíveis.
6. **Reactive `filters`** — empacota tudo num `list` nomeado e devolve. Capacidades aqui já saem anualizadas (`cap_mult * input$cap_*`). Usa `%||%` para fallback em HPV_DEFAULTS/CITO_DEFAULTS quando o input ainda não foi renderizado.

**Dependências implícitas:** `HPV_DEFAULTS`, `HPV_PRESETS`, `CITO_DEFAULTS`, `CITO_PRESETS_META` (de `00_constants_cc.R`); `%||%` (de `01_utils_cc.R`); `data.table` para manipular `dt_br`.
<!-- DOC:10_mod_filters_cc.R:END -->

### `R/11_mod_resumo_geral.R` — aba Summary
<!-- DOC:11_mod_resumo_geral.R:START -->
Aba "Summary" — camada fina em cima do engine. O módulo não carrega dados próprios: consome `input_global()` da sidebar, traduz em `cfg` via `cc_engine_settings()`, roda `cc_engine_run()` e renderiza 3 seções de cartões (Target population → Work-up → Treatment and follow-up) conectadas por setas.

**UI** — cabeçalho `cc-page-header` com título fixo "Summary" + subtítulo dinâmico (`textOutput("geo_desc")`) e um `uiOutput("cards_resumo")` onde os cards são renderizados.

**Server — 6 blocos:**

1. **Helpers locais** — `val_or()` (fallback NULL/NA) e `fmt_or_dash()` (formata inteiro ou devolve "–"). Ambos duplicam lógica já existente (`.val_or` em `app.R`, `fmt_int` em `01_utils_cc.R`) — ver pendências em `docs/INVENTORY.md`.
2. **Identificação do país** — `country_code()` reactive a partir de `input_global()$pais_sel`; `br_code` resolvido uma vez via `dim_country`; `is_brazil()` reactive; `country_label(code)` traduz código → nome e `pick_all()` é um helper definido mas não usado no corpo (código morto — candidato a limpeza).
3. **`output$geo_desc`** — subtítulo dinâmico. Fora do Brasil exibe só o nome do país ("World" se NULL/NA); dentro do Brasil concatena "Brazil - Total/SUS - UF - Macro - Região - Município". Cada filtro só aparece se tiver seleção; múltiplas seleções viram "Primeiro (n=K)".
4. **`cfg` reactive** — traduz `input_global()` em lista `cc_engine_settings()`. Centraliza toda a defesa (coerção, fallback, clamp de domínio fechado). Faz "retrocompatibilidade" com nomes antigos (`programa` ↔ `screen_method`; `proto1_age_*` ↔ `target_age_*`). Campos que chegam NA caem nos defaults `HPV_DEFAULTS`/`CITO_DEFAULTS` dentro do próprio engine.
5. **`res_engine` / `dt_sum`** — rodam o engine. `res_engine` encapsula em `tryCatch` para que erros virem objeto (o módulo mostra "No data" em vez de quebrar a sessão). `dt_sum` achata para 1 linha via `cc_engine_summary_dt()`.
6. **`output$cards_resumo`** — monta o layout final: 3 seções (`ccu-section-1/2/3`) com cards e notas HTML de parâmetros (% por braço, ages, coverage, teste), conectadas por setas SVG. Ícones SVG inline (22×22). Textos dos tooltips vêm de `cc_TOOLTIPS$resumo_geral_ccu`.

**Dependências implícitas:** `cc_engine_settings`, `cc_engine_run`, `cc_engine_summary_dt` (engine); `fmt_int` (utils); `cc_TOOLTIPS` (constants).
<!-- DOC:11_mod_resumo_geral.R:END -->

### `R/12_mod_resumo_pais.R` — aba Epidemiology
<!-- DOC:12_mod_resumo_pais.R:START -->
Aba "Epidemiology" — visão descritiva dos dados GLOBOCAN do país selecionado. **Não roda o engine**: consome direto `df_cc_completo` (populações WPP 2025 + casos/óbitos 2022) e `df_cc_taxas` (agregados ASR por país).

**UI:** `cc-page-header` com título "Epidemiology" + subtítulo fixo "World Population Prospects 2025 · GLOBOCAN 2022". Dois `conditionalPanel`s:

1. **pop_mode ≠ "other"** (caso padrão):
   - Linha superior — 2 blocos escuros lado a lado:
     * **Population** (ccu-section-1): cards "Total female population" e "Female population, 25–64 years" (faixa fixa, não depende dos sliders `target_age_*`).
     * **Incidence & Mortality** (ccu-section-2): cards "Cervical cancer cases" e "Cervical cancer deaths", cada um exibindo `n` absoluto + ASR per 100k mulheres (ASR world).
   - Linha inferior — 2 gráficos lado a lado:
     * `plot_pop` — barras horizontais com % da população feminina por faixa etária (2025), ordenadas por `AGE_ORDER`.
     * `plot_rates` — colunas agrupadas com taxas de incidência e mortalidade por 100k mulheres, por faixa etária.
2. **pop_mode == "other"** (população manual): mostra apenas "Epidemiology indicators are not available when using 'Other population'." — coerente pois indicadores GLOBOCAN exigem país.

**Helper interno da UI:** `epi_card(label, value_out, foot_out, ico, asr_out=NULL, asr_label=NULL)` — card horizontal compacto com ícone lateral e ASR opcional; usado só dentro do arquivo.

**Server — 5 blocos:**

1. **Seleção de país** — `sel_pais()` coage `input_global()$pais_sel` em integer com `req()`; `nome_pais()` faz lookup em `dim_country`.
2. **`df_pop` reactive** — subset de `df_cc_completo` onde `population_code = país` e `type == "Incidence"` (basta um tipo — pop é idêntica). Agrega `pop_2022` e `pop_2025` por faixa etária (primeiro valor não-NA), converte `age` em factor com `AGE_ORDER`.
3. **`df_tax` reactive** — subset de `df_cc_taxas` (1 linha por país, agregados GLOBOCAN).
4. **Cards** — renders de `pop_total_txt`, `pop_2564_txt` (faixa 25–64 hard-coded), `inc_n_txt`, `inc_asr_txt`, `mort_n_txt`, `mort_asr_txt`. Todos com fallback "–" para NA / zero / não-finito.
5. **Gráficos** — `plot_pop` (ggplot barras horizontais + rótulo "X%") e `plot_rates` (ggplot colunas agrupadas Incidence × Mortality, cores `CC_COLORS$primary`/`$forest`).

**Dependências:** `fmt_int`, `fmt_rate` (utils); `AGE_ORDER`, `CC_COLORS` (constants); `ggplot2` (plots).

**Observações:**
- Outputs `pop_total_foot`, `pop_2564_foot`, `inc_foot`, `mort_foot` são definidos no server mas a UI atual **não os consome** — resquícios de uma versão anterior que exibia footer em cada card; candidatos a limpeza.
- `plot_rates` refaz manualmente o subset de `df_cc_completo` em vez de reutilizar `df_pop()` — a granularidade é diferente (`cases_2022`/`pop_2022` por tipo), mas o filtro por `population_code` poderia ser fatorado.
- A faixa "25–64" dos cards é **fixa e independente** dos sliders `target_age_*` da sidebar. Isso é proposital (valor de referência epidemiológico), mas vale documentar em tooltip para evitar estranhamento do usuário.
<!-- DOC:12_mod_resumo_pais.R:END -->

### `R/14_mod_equipamentos.R` — aba Equipment & HR
<!-- DOC:14_mod_equipamentos.R:START -->
Aba "Equipment & HR needs" — camada fina sobre o engine, muito similar à aba Summary (mesmo padrão `input_global → cfg → cc_engine_run → cards`). Apresenta 4 cards no padrão "Demand | Capacity/year | Required" para:

- **Colposcope** — nº de aparelhos de colposcopia
- **Colposcopist (20h/week)** — médicos colposcopistas (meio-período)
- **Cytopathologist** — profissionais que leem lâminas de citologia
- **Pathologist (20h/week)** — patologistas para anatomopatológico de biópsia/EZT

Em todos, "Required" = ⌈demanda anual / capacidade anual⌉. O arredondamento para cima (`ceiling`) acontece aqui no módulo, enquanto a divisão propriamente dita é feita pelo engine (`cc_hr_metrics` → `*_needed`).

**UI:** `cc-page-header` com título "Equipment & HR needs" + subtítulo dinâmico (`textOutput("geo_desc")`) e um `uiOutput("ccu_cards")` onde a renderização completa acontece.

**Server — 9 blocos:**

1. **País selecionado** — `country_code()`, `br_code` (resolvido 1×), `is_brazil()`. Idêntico a `11_mod_resumo_geral.R`.
2. **Rótulo do país** — `country_label(code)` com lookup defensivo (tryCatch). Fallback "World"/"Selected country".
3. **`output$geo_desc`** — subtítulo que repete o padrão do Summary: fora do Brasil é o nome do país; dentro do Brasil concatena "Brazil - UF - Macro - Região - Município" (só os níveis com seleção).
4. **`cfg_reactive`** — traduz `input_global()` em `cc_engine_settings(...)` com os 40+ campos (coverage, método, ages, 16 params HPV, 16 params citologia, 4 capacidades, flags Brasil). Padrão replicado entre módulos (candidato a helper compartilhado).
5. **`engine_res`** — executa `cc_engine_run(df_completo, cfg, pop_mun_regional)`. Aqui só `res$hr` (data.table de 1 linha com métricas de RH) é consumida.
6. **Ícones SVG inline** — `ico_scope` (lupa+cruz/colposcópio), `ico_doctor` (profissional), `ico_micro` (microscópio), `ico_lab` (lâmina/AP). Sem dependência de fonte externa.
7. **`card_ui(title, demand, cap_year, required, icon_svg, cap_label)`** — fábrica do card. Produz `<div.card-ccu>` com título, valor grande (Required), e sub-linha "Demand: X · Capacity: Y / year · Required".
8. **`build_notes()`** — bloco HTML com (a) resumo do cenário ("Ages 25–64 · Coverage 70% · HPV every 5 years") e (b) todos os % do método ativo. Dois ramos: HPV (9 linhas de params) ou Citologia (12+ linhas).
9. **`output$ccu_cards`** — renderiza o layout final: extrai demandas (`colpo_demand`/`cito_demand`/`ap_demand`), capacidades (`cap_*`), aplica `ceiling` aos `*_needed`, e monta os 4 cards em `ccu-section-2` + `.ccu-note` com o HTML de `build_notes()`.

**Helpers locais duplicados:**
- `pick_all()` — definido mas **nunca chamado** (código morto, idêntico ao que existe em `11_mod_resumo_geral.R`).
- `fmt_or_dash()`, `num_or_na()`, `pct2()` — wrappers triviais também duplicados em `11_mod_resumo_geral.R`.

**Dependências:** `cc_engine_settings`, `cc_engine_run` (engine); `fmt_int`, `%||%`, `cc_with_tt` (utils); `cc_TOOLTIPS$equipamentos_ccu` (constants).
<!-- DOC:14_mod_equipamentos.R:END -->

### `R/15_mod_fluxo.R` — aba Pathway
<!-- DOC:15_mod_fluxo.R:START -->
Aba "Pathway" — diagrama do fluxo de rastreamento + diagnóstico com contagens absolutas e percentuais em cada nó. Renderizada pelos helpers `render_pathway_hpv()` / `render_pathway_cytology()` de `01_utils_cc.R`; o módulo aqui só constrói a **lista de contagens por nó** (`fluxo()`) e escolhe qual renderizador chamar.

**UI:** injeta `www/pathway.css` e um snippet JavaScript com `MutationObserver` que aplica `transform: scale(...)` a qualquer elemento `.pathway-scalable` proporcional à largura do contêiner (baseline 1200×780). Mantém o diagrama responsivo sem reflow dos nós absolutamente posicionados. Em seguida: `cc-page-header` com título "Pathway" e um `uiOutput("pathway_ui")`.

**Server — 7 blocos:**

1. **País selecionado** — `country_code()` / `br_code` / `is_brazil()`. Bloco idêntico aos de `11_mod_resumo_geral.R` e `14_mod_equipamentos.R`.
2. **Rótulo geográfico** — `country_label(code)` (lookup defensivo) e `geo_label()` reactive. `geo_label` devolve **uma única string** com o nível mais fino com seleção (Municipality > Health region > Macro-region > State > "Brazil"), padrão `"<nível>: <primeiro>"` ou `"<nível> (n=K): <primeiro>"` com múltiplas seleções. **Diferente** do `geo_desc` concatenado das outras abas — aqui a legenda do diagrama precisa ser curta.
3. **`cfg_reactive`** — empacota ~40 campos de `input_global()` em `cc_engine_settings()`. Padrão replicado entre módulos (candidato a helper compartilhado).
4. **`engine_res`** — roda `cc_engine_run(df_completo, cfg, pop_mun_regional)`. Apenas `$metrics$rastreada` (HPV) ou `$metrics$cit_rastreamento` (Citologia) é consumido daqui.
5. **`fluxo`** — reactive principal. A partir de `N` rastreada e dos percentuais de `input_global()`, **reaplica o modelo clínico** para obter contagens em cada nó do diagrama (3 braços HPV / 3 braços cito + colposcopia por braço + biópsia + desfechos NIC1-neg / NIC2/3 / câncer). Função interna `p2(x, def)` converte `%` em proporção defensivamente. O dicionário de chaves devolvido (`women_screened`, `negative`, `hpv16_18`, `cito_pos/neg`, `colpo_pos/neg_*`, `biopsy_neg_cin1_*`, `cin2_3_*`, `cancer_*` etc.) é o **contrato** consumido pelos renderizadores em `01_utils_cc.R`.
6. **`caption_txt`** — legenda sob o diagrama: `"Method: <HPV|Cytology> screening every <5|3> years. Age range: X–Y years. Coverage: Z%. Population source: World Population Prospects 2025. HPV parameters: …"`.
7. **`output$pathway_ui`** — valida `f$women_screened > 0` e `method ∈ {hpv, cytology}`; despacha para `render_pathway_hpv(f, geo, cap)` ou `render_pathway_cytology(f, geo, cap)`. Se inválido, mostra mensagem centralizada "Flow not available for current settings."

**Dependências:** `cc_engine_settings`, `cc_engine_run` (engine); `render_pathway_hpv`, `render_pathway_cytology`, `%||%` (utils); `data.table`.

**Observações:**
- O bloco `fluxo()` **duplica parcialmente** a lógica de `modelo_hpv()` e da parte inline de Citologia em `cc_workup_metrics()`. É proposital — aqui a granularidade é maior que a exposta pelo engine (ex.: `colpo_asch_pos/neg`, `biopsy_neg_cin1_asch`). Candidato de refatoração: o engine expor esses nós diretamente e o módulo só renderizar.
- Blocos 1–3 são duplicações das abas Summary/Equipment (país, `geo_*`, `cfg_reactive`). Reforça o pedido de helper compartilhado `cc_cfg_from_input(g)`.
- `geo_label` e `geo_desc` resolvem geografia com regras diferentes (uma string curta vs. concatenação). Coexistem por necessidade distinta; cada qual justificada.
<!-- DOC:15_mod_fluxo.R:END -->

### `R/16_mod_capacidade.R` — aba Capacity
<!-- DOC:16_mod_capacidade.R:START -->
Aba "Capacity" — única aba que **só faz sentido para o Brasil** (depende de SUS/SIA 2024). Cruza produção real × necessidade estimada para os 4 procedimentos do rastreamento (citologia, colposcopia, biópsia, EZT). Fora do Brasil renderiza placeholders ("–") com mensagem informando a limitação.

**UI — 2 sections:**

1. **Procedure delivery** (`ccu-section-1`) — 4 cards SVG verde-escuros com totais SUS/SIA 2024 (Cytology / Colposcopy / Biopsies / EZT). Título dinâmico: `"Procedure delivery — <geo mais específica>"` ou `"Procedure delivery — Brazil · Total/SUS-dependent population"` quando não há filtro geo.
2. **Comparison** (`comp-cards-row`) — 4 cards brancos com **% de cobertura por procedimento** (`realizado / necessário × 100`), barra horizontal proporcional (verde-claro <50%, verde-médio 50–99%, verde-escuro ≥100%, com seta de overflow se passar de 100%) e sub-linha `"Actual: X · Estimated: Y"`. Título dinâmico discrimina o `sia_geo_ref` ativo (place of care vs. residence). Logo abaixo dos cards, **nota metodológica HTML** com cenário (ages + coverage + intervalo) + todos os % do método ativo (HPV: 9 linhas; Citologia: 12+ linhas).

**Server — 11 blocos:**

1. **País selecionado** — `country_code()`, `br_code` (resolvido 1×), `is_brazil()`. Padrão idêntico aos módulos Summary/Equipment/Pathway.
2. **Helpers de geografia** — `cap_geo_label(g)` devolve a string com o nível mais fino com prefixo `"Type: nome"`; `pick_all()` é código morto (mesmo padrão de 11/14); `country_label(code)` faz lookup defensivo em `dim_country`; `geo_name_label(g)` é variante de `cap_geo_label` **sem prefixo** (usada no título da Section 1).
3. **`output$geo_desc`** — subtítulo do header (mesmo padrão concatenado de Summary/Equipment).
4. **Engine** — `cfg_reactive()` empacota `input_global()` em `cc_engine_settings()`; `engine_res()` roda `cc_engine_run(df_completo, cfg, pop_mun_regional)`. Aqui só `$metrics` é consumido.
5. **`sia_filtered`** — lê `sia_cc_resumo` (formato novo `sus_proc_resumo`: colunas `geo_ref`, `categoria`, `total_all`, `total_25_64`, UF, Macro, Região, Município), valida colunas obrigatórias (stop com diff em caso de missing), filtra por `geo_ref` (care/res) e pela cascata UF→Macro→Região→Município. Devolve NULL se não-Brasil ou se filtro zerar.
6. **`realized_cap`** — agrega o data.table filtrado em escalares por categoria via `cc_capacity_from_sia(dt, ano_cmp_ref = 2024L)`. Resultado consumido pelos cards da Section 1.
7. **`comp_dt`** — chama `mod_capacity_compare(engine_res, realized_cap)` (engine) e filtra os 4 itens exibidos: `citologia_total`, `colposcopia_total`, `biopsia_total`, `tratamento_total` (anatomo é omitido na UI, mas o engine retorna).
8. **Helpers de KPI + títulos dinâmicos** — `txt_na_val/sub` (placeholders fora do Brasil), `cap_sub()` (`"SUS/SIA 2024 — <geo>"`), `output$delivery_title`, `output$comp_title`.
9. **Cards Section 1 (delivery)** — pares `output$<item>_value` / `<item>_sub` para cada um dos 4 procedimentos. Estrutura repetitiva: testa `is_brazil()`, lê do `realized_cap()`, formata com `fmt_int` ou devolve "–".
10. **Cards Section 2 (comparison)** — `ratio_vals(d_all, item_name)` extrai `pct/pct_num/sub` de uma linha de `comp_dt`; `bar_indicator(pct_num)` monta a barra horizontal com classes CSS por faixa (`bar-fill-low/mid/ok`) e overflow visual se ≥100%; `comp_card_ui(title, rv, tooltip)` monta um cartão branco; `output$cap_comp_cards` orquestra os 4 cards (ou placeholders fora do Brasil).
11. **Nota metodológica** — `output$cap_comp_note` imprime HTML (3 linhas separadas por `<br/>`) com resumo do cenário e parâmetros completos do método ativo. Usa helpers locais `fmt_or_dash`/`num_or_na`/`pct2` (duplicados em 11/14).

**Dependências:** `cc_engine_settings`, `cc_engine_run`, `cc_capacity_from_sia`, `mod_capacity_compare` (engine); `fmt_int`, `fmt_rate`, `%||%` (utils); `cc_TOOLTIPS$capacidade_ccu` (constants); `data.table` (filtros).

**Observações:**
- Único módulo que **valida o schema** do dataset SIA (linhas 269–275): se faltar coluna esperada (`geo_ref`, `categoria`, `total_all`, etc.), `stop()` explícito.
- Bloco 1 (país) e 3 (`geo_desc`) replicados pela 4ª vez (Summary/Equipment/Pathway/Capacity) — confirma a urgência de helpers compartilhados `cc_country_info(g)` e `cc_geo_desc(g, mode)`.
- `cap_geo_label` (com prefixo "Type: nome") e `geo_name_label` (sem prefixo) coexistem no mesmo arquivo — possível unificar com argumento `prefix = TRUE/FALSE`.
- Anatomo (`anatomo_total`) é calculado pelo engine via `mod_capacity_compare` mas **propositalmente excluído** dos cards (linha 314 filtra os 4 itens). Decisão de UX (4 cards × 4 colunas).
- A nota metodológica em HTML é praticamente idêntica ao `build_notes()` da aba Equipment — mais um candidato a helper compartilhado em utils.
<!-- DOC:16_mod_capacidade.R:END -->

### `R/17_mod_compare.R` — aba Compare
<!-- DOC:17_mod_compare.R:START -->
Aba "Compare" — apresenta dois cenários **lado a lado** numa tabela única com pílulas de Δ% (B vs A). Scenario A herda toda a configuração da sidebar (`input_global`); Scenario B tem painel próprio à direita (acordeão Population / Screening protocol / Screening coverage / Resources) cujos IDs são sufixados `_b`. **Geografia é compartilhada**: país e filtros do Brasil são lidos sempre de A.

**Pipeline:**
```
input_global  → cfg_a → res_a → dt_a ─┐
input$*_b     → cfg_b → res_b → dt_b ─┴── tabela comparativa (Δ B vs A)
```

**UI — 2 colunas:**

1. **`compare-results`** (esquerda) — `cc-page-header` com título "Compare" + subtítulo dinâmico (`result_desc` = `geo_text()`); `scenario_badges` (cards-resumo de A e B com método/idades/cobertura); `compare_table` (a tabela em si).
2. **`scen-b-panel`** (direita) — header "Scenario B" + subheader "Geography is shared with Scenario A." + `geo_locked` (read-only) e 4 acordeões:
   - **Population** — `pop_b_ui` mostra o `custom_pop` herdado quando pop_mode = "other", senão só nota "GLOBOCAN (inherited)".
   - **Screening protocol** — `screen_method_b` (HPV/Cytology); idades (`target_age_min/max_b`); fonte de parâmetros + painel Customize com 16 inputs HPV ou ≈14 inputs citologia, **Lock proportions** + Reset.
   - **Screening coverage** — slider `coverage_b` 0–100%.
   - **Resources** — `cap_unidade_b` (Day/Week/Month/Year) + 4 inputs de capacidade (colposcópio, colposcopista, citopato, patologista). Anualizadas em `cfg_b` via `cap_mult_b`.

**Server — 14 blocos:**

1. **Helpers** — `val_or` / `fmt_or_dash` (duplicados de 11/14/16) + `fmt_dec1` (1 casa decimal — exclusivo deste módulo, usado nos cards de RH).
2. **Identificação do Brasil** — `br_code` resolvido 1× via `dim_country` + `is_brazil()` reactive (5ª duplicação no codebase).
3. **Resolvedores de presets** — `get_hpv_preset_params(src)` e `get_cito_preset_params(src, uf)` como funções puras (espelham `load_*_preset` do módulo de filtros, mas sem efeito colateral; usadas só no debug — os observers do Bloco 5 fazem update direto).
4. **Rótulo geográfico (`geo_text`)** — **5ª implementação distinta de geo label** no app: concatena "Brazil – Total/SUS – primeiro nível com seleção (n=K)". Diferente das 4 anteriores (geo_desc 11/14/16, geo_label 15, cap_geo_label 16, geo_name_label 16).
5. **Sincronização Preset → inputs B** — 2 observers (HPV e Citologia) que disparam `updateNumericInput` em todos os 16 inputs `_b` quando o usuário troca a fonte. Citologia escuta `filt_uf` herdado de A.
6. **Reset** — 2 observers (`reset_params_b` / `reset_params_cito_b`) que restauram `HPV_DEFAULTS` / `CITO_DEFAULTS`.
7. **Auto-balance "Lock proportions"** — `clamp100_b`, `approx_eq_b`, `lock_pair_b`, `lock_triplet_b` (espelham 10) com estado próprio em `rv_lock_b` e flag `which_method` para evitar disparos cruzados HPV/Cito. Aplicado a 3 pares HPV + 3 trios HPV + 3 trios Cito.
8. **Validação soma = 100%** — `params_alert_b` e `params_alert_cito_b` exibem alertas vermelhos quando algum grupo de proporções deixa de somar 100 (tol 0.1).
9. **`cfg_a`** — traduz `input_global()` em `cc_engine_settings(...)` — espelho do reactive `cfg` de `11_mod_resumo_geral.R`. **5ª replicação** do padrão.
10. **`cfg_b`** — espelho de `cfg_a` mas lê os `input$*_b`. Capacidades são anualizadas via `cap_mult_b` (Day×240, Week×48, Month×12, Year×1). Branch HPV vs Cytology.
11. **Engines** — `res_a` / `res_b` rodam `cc_engine_run` com `tryCatch` (erro → NULL); `dt_a` / `dt_b` aplicam `cc_engine_summary_dt` (1 linha cada).
12. **UIs auxiliares** — `geo_locked` (geo herdada read-only), `pop_b_ui` (custom_pop ou nota GLOBOCAN), `result_desc` (subtítulo do header).
13. **Scenario badges** — 2 cards "scen-badge" lado a lado com método + ages + coverage. Helpers internos `method_label`, `age_label`, `cov_label` tratam o fallback de 1ª render (dt = NULL).
14. **Tabela comparativa** — 4 grupos (Target population / Work-up / Treatment & follow-up / Resources needed). Helpers `get_num(dt, col)`, `get_hr(res, col)`, `delta_pill(va, vb)` (pílula ▲ +X% / ▼ -X% / — neutra), `row(label, col, ...)`, `group_row(label, colspan=4)`. Caso especial: quando A e B têm métodos diferentes (HPV vs Cito), a linha de citologia adota o rótulo de A e o follow-up vira "Follow-up test" + "Follow-up colposcopy" genéricos.

**Dependências:** `cc_engine_settings`, `cc_engine_run`, `cc_engine_summary_dt` (engine); `fmt_int` (utils); `HPV_PRESETS`, `HPV_DEFAULTS`, `CITO_PRESETS_META`, `CITO_DEFAULTS` (constants).

**Observações:**
- **5ª replicação** do padrão `cfg`/`country_code`/`is_brazil` (11/14/15/16/17) — confirma que o helper compartilhado `cc_cfg_from_input(g)` deveria ser priorizado.
- **5ª implementação** distinta de "rótulo geográfico" no app (`geo_text` aqui) — fortalece a pendência de unificar tudo em `cc_geo_label(g, mode, prefix, sep)`.
- **Replica os 3 helpers do bloco "Lock proportions"** de `10_mod_filters_cc.R` praticamente linha-a-linha (`lock_pair`/`lock_triplet`/`safe_update_num`). Refator: extrair um helper `cc_make_locks(session, suffix = "_b", state_rv = ...)` resolveria os 2 lugares.
- **`get_hpv_preset_params` e `get_cito_preset_params` são definidos mas nunca chamados no fluxo principal** — os observers do Bloco 5 fazem o lookup inline. Resíduo de uma versão anterior — candidato a remoção.
- O Compare é o **único módulo** que usa `cc_engine_summary_dt` (achatamento em data.table de 1 linha) — foi para esse caso de uso que a função foi criada no engine (vide INVENTORY).
- **Limitação de UX**: B não permite escolher UF/filtros geográficos diferentes de A. Se o usuário quisesse comparar "SP vs Brasil inteiro", teria que rodar em duas sessões.
<!-- DOC:17_mod_compare.R:END -->

### `R/18_mod_maps.R` — aba Maps
<!-- DOC:18_mod_maps.R:START -->
Aba "Maps" — coroplético interativo (`leaflet`) com a produção SUS/SIA 2024 em 4 níveis geográficos. **Aba só-Brasil**: fora do Brasil renderiza basemap em branco com mensagem amigável (`validate(need(is_brazil(), ...))`). Único módulo que carrega `{sf}` + `{leaflet}` (peso considerável, mas as transformações WGS-84 são feitas só uma vez por sessão).

**Particularidade arquitetural — não usa o engine.** Diferente de Summary/Equipment/Pathway/Capacity/Compare, este módulo **não roda `cc_engine_run`**. Trabalha direto sobre `sia_cc_resumo` agregando ao nível desejado. Consequência: nenhum reactive `cfg` aqui (rompe a 5ª replicação que se via nos demais).

**UI — header compacto + leaflet:**

- **Header (`maps-header`)** — 3 controles: `metric` (citologia/colposcopia/biópsia/anatomo/tratamento), `granularity` (estado/macro/região/município) e checkbox `per100k` ("Per 100k women 25–64"). À direita, `map_note` (texto de status dinâmico) + `reset_btn_ui` (botão "↑ Reset zoom" condicional).
- **Mapa (`leafletOutput`)** — preenche `calc(100vh - 210px)`. Polígonos clicáveis (drill-down).

**Server — 12 blocos numerados:**

1. **Setup único (1× por sessão)** — `sf::st_transform(..., 4326)` em todos os 4 shapefiles para WGS-84 (leaflet exige lon/lat). Cache fora dos reactives evita refazer a cada render.
2. **Hierarquia & denominador** — `mun_hier` é a tabela canônica que liga município → região → macro → UF (uf_codigo derivado dos 2 primeiros dígitos do `mun_code6`, convenção IBGE). `pop_denom_mun` é a soma de mulheres 25–64 por município, já merge-ada com `mun_hier` para permitir reagregação a qualquer nível sem refazer a soma.
3. **Lookups para resolver filtros da sidebar** — A sidebar entrega NOMES (`"MG"`, `"Macro Centro-Sul"`); o sf carrega CÓDIGOS. `mac_nome_to_codigo` e `uf_codigo_to_nome` traduzem entre eles. `mac_to_reg` deriva de `mun_hier` (1 macro → N regiões).
4. **`is_brazil` reactive** — gating principal (6ª replicação no codebase).
5. **Estado de drill-down** (`reactiveValues`) — local ao módulo, independente da sidebar. Quando a sidebar muda os filtros geográficos, drill é zerado (a sidebar reancorou a geografia).
6. **`auto_geo` reactive** — coração da lógica de granularidade. Resolve em uma passada quem manda na visualização, em prioridade decrescente: (1) drill local; (2) sidebar (do mais específico ao mais geral — região → mostrar municípios; macro → mostrar regiões; UF → mostrar macros); (3) seletor manual.
7. **`map_data` reactive** — pipeline central. (a) gating por Brasil; (b) filtra `sia_cc_resumo` por `geo_ref` + `categoria` e agrega ao nível município (granularidade base); (c) reagrega ao nível alvo + escolhe o sf correspondente; (d) aplica filtro do auto_geo (mantém só polígonos do recorte); (e) se `per100k`, divide pela população 25–64 do mesmo nível × 100 000 (NA quando denom é 0/NA, evita Inf); (f) anexa `map_key`/`map_value`/`map_label` ao sf preservando ordem original (leaflet é sensível a ordem após merge).
8. **Clique no mapa → drill-down** — `input$map_shape_click$id` recebe o `layerId` do polígono (=`map_key`). Cada nível desce para o seguinte: estado → macro → região → município (folha, sem drill adicional). Usa `isolate()` para evitar invalidar o map_data prematuramente.
9. **Reset drill-down** — observer do botão "↑ Reset zoom".
10. **Botão reset (UI condicional)** — só aparece quando há drill ativo.
11. **`map_note`** — string de status acima do mapa: sempre menciona "SIA 2024" + tipo de geo_ref (place of care/residence) + sufixo dinâmico ("drill-down active · click to zoom further" / "level auto-set by sidebar · click to zoom" / "click a polygon to drill down").
12. **Choropleth (`renderLeaflet`)** — paleta teal institucional em 7 bins (alinhada ao CSS); curto-circuito de "no data" → basemap puro centrado no Brasil (lon -52, lat -14, zoom 4); formatter de tooltip varia entre 1 casa decimal (taxa) e inteiro com vírgula (total); labels HTML por polígono com nome + valor + hint de drill (omitido em município); `addPolygons(layerId = ~map_key)` é a chave que habilita `input$map_shape_click`; `fitBounds` automático no bbox do sf filtrado (zoom ao mudar nível).

**Dependências:** `sf` (st_transform, st_drop_geometry, st_bbox), `leaflet` (renderLeaflet, addPolygons, addLegend, addProviderTiles), `htmltools` (HTML, htmlEscape), `data.table` (agregações), `%||%` (utils).

**Args do server** (todos vindos de `app.R`): `sia_cc_resumo`, `pop_municipio_regional`, `geo_municipios`, `geo_regioes_saude`, `geo_macrorregioes`, `geo_estados`, `input_global`, `br_code`.

**Observações:**
- 6ª replicação de `is_brazil` (mas sem `cfg`/`country_label`/`geo_desc` — o módulo não usa o engine, então a duplicação é menor que nos outros).
- O par `mun_hier` + `pop_denom_mun` é uma utilidade que poderia migrar para `02_engine_capacity_cc.R` ou `01_utils_cc.R` (toda análise por nível geográfico Brasil precisa dessa tabela; hoje está construída inline aqui).
- Nenhum dos 4 padrões anteriores de "rótulo geográfico" (`geo_desc`/`geo_label`/`cap_geo_label`/`geo_name_label`/`geo_text`) aparece aqui — o status do mapa usa apenas o tipo de `geo_ref` + sufixo do drill, sem concatenar nomes de UF/macro/região.
- Convenção IBGE explorada: `uf_codigo` derivado por `as.integer(substr(mun_code6, 1L, 2L))`. Se mudar a fonte de geometria, esse pressuposto pode quebrar — adicionar como assert seria boa prática.
- Drill-down por clique é uma pattern de navegação que não existe em nenhum outro módulo. Se o app crescer, vale considerar generalizá-la num helper.
<!-- DOC:18_mod_maps.R:END -->

### `R/19_mod_peers.R` — aba Peer Analysis
<!-- DOC:19_mod_peers.R:START -->
Aba "Peer Analysis" — comparação de **municípios brasileiros** em grupos formados por clusterização não-supervisionada (PAM — *Partitioning Around Medoids*) sobre população-alvo, % SUS-dependente e estrutura etária 25–64. O dataset `peers_data` (gerado em `data-raw/09_prepare_peers.R`) traz por município: scores de produção/necessidade (`score_geral`, `score_cito`, `score_colpo`, `score_biopsia`, `score_ezt`, todos saturados em 1), `rank_grupo` / `n_grupo`, `gap_mediana` e `gap_top25` (distância para o benchmark do grupo) e flag `prioritario` (bottom-10% ou 5 piores).

**Particularidades arquiteturais:**

- **Aba só-Brasil de fato** — `peers_data` é construído a partir de municípios BR; fora do BR a aba abre mas não há nada útil para mostrar (nenhum gating explícito é feito, apenas a sidebar global limita o universo).
- **NÃO usa o engine** (`cc_engine_run` não é chamado) — os scores foram pré-calculados no ETL com **cenário fixo** (INCA 2019, 70% cobertura, idades 25–64, intervalo de 3 anos), independente do que estiver na sidebar global.
- **Sidebar interna própria** com 2 controles (`grupo_sel` = peer group, `cor_sel` = faixa de score).
- **Drill-down por clique em linha de DT** — segundo módulo (depois do `mod_maps`) com ação reativa baseada em clique. Único uso de `*_rows_selected` no app.

**UI — sidebar interna + painel principal:**

- **Sidebar interna** (230px, `--cc-teal-surface`): título "PEER ANALYSIS", aviso de que o escopo geográfico é controlado pela sidebar global, `grupo_sel` ("All groups" ou um peer group), `cor_sel` ("All" / "Low <0.50" / "Moderate 0.50–0.79" / "Good ≥0.80") + nota com fontes (IBGE/DATASUS, SIA/SUS) e parâmetros do cenário pré-calculado.
- **Painel principal**: `cc-page-header` com título "Peer Analysis" + subtítulo dinâmico (`uiOutput("header_desc")`); card metodológico (`uiOutput("method_explain")`); seção condicional de detalhes (`uiOutput("detail_section")`, aparece quando há município selecionado, **acima** da tabela); card "Priority Municipalities" com `DT::dataTableOutput("tabela_prioritarios")`.

**Server — 12 blocos:**

1. **Setup do dataset** — retrocompatibilidade `gap_p25` → `gap_top25` (versões antigas do `.rds`).
2. **Helper `%||%` local** — variante string-aware do null-coalescing (trata também `""` como ausente, porque os `selectInput` devolvem string vazia em "All").
3. **`g <- reactive(input_global())`** — encapsula a sidebar global. Apenas filtros geográficos (`filt_uf`/`filt_macro`/`filt_reg`/`filt_mun`) são consumidos — não há `cfg`/método/cobertura aqui (não usa engine).
4. **Popular `grupo_sel`** — observer que filtra `dt` pelo escopo geo e atualiza as choices de peer group, preservando a seleção atual se ainda for válida.
5. **`mun_click` (drill-down state)** — `reactiveVal` que guarda o município escolhido por clique. Observer paralelo zera o clique sempre que filtros geo, peer group ou cor mudam (drill não pode "ficar preso" fora do recorte).
6. **`tabela_prio_data`** — reactive central: filtra geo + peer group + faixa de score (4 ramos: red <0.5, yellow 0.5–0.79, green ≥0.8) e ordena por `gap_mediana` desc.
7. **`observeEvent(input$tabela_prioritarios_rows_selected)`** — pega o índice da linha clicada, resolve em `tabela_prio_data()` corrente e salva (nome, uf) em `mun_click`. Defesas contra deselect (NULL/length-0) e índice fora do range.
8. **`municipio_sel`** — resolve o "município ativo" para os cards de detalhe, com prioridade: (1) clique na tabela; (2) `filt_mun` único na sidebar global. NULL → cards ocultos.
9. **`output$header_desc`** — subtítulo do header. Sem município ativo: contagem de prioritários no escopo + descrição do escopo (UF única, "K states" ou "Brazil"). Com município ativo: nome + UF + grupo PAM.
10. **`output$method_explain`** — card explicativo inline (Score / Gap / Rank / Peer groups). Os números #municipalities (n_mun) e #grupos (n_grupos) refletem o **universo de comparação no recorte UF** — propositalmente NÃO aplica filtros de macro/região/município.
11. **`output$tabela_prioritarios`** — `DT::datatable` com 7 colunas (Municipality / State / Peer Group / Health Region / Rank / Score / Gap), seleção single-row, ordenação default por Gap desc. Coluna Score recebe `formatStyle` com cores semânticas por faixa (hex literais — DT não consome CSS vars).
12. **`output$detail_section`** — cards condicionais (só renderiza se `municipio_sel()` não-NULL): Row 1 = "Municipality Profile" (5 col) + "Peer Ranking" (7 col, com scores por procedimento), Row 2 = "Gap to Best Performers" (4 col, com gap→mediana, gap→top25, frase descritiva e alerta de prioritário). Helpers locais `.gap_linha` (pílula colorida com sinal +/−) e `cor_score` (cor da faixa).

**Helpers globais do arquivo (fora do moduleServer):**

- `.peers_kv(label, valor)` — linha key-value horizontal nos cards "Profile".
- `.peers_score_row(label, val)` — linha de score por procedimento no "Peer Ranking" (cor por faixa danger/warning/success).

**Dependências:** `DT` (datatable + formatStyle/Round); `data.table` (filtros); `shiny`/`htmltools` (UI). **Não usa o engine.** **Não usa** `01_utils_cc.R` (`%||%`, `fmt_int`) — implementa wrappers locais.

**Observações:**

- 1ª aba **sem nenhum dos 6 padrões** (`country_code`/`is_brazil`/`country_label`/`cfg_reactive`/`engine_res`/`geo_*_label`) — porque não usa o engine e o gating de Brasil é implícito (dataset). Diferente do `mod_maps` (que ainda replica `is_brazil`).
- **Clique em linha (`*_rows_selected`)** — segunda navegação por clique no app (a primeira é `mod_maps`). Só `mod_compare` tem outra forma de "estado local" (`rv_lock_b`), mas sem clique. Se a UX expandir a pattern, vale generalizar.
- **`%||%` local** — variante string-aware redefinida no closure. Diferente do `%||%` global de utils que só checa NULL/length. Candidato a expor a versão string-aware em utils como `%||nz%` ou parametrizada.
- **Cenário pré-calculado em ETL** — score/gap não respondem aos sliders/preset da sidebar (HPV/Cito, cobertura, idade). Decisão de UX (estabilidade dos peer groups), mas pode confundir usuários que esperam reatividade. Tooltip explicativo seria útil.
- **`output$method_explain` aplica só `filt_uf`** — não acompanha macro/região/mun. Decisão semântica (descrever universo de comparação ≠ foco visual), mas merece doc explícita; o usuário pode estranhar que o número de "municipalities in scope" não muda ao filtrar por macro.
- **Cores hex literais no `formatStyle`** — DT não consome CSS vars. Mantém comentário no código mapeando aos tokens (`#FDECEA` = `--cc-danger-bg`, `#FFF3E0` = `--cc-warning-bg`, `#E6F5EE` = `--cc-success-bg`).
- **Helpers `.peers_kv` / `.peers_score_row`** ficam fora do `moduleServer` (escopo de arquivo). Padrão diferente dos demais módulos, que costumam definir helpers DENTRO do server. Ambas as abordagens são válidas; vale uniformizar em revisão final.
<!-- DOC:19_mod_peers.R:END -->

### `R/20_mod_tabela_detalhada.R` — aba Detailed table
<!-- DOC:20_mod_tabela_detalhada.R:START -->
**Funções públicas:** `mod_detailed_table_ui(id)`, `mod_detailed_table_server(id, pop_mun_regional, sia_cc_resumo, regional_sus_map, dim_country, input_global)`.

**Particularidades:**
- Aba **só-Brasil** (igual a Capacity / Maps); fora do BR exibe placeholder.
- Requer **pop IBGE** (total ou SUS). NÃO funciona com pop manual — `custom_pop` não pode ser distribuído entre municípios.
- **NÃO consome a saída por país do engine.** Em vez disso, roda o engine UMA vez com `custom_pop = 1` (cenário "per-capita") e depois multiplica as 4 taxas (tests/colpo/biopsia/EZT por mulher-alvo) pela pop real de cada linha. Vantagem: 1 chamada cobre N linhas.
- **Granularidade dinâmica**: usuário escolhe "By municipality" (sempre mun) ou "By smallest selected level" (UF→Macro→Reg→Mun conforme cascata).
- Download CSV pt-BR-friendly: separador `;`, decimal `,`, BOM UTF-8.
- DT com `formatStyle` por faixa de cobertura (<50/<100/≥100) usando hex literais (DT não consome CSS vars).

**UI** (`mod_detailed_table_ui`): cabeçalho `Detailed table` + subtítulo dinâmico (`output$geo_desc`) + container `output$body` (montado no server).

**Server (11 blocos numerados):**
1. **Gating triplo** — `br_code` + `is_brazil` + `pop_is_manual` + `available()` (gate central).
2. **`output$geo_desc`** — concatena "Brazil - UF - Macro - Reg - Mun"; fora do BR mostra país. **6ª implementação** distinta de "geo label" no app.
3. **`effective_gran`** — resolve "By municipality" vs "By smallest selected level" via cascata da sidebar.
4. **`per_capita`** — roda engine 1× com `pop_mode="other"` + `custom_pop=1` → resultado é interpretado como **taxa por mulher-alvo**. Devolve `list(method, tests, colpo, biopsia, ezt)`.
5. **`pop_agg`** — população filtrada por idade + cascata geo, agregada por nível.
6. **`sia_agg`** — produção SIA filtrada por `geo_ref` + cascata, agregada e pivotada (long → wide com 4 colunas: citologia/colposcopia/biopsia/tratamento).
7. **`tabela_base`** — **núcleo do módulo**: pop × per_capita → 4 colunas `*_needed`; pop × SIA → 4 colunas produzido; produzido / necessário → 4 colunas `cov_*` em %. `safe_pct` evita divisão por zero.
8. **Helpers de exibição** — `params_line` (linha de cenário em itálico), `col_labels` (rótulos por nível, atualmente código morto), `build_display_dt` (reordena geo cols + renomeia para inglês).
9. **`output$body`** — gating UI: placeholders para país≠BR ou pop manual; senão monta radio + linha de params + botão CSV + DT + nota.
10. **Render principal** — `output$params_text` + `output$table` (DT com sort por cobertura asc, `formatCurrency` para inteiros, `formatStyle` colorido por faixa de cobertura).
11. **`output$download_csv`** — `downloadHandler` com nome `detailed_table_<method>_<gran>_YYYYMMDD.csv`.

**Observações:**
- 7ª replicação do `is_brazil` (mas único módulo onde `available()` consolida 2 gates: BR + pop não-manual).
- `col_labels` é definida mas nunca chamada (`build_display_dt` faz a renomeação inline) — código morto.
- O preset **Citologia** não é resolvido para label legível em `params_line` (só HPV) — pequena assimetria.
- Drill-down por clique não é suportado (DT é só listagem). Diferente de `mod_peers` que usa `_rows_selected`.
<!-- DOC:20_mod_tabela_detalhada.R:END -->

### `R/99_mod_sobre.R` — aba About
<!-- DOC:99_mod_sobre.R:START -->
**Funções públicas:** `mod_sobre_ui(id)`, `mod_sobre_server(input, output, session)`.

**Particularidades:**
- **Único módulo 100% estático** do app — nenhum reactive, observer ou render. O server é uma função vazia.
- **Único módulo cuja função server NÃO usa `moduleServer(id, ...)`**: é uma função top-level com a assinatura clássica `mod_sobre_server(input, output, session)`. Funciona porque não há nada para namespacear (sem `ns()`, sem outputs).
- UI usa `fluidPage` (em vez do `tagList(div(...))` dos demais módulos) — herança do template inicial.
- Conteúdo bilíngue: textos em **inglês** (público IARC/internacional); créditos institucionais e logos `www/logo-iarc.svg`, `www/logo-fsp.svg`, `www/logo-ConeCta-SP.png`.
- `format(Sys.Date(), "%d/%m/%Y")` em "Last update" é avaliado **na renderização**, não em build time — exibe a data corrente da sessão.

**Estrutura:**
- Header `About NEEDS-BR` + tagline.
- Coluna esquerda (8/12): blocos `Overview`, `Main features` (lista de 6 itens), `Data and assumptions`.
- Coluna direita (4/12): card de créditos com Authors / Affiliations / Version (1.2) / Last update / Funding (FAPESP grants 2025/08308-1, 2025/00444-3, 2021/11794-4) + 3 logos institucionais.

**Observações:**
- Padrão divergente do resto do app (sem `moduleServer`, sem `tagList`). Em revisão final, considerar uniformizar — mas a divergência é benigna (pure-static).
- Versão hardcoded ("1.2") — pode ser parametrizada via constante em `00_constants_cc.R` para evitar drift entre arquivos.
<!-- DOC:99_mod_sobre.R:END -->

### `data-raw/` — pipeline ETL
<!-- DOC:data-raw:START -->
Cada script desta pasta é responsável por preparar um conjunto de objetos `.rds` em `data/`, que são carregados em bloco no início de `app.R`. Os scripts são independentes, mas há uma ordem natural de execução (o `06_make_all_cc.R` orquestra tudo). Documentação script a script:

#### `data-raw/01_prepare_cc.R` — bases globais (GLOBOCAN + WPP)

**Entrada (planilhas em `data-raw/`):**
- `WPP2019_POP_ANNUAL_POPULATION.csv` — população por país × idade × ano (ONU/WPP), CSV separado por `;`
- `Globocan_2022_cervical.xlsx` — projeções GLOBOCAN 2022 de câncer de colo do útero (.xlsx, apesar do sufixo do `path_*`)
- `dataset-inc-females-in-2022-cervix-uteri.xlsx` — agregados de incidência por país (GCO/IARC)
- `dataset-mort-females-in-2022-cervix-uteri.xlsx` — agregados de mortalidade por país (GCO/IARC)

**Saída (em `data/`):**
- `df_cc_completo.rds` — tabela granular com população (2022/2025) × incidência/mortalidade × predição GLOBOCAN, por país × faixa etária × tipo
- `df_cc_taxas.rds` — taxas agregadas (incidência + mortalidade) por país (sem desagregação por idade) — alimenta a aba **Epidemiology**

**Pipeline (6 blocos numerados no script):**

1. **População (WPP)** — filtra mulheres, padroniza códigos, agrega por país × idade nos anos 2022 e 2025; junta tudo num `df_infos_pop` (full join).
2. **Globocan câncer de colo** — filtra mulheres, renomeia `year` → `year_prediction`, agrega "World" (1001) por soma e empilha países + World em `df_infos_epidemiologicas_cc`.
3. **Labels** — extrai nomes oficiais da planilha de incidência (preferencial) com fallback para o nome do GLOBOCAN; valida unicidade por código.
4. **Join final** — left-join entre epidemiologia × população por `(population_code, sex_code, age_code)`; gera `df_cc_completo`. Validações: warning se faltar pop em ambos os anos; `stopifnot` para chave única.
5. **Taxas agregadas** — lê planilhas de incidência e mortalidade (sem desagregação por idade), seleciona dinamicamente a coluna de "número" via helper `pick_number_col()` (nome da coluna varia entre vintages do GCO), faz full join e unifica `population_name`. Gera `df_cc_taxas`.
6. **Sanity checks** — avisos no console (não param a execução) sobre presença de "World" e estratos sem população.

**Helpers internos:**
- `fix_population_code(code, year=NULL)` — padroniza códigos entre vintages (`900 → 1001`, `156 → 160` se `year == 2025`); aceita `year` escalar ou vetor do mesmo comprimento.
- ~~`must_have_cols(dt, cols)`~~ — removido em TASK-05; substituído por `cc_check_schema(dt, cols, "<context>")` (de `R/01_utils_cc.R`, sourced no topo do script).
- `pick_number_col(dt)` — descobre nome da coluna de número (varia: `number`, `number_7`, `number_10`, etc., porque o `clean_names()` ajusta de formas distintas conforme o Excel.

**Observações:**
- "World" (1001) é re-derivado por agregação dos países (não usa o World oficial do GLOBOCAN). Garante aditividade com qualquer recorte feito no app, mas pode ter pequena diferença em relação ao oficial.
- A planilha de incidência é lida 2× (no bloco 3 só pelos labels, no bloco 5 com as taxas). Pode-se otimizar lendo 1× e reaproveitando.
- Os dimensionais (`df_dim_country`, `df_dim_age`, `df_dim_type`, `df_dim_year`) **não** são gerados aqui — são produzidos por `data-raw/05_glossary_cc.R` (que lê `df_cc_completo.rds` e extrai os distintos canônicos).
- Compressão `xz` no `saveRDS` reduz ~30–40% em troca de gravação mais lenta (aceitável em ETL offline).

#### `data-raw/02_prepare_BR.R` — bases Brasil (IBGE + ANS + regionalização SUS)

**Entrada (planilhas em `data-raw/`):**
- `IBGE_pop.csv` — população feminina por município × faixa etária (formato wide; separador `;`, encoding Latin-1). 1ª coluna = `"código de 6 dígitos + nome"`; demais colunas = faixas IBGE (`"De 0 a 4 anos"`, ..., `"De 80 anos ou mais"`).
- `ANS_pop.csv` — beneficiárias (sexo Feminino) de planos privados por município × faixa etária (formato wide; `;`, Latin-1). Faixas começam em `"Até 1 ano"` + `"1 a 4 anos"` (separadas).
- `regional_sus.csv` — mapeamento oficial município → região de saúde → macrorregião → UF → região do país (separador `,`, UTF-8). Inclui coluna de população 2022 que é descartada.

**Saída (em `data/`):**
- `pop_municipio_faixas.rds` — IBGE puro, formato long, com `geo_id`/`geo_name`/`faixa`/`from`/`to`/`pop`/`level`.
- `ans_municipio_faixas.rds` — ANS puro, long, com `from`/`to` alinhados ao IBGE (após combinar 0–0 + 1–4).
- `regional_sus_map.rds` — mapa hierárquico SUS, schema `geo_id` + `regiao_pais_*` / `uf_*` / `macro_*` / `regiao_*` / `mun_*`.
- `pop_municipio_faixas_total_sus.rds` — IBGE × ANS combinados, com `pop_total` (IBGE) e `pop_sus` (IBGE − ANS, clamp em 0). Sem regionalização.
- `pop_municipio_regional.rds` — **base central do app só-Brasil**: pop_total + pop_sus + hierarquia SUS. Lida em `app.R`, alimenta `mod_filters` (cascata UF→Macro→Reg→Mun), engine (arg `pop_mun_regional`) e todos os módulos só-Brasil.

**Pipeline (5 blocos numerados no script):**

1. **IBGE — wide → long** (`run_etl_ibge`): força 1ª coluna a `"Município"`, derrete faixas, parsa `from/to` com `parse_faixa_bounds()`, separa `geo_id` (6 dígitos) de `geo_name` (com acentos preservados). Pop em formato pt-BR (`"12.345"`) é convertida para numérica; ausentes viram 0 (não NA, para não propagar em soma).
2. **ANS — wide → long** (`run_etl_ans`): mesmo padrão do IBGE, mas faixas vêm sem o prefixo `"De"` e a 1ª faixa é dividida em `"Até 1 ano"` + `"1 a 4 anos"`. Mapa estático `mapa_faixas` traduz rótulo → `(from, to)`; assert defensivo lista as faixas problemáticas se algo escapar do mapa. Combina 0–0 + 1–4 em `"De 0 a 4 anos"` para casar com o grid IBGE.
3. **Regionalização SUS** (`run_etl_regional`): valida schema (lista de 11 colunas esperadas), descarta coluna de população (fonte canônica é o IBGE), padroniza `geo_id` para 6 dígitos com zfill (`sprintf("%06s", ...)`), renomeia para o esquema usado no app (snake_case com prefixos consistentes).
4. **Montagem**:
   - `build_population_base(ibge, ans)` — merge por `(geo_id, from, to)`, gera `pop_total = pop_ibge` e `pop_sus = pmax(pop_ibge − ANS, 0)`. Município sem ANS = 0 beneficiárias (não some). Aceita `ans = NULL` (degenerado: pop_sus colapsa em pop_total).
   - `build_population_with_regions(pop_base, reg)` — merge com a hierarquia SUS, reordena colunas (geo + pop + UF/Macro/Reg/Mun) e salva o `.rds` central.
5. **Função mestre** `run_prepare_BR()` — orquestra a sequência e salva também o subproduto `pop_municipio_faixas_total_sus.rds` (sem regionalização).

**Helpers internos:**
- `parse_faixa_bounds(lbl)` — converte rótulo IBGE em `(from, to)`. Reconhece "De N a M anos" e "De 80 anos ou mais"; falha com erro claro se o rótulo não bater (assert contra mudança no layout).
- `path_data_raw(...)` / `path_data(...)` — wrappers `here::here()` para caminhos.

**Constantes:**
- `.IBGE_FILE` / `.IBGE_SEP` / `.IBGE_ENC` — parâmetros de leitura do IBGE (constantes no topo do bloco para facilitar ajuste).
- `.REG_FILE` — nome do arquivo de regionalização.

**Observações:**
- **As faixas IBGE e ANS NÃO casam exatamente** — IBGE inicia em "De 0 a 4 anos", ANS separa "Até 1 ano" + "1 a 4 anos". A combinação no bloco ANS é o ponto crítico do alinhamento; se um dia o ANS mudar o grid, o `merge` por `(geo_id, from, to)` em `build_population_base` perde silenciosamente a 1ª faixa.
- **`pop_sus` é uma proxy** — definida como `pmax(pop_IBGE − beneficiárias_ANS, 0)`. Supõe que cada beneficiária ANS é usuária do privado (não capta uso simultâneo SUS+privado).
- **Clamp em 0**: usar `pmax` evita pop_sus negativa em municípios com mais beneficiárias declaradas que população (raro, mas existe — ocorre por desalinhamento temporal IBGE 2022 vs ANS atualizado).
- **Coluna de população do `regional_sus.csv` é descartada** propositalmente — a fonte canônica é o IBGE com desagregação por faixa. Manter 2 fontes de "pop total" geraria drift.
- **`setcolorder` com filtro `%in%`**: caso uma coluna esperada não exista, é omitida em vez de erro. Conveniente em desenvolvimento, mas pode mascarar bug em produção — considerar trocar por `stop()` defensivo na revisão final.
- Validação de schema do `regional_sus.csv` é o único `stop()` por colunas faltantes — IBGE e ANS não validam (confiam no parse de faixa para detectar mudanças).
- "level" é setado como `"municipio"` em todos os outputs, deixando espaço para versões agregadas (UF/macro) sem mudar schema. Hoje, agregação é feita on-the-fly pelos módulos.

#### `data-raw/03_prepare_SUS.R` — produção SUS (SIA-PA + SIH-RD)

Script com **três pipelines independentes** que juntos produzem a base de produção SUS consumida pelo app:

1. **SIA-PA** (ambulatorial): `run_prepare_SUS()` → `data-raw/sia_cc_completo.rds`; `build_sia_cc_resumo()` → `data/sia_cc_resumo.rds`.
2. **SIH-RD** (hospitalar/EZT): `run_prepare_SIH_RD_EZT()` → `data-raw/sih_rd_ezt_completo.rds`; `build_sih_rd_ezt_resumo()` → `data/sih_rd_ezt_resumo.rds`.
3. **Resumo unificado para o app**: `build_sus_proc_resumo()` → **`data/sus_proc_resumo.rds`** (saída final).

**Entrada:** download via `microdatasus::fetch_datasus()` (SIA-PA / SIH-RD) — não há planilhas locais. SIGTAP via `microdatasus::fetch_sigtab()` para nomes de procedimento.

**Saída final consumida pelo app (em `data/`):**
- `sia_cc_resumo.rds` — agregação granular do SIA-PA por (categoria × geo × competência × procedimento × sexo × faixa).
- `sih_rd_ezt_resumo.rds` — agregação granular do SIH-RD (EZT) por (UF × competência × procedimento × geo × faixa).
- `sus_proc_resumo.rds` — **base lida pelo app** (objeto `sia_cc_resumo` em `app.R`, mantido por motivo histórico). Já vem com merge da hierarquia regional (`UF`, `Macrorregiao de Saude`, `Regiao de Saude`, `Municipio`).

**Códigos SIGTAP de interesse (10 categorias):**
- `oci` — oferta de cuidado integrado (4 códigos)
- `coleta` — coleta de material p/ exame citopatológico (1)
- `cito` — exames citopatológicos (2)
- `colposcopia` — colposcopia (1)
- `biopsia` — biópsia (1)
- `tratamento` — EZT/exérese da zona de transformação (3)
- `anatomo` — anatomia patológica (2)
- `cirurgia` — cirurgias de alta complexidade (6)
- `radioterapia` — (2) e `quimioterapia` — (2)

**Pipeline 1 — SIA-PA (`run_prepare_SUS`):**
- Loop UF × ano × mês (≈324 iterações por ano para 27 UFs); cada iteração baixa, filtra por `todos_codigos_interesse` e salva 1 chunk Parquet em `data-raw/sia_cc_chunks_sia_pa/`.
- **Idempotente**: chunks já existentes são pulados (`file.exists → next`). Permite retomar de onde parou.
- **Nunca salva chunk parcial**: se download falhar (`NULL`), retornar 0 linhas, ou filtro vazio → `next` sem `write_parquet`.
- Etapa 2 (consolidação): abre todos os Parquets como dataset Arrow lazily e faz `collect()` → `data-raw/sia_cc_completo.rds`.

**Pipeline 1b — `build_sia_cc_resumo`:**
- Tolerante a colunas ausentes (preenche com NA do tipo certo) — protege contra layouts antigos.
- Padroniza `PA_UFMUN`/`PA_MUNPCN` com zfill 6 (códigos IBGE de município).
- Decompõe `PA_CMP` (formato `AAAAMM`) em `ano_cmp` + `mes_cmp`.
- Mapeia código → categoria via `data.table::rbindlist` dos 10 vetores `codigos_*`.
- Anexa nome do procedimento via SIGTAP.
- Buckets etários quinquenais alinhados ao IBGE/GLOBOCAN (`0-4`, `5-9`, …, `85+`).
- Agregação preserva BOTH `PA_UFMUN` (atendimento) e `PA_MUNPCN` (residência) — separação em "care"/"res" só ocorre no pipeline 3.

**Pipeline 2 — SIH-RD (`run_prepare_SIH_RD_EZT` + `build_sih_rd_ezt_resumo`):**
- Por que existe: parte da produção de "tratamento" (EZT) é faturada como internação e portanto fica no SIH, não no SIA. Sem este pipeline, o app subestimaria EZT.
- Estrutura espelha o SIA, com diferenças: coluna do procedimento é `PROC_REA` (não `PA_PROC_ID`); competência vem em `ANO_CMPT`/`MES_CMPT` (não `PA_CMP`); contagem usa `uniqueN(N_AIH)` quando disponível, senão `.N`.
- Layout SIH-RD historicamente variável → checagem extra antes do filtro: se `coluna_proc` não existe no DataFrame baixado, pula o mês.
- `build_sih_rd_ezt_resumo` cria `ano_cmp`/`mes_cmp` com fallback 2-níveis: `ANO_CMPT` (oficial) → `.ano` (marcador do chunk).

**Pipeline 3 — `build_sus_proc_resumo`:**
- **Saída final do ETL SUS** — o que o app realmente lê.
- Filtra `ano_ref` (default `2024L`) ANTES da agregação — base final não tem ano/mês.
- Para cada sistema (SIA, SIH), produz 2 visões empilhadas:
  - `geo_ref = "care"` (SIA: `PA_UFMUN`; SIH: `MUNIC_MOV`) — produção territorial (município do prestador/hospital).
  - `geo_ref = "res"` (SIA: `PA_MUNPCN`; SIH: `MUNIC_RES`) — demanda da população (município de residência).
- 2 métricas por linha: `total_all` (todas idades) e `total_25_64` (faixa-alvo do rastreamento, hard-coded `c("25-29", …, "60-64")`).
- SIH entra com `categoria = "tratamento"`, `sistema = "SIH"` — soma com SIA tratamento (visão única de EZT no app via `cc_capacity_from_sia`).
- Merge com `regional_sus_map` é feito **aqui** (no ETL, não no app) para evitar joins repetidos em runtime.
- SIH é **opcional**: se `input_sih` não existir, gera base "só SIA" com warning.

**Observações:**
- `total_qtdapr` (qtd aprovada) é a métrica de produção usada pelo app — representa procedimento efetivamente faturado (proxy de "realizado"). `total_qtdpro` (apresentada) é mantida na base granular mas não consumida.
- Faixa `25-64` hard-coded **em ETL**: não responde aos sliders da sidebar — é coluna de contexto. Se alguém quiser cruzar com faixa diferente, precisa rebuildar este script com outra constante.
- `Sys.sleep(1)` entre meses no download — cortesia ao DATASUS para evitar rate-limit; pequena penalidade de tempo (~5 min por UF/ano) aceitável em ETL offline.
- `compression = "zstd"` no `write_parquet` — melhor razão velocidade/tamanho disponível em `arrow`.
- Nomes "humanos" mantidos no `regional_sus_map` (`"Macrorregiao de Saude"` etc.) divergem do snake_case gerado em `02_prepare_BR.R` — proposital, é o nome usado na UI de filtros.
- Definição duplicada de `faixa_25_64` em `build_sus_proc_resumo` (linhas 700 e 703) — inofensivo, candidato a limpeza.
- Variável `ano_cmp_sel` (filtro opcional pré-agregação) é redundante com a abordagem de `build_sus_proc_resumo` (que filtra `ano_ref` na função "downstream"), mas mantida para uso ad-hoc em desenvolvimento.

#### `data-raw/04_checks_cc.R` — checagens de sanidade (QA dos `.rds`)

Script de QA que **não modifica `data/`**: lê os objetos gerados por 01–03 e produz um conjunto de relatórios `.csv` (um por anomalia) + um sumário `.txt` em `reports/`. Roda offline; é a "rede de segurança" antes de empacotar uma versão para deploy.

**Entrada (lê de `data/` e `data-raw/`):**
- `data/df_cc_completo.rds` (obrigatório — `fail()` se ausente)
- `data/df_cc_taxas.rds` (obrigatório)
- `data/pop_municipio_faixas.rds`, `data/ans_municipio_faixas.rds`, `data/pop_municipio_faixas_total_sus.rds`, `data/regional_sus_map.rds`, `data/pop_municipio_regional.rds` (todas opcionais — bloco BR é pulado se algum `.rds` faltar)
- `data-raw/sia_cc_completo.rds` (intermediário SIA — opcional)
- `data/sia_cc_resumo.rds` (intermediário SIA — opcional)

**Saída (em `reports/`, criada lazy):**
- `reports/checks_cc_summary.txt` — sumário em texto plano (data/hora, linhas, dimensões únicas).
- `reports/<nome>.csv` — uma anomalia por arquivo. Lista canônica:
  - `ages_missing.csv`, `pop_na.csv`, `pop_negative.csv`, `duplicates_epi_key.csv`, `world_vs_sumcountries_incidence_2022.csv`, `incidence_country_vs_sheet.csv`, `rates_negative.csv`, `epi_without_population.csv`
  - `ibge_pop_negative.csv`, `ans_beneficiarios_negative.csv`, `pop_total_sus_negative.csv`, `pop_sus_greater_than_total.csv`, `pop_total_vs_ibge.csv`, `regional_geo_id_missing.csv`
  - `sia_qtd_negative.csv`
- Cada CSV é escrito **só se houver anomalia** (linha > 0). Console emite `warn()` para dar visibilidade.

**Convenção de severidade:**
- `ok("...")` → ✔ verde (passou — sem efeito).
- `warn("...")` → ⚠ amarelo (anomalia documentada em CSV, script continua).
- `fail("...")` → ✖ vermelho via `stop()` (schema quebrado — interrompe).

**Pipeline (4 blocos numerados):**

1. **Globocan/WPP** — 10 sub-checks em `df_cc_completo` + `df_cc_taxas`:
   - 1.1 Schema (colunas essenciais).
   - 1.2 Sexo: somente "Female" / `sex_code == 2`.
   - 1.3 Cobertura de faixa etária (lista hard-coded de 18 faixas).
   - 1.4 NAs e populações negativas (2 CSVs separados).
   - 1.5 Duplicatas na chave canônica `(população × sexo × faixa × câncer × tipo × ano)`.
   - 1.6 Presença de "World" (1001).
   - 1.7 Coerência aritmética: World 1001 ≡ soma dos países por idade (Incidence/2022, tolerância `1e-6`).
   - 1.8 Coerência: soma das idades em `df_cc_completo` × país × `number_incidence` em `df_cc_taxas` (tolerância 5%).
   - 1.9 Sanidade das taxas (não-negativas).
   - 1.10 Epi sem população (registros com epi mas sem `pop_2022`/`pop_2025`).
2. **Brasil — IBGE/ANS** — 5 sub-checks (todos opcionais — pulados se a base não foi gerada):
   - 2.1 IBGE: schema + pop não-negativa.
   - 2.2 ANS: schema + `beneficiarios` não-negativos.
   - 2.3 `pop_total_sus`: schema + não-negativos + `pop_sus <= pop_total` + cross-check `pop_total ≈ IBGE` (tolerância ±1).
   - 2.4 `regional_sus_map`: schema (11 colunas) + `geo_id` sem ausências.
   - 2.5 `pop_municipio_regional`: schema + cross-check de cardinalidade `n_reg >= n_total_sus`.
3. **Produção SUS (SIA)** — 2 sub-checks no intermediário (NÃO cobre `sus_proc_resumo.rds`):
   - 3.1 `sia_cc_completo`: presença + tipo tabular.
   - 3.2 `sia_cc_resumo`: schema + `total_qtdpro/qtdapr` não-negativos + `mes_cmp ∈ [1, 12]` + `faixa_idade` no domínio canônico (NA tolerado).
4. **Sumário final** — `reports/checks_cc_summary.txt` com timestamp + estatísticas básicas Globocan/WPP. Helpers `safe_*()` toleram objetos NULL (retornam "NA" como string).

**Helpers:**
- `ok(msg)` / `warn(msg)` / `fail(msg)` — printers de console com prefixo ✔/⚠/✖. `fail()` chama `stop()`.
- `expect_cols(dt, cols, nm)` — wrapper sobre `cc_check_schema(..., on_fail = "stop")` (TASK-05) envolvido em `tryCatch` para preservar o prefixo `✖`/`✔` do relatório do script. Mantém a assinatura original.
- `write_report(dt, file)` — `fwrite()` se `nrow > 0`, no-op se vazio. Cria `reports/` lazy.
- `%||%` — null-coalescing local (duplica utils, mantido para standalone).
- `load_rds_if_exists(path, required, nm)` — loader defensivo; coage para data.table.
- `safe_nrow(x)` / `safe_unique_chr(x)` / `safe_unique_int_n(x)` — formatters NULL-tolerantes para o sumário.

**Observações:**
- **Schema validation unificado** (TASK-05): `cc_check_schema(dt, expected, context, on_fail)` em `R/01_utils_cc.R` substituiu o pattern triplo (aqui via wrapper `expect_cols`, em 01 substituindo `must_have_cols`, em `R/16_mod_capacidade.R` substituindo o bloco inline). Mensagem padronizada `"[<context>] missing column(s): ..."`.
- **Hard-code de faixas etárias** (1.3 e 3.2) duplica `AGE_ORDER` de `R/00_constants_cc.R`. O script roda standalone (não dá `source` no app), então a duplicação é proposital — mas se mudar `AGE_ORDER`, atualizar aqui também.
- **Tolerância 5% (1.8) é ad-hoc**: experiência empírica com vintages do GCO mostrou que 1–3% é normal (por causa de "—" tratado como 0/NA inconsistentemente). 5% deixa folga sem mascarar bugs reais.
- **3.1 confere o intermediário, não a saída final**: `sus_proc_resumo.rds` (lido pelo app como `sia_cc_resumo`) NÃO é checado aqui. Lacuna — adicionar 3.3 que confira a base que o app realmente lê seria valioso.
- **Bloco BR é "tudo ou nada por arquivo"**: cada sub-check tem `if (!is.null(...))` próprio, então a ausência de um `.rds` não impacta os outros. Isso permite rodar em ambientes parciais (só dados globais, sem BR).
- **`reports/` é externo ao app**: não é commitado em git (typically em `.gitignore`), e o app nunca lê dele — é só para inspeção offline pelo desenvolvedor.
- **Console usa emojis (✔/⚠/✖)**: depende do encoding do terminal. Em consoles legados (Windows CMD sem UTF-8) podem aparecer como `?`. Funciona corretamente no RStudio e em terminais modernos.
- **Não há check explícito de `cito_presets` ou `peers_data`**: scripts 07 e 09 não geram entradas neste QA. Lacuna (mas baixo risco — schemas dessas bases são pequenos e estáveis).

#### `data-raw/05_glossary_cc.R` — dicionários (tabelas de dimensão)

Script standalone que **lê** `df_cc_completo.rds` (e `df_cc_taxas.rds` por consistência) e gera as **5 tabelas de dimensão** do app — listas canônicas de valores únicos para país, faixa etária, tipo de métrica, sítio anatômico e ano. Não modifica as bases de fatos; só extrai distincts ordenados.

**Entrada (em `data/`):**
- `df_cc_completo.rds` — base GLOBOCAN granular (gerada por 01).
- `df_cc_taxas.rds` — taxas agregadas (carregada por padronização; não consumida nesta versão — disponível para futura expansão).

**Saída (5 `.rds` em `data/`, todos `compress = "xz"`):**
- `df_dim_country.rds` — `(population_code, population_name)`, ordenado por nome (alfabético — amigável para o `selectInput` da sidebar). Inclui o `1001` ("World") re-derivado em 01.
- `df_dim_age.rds` — `(age, age_code)`, com **ordem canônica do GLOBOCAN preservada** (`0-4 → 5-9 → … → 85+`), não alfabética.
- `df_dim_type.rds` — `(type_code, type)`, ordenado por code (1=Incidence, 2=Mortality).
- `df_dim_cancer.rds` — `(cancer_code, cancer)`. **Subproduto**: gerado por completude mas **NÃO carregado em `app.R`** (o app filtra fixo em "Cervix uteri"). Reservado para futura expansão a outros sítios.
- `df_dim_year.rds` — `(year_prediction)`, ordenado crescente (tipicamente 2022 e 2025).

**Pipeline (7 blocos numerados):**

1. **Carregar** `df_cc_completo.rds` + `df_cc_taxas.rds` (`setDT` para data.table).
2. **`df_dim_country`** — `unique` em `(population_code, population_name)` ordenado por `population_name`.
3. **`df_dim_age`** — define `age_order` hard-coded (18 faixas), extrai `(age, age_code)` distintos do GLOBOCAN, faz **`merge(left=age_order, right=age_map, all.x=TRUE)`** para preservar a ordem canônica (não alfabética). Depois faz **assert defensivo**: se algum `age_code` ficou NA, dá `stop()` listando os rótulos problemáticos (defesa contra mudança de schema GLOBOCAN — ex.: aparecer "90+" em vintage futuro).
4. **`df_dim_type`** — `unique` em `(type_code, type)` ordenado por `type_code`.
5. **`df_dim_cancer`** — `unique` em `(cancer_code, cancer)` ordenado por `cancer_code`.
6. **`df_dim_year`** — `sort(unique(year_prediction))` empacotado em `data.table` de 1 coluna.
7. **`saveRDS`** dos 5 dicionários em `data/`, todos com `compress = "xz"`.

**Observações:**
- **`age_order` hard-coded** duplica `AGE_ORDER` em `R/00_constants_cc.R` (mesmo motivo das duplicações de 04: o script roda standalone, não dá `source` no app). Se a constante for centralizada (ex.: `data-raw/_etl_constants.R`), esta cópia some.
- **`df_dim_cancer` é subproduto NÃO consumido pelo app**: gerado para completude/futuro-proofing, mas `app.R` carrega só os 4 outros dicionários. Pode ser opt-out via flag se reduzir IO virar prioridade.
- **`df_dim_type` e `df_dim_year` também são subprodutos**: carregados em `app.R` mas (atualmente) sem consumidor identificado nos módulos. INVENTORY já lista como TODO — possível remoção da carga em `app.R` para reduzir startup, ou uso futuro em alguma feature de "ano de projeção" / filtro de tipo.
- **`df_dim_age` é a única dimensão com lógica não-trivial** — preservar a ordem canônica via `merge(all.x=TRUE)` é decisão correta (a ordem alfabética colocaria "10-14" antes de "5-9").
- **Compressão `xz`** mantém os 5 dicionários em arquivos ≪ 10 KB cada; overhead de leitura desprezível no startup do app.
- **Script idempotente**: re-executar gera os mesmos `.rds` (substitui).

#### `data-raw/06_make_all_cc.R` — orquestrador (master script) do pipeline ETL+QA

Script "master" que dispara, em sequência, os scripts standalone de `data-raw/` (01, 02, 03, 04, 05) que populam `data/*.rds` com as bases consumidas pelo Shiny. É o ponto de entrada único para "rebuildar tudo" — re-executá-lo é idempotente (cada sub-script sobrescreve `.rds` e os de SUS pulam chunks já baixados).

**Entrada:** cwd na raiz do projeto (caminhos são relativos a `data-raw/...`). Cada sub-script é responsável pelas suas próprias entradas (planilhas em `data-raw/`, downloads via `microdatasus`, etc.).

**Saída:** efeitos colaterais em `data/` (e `data-raw/` para intermediários do SUS) + bloco de RESUMO no console com nº de linhas dos `.rds` principais.

**Pipeline (6 blocos numerados no script):**

1. **Globocan/WPP** — `source("data-raw/01_prepare_cc.R")`. Script de top-level (sem função mestre) — gera `df_cc_completo.rds` + `df_cc_taxas.rds`.
2. **Brasil** — `source("data-raw/02_prepare_BR.R")` + callback `run_prepare_BR()`. Esse script só DEFINE funções; o callback orquestra IBGE → ANS → Regional → pop_base → +regional e salva 5 `.rds`.
3. **SUS** — `source("data-raw/03_prepare_SUS.R")` se o arquivo existir + callback parcial. **Bloco opcional**: pulado silenciosamente se o script não estiver presente (permite buildar o app só com bases globais). Por padrão, o callback dispara apenas os agregadores rápidos (`build_sih_rd_ezt_resumo` + `build_sus_proc_resumo(ano_ref = 2024L)`); os downloads via `microdatasus` (`run_prepare_SUS` / `run_prepare_SIH_RD_EZT` / `build_sia_cc_resumo`) ficam comentados por serem caros (~10–15 min) e tipicamente já populados em `data-raw/sia_cc_chunks_*/`. Para um build "do zero", descomentar.
4. **QA** — `source("data-raw/04_checks_cc.R")`. Script standalone: lê os `.rds` gerados em 1–3 e produz CSVs de anomalias + sumário em `reports/`. Bloco BR é tolerante a `.rds` ausentes.
5. **Dicionários** — `source("data-raw/05_glossary_cc.R")`. Roda APÓS 01 (precisa de `df_cc_completo.rds`) e idealmente APÓS 04 (QA antes de empacotar dimensionais). Gera os 5 `df_dim_*.rds`.
6. **Resumo final** — loga no console o nº de linhas dos 4 `.rds` principais (`df_cc_completo`, `df_cc_taxas`, `df_dim_country`, `df_dim_age`) para inspeção visual rápida pós-build. Tolerante a ausência (silencioso se algum `.rds` não existir).

**Helpers internos:**
- `run_file(path, after = NULL)` — executa um sub-script com `tryCatch` e callback opcional. Usa `source(path, echo = TRUE, max.deparse.length = Inf)` para imprimir cada expressão (debug em CI/log offline) sem truncar comandos longos. Re-emite o erro via `stop(e)` (interrompe o pipeline — falhas upstream NÃO são silenciadas, porque a ordem dos scripts pressupõe sucesso do anterior).

**Observações:**
- **Run 03 parcialmente comentado por padrão**: tradeoff entre "rebuild rápido" (apenas agregadores rápidos descomentados) e "rebuild do zero" (descomentar tudo). Documentar a escolha aqui evita surpresas quando o build "rápido" não atualiza os chunks SIA/SIH.
- **Idempotência total**: re-executar o `06` não corrompe nada — `saveRDS` sobrescreve, `file.exists() → next` em chunks SIA/SIH garante retomada de download interrompido.
- **Dependências entre blocos**: 5 (`df_dim_*`) precisa de 1 (`df_cc_completo.rds`); 4 (QA) cobre todos os outputs de 1–3. Inverter a ordem quebra o pipeline. **Não há paralelismo** — execução sequencial é assumida.
- **Bloco BR é "tudo ou nada"** (ver QA em 04): se 02 não rodar, o bloco BR de 04 simplesmente pula seus sub-checks (com `if (!is.null(...))`). Não há barreira em 06 para isso — confia na tolerância downstream.
- **Não há entry point para apenas o QA**: para rodar só o `04`, é mais simples chamar `source("data-raw/04_checks_cc.R")` direto (sem o `run_file` wrapper). O wrapper só agrega valor quando há callback (`after`).
- **Sem `make` ou `targets`**: o orquestrador é um script imperativo simples. Para builds incrementais (não rebuild tudo), o ideal seria migrar para `targets` ou um Makefile com dependências de arquivo. Hoje, cada `.rds` é regerado mesmo que sua entrada não tenha mudado.
- **Exemplo de uso típico**: `R --vanilla -e 'setwd("/path/to/shiny-cc"); source("data-raw/06_make_all_cc.R")'` em background (build de release ou setup inicial).

#### `data-raw/07_prepare_cito_presets.R` — presets de citologia (INCA + SISCAN por UF)

Script standalone que gera `data/cito_presets.rds`, **lookup table** consumida pela sidebar (`mod_filters_cc`) quando o usuário escolhe "Cytology" como método. Combina os 15 parâmetros nacionais INCA 2019 com os 4 parâmetros derivados do SISCAN 2022–2024 (que variam por UF), produzindo presets por fonte e por UF.

**Entrada (em `data-raw/`):**
- `tabela_uf_prest_categorias.xlsx` — exportação SISCAN 2022–2024 com 1 linha por UF + linhas-totais (descartadas via `is.na(uf_prest)`). Colunas usadas: `uf_prest`, `uf_prest_num`, `Total`, `insatisfatoria_rejeitada`, `ASC-H+`, `outras_alteracoes`, `Negativo`.

**Saída (em `data/`):**
- `cito_presets.rds` — lista nomeada de 2 níveis:
  - `cito_presets$inca2019$brasil` → list (15 params nacionais INCA 2019).
  - `cito_presets$siscan$brasil` → list (15 params: 4 SISCAN nacionais + 11 INCA fallback).
  - `cito_presets$siscan$<uf_nome>` → list (15 params: 4 SISCAN da UF + 11 INCA fallback). UF sem acento (`"Sao Paulo"`, `"Para"`, etc.).

**Pipeline (5 blocos numerados no script):**

0. **`inca2019`** — define lista nacional dos 15 parâmetros (escala 0–100). Fonte ENCC/INCA 2019. Usada como preset puro E como fallback para os 11 campos sem SISCAN-UF.
1. **Leitura SISCAN** — `read_excel` da planilha; descarta linhas-totais (`is.na(uf_prest)`); define `uf_name_map` (acento → sem-acento, hard-coded para 27 UFs); funções `calc_siscan_params` (4 params a partir de contagens — % insatisfatórios sobre total, % ASC-H+/outras/negativo sobre satisfatórios) e `make_params` (sobrescreve as 4 chaves SISCAN no fallback INCA, preserva os outros 11).
2. **Brasil** — `colSums` das contagens das 27 UFs → 1 chamada de `calc_siscan_params` para o agregado nacional SISCAN. Re-derivado por SOMA (não usa eventual linha-total da planilha) para garantir aditividade.
3. **Por UF** — loop linha-a-linha; resolve UF acento → chave; calcula 4 params SISCAN; empacota com fallback INCA via `make_params`. UF não mapeada → `warning` + `next` (não interrompe).
4. **Monta objeto final** — `cito_presets <- list(inca2019 = list(brasil = ...), siscan = c(list(brasil = ...), siscan_por_uf))`. Estrutura espelha `CITO_PRESETS_META` (em `00_constants_cc.R`): `inca2019` sem subdivisão por UF (`por_uf = FALSE`), `siscan` com `$brasil` + 27 entradas UF (`por_uf = TRUE`).
5. **Salva** — `saveRDS` sem compressão (objeto < 50 KB). Eco no console com fontes/UFs/campos.

**Parâmetros derivados do SISCAN (variam por UF):**
- `unsatisfactory_pct` — % exames insatisfatórios (sobre o total).
- `res_asch_pct` — % ASC-H+ (sobre satisfatórios).
- `res_other_pct` — % outras alterações (sobre satisfatórios).
- `res_neg_pct` — % negativos (sobre satisfatórios).

**Parâmetros INCA-fallback (todos os 11 vêm do INCA 2019 em todas as UFs até haver dados SISCAN):**
- `first_time_pct`, `colpo_asch_pct`, `colpo_other_follow_pct`, `biopsy_pos_asch_pct`, `biopsy_pos_other_pct`, `b_asch_nic23_pct`, `b_asch_cancer_pct`, `b_asch_neg_nic1_pct`, `b_other_nic23_pct`, `b_other_cancer_pct`, `b_other_neg_nic1_pct`.

**Helpers internos:**
- `calc_siscan_params(asch, outras, negativo, insatisf, total)` — calcula os 4 params SISCAN a partir de contagens. Definição matemática: insatisf/total para `unsatisfactory_pct`, e (asch|outras|negativo)/(total−insatisf) para os outros 3. Saída em escala 0–100 com `round(., 3)`.
- `make_params(siscan_part, fallback = inca2019)` — sobrescreve seletivamente no `fallback` apenas as chaves presentes em `siscan_part`. Preserva os outros 11 campos do INCA até dados SISCAN-UF estarem disponíveis.

**Observações:**
- **Standalone**: como 04 e 05, este script NÃO está em `06_make_all_cc.R`. Para regerar `cito_presets.rds`, chamar `source("data-raw/07_prepare_cito_presets.R")` direto (com cwd na raiz). Lacuna do orquestrador — mover ou documentar.
- **`uf_name_map` hard-coded** (27 UFs com acento → sem acento). Casa com a chave `pop_municipio_regional$UF` (também sem acento). Se o pipeline BR (02) mudar para preservar acentos, sincronizar aqui — o lookup quebra silenciosamente (UF sem mapeamento → `warning` + skip, mas o preset SISCAN para essa UF não é gerado).
- **"Brasil SISCAN" é re-derivado por soma** (não usa linha-total da planilha). Garante aditividade total ↔ UFs.
- **`make_params` é seletivo**: sobrescreve só as 4 chaves SISCAN; os 11 INCA fallback ficam idênticos em todas as UFs até SISCAN-UF expandir. Adicionar nova fonte (ex.: SISCAN 2025 com mais campos) é só estender `siscan_part`.
- **Metadados separados dos dados**: `CITO_PRESETS_META` (em `R/00_constants_cc.R`) carrega `label` (UI) e `por_uf` (gating do filtro UF na sidebar); este script gera só os valores numéricos. Adicionar nova fonte requer atualização nos dois lugares.
- **Não há QA explícito** em `04_checks_cc.R` para `cito_presets.rds` (lacuna apontada na documentação de 04). Como o objeto é < 50 KB e o schema é estável, baixo risco — mas em revisão final cabe um `expect_cols`-like que valide as 27 UFs + presença das 15 chaves.
- **Hard-code das colunas SISCAN** (`Total`, `insatisfatoria_rejeitada`, `ASC-H+`, `outras_alteracoes`, `Negativo`) — se a exportação SISCAN mudar nomes em vintages futuros, falha imediata no `colSums`/subset (visível, não silencioso). Adequado.

#### `data-raw/08_prepare_geometrias.R` — geometrias do Brasil para o módulo Maps

Script standalone que monta os 4 arquivos `.sf` consumidos pelo `mod_maps`. Baixa os polígonos oficiais via `geobr` (estados e municípios), simplifica via `rmapshaper`, anexa a hierarquia SUS (vinda de `regional_sus_map.rds`) e **deriva** as camadas de macrorregião e região de saúde por união topológica (`st_union`) — porque o `geobr` não publica essas malhas.

**Entrada:**
- `data/regional_sus_map.rds` — gerado por `02_prepare_BR.R`, contém o mapeamento município → (UF, macrorregião, região de saúde).
- `geobr::read_state(year = 2020)` — 27 polígonos UF (download em runtime).
- `geobr::read_municipality(year = 2020)` — ~5,5 mil polígonos municipais (download em runtime).

**Saída (4 `.rds` em `data/`, todos `sf` — sem compressão; geometrias já são binárias):**
- `geo_estados.rds` — sf 27 polígonos UF, colunas `uf_codigo`, `uf_sigla`, `uf_nome`, `geometry`.
- `geo_municipios.rds` — sf ~5.5k municípios + hierarquia SUS já anexada (`mun_code6`, `uf_sigla`, `macro_codigo/nome`, `regiao_codigo/nome`, +nomes oficiais com acentos para tooltips).
- `geo_macrorregioes.rds` — sf ~120 polígonos derivados, colunas `macro_codigo`, `macro_nome`, `uf_sigla`, `geometry`.
- `geo_regioes_saude.rds` — sf ~440 polígonos derivados, colunas `regiao_codigo`, `regiao_nome`, `macro_nome`, `uf_sigla`, `geometry`.

**Pipeline (4 blocos numerados):**

1. **Estados** — `read_state(year=2020)`, subset 4 colunas, rename para padrão pt-BR (`uf_codigo`/`uf_sigla`/`uf_nome`/`geometry`), reativa coluna geom como ativa via `st_geometry()`, simplifica `keep=0.05` + `keep_shapes=TRUE`, salva.
2. **Municípios** — `read_municipality(year=2020)`, deriva `mun_code6 = substr(code_muni, 1, 6)` (IBGE 7 dígitos → SUS 6 dígitos, descartando o verificador), faz `merge(all.x=TRUE)` com `mun_hier` (lookup deduplicado de `regional_sus_map`), simplifica e salva. Esta camada cumpre 2 papéis: saída direta (mapa municipal) + base para os blocos 3 e 4.
3. **Macrorregiões** — filtra municípios com `macro_nome` válido (`!is.na & nzchar`), aplica `aggregate.sf(geometry, by=list(macro_codigo, macro_nome, uf_sigla), FUN=st_union)`, re-promove a sf, simplifica e salva. Macrorregião não existe no `geobr`; é derivada aqui.
4. **Regiões de saúde** — mesmo padrão do bloco 3, agora chave = `(regiao_codigo, regiao_nome, macro_nome, uf_sigla)`. O `macro_nome` na chave permite herdar a macro da região (drill-up no mapa).

**Helpers internos:**
- `path_data(...)` / `path_data_raw(...)` — wrappers `file.path("data", ...)` / `file.path("data-raw", ...)`. Diferem dos homônimos em `02_prepare_BR.R` (que usam `here::here`); aqui exigem cwd na raiz. `path_data_raw` está definido mas não é chamado — futuro-proofing.

**Observações:**
- **Standalone — NÃO está em `06_make_all_cc.R`**: mesma lacuna de 04/05/07. Para regerar, chamar `source("data-raw/08_prepare_geometrias.R")` direto. Como o script depende de `regional_sus_map.rds` (gerado em 02), a ordem correta é "rodar 02 antes". Idealmente: incluir no orquestrador entre os blocos 5 e 6.
- **Hierarquia SUS é única fonte de verdade** — macrorregião e região de saúde são derivadas do `regional_sus_map.rds`, não baixadas. Se a hierarquia mudar (ex.: criação de uma nova região), basta regerar 02 e depois 08; nenhum hard-code de polígonos.
- **`year = 2020` hardcoded** em `read_state` e `read_municipality`: alinha com o vintage da malha que casa com o `regional_sus_map`. Se for atualizado para um vintage pós-Censo 2022, validar compat com 02 antes (pode haver desmembramento/criação de municípios).
- **Simplificação 5% (`keep = 0.05`)** é agressiva e deliberada: reduz polígonos pesados (BR completo ~30 MB → poucos MB) sem perder fronteiras críticas para visualização leaflet. `keep_shapes = TRUE` impede colapso de ilhas/distritos isolados (ex.: Fernando de Noronha).
- **`mun_code6 = substr(code_muni, 1, 6)`**: convenção IBGE — 7º dígito é o verificador, descartado para casar com o esquema SUS de 6 dígitos. Se algum dataset upstream mudar para 7 dígitos, a chave `mun_code6` deixa de bater silenciosamente — `merge(all.x=TRUE)` mascararia a falha.
- **`uf_sigla` nas chaves de macro/região** é proteção contra inconsistências no `regional_sus_map` (em tese, uma macro/região não atravessa UF; se atravessar por erro de cadastro, o `aggregate` não funde geometrias entre UFs distintas).
- **`aggregate.sf`** com `FUN = st_union` é o idiom canônico para dissolver fronteiras internas; mais robusto que `dplyr::group_by + summarise(st_union)` em pipelines puramente sf+base.
- **Sem `compress = "xz"`** nos `saveRDS` (diverge de 05): geometrias `sf` já são binárias e raramente comprimem bem com xz; o ganho de leitura mais rápida vale o tamanho extra.
- **Idempotente**: re-executar regenera os 4 `.rds` (substitui).
- **Custo de execução**: 1ª execução tem download via `geobr` (~1 min para municípios). Re-execuções usam o cache do pacote (`~/.cache/R/geobr/`).

#### `data-raw/09_prepare_peers.R` — peer groups municipais (clustering PAM + scores)

Script standalone que monta `data/peers.rds`, **base mestre** consumida pela aba `mod_peers`. Combina três coisas em uma tabela plana (1 linha = 1 município): (i) **peer groups** por similaridade demográfica e perfil SUS via clustering PAM com k ótimo selecionado por silhouette, (ii) **necessidade anual estimada** (cenário INCA 2019 hard-coded, cobertura 70%, intervalo 3 anos), (iii) **produção SIA real** (visão "res") e os **scores 0–1 + ranking dentro do grupo** que daí derivam. Toda essa pré-computação acontece em ETL para que a aba seja um leitor estático — a sidebar do app não afeta peers.

**Entrada (em `data/`):**
- `pop_municipio_regional.rds` — base de população (gerada por 02).
- `sus_proc_resumo.rds` — produção SUS (gerada por 03; o objeto é chamado `sia_cc_resumo` em `app.R` por motivo histórico).
- `cito_presets.rds` — presets de citologia (gerado por 07; usado só para puxar os 15 params INCA 2019 nacionais).

**Saída (em `data/`):**
- `peers.rds` — data.table municipal com schema completo (~25 colunas — ver bloco 9 do script). Carregada como `peers_data` em `app.R`.

**Pipeline (9 blocos numerados no script):**

1. **Carregar dados** — leitura dos 3 `.rds`; `setDT` idempotente; 4 `cat()/print()` defensivos para detectar mudanças de schema (faixa nova, geo_ref novo, categoria nova) antes que o pipeline silencie a falha em joins downstream.
2. **Agregar população por município (faixa 25–64)** — constrói `mun_pop` com `pop_alvo_total` (IBGE 25–64), `pop_sus_total` (IBGE − ANS clamp ≥ 0), 3 bandas etárias (25-34/35-49/50-64) convertidas em **proporções** sobre `pop_alvo_total`, e `prop_sus`. Filtra municípios com NA nas features ou pop_alvo=0. Faixa hard-coded — coerente com cenário fixo (não responde aos sliders).
3. **Clustering PAM (k ótimo por silhouette)** — `scale()` z-score nas 6 features (necessário porque pop em milhares × proporções em [0,1]); `set.seed(42)` para determinismo; pré-computa `dist(feat_scaled)` UMA vez (O(n²) é caro com ~5,5k municípios) e reusa nas iterações. Loop `k ∈ 3..12` com `silhouette()` médio como métrica; seleciona `k_opt = which.max(sil_vals)`. Re-roda PAM final com `feat_scaled` para ter `medoids` interpretáveis.
4. **Labels de grupo** — `grupo` original (int aleatório do PAM) é re-rotulado como "Group 1..k" **ordenado por mediana de `pop_alvo_total`** — Group 1 sempre é dos menores. Determinístico entre re-runs (mesmo seed). `grupo_size_rank` redundante com `size_rank`, mantido por compat com o módulo.
5. **Necessidade anual (INCA 2019)** — após TASK-04, o volume `need_cito` é calculado via `cc_workup_volumes(eligible_sus, list(ciclo=3, ft=ft, uns=uns))$volume_anual` (helper único do engine, sourced via `source("R/02_engine_capacity_cc.R")` no topo do script). Cobertura `cov_ref = 0.70` hard-coded e parâmetros de `cito_p$inca2019$brasil`. `taxa_colpo`, `taxa_biopsia`, `taxa_nic2mais` são médias ponderadas pelos 2 braços (ASC-H+ vs outras alterações) — matematicamente equivalentes a aplicar braço a braço e somar.
6. **Produção real (SIA, geo_ref="res")** — filtra visão de **residência** (não atendimento), porque o objetivo é medir cobertura DA POPULAÇÃO de cada município. `total_all` (não `total_25_64`) — SIA não restringe rastreio à faixa-alvo no campo de idade; cap em 1 no bloco 7 absorve oferta excedente. Closure `agg_sia(cat_name)` agrega 4×; ausência no SIA → 0 (não exclui do score).
7. **Scores 0–1 + score geral** — `score_col(real, need) = pmin(real/need, 1)`; converte Inf/NaN (need=0) para NA (não pune município sem demanda); `score_geral = rowMeans(.SD, na.rm=TRUE)` dos 4 procedimentos (peso simétrico 1/4).
8. **Ranking dentro do grupo** — `rank_grupo` crescente (1 = pior), `na.last="keep"`. `mediana_grupo` e `p_top25_grupo` (referências). `gap_mediana`/`gap_top25` positivos = abaixo do referencial. `prioritario`: 10% piores ranks, piso 5 municípios; `!is.na(rank_grupo)` exclui municípios sem demanda modelada.
9. **Salvar** — `saveRDS` sem compressão (objeto poucos MB; leitura rápida no startup importa mais que tamanho).

**Observações:**
- **Standalone — NÃO está em `06_make_all_cc.R`** (5ª lacuna do orquestrador, junto com 04/05/07/08). Para regerar: `source("data-raw/09_prepare_peers.R")` direto, com cwd na raiz e DEPOIS de 02 + 03 + 07. Ordem importante.
- **Cenário pré-calculado é hard-coded** (INCA 2019, cobertura 70%, faixa 25–64, intervalo 3 anos). A sidebar do app **não afeta** os peer groups nem os scores — decisão deliberada de UX para manter peers estáveis entre sessões. Se essa decisão mudar, o cenário tem que voltar para runtime no engine.
- **Dependência forte com o engine** (`02_engine_capacity_cc.R`): a fórmula `((E/3) + (E*ft)) * (1+uns)` é replicada aqui. Reforça o débito técnico de extrair um helper `cc_workup_metrics` reutilizável (já anotado em outras observações cruzadas — agora com 4ª ocorrência incluindo este script).
- **Faixa 25–64 é hard-coded em 3 lugares relacionados**: aqui (bloco 2 e cenário), em `03_prepare_SUS.R` (`build_sus_proc_resumo`) e em `mod_resumo_pais.R` (cards de pop). Consistente, mas se algum dia mudar uma referência, atualizar as três.
- **`pop_sus_total` (não `pop_alvo_total`) é a base do denominador** de `eligible_sus` — coerente com o engine (rastreamento SUS, não total). Município com `pop_sus=0` (raro: pop só atendida pelo privado) tem `need_*=0` e cai com NA no score.
- **`real_*` usa `total_all`** (todas idades) enquanto `need_*` é restrita a 25–64 — assimetria intencional. Município com rastreio rotineiro fora da faixa pode ter `real/need > 1`; `pmin(., 1)` em (7) faz o cap.
- **`cod_municipio` em `mun_pop` vem de `mun_code6`** (6 dígitos IBGE/SUS); o join com SIA usa `geo_id` que também é 6 dígitos (padronizado no ETL 03). Se algum dia o esquema mudar para 7 dígitos, o join seria silenciosamente vazio — TODO assert pós-join (`stopifnot(mean(is.na(real_cito)) < 0.5)`).
- **Falta QA explícito em 04** para `peers.rds` — lacuna documentada também no header de 04. Schema é estável, baixo risco; em revisão final, adicionar 3.4 que valide presença das ~25 colunas e ranges (`prop_sus ∈ [0, 1]`, `score_* ∈ [0, 1] ∪ NA`, `prioritario ∈ {TRUE, FALSE}`).
- **Custo de execução** ~2–5 min em CPU moderna (dominado pela `dist()` da matriz ~5,5k × 6). Idempotente.
- **Determinismo**: depende de `set.seed(42)` ANTES do `dist()` — alterar a ordem das chamadas pode mudar os grupos. Mantida a sequência atual.

<!-- DOC:data-raw:END -->

---

## Helpers compartilhados (refator pós-documentação)

Helpers extraídos a partir das observações cruzadas geradas durante a documentação inicial (lote TASK-01..05). Cada um substitui um padrão antes duplicado em múltiplos módulos.

<!-- DOC:cc_cfg_from_input:START -->
### `cc_cfg_from_input(g, br_code = 1001L)`

Função pura no engine (`R/02_engine_capacity_cc.R`) que traduz a saída do reactive `filters` (entregue pelo `mod_filters_cc`) em `cfg` consumível por `cc_engine_run()`. Substitui o bloco `cfg <- reactive({ cc_engine_settings(...) })` que era replicado em 5 módulos (Summary / Equipment / Pathway / Capacity / Compare).

Encapsula:

- coerção de `country_code` (integer; fallback para `br_code` se ausente/NA);
- detecção de `is_brazil` (`country_code == br_code`);
- clamp de `coverage` em `[0, 100]`;
- retrocompat com nomes antigos: `programa` (→ `screen_method`) e `proto1_age_min/max` (→ `target_age_*`);
- leitura defensiva de `custom_pop` (lê `g$custom_pop` e, em retrocompat, `g$custom_pop_main`);
- mapeamento dos 16 parâmetros HPV + 16 parâmetros Citologia (NA → defaults `HPV_DEFAULTS`/`CITO_DEFAULTS` aplicados em `cc_engine_settings`);
- 4 capacidades anualizadas (NA → fallback `BASE_ANO` aplicado em `cc_hr_metrics`);
- filtros subnacionais Brasil (`filt_uf`, `filt_macro`, `filt_reg`, `filt_mun`, `br_pop_tipo`).

Uso típico nos módulos:

```r
cfg <- reactive(cc_cfg_from_input(input_global(), br_code))
res <- reactive(cc_engine_run(df_completo, cfg(), pop_mun_regional = pop_mun_regional))
```

No `mod_compare`, o cenário B constrói um pseudo-`g_b` que herda da sidebar global apenas país/`pop_mode`/`custom_pop`/`br_pop_tipo`/filtros geográficos e sobrepõe os campos próprios (método, cobertura, faixa, parâmetros, capacidades) antes de chamar `cc_cfg_from_input(g_b, br_code)`.
<!-- DOC:cc_cfg_from_input:END -->

<!-- DOC:cc_country_info:START -->
### `cc_country_info(g, dim_country, br_code = 1001L)`

Helper único que substitui o trio `country_code` + `country_label` + `is_brazil` antes replicado em 5 módulos (11/14/15/16/17). Devolve `list(code, label, is_brazil)`:

- `code` — `as.integer(g$pais_sel)`. Aplica `req()` em `g$pais_sel` e em `!is.na(code)`, então halta a reactive chamadora silenciosamente quando o país ainda não foi selecionado.
- `label` — `population_name` resolvido por lookup em `dim_country$population_code == code`. Fallback `"Selected country"` se o lookup falhar (vazio, NA ou erro).
- `is_brazil` — `!is.na(br_code) && code == br_code`.

Convenção de uso (dentro de `moduleServer`):

```r
br_code <- tryCatch({
  x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
  if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
}, error = function(e) NA_integer_)

ci <- reactive(cc_country_info(input_global(), dim_country, br_code))
# uso: ci()$code, ci()$label, ci()$is_brazil
```

`br_code` continua sendo resolvido localmente em cada módulo (não é parte do helper) porque também é argumento de `cc_cfg_from_input(g, br_code)`.
<!-- DOC:cc_country_info:END -->

<!-- DOC:cc_geo_label:START -->
### `cc_geo_label(g, mode, ...)`

Helper único de rótulo geográfico que substitui as 5 implementações distintas antes espalhadas em 11/14/15/16/17/20 (cada uma com seu próprio closure `add_if`/`pick`/`pick_name`/`add_geo`).

Devolve `character(1)` (ou `NULL` no caso especial documentado abaixo). Dois modos:

- **`mode = "concat"`** — concatena `"Brazil"` + (opcionalmente) tipo de população + cada filtro com seleção, separados por ` - `.
  Exemplo: `"Brazil - SUS-dependent - SP - ABC (n=2)"`.
- **`mode = "shortest"`** — devolve UMA string com o nível **mais fino** com seleção (Mun > Reg > Macro > UF). Com prefixo de nível por padrão (`"State: SP"`); sem prefixo quando `level_label = FALSE` (`"SP"`).

| Argumento | Default | Descrição |
|-----------|---------|-----------|
| `g` | — | output de `input_global()` (precisa `pais_sel`, `filt_uf/macro/reg/mun`, `br_pop_tipo` se `pop_tipo=TRUE`) |
| `mode` | `"concat"` | `"concat"` ou `"shortest"` |
| `dim_country` | `NULL` | resolve nome do país fora do Brasil; se NULL e `non_br_label` NULL, usa `"Selected country"` |
| `br_code` | `1001L` | código GLOBOCAN do Brasil |
| `pop_tipo` | `FALSE` | em concat, prepende `"Total population"`/`"SUS-dependent"` após `"Brazil"` |
| `first_level_only` | `FALSE` | em concat, restringe a 1 nível (UF > Macro > Reg > Mun, primeiro com seleção) |
| `sep` | `" - "` | separador do modo concat |
| `level_label` | `TRUE` | em shortest, prefixa `"<Nível>: "` (ex.: `"Municipality: ABC"`) |
| `multi_format` | `"count"` | `"count"` → `"(n=K)"`; `"increment"` → `"(+K-1)"` |
| `non_br_label` | `NULL` | fora do Brasil, retorna esta string literal (ex.: `"selected geography"`) em vez do nome do país |
| `br_empty_label` | `"Brazil"` | em shortest, valor de fallback quando Brasil sem filtros (NULL devolve NULL) |

Mapeamento das 5 implementações antigas:

| Original | Substitutivo |
|----------|--------------|
| `geo_desc` (11) | `cc_geo_label(g, "concat", dim_country, br_code, pop_tipo = TRUE)` |
| `geo_desc` (14, 16, 20) | `cc_geo_label(g, "concat", dim_country, br_code)` |
| `geo_label` (15) | `cc_geo_label(g, "shortest", dim_country, br_code)` |
| `cap_geo_label` (16) | `cc_geo_label(g, "shortest", non_br_label = "selected geography")` |
| `geo_name_label` (16) | `cc_geo_label(g, "shortest", level_label = FALSE, multi_format = "increment", br_empty_label = NULL)` (com gate `is_brazil` explícito no caller) |
| `geo_text` (17) | `cc_geo_label(g, "concat", dim_country, br_code, pop_tipo = TRUE, first_level_only = TRUE)` |

Detalhe de formato em modo shortest com `level_label = TRUE`: o sufixo `(n=K)`/`(+K-1)` anexa ao **nome do nível** (ex.: `"State (n=2): SP"`), replicando o formato original de `geo_label`/`cap_geo_label`. Com `level_label = FALSE`, anexa ao **valor** (`"SP (+1)"`), replicando `geo_name_label`.
<!-- DOC:cc_geo_label:END -->

<!-- DOC:cc_workup_volumes:START -->
### `cc_workup_volumes(eligible, params)`

Helper único para o volume anual de citologias de rastreamento. Consolida a fórmula `((E/ciclo) + (E*ft)) * (1+uns)` que antes existia replicada em 3 lugares (`cc_engine_run`, `cc_workup_metrics`, `data-raw/09_prepare_peers.R`).

| Argumento | Tipo | Default | Descrição |
|-----------|------|---------|-----------|
| `eligible` | numeric (escalar ou vetor) | — | N elegíveis no ciclo. Aceita vetor para o caso `09_prepare_peers` (1 valor por município). |
| `params$ciclo` | numeric(1) | `3` | Intervalo do ciclo de rastreamento, em anos (3 = citologia padrão BR). |
| `params$ft` | numeric(1) ∈ [0,1] | `0` | Fração de "primeiros exames" / follow-up no ano de entrada. |
| `params$uns` | numeric(1) ∈ [0,1] | `0` | Fração esperada de exames insatisfatórios. |

Retorna `list` com 4 componentes (mesma forma de `eligible`):

- `volume_anual` — volume anual final (já com reforço por insatisfatórios)
- `volume_ciclo` — `E / ciclo` (rastreamento cíclico)
- `exames_followup` — `E * ft` (parcela "primeiros exames")
- `exames_insat` — `base * uns` (reforço por insatisfatórios)

Mapeamento das 3 réplicas antigas:

| Original | Substitutivo |
|----------|--------------|
| `cc_engine_run`: `base = (E/3) + (E*ft); screened = base + base*uns` | `cc_workup_volumes(E, list(ciclo=3, ft=ft, uns=uns))$volume_anual` |
| `cc_workup_metrics`: `n_cyt_screen = ((E/3) + (E*ft)) * (1+uns)` | idem |
| `09_prepare_peers`: `mun_pop[, need_cito := ((E/3)+(E*ft))*(1+uns)]` | `mun_pop[, need_cito := cc_workup_volumes(E, list(ciclo=3, ft=ft, uns=uns))$volume_anual]` (com `source("R/02_engine_capacity_cc.R")` no topo) |

**Forma de cálculo:** o helper computa `volume_anual = base * (1 + uns)`. Antes do refator, `cc_engine_run` usava a forma algebricamente idêntica `base + base*uns` — pode haver diferença de até 1 ULP entre o KPI "Cytology screenings/year" pré e pós-refator, dentro da tolerância de 4 decimais explicitada no spec da TASK-04. As outras 2 réplicas (`cc_workup_metrics` e `09_prepare_peers`) já usavam `base * (1+uns)` — bit-exact.
<!-- DOC:cc_workup_volumes:END -->

<!-- DOC:cc_check_schema:START -->
### `cc_check_schema(dt, expected, context, on_fail)`

Helper único de validação de presença de colunas. Substitui 3 implementações com assinatura quase idêntica antes espalhadas pelo projeto:

| Origem | Antes | Depois |
|--------|-------|--------|
| `data-raw/01_prepare_cc.R` | `must_have_cols(dt, cols)` (definição local + 5 chamadas) | `cc_check_schema(dt, cols, "<contexto>")` (default `on_fail = "stop"`) |
| `data-raw/04_checks_cc.R` | `expect_cols(dt, cols, nm)` (definição local) | wrapper local que delega a `cc_check_schema(..., on_fail = "stop")` via `tryCatch` para preservar o prefixo `✖`/`✔` do relatório |
| `R/16_mod_capacidade.R` | bloco inline `setdiff` + `stop()` na reactive `sia_filtered` | `cc_check_schema(dt, cols, "mod_capacidade:sia_filtered", on_fail = "stop")` |

| Argumento | Tipo | Default | Descrição |
|-----------|------|---------|-----------|
| `dt` | data.table \| data.frame | — | Tabela a validar. |
| `expected` | character | — | Colunas obrigatórias. |
| `context` | character(1) | — | Descrição usada na mensagem (`"[<context>] missing column(s): ..."`). |
| `on_fail` | `"stop"` \| `"warn"` \| `"alert"` | `"stop"` | Comportamento em ausência: erra, avisa (warning) ou alerta (`cli::cli_alert_danger`, fallback `message()`). |

Retorno: `invisible(TRUE)` em sucesso; em falha, comportamento conforme `on_fail` (e `invisible(FALSE)` para `warn`/`alert`).

Escopo restrito a **presença** de colunas. Validação de tipo/coerção fica para extensão futura. Esta task desbloqueia TASK-33, TASK-35, TASK-36 e TASK-37 (asserts/QA gaps que dependiam do helper).
<!-- DOC:cc_check_schema:END -->

---

## Requisitos

**Para rodar o app:**
- R ≥ 4.1
- Pacotes: `shiny`, `data.table`, `ggplot2`, `grid`, `DiagrammeR`, `leaflet`, `sf`, `htmltools`, `DT`
- Para export de PDF: `pagedown`, `rmarkdown`, `rsvg`

**Para recriar as bases (ETL em `data-raw/`):**
- `here`, `readxl`, `janitor`, `fs`, `microdatasus`, `arrow`, `dplyr`, `geobr`, `rmapshaper`

---

## Como rodar localmente

1. Garanta que `data/` contém todos os `.rds` listados na descrição de `app.R` acima.
2. Na raiz do projeto:

```r
install.packages(c("shiny", "data.table", "DiagrammeR", "leaflet", "sf", "ggplot2", "DT"))
shiny::runApp()
```

---

## Recriar as bases (ETL)

Rodar os scripts **a partir da raiz do projeto**. Saída esperada: `data/` com todos os `.rds`.

```r
# Pipeline completo:
source("data-raw/06_make_all_cc.R")

# Ou etapa por etapa:
source("data-raw/01_prepare_cc.R")           # globais GLOBOCAN
source("data-raw/02_prepare_BR.R")           # populações Brasil
source("data-raw/03_prepare_SUS.R")          # SIA 2024
source("data-raw/07_prepare_cito_presets.R") # presets citologia
source("data-raw/08_prepare_geometrias.R")   # shapefiles
source("data-raw/09_prepare_peers.R")        # peers
source("data-raw/04_checks_cc.R")            # checagens
```

---

## Deploy (shinyapps.io)

A pasta publicada deve conter:
- `app.R`
- `R/` (todos os `.R`)
- `data/` (todos os `.rds`)
- `www/` (CSS, logos, template Rmd)

```r
library(rsconnect)
rsconnect::deployApp(
  appDir  = ".",
  appName = "shiny-cc-dashboard",
  account = "SEU_USUARIO",
  server  = "shinyapps.io",
  quarto  = FALSE
)
```

---

## Troubleshooting

1. **`inferAppPrimaryDoc()` falha** — confirme que `app.R` existe na raiz do `appDir`.
2. **App sobe mas reclama de arquivo em `data/`** — rode `source("data-raw/06_make_all_cc.R")`.
3. **Pacote faltando em produção** — instale localmente e rode o app antes do deploy. Não use `install.packages()` dentro de `app.R`.

---

## Versionamento

- `vX.Y` — releases (ex.: v1.7)
- `vX.Y.Z` — hotfix (ex.: v1.7.1)

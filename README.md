
# Shiny-BC (Breast Cancer Screening Planning Dashboard)

Dashboard Shiny para planejamento e simulação de necessidades em rastreamento/diagnóstico do câncer de mama, integrando:
- **cenários epidemiológicos e operacionais** (cobertura, intervalo, positividade, BI-RADS etc.)
- **estimativas globais** (bases preparadas via `data-raw/01_prepare_bc.R`)
- **bases Brasil/SUS** (IBGE/ANS/regionalização + produção SIA/SUS)

---

## Principais funcionalidades

- **Filtros globais** (país, idade-alvo, modo de população, etc.)
- **Resumo geral** (KPIs e indicadores consolidados)
- **Resumo por país** (cards e gráficos por idade)
- **Equipamentos e RH** (necessidade vs capacidade)
- **Fluxo de rastreamento e diagnóstico** (diagrama com contagens e percentuais)
- **Capacidade** (produção SUS/SIA e comparação com necessidade)

---

## Estrutura do projeto

app.R
R/
00_constants_bc.R
01_utils_bc.R
02_engine_capacity_bc.R
04_engine_programming_bc.R
10_mod_filters_bc.R
11_mod_resumo_geral.R
12_mod_resumo_pais.R
14_mod_equipamentos.R
15_mod_fluxo.R
16_mod_capacidade.R
99_mod_sobre.R
data/
df_cancer_de_mama_completo.rds
df_cancer_de_mama_taxas.rds
df_dim_country.rds
df_dim_age.rds
df_dim_type.rds
df_dim_year.rds
pop_municipio_faixas.rds
pop_municipio_faixas_total_sus.rds
pop_municipio_regional.rds
regional_sus_map.rds
sia_bc_resumo.rds
data-raw/
01_prepare_bc.R
02_prepare_BR.R
03_prepare_SUS.R
04_checks_bc.R
05_glossary_bc.R
06_make_all_bc.R


---

## Requisitos

### Para rodar o app
- R (>= 4.1 recomendado)
- Pacotes:
  - `shiny`, `data.table`, `grid`, `DiagrammeR`

### Para recriar as bases (ETL em `data-raw/`)
- Pacotes adicionais (instalados conforme necessário pelos scripts):
  - `here`, `readxl`, `janitor`, `fs`, `microdatasus`, `arrow`, `dplyr`

---

## Como rodar localmente

1) Garanta que a pasta `data/` contém os `.rds` exigidos (lista acima).  
2) No R, a partir da raiz do projeto:

```r
install.packages(c("shiny","data.table","DiagrammeR"))
shiny::runApp()
Dados de entrada esperados (app)

O app.R faz readRDS() dos seguintes arquivos em data/:

df_cancer_de_mama_completo.rds
df_cancer_de_mama_taxas.rds
df_dim_country.rds
df_dim_age.rds
df_dim_type.rds
df_dim_year.rds
pop_municipio_faixas.rds
pop_municipio_faixas_total_sus.rds
pop_municipio_regional.rds
regional_sus_map.rds
sia_bc_resumo.rds
## Como recriar as bases (ETL)

> Rode os scripts **a partir da raiz do projeto**.  
> A saída final esperada é a pasta `data/` com os `.rds` consumidos pelo `app.R`.

### Pipeline recomendado

1) **Preparar bases globais (câncer de mama / parâmetros / dimensões)**
```r
source("data-raw/01_prepare_bc.R")
Preparar bases Brasil (população / dimensões / recortes)

r
Copiar código
source("data-raw/02_prepare_BR.R")
Preparar bases SUS (produção / SIA)

r
Copiar código
source("data-raw/03_prepare_SUS.R")
Checagens

r
Copiar código
source("data-raw/04_checks_bc.R")
Glossário (opcional)

r
Copiar código
source("data-raw/05_glossary_bc.R")
Rodar tudo de uma vez

r
Copiar código
source("data-raw/06_make_all_bc.R")
Deploy (shinyapps.io / rsconnect)
Estrutura recomendada para deploy
A pasta que será publicada deve conter:

app.R

R/ (scripts carregados via source())

data/ (os .rds já prontos)

Exemplo de deploy
r
Copiar código
library(rsconnect)

rsconnect::deployApp(
  appDir  = ".",                 # raiz do projeto (onde está o app.R)
  appName = "shiny-bc-dashboard",
  account = "SEU_USUARIO",
  server  = "shinyapps.io",
  quarto  = FALSE
)
Troubleshooting comum
1) Erro inferAppPrimaryDoc() / Failed to determine appPrimaryDoc
Garanta que existe app.R na raiz do appDir

Evite deploy de pasta com conteúdo “static” (sem app.R/server.R/ui.R)

Se você está publicando uma subpasta (ex.: app/), confirme que app/app.R existe e que o appDir="app" aponta para ela

2) App sobe mas quebra por arquivo ausente em data/
Rode o ETL (data-raw/06_make_all_bc.R)

Confirme que os .rds listados na seção “Dados de entrada esperados” existem em data/

3) Falta de pacotes no servidor
Instale localmente e rode o app antes do deploy

Evite install.packages() dentro do app

Versionamento
Sugestão (git tags):

vX.Y para releases (ex.: v1.7)

vX.Y.Z para hotfix (ex.: v1.7.1)


# ===========================================================
# Shiny-cc — R/00_constants_cc.R
# Constantes e paletas
# ===========================================================

AGE_ORDER <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29",
  "30-34","35-39","40-44","45-49","50-54","55-59",
  "60-64","65-69","70-74","75-79","80-84","85+"
)

TYPE_ORDER <- c("Incidence", "Mortality")

SEX_LABELS <- c(Female = "Female")

# --- CORES ---
CC_COLORS <- list(
  primary        = "#4ABDAC",
  primary_dark   = "#0B7285",
  petrol         = "#0F6B7A",
  forest         = "#2B7A5E",
  primary_darker = "#094B59",
  primary_light  = "#D9F3F2",
  
  text           = "#111111",
  gray1          = "#343A40",
  gray2          = "#868E96",
  bg             = "#F1F3F5",
  white          = "#FFFFFF"
)

PALETTE_MAIN <- c(
  CC_COLORS$primary_dark,
  CC_COLORS$petrol,
  CC_COLORS$primary,
  CC_COLORS$forest,
  "#0CA678",
  "#2F9E44",
  "#087F5B",
  "#2B8A3E",
  "#20C997",
  "#96F2D7"
)

# ---- Defaults HPV (percentuais em %) ----
HPV_DEFAULTS <- list(
  p16_18       = 3.43231398040085,
  poutros      = 9.26281577781061,
  pneg         = 87.3048702417885,
  cito_out_pos = 29.7164667393675,
  cito_out_neg = 70.2835332606325,
  colpo16_pos  = 68.2885906040269,
  colpo16_neg  = 31.7114093959732,
  colpoout_pos = 72.1428571428571,
  colpoout_neg = 27.8571428571429,
  b16_neg_nic1 = 59.7051597051597,
  b16_nic23    = 37.3464373464373,
  b16_cancer   = 2.94840294840295,
  bo_neg_nic1  = 65.016501650165,
  bo_nic23     = 33.993399339934,
  bo_cancer    = 0.99009900990099
)

# ---- Defaults Citologia (modelo de fluxo; percentuais em %) ----
CITO_DEFAULTS <- list(
  # nó 1: realização do exame
  first_time_pct    = 6.0,   # % (repetição anual)
  unsatisfactory_pct = 1.2,  # % (calculado sobre (trienal + first_time))
  
  # nó 2: resultado da citologia (somar 100%; auto-ajuste no filtro)
  res_asch_pct  = 1.4,
  res_other_pct = 2.8,
  res_neg_pct   = 95.8,      # placeholder (auto-ajuste)
  
  # nó 3: colposcopia
  colpo_asch_pct        = 100.0,
  colpo_other_follow_pct = 20.9,
  
  # biópsia (positividade entre colposcopias)
  biopsy_pos_asch_pct  = 33.3,
  biopsy_pos_other_pct = 33.3,
  
  # desfecho da biópsia (entre biópsias positivas) — ASC-H+
  b_asch_nic23_pct    = 70.0,
  b_asch_cancer_pct   = 15.0,
  b_asch_neg_nic1_pct = 15.0,
  
  # desfecho da biópsia (entre biópsias positivas) — outras alterações
  b_other_nic23_pct    = 70.0,
  b_other_cancer_pct   = 15.0,
  b_other_neg_nic1_pct = 15.0
)



# ---- Tooltips (CCU) — Summary cards ----
if (!exists("cc_TOOLTIPS", inherits = FALSE)) cc_TOOLTIPS <- list()

cc_TOOLTIPS$resumo_geral_ccu <- list(
  common = list(
    scope_prefix   = "Geographic selection: ",
    pop_selected   = "Target female population in the selected age range, based on the selected population source and (if Brazil) geographic filters.",
    eligible       = "Eligible women = pop_selected × coverage.",
    screened_year  = "HPV: eligible / 5 (assumes 5-year interval). Cytology: eligible × screening cytology proportion."
  ),
  hpv = list(
    cito_reflexa      = "Women with other HR-HPV positive requiring reflex cytology (per HPV pathway parameters).",
    colpo_indicada    = "Colposcopy indicated: HPV16/18 positive OR other HR-HPV positive with reflex cytology positive.",
    biopsia_indicada  = "Biopsy indicated: positive colposcopy.",
    ezt               = "Excisional treatment indicated (CIN2+ excluding invasive cancer).",
    alta_complexidade = "High complexity: invasive cancer (from biopsy).",
    retorno_1ano      = "Return in 1 year: negative/NIC1 biopsy OR negative colposcopy OR other HR-HPV positive with reflex cytology negative."
  ),
  cytology = list(
    cit_rastreamento      = "Screening cytologies per year: eligible/3 + eligible×first-time% + eligible×unsatisfactory%.",
    cit_diagnostica       = "Diagnostic (repeat) cytologies: screening cytologies × proportion with 'Other abnormalities'.",
    colpo_indicada        = "Initial colposcopies: (ASC-H+ cytologies × colposcopy after ASC-H+) + (Other abnormalities × colposcopy after other abnormalities).",
    biopsia_indicada      = "Biopsies indicated: initial colposcopies × colposcopy positivity (biopsy indication), for each arm (ASC-H+ and Other).",
    followup_cytologies   = "Follow-up cytologies: negative colposcopies + (biopsy negative/NIC1) + (EZT × 6 follow-up cytologies).",
    followup_colposcopies = "Follow-up colposcopies: negative colposcopies + (EZT × 2 follow-up colposcopies).",
    ezt                  = "Excisional treatment indicated (CIN2/3 from positive biopsies; ASC-H+ and Other arms).",
    alta_complexidade     = "High complexity: invasive cancer (from positive biopsies; ASC-H+ and Other arms)."
  )
)
cc_TOOLTIPS$capacidade_ccu <- list(
  citologia_total   = "Total SUS (SIA) cytology tests (2024) in the selected geography.",
  colposcopia_total = "Total SUS (SIA) colposcopies (2024) in the selected geography.",
  biopsia_total     = "Total SUS (SIA) biopsies (2024) in the selected geography.",
  ezt_total         = "Total SUS (SIA) excisional treatments (EZT) (2024) in the selected geography.",
  comp_citologia    = "Comparison between actual production (SUS/SIA, 2024) and estimated need for cytology.",
  comp_colposcopia  = "Comparison between actual production (SUS/SIA, 2024) and estimated need for colposcopy.",
  comp_biopsia      = "Comparison between actual production (SUS/SIA, 2024) and estimated need for biopsies.",
  comp_ezt          = "Comparison between actual production (SUS/SIA, 2024) and estimated need for excisional treatment (EZT)."
)

cc_TOOLTIPS$equipamentos_ccu <- list(
  colposcope      = "Colposcopes required to meet the estimated colposcopy demand (need / annual device capacity).",
  colposcopist    = "Colposcopists (20h/week) required to meet the estimated colposcopy demand (need / annual provider capacity).",
  cytopathologist = "Cytopathologists required to meet the estimated cytology volume (need / annual capacity per cytopathologist).",
  pathologist     = "Pathologists (20h/week) required to meet the estimated pathology workload (need / annual capacity per pathologist)."
)



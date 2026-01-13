

extract_kv_pdf <- function(file) {
  
  txt <- pdf_text(file)
  
  # drop first page (graphs)
  pages <- txt[-1]
  
  rows <- pages |>
    str_split("\n") |>
    unlist() |>
    str_squish()
  
  # keep only rows starting with date MM.YYYY
  rows <- rows[str_detect(rows, "^\\d{2}\\.\\d{4}")]
  
  tibble(raw = rows) |>
    separate(
      raw,
      into = c("Kuupäev", "Hind_m2", "Kuulutusi", "Aktiivseid"),
      sep = "\\s+",
      fill = "right"
    ) |>
    mutate(
      Hind_m2   = as.numeric(Hind_m2),
      Kuulutusi = as.numeric(na_if(Kuulutusi, "-")),
      Aktiivseid = as.numeric(na_if(Aktiivseid, "-")),
      source_file = basename(file)
    )
}


pdf_dir <- file.path(base_path,"EE", "sourcedata", "pdf_prices", "flat_all")

pdf_files <- list.files(
  pdf_dir,
  pattern = "\\.pdf$",
  full.names = TRUE
)

kv_panel <- map_dfr(pdf_files, extract_kv_pdf)










setDT(kv_panel)

#------------------------------------------------------
# Clean variables + extract year
#------------------------------------------------------
kv_panel[, year := as.integer(str_extract(Kuupäev, "\\d{4}$"))]

kv_panel[, Hind_m2 := as.numeric(Hind_m2)]
kv_panel[, Kuulutusi := as.numeric(Kuulutusi)]
kv_panel[, Aktiivseid := as.numeric(Aktiivseid)]

#------------------------------------------------------
# Collapse to YEARLY data
#------------------------------------------------------

## OPTION A (RECOMMENDED): by year × source_file
kv_year <- kv_panel[
  ,
  .(
    price_m2   = mean(Hind_m2, na.rm = TRUE),
    advertisments_sum = sum(Kuulutusi, na.rm = TRUE), 
    actives_sum = sum(Aktiivseid, na.rm= TRUE)
  ),
  by = .(source_file, year)
]

#------------------------------------------------------
# Order result
#------------------------------------------------------
setorder(kv_year, source_file, year)

#------------------------------------------------------
# Final result
#------------------------------------------------------
kv_year

kv_year[, parish :=
           str_trim(
             str_extract(
               source_file,
               "(?<=Korterite müük - ).*?(?= VS)"
             )
           )
]

kv_year$type<-"flat"
kv_year$typev2<-"all"
kv_year[, price_m2 := ifelse(is.nan(price_m2) | price_m2 == 0, NA, round(price_m2))]



# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "EE_lau2.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names for merging ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
kv_year$region_norm <- normalize_name(kv_year$parish)
lau$region_norm         <- normalize_name(lau$`LAU NAME NATIONAL`)
lau$region_norm <- gsub(" linn", "", lau$region_norm)
# --- Merge dwelling data with LAU codes ---
stock_final <- kv_year %>%
  left_join(lau, by = "region_norm") %>%
  dplyr::select(
    parish, year, price_m2,advertisments_sum, actives_sum, type, typev2, 
    `NUTS 3 CODE 2021`, `NUTS 3 CODE 2016`, `LAU CODE`
  )

write.csv(
  stock_final,
  file = file.path(source_prices_path, "EE_flat_all.csv"),
  row.names = FALSE
)

kv_year[, .(
  n_parish      = uniqueN(parish),
  n_price_na    = uniqueN(parish[is.na(price_m2)])
), by = year][order(year)]

kv_year[year == 2025 & is.na(price_m2), unique(parish)]

# 1] "Kihnu vald"  "Muhu vald"   "Ruhnu vald"  "Vormsi vald" missing
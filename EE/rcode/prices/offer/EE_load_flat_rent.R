

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


pdf_dir <- file.path(base_path,"EE", "sourcedata", "pdf_prices", "flat_rent")

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
              "(?<=Korterite üürimine - ).*?(?= VS)"
            )
          )
]

#Hinnastatistika - Korterite üürimine - Jõgeva vald VS Jõgeva vald.pdf

kv_year$type<-"flat"
kv_year$typev2<-"rent"
kv_year[, price_m2 := ifelse(is.nan(price_m2) | price_m2 == 0, NA, round(price_m2))]


write.csv(
  kv_year,
  file = file.path(source_prices_path, "EE_flat_rent.csv"),
  row.names = FALSE
)

kv_year[, .(
  n_parish      = uniqueN(parish),
  n_price_na    = uniqueN(parish[is.na(price_m2)])
), by = year][order(year)]

url1<- "https://statistikdatabasen.scb.se/api/v2/tables/TAB1148/data?lang=en&valueCodes[ContentsCode]=BO0501R5&valueCodes[Lan]=00,01,03,04,05,06,07,08%2B09,10,12,13,14,17,18,19,20,21,22,23,24,25&valueCodes[Tid]=1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024"


# Helper function to download PX and convert to data frame
download_px <- function(url) {
  px_file <- tempfile(fileext = ".px")
  download.file(url, px_file, mode = "wb")
  df <- as.data.frame(read.px(px_file))
  return(df)
}

# Download both PX files
df1 <- download_px(url1)

df1<-df1[df1$county!="Sweden",]

df1$county <- as.character(df1$county)

gotland<-df1[df1$county=="Counties of Kalmar och Gotland",]
gotland$county<-"Counties of Gotland"

df1<-rbind(df1,gotland)
nuts_map <- c(
  "Stockholm county"               = "SE110",
  "Uppsala county"                 = "SE121",
  "Södermanland county"            = "SE122",
  "Östergötland county"            = "SE123",
  "Jönköping county"               = "SE211",
  "Kronoberg county"               = "SE212",
  "Counties of Kalmar och Gotland" = "SE213",
  "Counties of Gotland"            = "SE214",
  "Blekinge county"                = "SE221",
  "Skåne county"                   = "SE224",
  "Halland county"                 = "SE231",
  "Västra Götaland county"         = "SE232",   
  "Värmland county"                = "SE311",
  "Örebro county"                  = "SE124",   
  "Västmanland county"             = "SE125",   
  "Dalarna county"                 = "SE312",   
  "Gävleborg county"               = "SE313",   
  "Västernorrland county"          = "SE321",   
  "Jämtland county"                = "SE322",   
  "Västerbotten county"            = "SE331",
  "Norrbotten county"              = "SE332"
)


df1$nuts3 <- nuts_map[df1$county]

# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  df1,
  file = file.path(source_prices_path, "SE_hpi_region.csv"),
  row.names = FALSE
)


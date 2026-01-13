setwd("C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_ari/SE")
# SWEDEN WEBSCRAPED

base <- fread(paste0('sourcedata/prices/',cc,'_ads_nuts.csv'),encoding="UTF-8")
dmicro <- base

dmicro <- dmicro %>% mutate(date_adscrape=ad_scraping_date,
                            date_adcreate=NA,
                            date_adadjust=NA,
                            idhref=ad_id,
                            type="flat",
                            type=replace(type,ad_dwelling_type=="detached house","house"),
                            type=replace(type,ad_dwelling_type=="link-detached house","house"),
                            type=replace(type,ad_dwelling_type=="townhouse","house"),
                            type=replace(type,ad_dwelling_type=="house","house"),
                            postcode=NA,
                            city_name=ad_municipality,
                            city_name=gsub("?s kommun","?s",city_name,fixed=TRUE),
                            city_name=gsub("fors kommun","fors",city_name,fixed=TRUE),
                            city_name=gsub("s kommun","",city_name,fixed=TRUE),
                            city_name=gsub(" kommun","",city_name,fixed=TRUE),
                            ad_price=ad_num_price,
                            ad_price_fx = ad_price/10,2805,
                            ad_fx = "EUR",
                            rooms= ad_rooms,
                            rooms=replace(rooms,rooms=="n/a",NA),
                            floor_m2=as.numeric(ad_dwelling_num_size),
                            construct_year=as.numeric(ad_construction_year),
                            construct_year=replace(construct_year,construct_year=="n/a",NA),
                            construct_year=replace(construct_year,construct_year<1000,NA),
                            price_m2=ad_price/floor_m2,
                            LAU_code = NA,
                            GEOnat_code=NA,
                            NUTS_code=NA,
                            isnew=FALSE,
                            isnew=replace(isnew,construct_year>=2018,TRUE),
                            isnew=replace(isnew,construct_year=="n/a",NA),
                            ad_details=ad_href,
                            scraper="luca",
                            plot_m2=NA) %>% 
                     dplyr::filter(rooms>0 & floor_m2>10 & floor_m2<5000 & price_m2>10 & price_m2<100000)


#################################################
######### MATCH ZIP CODES WITH LAU CODES ########
#################################################
# 
# Load coordinates from donwload.geonames.org
# temp <- tempfile()
# download.file(paste0('http://download.geonames.org/export/zip/',cc,'.zip'), temp)
# geodata <-  fread(unzip(temp, paste0('sourcedata/laucodes/',cc,'.txt')))
# geodata <- geodata %>% mutate(id=as.character(str_pad(V2, width=5, side="left", pad="0")),
#                               latitude=V10,
#                               longitude=V11) %>% dplyr::select(id,latitude,longitude)
# 
# fwrite(geodata,'sourcedata/laucodes/latlongstuff.csv')
# 
# # Match zip with lau codes using coordinates
# if (!file.exists('sourcedata/laucodes/latlongstuff_withLAUcode.csv')) {
# source("C:/Users/schufjo/Housing/RegionalLevels/reghp_sourcedata/00_utilities/matchlatlong2LAU.R")
# } else {blist <- fread('sourcedata/laucodes/latlongstuff_withLAUcode.csv') %>% dplyr::select(id,LAUcode) %>% distinct(id, LAUcode,.keep_all=TRUE)}
# # blist <- blist %>% mutate(id=as.character(str_pad(id, width=5, side="left", pad="0")))

lau_pop <- read_excel("C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_ari/00_utilities/EU-28-LAU-2018-NUTS-2016.xlsx",sheet=paste0(cc)) %>% 
  dplyr::select(`NUTS 3 CODE`,`LAU CODE`,POPULATION, `LAU NAME NATIONAL`)
lau_pop <- lau_pop %>% mutate(lau=as.character(paste0(cc,'_',`LAU CODE`))) %>% dplyr::select(-`LAU CODE`)
# 
#  lau_pop <- blist %>% left_join(lau_pop,by=c("LAUcode"="lau")) %>% dplyr::filter(!is.na(POPULATION))
# 
#  zip_lau_unique<-lau_pop %>%
#    group_by(id) %>%
#    slice(which.max(POPULATION)) %>% dplyr::select(-POPULATION)


dmicro <- dmicro %>% left_join(lau_pop,by=c("city_name"="LAU NAME NATIONAL")) %>% dplyr::filter(!is.na(lau))
dmicro$LAU_code <- dmicro$lau

# drop all observations that could not be matched to LAU code (<100)
dmicro <- dmicro[!is.na(dmicro$LAU_code),]
dmicro$NUTS_code <- dmicro$`NUTS 3 CODE`

dmicro <- subset(dmicro,select=c(date_adscrape,date_adcreate,date_adadjust,idhref,type,postcode,city_name,ad_price,ad_price_fx,ad_fx,rooms,floor_m2,construct_year,price_m2,LAU_code,GEOnat_code,NUTS_code,isnew,plot_m2,ad_details,scraper))
dmicro <- dmicro %>% mutate_all(as.character)

fwrite(dmicro,paste0('outputdata/webscraped/',cc,'_webscraped_clean.csv'))

            

library(data.table)
#this is to be run whenever microdata/EU_webscraped_clean.csv is updated
if (!dir.exists('00_disseminate')) setwd('U:/Topics/Housing/RegionalLevels/reghp_sourcedata/')
source('00_utilities/utilities_light.R')


if (!exists('eumicroraw')) eumicroraw=fread('00_fulldataset/outputdata/microdata/EU_webscraped_clean.csv')
eumicro = eumicroraw[isnew==FALSE | is.na(isnew),list(year=as.integer(substr(date_adscrape,0,4)), type=type, price_m2=ifelse(is.na(ad_price_fx),ad_price,ad_price_fx)/floor_m2, nuts=NUTS_code)]

if (!exists('dfx')) dfx=fread('00_utilities/fxrates/fx2eur_annual.csv',header=TRUE)

if (!exists('eudataraw')) eudataraw=fread('00_fulldataset/outputdata/EU_regio.csv', colClasses=c('year'='integer'))
if (!exists('dmetro')) dmetro=fread('00_disseminate/countrypages/sourcedata/Metro-regions-NUTS-2016.csv',header=TRUE)


pricebyregtype=function(cc,cutoff_topmetro=.15,yyear=2018, localccy=TRUE) {
  dictmetro=dmetro[,MREG_CODE]; names(dictmetro) = dmetro[,NUTS_ID]
  dcc=eudataraw[nchar(nuts)==5 & substr(nuts,0,2)==cc &  indicator %in% c('totalm2','ppm2'),list(nuts,year,indicator,type,value)]  #dcast(,nuts+year~indicator)
  dcc=dcc[!(nuts %in% c('ES630','ES640','FRY50','FRY40','FRY30','FRY20','FRY10'))]
  dcc=dcc[year %in% dcc[,length(unique(.SD$nuts))==length(unique(dcc$nuts)),by='year'][V1==TRUE,year]]
  dcc[,mreg:=dictmetro[nuts]]
  topmetros = dcc[indicator=='totalm2' & year==2016,sum(value,na.rm=TRUE)/sum(dcc[indicator=='totalm2' & year==2016,value],na.rm=TRUE),by='mreg'][!is.na(mreg)]
  setkey(topmetros,V1)
  topmetros=topmetros[nrow(topmetros):1,]
  ttt=topmetros[V1>= topmetros[cumsum(V1)>cutoff_topmetro,V1][1],mreg]; if (!length(ttt)) { if (sum(topmetros[,V1])>cutoff_topmetro) ttt=topmetros[1,mreg] else ttt=topmetros[,mreg]}
  dcc[nuts %in% dmetro[MREG_CODE %in% ttt & `Country code`==cc,NUTS_ID],metrorank:=1L]
  dcc[nuts %in% dmetro[`Country code`==cc,NUTS_ID] & is.na(metrorank),metrorank:=2L]
  dcc[is.na(metrorank),metrorank:=3L]
  temp=dcast(dcc,nuts+year+type+metrorank~indicator)
  if (temp[year==yyear,all(is.na(ppm2))]) {yyear= max(temp[!is.na(ppm2),year]); warning('yyear adjusted to ',yyear,' for lack of data') }
  temp=temp[year==yyear,list(price_m2=sum(ppm2*totalm2)/sum(totalm2)),by=c('metrorank','year','type')]
  if (!localccy) return(temp)
  if (cc %in% substr(dfx[,CURRENCY],0,2)) { 
    temp[,price_m2:=price_m2*dfx[substr(CURRENCY,0,2)==cc,as.character(yyear),with=FALSE][[1]]]
  }
  temp
}

gettopmetros=function(cc,cutoff_topmetro=.15,yyear=2018) {
  dictmetro=dmetro[,MREG_CODE]; names(dictmetro) = dmetro[,NUTS_ID]
  dcc=eudataraw[nchar(nuts)==5 & substr(nuts,0,2)==cc &  indicator == 'totalm2' & year==yyear & type=='all',list(nuts,year,indicator,type,value)]  #dcast(,nuts+year~indicator)
  dcc=dcc[!(nuts %in% c('ES630','ES640','FRY50','FRY40','FRY30','FRY20','FRY10'))]
  #dcc=dcc[year %in% dcc[,length(unique(.SD$nuts))==length(unique(dcc$nuts)),by='year'][V1==TRUE,year]]
  dcc[,mreg:=dictmetro[nuts]]
  topmetros = dcc[,sum(value,na.rm=TRUE)/sum(dcc[,value],na.rm=TRUE),by='mreg'][!is.na(mreg)]
  setkey(topmetros,V1)
  topmetros=topmetros[nrow(topmetros):1,]
  ttt=topmetros[V1>= topmetros[cumsum(V1)>cutoff_topmetro,V1][1],mreg]; if (!length(ttt)) { if (sum(topmetros[,V1])>cutoff_topmetro) ttt=topmetros[1,mreg] else ttt=topmetros[,mreg]}
  dcc[nuts %in% dmetro[MREG_CODE %in% ttt & `Country code`==cc,NUTS_ID],metrorank:=1L]
  dcc[nuts %in% dmetro[`Country code`==cc,NUTS_ID] & is.na(metrorank),metrorank:=2L]
  dcc[is.na(metrorank),metrorank:=3L]
  tempvec = dcc$metrorank; names(tempvec)=dcc$nuts; 
  return(tempvec)
}

ccc=unique(substr(eudataraw$nuts,0,2)); vmetrorank=integer()
for (cc in ccc) {
  vmetrorank=c(vmetrorank,gettopmetros(cc))
}

eumicro=eumicro[nuts%in% eudataraw$nuts]
eumicro[,metrorank:=vmetrorank[nuts]]
eumicro=merge(eumicro,eudataraw[indicator=='ppm2' & nchar(nuts)==5L & year>2016,list(nuts,year,type,avprice=value)],by=c('nuts','year', 'type'),all.x=TRUE)



library(ggplot2)
for (cc in list.files('00_disseminate/countrypages/countryinfo')) {

  temp=eumicro[substr(nuts,0,2) ==cc,];# qqq = quantile(temp[,price_m2],c(.02,.98))
  temp[,metrorank:=factor(metrorank, labels=c('Major metro','Other metro','Rural')[sort(unique(metrorank))])]
  temp[type=="",type:='all']
  temp=temp[is.finite(price_m2)]
  temp2=copy(temp); temp2[,type:='all']
  
  ppp = ggplot(rbind(temp2,temp), aes(x=metrorank, y=price_m2, fill=type)) + geom_boxplot()  + 
    ylim(c(0,quantile(temp$price_m2,0.995, na.rm=TRUE))) + ylab(paste0('price per m2 in EUR, ',max(temp$year))) + xlab(paste0('NUTS-3 regions by metropolitan area, N=',prettyNum(NROW(temp),,big.mark = ',')))
  #ppp + scale_fill_grey() + theme_classic()
  
  tempp=pricebyregtype(cc,yyear =median(temp$year),localccy = FALSE )[,c('metrorank','type','price_m2')]
  tempp[,metrorank:=factor(metrorank, labels=c('Major metro','Other metro','Rural')[sort(unique(metrorank))])]
  ppp= ppp+geom_point(data = tempp, shape=23, size=4 , position=position_dodge2(width=.75),  aes(fill = factor(type)))
  ggsave(ppp +theme_light(), filename = paste0('00_disseminate/countrypages/countryinfo/',cc,'/boxplots_',cc,'.png'), height =12, width=24,units='cm')
}
   
#pp2 = ggplot(temp, aes(x=metrorank, y=100*(price_m2-avprice)/avprice, fill=type)) + geom_boxplot() +ylim(c(-50,100))
#pp2 +theme_light()




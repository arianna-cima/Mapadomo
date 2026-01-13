
sw <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_v2"
setwd(sw)

library(dplyr); library(ggplot2); library(magrittr)
library(zoo)
library(data.table)
if (!exists('eudata')) eudata=fread('00_disseminate/EU_regio.csv')
if (!exists('destat')) destat=fread('00_disseminate/countrypages/sourcedata/estat_prc_hpi_a_exst.csv')
if (!exists('dmetro')) dmetro=fread('00_disseminate/countrypages/sourcedata/Metro-regions-NUTS-2016.csv',header=TRUE)

plotcontribs =function(cc, cutoff_topmetro=.15, plotm2=FALSE) {
  
  if (!is.character(cc)) stop('cc needs to be country code like FR')
  if (length(cutoff_topmetro)!=1 | cutoff_topmetro[[1]]<0| cutoff_topmetro[[1]]>1) { stop('cutoff_topmetro needs to be number between 0 and 1')}
  
  dictmetro=dmetro[,MREG_CODE]; names(dictmetro) = dmetro[,NUTS_ID]
  dcc=eudata[nchar(nuts)==5 & substr(nuts,0,2)==cc & type=='all' & indicator %in% c('totalm2','ppm2'),list(nuts,year,indicator,value)]  #dcast(,nuts+year~indicator)
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
  dc2=data.table(dcast(dcc,nuts+year+metrorank~indicator))
  temp=dc2[!is.na(totalm2),.SD[year==min(year),totalm2],by='nuts']; tempdictm2=temp[[2]]; names(tempdictm2)=temp[[1]]
  dc2[is.na(totalm2) & year<2016,totalm2:=tempdictm2[nuts]]
  temp=dc2[!is.na(totalm2),.SD[year==max(year),totalm2],by='nuts']; tempdictm2=temp[[2]]; names(tempdictm2)=temp[[1]]
  dc2[is.na(totalm2) & year>2015,totalm2:=tempdictm2[nuts]]
  dc2[,tval:=ppm2*totalm2]
  dc2=rbind(dc2[,lapply(.SD[,-1],sum),by=list(year,metrorank)],dc2[,lapply(.SD[,-(1:2)],sum),by=list(year)],fill=TRUE)
  dc2[,ppm2:=tval/totalm2]
  
  dc3=data.table(merge(dcast(dc2[,1:3], year~ metrorank, value.var = 'ppm2'),dcast(dc2[,c(1,2,4)], year~ metrorank, value.var = 'totalm2'),by='year',suffixes = c('price','m2')))
  if (!any(colnames(dc3)=='2price')) { dc3[,`2price`:=1]}
  if (!any(colnames(dc3)=='2m2')) { dc3[,`2m2`:=1]}
  
  stopmname=ifelse(length(ttt)>1,paste0('Top ',length(ttt),' metros'),paste0(dmetro[MREG_CODE %in% ttt][1,][[4]],' metro'))
  
  
  zz=zooreg(dc3[,-1],start = min(dc3$year),end=max(dc3$year), frequency = 1)
  zz=zz[!apply(zz,1,anyNA),]
  mycols=c('#333333','#666666','#999999','#FFFFFF')
  if (plotm2) {
    dd=(zz[,paste0(c(1:3,'NA'),'m2')]-stats::lag(zz[,paste0(c(1:3,'NA'),'m2')],-1))/stats::lag(zz[,'NAm2'],-1)*100
    dd[apply(dd,1,function(x) all(x==0)),]=NA
    tt=barplot(dd[,1:3], ylab='m2 added as % of national total',col=mycols)
    lines(tt,dd[,'NAm2'],col= 1, lwd=2)
    
  } else {
    
    
    mm=100*(zz[,paste0(1:3,'price')]/stats::lag(zz[,paste0(1:3,'price')],-1)-1)*stats::lag(zz[,paste0(1:3,'m2')]/zz[,'NAm2'],-1)
    mm=cbind(mm,compo=100*(zz[,'NAprice']/stats::lag(zz[,'NAprice'],-1)-1)-rowSums(mm))
    par(mar=c(7,4,1,1))
    tt=barplot(mm, ylab='% annual change',col=mycols)
    lines(tt,100*(zz[,1]/stats::lag(zz[,1],-1)-1),col= 1, lwd=2)
    testat=zooreg(destat[geo==cc ,v_a_l], start = min(destat[geo==cc ,TIME]), end=max(destat[geo==cc ,TIME]), frequency = 1)
    zz=cbind(zz,estat_exst=testat[as.character(index(mm))])
    lines(tt,zz[,9]/stats::lag(zz[,9],-1)*100-100,col= 2, lwd=2)
  }
  
  par(xpd=TRUE)
  if (nrow(topmetros)>length(ttt)) { legend(x=par('usr')[1]+diff(par('usr')[1:2])*.05,,y=par('usr')[3]-diff(par('usr')[3:4])*.1,legend = c(stopmname,paste0('Other ', nrow(topmetros)-length(ttt),  ' metros')),fill=mycols[1:2],bty='n') }
  if (nrow(topmetros)<=length(ttt)) { legend(x=par('usr')[1]+diff(par('usr')[1:2])*.05,,y=par('usr')[3]-diff(par('usr')[3:4])*.1,legend = c(stopmname),fill=mycols[1],bty='n') }
  legend(x=par('usr')[1]+diff(par('usr')[1:2])*.4,,y=par('usr')[3]-diff(par('usr')[3:4])*.1,legend = if (plotm2) 'Other areas' else c('Other areas','Compo. effect'),fill=mycols[if(plotm2) 3 else 3:4],bty='n')
  legend(x=par('usr')[1]+diff(par('usr')[1:2])*.65,y=par('usr')[3]-diff(par('usr')[3:4])*.1,legend = if (plotm2) 'Overall' else c('Overall','Estat index'),col=if(plotm2) 1 else 1:2,bty='n',lwd=ifelse(plotm2,2,c(2,2)))
  text(par('usr')[1],par('usr')[3]-diff(par('usr')[3:4])*.03,labels=cc)
  return(invisible(zz))
}





plotbubbles = function(cc) {
  
  dd=dcast(eudata[nchar(nuts)==5 & substr(nuts,0,2)==cc & type=='all' & indicator %in% c('totalm2','ppm2'),list(nuts,year,indicator,value)],nuts+year~indicator)
  dd[,totalm2:=totalm2/1e6]
  temp=dd[!is.na(totalm2),.SD[year==min(year),totalm2],by='nuts']; tempdictm2=temp[[2]]; names(tempdictm2)=temp[[1]]
  dd[is.na(totalm2),totalm2:=tempdictm2[nuts]]
  
  
  scapital=merge(dmetro[`Country code`==cc & grepl('MC$',MREG_CODE)],dd[year==max(year)],by.x='NUTS_ID',by.y='nuts')[totalm2==max(totalm2),NUTS_ID]
  dd[,NUTS3:=' other']; dd[nuts==scapital,NUTS3:=dmetro[NUTS_ID==scapital,`Name of the metro region`]]
  dd[!is.na(ppm2)] %>%
    mutate(nuts = as.factor(nuts)) %>%
    ggplot(aes(x=year, y=ppm2, size = totalm2,color=NUTS3)) +
    geom_point(alpha=0.2) +
    scale_size(range = c(.1, 24), name="Million m�") +
    scale_color_manual(values = c('darkgrey','darkblue')) 
  
}




# 
# 
# temp=dcast(eudata[ nchar(nuts)==5 & indicator %in% c('ppm2', 'm2perperson') & type=='all' & year %in% c(2011,2019)],nuts+year ~indicator, value.var='value')
# plot(temp[,3:4],log='y',type='n')
# points(temp[year==2011,3:4],col='grey')
# points(temp[year==2019,3:4],col='darkblue')
# 
# 
# temp2=merge(temp[year==2011,],temp[year==2019,], by='nuts')
# temp2[,gm2pp:=m2perperson.y/m2perperson.x-1]
# temp2[,gppm2:=ppm2.y/ppm2.x-1]
# plot(temp2[,8:9])

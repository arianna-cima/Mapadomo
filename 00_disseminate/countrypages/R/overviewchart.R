
#eudata[grepl('^AT',nuts) &indicator=='totalm2',value:=value*1000]

dmetro=fread('00_disseminate/countrypages/sourcedata/Metro-regions-NUTS-2016.csv',header=TRUE)

yy=2019
myd=merge(
eudata[nchar(nuts)==2L & indicator =='ppm2' & type=='all',.SD[year==min(max(year),yy), value],by='nuts'],
eudata[nchar(nuts)==2L & indicator =='totalm2' & type=='all',.SD[year==min(max(year),yy),list(value)]/1e6,by='nuts'], by='nuts',all=TRUE)
colnames(myd)=c('country','ppm2','m2')
#myd[,totalval:=V1*value]
setkey(myd,ppm2)
myd=myd[!is.na(m2) & !is.na(ppm2)]


plot(myd[[3]],myd[[2]],xlim=c(0,sum(myd[[3]])),type='n',
     xlab='Million m²', ylab=paste0('Price per m², ',yy),ylim=c(0,3400))
#rect(c(0,5000,7000),0,c(5000,7000,15000),c(2000,1500))
stackedm2=cumsum(rev(myd[,m2]))
rect(
  c(0,stackedm2[-length(stackedm2)]),
  0,
  stackedm2,
  rev(myd[,ppm2]),
  col='#CCCCCC')


metroreg=dmetro[grepl('MC$',MREG_CODE),NUTS_ID]
temp=data.table(dcast(eudata[nchar(nuts)==5L & nuts %in% metroreg & type=='all',list(nuts,indicator,year,value)], nuts+year~ indicator))
temp=temp[!is.na(ppm2) & !is.na(totalm2)]
temp=temp[year<yy+1, list(metro=sum(ppm2*totalm2)/1e6),by=list(country=substr(nuts,0,2),year)][,.SD[year==max(year)],by=country]

myd=merge(myd,temp[,-2],all=TRUE,by='country')
myd[,metrostart:=ppm2*(1-metro/(ppm2*m2))]
setkey(myd,ppm2)
rect(
  c(0,stackedm2[-length(stackedm2)]),
  rev(myd[,metrostart]),
  stackedm2,
  rev(myd[,ppm2]),
  col='#999999')

legend('topright',legend=c('capital metro', 'other regions'), bty='n',cex=.8,fill = c('#999999','#CCCCCC'))
text(.5*stackedm2+.5*c(0,stackedm2)[-length(stackedm2)],rev(myd[,ppm2])+c(350,600,100),rev(myd[,country]))

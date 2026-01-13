
#cc='SE'
if (!exists('surlserver')) surlserver='\\\\s-ecfin-web.net1.cec.eu.int\\wwwroot\\directorates\\db\\u1\\data\\housing\\regional_hp\\map2\\'
if (!exists('lsources')) lsources=readRDS('00_disseminate/interimdata/lsources.rds')
if (!exists('eudata')) destat=fread('00_disseminate/EU_regio.csv')

if (!exists('sheader')) sheader=paste(readLines('00_disseminate/countrypages/layoutstuff/header.html'),collapse='\n')
if (!exists('sfooter')) sfooter=paste(readLines('00_disseminate/countrypages/layoutstuff/footer.html'),collapse='\n')
source('00_disseminate/countrypages/R/chartpricegrowth.R')

lastestval=function(cc) {
  temp=copy(eudata[nuts==cc & type=='all',.SD[!is.na(value)][year==max(year),round(value,0)],by='indicator'])
  temp[abs(V1) %/% 1e12 != 0,lastval:=paste0(prettyNum(V1 %/% 1e9,big.mark = ' '), ' bn')]
  temp[abs(V1) %/% 1e9  != 0 & is.na(lastval),lastval:=paste0(prettyNum(V1 %/% 1e6,big.mark = ' '), ' mn')]
  temp[abs(V1) %/% 1e6  != 0 & is.na(lastval),lastval:=paste0(prettyNum(V1 %/% 1e3,big.mark = ' '), ' k')]
  temp[is.na(lastval),lastval:=paste0(prettyNum(V1,big.mark = ' '), '  ')]
  return(temp)
}


makecountry= function(cc) {
  myfolder=paste0('00_disseminate/countrypages/countryinfo/',cc)
  if (!dir.exists(myfolder)) dir.create(myfolder)
  sout=paste0('<h1>Metadata for ',cc,'</h1>\n')
  sout=paste0(sout,'<ul><li><a href="#avdata">Available data</a></li><li><a href="#results">Results overview</a></li><li><a href="#sources">Sources and definitions</a></li></ul>\n')
  
  sout=paste0(sout,'\n<h3 id="avdata">Available data</h3>\n')
  overviewtable=eudata[grepl(paste0('^',cc),nuts) & nchar(nuts)==5,c(as.list(range(year)),NROW(.SD[type=='all' &year==tail(year,1)]),as.integer(NROW(.SD[type=='flat'])>0)),by=c('indicator')]
  overviewtable=merge(overviewtable,lastestval(cc)[,c(1,3)])
  colnames(overviewtable) = c('Code','Start year','End year','NUTS-3 regions','Flat / houses','Latest value')
  sout=paste0(sout, '\n', print(xtable::xtable(overviewtable),type='html',print.results = FALSE, include.rownames = FALSE))
  sout=paste0(sout,'\n<p>Start and end year denote time range for which NUTS-3 data is available. NUTS-e3 regions denotes the number of NUTS-3 regions for which data is available. flats/houses: If indicator = 1 then this data is avaialable for the subset of apartments and houses. Latest value: shows the last available data for the national aggregate.</p>\n') 
  sout=paste0(sout,'\n<hr>\n<h3 id="results">Results overview</h3>\n') 
  sout=paste0(sout,'\n<h4>Prices over time</h4>\n') 
  sout=paste0(sout,'\n<p><img  id="bubbles" style="height:22em;" src="bubbles_',cc,'.png"><br>Each bubble denotes a NUTS-3 region. Blue colour indicates the capital NUTS-3 region.</p>\n') 
  sout=paste0(sout,'\n<h4>Contribution to overall price evolution</h4>\n') 
  sout=paste0(sout,'\n<p><img  id="contribs" style="height:22em;" src="contribs_',cc,'.png"><br>Each bar denotes the contribution from a specific group of regions to annual house price growth for the national aggregate, weighted by their residential square metres from the previous year.',
              'Capital/top metros denotes any NUTS-3 regions of which more than half belongs to the largest metropolitan areas (that together cover more than 15 percent of the population). Other metros denotes the rest of NUTS-3 regions which belong to metropolitan areas. For reference, the red line indicates the annual change of the Eurostat house price index for existing dwellings.</p>\n') 
  sout=paste0(sout,'\n<h4>Residential square metre growth, regional contribution</h4>\n') 
  sout=paste0(sout,'\n<p><img id="m2growth" style="height:22em;" src="m2growth_',cc,'.png"><br>Each bar denotes the contribution from a specific group of regions to annual growth of residential useful square metres for the national aggregate.</p>\n') 
  sout=paste0(sout,'\n<h4>Scraped prices per square metre, compared to stock-weighted average</h4>\n') 
  sout=paste0(sout,'\n<p><img id="boxplots" style="height:22em;" src="boxplots_',cc,'.png"><br>Boxplots denote distribution of the offer price per m2 for scraped listings, within regions that belong to three types of metropolitan area. Box denote 25% and 75% quantiles, whiskers correpond to 1.5 the inter-quartile range. Diamonds indicate the headline (stock-weighted) average price for the same period, i.e. the price from the main database used in the bubble chart above.</p>\n') 
  sout=paste0(sout,'\n<hr>\n<h3 id="sources">Sources and definitions</h3>\n',(paste(paste('<h4>',dictheader[names(lsources[[cc]])],' for ',cc,'</h4>\n<p>',trimws(gsub('\\n','<br>',lsources[[cc]])),'</p>',sep=''),collapse='\n')))
  
  
  cat(sheader,sout,sfooter,
      file=paste0(myfolder,'/index.html'))
  
  
  graphics.off()
  png(paste0(myfolder,'/contribs_',cc,'.png'), width = 512, height = 384)
  suppressMessages(plotcontribs(cc))
  dev.off()
  png(paste0(myfolder,'/m2growth_',cc,'.png'), width = 512, height = 384)
  suppressMessages(plotcontribs(cc, plotm2 = TRUE))
  dev.off()
  ggsave(filename=paste0(myfolder,'/bubbles_',cc,'.png'),plot=plotbubbles(cc), height =12, width=24,units='cm')
}


for (cc in unique(substr(eudata[!is.na(value) & indicator=='ppm2']$nuts,0,2))) {
  message(cc)
  makecountry(cc)
}

# file.copy('00_disseminate/countrypages/countryinfo',
#           to = surlserver, recursive = TRUE,overwrite = TRUE)



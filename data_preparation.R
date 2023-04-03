### Data preparation for the dashboard of asylum data in the EU+
### Libraries and functions -------------------------------------------------
library(eurostat)
library(lubridate)
library(tidyr)
library(dplyr)
library(zoo)
library(countrycode)

### Get asylum data from Eurostat -------------------------------------------
## Asylum applications
daty_o <- get_eurostat("migr_asyappctza", time_format = "date", type='label')

daty <- daty_o %>%
  filter(citizen=="Total", sex=='Total', age == 'Total', time >= "2011-01-01", geo!='Total') %>% 
  droplevels() %>% 
  rename (aseekers = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          id = countrycode(country, origin='country.name', destination='iso2c'),
          index.y = paste0(country, ".", year)) %>%
  filter(country!='European Union - 27 countries (from 2020)', country!='Montenegro') %>%
  select(-unit, -sex, -age, -citizen, -time) 

daty <- daty %>% 
  tidyr::pivot_wider(names_from=asyl_app, values_from=aseekers) %>%
  rename (aseekers =`Asylum applicant`, firstaseekers = `First time applicant`) %>%
  select (- `Subsequent applicant`)

### Asylum decisions
dec <- get_eurostat("migr_asydcfsta", time_format = "date", type='label')

decy <- dec %>%
  filter(citizen=="Total", sex=='Total', age == 'Total', time >= "2011-01-01", geo!='Total',
         decision=='Total positive decisions'| decision == 'Geneva Convention status') %>% 
  droplevels() %>% 
  rename (aseekersdecisions = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          id = countrycode(country, origin='country.name', destination='iso2c'),
          index.y = paste0(country, ".", year)) %>%
  filter(country!='European Union - 27 countries (from 2020)', country!='Montenegro') %>%
  select(-unit, -sex, -age, -citizen, -time) 

decy <- decy %>% 
  tidyr::pivot_wider(names_from=decision, values_from=aseekersdecisions) %>%
  rename (firstaseekersdec =`Geneva Convention status`, aseekersdec = `Total positive decisions`) %>% 
  select (-country, -year, -id)

# Temporary protection
tp<- get_eurostat("migr_asytpfa", time_format = "date", type='label')
tpy<- tp %>%
  filter(citizen=="Total", sex=='Total', age == 'Total', time >= "2011-01-01", geo!='Total') %>% 
  droplevels() %>% 
  rename (aseekersdecisionstemp = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          id = countrycode(country, origin='country.name', destination='iso2c'),
          index.y = paste0(country, ".", year)) %>%
  filter(country!='European Union - 27 countries (from 2020)', country!='Montenegro') %>%
  select(-unit, -sex, -age, -citizen, -time, -country, -id, -year) 

# Population
pop<- get_eurostat("tps00001", time_format = "date", type='label')
popul.y<-pop%>%
  filter(time >= "2011-01-01") %>%
  rename (popul.y = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          index.y = paste0(country, ".", year)) %>%
  select(-indic_de, -time) 

# GDP
gdp3<- get_eurostat("tec00001", time_format = "date", type='label')
gdp.y<-gdp3%>%
  filter(time >= "2011-01-01", unit=="Current prices, million euro") %>%
  droplevels() %>% 
  rename (gdp.y = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          index.y = paste0(country, ".", year)) %>%
  select(-na_item, -unit)

# GDP in PP
gdppp<- get_eurostat("nama_10_gdp", time_format = "date", type='label')
gdppp.y<-gdppp%>%
  filter(time >= "2011-01-01", unit=="Current prices, million purchasing power standards (PPS, EU27 from 2020)", na_item=='Gross domestic product at market prices') %>%
  droplevels() %>% 
  rename (gdppp.y = values, country = geo) %>%
  mutate (year = year(time),
          country = recode(country, "Germany (until 1990 former territory of the FRG)"="Germany"),
          index.y = paste0(country, ".", year)) %>%
  select(-na_item, -unit)

#Merge
daty<-left_join(daty, decy, by='index.y')  
daty<-left_join(daty, tpy, by='index.y')  
daty<-left_join(daty, select(popul.y, -country, -year), by='index.y')  
daty<-left_join(daty, select(gdp.y, -country, -time, -year), by='index.y')
daty<-left_join(daty, select(gdppp.y, -country, -time, -year), by='index.y')

# add missing data for Switzerland
summary(m1<-lm(gdppp.y~ gdp.y, data=daty[daty$country=='Switzerland', ]))
daty$gdppp.y[daty$country=='Switzerland' & daty$year==2021] <- coef(m1)[1]+coef(m1)[2]*daty$gdp.y[daty$country=='Switzerland' & daty$year==2021]
daty$gdppp.y[daty$country=='Switzerland' & daty$year==2022] <- coef(m1)[1]+coef(m1)[2]*daty$gdp.y[daty$country=='Switzerland' & daty$year==2022]

# add missing data for Norway
summary(m2<-lm(gdppp.y~ gdp.y, data=daty[daty$country=='Norway', ]))
daty$gdppp.y[daty$country=='Norway' & daty$year==2022] <- coef(m2)[1]+coef(m2)[2]*daty$gdp.y[daty$country=='Norway' & daty$year==2022]

# add missing data for Liechtenstein
daty[daty$country=='Liechtenstein' & daty$year=='2022', 'gdp.y'] <- daty[daty$country=='Liechtenstein' & daty$year=='2020', 'gdp.y']
daty[daty$country=='Liechtenstein' & daty$year=='2021', 'gdp.y'] <- daty[daty$country=='Liechtenstein' & daty$year=='2020', 'gdp.y']
daty[daty$country=='Liechtenstein' & daty$year=='2012', 'gdp.y'] <- daty[daty$country=='Liechtenstein' & daty$year=='2013', 'gdp.y']
daty[daty$country=='Liechtenstein' & daty$year=='2011', 'gdp.y'] <- daty[daty$country=='Liechtenstein' & daty$year=='2013', 'gdp.y']

daty$gdppp.y[daty$country=='Liechtenstein'] <- daty$gdp.y[daty$country=='Liechtenstein'] * 0.64

# add missing data for first-instance asylum applicants
daty$firstaseekers <- ifelse (is.na(daty$firstaseekers)==TRUE, daty$aseekers, daty$firstaseekers)

daty <- daty %>%
  mutate(aseekersdectemp = rowSums(dplyr::select(.,aseekersdecisionstemp,aseekersdec), na.rm=TRUE),
         
         aperpop = round(aseekers/popul.y*1e5,0),
         apergdp = round(aseekers/gdp.y*1e3,0),
         apergdppp = round(aseekers/gdppp.y*1e3,0),
         
         firstaperpop = round(firstaseekers/popul.y*1e5,0),
         firstapergdp = round(firstaseekers/gdp.y*1e3,0),
         firstapergdppp = round(firstaseekers/gdppp.y*1e3,0),
         
         aperpopdec = round(aseekersdec/popul.y*1e5,0),
         apergdpdec = round(aseekersdec/gdp.y*1e3,0),
         apergdpppdec = round(aseekersdec/gdppp.y*1e3,0),
         
         firstaperpopdec = round(firstaseekersdec/popul.y*1e5,0),
         firstapergdpdec = round(firstaseekersdec/gdp.y*1e3,0),
         firstapergdpppdec = round(firstaseekersdec/gdppp.y*1e3,0),
         
         aperpopdectemp = round(aseekersdectemp/popul.y*1e5,0),
         apergdpdectemp = round(aseekersdectemp/gdp.y*1e3,0),
         apergdpppdectemp = round(aseekersdectemp/gdppp.y*1e3,0),
         )

daty$id<-car::recode(daty$id, "'GB'='UK'")

daty.y <- daty %>% group_by(year) %>% 
  summarize(total.popul.y = sum (popul.y, na.rm=T),
            total.gdp.y = sum (gdp.y, na.rm=T),
            total.gdppp.y = sum (gdppp.y, na.rm=T),
            total.aseekers = sum (aseekers, na.rm=T),
            total.aseekersdec = sum (aseekersdec, na.rm=T),
            total.aseekersdectemp = sum (aseekersdectemp, na.rm=T),
            total.firstaseekersdec = sum (firstaseekersdec, na.rm=T))

daty <- left_join(daty, daty.y, by='year')

daty <- daty %>% mutate (
  aseekers.share = (aseekers+1)/total.aseekers*100,
  firstaseekers.share = (firstaseekers+1)/total.aseekers*100,
  aseekersdec.share = (aseekersdec+1)/total.aseekersdec*100,
  aseekersdectemp.share = (aseekersdectemp+1)/total.aseekersdectemp*100,
  firstaseekersdec.share = (firstaseekersdec+1)/total.firstaseekersdec*100,

  popul.y.share = popul.y/total.popul.y*100,
  aseekers.inequality.pop = log2(aseekers.share / popul.y.share),
  firstaseekers.inequality.pop = log2(firstaseekers.share / popul.y.share),
  aseekersdec.inequality.pop = log2(aseekersdec.share / popul.y.share),
  aseekersdectemp.inequality.pop = log2(aseekersdectemp.share / popul.y.share),
  firstaseekersdec.inequality.pop = log2(firstaseekersdec.share / popul.y.share),
  
  gdp.y.share = gdp.y/total.gdp.y*100,
  aseekers.inequality.gdp = log2(aseekers.share / gdp.y.share),
  firstaseekers.inequality.gdp = log2(firstaseekers.share / gdp.y.share),
  aseekersdec.inequality.gdp = log2(aseekersdec.share / gdp.y.share),
  aseekersdectemp.inequality.gdp = log2(aseekersdectemp.share / gdp.y.share),
  firstaseekersdec.inequality.gdp = log2(firstaseekersdec.share / gdp.y.share),
  
  gdppp.y.share = gdppp.y/total.gdppp.y*100,
  aseekers.inequality.gdppp = log2(aseekers.share / gdppp.y.share),
  firstaseekers.inequality.gdppp = log2(firstaseekers.share / gdppp.y.share),
  aseekersdec.inequality.gdppp = log2(aseekersdec.share / gdppp.y.share),
  aseekersdectemp.inequality.gdppp = log2(aseekersdectemp.share / gdppp.y.share),
  firstaseekersdec.inequality.gdppp = log2(firstaseekersdec.share / gdppp.y.share)
)

daty$country<-car::recode(daty$country, "'United Kingdom'='UK'")

summary(daty)

save (daty, file ='./data/asylum2012-2022.RData')

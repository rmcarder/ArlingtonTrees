---
title: "Arlington Trees"
author: "Rich Carder"
date: "March 31, 2020"
output: html_document
---
  
  
#install.packages("googlesheets4")
#install.packages("formattable")
#install.packages("htmltools")
  #install.packages("geojsonio")
library(googlesheets4)
library(formattable)
library(kableExtra)
library(ggthemes)
library(knitr)
library(tidycensus)
library(htmltools)
library(webshot)
library(sf)
library(haven)
library(jsonlite)
library(geojsonio)
#This script extracts ACS 5-year estimates at the block group (or any larger 
#geography) using the tidycensus package. To run tidycensus, you first need
#to set up a Census API key and run census_api_key(). Set working directory
#to where you want output files to save, or use the collect_acs_data function 
#to set a different outpath.
#

setwd("C:/Users/rcarder/Documents/dev/ArlingtonTrees")
countiesjson <- topojson_read("uscounties.json")
cities<-read.csv("cities.csv")

counties <- st_read("Tree_Canopy_2016_Polygons.shp") 


census_api_key('b2e47f1f1e9c7115a34a02992c149628712ecff8', install=TRUE, overwrite = TRUE)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidycensus, viridis,stringr,dplyr,knitr,DT,datasets)


##Fix fips field
countiesjson$fips<-as.character(countiesjson$fips)
topojson_write(FullData,file="TreeData.json")


#For code to run, need to first set up Census API key and run census_api_key()

acs_table <- load_variables(2018, "acs5", cache = TRUE)
view(acs_table)

#Set ACS parameters
# geography = 'block group'
# year = 2016
# state = 'OR'
# county = 'Lane'

collect_acs_data <- function(geography = 'tract', 
                             year = 2018, 
                             state = NULL, 
                             county = NULL,
                             outpath = '') {
  
    print(paste0('Processing state: ',state))
  
    #Income
    income <- get_acs(geography = geography, sumfile = 'acs5', 
                      variables = c('B19301_001', 'B17021_001', 'B17021_002'),
                      year = year, state = state, county = county, geometry = FALSE) %>%
              dplyr::select(-moe)%>%
              spread(key = 'variable', value = 'estimate') %>%
              mutate(
                tot_population = B17021_001,
                in_poverty = B17021_002) %>%
              mutate(
                inc_pct_poverty = in_poverty/tot_population,
                inc_pcincome = B19301_001
              ) %>%
              dplyr::select(-starts_with("B1"))
    
    language <- get_acs(geography = geography, sumfile = 'acs5', 
                      variables = c('B16001_001', 'B16001_002', 'B16001_003', 'B16001_004', 'B16001_005'),
                      year = year, state = state, county = county, geometry = FALSE) %>%
      dplyr::select(-moe)%>%
      spread(key = 'variable', value = 'estimate') %>%
      mutate(
        tot_population = B16001_001,
        only_english = B16001_002,
        spanish=B16001_003,
        spanish_with_english=B16001_004,
        spanish_no_english=B16001_005) %>%
      mutate(
        only_english_pct = only_english/tot_population,
        spanish_pct=B16001_003/tot_population,
        spanish_with_english_pct=B16001_004/tot_population,
        spanish_no_english_pct=B16001_005/tot_population
      ) %>%
      dplyr::select(-starts_with("B1"))
    
    #Race
    
    race <- get_acs(geography = geography, sumfile = 'acs5',
                    variables = c(sapply(seq(1,10,1), function(v) return(paste("B02001_",str_pad(v,3,pad ="0"),sep=""))),
                                  'B03002_001','B03002_002','B03002_003','B03002_012','B03002_013'),
                    year = year, state = state,county = county, geometry = FALSE) %>%
                    dplyr::select(-moe) %>%
                    spread(key = 'variable', value = 'estimate') %>% 
                    mutate(
                      race_pct_white = B02001_002/B02001_001,
                      race_pct_whitenh = B03002_003/B03002_001,
                      race_pct_nonwhite = 1 - race_pct_white,
                      race_pct_nonwhitenh = 1 - race_pct_whitenh,
                      race_pct_amind = B02001_004/B02001_001,
                      race_pct_black = B02001_003/B02001_001,
                      race_pct_aapi = (B02001_005+B02001_006)/B02001_001,
                      race_pct_native = B02001_004/B02001_001,
                      race_pct_hisp = B03002_012/B03002_001
                    ) %>%
                    dplyr::select(-starts_with("B0"))  
    
    #Age
    
 
    #Education
    
    educ <- get_acs(geography = geography, sumfile = 'acs5',
                    variables = c(sapply(seq(1,35,1), 
                                         function(v) return(paste("B15002_",str_pad(v,3,pad ="0"),sep="")))),
                    year = year, state = state, county = county, geometry = FALSE)
    
    output <- income %>% merge(age) %>% merge(educ) %>% merge(race)%>%merge(language)
    
    dst = paste0(outpath,state,'.csv')
    write.csv(output,dst)

    }

states <- state.name

lapply(states %>% head(3), collect_acs_data, geography = 'block group', 
       year = 2017,
       county = NULL,
       outpath = '')


write.csv(acs_table,"ACS Variables.csv")




#language <- get_acs(geography = 'tract', sumfile = 'acs5', year=2018,
#                    variables = c('B16001_001', 'B16001_002', 'B16001_003', 'B16001_004', 'B16001_005')) %>%
#  dplyr::select(-moe)%>%
 # spread(key = 'variable', value = 'estimate') %>%
 # mutate(
#    tot_population = B16001_001,
 #   only_english = B16001_002,
  #  spanish=B16001_003,
   # spanish_with_english=B16001_004,
    #spanish_no_english=B16001_005) %>%
#  mutate(
 #   only_english_pct = only_english/tot_population,
  #  spanish_pct=B16001_003/tot_population,
   # spanish_with_english_pct=B16001_004/tot_population,
    #spanish_no_english_pct=B16001_005/tot_population
#  ) %>%
 # dplyr::select(-starts_with("B1"))




language <- get_acs(geography = 'block group',
                    variables = c('B16001_001','B16001_002','B16001_003','B16001_004','B16001_005',
                                  'B16001_075','B16001_006'),
                    year = 2014, state = 'Virginia',county="Arlington County", geometry = FALSE) %>%
  dplyr::select(-moe) %>%
  spread(key = 'variable', value = 'estimate') %>% 
  mutate(
    tot_population_language=B16001_001,
    only_english_pct = B16001_002/tot_population_language,
    any_other_than_english_pct = 1-(B16001_002/tot_population_language),
    spanish_pct=B16001_003/tot_population_language,
    french_pct=B16001_006/tot_population_language,
    chinese_pct=B16001_075/tot_population_language,
    spanish_with_english_pct=B16001_004/tot_population_language,
    spanish_no_english_pct=B16001_005/tot_population_language) %>%
  dplyr::select(-c(NAME))


age <- get_acs(geography = 'block group',
               variables = c(sapply(seq(1,49,1), function(v) return(paste("B01001_",str_pad(v,3,pad ="0"),sep="")))),
               year = 2018, state = "Virginia",county = "Arlington County", geometry = FALSE)%>%
  dplyr::select(-moe) %>%
  spread(key = 'variable', value = 'estimate') %>% 
  mutate(
    denom = B01001_001,
    age_under30_ma = dplyr::select(., B01001_007:B01001_011) %>% rowSums(na.rm = TRUE),
    # age_25_64_ma = dplyr::select(., B01001_011:B01001_019) %>% rowSums(na.rm = TRUE),
    age_over65_ma = dplyr::select(., B01001_020:B01001_025) %>% rowSums(na.rm = TRUE),
    age_under30_fe = dplyr::select(., B01001_031:B01001_035) %>% rowSums(na.rm = TRUE),
    #age_25_64_fe = dplyr::select(., B01001_035:B01001_043) %>% rowSums(na.rm = TRUE),
    age_over65_fe = dplyr::select(., B01001_044:B01001_049) %>% rowSums(na.rm = TRUE),
    age_pct_under30 = (age_under30_ma + age_under30_fe)/denom,
    #age_pct_25_64 = (age_25_64_ma + age_25_64_fe)/denom,
    age_pct_over65 = (age_over65_ma + age_over65_fe)/denom
  ) %>%
  dplyr::select(-starts_with("B0"))%>%dplyr::select(-ends_with("_ma")) %>% dplyr::select(-ends_with("_fe")) %>% dplyr::select(-denom)


ACS <- get_acs(geography = 'block group',
                variables = c(sapply(seq(1,10,1), function(v) return(paste("B02001_",str_pad(v,3,pad ="0"),sep=""))),
                              'B03002_001','B03002_002','B03002_003','B03002_012','B03002_013','B02017_001',
                              'B19301_001', 'B17021_001', 'B17021_002',"B02001_005","B02001_004","B02001_006"),
               year = 2018, state = "Virginia",county = "Arlington County", geometry = TRUE)%>%
  dplyr::select(-moe) %>%
  spread(key = 'variable', value = 'estimate') %>% 
  mutate(
    tot_population_race = B02001_001,
    pop_nonwhite=B02001_001-B02001_002,
    pop_nonwhitenh=B03002_001-B03002_003,
    race_pct_white = B02001_002/B02001_001,
    race_pct_whitenh = B03002_003/B03002_001,
    race_pct_nonwhite = 1 - race_pct_white,
    race_pct_nonwhitenh = 1 - race_pct_whitenh,
    race_pct_black = B02001_003/B02001_001,
    race_pct_aapi = (B02001_005+B02001_006)/B02001_001,
    race_pct_native = B02001_004/B02001_001,
    race_pct_hisp = B03002_012/B03002_001) %>%
  mutate(
    tot_population_income = B17021_001,
    in_poverty = B17021_002) %>%
  mutate(
    inc_pct_poverty = in_poverty/tot_population_income,
    inc_percapita_income = B19301_001) %>%
  left_join(language, by="GEOID")%>%
  left_join(age, by="GEOID")%>%
  dplyr::select(-starts_with("B0"))%>%
  dplyr::select(-starts_with("B1"))


ACS$GEOID<-as.integer(ACS$GEOID)
turnout<-read.csv("countypres_2000-2016.csv")
electionanalysis<-read.csv("election-context-2018.csv")

FullData<-ACS%>%
  mutate()

topojson_write(FullData,file="TreeData.json")




nonwhite<-ggplot() +
  geom_sf(data = FullData, aes(fill=(race_pct_nonwhitenh)),color=NA,alpha=1) +
  scale_fill_gradient(low="white",high="#ffbb00",na.value="white",limits=c(min(FullData$race_pct_nonwhitenh, na.rm = TRUE), max(FullData$race_pct_nonwhitenh, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = FullData, color = '#ffbb00', fill = NA, lwd=.1)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="% Non-White")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  map_theme()+
  theme(legend.position = "right")





customRange2 = c(min(FullData$race_pct_nonwhitenh, na.rm = TRUE), max(FullData$race_pct_nonwhitenh, na.rm = TRUE)) # custom min / max values
customRange3 = c(min(FullData$only_english_pct, na.rm = TRUE), max(FullData$only_english_pct, na.rm = TRUE)) # custom min / max values
customRange4 = c(min(FullData$age_pct_under30, na.rm = TRUE), max(FullData$age_pct_under30, na.rm = TRUE)) # custom min / max values
customRange5 = c(min(FullData$inc_pct_poverty, na.rm = TRUE), max(FullData$inc_pct_poverty, na.rm = TRUE)) # custom min / max values

export_formattable <- function(f, file, width = "85%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay,
          zoom=8)
}

setwd("C:/Users/rcarder/Documents/dev/electionprotection/StateCharts")

#write.csv(savedtable,"savedtable.csv")

FullData<-FullData[!is.na(FullData$state),]


for (i in unique(FullData$state)){

statetable<-FullData%>%
  filter(state==i)%>%
  dplyr::select(24,3,5,9,17,22,62)%>%
  arrange(-pop_nonwhitenh)%>%
  mutate(TotalPop=comma(tot_population_race,0),
         NonWhite=comma(pop_nonwhitenh,0),
         NonwhitePercent=formattable::percent(race_pct_nonwhitenh,1),
         LanguagePercent=formattable::percent(1-only_english_pct,1),
         Under30=formattable::percent(age_pct_under30,1),
         Turnout2016=formattable::percent(turnout2016,1))%>%
  dplyr::select(-(2:8))

statetable<-as.data.frame(statetable[,1:7])
statetable<-as.data.frame(statetable[,1:7])
statetable<-head(statetable,25)

colors2      = csscolor(gradient(as.numeric(c(customRange2, statetable$NonwhitePercent)), "white", "#ffbb00"))
colors2      = colors2[-(1:2)] ## remove colors for min/max

colors3      = csscolor(gradient(as.numeric(c(customRange3, statetable$LanguagePercent)), "white", "#71ED54"))
colors3      = colors3[-(1:2)] ## remove colors for min/max

colors4      = csscolor(gradient(as.numeric(c(customRange4, statetable$Under30)), "white", "#0082BC"))
colors4      = colors4[-(1:2)] ## remove colors for min/max

colors5      = csscolor(gradient(as.numeric(c(customRange5, statetable$Turnout2016)), "white", "#FF4996"))
colors5      = colors5[-(1:2)] ## remove colors for min/max


fmt2    = formatter("span", 
                   style = function(x){
                     style(display            = "block",
                           padding            = "0 4px",
                           `border-radius`    = "4px",
                           `font-family`='Montserrat',
                           width             = "80px",
                           `background-color` = colors2
                     )})

fmt3    = formatter("span", 
                    style = function(x){
                      style(display            = "block",
                            padding            = "0 4px",
                            width             = "80px",
                            `font-family`='Montserrat',
                            `border-radius`    = "4px",
                            `background-color` = colors3
                      )})

fmt4    = formatter("span", 
                    style = function(x){
                      style(display            = "block",
                            padding            = "0 4px",
                            `font-family`='Montserrat',
                            width             = "80px",
                            `border-radius`    = "4px",
                            `background-color` = colors4
                      )})

fmt5    = formatter("span", 
                    style = function(x){
                      style(display            = "block",
                            padding            = "0 4px",
                            width             = "80px",
                            `font-family`='Montserrat',
                            `border-radius`    = "4px",
                            `background-color` = colors5
                      )})



colnames(statetable)=c("County","Populati","Non-White","% Non-Whi","% Non-Eng","% Age 18", "% 2016 Tu")

savedtable<-formattable(statetable, align =c("l","c","c","c","c", "c","c"), list(
  County = formatter("span", style = ~ style(color = "black",font.weight = "bold", `font-family`='Montserrat',width = "160px")),
  Populati = formatter("span", style = ~ style(`font-family`='Montserrat',width = "80px")),
  `Non-White` = formatter("span", style = ~ style(`font-family`='Montserrat',width = "80px")),
  `% Non-Whi`=fmt2,
  `% Non-Eng`=fmt3,
  `% Age 18`=fmt4,
  `% 2016 Tu`=fmt5))


export_formattable(savedtable,paste(i,"CountyTable.png",sep=''))


}


map_theme<- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      text = element_text(color = "#000000",family="Montserrat"),
      legend.title = element_text(size=6, family="Montserrat SemiBold"),
      legend.text = element_text(size=6),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}




for (i in unique(FullData$state)){

  setwd("C:/Users/rcarder/Documents/dev/electionprotection/StateCharts")
  
highlight_counties<-FullData%>%
  filter(state==i)%>%
  arrange(-pop_nonwhite)%>%
  head(25)

bama<-FullData%>%
  filter(state==i)

state<-states%>%
  filter(NAME==i)

bamacities<-cities %>%
  filter(State==i&Rank<=750)%>%
  arrange(Rank)%>%
  head(5)

SurveyCounties<-bama%>%
  mutate(FIPS=as.character(GEOID))%>%
  mutate(FIPS=ifelse(str_length(FIPS)==4,paste("0",FIPS,sep=''),FIPS))%>%
  left_join(Counties,by=c("FIPS"="FIPS"))%>%
  filter(!is.na(County))



statemap<-ggplot() +
  geom_sf(data = state, fill="white",color=NA) +
  map_theme()+
  theme(legend.position = "none")


nonwhite<-ggplot() +
  geom_sf(data = bama, aes(fill=(race_pct_nonwhitenh)),color=NA,alpha=1) +
  scale_fill_gradient(low="white",high="#ffbb00",na.value="white",limits=c(min(FullData$race_pct_nonwhitenh, na.rm = TRUE), max(FullData$race_pct_nonwhitenh, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = bama, color = '#ffbb00', fill = NA, lwd=.1)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="% Non-White")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  map_theme()+
  theme(legend.position = "right")

language<-ggplot() +
  geom_sf(data = bama, aes(fill=(1-only_english_pct)),color=NA,alpha=1) +
  scale_fill_gradient(low="white",high="#71ED54",na.value="white",limits=c(min(FullData$only_english_pct, na.rm = TRUE), max(FullData$only_english_pct, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = bama, color = '#71ED54', fill = NA, lwd=.1)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="% Non-English")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  map_theme()+
  theme(legend.position = "right")

under30<-ggplot() +
  geom_sf(data = bama, aes(fill=age_pct_under30),color=NA,alpha=1) +
  scale_fill_gradient(low="white",high="#0082BC",na.value="white",limits=c(min(FullData$age_pct_under30, na.rm = TRUE), max(FullData$age_pct_under30, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = bama, color = '#0082BC', fill = NA, lwd=.1)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="% Age 18-30")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  map_theme()+
  theme(legend.position = "right")

turnout<-ggplot() +
  geom_sf(data = bama, aes(fill=turnout2016),color=NA,alpha=1) +
  scale_fill_gradient(low="white",high="#FF4996",na.value="white",limits=c(min(FullData$turnout2016, na.rm = TRUE), max(FullData$turnout2016, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = bama, color = '#FF4996', fill = NA, lwd=.1)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="2016 Turnout")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  theme(legend.position = "right")+
  map_theme()


CountiesCovered<-ggplot() +
  geom_sf(data = bama, fill="White",color="#E2E3E4",alpha=1,lwd=.2) +
  #scale_fill_gradient(low="white",high="#FF4996",na.value="white",limits=c(min(FullData$turnout2016, na.rm = TRUE), max(FullData$turnout2016, na.rm = TRUE)),labels=scales::percent_format(accuracy=1))+
  geom_sf(data = SurveyCounties, color = '#E2E3E4', fill = "#cc464a", lwd=.2)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  labs(fill="2016 Turnout")+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="Montserrat",size=2)+
  theme(legend.position = "right")+
  map_theme()

ggsave(paste(i,"NonWhiteMap.pdf",sep=''), plot = nonwhite, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4)
embed_fonts(paste(i,"NonWhiteMap.pdf",sep=''), outfile=paste(i,"NonWhiteMap.pdf",sep=''))

ggsave(paste(i,"Language.pdf",sep=''), plot = language, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4)
embed_fonts(paste(i,"Language.pdf",sep=''), outfile=paste(i,"Language.pdf",sep=''))


ggsave(paste(i,"Under30.pdf",sep=''), plot = under30, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4)
embed_fonts(paste(i,"Under30.pdf",sep=''), outfile=paste(i,"Under30.pdf",sep=''))

ggsave(paste(i,"Turnout.pdf",sep=''), plot = turnout, device = NULL, path = NULL,
       scale = 1, width = 4, height = 4)
embed_fonts(paste(i,"Turnout.pdf",sep=''), outfile=paste(i,"Turnout.pdf",sep=''))

ggsave(paste(i,"State.pdf",sep=''), plot = statemap, device = NULL, path = NULL,
       scale = 1, width = 1.5, height = 1.5)
embed_fonts(paste(i,"State.pdf",sep=''), outfile=paste(i,"State.pdf",sep=''))

setwd("C:/Users/rcarder/Documents/dev/electionprotection/StateCharts/CountiesCovered")

ggsave(paste(i,"SurveyCounties.pdf",sep=''), plot = CountiesCovered, device = NULL, path = NULL,
       scale = 1, width = 2.5, height = 2.5)
embed_fonts(paste(i,"SurveyCounties.pdf",sep=''), outfile=paste(i,"SurveyCounties.pdf",sep=''))

}






ggplot() +
  geom_sf(data = bama, aes(fill=DemPlus),color=NA,alpha=.7) +
  scale_fill_gradient2(low="red",mid="white",high="blue",midpoint=0,labels=scales::percent_format(accuracy=1))+
  geom_sf(data = highlight_counties, color = '#606060', fill = NA)+
  geom_point(data=bamacities,aes(x=lon,y=lat))+
  geom_text_repel(data=bamacities,aes(x=lon,y=lat,label=City),family="mono")+
  theme_map()+
  theme(legend.position = "right")

?select


#######SurveyResults

setwd("C:/Users/rcarder/Documents/dev/electionprotection")
survey_results<-read_sheet("https://docs.google.com/spreadsheets/d/1cfCeHmCjMW9uc6u_sSagHdfASjIple2d3lZsU9BUfrc/edit#gid=385449943")
survey_results2<-read_sheet("https://docs.google.com/spreadsheets/d/1witV5GWdof-C5xikgjj6BYP4LrQQDq4YGZFh2UHoryI/edit#gid=166937486")


survey_resultsMaster<-bind_rows(survey_results,survey_results2)

counties<-read.csv("counties.csv")
stateIntros<-read.csv("StateIntros.csv", stringsAsFactors = FALSE)
StateAbbrs<-read.csv("StateAbbrs.csv", stringsAsFactors = FALSE)

Nationals<-survey_results2%>%
  dplyr::select(3,6)%>%
  dplyr::rename("State"=2)%>%
  separate(State,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","aa","bb","cc","dd","ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo","pp","qq","aaa","bbb","ccc","ddd","eee","fff","ggg","hhh","lll","ooo","ppp","qqq","rrr","sss"), ", ")%>%
  pivot_longer(2:58)%>%
  filter(!is.na(value))%>%
  dplyr::select(-2)%>%
  dplyr::rename("State"=2)






Base<-survey_results%>%
  dplyr::select(3,6)%>%
  dplyr::rename("State"="What state are you filling out this survey for?")

Base<-bind_rows(Base,Nationals)

CommsStrategies<-survey_resultsMaster%>%
  dplyr::select(3,24)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  filter(Election=="General")%>%
  dplyr::select(-3)%>%
  dplyr::rename("Comms"=3)%>%
  left_join(Base)

CommsStrategiesGeneral<-survey_resultsMaster%>%
  dplyr::select(3,24)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  dplyr::select(-3)%>%
  dplyr::rename("Comms"=3)%>%
  filter(Election=="General")%>%
  left_join(Base)

CommsStrategiesPrimary<-survey_resultsMaster%>%
  dplyr::select(3,24)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  dplyr::select(-3)%>%
  dplyr::rename("Comms"=3)%>%
  filter(Election=="Presidential Primary")%>%
  left_join(Base)

Activities<-survey_resultsMaster%>%
  dplyr::select(3,22)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  filter(Election=="General")%>%
  dplyr::select(-3)%>%
  dplyr::rename("Activity"=3)%>%
  left_join(Base)


CurrentGaps<-survey_resultsMaster%>%
  dplyr::select(3,25)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=ifelse(is.na(Activity),"NA",Activity))%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  dplyr::rename("Gap"=2)%>%
  filter(!is.na(Gap))%>%
  left_join(Base)


CountyJoin<-st_drop_geometry(FullData)

CountyJoin<-CountyJoin%>%
  left_join(StateAbbrs, by=c("state"="State"))%>%
  mutate(JoinField=paste(Abbr,county,sep=''))%>%
  full_join(counties, by=c("JoinField"="Join"))

FIPS<-CountyJoin%>%
  dplyr::select(1,66)%>%
  mutate(FIPS=as.character(GEOID))%>%
  mutate(FIPS=ifelse(str_length(FIPS)==4,paste("0",FIPS,sep=''),FIPS))

Master<-Base%>%
  left_join(CommsStrategies,by="Organization")%>%
  left_join(Activities, by="Organization")%>%
  dplyr::select(-3,-5,-6,-8)%>%
  left_join(CurrentGaps,by="Organization")%>%
  left_join(counties, by="Organization")%>%
 dplyr::select(-3,-5,-6,-8,-10,-11,-12,-13,-14)%>%
  left_join(stateIntros, by=c("State.x"="StateAbbr"))%>%
  mutate(Join=paste(State.x,County,sep=''))%>%
  left_join(FIPS, by=c("Join"="JoinField"))%>%
  left_join(StateAbbrs,by=c("State.x"="Abbr"))%>%
            dplyr::rename(StateName=State)%>%
            dplyr::select(-10)



newOrgs<-survey_results%>%
  anti_join(counties,by=c(Organization="Organization"))
newOrgs<-as.data.frame(newOrgs)%>%dplyr::select(-5)


  
  
write.csv(Master, "MasterEP.csv",row.names = FALSE)  
write.csv(CommsStrategies, "CommsStrategies.csv",row.names = FALSE)  
write.csv(Activities, "Activities.csv",row.names = FALSE)  
write.csv(CurrentGaps, "CurrentGaps.csv",row.names = FALSE)  


write.csv(CountyJoin, "countiesJoined.csv",row.names = FALSE)  

write.csv(newOrgs, "MissingCounties2.csv",row.names = FALSE)  


##Memory Saving Approach
setwd("C:/Users/rcarder/Documents/dev/electionprotection")
survey_results<-read_sheet("https://docs.google.com/spreadsheets/d/1cfCeHmCjMW9uc6u_sSagHdfASjIple2d3lZsU9BUfrc/edit#gid=385449943")
survey_results2<-read_sheet("https://docs.google.com/spreadsheets/d/1witV5GWdof-C5xikgjj6BYP4LrQQDq4YGZFh2UHoryI/edit#gid=166937486")


survey_resultsMaster<-bind_rows(survey_results,survey_results2)

counties<-read.csv("counties.csv")
stateIntros<-read.csv("StateIntros.csv", stringsAsFactors = FALSE)
StateAbbrs<-read.csv("StateAbbrs.csv", stringsAsFactors = FALSE)
BudgetGaps<-read.csv("BudgetGaps.csv", stringsAsFactors = FALSE)

Nationals<-survey_results2%>%
  dplyr::select(3,6)%>%
  dplyr::rename("State"=2)%>%
  separate(State,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","aa","bb","cc","dd","ee","ff","gg","hh","ii","jj","kk","ll","mm","nn","oo","pp","qq","aaa","bbb","ccc","ddd","eee","fff","ggg","hhh","lll","ooo","ppp","qqq","rrr","sss"), ", ")%>%
  pivot_longer(2:58)%>%
  filter(!is.na(value))%>%
  dplyr::select(-2)%>%
  dplyr::rename("State"=2)

Base<-survey_results%>%
  dplyr::select(3,6)%>%
  dplyr::rename("State"="What state are you filling out this survey for?")

Base<-bind_rows(Base,Nationals)%>%
  mutate(NationalState=ifelse(str_detect(Organization,"- National"),"National","State"))

MasterMemory<-Base%>%
  left_join(stateIntros, by=c("State"="StateAbbr"))%>%
  left_join(StateAbbrs,by=c("State"="Abbr"))%>%
  dplyr::rename(StateName=State.y)

counties$FIPS<-as.character(counties$FIPS)

Counties<-counties%>%
  group_by(Organization,County,FIPS)%>%
  summarize(remove=first(FIPS))%>%
  mutate(FIPS=ifelse(str_length(FIPS)==4,paste("0",FIPS,sep=''),FIPS))%>%
  dplyr::select(-4)

CommsStrategies<-survey_resultsMaster%>%
  dplyr::select(3,24)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  filter(Election=="General")%>%
  dplyr::select(-3)%>%
  dplyr::rename("Comms"=3)%>%
  left_join(Base)

Activities<-survey_resultsMaster%>%
  dplyr::select(3,22)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  separate(value,c("Activity","Election"), " - ")%>%
  pivot_longer(2)%>%
  filter(!is.na(Election))%>%
  filter(Election=="General")%>%
  dplyr::select(-3)%>%
  dplyr::rename("Activity"=3)%>%
  left_join(Base)


CurrentGaps<-survey_resultsMaster%>%
  dplyr::select(3,25)%>%
  dplyr::rename("Activity"=2)%>%
  mutate(Activity=ifelse(is.na(Activity),"NA",Activity))%>%
  mutate(Activity=str_replace_all(Activity,"((TV, radio, etc.))",""))%>%
  separate(Activity,c("A","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s"), ", ")%>%
  pivot_longer(2:20)%>%
  dplyr::select(-2)%>%
  dplyr::rename("Gap"=2)%>%
  filter(!is.na(Gap))%>%
  left_join(Base)

InteractiveDems<-st_drop_geometry(FullData)%>%
  mutate(FIPS=as.character(GEOID))%>%
  mutate(FIPS=ifelse(str_length(FIPS)==4,paste("0",FIPS,sep=''),FIPS))


BudgetGaps<-BudgetGaps%>%
  filter(!is.na(BudgetGap))

setwd("C:/Users/rcarder/Documents/dev/electionprotection")
write.csv(MasterMemory, "MasterMemory.csv",row.names = FALSE)  
write.csv(CurrentGaps, "Gaps.csv",row.names = FALSE)  
write.csv(Activities, "Activities.csv",row.names = FALSE)  
#write.csv(Counties, "Counties.csv",row.names = FALSE)  
write.csv(CommsStrategies, "Comms.csv",row.names = FALSE)  
write.csv(BudgetGaps, "Budgets.csv",row.names = FALSE) 
#write.csv(InteractiveDems, "InteractiveDems.csv",row.names = FALSE)  
#write.csv(BudgetTemplate, "BudgetTemplate.csv",row.names = FALSE)  

CoveredCounties


  
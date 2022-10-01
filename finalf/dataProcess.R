library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(alphavantager)
library(shinyWidgets)
library(shinythemes)
library(rvest)
library(forcats)
library(ggmap)
library(maps)

cwur <- read_csv("https://raw.githubusercontent.com/data-science-stat-220-s21/final-project-nina-ruofei/main/cwurData.csv?token=ASUROBIIAVQCCUGV3MPC3BLAY2X6M",
                 col_types = cols(.default = col_integer(),
                                  institution = col_character(),
                                  country = col_character(),
                                  score = col_double()))
sh <- read_csv("https://raw.githubusercontent.com/data-science-stat-220-s21/final-project-nina-ruofei/main/shanghaiData.csv?token=ASUROBOB3NXN6MLHBL6TCJLAY2HEM",
               col_types = cols(year = col_integer()))
tm <- read_csv("https://raw.githubusercontent.com/data-science-stat-220-s21/final-project-nina-ruofei/main/timesData.csv?token=ASUROBPNR6DTCIRUIGQNK73AY5EII",
               col_types = cols(year = col_integer()))
exp <-  read_csv("https://raw.githubusercontent.com/data-science-stat-220-s21/final-project-nina-ruofei/main/education_expenditure_supplementary_data.csv?token=ASUROBOOCQSSN4VDOWP66ITAYSBLK",
                 col_types = cols(.default = col_double(),
                                  country = col_character()))
supp <- read_csv("https://raw.githubusercontent.com/data-science-stat-220-s21/final-project-nina-ruofei/main/education_expenditure_supplementary_data.csv?token=ASUROBIYO44WTQCDKPNRLQDAY5JZO",
                 col_types = cols(country = col_character(),
                                  institute_type = col_character(),
                                  direct_expenditure_type = col_character()))

tm <- tm %>%
  filter(year %in% 2012:2015) %>%
  group_by(country, year) %>%
  mutate(national_rank = seq(1:n()))
sh <- sh %>%
  filter(year %in% 2012:2015)
sh$university_name <- str_replace_all(sh$university_name, pattern = "\\s\\(.*\\)$", replacement = "") # Delete Acronyms
sh$university_name <- str_replace_all(sh$university_name, pattern = "^The\\s", replacement = "") # Delete prefixes
sh$national_rank <- str_replace_all(sh$national_rank, pattern = "(\\.0)$", replacement = "") # Delete suffixes

cwur2014 <- cwur %>% filter(year == 2014) %>%
  group_by(country) %>%
  summarize(numU = n()) %>%
  mutate(class = ifelse(numU >= 10, yes = ">=10", no = "<10"))
table(cwur2014$class)
exp <- exp %>% select(country, institute_type, direct_expenditure_type, `2011`) %>%
  filter(institute_type == "Higher Education Institutions")
j <- left_join(cwur2014, exp)

rk <- full_join(cwur, tm, by = c("institution" = "university_name", "year"))  %>%
  mutate(country = ifelse(is.na(country.y), yes = country.x, no = country.y)) %>%
  select(institution, country, cwur_w = world_rank.x, cwur_n = national_rank.x,  tm_w = world_rank.y,  tm_n = national_rank.y, year, cwur_s = score, tm_s = total_score)

# View( rk %>% arrange(institution))
# e.g. cwur_w means cwur world ranking. cwur_n means cwur national ranking.
rk2 <- full_join(rk, sh, by = c("institution" = "university_name", "year")) %>%
  select(institution, country, cwur_w, cwur_n, tm_w, tm_n, sh_w = world_rank, sh_n = national_rank, year,cwur_s, tm_s, sh_s = total_score)
rk2$tm_s <- parse_number(rk2$tm_s, na = c("","NA","-"))
tm$international <- parse_number(tm$international, na = c("","NA","-"))
tm$income <- parse_number(tm$income, na = c("","NA","-"))
instName <- tolower(rk2$institution)
tmName <- tolower(tm$university_name)
shName <- tolower(sh$university_name)
cwurName <- tolower(cwur$institution)

supp_public <- supp %>%
  filter(direct_expenditure_type == "Public")
#supplement data distinct countries
supplement_coun <- supp_public %>%
  select(country) %>%
  distinct() %>%
  arrange(country)
#supplement data tidying for public expenditure (year variation)
supp_longer<- supp_public %>%
  pivot_longer(
    cols = c('1995', '2000', '2005', '2009', '2010', '2011'),
    names_to = "year",
    values_to = "expenditure",
    names_transform = list(year = as.integer)
  ) 
#supplement data tidying for 2011 public, private, total relation
supp_2011<-supp %>%
  select(country, institute_type, direct_expenditure_type, "2011") %>%
  pivot_wider(names_from = direct_expenditure_type,
              values_from = "2011") %>%
  drop_na()

sh_uni <- sh %>%
  select(university_name) %>%
  distinct() %>%
  drop_na()

sh_longer<- sh%>%
  pivot_longer(
    cols = c('total_score', 'alumni', 'award', 'hici', 'ns', 'pub', 'pcp'),
    names_to = "score_types",
    values_to = "scores"
  )

#Times Ranking
tm$international<- tm$international%>%
  as.numeric()
tm$income<- tm$income%>%
  as.numeric()
tm$total_score<- tm$total_score%>%
  as.numeric()


tm_2 <- tm %>%
  select(international, international_students, year)
tm_2$international_students <- str_replace_all(tm_2$international_students, pattern = "\\%", replacement = "") %>%
  as.integer()
tm_2$international<-tm$international%>%
  as.numeric()

tm_uni <- tm %>%
  select(university_name) %>%
  distinct() %>%
  ungroup() %>%
  select(university_name)


tm_longer <- tm%>%
  pivot_longer(
    cols = c('teaching', 'international', 'research', 'citations', 'income', 'total_score'),
    names_to = "score_types",
    values_to = "scores"
  )

#cwur 
cwur_uni <- cwur %>%
  select(institution) %>%
  distinct()

isInst <- function(n) { # if in the list, return the index; otherwise NA
  match(tolower(n),instName)
}

getRightName <- function(n) {
  (rk2 %>% slice(isInst(n)))[[1]]
}

getInstURL <- function(n) {
  inst <- getRightName(n)
  url <- paste("http://en.wiki.sxisa.org/wiki/",str_replace_all(inst, pattern = "\\s", replacement = "_"), sep = "")
  url
}

getInstImage <- function(n, p) {
  if(!is.na(isInst(n))){
    url <- getInstURL(n)
    # WARNING: replace with wikipedia!
    tryCatch(
      expr = {
        page <- read_html(url)
        images.html <- html_nodes(page, "img")
        icon <- paste("http:",html_attr(images.html[[p]],"src"), sep = "")
        tags$img(src = icon)
      },
      error = function(e) {
        HTML("Sorry, but there are no images available.")
      }
    )
  }
  
}

makeTable <- function(n, r) {
  t <- rk2 %>% filter(institution == n) %>%
    select(year, ends_with(r)) %>%
    arrange(year)
  colnames(t)[2:4] <- c("CWUR","Times","Shanghai")
  t$CWUR[is.na(t$CWUR)] <- "-"
  t$Times[is.na(t$Times)] <- "-"
  t$Shanghai[is.na(t$Shanghai)] <- "-"
  t
}
instnc <- function(n) {
  if(!is.na(isInst(n))) {
    case <- rk2 %>%
      slice(isInst(n))
    line1 <- paste("Institution Name: <b>", case[[1]], "</b>;")
    line2 <- paste("Country (Region) <b>: ", case[[2]], "</b>")
    HTML(paste(line1, line2, sep = "<br/>"))
  } else {
    HTML("Sorry, the university is not found in our ranking data. Please check your spellings or try another name.")
  }
}

findHigh <- function(v){ #"50-100" "100-200" "50-100"
  comp <- as.integer(str_replace_all(v, "\\-.*","")) #"50" "100" "50" 
  min_comp <- min(comp) #50
  if (min_comp %in% c(-Inf,Inf)){
    c("0",0)
  } else{
    i <- as.integer(match(min_comp, comp)) # "1"
    c(v[i],i)
  }
}

getHighRank <- function(r) {
  ifelse(r == "0", yes = NA, no = r)
}
getHighRankYear <- function(p) {
  ifelse(as.integer(p) == 0, yes = NA, no = (tmn%>% slice(as.integer(p)))[[1]])
}
getHighScore <- function(d) {
  ifelse(max(d$score) %in% c(Inf, "-", -Inf), yes = NA, no = max(d$score))
}
getHighScoreYear <- function (d){
  ifelse(max(d$score) %in% c(Inf, "-", -Inf), yes = NA, no = (d %>% slice(match(max(d$score), d$score)))[[1]])
}

map.world <- map_data("world")
map_world<- map.world$region[map.world$subregion == "Hong Kong"] <- "Hong Kong"

write.csv(cwur_uni, file = "cwur_uni.csv")
write.csv(tm_uni, file = "tm_uni.csv")
write.csv(sh_uni, file = "sh_uni.csv")
write.csv(supplement_coun, file = "supplement_coun.csv")
write.csv(supp_2011, file = "supp_2011.csv")
write.csv(map.world, file = "map_world.csv")
write.csv(tm, file = "tm.csv")
write.csv(sh, file = "sh.csv")
write.csv(cwur2014, file = "cwur2014.csv")
write.csv(j, file = "j.csv")
write.csv(rk2, file = "rk2.csv")
write.csv(sh_longer, file = "sh_longer.csv")
write.csv(supp, file = "supp.csv")
write.csv(supp_2011, file = "supp_2011.csv")
write.csv(supplement_coun, file = "supplement_coun.csv")
write.csv(tm2, file = "tm2.csv")
write.csv(tm_longer, file = "tm_longer.csv")
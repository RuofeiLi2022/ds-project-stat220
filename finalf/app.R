
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
library(caret)
library(e1071)



exp <-  read_csv("expenditure2.csv",
                 col_types = cols(.default = col_double(),
                                  country = col_character()))
cwur <- read_csv("cwur.csv")
sh <- read_csv("sh.csv")
tm <- read_csv("tm.csv")
supp <- read_csv("supp.csv")
#https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS
# cwur <- read_csv("D:\\Awaxx\\STAT220\\final\\cwurData.csv",
#                  col_types = cols(.default = col_integer(),
#                                   institution = col_character(),
#                                   country = col_character(),
#                                   score = col_double()))
# sh <- read_csv("D:\\Awaxx\\STAT220\\final\\shanghaiData.csv",
#                col_types = cols(year = col_integer()))
# tm <- read_csv("D:\\Awaxx\\STAT220\\final\\timesData.csv",
#                col_types = cols(year = col_integer()))
# exp <-  read_csv("D:\\Awaxx\\STAT220\\final\\expenditure2.csv",
#                  col_types = cols(.default = col_double(),
#                                   country = col_character()))
# #https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS unused

# tm <- tm %>%
#   filter(year %in% 2012:2015) %>%
#   group_by(country, year) %>%
#   mutate(national_rank = seq(1:n()))
# sh <- sh %>%
#   filter(year %in% 2012:2015)
# sh$university_name <- str_replace_all(sh$university_name, pattern = "\\s\\(.*\\)$", replacement = "") # Delete Acronyms
# sh$university_name <- str_replace_all(sh$university_name, pattern = "^The\\s", replacement = "") # Delete prefixes
# sh$national_rank <- str_replace_all(sh$national_rank, pattern = "(\\.0)$", replacement = "") # Delete suffixes



cwur2014 <- cwur %>% filter(year == 2014) %>%
  group_by(country) %>%
  summarize(numU = n()) %>%
  mutate(class = ifelse(numU >= 10, yes = ">=10", no = "<10"))
cwur2014$class <- parse_factor(cwur2014$class) %>%
  fct_relevel(">=10")
# table(cwur2014$class)
cwur2014$country[match("USA",cwur2014$country)] <- "United States"
cwur2014$country[match("Hong Kong",cwur2014$country)] <- "Hong Kong SAR, China"
exp$country[match("Korea, Dem. People's Rep.",exp$country)] <- "South Korea"
exp$country[match("Russian Federation",exp$country)] <- "Russia"
exp$country[match("Egypt, Arab Rep.",exp$country)] <- "Egypt"
exp$country[match("Iran, Islamic Rep.",exp$country)] <- "Iran"
#  exp <- exp %>%
# #   filter(institute_type == "Higher Education Institutions",
# #          direct_expenditure_type == "Total") %>%
#    select(country, `2012`,`2013`)

# exp <- exp %>%
#   filter_all(any_vars(!is.na(.)))
exp <- exp[rowSums(is.na(exp)) != ncol(exp) -1, ]
expf <- exp %>% select(-country)
ind <- !is.na(expf)
expfn <- tapply(expf[ind],row(expf)[ind],tail, 1)
exp <- exp %>% select(country) %>% mutate(expenditure = NA)
exp$expenditure <- expfn

j <- left_join(cwur2014,exp) %>%
  drop_na()
#   slice(-53)  %>% # NO Taiwan Data
#   slice(-56) # NO Arab Data
# No South Korea Data



# Make Train Test Data
set.seed(20210607)
n <- nrow(j)
train_index <- sample(n, size = round(.8*n))
j_train <- j %>%
  slice(train_index)
j_test <- j %>%
  slice(-train_index)
k_df <- data.frame(k = seq(1,8, by = 1))

train_control <- trainControl(
  method = "cv",
  number = 20
)
j_knn_cv <- train(
  class ~ expenditure,
  data = j_train,
  method = "knn",
  preProc = c("center","scale"),
  tuneGrid = k_df,
  trControl = train_control
)

knnPlot <- ggplot(j_knn_cv) +
  scale_x_continuous(breaks = k_df$k) +
  labs(title = "Knn - Performances on different k values")

# Prediction

j_test <- j_test %>%
  mutate(k_prediction = predict(j_knn_cv, newdata = j_test))

test_conf_mat_k <- confusionMatrix(
  data = j_test$k_prediction,
  reference = j_test$class,
  positive = ">=10"
)

standard_fun <- function(x) { (x - mean(x, na.rm=TRUE))/sd(x, na.rm = TRUE)}

j_pred <- j %>%
  select(numU, expenditure) %>%
  mutate_all(standard_fun)

km_out_3 <- kmeans(
  j_pred,
  centers = 3,
  nstart = 20
)

j_clus <- j %>%
  mutate(km_clus_3 = str_c("cluster_", km_out_3$cluster))

# test_conf_mat_r <- confusionMatrix(
#   data = j_test$t_prediction,
#   reference = j_test$class,
#   positive = ">=10"
# )

clusPlot <- ggplot(j_clus, aes(x = numU, y = expenditure)) +
  geom_point(aes(color = km_clus_3)) +
  labs(x = "# Universities in ranking", y = "Expenditure (%GDP)", title = "Clustering Plot with K = 3")



rk <- full_join(cwur, tm, by = c("institution" = "university_name", "year"))  %>%
  mutate(country = ifelse(is.na(country.y), yes = country.x, no = country.y)) %>%
  select(institution, country, cwur_w = world_rank.x, cwur_n = national_rank.x,  tm_w = world_rank.y,  tm_n = national_rank.y, year, cwur_s = score, tm_s = total_score)

# View( rk %>% arrange(institution))
# e.g. cwur_w means cwur world ranking. cwur_n means cwur national ranking.
rk2 <- full_join(rk, sh, by = c("institution" = "university_name", "year")) %>%
  select(institution, country, cwur_w, cwur_n, tm_w, tm_n, sh_w = world_rank, sh_n = national_rank, year,cwur_s, tm_s, sh_s = total_score)

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

cwur_uni <- cwur %>%
  select(institution) %>%
  distinct()


ui <- navbarPage("University Rankings",
                 #  navbarMenu("Explore",
                 tabPanel("Introduction",
                          img(),
                          p("In this ayalysis, we focused on three different World University Rankings, along with the direct expenditures on education across nations. The three different ranking data sets are the Center for World University Rankings (written as CWUR in this app), the Times Higher Education World University Ranking (written as Times in this app), and the Academic Ranking of World Universities(as know as Shanghai ranking, so it will be noted as Shanghai in this app). In this app, we not only investigated the questions that we were interested in, but also provide some useful ways for the users to learn about the Top Univesrities around the world."),
                          p("For each of the data sets, we first looked at the number of Top universities across countries in a world map, and how the numbers varied throughout the years. We then investigated different types of scores and total scores for each universities in each rankings. We were also curious about the Times data, which included some information about the universities. Therefore, we used Times data to look at if some scoring criteria, such as international scores and teaching scores, are related to the basic information of a college, such as student and faculty ratio. We also looked the expenditure of education of across countries, and the relation between different types of expenditures(whether they are public or private)."),
                          p("If you want to learn more about the Top universities, the search tab does it's job. You can type in a university name and find information about the university and its ranking. You can also search the rank number of a ranking, and find the universities ranked at this number in each year. "
                            
                          ),p("In the prediction tab, you can see us building a model on expenditures of countries to predict whether they have over 10 universities in the 2014 CWUR ranking data. We have also clustered the data to provide a better glimpse on possible relations of  the number of universities in ranking for a country to their educational spendings.")
                          
                          
                 ), tabPanel("About Data",
                             p("Our data source come from Kaggle, each ranking included the world's top universities from 2012 to 2015. The Times ranking was founed in UK in 2010. From the Times Official website, they judge from the following aspects: 'teaching (the learning environment); research (volume, income and reputation), international outlook (staff, students and research); citations (research influence); industry income (knowledge transfer)'. "),
                             p("The Shanghai ranking was founded in China in 2003. According to the official website, the Shanghai Ranking 'uses six objective indicators to rank world universities, including the number of alumni and staff winning Nobel Prizes and Fields Medals, number of highly cited researchers selected by Clarivate Analytics, number of articles published in journals of Nature and Science, number of articles indexed in Science Citation Index - Expanded and Social Sciences Citation Index, and per capita performance of a university'. "),
                             p("The CWUR ranking comes from Saudi Arabia and was founded in 2012. According to the website, they use the following indicators: 'Quality of Education (the number of a university's alumni who have won major academic distinctions relative to the university's size), Alumni Employment (the number of a university's alumni who have held top executive positions at the world's largest companies relative to the university's size), Quality of Faculty(the number of faculty members who have won major academic distinctions), Research Output(the total number of research papers), High-Quality Publications(the number of research papers appearing in top-tier journals), Influence (the number of research papers appearing in highly-influential journals), Citations(the number of highly-cited research papers)' "),
                             p("Different ranking included different information about the universities. For exapmple, Times and CWUR including the countries that the top universities belong to, whereas Shanghai didn't include the country data. CWUR only scored the total score in 100 scale. The rest catagories in CWUR are in rank numbers. However, Shanghai and CWUR rankings all score their indicators in 100 point scale. Sometimes the naming of the universities are also different, and this might affect the searching results a little."),
                             p("The expenditure data comes from the National Center for Education Statistics according to Kaggle. It not only includes the expenditure on education by countries over the year, but also provides information about public and private direct expenditure. The expenditure in this data means the percentage of the gross domestic product of that country."),
                             p("reference: https://www.timeshighereducation.com/world-university-rankings, http://www.shanghairanking.com/rankings, https://cwur.org/, https://www.kaggle.com/mylesoneill/world-university-rankings")
                             
                 ),
                 tabPanel("Map",
                          h1("Where are the Top Universities?"),
                          
                          
                          awesomeRadio(inputId = "RankType",
                                       label = "Select a Unviersity Ranking",
                                       choices = c("CWUR","Times"),
                                       selected = "CWUR"
                          ),
                          
                          sliderInput(inputId = "year",
                                      label = "Select a year",
                                      min = 2012,
                                      max = 2015,
                                      value = 2012,
                                      step = 1),
                          p("This is a world map showing the number of Top universities for each country. You can find that the US always has exceed number of top universities. The number of European and Asian countries increases rapidly. As time goes by, the rankings also added more countries to their rankings lists. Shanghai ranking didn't have the country information inclueded, so we were not able to produce a map for Shanghai Ranking."),
                          
                          plotOutput("mapPlot"),
                          dataTableOutput("maptable")
                          
                          
                 ),
                 tabPanel("Prediction",
                          tabsetPanel(
                            tabPanel("K-Nearest",
                                     plotOutput("knnPlot"),
                                     h5("KNN Model Confusion Matrix:"),
                                     tableOutput("kmodel"),
                                     htmlOutput("explknn")
                            ),
                            tabPanel("Clustering",
                                     plotOutput("treePlot"),
                                     h5("Decision Tree Model Confusion Matrix:"),
                                     htmlOutput("tmodel"),
                                     htmlOutput("expltree")
                            )
                            
                          ) # end tabset Panel
                 ),
                 navbarMenu(
                   "Search",
                   tabPanel("By University Name",
                            sidebarLayout(
                              sidebarPanel(
                                searchInput(
                                  inputId = "name",
                                  label = "Searching (click the icon or press 'Enter' to update): ", 
                                  value = "University of Minnesota, Twin Cities",
                                  btnSearch = icon("search"), 
                                  btnReset = icon("remove"),
                                  width = "100%"
                                ),
                                # awesomeRadio(
                                #   inputId = "by",
                                #   label = "Category:",
                                #   choices = c("School Name","Country"),
                                #   selected = "School Name",
                                #   inline = TRUE
                                # ),
                                strong("Cases ignored but names should be exact in terms of not only letters but also punctuations and spaces."),
                                tags$p("========Introduction========"),
                                htmlOutput("intro")
                              ),
                              mainPanel(tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         htmlOutput("instnc"), # Institution name : xxx, Country: XXX
                                         fluidRow(
                                           column(
                                             textOutput("wr"),
                                             tableOutput("wranking"),
                                             width = 3
                                           ),
                                           column(width = 3),
                                           column(
                                             textOutput("nr"),
                                             tableOutput("nranking"),
                                             width = 3
                                           )
                                         ),
                                         
                                         # world ranking
                                         # if university, the output will be
                                         # |     | times| shanghai| cwur|
                                         # |2012 | 1    | 1       | 3   |
                                         # |.... | ..   | ..      | 3   |
                                         # |2015 | 2    | 4       | 3   |
                                         # national ranking
                                         #h5("Some Images about the School (would like a few moment to load):"),
                                         h5(htmlOutput("imgHint")),
                                         h3("         "),# placeholder
                                         fluidRow(
                                           column(3, uiOutput("img1")),
                                           column(3),
                                           column(3, uiOutput("img2")),
                                         ),
                                         fluidRow(
                                           column(3, uiOutput("img3")),
                                           column(3),
                                           column(3, uiOutput("img4"))
                                         ),
                                         #scrape some images from wikipedia
                                         htmlOutput("wikilink")
                                ),
                                tabPanel("Visualization",
                                         htmlOutput("instnc2"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             awesomeRadio(inputId = "scoretype",
                                                          label = "Plot Types:",
                                                          choices = c("Overview","Times","Shanghai"),
                                                          selected = "Overview")
                                           ),
                                           mainPanel(htmlOutput("scoreplot"))
                                         )
                                ),
                                tabPanel("Statistics",
                                         htmlOutput("instnc3"),
                                         h5("Times Scores:"),
                                         tableOutput("timesscores"),
                                         h5("Shanghai Scores:"),
                                         tableOutput("shanghaiscores"),
                                         h5("CWUR Scores:"),
                                         tableOutput("cwurscores"))
                                # max score, min score, etc.
                              ) # end tabset Panel
                              ) # end main Panel
                            ) # end Layout
                   ), # end tab Panel
                   tabPanel(
                     "By Rank Number",
                     p("This search feature allow you to find a certain ranking number of the world's Top 100 universities. You can type in a number form 1 to 100 and search for the colleges at this world rank on a certain ranking through out the years. Some blanks might be missing in the dataset because the original data is missing."),
                     numericInput(inputId = "rank_num",
                                  label = "Search the World's Top 100 Colleges by Rank Number(only input 1-100)",
                                  value = 1),
                     awesomeRadio(inputId = "RankType_num",
                                  label = "Select a Unviersity Ranking",
                                  choices = c("CWUR","Shanghai","Times"),
                                  selected = "CWUR"
                     ),
                     dataTableOutput("search_rank")
                   )),
                 navbarMenu(
                   "Ranking Score",
                   tabPanel(
                     "CWUR",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "uni_cwur",
                                     label = "Select a University in the rankings",
                                     choices = cwur_uni,
                                     selected = "Harvard University"
                         ),
                         p("CWUR data set has only total score as 100 piont scale. In order to better compare between rankings, we only choose the total for comparison. You can select a university and find the total score of that university across years."),
                       ),
                       mainPanel(
                         plotOutput("uniPlot_cwur")
                       )
                     )
                   ),
                   tabPanel(
                     "Times",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "uni_tm",
                                     label = "Select a University in the rankings",
                                     choices = tm_uni,
                                     selected = "California Institute of Technology
"),
                         p("citations: research influence"),
                         p("income: industry income after the students graduate"),
                         p("international: international outlook, including staff, students and research"),
                         p("research: volume, income and reputation"),
                         p("teaching: the learning environment")
                       ),
                       mainPanel(
                         plotOutput("uniPlot_tm")
                       )
                     )
                   ),
                   tabPanel(
                     "Shanghai",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "uni",
                                     label = "Select a University in the rankings",
                                     choices = sh_uni,
                                     selected = "Stanford University"),
                         p("alumni: the number of alumni and staff winning Nobel Prizes and Fields Medals"),
                         p("award: performance on winning Nobel Prizes and Fields Medals"),
                         p("hici : number of highly cited researchers selected by Clarivate Analytics"),
                         p("ns: number of articles published in journals of Nature and Science "),
                         p("pcp: per capita performance of a university"),
                         p("pub: number of articles indexed in Science Citation Index - Expanded and Social Sciences Citation Index ")
                       ),
                       mainPanel(
                         plotOutput("uniPlot_sh")
                       )
                     )
                 )
                 ),
                 navbarMenu(
                   "Relation",
                   
                   tabPanel(
                     "Teaching",
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput(inputId = "year_tm",
                                     label = "Select a year",
                                     min = 2012,
                                     max = 2015,
                                     value = 2012,
                                     step = 1),
                         p("We were curious to see if what Times claimed about the learning enivorment is related to the student and faculty ratio, so we made a scatter plot with a linear regession line to find out. And we found out there is a relationship between the student faculty ratio and teaching score. Generally, the lower the ratio is, the higher the teaching score is.")
                       ),
                       mainPanel(
                         plotOutput("teaching_tm")
                       )
                     )
                   ),
                   tabPanel(
                     "International",
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput(inputId = "year_tm_2",
                                     label = "Select a year",
                                     min = 2012,
                                     max = 2015,
                                     value = 2012,
                                     step = 1),
                         p("We were also curious about the relation between the international outlook score and the international student percentage in a college. The score fits well with the percentage of international students. The various student composition will result in a higher score in the international outlook score.")
                       ),
                       mainPanel(
                         plotOutput("international_tm")
                       )
                     )
                   )
                   
                 ),
                 
                 navbarMenu(
                   "Expenditure",
                   tabPanel(
                     "Year", #public over years
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "country",
                                     label = "Select a Country's expenditure",
                                     choices = supplement_coun,),
                         p("You can select a country and look at this countries's expenditure across years and how the public and private direct expenditures are allocated.")
                       ),
                       mainPanel(
                         plotOutput("suppPlot")
                       )
                     )
                   ),
                   tabPanel(
                     "Instituation Types", #public, private, total in 2011
                     p("We also investiaged the ratio between total, public and private expenditure, and showed the relation using a scatterplot. You can click on the dots to see the specific data."),
                     checkboxInput(inputId = "color_check",
                                   label = "Check to color by instituation types",
                                   value = TRUE),  # default is not checked
                     varSelectInput(inputId = "x", 
                                    label = "select your x variable",
                                    data = select(supp_2011, 3:5), 
                                    selected = "Public"),
                     varSelectInput(inputId = "y", 
                                    label = "select your y variable",
                                    data = select(supp_2011, 3:5), 
                                    selected = "Private"),
                     
                     plotOutput("supp_ratio_plot", click = "plot_click"),
                     dataTableOutput("supp_ratio_table")
                   )  
                 )
                 
)


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

map.world <- map_data("world")
map.world$region[map.world$subregion == "Hong Kong"] <- "Hong Kong"

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




server <- function(input, output) {
  nn <- eventReactive(input$name, {input$name})
  output$testPlot <- renderPlot({
    cwur %>%
      group_by(country) %>%
      summarize(n = n()) %>%
      ggplot(aes(x = country, y = n)) +
      geom_col() +
      coord_flip()
  })
  #-----------------------------
  output$instnc <- renderUI({instnc(nn())})
  output$instnc2 <- renderUI({instnc(nn())})
  output$instnc3 <- renderUI({instnc(nn())})
  output$intro <- renderUI({HTML('<p>Hey! Here is the place you could virtually wander around the world-wide top universities of your choices (and their ranking data). Just a few side notes about the data along your journey: 1, loading is SLOW. We would greatly appreciate your patience! 2, the three rankings we are looking at are using substantially different scoring criteria so that most of the universities are absent in at least one of the three rankings. Try out the examples below if you like. 3, I would always want to highlight that all rights are reserved to source/original authors. See sources: <a href = "https://www.kaggle.com/mylesoneill/world-university-rankings?select=shanghaiData.csv"> data </a> and <a href = "https://en.wikipedia.beta.wmflabs.org/wiki/Main_Page"> images </a>. 3, pitifully, Carleton is not in any of these rankings. </p><p>Examples: (Copy to the input box. Remember to delete the quotes!):</br><strong>"Massachusetts Institute of Technology"</br>"University of Cambridge"</br>"University of Tokyo"</br>"University of Turin"</strong></p>')})
  #-----------------------------
  output$imgHint <- renderUI({
    if(!is.na(isInst(input$name))){
      HTML("Some Images about the School (Need a few moments to load. Your Patience would be appreciated! :) )")
    }
  })
  output$wranking <- renderTable({
    if(!is.na(isInst(nn()))) {
      n <- getRightName(nn())
      makeTable(n = n, r = "_w")
    }
  })
  output$nranking <- renderTable({
    if(!is.na(isInst(nn()))) {
      n <- getRightName(nn())
      makeTable(n = n, r = "_n")
    }
  })
  output$wr <- renderText({
    if(!is.na(isInst(input$name))){
      "World Rankings: "}})
  output$nr <- renderText({
    if(!is.na(isInst(input$name))){
      "National Rankings: "}})
  output$img1 <- renderUI({
    getInstImage(input$name, p = 4)
  })
  output$img2 <- renderUI({
    getInstImage(input$name, p = 5)
  })
  output$img3 <- renderUI({
    getInstImage(input$name, p = 7)
  })
  output$img4 <- renderUI({
    getInstImage(input$name, p = 8)
  })
  output$wikilink <- renderUI({
    if(!is.na(isInst(nn()))) {
      url <- getInstURL(nn())
      HTML(paste("Intrigued by the beautiful images? Want to learn more about this school? <a href=", url, ">Click here to visit the Source Wikipedia &gt;&gt;&gt;</a>"))
    }
  })
  #====================Plots======================
  output$overview <- renderPlot({
    try(expr = {
      n <- getRightName(nn())
      nData <- rk2 %>% filter(institution == n) %>%
        select(ends_with("s"), year)
      plotTitle <- paste("2012-2015",(rk2 %>% filter(institution == n))[[1]],"Ranking Total Scores")
      longNData<- pivot_longer(nData, cols = ends_with("s"),
                               values_to = "score",
                               names_to = "type",
                               names_transform = list(type = as.factor))
      longNData$type <- fct_recode(longNData$type, CWUR = "cwur_s", Shanghai = "sh_s", Times = "tm_s")
      ggplot(longNData, aes(x = year, y = score, color = type))+
        geom_point(na.rm = TRUE) +
        geom_line(na.rm = TRUE) +
        labs(title = plotTitle)
    }, silent = TRUE)
  })
  #===============
  output$times <- renderPlot({
    n <- getRightName(nn())
    if(!is.na(match(tolower(nn()), tmName))){
      nData <- tm %>% filter(university_name == n) %>% ungroup() %>%
        select(year, teaching, international, research, citations, income)
      longNData<- pivot_longer(nData, cols = c("teaching", "international", "research", "citations", "income"),
                               values_to = "score",
                               names_to = "criteria",
                               names_transform = list(criteria = as.factor))
      ggplot(longNData, aes(x = criteria, y = score, fill = year)) +
        geom_col(na.rm = TRUE, show.legend = FALSE) +
        facet_wrap(vars(year)) +
        coord_flip() +
        labs(title = "2012-2015 Times Hi-Ed Ranking Scores", subtitle = (tm %>% filter(university_name == n))[[2]])
    }
  })
  #===============
  output$shanghai <- renderPlot({
    n <- getRightName(nn())
    if(!is.na(match(tolower(nn()), shName))){
      nData <- sh %>% filter(university_name == n) %>%
        ungroup() %>%
        select(year, alumni, award, hici, ns, pub, pcp)
      longNData<- pivot_longer(nData, cols = c("alumni", "award", "hici", "ns", "pub", "pcp"),
                               values_to = "score",
                               names_to = "criteria",
                               names_transform = list(criteria = as.factor))
      ggplot(longNData, aes(x = criteria, y = score, fill = year)) +
        geom_col(na.rm = TRUE, show.legend = FALSE) +
        facet_wrap(vars(year)) +
        coord_flip() +
        labs(title = "2012-2015 Shanghai Academic Ranking Scores", subtitle = (tm %>% filter(university_name == n))[[2]])
    }
  })
  
  #==============================================
  output$scoreplot <- renderUI({
    # n <- input$name
    t <- input$scoretype
    if(!is.na(isInst(nn()))){
      if (t == "Overview"){
        plotOutput("overview")
      } else if (t == "Times"){
        tryCatch(expr = {plotOutput("times")},
                 error = function(e){HTML("Sorry, there are no available Times ranking data for this university.")})
      } else { # Shanghai
        tryCatch(expr = {plotOutput("shanghai")},
                 error = function(e){HTML("Sorry, there are no available Shanghai ranking data for this university.")})
      }
    }
  })
  #---------------------------
  output$timesscores <- renderTable({
    if(!is.na(match(tolower(nn()), tmName))){
      n <- getRightName(nn())
      nData <- tm %>% filter(university_name == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank,score = total_score)
      nData$score[is.na(nData$score)] <- "-"
      nData}
    else{
      data.frame(year = "-", world_rank = "-", national_rank = "-",score = "-")
    }
  })
  output$shanghaiscores <- renderTable({
    if(!is.na(match(tolower(nn()), shName))){
      n <- getRightName(nn())
      nData <- sh %>% filter(university_name == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank,score = total_score)
      nData$score[is.na(nData$score)] <- "-"
      nData}
    else{
      data.frame(year = "-", world_rank = "-", national_rank = "-",score = "-")
    }
  })
  output$cwurscores <- renderTable({
    if(!is.na(match(tolower(nn()), cwurName))){
      n <- getRightName(nn())
      nData <- cwur %>% filter(institution == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank, score)
      nData$score[is.na(nData$score)] <- "-"
      nData}
    else{
      data.frame(year = "-", world_rank = "-", national_rank = "-",score = "-")
    }
  })
  output$stats <- renderUI({
    if(!is.na(isInst(nn()))) {
      n <- getRightName(nn())
      tmn <- tm %>% filter(university_name == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank,score = total_score) %>%
        # drop_na() %>%
        arrange(year)
      shn <- sh %>% filter(university_name == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank,score = total_score) %>%
        # drop_na() %>%
        arrange(year)
      
      cwurn <- cwur %>% filter(institution == n) %>%
        ungroup() %>%
        select(year, world_rank, national_rank, score) %>%
        # drop_na() %>%
        arrange(year)
      tmn$score[is.na(tmn$score)] <- "-"
      shn$score[is.na(shn$score)] <- "-"
      cwurn$score[is.na(cwurn$score)] <- "-"
      ranktm <- findHigh(tmn$world_rank)
      ranksh <- findHigh(shn$world_rank)
      rankcwur <- findHigh(cwurn$world_rank)
      pa1 <- paste("<p>The highest rank achieved by this school in  <b>Times</b> Higher Education World University Ranking 2012-2015 is <b>",getHighRank(ranktm[1]),"</b>. The earliest year the school achieved this rank is <b>",getHighRankYear(ranktm[2]),"</b>. The maximum score is <b>",getHighScore(tmn),"</b>. The earliest year achieving the maximum score is <b>",getHighScoreYear(tmn),"</b>.</p>", sep = "")
      pa2 <- paste("<p>The highest rank achieved by this school in  <b>Shanghai</b> Academic Ranking of World Universities 2012-2015 is <b>",getHighRank(ranksh[1]),"</b>. The earliest year the school achieved this rank is <b>",getHighRankYear(ranksh[2]),"</b>. The maximum score is <b>",getHighScore(shn),"</b>. The earliest year achieving the maximum score is <b>",getHighScoreYear(shn),"</b>.</p>", sep = "")
      pa3 <- paste("<p>The highest rank achieved by this school in  <b>Center for World</b> University Ranking 2012-2015 is <b>",getHighRank(rankcwur[1]),"</b>. The earliest year the school achieved this rank is <b>",getHighRankYear(rankcwur[2]),"</b>. The maximum score is <b>",getHighScore(cwurn),"</b>. The earliest year achieving the maximum score is <b>", getHighScoreYear(cwurn),"</b>.</p>", sep = "")
      HTML(paste(pa1,pa2,pa3,sep = ""))
      
    }
  })
  
  #Map
  output$mapPlot <- renderPlot({
    if (input$RankType == "CWUR"){
      cwur_country_count<- cwur %>%
        filter(year == input$year) %>%
        group_by(country)%>%
        summarise(count = n())
      cwur_country_count$country <- recode(cwur_country_count$country, 
                                           'Slovak Republic' = 'Slovakia', 
                                           'United Kingdom' = 'UK'
      )
      map.world_joined <- left_join(map.world, cwur_country_count, by = c('region' = 'country'))
      cwur_map<- ggplot() + 
        geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = count)) +
        scale_fill_continuous() +
        labs(title = 'Distribution of Top Universities around the World', subtitle = "From The Center for World University Rankings")
      cwur_map
      
    } 
    else{
      tm_country_count <- tm %>%
        filter(year == input$year) %>%
        group_by(country)%>%
        summarise(count = n())
      tm_country_count$country <- recode(tm_country_count$country, 
                                         'Republic of Ireland' = 'Ireland', 
                                         'United Kingdom' = 'UK',
                                         
                                         'Russian Federation' = 'Russia',
                                         'United States of America' = 'USA'
      )
      map.world_joined <- left_join(map.world, tm_country_count, by = c('region' = 'country'))
      tm_map<- ggplot() +
        geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = count)) +
        scale_fill_continuous() +
        labs(title = 'Distribution of Top Universities around the World', subtitle = "From The Times Higher Education World University Ranking")
      tm_map
    }
  })
  
  output$maptable <- renderDataTable({
    if (input$RankType == "CWUR"){
      cwur_country_count<- cwur %>%
        filter(year == input$year) %>%
        group_by(country)%>%
        summarise(count = n())
      
    } 
    else{
      tm_country_count <- tm %>%
        filter(year == input$year) %>%
        group_by(country)%>%
        summarise(count = n())
    }
  })
  
  #search ranking number
  output$search_rank <- renderDataTable({
    
    if (input$RankType_num == "CWUR"){
      cwur%>%
        filter(world_rank == input$rank_num)%>%
        drop_na("institution")
      
    } else if (input$RankType_num == "Shanghai"){
      sh_rank_num <- sh%>%
        filter(world_rank == input$rank_num) %>%
        drop_na("university_name")
      sh_rank_num
    }
    else if (input$RankType_num == "Times"){
      tm_rank_num <- tm%>%
        filter(world_rank == input$rank_num) %>%
        drop_na("university_name")
      tm_rank_num
    }
    
  })
  
  #cwur
  output$uniPlot_cwur <- renderPlot({
    cwur %>%
      filter(institution == input$uni_cwur) %>%
      ggplot(aes(x= year, y = score)) + geom_point() + geom_line() + ylim(0,100)
  })
  
  #Shanghai
  output$uniPlot_sh <- renderPlot({
    sh_longer%>%
      filter(university_name == input$uni) %>%
      ggplot(aes(x = year, y = scores, color = score_types)) + geom_point() + geom_line() + ylim(0,100)
  })
  
  #Times
  
  output$uniPlot_tm <- renderPlot({
    tm_longer%>%
      filter(university_name == input$uni_tm) %>%
      ggplot(aes(x = year, y = scores, color = score_types)) + geom_point() + geom_line() + ylim(0,100)
  })
  
  output$teaching_tm <- renderPlot({
    tm %>% 
      filter(year == input$year_tm) %>%
      ggplot(aes(x = student_staff_ratio, y = teaching)) + geom_point() + geom_smooth(method = lm)
  })
  
  output$international_tm <- renderPlot({
    tm_2 %>% 
      filter(year == input$year_tm_2) %>%
      ggplot(aes(x = international_students, y = international)) + geom_point() + geom_smooth(method = lm)
  })
  
  #Expenditure
  output$suppPlot <- renderPlot ({
    supp_country<- supp_longer %>%
      filter(country == input$country) 
    ggplot(supp_country, aes(x = year, y = expenditure, color = institute_type)) + 
      geom_line() +
      geom_point() +
      ylim(0,8)
  })
  
  output$supp_ratio_plot <- renderPlot({
    g <- ggplot(supp_2011, aes(x = !!input$x, y = !!input$y)) + geom_label(aes(label = country), position = "nudge")
    if (input$color_check){
      g + geom_point(aes(color = institute_type))
    }else{
      g + geom_point()
    }
  })
  
  output$supp_ratio_table <- renderDataTable({
    nearPoints(supp_2011, input$plot_click)
  })
  
  #data
  output$data_times <- renderDataTable({
    tm}
  )
  output$knnPlot <- renderPlot({knnPlot})
  output$kmodel <- renderTable({j_knn_cv$finalModel
    test_conf_mat_k$byClass
    j_test %>% head(5)})
  output$explknn <- renderUI({HTML('<p>We used the K-nearest neighbours method to predict whether a country will have over 10 universities getting in the CWUR ranking in year 2014 based on its government total spending on education. Notice that here we are using a different dataset credit to <a href = "https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS?end=2019&most_recent_value_desc=false&start=2019&view=bar"> The World Bank </a>. There are a few limitations to this study: first, the data set is very small so that the classification model predictably does not work so well, although we have optimized our K-value with CV as possible. Out of 1000 universities entering the CWUR ranking, they only spread over a total of about 60 countries, and we are using a 2-8 split on training-testing datasets. Also there are a 3-5 missing values for expenditure data so there are finally 56 cases in total. Second, expenditure is the only predictor available, and there has not been chances for us to check out other possibilities due to limitation of data.</p><p>In our result, we have found that the model performed fairly well in predicting the countries having less than 10 universities in the rank (Specificity). Out of k values 1 to 8, we have used the best k value 8.</p>')})
  output$treePlot <- renderPlot({clusPlot})
  output$tmodel <- renderUI({HTML(paste("Total Within SS:",
                                        km_out_3$tot.withinss,
                                        "</br>Total Variations:",
                                        km_out_3$totss,
                                        "</br>Variations Explained by Clusters:",
                                        km_out_3$tot.withinss/km_out_3$totss,
                                        "</br>Variations Unexplained by Clusters:",
                                        1- km_out_3$tot.withinss/km_out_3$totss))})
  output$expltree <- renderUI({HTML("<p></br></p><p>Due to the same reason that our data size is small, the clustering method with number of centers 3 has clustered the countries on simple criteria. We can see from the graph that, roughly, all countries with educational spending over 5 percent GDP are in cluster 2 (green), all countries spending below 5 percent GDP are in cluster 3 (blue), and the single outlier country (USA) has been separated into cluster 1.</p>")})
}



shinyApp(ui, server, options = list(height = 1000))


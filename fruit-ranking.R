## Author: August Warren
## Description: Analysis of DC Stop and Frisk Data
## Date: 11/25/2018
## Status: Published
## Specs: R version 3.3.2 (2016-10-31)

library(tidyverse)
library(highcharter)
library(googlesheets)
library(ggridges)
library(ggplot2)
library(reshape2)
library(scales)
library(viridis)
library(lazyeval)
library(sf)
library(survey)
library(htmlTable)
library(highcharter)
library(leaflet)
library(rgdal)
library(sp)
library(rgeos)
library(mpaptools)

library(googledrive)

#####################################################
##
## Connect to data via googlesheets
##
#####################################################

setwd("/Users/augustwarren/github/fruit-ranking/")

sheets <- drive_find(type = "spreadsheet")

sheet_id <- "104eRCUeyIsyHZpWiGP5XcZ4oiXnBBos55Z5uUYh0TpM"

drive_download(as_id(sheet_id), type = "csv",overwrite = T)

survey_data <- read.csv("Fruit Ranking (Responses).csv")

#####################################################
##
## track completion over time
##
#####################################################

completion_over_time <- survey_data %>%
  select(Timestamp) %>%
  mutate(count = row_number())

completes <- ggplot(completion_over_time,aes(x=as.POSIXct(strptime(Timestamp,'%m/%d/%Y %H:%M:%S')),y=count)) +
  geom_line(size = 2) +
  labs(y="Number of completes",
       x="",
       title="Total number of fruit survey responses 11/13-11/17") +
  theme(axis.title = element_text(),
        plot.title = element_text(size = 18),
        axis.text = element_text(size=12),
        strip.text = element_text(size=16))

ggsave(plot = completes, "images\\completes.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## clean data 
##
#####################################################

## get rid of question text in column/variable names

columns <- colnames(survey_data)

columns <- sub(x=columns,pattern = "Assign.the.following.fruits.to.tiers..where.A.tier.is.the.best.highest.quality.and.F.tier.is.reserved.for.the.fruits.that.deserve.to.be.banished.....",
               replacement = "")

columns <- sub(x=columns,pattern = ".$",
               replacement = "")

colnames(survey_data) <- columns

fruits <- c("Raspberries","Strawberries","Bananas","Watermelon","Green.Apples",
            "Blueberries","Canteloupe","Honeydew","Kiwi","Mango","Apricots",
            "Blackberries","Clementines","Cherries","Grapes","Oranges","Peaches",
            "Pears","Pineapple","Grapefruit","Red.Apples")

survey_data$gender_recode <- ifelse(survey_data$To.which.gender.do.you.most.closely.identify  == "Male","Male",
                                    ifelse(survey_data$To.which.gender.do.you.most.closely.identify  == "Female","Female","Other"))

survey_data$race_recode <- factor(survey_data$Which.race.ethnicity.best.describes.you...Please.choose.only.one.,
         levels = c("White/Caucasian","Black or African American","Hispanic or Latino","Asian/Pacific Islander",
                  "American Indian or Alaskan Native","Multiple ethnicity/Other"))

survey_data$income_recode <- factor(survey_data$What.was.your.total.household.income.before.taxes.during.the.past.12.months,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Not sure/Refuse"))

survey_data$fruit_servings <- ifelse(survey_data$How.many.servings.of.fruit.do.you.eat.a.day..on.average == "3 or more","4 or more",
                                     survey_data$How.many.servings.of.fruit.do.you.eat.a.day..on.average)

survey_data$Do.you.consider.yourself.to.be.Conservative..Moderate..or.Liberal. <- factor(survey_data$Do.you.consider.yourself.to.be.Conservative..Moderate..or.Liberal,
                                                                                           levels = c("Liberal","Moderate","Conservative","Not sure")) 

survey_data$Do.you.consider.yourself.a.Democrat..Republican..or.something.else. <- factor(survey_data$Do.you.consider.yourself.a.Democrat..Republican..or.something.else,
                                                                                            levels = c("Democrat","Independent","Republican","Not sure")) 

survey_data$In.the.2016.election.for.President..did.you.vote.for.Democrat.Hillary.Clinton.or.Republican.Donald.Trump. <- factor(survey_data$In.the.2016.election.for.President..did.you.vote.for.Democrat.Hillary.Clinton.or.Republican.Donald.Trump,
                                                                                                                                  levels = c("Democrat Hillary Clinton","Republican Donald Trump","Green Party Jill Stein","Libertarian Gary Johnson","Did not vote","Refuse"))

## reshape fruit data to long for top-level aggregation

clean <- survey_data %>%
  select(fruits)

clean$id <- seq.int(nrow(clean))

clean_l <- melt(clean,id.vars = "id")

clean_l$value_recode <- ifelse(clean_l$value == "A-tier" | clean_l$value == "B-tier","A/B-tier",
                               ifelse(clean_l$value == "D-tier" | clean_l$value == "F-tier","D/F-tier",
                                      clean_l$value))

clean_l$gpa <- ifelse(clean_l$value == "A-tier",4,
                      ifelse(clean_l$value == "B-tier",3,
                             ifelse(clean_l$value == "C-tier",2,
                                    ifelse(clean_l$value == "D-tier",1,
                                           ifelse(clean_l$value == "F-tier",0,
                                                  NA)))))

#####################################################
##
## Plot 1: Overall distribution - everyone loves fruits
##
#####################################################

overall_stats <- clean_l %>%
  group_by(value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

overall_stats$value <- factor(overall_stats$value,levels = c("A-tier","B-tier","C-tier","D-tier","F-tier","Don't Know/Care"))

overall_bar_plot <- ggplot(overall_stats,aes(x=value,y=freq,fill=value)) +
  geom_bar(stat= "identity",color="black") +
  geom_text(aes(x=value,y=freq,label=percent(round(freq,2))),vjust = -.5) +
  scale_fill_manual(values = c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c","#D3D3D3")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Average Fruit Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  guides(fill=F) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12))

ggsave(plot = overall_bar_plot, "images\\overall_bar_plot.png", w = 10.67, h = 8,type = "cairo-png")

## Total & Average number of F-tier ratings given

fruit_df <- survey_data %>%
  select(fruits)

survey_data$f_count <- 0

df.new <- as.data.frame(lapply(fruit_df, function(x) ifelse(x == "F-tier", 1, 0)))

df.new <- df.new %>%
  mutate(count_f = rowSums(.))

df.new$id <- seq.int(nrow(df.new))

df.a <- as.data.frame(lapply(fruit_df, function(x) ifelse(x == "A-tier", 1, 0)))

df.a <- df.a %>%
  mutate(count_a = rowSums(.)) %>%
  select(count_a)

df.a$id <- seq.int(nrow(df.a))

df.new <- merge(df.a,df.new, by="id")

mean_f <- mean(df.new$count_f)
mean_a <- mean(df.new$count_a)

df.new <- df.new %>%
  select(id,count_a,count_f) %>%
  melt(id.vars ="id")

df.new$variable <- ifelse(df.new$variable == "count_a","A-Tier","F-Tier")

a_f_tier_ratings <- ggplot(df.new,aes(x=value,fill=variable)) +
  geom_density(alpha = .5) +
  geom_vline(xintercept = mean_f,color="blue") +
  geom_vline(xintercept = mean_a,color="red") +
  labs(title = "Distribution of A-tier & F-tier Ratings",
       subtitle = paste("among a very non-random sample of",count,"people"),
       x = "Total F-tier Ratings",
       fill = "Tier") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text = element_text(size=12)) 

ggsave(plot = a_f_tier_ratings, "images\\a_f_tier_ratings.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Heat map of results
##
#####################################################

stats <- clean_l %>%
  group_by(variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

stats_a_tier <- stats %>%
  filter(value == "A-tier") %>%
  rename(a_freq = freq) %>%
  select(a_freq,variable)

stats <- merge(stats,stats_a_tier)

stats$value <- factor(stats$value,levels = c("A-tier","B-tier","C-tier","D-tier","F-tier","Don't Know/Care"))

count <- nrow(survey_data)

fruit_heatmap_plot <- ggplot(stats,aes(x=value,y=reorder(variable,a_freq))) +
  geom_tile(aes(fill = freq),colour = "white") +
  geom_text(aes(x=value,y=reorder(variable,a_freq),label=percent(round(freq,2)))) +
  scale_fill_viridis(name="",labels = percent) +
  labs(title = "Overall Fruit Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm"))

ggsave(plot = fruit_heatmap_plot, "images\\fruit_heatmap_plot.png", w = 10.67, h = 8,type = "cairo-png")

stats_gpa <- clean_l %>%
  filter(!is.na(gpa)) %>%
  group_by(variable) %>%
  summarise(avg_gpa = mean(gpa),
            wtd_gpa = weighted.mean(gpa,weights)) %>%
  rename(fruit = variable)

stats_gpa <- melt(stats_gpa)

ggplot(stats_gpa,aes(x=reorder(fruit,value),y=variable)) +
  geom_tile(aes(fill = value),colour = "white") +
  geom_text(aes(x=fruit,y=variable,label=round(value,2))) +
  coord_flip() +
  scale_fill_viridis(name="GPA") +
  labs(title = paste("Overall Fruit GPA - Weighted vs. unweighted"),
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm"))
  

#####################################################
##
## Plot 1: Heat map of results WEIGHTED
##
#####################################################

survey_data$age <- survey_data$`What is your age?`

survey_data.svy.unweighted <- svydesign(ids=~1, data=survey_data)

sex.dist <- data.frame(gender_recode = c("Male", "Female","Other"),
                       Freq = nrow(survey_data) * c(0.49, 0.51,.01))

race.dist <- data.frame(race_recode = c("White/Caucasian", "Black or African American","Hispanic or Latino",
                                          "Asian/Pacific Islander","American Indian or Alaskan Native","Multiple ethnicity/Other"),
                        Freq = nrow(survey_data) * c(0.766, 0.134,.181,.058,.013,.027))

age.dist <- data.frame(age = c("18-24", "25-29","30-34","35-39","40-44","45-49","50+"),
                       Freq = nrow(survey_data) * c(0.0957, 0.0936,0.0885,0.0895,0.0927,0.1007,0.4393)) 

survey_data.svy.rake <- rake(design = survey_data.svy.unweighted,
                       sample.margins = list(~gender_recode,~race_recode,~age),
                       population.margins = list(sex.dist,race.dist,age.dist))

survey_data$weights <- weights(survey_data.svy.rake)

summary(weights(survey_data.svy.rake)) ## high weight of 17 ...

clean <- survey_data %>%
  select(fruits,weights)

clean$id <- seq.int(nrow(clean))

clean_l <- melt(clean,id.vars = c("id","weights"))

stats <- clean_l %>%
  group_by(variable,value) %>%
  summarise(n=sum(weights)) %>%
  mutate(freq=n/sum(n))

stats_a_tier <- stats %>%
  filter(value == "A-tier") %>%
  rename(a_freq = freq) %>%
  select(a_freq,variable)

stats <- merge(stats,stats_a_tier)

stats$value <- factor(stats$value,levels = c("A-tier","B-tier","C-tier","D-tier","F-tier","Don't Know/Care"))

count <- nrow(survey_data)

fruit_heatmap_wtd_plot <- ggplot(stats,aes(x=value,y=reorder(variable,a_freq))) +
  geom_tile(aes(fill = freq),colour = "white") +
  geom_text(aes(x=value,y=reorder(variable,a_freq),label=percent(round(freq,2)))) +
  scale_fill_viridis(name="",labels = percent) +
  labs(title = "Overall Fruit Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit weighted to the US Adult population")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm"))

ggsave(plot = fruit_heatmap_wtd_plot, "images\\fruit_heatmap_wtd_plot.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Correlation Matrix
##
#####################################################

clean_l_filtered <- clean_l %>%
  filter(!is.na(gpa))

wide_fruits <- dcast(clean_l_filtered, id ~ variable, value.var = "gpa") 

correlations <- cor(wide_fruits,use="complete.obs")

wide_corr <- melt(correlations)

drop <- c("id")

wide_corr <- wide_corr %>%
  filter(Var1 != "id" & Var2 != "id")

correlations_matrix <- ggplot(wide_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(aes(fill = value),colour = "white") +
  geom_text(aes(x=Var1,y=Var2,label=round(value,2))) +
  scale_fill_gradientn(colours = c("red","white","#1a9641"), 
                       values = rescale(c(-.5,0,.7)),
                       guide = "colorbar", limits=c(-.5,.7)) +
  labs(title = "Fruit Correlation Matrix",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit"),
       fill = "R-Squared") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width = unit(1, "cm"))

ggsave(plot = correlations_matrix, "images\\correlations_matrix.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Overall Average GPA & Standard Deviations
##
#####################################################

overall_gpa <- clean_l %>%
  filter(!is.na(gpa)) %>%
  group_by(variable) %>%
  summarise(avg_gpa = mean(gpa),
            sd_gpa = sd(gpa))

overall_gpa_se <- ggplot(overall_gpa, aes(x=reorder(variable,avg_gpa), y=avg_gpa,fill=avg_gpa)) + 
  geom_errorbar(aes(ymin=avg_gpa-sd_gpa, ymax=avg_gpa+sd_gpa), width=.2) +
  geom_point(pch=21,size=3) +
  coord_flip() +
  scale_fill_viridis(name="Average GPA") +
  labs(title = paste("Overall Fruit GPA with Standard Error"),
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.key.width = unit(1, "cm"))

ggsave(plot = overall_gpa_se, "images\\overall_gpa_se.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Fruit Rankings by Demos
##
#####################################################

clean_demos <- survey_data %>%
  select(fruits,weights,
         `What is your age?`,
         gender_recode,
         race_recode,
         income_recode,
         fruit_servings,
         `Do you consider yourself to be Conservative, Moderate, or Liberal?`,
         `In the 2016 election for President, did you vote for Democrat Hillary Clinton or Republican Donald Trump?`) %>%
  rename(age = "What is your age?",
         ideo = "Do you consider yourself to be Conservative, Moderate, or Liberal?",
         pres = "In the 2016 election for President, did you vote for Democrat Hillary Clinton or Republican Donald Trump?")

clean_l_demos <- melt(clean_demos,id.vars = c("weights","age","race_recode","gender_recode","income_recode","fruit_servings","ideo","pres"))

clean_l_demos$gpa <- ifelse(clean_l_demos$value == "A-tier",4,
                             ifelse(clean_l_demos$value == "B-tier",3,
                                    ifelse(clean_l_demos$value == "C-tier",2,
                                           ifelse(clean_l_demos$value == "D-tier",1,
                                                  ifelse(clean_l_demos$value == "F-tier",0,
                                                         NA)))))

demos <- c("age","gender_recode","income_recode","race_recode","fruit_servings","ideo","pres")
demo_label <- c("Age","Gender","Income","Race/Ethnicity","Daily Fruit Servings","Ideology","2016 Presidential Vote")

num <- 1

for (d in demos) {
  
  group_var <- d[1]
  
  stats_demos <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var,variable) %>%
    summarise(avg_gpa = mean(gpa),
              wtd_gpa = weighted.mean(gpa,weights),
              n = n()) %>%
    filter(n >= 10)
  
  stats_demos$sort_gpa <- stats_demos$avg_gpa
  
  avg_all_fruits <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var) %>%
    summarise(avg_gpa = mean(gpa),
              wtd_gpa = weighted.mean(gpa,weights),
              n = n()/21) %>%
    filter(n >= 10)%>%
    select(group_var,avg_gpa,wtd_gpa) 
  
  avg_all_fruits$variable <- "All Fruits on Average"
  
  avg_all_fruits$sort_gpa <- 0
  
  avg_all_fruits <- avg_all_fruits[,c(1,3,2,4)]
  
  stats_demos <- bind_rows(data.frame(stats_demos),data.frame(avg_all_fruits))
  
  avg_all_fruits <- avg_all_fruits %>%
    rename(avg_gpa_all = avg_gpa) %>%
    select(avg_gpa_all,group_var)
  
  stats_demos <- merge(stats_demos,avg_all_fruits,by=group_var)
  
  stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_gpa)
  stats_demos$avg_gpa <- round(stats_demos$avg_gpa,2)
  stats_demos$wtd_gpa <- round(stats_demos$wtd_gpa,2)
    
  demo_plot <- ggplot(stats_demos,aes_string(x=d,y="variable")) +
    geom_tile(aes(fill = avg_gpa),colour = "white") +
    geom_text(aes_string(x=d,y="variable",label="avg_gpa")) +
    scale_fill_viridis(name="GPA") +
    labs(title = paste("Overall Fruit GPA by",demo_label[num]),
         subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size=12),
          legend.key.width = unit(1, "cm"))
  
  ggsave(plot = demo_plot, paste0("images\\demo_",d,"_plot.png"), w = 10.67, h = 8,type = "cairo-png")
  
  demo_wtd_plot <- ggplot(stats_demos,aes_string(x=d,y="variable")) +
    geom_tile(aes(fill = wtd_gpa),colour = "white") +
    geom_text(aes_string(x=d,y="variable",label="wtd_gpa")) +
    scale_fill_viridis(name="GPA") +
    labs(title = paste("Overall Fruit GPA by",demo_label[num]),
         subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit (weighted to the US adult population)")) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size=12),
          legend.key.width = unit(1, "cm"))
  
  ggsave(plot = demo_wtd_plot, paste0("images\\demo_",d,"_wtd_plot.png"), w = 10.67, h = 8,type = "cairo-png")
  
  num <- num + 1
  
}

#####################################################
##
## Plot 1: Map of Favorite Fruits (Most A-tiered)
##
#####################################################

clean_states <- survey_data %>%
  select(fruits,`In which state do you currently live?`) %>%
  rename(state = "In which state do you currently live?")

clean_l_states <- melt(clean_states,id.vars = "state")

clean_l_states$gpa <- ifelse(clean_l_states$value == "A-tier",4,
                      ifelse(clean_l_states$value == "B-tier",3,
                             ifelse(clean_l_states$value == "C-tier",2,
                                    ifelse(clean_l_states$value == "D-tier",1,
                                           ifelse(clean_l_states$value == "F-tier",0,
                                                  NA)))))

state_stats <- clean_l_states %>%
  filter(!is.na(clean_l_states$gpa)) %>%
  group_by(state,variable) %>%
  summarise(avg_gpa = mean(gpa)) %>%
  mutate(min_gpa = min(avg_gpa),
         max_gpa = max(avg_gpa))

state_n <- survey_data %>%
  rename(state = "In which state do you currently live?") %>%
  group_by(state) %>%
  summarise(total_n = n())

state_stats <- merge(state_stats,state_n) %>%
  filter((avg_gpa == min_gpa | avg_gpa == max_gpa) &
        total_n > 10) %>%
  select(state,variable,avg_gpa) 

state_stats$rank <- ifelse(state_stats$avg_gpa<2,"Lowest Rated","Highest Rated")

state_cols <- unique(state_stats$state)

state_stats <- state_stats %>%
  select(variable,avg_gpa,rank) %>%
  mutate(avg_gpa = round(avg_gpa,2))

state_stats <- state_stats[,c(3,1,2)]

state_table <- htmlTable(
  x        = state_stats,
  caption  = paste("Highest/Lowest Rated Fruits by State"),
  header    = c("","Fruit","Average GPA"),
  rowlabel = "",
  rgroup   = state_cols,
  n.rgroup = c(2,2,2,2,2,2,2,2,2,2,2,2),
  ctable   = TRUE,
  type="html")

print(state_table,useViewer = utils::browseURL)

## create census regions

clean_l_states$region_census <- ifelse(clean_l_states$state %in% c("Arizona","Colorado","Idaho","New Mexico","Montana","Utah","Nevada","Wyoming","Alaska","California","Hawaii","Oregon","Washington"),"Pacific",
                                ifelse(clean_l_states$state %in% c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas"),"South",
                                       ifelse(clean_l_states$state %in% c("Indiana","Illinois","Michigan","Ohio","Wisconsin","Iowa","Nebraska","Kansas","North Dakota","Minnesota","South Dakota","Missouri"),"Midwest",
                                              ifelse(clean_l_states$state %in% c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania"),"Northeast","Other"))))

region_stats <- clean_l_states %>%
  filter(!is.na(clean_l_states$gpa)) %>%
  group_by(region_census,variable) %>%
  summarise(avg_gpa = mean(gpa)) %>%
  mutate(min_gpa = min(avg_gpa),
         max_gpa = max(avg_gpa)) %>%
  filter((avg_gpa == min_gpa | avg_gpa == max_gpa)) %>%
  select(region_census,variable,avg_gpa) %>%
  mutate(jvar = row_number())

region_rating <- dcast(region_stats,region_census ~ jvar, value.var = c("avg_gpa"))
region_stats <- dcast(region_stats,region_census ~ jvar, value.var = c("variable"))

region_stats <- region_stats %>%
  rename(highest_rated = `1`,
         lowest_rated = `2`)

region_stats <- merge(region_rating,region_stats,by="region_census")

states <- readOGR(dsn = ".", 
                  layer = "cb_2017_us_state_500k")

states$region_census <- ifelse(states$NAME %in% c("Arizona","Colorado","Idaho","New Mexico","Montana","Utah","Nevada","Wyoming","Alaska","California","Hawaii","Oregon","Washington"),"Pacific",
                               ifelse(states$NAME %in% c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas"),"South",
                                      ifelse(states$NAME %in% c("Indiana","Illinois","Michigan","Ohio","Wisconsin","Iowa","Nebraska","Kansas","North Dakota","Minnesota","South Dakota","Missouri"),"Midwest",
                                             ifelse(states$NAME %in% c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania"),"Northeast","Other"))))

regions <- states %>%
  group_by(region_census) %>%
  summarize(
    water = sum(AWATER),
    land  = sum(ALAND))

states.coord <- coordinates(states)

region.id <- ifelse(states$NAME %in% c("Arizona","Colorado","Idaho","New Mexico","Montana","Utah","Nevada","Wyoming","Alaska","California","Hawaii","Oregon","Washington"),"Pacific",
                    ifelse(states$NAME %in% c("Delaware","District of Columbia","Florida","Georgia","Maryland","North Carolina","South Carolina","Virginia","West Virginia","Alabama","Kentucky","Mississippi","Tennessee","Arkansas","Louisiana","Oklahoma","Texas"),"South",
                           ifelse(states$NAME %in% c("Indiana","Illinois","Michigan","Ohio","Wisconsin","Iowa","Nebraska","Kansas","North Dakota","Minnesota","South Dakota","Missouri"),"Midwest",
                                  ifelse(states$NAME %in% c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania"),"Northeast","Other"))))

region.union <- unionSpatialPolygons(states, region.id)

states.df <- as(states, "data.frame")

regions <- merge(region_stats,states.df,by="region_census")

states.df.agg <- aggregate(regions[, 2:3], list(region.id), mean)
states.df.agg <- merge(states.df.agg,region_stats,by.x="Group.1",by.y="region_census")
row.names(states.df.agg) <- as.character(states.df.agg$Group.1)

states.shp.agg <- SpatialPolygonsDataFrame(region.union, states.df.agg)

factpal <- colorFactor("Set1", states.shp.agg$highest_rated)

labels <- sprintf(
  "<strong>%s</strong><br/>Highest Rated Fruit: %s",
  states.shp.agg$Group.1, states.shp.agg$highest_rated
) %>% lapply(htmltools::HTML)

leaflet(states.shp.agg) %>%
  addTiles() %>%
  addPolygons(stroke = T, 
              smoothFactor = 0.2, 
              fillOpacity = .75,
              fillColor  = ~factpal(highest_rated),
              weight = 2,
              opacity = 1,
              color = "black", 
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$OpenStreetMap)
  
#####################################################
##
## Urban/Rural
## source: https://www.census.gov/geo/maps-data/data/ua_rel_download.html
##
#####################################################

urban_areas <- read.csv(file = "https://www2.census.gov/geo/docs/maps-data/data/rel/ua_zcta_rel_10.txt")

urban_areas <- urban_areas %>%
  filter(UA != 99999) %>%
  select(ZCTA5,ZPOP)

urban_areas <- distinct(urban_areas)

survey_data$int_zip <- as.numeric(survey_data$`What zip code did you grow up in?`)

survey_urban <- merge(survey_data,urban_areas,by.x="int_zip",by.y="ZCTA5",all.x = T)

survey_urban$urban_rural <- ifelse(!is.na(survey_urban$ZPOP),"Urban","Rural")

survey_urban <- survey_urban %>%
  filter((int_zip != 0 & int_zip != 99999))

clean_demos <- survey_urban %>%
  select(fruits,urban_rural)

clean_l_demos <- melt(clean_demos,id.vars = c("urban_rural"))

clean_l_demos$gpa <- ifelse(clean_l_demos$value == "A-tier",4,
                            ifelse(clean_l_demos$value == "B-tier",3,
                                   ifelse(clean_l_demos$value == "C-tier",2,
                                          ifelse(clean_l_demos$value == "D-tier",1,
                                                 ifelse(clean_l_demos$value == "F-tier",0,
                                                        NA)))))

stats_demos <- clean_l_demos %>%
  filter(!is.na(clean_l_demos$gpa)) %>%
  group_by(urban_rural,variable) %>%
  summarise(avg_gpa = mean(gpa),
            n = n()) %>%
  filter(n >= 10)

stats_demos$sort_gpa <- stats_demos$avg_gpa

avg_all_fruits <- clean_l_demos %>%
  filter(!is.na(clean_l_demos$gpa)) %>%
  group_by(urban_rural) %>%
  summarise(avg_gpa = mean(gpa),
            n = n()/21) %>%
  filter(n >= 10)%>%
  select(urban_rural,avg_gpa) 

avg_all_fruits$variable <- "All Fruits on Average"

avg_all_fruits$sort_gpa <- 0

avg_all_fruits <- avg_all_fruits[,c(1,3,2)]

stats_demos <- bind_rows(data.frame(stats_demos),data.frame(avg_all_fruits))

avg_all_fruits <- avg_all_fruits %>%
  rename(avg_gpa_all = avg_gpa) %>%
  select(avg_gpa_all,urban_rural)

stats_demos <- merge(stats_demos,avg_all_fruits,by="urban_rural")

stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_gpa)
stats_demos$avg_gpa <- round(stats_demos$avg_gpa,2)

scatter_data <- dcast(stats_demos,variable ~ urban_rural, value.var = "avg_gpa")

scatter_data$diff <- scatter_data$Urban - scatter_data$Rural 


x <- c("Fruit: ","Rural: ", "Urban: ")
y <- c("{point.variable}",sprintf("{point.%s:.2f}", c("Rural", "Urban")))
tltip <- tooltip_table(x, y)

count_urban_rural <- nrow(survey_urban)

urban_rural_chart <- hchart(scatter_data, "scatter", hcaes(x= Urban, y = Rural, color = diff)) %>% 
  hc_add_series(data = abs(c(0,1,2,3,4)), type = "line",tooltip =F) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) %>%
  hc_title(
    text = "Overall Fruit GPA by Urban/Rural Roots") %>%
  hc_subtitle(text = paste("among a very non-random sample of",count_urban_rural,"people with opinions about fruit")) %>%
  hc_yAxis(min = 1,title=list(text="Average Fruit Rating among Rural Roots")) %>%
  hc_xAxis(min = 1,title=list(text="Average Fruit Rating among Urban Roots")) %>%
  hc_size(600, 600)


#####################################################
##
## Plot 1: Regressions!
##
#####################################################

clean <- survey_data %>%
  select(fruits,gender_recode,
         income_recode,
         `What is your age?`,
         race_recode,
         fruit_servings) %>%
  rename(age_recode = `What is your age?`)

recode_fruits <- function(df,fruit) {
  new_var <- paste0(fruit,"_recode")

  df[new_var] <- ifelse(df[,fruit] == "A-tier",1,
                       ifelse(df[,fruit] == "B-tier",.75,
                              ifelse(df[,fruit] == "C-tier",.5,
                                     ifelse(df[,fruit] == "D-tier",.25,
                                            ifelse(df[,fruit] == "F-tier",0,
                                                   NA)))))
  return(as.data.frame(df))
}

for (f in fruits) {
  clean <- recode_fruits(clean,f)
}

clean$Raspberries_recode <- as.vector(clean$Raspberries_recode)

clean <- clean %>%
  mutate(avg_fruit_rating = rowSums(.[27:47])/21)

clean$male <- ifelse(clean$gender_recode == "Male",1,0)
clean$white <- ifelse(clean$race_recode == "White/Caucasian",1,0)
clean$youth <- ifelse(clean$age_recode == "18-24" | 
                        clean$age_recode == "25-29",1,0)
clean$low_income <- ifelse(clean$income_recode == "Under $50,000",1,0)

model_results <- data.frame()

for(f in fruits) {
  
  dv <- paste0(f,"_recode")

  model <- glm(get(dv) ~ male + race_recode + youth + low_income + fruit_servings, family = "binomial",data=clean)

  model_df <- as.data.frame(summary.glm(model)$coefficients,row.names = F)
  model_df$iv <- rownames(as.data.frame(summary.glm(model)$coefficients))
  model_df$fruit <- f
  model_df$odds <- exp(model_df$Estimate)
  
  model_results <- rbind(model_results,model_df)
  
}

#####################################################
##
## Plot 1: % A/B v. % D/F scatter
##
#####################################################

stats <- clean_l %>%
  group_by(variable,value_recode) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(value_recode %in% c("A/B-tier","D/F-tier"))

a_v_f_tier <- dcast(stats,variable ~ value_recode, value.var = "freq")

ggplot(a_v_f_tier,aes(x=`D/F-tier`,y=`A/B-tier`)) + 
  geom_point() +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

#####################################################
##
## Plot 1: Fruit or Not Fruit?
##
#####################################################

fruits <- c("[Tomato]","[Cucumber]","[Avocado]","[Pumpkin]","[Bell Pepper]","[Coconut]")

clean <- survey_data %>%
  select(fruits,
         gender_recode,
         income_recode,
         `What is your age?`,
          race_recode) %>%
  rename(age = `What is your age?`)

clean_l <- melt(clean,id.vars = c("gender_recode","income_recode","age","race_recode"))

clean_l$variable <- gsub(replacement = "",pattern = "\\[|\\]", x= clean_l$variable)

fruit_not_fruit <- clean_l %>%
  group_by(variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))

pct_fruit <-  fruit_not_fruit %>%
  filter(value == "Fruit!") %>%
  rename(fruit_freq = freq) %>%
  select(variable,fruit_freq)

fruit_not_fruit <- merge(fruit_not_fruit,pct_fruit)
fruit_not_fruit$variable <- reorder(fruit_not_fruit$variable,-fruit_not_fruit$fruit_freq)

fruit_not_fruit_plot <- ggplot(fruit_not_fruit,aes(x=value,y=freq,fill=value)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(x=value,y=freq,label=percent(round(freq,2))),vjust = -.5) +
  facet_wrap(~variable) +
  scale_fill_manual(values = c("#43a2ca","#fc8d59")) +
  labs(title = "Fruit or Not Fruit?",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  guides(fill=F) +
  scale_y_continuous(labels=percent,limits = c(0,.9)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12),
        strip.text.x = element_text(size = 12)) 
  
ggsave(plot = fruit_not_fruit_plot, "images\\fruit_not_fruit_plot.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Fruit or Not Fruit?
##
#####################################################

demos <- c("age","gender_recode","income_recode","race_recode")
demo_label <- c("Age","Gender","Income","Race/Ethnicity")

num <- 1

d <- "race_recode"

for (d in demos) {
  
  group_var <- d[1]
  
  stats_demos <- clean_l %>%
    group_by_(.dots=c(group_var,"variable","value")) %>%
    summarise(n = n()) %>%
    mutate(freq = n/sum(n))
  
  stats_demos$sort_freq <- stats_demos$freq
  
  n_sizes <-  clean %>%
    group_by_(.dots=c(group_var)) %>%
    summarise(n_size = n()) 
  
  avg_all_fruits <- clean_l %>%
    group_by(.dots=c(group_var,"value")) %>%
    summarise(n = n()) %>%
    mutate(freq = n/sum(n)) %>%
    select(group_var,value,freq)
  
  avg_all_fruits$variable <- "Average"
  
  avg_all_fruits$sort_freq <- 1
  
  avg_all_fruits <- avg_all_fruits[,c(1,4,2,3,5)]
  
  stats_demos <- bind_rows(data.frame(stats_demos),data.frame(avg_all_fruits))

  stats_demos <- merge(stats_demos,n_sizes) %>%
    filter(n_size >= 10 & value == "Fruit!")
  
  stats_demos$merge_var <- stats_demos[,1]

  ## create complete df of fruits and options to merge in for 100% & 0% results
  
  complete_options <- distinct(stats_demos,variable,value) 
  
  demos <- stats_demos %>%
    select(.dots = d) %>%
    distinct(.dots = d)
  
  demos <- as.list(demos[,1])
  
  total_df <- data.frame()
  
  for (i in demos) {
    temp_df <- complete_options
    temp_df$merge_var <- i  
    
    total_df <- rbind(total_df,temp_df)
  }
  
  stats_demos <- merge(stats_demos,total_df,by=c("merge_var","variable","value"),all=T,how="outer")
  
  stats_demos$freq <- ifelse(is.na(stats_demos$freq),0,stats_demos$freq)
  stats_demos$sort_freq <- ifelse(is.na(stats_demos$sort_freq),0,stats_demos$sort_freq)
  
  stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_freq)
  stats_demos$freq_lab <- percent(round(stats_demos$freq,2))
  
  demo_plot <- ggplot(stats_demos,aes_string(x="merge_var",y="variable")) +
    geom_tile(aes(fill = freq),colour = "white") +
    geom_text(aes_string(x="merge_var",y="variable",label="freq_lab"),size=6) +
    scale_fill_viridis(name="% Fruit!",labels = percent) +
    labs(title = paste("% Fruit! by",demo_label[num]),
         subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size=12),
          legend.key.width = unit(1, "cm"))
  
  ggsave(plot = demo_plot, paste0("images\\fruit_demo_",d,"_plot.png"), w = 10.67, h = 8,type = "cairo-png")
  
  num <- num + 1
  
}

#####################################################
##
## Fruit Production
##
#####################################################

clean <- survey_data %>%
  select(fruits,
         gender_recode,
         income_recode,
         What.is.your.age,
         race_recode,
         fruit_servings,
         What.zip.code.did.you.grow.up.in) %>%
  rename(age_recode = What.is.your.age)

recode_fruits <- function(df,fruit) {
  new_var <- paste0(fruit,"_recode")
  
  df[new_var] <- ifelse(df[,fruit] == "A-tier",1,
                        ifelse(df[,fruit] == "B-tier",.75,
                               ifelse(df[,fruit] == "C-tier",.5,
                                      ifelse(df[,fruit] == "D-tier",.25,
                                             ifelse(df[,fruit] == "F-tier",0,
                                                    NA)))))
  return(as.data.frame(df))
}

for (f in fruits) {
  clean <- recode_fruits(clean,f)
  clean <- clean %>%
    select(-f)
}

clean$Raspberries_recode <- as.vector(clean$Raspberries_recode)

clean$respondent_id <- seq.int(nrow(clean))

id_vars <- c("respondent_id","gender_recode","age_recode",
             "income_recode","fruit_servings",
             "What.zip.code.did.you.grow.up.in",
             "race_recode")

clean_l <- melt(clean, id.vars = id_vars) %>%
  filter(!is.na(What.zip.code.did.you.grow.up.in))

clean_l$variable <- ifelse(as.character(clean_l$variable) == "Red.Apples_recode" | 
                             as.character(clean_l$variable) == "Green.Apples_recode","Apples_recode",
                           as.character(clean_l$variable))

clean_l <- clean_l %>%
  group_by(respondent_id,gender_recode,age_recode,
           income_recode,fruit_servings,
           What.zip.code.did.you.grow.up.in,
           race_recode,variable) %>%
  summarise(adj_value = mean(value,rm.na=T)) %>%
  mutate(variable = sub(x=variable,pattern = "_recode","",replacement = ""))

## pull in zip to state look-up file

zip_state <- read.csv("ZIP-COUNTY-FIPS_2010-03.csv")

clean_l_state <- merge(clean_l,zip_state,by.x="What.zip.code.did.you.grow.up.in",by.y="ZIP",all.x=T)

clean_l_state$state_fips <- ifelse(clean_l_state$STCOUNTYFP < 10000,
                                   as.numeric(substr(as.character(clean_l_state$STCOUNTYFP), start = 1, stop = 1)),
                                   as.numeric(substr(as.character(clean_l_state$STCOUNTYFP), start = 1, stop = 2)))

## pull in state level fruit production 

fruit_production <- read.csv("ECE5A911-6DC5-315F-A12F-4FB24A135D63.csv") %>%
  mutate(fruit_recode = trimws(sub(x=Data.Item,pattern = " -.*",
                                replacement = "")),
         Data.Item = trimws(Data.Item)) %>%
  filter(Period == "YEAR" &
           fruit_recode %in% c("APPLES","BANANAS","BLACKBERRIES","BLUEBERRIES, TAME","CHERRIES",
                            "GRAPES","GRAPEFRUIT","KIWIFRUIT","ORANGES","PEACHES","PEARS","RASPBERRIES",
                            "STRAWBERRIES") &
           (Data.Item == paste0(fruit_recode," - PRODUCTION, MEASURED IN TONS") | 
              Data.Item == paste0(fruit_recode," - PRODUCTION, MEASURED IN LB")) &
            !grepl(pattern = "\\$",x = Data.Item)) %>%
  mutate(Commodity = str_to_title(Commodity),
         value_int = as.numeric(gsub(",","",Value)),
         value_int = if_else(grepl("TONS",Data.Item),value_int*2000,value_int)) %>%
  group_by(State,State.ANSI,Commodity) %>%
  summarise(avg_production = mean(value_int,na.rm=T))

## merge

opinion_production <- merge(clean_l_state,fruit_production,
                            by.x=c("state_fips","variable"),
                            by.y=c("State.ANSI","Commodity")) %>%
  filter(!is.na(COUNTYNAME)) %>%
  mutate(production_bins = ntile(avg_production, 4))

rating_by_bins <- opinion_production %>%
  group_by(variable,production_bins) %>%
  filter(!is.na(adj_value)) %>%
  summarise(avg_rating = mean(adj_value))

ggplot(opinion_production,aes(avg_production,y=adj_value)) +
  geom_point()

#####################################################
##
## Appendix: Demographics table
##
#####################################################

demos <- survey_data %>%
  select(gender_recode,race_recode,income_recode,
         `In the 2016 election for President, did you vote for Democrat Hillary Clinton or Republican Donald Trump?`,
         `Do you consider yourself a Democrat, Republican, or something else?`,
         `Do you consider yourself to be Conservative, Moderate, or Liberal?`) %>%
  rename(pres_vote = `In the 2016 election for President, did you vote for Democrat Hillary Clinton or Republican Donald Trump?`,
         pid = `Do you consider yourself a Democrat, Republican, or something else?`,
         ideo = `Do you consider yourself to be Conservative, Moderate, or Liberal?`)

demos$id <- "ix"

demos_l <- melt(demos,id.vars = "id")

demos_summary <- demos_l %>%
  group_by(variable,value) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) 

demos_summary$value <- ordered(demos_summary$value,
                                  levels = c("Male","Female","Other","White/Caucasian",
                                             "Black or African American" ,"Hispanic or Latino" ,
                                             "Asian/Pacific Islander","American Indian or Alaskan Native" ,
                                             "Multiple ethnicity/Other" ,"Under $50,000","$50,000 to $100,000",
                                             "Over $100,000","Not sure/Refuse","Democrat Hillary Clinton",
                                             "Republican Donald Trump","Green Party Jill Stein","Libertarian Gary Johnson",
                                             "Did not vote" ,"Refuse","Democrat","Independent","Republican",
                                             "Liberal","Moderate","Conservative","Not sure"))

demos_summary <- demos_summary %>%
  group_by(variable,value) %>%
  arrange(variable,value) %>%
  ungroup() %>%
  select(value,n,freq) %>%
  rename(Response = value,
         N = n,
         Percent = freq)

demos_summary$Percent <- percent(round(demos_summary$Percent,2))

demo_table <- htmlTable(
  x        = demos_summary,
  caption  = paste("Demographic Summary Table"),
  label    = "",
  rowlabel = "",
  rgroup   = c("Gender",
               "Race/Ethnicity",
               "Household Income",
               "2016 Presidential Vote",
               "Party ID Initial",
               "Ideology"),
  n.rgroup = c(3,
               6,
               4,
               6,
               4,
               4),
  ctable   = TRUE,
  type="html")

print(demo_table,useViewer = utils::browseURL)


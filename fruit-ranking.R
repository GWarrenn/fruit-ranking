library(tidyverse)
library(highcharter)
library(googlesheets)
library(ggplot2)
library(reshape2)
library(scales)
library(viridis)

#####################################################
##
## Connect to data via googlesheets
##
#####################################################

gs_ls()

fruit_doc <- gs_title("Fruit Ranking (Responses)")

gs_ws_ls(fruit_doc)

survey_data <- gs_read(ss = fruit_doc)

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

ggsave(plot = completes, "images\completes.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## clean data 
##
#####################################################

## get rid of question text in column/variable names

columns <- colnames(survey_data)

columns <- sub(x=columns,pattern = "Assign the following fruits to tiers \\(where A-tier is the best/highest quality and F-tier is reserved for the fruits that deserve to be banished\\)\\.  \\[(.*)\\]",
         replacement = "\\1")

colnames(survey_data) <- columns

fruits <- c("Raspberries","Strawberries","Bananas","Watermelon","Green Apples",
            "Blueberries","Canteloupe","Honeydew","Kiwi","Mango","Apricots",
            "Blackberries","Clementines","Cherries","Grapes","Oranges","Peaches",
            "Pears","Pineapple","Grapefruit","Red Apples")

survey_data$gender_recode <- ifelse(survey_data$`To which gender do you most closely identify?` == "Male","Male",
                                    ifelse(survey_data$`To which gender do you most closely identify?` == "Female","Female","Other"))

survey_data$race_recode <- factor(survey_data$`Which race/ethnicity best describes you? (Please choose only one.)`,
         levels = c("White/Caucasian","Black or African American","Hispanic or Latino","Asian/Pacific Islander",
                  "American Indian or Alaskan Native","Multiple ethnicity/Other"))

survey_data$income_recode <- factor(survey_data$`What was your total household income before taxes during the past 12 months?`,
                                    levels = c("Under $50,000","$50,000 to $100,000","Over $100,000","Not sure/Refuse"))

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
## Plot 1: Overall distribution - everyone oves fruits
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
  labs(title = "Overall Fruit Rankings",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  guides(fill=F) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12))

ggsave(plot = overall_bar_plot, "images\overall_bar_plot.png", w = 10.67, h = 8,type = "cairo-png")

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

ggsave(plot = fruit_heatmap_plot, "images\fruit_heatmap_plot.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Percent of fruits ranked F-tier
##
#####################################################

f_tier <- clean_l %>%
  group_by(value,variable) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n)) %>%
  filter(value == "F-tier")

f_tier$variable <- reorder(f_tier$variable, f_tier$freq)

ggplot(f_tier,aes(x=variable,y=freq,fill=variable)) +
  geom_bar(stat= "identity",color="black") +
  coord_flip() +
  geom_text(aes(x=variable,y=freq,label=percent(round(freq,2))),hjust = -.25) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = percent) +
  labs(title = "Distribution of Fruits Ranked F-tier",
       subtitle = paste("among a very non-random sample of",count,"people with opinions about fruit")) +
  guides(fill=F) +
  theme(axis.title = element_blank(),
        axis.text = element_text(size=12))

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
  scale_fill_gradientn(colours = c("red","white","green"), 
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

ggsave(plot = correlations_matrix, "images\correlations_matrix.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Plot 1: Fruit Rankings by Demos
##
#####################################################

clean_demos <- survey_data %>%
  select(fruits,`What is your age?`,
         gender_recode,
         race_recode,
         income_recode) %>%
  rename(age = "What is your age?")

clean_l_demos <- melt(clean_demos,id.vars = c("age","race_recode","gender_recode","income_recode"))

clean_l_demos$gpa <- ifelse(clean_l_demos$value == "A-tier",4,
                             ifelse(clean_l_demos$value == "B-tier",3,
                                    ifelse(clean_l_demos$value == "C-tier",2,
                                           ifelse(clean_l_demos$value == "D-tier",1,
                                                  ifelse(clean_l_demos$value == "F-tier",0,
                                                         NA)))))

demos <- c("age","gender_recode","income_recode","race_recode")
demo_label <- c("Age","Gender","Income","Race/Ethnicity")

num <- 1

for (d in demos) {
  
  group_var <- d[1]
  
  stats_demos <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var,variable) %>%
    summarise(avg_gpa = mean(gpa),
              n = n()) %>%
    filter(n >= 10)
  
  stats_demos$sort_gpa <- stats_demos$avg_gpa
  
  avg_all_fruits <- clean_l_demos %>%
    filter(!is.na(clean_l_demos$gpa)) %>%
    group_by(.dots = group_var) %>%
    summarise(avg_gpa = mean(gpa),
              n = n()/21) %>%
    filter(n >= 10)%>%
    select(group_var,avg_gpa)
  
  avg_all_fruits$variable <- "All Fruits on Average"
  
  avg_all_fruits$sort_gpa <- 0
  
  avg_all_fruits <- avg_all_fruits[,c(1,3,2)]
  
  stats_demos <- bind_rows(data.frame(stats_demos),data.frame(avg_all_fruits))
  
  stats_demos$variable <- reorder(stats_demos$variable,stats_demos$sort_gpa)
  stats_demos$avg_gpa <- round(stats_demos$avg_gpa,2)
  
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
  
  ggsave(plot = demo_plot, paste0("images\demo_",d,"_plot.png"), w = 10.67, h = 8,type = "cairo-png")
  
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
        total_n > 10) 

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


########################

fruits <- c("[Tomato]","[Cucumber]","[Avocado]","[Pumpkin]","[Bell Pepper]","[Coconut]","To which gender do you most closely identify?")

clean <- survey_data %>%
  select(fruits) %>%
  rename(gender = "To which gender do you most closely identify?")

clean$id <- "ix"

clean_l <- melt(clean,id.vars = "gender")

stats <- clean_l %>%
  group_by(gender,variable,value) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))


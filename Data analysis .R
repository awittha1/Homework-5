
library(acs)
library(tidyverse)
library(data.table)
library(fixest)

final.insurance <- fread("data/output/acs_insurance.txt")
final.data <- fread("data/output/acs_medicaid.txt")
#kff.final <- fread("data/output/medicaid_expansion.txt")


################################### Summary of the data 
#1 

final.data$perc_unins <- final.data$ins_direct/ final.data$adult_pop *100

graph1 <-final.data %>% group_by(year) %>% summarize(avg_share = mean(perc_unins))%>% 
 ggplot(aes(x = year, y = avg_share)) +
  geom_line() +
  labs(x = "Year", y = "Share of Insured Individuals with Direct Purchase Health Insurance Over Years") +
  theme_bw()

graph1 

#2 

#There have been several policy changes leading to the reduction in direct purchase health insurance recently. For example, the ACA expanding Medicaid coverage. Several states adopting medicaid expansion may decrease the share of insured individuals with direct purchase health insurance because they are eligible for medicaid. Thus, they will enroll in medicaid over direct purchase health insurance. Additionally, subsidies and tax credits from the ACA made insurance purchases through exchanges more affordable compared to direct purchase products. 

#3 

final.data$share_medicaid <- final.data$ins_medicaid/ final.data$adult_pop * 100


graph2 <-  final.data %>% group_by(year) %>% summarize(avg_medicaid = mean(share_medicaid))%>% 
ggplot(aes(x = year, y = avg_medicaid)) +
  geom_line() +
  labs(x = "Year", y = "Share of Individuals with Medicaid") +
  theme_bw()

graph2


#4 

data_q4 <- final.data %>% filter((expand_ever == FALSE | expand_year == 2014) & State != "South Dakota")


graph_dta <-  data_q4 %>% group_by(expand_year, year) %>% 
  summarize(avg = mean(perc_unins))



graph_dta$expand_year  <- if_else(is.na(graph_dta$expand_year), "Average of states that never expanded", "Average of states that expanded in 2014")


graph_dta


graph4 <- ggplot(graph_dta, aes(x = as.factor(year), y = avg, group = expand_year)) +
  geom_line(aes(color = expand_year)) +
  labs( color = "Average of states", x = "Year", y = "Share of Uninsured") +
  theme_bw()

graph4 


######################## Estimate ATEs 

# 1 

ate_1 <- data_q4 %>% filter(year %in% c(2012, 2015)) %>% 
  group_by(expand_ever, year) %>% 
  summarize(perc_unin = mean(perc_unins)) %>%
  spread(year, perc_unin)%>% 
  mutate(expand_ever = if_else(expand_ever == FALSE, "States that never expanded Medicaid", "States that expanded in 2014"))



ate_1


#2 

data_q4$treatment <- ifelse(data_q4$expand_ever == TRUE, 1,0)

data_q4$time <- ifelse(data_q4$year >= 2014, 1, 0)


ate_2 <- lm(perc_unins ~ treatment + time + treatment*time, data = data_q4)

summary(ate_2)


#3

ate_3 <- feols(perc_unins~i(year, expand_ever, ref= 2013) | State + year,
               cluster=~State,
               data=data_q4)

summary(ate_3)

# 4 

data <- final.data %>% 
  filter(!is.na(expand_ever) & State != "South Dakota") %>%
  mutate(time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year), 
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))


ate_4<- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=data)


summary(ate_4)


# 5 

coefplot(ate_3)



# 6 

coefplot(ate_4)













save.image("Hwk5_workspace.Rdata")



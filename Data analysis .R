
library(acs)
library(tidyverse)
library(data.table)
library(fixest)

final.insurance <- fread("data/output/acs_insurance.txt")
final.data <- fread("data/output/acs_medicaid.txt")
#kff.final <- fread("data/output/medicaid_expansion.txt")
final.data <- final.data %>% filter(!(State %in% c("Puerto Rico", "District of Columbia")))

################################### Summary of the data 
#1 

final.data$perc_direct <-(final.data$ins_direct/ final.data$adult_pop) *100
final.data$perc_unins <- (final.data$uninsured/ final.data$adult_pop)*100

graph1 <-final.data %>% group_by(year) %>% summarize(avg_share = mean(perc_direct))%>% 
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

graph_dta <- final.data %>% filter(expand_year == 2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarise(mean=mean(perc_unins)) 


graph4 <- ggplot(data = graph_dta, aes(x = year, y = mean, 
                                       group = expand_ever,
                                       linetype = expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = graph_dta %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(x = "Year", y = "Fraction Uninsured", title = "Share of Uninsured over Time") +
  theme(plot.title = element_text(hjust = 0.5)) 

graph4 


######################## Estimate ATEs 

# 1 

ate_1 <- final.data %>% filter(year %in% c(2012, 2015)) %>% 
  filter(!is.na(expand_ever))%>% 
  filter(expand_year == 2014 | is.na(expand_year)) %>% 
  group_by(expand_ever, year) %>% 
  summarize(perc_unins = mean(perc_unins, na.rm = TRUE)) %>%
  spread(year, perc_unins)%>% 
  mutate(expand_ever = if_else(expand_ever == FALSE, "Non-Expansion", "Expansion"))




ate_1


#2 

year_2014<- final.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever)


ate_2 <- feols(perc_unins ~ expand_ever + post + treat, data = year_2014)

summary(ate_2)


#3

ate_3 <- feols(perc_unins~i(year, expand_ever, ref= 2013) | State + year,
               cluster=~State,
               data=year_2014)

summary(ate_3)



ate_q3 <-  feols(perc_unins~expand_ever + post + treat | State + year,
                 cluster=~State,
                 data=year_2014)
# 4 

data <- final.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(time_to_treat = ifelse(expand_ever==TRUE,year-expand_year, -1), 
         time_to_treat = ifelse(time_to_treat <= -4, -4, time_to_treat))


ate_4<- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=data)

ate_q4 <- 
summary(ate_4)


# 5 

coefplot(ate_3)

iplot(ate_3, 
      xlab = 'Time to treatment',
      main = 'Event study')

# 6 

iplot(ate_4, 
      xlab = 'Time to treatment',
      main = 'Event study')


save.image("Hwk5_workspace.Rdata")



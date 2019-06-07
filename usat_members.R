library(tidyverse)
library(dplyr)
library(here)
library(janitor)
library(tidycensus)
library(cdlTools)

# import members by gender/age/state for 2018 only
gender_age_state <- read_csv(here("triathlon", "original_members_gender_age_state.csv")) %>%
  clean_names() %>% 
  remove_empty("rows") %>% 
  select(-x66, -x67, -fm, -x29, -gu, -x33)

#restructure and tidy gender/age/state for 2018 only
spread_gender_age_state <- gender_age_state %>%
  rename(age = 'x1') %>% 
  rename(no_state = "unkown") %>% 
  gather(key, value, -age) %>% 
  mutate(state = ifelse(str_detect(key, "^x"), NA, key), # ^x accounts for texas
         gender = ifelse(!str_detect(value, "F|M"), NA, value),
         count = as.numeric(value)) %>% #hacky to remove the M/F leftover in the count column
  fill(state, gender)  %>% 
  select(-key, -value) %>% 
  filter(!is.na(age)) %>% #there are a few empty rows at the bottom 
  spread(gender, count) %>% 
  clean_names()

# ratios by state
gender_ratios_by_state <- spread_gender_age_state %>% 
  group_by(state) %>% 
  summarize(fembystate = sum(f, na.rm = T),
            malesbystate = sum(m, na.rm = T)) %>% 
  mutate(ratio = malesbystate/fembystate)

spread_gender_age_state %>% 
  filter(state == "FL", )

# import members by age and gender per year
age_gender_year <- read_csv(here("triathlon", "original_annual_members_by_age_and_gender.csv")) %>% 
  clean_names()

# restructure and tidy members by age and gender per year
spread_age_gender_year <- age_gender_year %>% 
  rename(age = 'x1') %>% 
  gather(key, value, -age) %>% 
  mutate(age = as.numeric(age)) %>% # makes unknowns --> NAs
  mutate(key = str_replace(key, "x2014", "2014")) %>% 
  mutate(key = str_replace(key, "x2015", "2015")) %>% 
  mutate(key = str_replace(key, "x2016", "2016")) %>% 
  mutate(key = str_replace(key, "x2017", "2017")) %>% 
  mutate(key = str_replace(key, "x2018", "2018")) %>% 
  mutate(year = ifelse(str_detect(key, "^x"), NA, key),
         gender = ifelse(!str_detect(value, "F|M"), NA, value)) %>% 
  mutate(value = as.numeric(gsub(",", "", value))) %>% 
  fill(year, gender) %>% 
  select(-key) %>%  
  filter(!is.na(age) | !is.na(value)) %>% 
  spread(gender, value) %>% 
  clean_names() 

spread_age_gender_year %>% 
  filter(is.na(age))

gender_ratios_by_age <- spread_age_gender_year %>% 
  mutate(ratio = m/f) %>% 
  filter(year == "2018")

gender_ratios_by_age %>% 
  filter(year == "2018") %>% 
  ggplot(aes(age, ratio))+
  geom_point()+
  geom_smooth(method = "lm")

# count female members by year and age 
femaleagesperyear <- spread_age_gender_year %>% 
  group_by(year, age) %>% 
  summarize(femagesperyear = sum(f, na.rm = T)) %>% 
  filter(year == "2018")

# viz female members by year and age
femaleagesperyear %>%
  ggplot(aes(age, femagesperyear, color = year)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# count male members by year and age
maleagesperyear <- spread_age_gender_year %>% 
  group_by(year, age) %>% 
  summarize(maleagesperyear = sum(m, na.rm = T)) 

# viz male members by year and age 
maleagesperyear %>% 
  ggplot(aes(age, maleagesperyear, color = year)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# count all members by year and age 17 and older
agesperyear_17andolder <- spread_age_gender_year %>% 
  filter(year == "2018") %>% 
  filter(age > 17) %>% 
  group_by(year, age) %>% 
  summarize(agesperyear = sum(m, f, na.rm = T))

agesperyear <- spread_age_gender_year %>% 
  group_by(year, age) %>% 
  summarize(agesperyear = sum(m, f, na.rm = T))

write_csv(agesperyear, "agesperyear.csv")

# viz all members by year and age
agesperyear %>% 
  ggplot(aes(age, agesperyear, color = year)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# count females by year
femalesbyyear <- spread_age_gender_year %>% 
  group_by(year) %>% 
  summarize(femsumperyear = sum(f, na.rm = T)) 

# viz females by year
femalesbyyear %>% 
  ggplot(aes(year, femsumperyear, group = 1)) +
  geom_line()

# count males by year
malesbyyear <- spread_age_gender_year %>% 
  group_by(year) %>% 
  summarize(malesumperyear = sum(m, na.rm = T))

# viz males by year
malesbyyear %>% 
  ggplot(aes(year, malesumperyear, group = 1)) +
  geom_line()

# count all members by year
membersbyyear <- spread_age_gender_year %>% 
  group_by(year) %>% 
  summarize(membersperyear = sum(m, f, na.rm = T))

# viz all members by year
membersbyyear %>% 
  ggplot(aes(year, membersperyear, group = 1)) +
  geom_line()

# create age groups
age_groups_spread_age_gender_year <- spread_age_gender_year
attach(spread_age_gender_year)
spread_age_gender_year$age_group[age < 13] <- "12 and under"
spread_age_gender_year$age_group[age > 12 & age < 17] <- "13 - 16"
spread_age_gender_year$age_group[age > 16 & age < 20] <- "17-19"
spread_age_gender_year$age_group[age > 19 & age < 25] <- "20-24"
spread_age_gender_year$age_group[age > 24 & age < 30] <- "25-29"
spread_age_gender_year$age_group[age > 29 & age < 35] <- "30-34"
spread_age_gender_year$age_group[age > 34 & age < 40] <- "35-39"
spread_age_gender_year$age_group[age > 39 & age < 45] <- "40-44"
spread_age_gender_year$age_group[age > 44 & age < 50] <- "45-49"
spread_age_gender_year$age_group[age > 49 & age < 55] <- "50-54"
spread_age_gender_year$age_group[age > 54 & age < 60] <- "55-59"
spread_age_gender_year$age_group[age > 59 & age < 65] <- "60-64"
spread_age_gender_year$age_group[age > 64 & age < 70] <- "65-69"
spread_age_gender_year$age_group[age > 69 & age < 75] <- "70-74"
spread_age_gender_year$age_group[age > 74 & age < 80] <- "75-79"
spread_age_gender_year$age_group[age > 79 & age < 85] <- "80-84"
spread_age_gender_year$age_group[age > 84] <- "85 and up"
detach(spread_age_gender_year)

# count all members by year and age group
agegroupsperyear_17andolder <- spread_age_gender_year %>% 
  filter(year == "2018") %>%
  group_by(year, age_group) %>% 
  summarize(agegroupperyear = sum(m, f, na.rm = T)) %>% 
  filter(age_group != "12 and under",
         age_group != "13 - 16")

agegroupsperyear <- spread_age_gender_year %>%
  group_by(year, age_group) %>% 
  summarize(agegroupperyear = sum(m, f, na.rm = T))

# viz all members by year and age group
agegroupsperyear %>% 
  ggplot(aes(age_group, agegroupperyear, group = year, color = year)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# age groups in 2018
agegroupsperyear %>% 
  filter(year == "2018") %>%
  arrange(desc(agegroupperyear)) %>% 
  ggplot(aes(age_group, agegroupperyear, group = 1)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot men and women per year 
p <-  ggplot()+
  geom_line(data = femalesbyyear, aes(x = year, y = femsumperyear, group = 1), color = "red") +
  geom_line(data = malesbyyear, aes(x = year, y = malesumperyear, group = 1), color = "blue") +
  xlab('Year')+
  ylab('Number of Members')

gender_membersbyyear <- bind_rows(femalesbyyear, malesbyyear)

# import members by state
members_by_state <- read_csv(here("triathlon", "original_members_by_state.csv")) %>% 
  clean_names()

gather_members_by_state <- members_by_state %>% 
  rename(state = 'x1') %>% 
  gather(key, value, -state) %>% 
  mutate(key = str_replace(key, "x2014", "2014")) %>% 
  mutate(key = str_replace(key, "x2015", "2015")) %>% 
  mutate(key = str_replace(key, "x2016", "2016")) %>% 
  mutate(key = str_replace(key, "x2017", "2017")) %>% 
  mutate(key = str_replace(key, "x2018", "2018")) %>% 
  rename(count = "value") %>% 
  rename(year = "key")

membersbystate2018 <- gather_members_by_state %>% 
  filter(year == "2018") %>% 
  filter(state != "Total") %>% 
  arrange(desc(count))

gather_members_by_state %>% 
  filter(year == "2017") %>% 
  filter(state != "Total") %>% 
  arrange(desc(count))

gather_members_by_state %>% 
  filter(year == "2016") %>% 
  filter(state != "Total") %>% 
  arrange(desc(count))

gather_members_by_state %>% 
  filter(year == "2015") %>% 
  filter(state != "Total") %>% 
  arrange(desc(count))

gather_members_by_state %>% 
  filter(year == "2014") %>% 
  filter(state != "Total") %>% 
  arrange(desc(count))

gather_members_by_state %>% 
  filter(state == "CA" | 
         state == "TX" | 
         state == "FL" | 
         state == "NY" |
         state == "VA" |
         state == "GA" |
         state == "MA" |
         state == "IL" |
         state == "PA" |
         state == "NC") %>% 
  ggplot(aes(year, count, group = state, color = state)) +
  geom_line()

# triathletes per capita
options(tigris_use_cache = TRUE)
census_api_key(***REMOVED***, install = TRUE, overwrite=TRUE)

us_components <- get_estimates(geography = "state", product = "components")

acs5_vars<- load_variables(2017, "acs5", cache = TRUE)

pop <- get_acs(geography = "state", 
               variables = "B01003_001") %>% 
  select(GEOID, NAME, estimate) %>% 
  mutate(abb = fips(pop$GEOID, to = "Abbreviation"))

percapita <- pop %>% 
  right_join(gather_members_by_state, by = c("abb" = "state")) %>% 
  filter(!is.na(NAME)) %>% 
  filter(year == "2018") %>% 
  mutate(percapita = estimate/count)
  
percapita %>%   
  filter(is.na(NAME)) %>% 
  count(abb)

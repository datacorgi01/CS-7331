#Data Preparation 
#Create dataset of 20 features with California counties as observations
ca_vaccine <- read.csv("/Users/allisonking/Downloads/covid19vaccinesbycounty.csv", stringsAsFactors = T)
covid_imp_81 <- read.csv("/Users/allisonking/Downloads/covidcadata.csv", stringsAsFactors = T)
ca <- read.csv("/Users/allisonking/Downloads/cases_CA.csv", stringsAsFactors = T)

cases_CA1 <- covid_imp_81 %>% dplyr::filter(state == "CA")  %>% dplyr::rename(c(county = county_name))

cases_CA2 <- cases_CA1 %>% dplyr::select(c(county, total_pop, median_income,
                                           median_age, black_pop, white_pop, asian_pop, employed_pop, 
                                           unemployed_pop, bachelors_degree_or_higher_25_64, children, in_school, 
                                           pop_determined_poverty_status, less_than_high_school_graduate))

cases_CA2 <- cases_CA2[!duplicated(cases_CA2$county), ]

cali <- ca %>% dplyr::rename(c(county = county_name)) %>% dplyr::select(county, date, confirmed_cases, deaths)

cali <- cali[!(cali$county == "Statewide Unallocated"), ]

cali <- cali[(cali$date == "2023-01-16"), ]

cali$county <- as.factor(str_trim(cali$county))

cases_CA2$county <- as.factor(str_trim(cases_CA2$county))

newtable_CA <- cali %>% left_join(cases_CA2 %>% 
                                    dplyr::select(c(county, total_pop, median_income,
                                                    median_age, black_pop, white_pop, asian_pop, employed_pop, 
                                                    unemployed_pop, bachelors_degree_or_higher_25_64, children, in_school, 
                                                    pop_determined_poverty_status, less_than_high_school_graduate)))

ca_vaccine$county <- as.factor(paste(ca_vaccine$county, "County", sep=" "))

ca_vaccine2 <- ca_vaccine %>% 
  dplyr::select(c(county, total_doses, fully_vaccinated, partially_vaccinated, at_least_one_dose)) %>%
  dplyr::group_by(county) %>%
  dplyr::summarise(across(everything(), sum),
                   .groups = 'drop')  %>%
  as.data.frame()

levels(ca_vaccine2$county)[match("San Francisco County",levels(ca_vaccine2$county))] <- "City and County of San Francisco"

newtable_CA3 <- newtable_CA %>% left_join(ca_vaccine2)

#Final table for part 3 
#from covid - total_pop, median_age, black_pop, white_pop, asian_pop, median_income, employed_pop,
#unemployed_pop, bachelors_degree_or_higher, children, in_school, pop_determined_poverty_status, confirmed_cases, deaths, county,
#from vaccine - total_doses, fully_vaccinated, partially_vaccinated, at_least_one_dose
final_CA_table <- newtable_CA3 %>% dplyr::select(county, confirmed_cases, deaths, total_pop, median_income,
                                                 median_age, black_pop, white_pop, asian_pop, employed_pop, 
                                                 unemployed_pop, bachelors_degree_or_higher_25_64, children, in_school, 
                                                 pop_determined_poverty_status, total_doses, fully_vaccinated, 
                                                 partially_vaccinated, less_than_high_school_graduate)

#Normalize data for final_CA_table
ca4 <- final_CA_table %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases))

ca4 <- ca4 %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  bachelors_degree_or_higher_25_64_per_1000 = bachelors_degree_or_higher_25_64/total_pop*1000, 
  children_per_1000 = children/total_pop*1000, 
  in_school_per_1000 = in_school/total_pop*1000,
  fully_vaccinated_per_1000 = fully_vaccinated/total_pop*1000, 
  partially_vaccinated_per_1000 = partially_vaccinated/total_pop*1000,
  asian_pop_per_1000 = asian_pop/total_pop*1000,
  white_pop_per_1000 = white_pop/total_pop*1000,
  black_pop_per_1000 = black_pop/total_pop*1000,
  employed_pop_per_1000 = employed_pop/total_pop*1000,
  unemployed_pop_per_1000 = unemployed_pop/total_pop*1000,
  poverty_per_1000 = pop_determined_poverty_status/total_pop*1000,
  total_doses_per_1000 = total_doses/total_pop*1000,
  less_than_high_school_graduate_per_1000 = less_than_high_school_graduate/total_pop*1000,
  death_per_case = deaths/confirmed_cases)

#export files 
which(is.na(ca5), arr.ind=TRUE) #check for NA
write.csv(final_CA_table, file="/Users/allisonking/Desktop/ca5.csv",row.names = FALSE)

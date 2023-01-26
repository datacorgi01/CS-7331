#Libraries
library(ggplot2)
library(caret)
library(randomForest)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)

#Load in data
covid <- read.csv("/Users/allisonking/Downloads/COVID-19_cases_plus_census.csv", stringsAsFactors = T)
str(covid)
summary(covid)

#Visualize missing data
miss_var_summary(covid) %>% print(n=259)
names(which(colSums(is.na(covid)) > 0))

#Remove 100% missing columns 
covid <- covid %>% dplyr::select(-c(pop_5_years_over, speak_only_english_at_home, speak_spanish_at_home, speak_spanish_at_home_low_english,
                                    pop_15_and_over, pop_never_married, pop_now_married, pop_separated, pop_widowed, pop_divorced))

#Impute others using KNN
covid_imp <- kNN(covid, variable = "aggregate_travel_time_to_work")
covid_imp_2 <- kNN(covid_imp, variable = "owner_occupied_housing_units_lower_value_quartile")
covid_imp_3 <- kNN(covid_imp_2, variable = "owner_occupied_housing_units_upper_value_quartile")
covid_imp_4 <- kNN(covid_imp_3, variable = "median_rent")
covid_imp_5 <- kNN(covid_imp_4, variable = "owner_occupied_housing_units_median_value")
covid_imp_6 <- kNN(covid_imp_5, variable = "median_year_structure_built")
covid_imp_7 <- kNN(covid_imp_6, variable = "percent_income_spent_on_rent")
covid_imp_8 <- kNN(covid_imp_7, variable = "renter_occupied_housing_units_paying_cash_median_gross_rent")

#Check for duplicate rows - no dupes
covid[duplicated(covid)]

#Check for outliers with k-means clustering
#Calculate the distance between the objects and cluster centers to determine the outliers and identify 10 largest distances which are 
#outliers. Finally we will print the outliers.

#Removing factor columnns
covid_imp_8.1 <- covid_imp_8[, -c(2,3,5)]

set.seed(240) 
kmeans.result <- kmeans(covid_imp_8.1, centers = 3)
kmeans.result

centers <- kmeans.result$centers[kmeans.result$cluster, ] 
distances <- sqrt(rowSums((covid_imp_8.1 - centers)^2))
outliers <- order(distances, decreasing=T)[1:10]

print(outliers)
print(covid_imp_8.1[outliers,])

plot(covid_imp_8.1[,c("confirmed_cases", "deaths")], pch=19, col=kmeans.result$cluster, cex=1)
points(kmeans.result$centers[,c("confirmed_cases", "deaths")], col=1:3, pch=15, cex=2)
points(covid_imp_8.1[outliers, c("confirmed_cases", "deaths")], pch="+", col=4, cex=3)

#Most important variables - taking out county_name as randomForest will not work on factors with more than 53 levels
temp.covid.data <- covid_imp_8 %>% dplyr::select(-county_name)
set.seed(240)
regressor <- randomForest(confirmed_cases ~ . , data = temp.covid.data, importance=TRUE) 
varImp(regressor) 


#Visualize variable importance 
#Get variable importance from the model fit
ImpData <- as.data.frame(importance(regressor))
ImpData$Var.Names <- row.names(ImpData)

#Visualize most important variables when it comes to confirmed cases
data_1a <- subset(ImpData, ImpData$`%IncMSE` > 2.5)
data_1b <- subset(ImpData, ImpData$`%IncMSE` < -1.2)

data_1c <- rbind(data_1a, data_1b)

ggplot(data_1c, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + ggtitle("Most Important Variables (Ranked by Random Forest)")

#PCA - unsupervised alternative to finding important variables - may remove as we used random forest
covid.pca <- prcomp(covid_imp_8[,c(1,3,5:230)], center = TRUE,scale. = TRUE)
summary(covid.pca)

ggbiplot(covid.pca)


#Summary statistics of most important variables
imp.var.df <- subset(covid_imp_8, select = c(county_name, state, confirmed_cases, deaths, rent_35_to_40_percent, rent_25_to_30_percent, 
              rent_10_to_15_percent, rent_under_10_percent, total_pop, male_pop, black_pop, families_with_young_children, 
              father_one_parent_families_with_young_children, father_in_labor_force_one_parent_families_with_young_children, 
              commute_25_29_mins, commute_45_59_mins, male_under_5, male_5_to_9, male_10_to_14, male_18_to_19, male_21, male_22_to_24, 
              male_30_to_34, male_35_to_39, male_75_to_79, female_under_5, female_5_to_9, female_10_to_14, female_15_to_17, female_21, 
              female_75_to_79, female_80_to_84, female_85_and_over, black_including_hispanic, commute_35_39_mins, children, 
              employed_construction, employed_retail_trade, in_grades_1_to_4, in_grades_5_to_8, in_grades_9_to_12, in_school, 
              male_45_64_less_than_9_grade, male_45_64_grade_9_12, male_45_64_high_school, occupation_natural_resources_construction_maintenance, 
              occupation_sales_office, poverty, sales_office_employed, worked_at_home, less_one_year_college, one_year_more_college, 
              hispanic_any_race, county_fips_code, amerindian_pop, owner_occupied_housing_units_lower_value_quartile, 
              owner_occupied_housing_units_upper_value_quartile, dwellings_3_to_4_units, dwellings_10_to_19_units))

avgs <- apply(imp.var.df[,3:59], 2, mean)
ranges <- apply(imp.var.df[,3:59], 2, range)
medians <- apply(imp.var.df[,3:59], 2, median)
variances <- apply(imp.var.df[,3:59], 2, var)
stdevs <- apply(imp.var.df[,3:59], 2, sd)

#Writing function to find modes of every column
mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

modes <- apply(imp.var.df[,3:59], 2, mode)

#Visualize most important variables
#Look at correlation
cor_imp <- cor(imp.var.df[,-c(1,2)])
ggcorrplot(cor_imp, p.mat = cor_pmat(imp.var.df[,-c(1,2)]), insig = "blank", hc.order = TRUE)


#Confirmed cases by state
ggplot(data = aggregate(imp.var.df$confirmed_cases, list(imp.var.df$state), mean), aes(Group.1, x)) + geom_col() + 
  xlab("State") +  ylab("Confirmed Cases") + ggtitle("Confirmed COVID-19 Cases by State") + 
  geom_bar(stat = "identity", fill = "orange")

#Deaths by state
ggplot(data = aggregate(imp.var.df$deaths, list(imp.var.df$state), mean), aes(Group.1, x)) + geom_col() + 
  xlab("State") +  ylab("Deaths") + ggtitle("COVID-19 Deaths by State") + 
  geom_bar(stat = "identity", fill = "black")

#VT and MT had lowest cases - let's see what counties are doing well and look at those policies
#Vermont
cases_VT <- covid_imp_8 %>% filter(state == "VT")

cases_VT_select <- cases_VT %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  dplyr::select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_VT_select <- cases_VT_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

counties <- as_tibble(map_data("county"))

counties_VT <- counties %>% dplyr::filter(region == "vermont")  %>% dplyr::rename(c(county = subregion))

cases_VT <- cases_VT_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_VT <- counties_VT %>% left_join(cases_VT %>% 
                                           dplyr::select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

ggplot(counties_VT, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  geom_text_repel(data = counties_VT %>% filter(complete.cases(.)) %>% dplyr::group_by(county) %>%  
                    dplyr::summarize(long = mean(long), lat = mean(lat)) %>% dplyr::mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People in Vermont", subtitle = "Only counties reporting 100+ cases")

#Montana
cases_MT <- covid_imp_8 %>% filter(state == "MT")

cases_MT_select <- cases_MT %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  dplyr::select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_MT_select <- cases_MT_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

counties_MT <- counties %>% dplyr::filter(region == "montana")  %>% dplyr::rename(c(county = subregion))

cases_MT <- cases_MT_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_MT <- counties_MT %>% left_join(cases_MT %>% 
                                           dplyr::select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

ggplot(counties_MT, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  geom_text_repel(data = counties_MT %>% filter(complete.cases(.)) %>% dplyr::group_by(county) %>%  
                    dplyr::summarize(long = mean(long), lat = mean(lat)) %>% dplyr::mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People in Montana", subtitle = "Only counties reporting 100+ cases")

#Look at counties in CA, AZ, and MA
CA.dat <- subset(imp.var.df, imp.var.df$state == "CA")
CA.dat2 <- subset(CA.dat, CA.dat$confirmed_cases >= 100000)
ggplot(data = aggregate(CA.dat2$confirmed_cases, list(CA.dat2$county_name), mean), aes(Group.1, x)) + geom_col() + 
  xlab("County") +  ylab("Confirmed Cases") + ggtitle("Highest Confirmed COVID-19 Cases by California County") + 
  geom_bar(stat = "identity", fill = "pink")

AZ.dat <- subset(imp.var.df, imp.var.df$state == "AZ")
AZ.dat2 <- subset(AZ.dat, AZ.dat$confirmed_cases >= 30000)
ggplot(data = aggregate(AZ.dat$confirmed_cases, list(AZ.dat$county_name), mean), aes(Group.1, x)) + geom_col() + 
  xlab("County") +  ylab("Confirmed Cases") + ggtitle("Confirmed COVID-19 Cases by Arizona County") + 
  geom_bar(stat = "identity", fill = "purple")

MA.dat <- subset(imp.var.df, imp.var.df$state == "MA")
MA.dat2 <- subset(MA.dat, MA.dat$confirmed_cases >= 50000)
ggplot(data = aggregate(MA.dat$confirmed_cases, list(MA.dat$county_name), mean), aes(Group.1, x)) + geom_col() + 
  xlab("County") +  ylab("Confirmed Cases") + ggtitle("Confirmed COVID-19 Cases by Massachusetts County") + 
  geom_bar(stat = "identity", fill = "blue")

#Deaths and confirmed cases
ggplot(imp.var.df, mapping = aes(x = confirmed_cases, y = deaths, label = state)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = covid_imp_8$total_pop), color = "red") + 
  geom_text_repel(data = subset(imp.var.df, deaths > quantile(deaths, .95))) +
  ggtitle("Confirmed Cases vs Deaths with Total Population for Each State")

#Looking at if younger children have more cases
#Female dataset
DFfem <- imp.var.df %>% gather(key = AgeGroup, value = confirmed_cases, female_under_5:female_85_and_over)

ggplot(DFfem, mapping = aes(x = reorder(AgeGroup, -confirmed_cases), y = confirmed_cases)) + geom_bar(stat = "identity", fill = "pink") +
 ggtitle("Confirmed Cases by Age Group - Female")

#Male dataset
DFmale <- imp.var.df %>% gather(key = AgeGroup, value = confirmed_cases, male_under_5:male_75_to_79)

ggplot(DFmale, mapping = aes(x = reorder(AgeGroup, -confirmed_cases), y = confirmed_cases)) + geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Confirmed Cases by Age Group - Male")

#Looking at age/demographic with highest deaths

#Does education have negative correlation with confirmed cases? (i.e. blue collar jobs unable to WFH)

#Major metropolitan areas - Dallas, Houston, Austin. Who is doing a better job of flattening the curve in TX?


#What is the trend in different areas (states, counties) of the US?
#Is social distancing done and is it working?
#Can we identify regions that do particularly well?
#Can we predict the development in a region given the data of other regions?


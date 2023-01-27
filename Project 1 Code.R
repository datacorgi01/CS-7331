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
mobrep <- read.csv("/Users/allisonking/Downloads/Global_Mobility_Report.csv", stringsAsFactors = T)
tx <- read.csv("/Users/allisonking/Downloads/COVID-19_cases_TX.csv", stringsAsFactors = T)
str(covid)
summary(covid)

#Look at missing data
miss_var_summary(covid) %>% print(n=259)
names(which(colSums(is.na(covid)) > 0))

miss_var_summary(mobrep) %>% print(n=14)
names(which(colSums(is.na(mobrep)) > 0))

miss_var_summary(tx) %>% print(n=7)
names(which(colSums(is.na(tx)) > 0))

#Remove 100% missing columns 
covid <- covid %>% dplyr::select(-c(pop_5_years_over, speak_only_english_at_home, speak_spanish_at_home, speak_spanish_at_home_low_english,
                                    pop_15_and_over, pop_never_married, pop_now_married, pop_separated, pop_widowed, pop_divorced))

#Remove census_fips_code from mobility data as it has 78.6% miss and doesn't appear of use to us
mobrep <- mobrep %>% dplyr::select(-c(census_fips_code))


#Impute covid data using KNN
covid_imp <- kNN(covid, variable = "aggregate_travel_time_to_work")
covid_imp_2 <- kNN(covid_imp, variable = "owner_occupied_housing_units_lower_value_quartile")
covid_imp_3 <- kNN(covid_imp_2, variable = "owner_occupied_housing_units_upper_value_quartile")
covid_imp_4 <- kNN(covid_imp_3, variable = "median_rent")
covid_imp_5 <- kNN(covid_imp_4, variable = "owner_occupied_housing_units_median_value")
covid_imp_6 <- kNN(covid_imp_5, variable = "median_year_structure_built")
covid_imp_7 <- kNN(covid_imp_6, variable = "percent_income_spent_on_rent")
covid_imp_8 <- kNN(covid_imp_7, variable = "renter_occupied_housing_units_paying_cash_median_gross_rent")

#Remove rows with NA from mobrep dataset. KNN imputation won't work with such a large dataset b/c of memory issues
mobrep1 <- mobrep[complete.cases(mobrep), ]


#Check for duplicate rows - no dupes
covid[duplicated(covid)]
mobrep1[duplicated(mobrep1)]
tx[duplicated(tx)]

#Check for outliers with k-means clustering - covid dataset
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

#Check for outliers with k-means clustering - mobrep dataset
#Calculate the distance between the objects and cluster centers to determine the outliers and identify 10 largest distances which are 
#outliers. Finally we will print the outliers.

#Removing factor columnns
mobrep2 <- mobrep1[, -c(1:7)]

set.seed(240) 
kmeans.result1 <- kmeans(mobrep2, centers = 3)
kmeans.result1

centers1 <- kmeans.result1$centers[kmeans.result1$cluster, ] 
distances1 <- sqrt(rowSums((mobrep2 - centers1)^2))
outliers1 <- order(distances1, decreasing=T)[1:10]

print(outliers1)
print(mobrep2[outliers1,])

plot(mobrep2[,c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline")], pch=19, col=kmeans.result1$cluster, cex=1)
points(kmeans.result1$centers1[,c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline")], col=1:3, pch=15, cex=2)
points(mobrep2[outliers1, c("retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline")], pch="+", col=4, cex=3)

#Replicate above for tx dataset

#Most important variables for covid data - taking out county_name as randomForest will not work on factors with more than 53 levels
temp.covid.data <- covid_imp_8 %>% dplyr::select(-county_name)
set.seed(123)
regressor <- randomForest(confirmed_cases ~ . , data = temp.covid.data, importance=TRUE) 
varImp(regressor) 

#Visualize variable importance 
#Get variable importance from the model fit
ImpData <- as.data.frame(importance(regressor))
ImpData$Var.Names <- row.names(ImpData)

#Visualize most important variables when it comes to confirmed cases
data_1a <- subset(ImpData, ImpData$`%IncMSE` > 3.5)
data_1b <- subset(ImpData, ImpData$`%IncMSE` < -3)

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


#Summary statistics of most important variables for covid data
imp.var.df <- subset(covid_imp_8, select = c(county_name, state, confirmed_cases, deaths, families_with_young_children, 
              father_one_parent_families_with_young_children, male_under_5, male_5_to_9, male_10_to_14, male_15_to_17, male_30_to_34, 
              female_under_5, female_5_to_9, female_10_to_14, female_15_to_17, children, employed_construction, high_school_including_ged, 
              in_grades_1_to_4, in_grades_5_to_8, in_school, occupation_natural_resources_construction_maintenance, 
              occupation_sales_office, sales_office_employed, one_year_more_college))

avgs.covid <- apply(imp.var.df[,3:25], 2, mean)
ranges.covid <- apply(imp.var.df[,3:25], 2, range)
medians.covid <- apply(imp.var.df[,3:25], 2, median)
variances.covid <- apply(imp.var.df[,3:25], 2, var)
stdevs.covid <- apply(imp.var.df[,3:25], 2, sd)

#Writing function to find modes of every column
mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

modes.covid <- apply(imp.var.df[,3:25], 2, mode)

#Summary statistics for mobrep data 
avgs.mobrep <- apply(mobrep1[,8:13], 2, mean)
ranges.mobrep <- apply(mobrep1[,8:13], 2, range)
medians.mobrep <- apply(mobrep1[,8:13], 2, median)
variances.mobrep <- apply(mobrep1[,8:13], 2, var)
stdevs.mobrep <- apply(mobrep1[,8:13], 2, sd)
modes.mobrep <- apply(mobrep1[,8:13], 2, mode)

#Summary statistics for tx data 
avgs.tx <- apply(tx[,c(1,4,6,7)], 2, mean)
ranges.tx <- apply(tx[,c(1,4,6,7)], 2, range)
medians.tx <- apply(tx[,c(1,4,6,7)], 2, median)
variances.tx <- apply(tx[,c(1,4,6,7)], 2, var)
stdevs.tx <- apply(tx[,c(1,4,6,7)], 2, sd)
modes.tx <- apply(tx[,c(1,4,6,7)], 2, mode)

#Visualize most important variables
#Look at correlation - seems to be lots of multicollinearity?
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

#Deaths and confirmed cases - MAKE THIS ABOUT CA WITH THE COUNTIES
ggplot(imp.var.df, mapping = aes(x = confirmed_cases, y = deaths, label = state)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = covid_imp_8$total_pop), color = "red") + 
  geom_text_repel(data = subset(imp.var.df, deaths > quantile(deaths, .95))) +
  ggtitle("Confirmed Cases vs Deaths with Total Population for Each State")

#TX visuals - ADD IN HERE

#CA had highest cases - let's see what counties are bad and look at those policies
cases_CA <- covid_imp_8 %>% filter(state == "CA")

cases_CA_select <- cases_CA %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  dplyr::select(county_name, confirmed_cases, deaths, total_pop, median_income)

cases_CA_select <- cases_CA_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)

counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

cases_CA <- cases_CA_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_CA <- counties_CA %>% left_join(cases_CA %>% 
                                           dplyr::select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))

#Cases
ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  geom_text_repel(data = counties_CA %>% filter(complete.cases(.)) %>% dplyr::group_by(county) %>%  
                    dplyr::summarize(long = mean(long), lat = mean(lat)) %>% dplyr::mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People in California", subtitle = "Only counties reporting 100+ cases")

#Deaths
ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  geom_text_repel(data = counties_CA %>% filter(complete.cases(.)) %>% dplyr::group_by(county) %>%  
                    dplyr::summarize(long = mean(long), lat = mean(lat)) %>% dplyr::mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="lightblue", high="blue") +
  labs(title = "COVID-19 Deaths per 1000 People in California", subtitle = "Only counties reporting 100+ cases")

#Look at counties in CA
CA.dat <- subset(imp.var.df, imp.var.df$state == "CA")
CA.dat2 <- subset(CA.dat, CA.dat$confirmed_cases >= 100000)
ggplot(data = aggregate(CA.dat2$confirmed_cases, list(CA.dat2$county_name), mean), aes(Group.1, x)) + geom_col() + 
  xlab("County") +  ylab("Confirmed Cases") + ggtitle("Highest Confirmed COVID-19 Cases by California County") + 
  geom_bar(stat = "identity", fill = "pink")


#Looking at if younger children have more cases - MAKE THIS ABOUT CA
#Female dataset
DFfem <- imp.var.df %>% gather(key = AgeGroup, value = confirmed_cases, female_under_5:female_15_to_17)

ggplot(DFfem, mapping = aes(x = reorder(AgeGroup, -confirmed_cases), y = confirmed_cases)) + geom_bar(stat = "identity", fill = "pink") +
 ggtitle("Confirmed Cases by Age Group - Female")

#Male dataset
DFmale <- imp.var.df %>% gather(key = AgeGroup, value = confirmed_cases, male_under_5:male_30_to_34)

ggplot(DFmale, mapping = aes(x = reorder(AgeGroup, -confirmed_cases), y = confirmed_cases)) + geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Confirmed Cases by Age Group - Male")

#Over time level of cases - US
#Over time level of cases - CA
#Over time level of cases for particular counties
#Research policies surrounding particular dates for said counties

#Identifying hot spots for next pandemic and why (i.e. when children started going back to school, nursing homes)

#Does education have negative correlation with confirmed cases? (i.e. blue collar jobs unable to WFH)

#Major metropolitan areas - Dallas, Houston, Austin. Who is doing a better job of flattening the curve in TX?

#Rate of transmission/confirmed cases for populations with lower income and lower education (counties with greater num of lower levels of edu)

#Death rate in areas with lower income/more remote areas 

#Mask mandates?


#What is the trend in different areas (states, counties) of the US?
#Is social distancing done and is it working?
#Can we identify regions that do particularly well?
#Can we predict the development in a region given the data of other regions?


library("tidyverse")
library("ggplot2")
library("DT")
install.packages("fpc")
install.packages("dbscan")
library("fpc")
library("dbscan")

#Read in already prepared data from project 1
ca_raw <- read.csv('/Users/allisonking/Desktop/ca23.csv', stringsAsFactors = T)
str(ca_raw)

#Normalize data 
ca.raw1 <- ca_raw %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases))

ca.raw2 <- ca.raw1 %>% mutate(
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

#Extract only normalized variables
ca.raw3 <- ca.raw2[c("county","total_pop","median_income", "median_age", "cases_per_1000", "deaths_per_1000", 
                     "bachelors_degree_or_higher_25_64_per_1000", "children_per_1000", "in_school_per_1000", "fully_vaccinated_per_1000",
                     "partially_vaccinated_per_1000", "asian_pop_per_1000", "white_pop_per_1000", "black_pop_per_1000",
                     "employed_pop_per_1000", "unemployed_pop_per_1000", "poverty_per_1000", "total_doses_per_1000", 
                     "less_than_high_school_graduate_per_1000", "death_per_case")]

#Summary statistics for transformed data set
avgs.ca <- apply(ca.raw3[,2:20], 2, mean)
ranges.ca <- apply(ca.raw3[,2:20], 2, range)
medians.ca <- apply(ca.raw3[,2:20], 2, median)
variances.ca <- apply(ca.raw3[,2:20], 2, var)
stdevs.ca <- apply(ca.raw3[,2:20], 2, sd)

#Writing function to find modes of every column
mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

modes.ca <- apply(ca.raw3[,2:20], 2, mode)

#Summary statistics for untransformed data set
avgs.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, mean)
ranges.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, range)
medians.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, median)
variances.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, var)
stdevs.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, sd)
modes.ca1 <- apply(ca.raw2[,c(2:20,35)], 2, mode)

#Visualize data using map of California
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

cases_CA <- ca.raw3 %>% mutate(county = county %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_CA_cl <- counties_CA %>% left_join(cases_CA)

ggplot(counties_CA_cl, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = death_per_case)) +
  coord_quickmap() +
  scale_fill_continuous(type = "gradient") +
  labs(title = "Clusters of Deaths Per Case", subtitle = "Only Counties Reporting 100+ Cases", fill = "Deaths Per Case", 
       x = "Longitude", y = "Latitude") + 
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), axis.title=element_text(size=16), 
        legend.title = element_text(size=22), legend.text = element_text(size=18), plot.title = element_text(size=22))

#Standardize untransformed variables
cases_CA_scaled <- ca.raw1 %>% 
  select(
    median_income,
    median_age, 
    # total_pop, # you should use density
    black_pop,
    white_pop,
    asian_pop,
    employed_pop,
    unemployed_pop,
    bachelors_degree_or_higher_25_64,
    children,
    in_school,
    pop_determined_poverty_status, 
    total_doses,
    fully_vaccinated,
    partially_vaccinated,
    less_than_high_school_graduate
  ) %>% 
  scale() %>% as_tibble()

summary(cases_CA_scaled)

#K-means with 5 clusters
km <- kmeans(cases_CA_scaled, centers = 5, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled)

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

cases_CA <- ca.raw3 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

#Try with manhattan distance for outliers



#Hierarchical clustering
clusters1 <- hclust(dist(cases_CA_scaled))
plot(clusters1)

#Cut clusters off at 10
clusterCut <- cutree(clusters1, 10)

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters - K-means
cases_CA_km <- cases_CA %>% add_column(cluster = factor(km$cluster))

cases_CA_km %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))

#Check if cases and deaths are different in different clusters - Hierarchical
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))

#Check optimal number of clusters for hierarchical
fviz_nbclust(cases_CA_scaled, cluster::hcut, method='silhouette')


#Try out DBSCAN algorithm - find optimal epsilon
dbscan::kNNdistplot(cases_CA_scaled, k =  5)
abline(h = 0.15, lty = 2)

set.seed(123)
db <- fpc::dbscan(cases_CA_scaled, eps = 0.01, MinPts = 5)
fviz_cluster(db, cases_CA_scaled, stand = FALSE, geom = "point")



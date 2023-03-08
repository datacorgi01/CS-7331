#install.packages("fpc")
#install.packages("dbscan")
install.packages("seriation")
library(tidyverse)
library(ggplot2)
library(DT)
library(fpc)
library(dbscan)
library(cluster)
library(dbscan)
library(seriation)

#Read in already prepared data from project 1
ca_raw <- read.csv('/Users/allisonking/Desktop/ca5.csv', stringsAsFactors = T)
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

#Find outliers in dataset
#Removing factor column and calculating LOF
lof <- lof(ca.raw3[, -1], minPts= 10)
lof

ca.raw4 <- ca.raw3[, -1]

ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 3, color = "red", linetype = 2)

ggplot(ca.raw4 %>% add_column(outlier = lof >= 3), aes(fully_vaccinated_per_1000, in_school_per_1000, color = outlier)) +
  geom_point()

#LA is outlier - remove this in below code
ca.raw5 <- ca.raw4 %>% add_column(outlier = lof >= 3)

ca_scaled_clean <- ca.raw4  %>% filter(lof < 3)

km <- kmeans(ca_scaled_clean, centers = 4, nstart = 10)

ca_scaled_clean_km <- ca_scaled_clean %>%
  add_column(cluster = factor(km$cluster))

centroids <- as_tibble(km$centers, rownames = "cluster")

ggplot(ca_scaled_clean_km, aes(x = fully_vaccinated_per_1000, y = in_school_per_1000, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x = fully_vaccinated_per_1000, y = in_school_per_1000, color = cluster), shape = 3, size = 10)

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
  labs(title = "Deaths Per Case", subtitle = "Only Counties Reporting 100+ Cases", fill = "Deaths Per Case", 
       x = "Longitude", y = "Latitude") + 
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), axis.title=element_text(size=16), 
        legend.title = element_text(size=22), legend.text = element_text(size=18), plot.title = element_text(size=22))

#Use transformed variables
cases_CA_scaled <- ca_scaled_clean %>% 
  select(
    median_age,
    median_income,
    # total_pop, # you should use density
    black_pop_per_1000,
    white_pop_per_1000,
    asian_pop_per_1000,
    employed_pop_per_1000,
    unemployed_pop_per_1000,
    bachelors_degree_or_higher_25_64_per_1000,
    children_per_1000,
    in_school_per_1000,
    poverty_per_1000, 
    total_doses_per_1000,
    fully_vaccinated_per_1000,
    partially_vaccinated_per_1000,
    less_than_high_school_graduate_per_1000
  ) 

summary(cases_CA_scaled)

#Break into three subsets
cases_CA_scaled_edu <- ca_scaled_clean %>% 
  select(
    employed_pop_per_1000,
    unemployed_pop_per_1000,
    bachelors_degree_or_higher_25_64_per_1000,
    children_per_1000,
    in_school_per_1000,
    less_than_high_school_graduate_per_1000
  ) 

cases_CA_scaled_vac <- ca_scaled_clean %>% 
  select(
    total_doses_per_1000,
    fully_vaccinated_per_1000,
    partially_vaccinated_per_1000
  ) 

cases_CA_scaled_demo <- ca_scaled_clean %>% 
  select(
    median_age,
    median_income,
    black_pop_per_1000,
    white_pop_per_1000,
    asian_pop_per_1000,
    poverty_per_1000
  ) 

#PCA for PVE
caPCA <- prcomp(cases_CA_scaled, center=T, scale=T)
summary(caPCA)

#Inspect loadings
caPCA$rotation
caPCA$rotation[,1:3]

fviz_eig(caPCA, ncp=4)

#Biplot
fviz_pca_biplot(caPCA, repel = TRUE, col.var = "blue", 
                col.ind = "red")

set.seed(123)

#Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cases_CA_scaled, k, nstart = 10 )$tot.withinss
}

#Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#K-means with 4 clusters
km <- kmeans(cases_CA_scaled, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled)

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map - Total
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-1,]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance", subtitle = "Only counties reporting 100+ cases")

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d1))

#Broken up by the 3 subsets
#Education
d <- dist(cases_CA_scaled_edu, method = "euclidean")

km <- kmeans(cases_CA_scaled_edu, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled_edu, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled_edu)

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map - Education
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-1,]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance", subtitle = "Only counties reporting 100+ cases")

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_edu, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d1))

#Vaccine
km <- kmeans(cases_CA_scaled_vac, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled_vac, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled_vac)

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map - Vaccine
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-1,]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance", subtitle = "Only counties reporting 100+ cases")

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_vac, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d1))

#Demographics
km <- kmeans(cases_CA_scaled_demo, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled_demo, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled_demo)

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map - Demographics
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-1,]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance", subtitle = "Only counties reporting 100+ cases")

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_demo, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d1))

#K-means with 4 clusters with manhattan distance - Total
d1 <- dist(cases_CA_scaled, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

fviz_cluster(km1, data = cases_CA_scaled)

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

fviz_silhouette(silhouette(km1$cluster, d1))


#K-means with 4 clusters with manhattan distance - Education
d1 <- dist(cases_CA_scaled_edu, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

fviz_cluster(km1, data = cases_CA_scaled)

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

fviz_silhouette(silhouette(km1$cluster, d1))

#K-means with 4 clusters with manhattan distance - Vaccine
d1 <- dist(cases_CA_scaled_vac, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

fviz_cluster(km1, data = cases_CA_scaled)

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

fviz_silhouette(silhouette(km1$cluster, d1))

#K-means with 3 clusters with manhattan distance - Demographic
d1 <- dist(cases_CA_scaled_demo, method = "manhattan")

km1 <- kmeans(d1, centers = 3, nstart = 25)

fviz_cluster(km1, data = cases_CA_scaled)

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

fviz_silhouette(silhouette(km1$cluster, d1))




#Hierarchical clustering - complete linkage used automatically - Total
clusters1 <- hclust(dist(cases_CA_scaled))
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)


fviz_cluster(list(data = cases_CA_scaled, cluster = cutree(clusters1, k = 4)), geom = "point")

#Cut clusters off at 4
clusterCut <- cutree(clusters1, 4)

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage", subtitle = "Only counties reporting 100+ cases")


#Hierarchical clustering - complete linkage used automatically - Education
clusters1 <- hclust(dist(cases_CA_scaled_edu))
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled_edu, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)


fviz_cluster(list(data = cases_CA_scaled_edu, cluster = cutree(clusters1, k = 4)), geom = "point")

#Cut clusters off at 4
clusterCut <- cutree(clusters1, 4)

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage", subtitle = "Only counties reporting 100+ cases")









#Hierarchical clustering - single linkage
clusters2 <- hclust(dist(cases_CA_scaled), method = "single")
plot(clusters2)

#Cut clusters off at X
clusterCut1 <- cutree(clusters2, 30)

#Graph the hierarchical clusters
counties_CA_clust2 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut1)))

ggplot(counties_CA_clust2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Single Linkage", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters - K-means
cases_CA_km <- cases_CA %>% add_column(cluster = factor(km$cluster))

cases_CA_km %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))

#Check if cases and deaths are different in different clusters - Hierarchical (complete linkage)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



#Agglomerative clustering usign euclidean distance
#Dissimilarity matrix
d <- dist(cases_CA_scaled, method = "euclidean")

#Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

#Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


#Agglomerative clustering using manhattan distance
#Dissimilarity matrix
d1 <- dist(cases_CA_scaled, method = "manhattan")

#Hierarchical clustering using Complete Linkage
hc2 <- hclust(d1, method = "complete" )

#Plot the obtained dendrogram
plot(hc2, cex = 0.6, hang = -1)


#Which clustering hierarchical method is the strongest
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


#Function to compute coefficient
ac <- function(x) {
  agnes(cases_CA_scaled, method = x)$ac
}

map_dbl(m, ac)

#Agglomerative clustering using ward's
#Dissimilarity matrix
hc3 <- agnes(cases_CA_scaled, method = "ward")

#Plot the obtained dendrogram
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")




k <- clusGap(cases_CA_scaled, FUN = hcut,  nstart = 10, K.max = 10)
plot(k)

pimage(d2, order=order(hc3$cluster), col = bluered(100))

#Try out DBSCAN algorithm - find optimal epsilon
kNNdistplot(cases_CA_scaled, k = 5)
abline(h = 5, col = "red")

db <- dbscan(cases_CA_scaled, eps = 5, minPts = 4)
db

fviz_cluster(db, data = cases_CA_scaled)



#install.packages("fpc")
#install.packages("dbscan")
#install.packages("seriation")
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
  med_inc_in_h = median_income/100,
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
ca.raw3 <- ca.raw2[c("county","total_pop","med_inc_in_h", "median_age", "cases_per_1000", "deaths_per_1000", 
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
  geom_hline(yintercept = 2.4, color = "red", linetype = 2)

ggplot(ca.raw4 %>% add_column(outlier = lof >= 2.4), aes(fully_vaccinated_per_1000, in_school_per_1000, color = outlier)) +
  geom_point()

#LA, San Diego, and Orange County are outliers - remove these in below code
ca.raw5 <- ca.raw4 %>% add_column(outlier = lof >= 2.4)

ca_scaled_clean <- ca.raw4  %>% filter(lof < 2.4)

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
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "gradient", low = "yellow", high = "red") +
  labs(title = "COVID-19 Cases Per 1000", subtitle = "Only Counties Reporting 100+ Cases", fill = "Cases Per 1000", 
       x = "Longitude", y = "Latitude") + 
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), axis.title=element_text(size=16), 
        legend.title = element_text(size=22), legend.text = element_text(size=18), plot.title = element_text(size=22))

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
    med_inc_in_h,
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
    children_per_1000,
    median_age,
    med_inc_in_h,
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
                col.ind = "red", title = "California COVID-19 Data Biplot (PCA)")

set.seed(123)

#Broken up by the 3 subsets
#Education
#Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cases_CA_scaled_edu, k, nstart = 10 )$tot.withinss
}

#Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares",
     main="Finding The Optimal Amount of Clusters Using Within-Cluster Sum of Squares (Education Subset)",
     cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5)

d <- dist(cases_CA_scaled_edu, method = "euclidean")

km <- kmeans(cases_CA_scaled_edu, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled_edu, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled_edu,  main='Cluster Plot - K-Means Using Euclidean Distance (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) + geom_bar(stat = "identity") + facet_grid(rows = vars(cluster)) + 
       labs(fill = "Cluster", x = "Value", y = "Variable Names") + 
       ggtitle("Cluster Profiles for Education-Related Variables") + 
       theme(axis.title=element_text(size=18), plot.title = element_text(size=18), legend.title = element_text(size=18), 
             legend.text = element_text(size=16), axis.text = element_text(size=13))

#Look at clusters on a map - Education
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-c(1,2,5),]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance", subtitle = "Only counties reporting 100+ cases (Education-Related Variables)", 
       fill = "Clusters", x = "Longitude", y = "Latitude") + 
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), axis.title=element_text(size=16), 
        legend.title = element_text(size=22), legend.text = element_text(size=18), plot.title = element_text(size=22))
       
       
#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_edu, method = "euclidean")

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d), main='Clusters Silhouette Plot with Average Silouette Width: 0.42 (Education-Related Variables)') +
  theme(axis.text.y = element_text(size = 15), title = element_text(size = 15))

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Vaccine
#Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cases_CA_scaled_vac, k, nstart = 10 )$tot.withinss
}

#Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares",
     main="Finding The Optimal Amount of Clusters Using Within-Cluster Sum of Squares (Vaccine Subset)",
     cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5)

km <- kmeans(cases_CA_scaled_vac, centers = 4, nstart = 25)
km

#Look at silhouette coefficient
fviz_nbclust(cases_CA_scaled_vac, kmeans, method='silhouette')

fviz_cluster(km, data = cases_CA_scaled_vac, main='Cluster Plot - K-Means Using Euclidean Distance (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look at profiles
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value, fill = cluster)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

#Look at clusters on a map - Vaccine
counties <- as_tibble(map_data("county"))

counties_CA <- counties %>% dplyr::filter(region == "california")  %>% dplyr::rename(c(county = subregion))

ca.raw5 <- ca.raw3[-c(1,2,5),]

cases_CA <- ca.raw5 %>% mutate(county = county %>% str_to_lower() %>% 
                                 str_replace('\\s+county\\s*$', ''))

counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_CA_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "K-Means Clusters with Euclidean Distance (Vaccinne-Related Variables)", subtitle = "Only counties reporting 100+ cases", 
       fill = "Cluster", x = "Longitude", y = "Latitude") + 
  theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12), axis.title=element_text(size=16), 
        legend.title = element_text(size=22), legend.text = element_text(size=18), plot.title = element_text(size=22))

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_vac, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d), main='Clusters Silhouette Plot with Average Silouette Width: 0.57 (Vaccine-Related Variables)') +
  theme(axis.text.y = element_text(size = 15), title = element_text(size = 15))

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Demographics
#Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cases_CA_scaled_demo, k, nstart = 10 )$tot.withinss
}

#Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares",
     main="Finding The Optimal Amount of Clusters Using Within-Cluster Sum of Squares",
     cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5)

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
  labs(title = "K-Means Clusters with Euclidean Distance (Demographic-Related Variables)", subtitle = "Only counties reporting 100+ cases")

#Look at measures of separation and measures of cohesion
d <- dist(cases_CA_scaled_demo, method = "euclidean")
km <- kmeans(d, centers = 4, nstart = 25)

fpc::cluster.stats(d, km$cluster)

fviz_silhouette(silhouette(km$cluster, d), main='Clusters Silhouette Plot with Average Silouette Width: 0.44 (Demographic-Related Variables)') +
  theme(axis.text.y = element_text(size = 15), title = element_text(size = 15))

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))





#Education - Manhattan K-means
d1 <- dist(cases_CA_scaled_edu, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

#Look at clusters
fviz_cluster(km1, data = cases_CA_scaled_edu,  main='Cluster Plot - K-Means Using Manhattan Distance (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km1$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Vaccine - Manhattan K-means
d1 <- dist(cases_CA_scaled_vac, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

#Look at clusters
fviz_cluster(km1, data = cases_CA_scaled_vac,  main='Cluster Plot - K-Means Using Manhattan Distance (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km1$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Demographics
d1 <- dist(cases_CA_scaled_demo, method = "manhattan")

km1 <- kmeans(d1, centers = 4, nstart = 25)

#Look at clusters
fviz_cluster(km1, data = cases_CA_scaled_demo,  main='Cluster Plot - K-Means Using Manhattan Distance (Demographic-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look at measures of separation and measures of cohesion
fpc::cluster.stats(d1, km1$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(km1$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))




#Hierarchical clustering - complete linkage used automatically - Education
clusters1 <- hclust(dist(cases_CA_scaled_edu))
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled_edu, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_edu, cluster = cutree(clusters1, k = 9)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Complete Linkage (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 5
clusterCut <- cutree(clusters1, 9)

#Look for internal validation 
d <- dist(cases_CA_scaled_edu)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100), main = "Matrix Shading for Clusters (Education)")
dissplot(d, labels = cutree(clusters1, k = 9), col = bluered(100), main = "Dissimilarity Matrix Between Clusters (Education)")

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))
#FIX BELOW GRAPH - CLUSTER 4 IS LEFT OUT
ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Education)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



#Hierarchical clustering - complete linkage used automatically - Vaccine
clusters1 <- hclust(dist(cases_CA_scaled_vac))
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled_vac, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_vac, cluster = cutree(clusters1, k = 15)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Complete Linkage (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 15
clusterCut <- cutree(clusters1, 15)

#Look for internal validation 
d <- dist(cases_CA_scaled_vac)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100), main = "Matrix Shading for Clusters (Vaccine)")
dissplot(d, labels = cutree(clusters1, k = 9), col = bluered(100), main = "Dissimilarity Matrix Between Clusters (Vaccine)")

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Vaccine)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



#Hierarchical clustering - complete linkage used automatically - Demographic
clusters1 <- hclust(dist(cases_CA_scaled_demo))
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled_demo, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_demo, cluster = cutree(clusters1, k = 5)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Complete Linkage (Demographic-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 5
clusterCut <- cutree(clusters1, 5)

#Look for internal validation 
d <- dist(cases_CA_scaled_demo)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100), main = "Matrix Shading for Clusters (Demographic)")
dissplot(d, labels = cutree(clusters1, k = 9), col = bluered(100), main = "Dissimilarity Matrix Between Clusters (Demographic)")

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Demographic)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))





#Hierarchical clustering - wards - Education
#Which clustering hierarchical method is the strongest
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#Function to compute coefficient
ac <- function(x) {
  agnes(cases_CA_scaled_edu, method = x)$ac
}

map_dbl(m, ac)

#Hierarchical clustering using Wards
clusters1 <- hclust(dist(cases_CA_scaled_edu), method="ward.D")

gap.stat <- clusGap(cases_CA_scaled_edu, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_edu, cluster = cutree(clusters1, k = 5)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Ward\'s (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 4
clusterCut <- cutree(clusters1, 4)

#Look for internal validation 
d <- dist(cases_CA_scaled_edu)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100))
dissplot(d, labels = cutree(clusters1, k = 9), col = bluered(100))

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))
#FIX BELOW GRAPH - CLUSTER 4 IS LEFT OUT
ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Education)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



#Hierarchical clustering - wards - Vaccine
#Which clustering hierarchical method is the strongest
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#Function to compute coefficient
ac <- function(x) {
  agnes(cases_CA_scaled_vac, method = x)$ac
}

map_dbl(m, ac)

#Hierarchical clustering using Wards
clusters1 <- hclust(dist(cases_CA_scaled_vac), method="ward.D")

gap.stat <- clusGap(cases_CA_scaled_vac, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_vac, cluster = cutree(clusters1, k = 5)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Ward\'s (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 5
clusterCut <- cutree(clusters1, 5)

#Look for internal validation 
d <- dist(cases_CA_scaled_vac)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100))
dissplot(d, labels = cutree(clusters1, k = 7), col = bluered(100))

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Vaccine)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



#Hierarchical clustering - wards - Demographic
#Which clustering hierarchical method is the strongest
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#Function to compute coefficient
ac <- function(x) {
  agnes(cases_CA_scaled_demo, method = x)$ac
}

map_dbl(m, ac)

clusters1 <- hclust(dist(cases_CA_scaled_demo), method="ward.D")
plot(clusters1)

gap.stat <- clusGap(cases_CA_scaled_demo, FUN = hcut, K.max = 15)
fviz_gap_stat(gap.stat)

fviz_cluster(list(data = cases_CA_scaled_demo, cluster = cutree(clusters1, k = 5)), geom = "point", 
             main='Cluster Plot - Hierarchical Using Ward\'s (Demographic-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Cut clusters off at 5
clusterCut <- cutree(clusters1, 5)

#Look for internal validation 
d <- dist(cases_CA_scaled_demo)
fpc::cluster.stats(d, clusterCut)

#Show average dissimilarities between clusters
pimage(d, order=order(clusterCut), col = bluered(100))
dissplot(d, labels = cutree(clusters1, k = 5), col = bluered(100))

#Graph the hierarchical clusters
counties_CA_clust1 <- counties_CA %>% left_join(cases_CA %>% 
                                                  add_column(cluster = factor(clusterCut)))

ggplot(counties_CA_clust1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters Using Complete Linkage (Demographic)", subtitle = "Only counties reporting 100+ cases")

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(clusterCut))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))




#Try out DBSCAN algorithm - Education
kNNdistplot(cases_CA_scaled_edu, k = 5)
abline(h = 100, col = "red")

db <- dbscan(cases_CA_scaled_edu, eps = 100, minPts = 4)
db

fviz_cluster(db, data = cases_CA_scaled_edu, main='Cluster Plot - DBSCAN (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
d <- dist(cases_CA_scaled_edu)
fpc::cluster.stats(d, db$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(db$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Try out DBSCAN algorithm - Vaccine
kNNdistplot(cases_CA_scaled_vac, k = 5)
abline(h = 300, col = "red")

db <- dbscan(cases_CA_scaled_vac, eps = 300, minPts = 4)
db

fviz_cluster(db, data = cases_CA_scaled_vac, main='Cluster Plot - DBSCAN (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
d <- dist(cases_CA_scaled_vac)
fpc::cluster.stats(d, db$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(db$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#Try out DBSCAN algorithm - Demographics
kNNdistplot(cases_CA_scaled_demo, k = 5)
abline(h = 125, col = "red")

db <- dbscan(cases_CA_scaled_demo, eps = 125, minPts = 4)
db

fviz_cluster(db, data = cases_CA_scaled_demo, main='Cluster Plot - DBSCAN (Demographic-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
d <- dist(cases_CA_scaled_demo)
fpc::cluster.stats(d, db$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(db$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))




#PAM clustering - Education
d <- dist(cases_CA_scaled_edu)

gap.stat <- clusGap(cases_CA_scaled_edu, FUN = pam, K.max = 15)
fviz_gap_stat(gap.stat)

p <- pam(d, k = 4)

ca_clustered <- cases_CA_scaled_edu %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(cases_CA_scaled_edu[p$medoids, ], rownames = "cluster")
medoids

fviz_cluster(c(p, list(data = cases_CA_scaled_edu)), geom = "point", ellipse.type = "norm", 
             main='Cluster Plot - PAM (Education-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
fpc::cluster.stats(d, p$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(p$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#PAM clustering - Vaccine
d <- dist(cases_CA_scaled_vac)

gap.stat <- clusGap(cases_CA_scaled_vac, FUN = pam, K.max = 15)
fviz_gap_stat(gap.stat)

p <- pam(d, k = 7)

ca_clustered <- cases_CA_scaled_vac %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(cases_CA_scaled_vac[p$medoids, ], rownames = "cluster")
medoids

fviz_cluster(c(p, list(data = cases_CA_scaled_vac)), geom = "point", ellipse.type = "norm", 
             main='Cluster Plot - PAM (Vaccine-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
fpc::cluster.stats(d, p$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(p$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))


#PAM clustering - Demographics
d <- dist(cases_CA_scaled_demo)

gap.stat <- clusGap(cases_CA_scaled_demo, FUN = pam, K.max = 15)
fviz_gap_stat(gap.stat)

p <- pam(d, k = 5)

ca_clustered <- cases_CA_scaled_demo %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(cases_CA_scaled_demo[p$medoids, ], rownames = "cluster")
medoids

fviz_cluster(c(p, list(data = cases_CA_scaled_demo)), geom = "point", ellipse.type = "norm", 
             main='Cluster Plot - PAM (Demographic-Related Variables)') +
  theme(axis.text.x = element_text(size = 15), title = element_text(size = 15))

#Look for internal validation 
fpc::cluster.stats(d, p$cluster)

#Check if cases and deaths are different in different clusters (Ground Truth)
cases_CA_h <- cases_CA %>% add_column(cluster = factor(p$cluster))

cases_CA_h %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000))



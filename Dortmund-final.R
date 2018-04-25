library(reshape2)
library(ggplot2)
library(RColorBrewer)

dortmund <- read.table("Dortmund_G3019ica.dat",header=TRUE)
cityid <- read.table("Dortmund_names_numbers.dat",header=TRUE)

set.seed(12345)


##### Exploratory Analysis #####
# Quick validation: sum of male and female should match sum of all age groups. Check if this is true for all cities
age_sum <- rowSums(dortmund[c("age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65")])
gender_sum <- rowSums(dortmund[c("male", "female")])
sum(age_sum == gender_sum)
# Sums to 170 which is total number of cities. Validation passed

# Unemployed, households, social insurance and benefits should be fewer than sum of males & females
sum(dortmund["unemployed"] < gender_sum)
sum(dortmund["households"] < gender_sum)
sum(dortmund["social_insurance"] < gender_sum)
sum(dortmund["benefits"] < gender_sum)
# Sums to 170 which is total number of cities. Validation passed


# Extract columns related to age only and produce a plot
ages <- dortmund[c("age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65")]
# Normalise age group size by total population
ages <- ages/rowSums(ages)
# Plot age group distribution for all cities
ages <- melt(ages)
ages$rowid <- cityid$MapNumber
ggplot(ages, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)))


# Extract columns related to buildings only and produce a plot
buildings <- dortmund[c("buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")]
# Normalise building group size by total number of buildings
buildings <- buildings/rowSums(buildings)
# Plot buildings group distribution for all cities
buildings <- melt(buildings)
buildings$rowid <- cityid$MapNumber
ggplot(buildings, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)))


# Plot of male vs. female
plot(dortmund[c("male", "female")])

# Plot of births vs. deaths
plot(dortmund[c("births", "deaths")])

# Plot of move ins vs. move outs
plot(dortmund[c("moves_in", "moves_out")])

# Plot of variables apart from buildings & age
drops <- c("age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65", "buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")
plot(dortmund[, !(names(dortmund) %in% drops)])



##### K-Means #####
library(fpc) # library for clusym function
library(cluster)

# Normalise dataset
sdortmund <- scale(dortmund)

# Gap Statistic for K-Means
cg1 <- clusGap(sdortmund, kmeans, 10, B=30, d.power=21, spaceH0='original', nstart=100)
print(cg1,method="Tibs2001SEmax")
plot(cg1)
# It appears that 6 is the optimal # of clusters
# Values of gap
plot(1:10,exp(cg1$Tab[,1]),xlab="k",ylab="S_k",type="l")
# Values of S_k; cg1$Tab[,1] has values of log(S_k), therefore the exp.
plot(1:10,cg1$Tab[,1],xlab="k",ylab="log S_k",type="l")
points(1:10,cg1$Tab[,2],xlab="k",ylab="log S_k",type="l",lty=2)
legend(6,6.5,c("log S_k in data","E(log S_k) uniform"),lty=1:2)

# K-means clustering
dortmund_kmeans <- kmeans(sdortmund,6,nstart=10)

# Visaulise clustering with MDS
dortmund_euc <- dist(sdortmund, method='euclidean')
mdsdortmund <- cmdscale(dortmund_euc,k=2)
plot(mdsdortmund,pch=clusym[dortmund_kmeans$cluster],col=dortmund_kmeans$cluster)

# Visualise clustering with variables
sdortmund <- data.frame(sdortmund)
drops <- c("male", "female", "cars", "trucks", "motorbikes", "age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65", "buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")
dortmund_remaining_var <- sdortmund[, -which(names(sdortmund) %in% drops)]
plot(dortmund_remaining_var,col=tclusk[[6]],pch=clusym[tclusk[[6]]])

# Compute the Average Silhouette Width (ASW) for K-Means
euc <- dist(sdortmund, method='euclidean') # Compute Euclidean distance for standardised Dortmund data
# Compute silhouette
tsil <- silhouette(dortmund_kmeans$cluster,dist=euc)
# Calculate ASW
tasw <- summary(silhouette(dortmund_kmeans$cluster,dist=euc))$avg.width
tasw




##### Hierarchical clustering #####
dortmund2 <- dortmund
# Normalise age distribution
ages <- dortmund2[c("age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65")]
dortmund2$population <- rowSums(ages) # Add population variable: total population in the city
ages <- ages/rowSums(ages)
dortmund2[c("age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65")] <- ages

# Normliase gender distribution
gender <- dortmund2[c("male", "female")]
gender <- gender/rowSums(gender)
dortmund2[c("male", "female")] <- gender

# Normliase building distribution
buildings <- dortmund2[c("buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")]
dortmund2$buildings <- rowSums(buildings) # Add buildings variable: total # of buildings in the city
buildings <- buildings/rowSums(buildings)
dortmund2[c("buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")] <- buildings

# Normalise vehicles distribution
vehicles <- dortmund2[c("cars", "trucks", "motorbikes")]
dortmund2$vehicles <- rowSums(vehicles) # Add vehicles variable: total # of vehicles in the city
vehicles <- vehicles/rowSums(vehicles)
dortmund2[c("cars", "trucks", "motorbikes")] <- vehicles

# Convert all variables apart from age, gender, building and vehicles related variables as a proportion of the total population
drops <- c("male", "female", "cars", "trucks", "motorbikes", "age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65", "buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001", "population")
dortmund2[, -which(names(dortmund2) %in% drops)] <- dortmund2[, -which(names(dortmund2) %in% drops)]/dortmund2$population

# Standardise data
dortmund2 <- data.frame(scale(dortmund2))


# Modified Jaccard distance for age, buildings, gender & vehicle distribution variables
ages_jac <- matrix(0,ncol=170,nrow=170)
for (i in 1:170) {
  for (j in 1:170) {
    inter <- 0
    union <- 0
    for (k in 1:6) {
      inter <- inter + min(ages[i,k], ages[j,k])
      union <- union + max(ages[i,k], ages[j,k])
    }
    ages_jac[i,j] <- 1 - inter/union
  }
}
ages_jac <- as.dist(ages_jac)

buildings_jac <- matrix(0,ncol=170,nrow=170)
for (i in 1:170) {
  for (j in 1:170) {
    inter <- 0
    union <- 0
    for (k in 1:9) {
      inter <- inter + min(buildings[i,k], buildings[j,k])
      union <- union + max(buildings[i,k], buildings[j,k])
    }
    buildings_jac[i,j] <- 1 - inter/union
  }
}
buildings_jac <- as.dist(buildings_jac)

gender_jac <- matrix(0,ncol=170,nrow=170)
for (i in 1:170) {
  for (j in 1:170) {
    inter <- 0
    union <- 0
    for (k in 1:2) {
      inter <- inter + min(gender[i,k], gender[j,k])
      union <- union + max(gender[i,k], gender[j,k])
    }
    gender_jac[i,j] <- 1 - inter/union
  }
}
gender_jac <- as.dist(gender_jac)

vehicle_jac <- matrix(0,ncol=170,nrow=170)
for (i in 1:170) {
  for (j in 1:170) {
    inter <- 0
    union <- 0
    for (k in 1:3) {
      inter <- inter + min(vehicles[i,k], vehicles[j,k])
      union <- union + max(vehicles[i,k], vehicles[j,k])
    }
    vehicle_jac[i,j] <- 1 - inter/union
  }
}
vehicle_jac <- as.dist(vehicle_jac)


# Obtain remaining variables
drops <- c("male", "female", "cars", "trucks", "motorbikes", "age_under_26", "age_26.35", "age_36.45", "age_46.55", "age_56.65", "age_above_65", "buildings_until_1900", "buildings_1900.1918", "buildings_1919.1948", "buildings_1949.1957", "buildings_1958.1962", "buildings_1963.1972", "buildings_1973.1982", "buildings_1983.1992", "buildings_1993.2001")
dortmund2_remaining_var <- dortmund2[, -which(names(dortmund2) %in% drops)]


dortmund2_euc <- dist(dortmund2_remaining_var, method='euclidean')

# Compute aggregated distance: Euclidean distance for remaining variables (13 in total: originally 30 variables, 6 age distribution variables removed, 2 gender distribution variables removed, 9 buildings distribution variables removed, 3 vehicle distribution variables removed, add 3 count variables 'buildings', 'population' and 'vehicles'), standardise to [0,1]-range, multiply by 13 and then add the modified jaccard distances for distributions to it (i.e. treating each of age/building/gender/vehicle as one variable only)
dortmund2_dist <- as.dist(as.matrix(dortmund2_euc)/max(as.matrix(dortmund2_euc)) * 13
                          + as.matrix(ages_jac)/max(as.matrix(ages_jac))
                          + as.matrix(gender_jac)/max(as.matrix(gender_jac))
                          + as.matrix(vehicle_jac)/max(as.matrix(vehicle_jac)))


# Separate all remaining variables in Dortmund into: 1. Population; 2. Wealth; 3. Infrastructure
dortmund2_population <- dortmund2_remaining_var[c("births", "deaths", "children", "population")]
dortmund2_wealth <- dortmund2_remaining_var[c("unemployed", "social_insurance", "benefits")]
dortmund2_infrastructure <- dortmund2_remaining_var[c("area_buildings", "vehicles", "buildings")]

# Compute aggregated dissimilarity distance in each of the 3 main categories
# 1. Population
dortmund2_population_euc <- dist(dortmund2_population, method='euclidean')
# Combine proportionately the Euclidean distance for population variables with the modified Jaccard distances for age and gender distribution
dortmund2_population_agg_dist <- as.dist(as.matrix(dortmund2_population_euc)/max(as.matrix(dortmund2_population_euc)) * 4
                                         + as.matrix(ages_jac)/max(as.matrix(ages_jac))
                                         + as.matrix(gender_jac)/max(as.matrix(gender_jac)))

# 2. Wealth
dortmund2_wealth_agg_dist <- dist(dortmund2_wealth, method='euclidean')

# 3. Infrastructure
dortmund2_infrastructure_euc <- dist(dortmund2_infrastructure, method='euclidean')
# Combine proportionately the Euclidean distance for infrastructure variables with the modified Jaccard distances for buildings and vehicles distribution
dortmund2_infrastructure_agg_dist <- as.dist(as.matrix(dortmund2_infrastructure_euc)/max(as.matrix(dortmund2_infrastructure_euc)) * 3
                                         + as.matrix(buildings_jac)/max(as.matrix(buildings_jac))
                                         + as.matrix(vehicle_jac)/max(as.matrix(vehicle_jac)))

# Compute the aggregated dissimilarity distance over all 3 main categories (with equal weighting)
dortmund2_agg_dist <- as.dist(as.matrix(dortmund2_population_agg_dist)/max(as.matrix(dortmund2_population_agg_dist))
                              + as.matrix(dortmund2_wealth_agg_dist)/max(as.matrix(dortmund2_wealth_agg_dist))
                              + as.matrix(dortmund2_infrastructure_agg_dist)/max(as.matrix(dortmund2_infrastructure_agg_dist)))


# Hierarchical Clustering - Complete Linkage
dortmund_hclust <- hclust(dortmund2_agg_dist, method='complete')
# Plot dendogram
plot(dortmund_hclust)

# Compute the Average Silhouette Width (ASW) to determine best # of clusters that maximises ASW
tasw <- NA
tclusk <- list()
tsil <- list()
for (k in 2:30) {
  # Cut tree at k clusters
  tclusk[[k]] <- cutree(dortmund_hclust,k)
  # Compute silhouette
  tsil[[k]] <- silhouette(tclusk[[k]],dist=dortmund2_agg_dist)
  # Calculate ASW
  tasw[k] <- summary(silhouette(tclusk[[k]],dist=dortmund2_agg_dist))$avg.width
}
plot(1:30,tasw,type="l",xlab="Number of clusters",ylab="ASW")
tasw


# Visaulise clustering with MDS
mdsdortmund2 <- cmdscale(dortmund2_agg_dist,k=2)
plot(mdsdortmund2,pch=clusym[tclusk[[6]]],col=tclusk[[6]])

# Visualise clustering with variables
plot(dortmund2_remaining_var,col=tclusk[[6]],pch=clusym[tclusk[[6]]])


# Visualise age distribution in clusters
# Plot age group distribution for all cities
ages <- melt(ages)
ages$rowid <- cityid$MapNumber
ages$clusterid <- cutree(dortmund_hclust,6)

for (i in c(1:6)) {
  print(ggplot(ages[ages$clusterid==i,], aes(variable, value, group=factor(rowid))) + geom_line(color=brewer.pal(6,"Set2")[i]) + ylim(0,1))
}

# Visualise gender distribution in clusters
# Plot gender group distribution for all cities
gender <- melt(gender)
gender$rowid <- cityid$MapNumber
gender$clusterid <- cutree(dortmund_hclust,6)

for (i in c(1:6)) {
  print(ggplot(gender[gender$clusterid==i,], aes(variable, value, group=factor(rowid))) + geom_line(color=brewer.pal(6,"Set2")[i]) + ylim(0,1))
}

# Visualise vehicle distribution in clusters
# Plot vehicles group distribution for all cities
vehicles <- melt(vehicles)
vehicles$rowid <- cityid$MapNumber
vehicles$clusterid <- cutree(dortmund_hclust,6)

for (i in c(1:6)) {
  print(ggplot(vehicles[vehicles$clusterid==i,], aes(variable, value, group=factor(rowid))) + geom_line(color=brewer.pal(6,"Set2")[i]) + ylim(0,1))
}

# Visualise buildings distribution in clusters
# Plot buildings group distribution for all cities
buildings <- melt(buildings)
buildings$rowid <- cityid$MapNumber
buildings$clusterid <- cutree(dortmund_hclust,6)

for (i in c(1:6)) {
  print(ggplot(buildings[buildings$clusterid==i,], aes(variable, value, group=factor(rowid))) + geom_line(color=brewer.pal(6,"Set2")[i]) + ylim(0,1))
}


# Compare K-Means vs. Hierarchical Clustering
adjustedRandIndex(cutree(dortmund_hclust,6), dortmund_kmeans$cluster)



#load the data into R
setwd("C:/Users/Gourab/Downloads")
movies = read.table("movies.txt", header=FALSE, sep="|",quote="\"")

View(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation", 
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")

View(movies)


# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL 

#str
str(movies)


# Remove duplicates
movies = unique(movies)


#The number of movies in each Genre
colSum <- colSums(movies[,2:20])
colSum

colSum
as.matrix(colSum)

genre = as.data.frame(as.matrix(colSum))
colnames(genre) = 'No. of Movies'
View(genre)
class(genre)



#Hierarchical Clustering
# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward")

#Dendrogram
plot(clusterMovies)



# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

head(clusterGroups, n=20)
movies2 <- cbind(movies, clusterGroups)
View(movies2)



#Understanding the cluster
tapply(movies2$Action, movies2$clusterGroups, mean)
tapply(movies2$Western, movies2$clusterGroups, mean)
tapply(movies2$War, movies2$clusterGroups, mean)



genre.list <- c("Unknown", "Action", "Adventure", "Animation", 
                "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                "Romance", "SciFi", "Thriller", "War", "Western")



clust.profile <- tapply(movies2$Unknown, movies2$clusterGroups, mean)
clust.profile

List = list()

for(i in genre.list)
{
  #if(i != "Unknown")
      newCol = tapply(movies2[,i], movies2$clusterGroups, mean)
  
      #clust.profile = cbind(clust.profile, newCol)
      #clust.profile[,i] = newCol
      
      List[[i]] = newCol 
  
}

List

#========================================================================================
#A Brief intro to List:

x <- list("John", 23, 169, 76, "black", c("phy", "chem", "math"))
x

x[6]
x[[6]]

x <- list(name="John", age=23, height=169, weight=76, hair="black", fav=c("phy", "chem", "math"))
x

x[name]
x[['name']]
x$name

#======================================================================================

clust.profile <- as.data.frame(List)

View(clust.profile)

clust.profile = t(clust.profile)
clust.profile = round(clust.profile,4)*100
clust.profile = clust.profile[-1, ]

View(clust.profile)

write.csv(clust.profile, "Cluster Profile for Movie Segmentation.csv")



#Recommending
#Which Cluster group include MIB?

which(movies2$Title == "Men in Black (1997)")

movies2$clusterGroups[257]

#List the movies in cluster 2
head(movies2$Title[movies2$clusterGroups == 2], 15)


#RECOMMEND A MOVIE FROM THIS CLUSTER





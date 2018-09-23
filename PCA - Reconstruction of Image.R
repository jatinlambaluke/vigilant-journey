#loading the library
library(jpeg)


#Reading the image
image = readJPEG("C:\\Users\\Gourab\\Desktop\\Test Images\\river.jpg")
dim(image)


#Red, Green and Blue components
r <- image[,,1]
g <- image[,,2]
b <- image[,,3]


#PCs for the Red, Green and Blue components
image.r.pca <- prcomp(r, center = FALSE)
image.g.pca <- prcomp(g, center = FALSE)
image.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)


#Re-construction:
ncomp = 60
R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])


#Dimension Check:
dim(r)
dim(image.r.pca$x)
dim(image.r.pca$rotation)
dim(image.r.pca$x[,1:ncomp])
dim(t(image.r.pca$rotation[,1:ncomp]))



#Some Adjustment:
R = ifelse(R>1,1,R)
G = ifelse(G>1,1,G)
B = ifelse(B>1,1,B)

R = ifelse(R<0,0,R)
G = ifelse(G<0,0,G)
B = ifelse(B<0,0,B)


#Re-construction of the Image
img = array(c(R,G,B), dim=c(dim(image)))
summary(img)


writeJPEG(img, "river_compressed.jpg")



#--------------------------------------------------------------------------------------

# https://stats.stackexchange.com/questions/46475/is-pca-appropriate-when-np
# for p >> n



image = readJPEG("C:/Users/Gourab/Downloads/katia.jpg")
dim(image)

r <- image[,,1]
g <- image[,,2]
b <- image[,,3]


image.r.pca <- prcomp(r, center = FALSE)
image.g.pca <- prcomp(g, center = FALSE)
image.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)


dim(image.r.pca$x)
dim(image.r.pca$rotation)

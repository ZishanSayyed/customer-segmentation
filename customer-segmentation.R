cust=read.csv("C:/Users/Administrator/Documents/my projects/Customer segmentation analysis (R)/Mall_Customers.csv"
              ,header =T,sep = ",")
head(cust)
str(cust)
names(cust)



summary(cust$Age)
sd(cust$Age)


summary(cust$Annual.Income..k..)
sd(cust$Annual.Income..k..)


#understang data by visulisation 



                                 #Analysis of Gender Ratio 
a=table(cust$Gender)
barplot(a,main = "Gender comparsion using Barplot",
        xlab = "Sex",
        ylab = "count",
        col = rainbow(2),
        legend=row.names(a))

#From the above barplot, we observe that the number of females is higher than the males

pct=round(a/sum(a)*100)       
lbs=paste(c("Female","Male")," ",pct,"%",sep = " ")
lbs

library(plotrix)
pie3D(a,labels = lbs,main="pie chart of Gender ratio")


# we conclude that the percentage of females is 56%, whereas the percentage of male in the customer dataset is 44%.



                            #Analysis of Age Distribution 


summary(cust$Age)
hist(cust$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)


boxplot(cust$Age,
        col="red",
        main="Boxplot for Descriptive Analysis of Age")
#we conclude that the maximum customer ages are between 30 and 35.
#The minimum age of customers is 18, whereas, the maximum age is 70.

                        #Analysis of the Annual Income of the Customers

summary(cust$Annual.Income..k..)
hist(cust$Annual.Income..k..,
     col = "green",
     main = "Histogram to show Annual Income ",
     xlab = "Annual income class",
     ylab = "Freqency",
     labels = T)


plot(density(cust$Annual.Income..k..),
             col="yellow",
             main="Density Plot for Annual Income",
             xlab="Annual Income Class",
             ylab="Density")



polygon(density(cust$Annual.Income..k..),
        col="#ccff66")


#we conclude that the minimum annual income of the customers is 15 
#the maximum income is 137. People earning an average income of 70 have the highest frequency count 
#in our histogram distribution. The average salary of all the customers is 60.56.


                       #Analyzing Spending Score of the Customers
summary(cust$Spending.Score..1.100.)

hist(cust$Spending.Score..1.100.,
     col="purple",
     main = "Histogram to show Spwnding score ",
     xlab = "Spending Score",
     ylab = "Freqency",
     labels = T)

boxplot(cust$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")



#The minimum spending score is 1, maximum is 99 and the average is 50.20. We can
#see Descriptive Analysis of Spending Score is that Min is 1, Max is 99 and avg. is 50.20.
#From the histogram, we conclude that customers
#between class 40 and 50 have the highest spending score among all the classes.



                                      #K-means Algorithm
#here are three popular methods to define optimal number of clusters -

#Elbow method
#Silhouette method
#Gap statistic


                                        #Elbow Method
#The main goal behind cluster partitioning methods like k-means is 
#to define the clusters such that the intra-cluster variation stays minimum.

library("purrr")
set.seed(0)

#function to calculate intra cluster sum of square

fun=function(k){
  kmeans(cust[,3:5],k,iter.max = 100,nstart = 100,algorithm = "Lloyd")$tot.withinss
}

k.values=1:10
fun_values=map_dbl(k.values,fun)

fun_values

plot(k.values,fun_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#From the above graph, we conclude that 4 is the appropriate number of clusters 
#since it seems to be appearing at the bend in the elbow plot.



                                            #Average Silhouette Method
#With the help of the average silhouette method, we can measure the quality of our clustering operation. With this, 
#we can determine how well within the cluster is the data object.

library("cluster")
library("gridExtra")
library("grid")


k2=kmeans(cust[,3:5],2,iter.max = 100,nstart =50,algorithm = "Lloyd")
s2=plot(silhouette(k2$cluster,dist(cust[,3:5],"euclidean" )))


k3=kmeans(cust[,3:5],3,iter.max = 100,nstart =50,algorithm = "Lloyd")
s3=plot(silhouette(k3$cluster,dist(cust[,3:5],"euclidean" )))


k4=kmeans(cust[,3:5],4,iter.max = 100,nstart =50,algorithm = "Lloyd")
s4=plot(silhouette(k4$cluster,dist(cust[,3:5],"euclidean" )))

k5=kmeans(cust[,3:5],5,iter.max = 100,nstart =50,algorithm = "Lloyd")
s5=plot(silhouette(k5$cluster,dist(cust[,3:5],"euclidean" )))


k6=kmeans(cust[,3:5],6,iter.max = 100,nstart =50,algorithm = "Lloyd")
s6=plot(silhouette(k6$cluster,dist(cust[,3:5],"euclidean" )))


k7=kmeans(cust[,3:5],7,iter.max = 100,nstart =50,algorithm = "Lloyd")
s7=plot(silhouette(k7$cluster,dist(cust[,3:5],"euclidean" )))

k8=kmeans(cust[,3:5],8,iter.max = 100,nstart =50,algorithm = "Lloyd")
s8=plot(silhouette(k8$cluster,dist(cust[,3:5],"euclidean" )))


k9=kmeans(cust[,3:5],9,iter.max = 100,nstart =50,algorithm = "Lloyd")
s9=plot(silhouette(k9$cluster,dist(cust[,3:5],"euclidean" )))

k10=kmeans(cust[,3:5],10,iter.max = 100,nstart =50,algorithm = "Lloyd")
s10=plot(silhouette(k10$cluster,dist(cust[,3:5],"euclidean" )))


library("NbClust")
library("factoextra")
fviz_nbclust(cust[,3:5], kmeans, method = "silhouette")




                                              #Gap Statistic Method
#We can use this method to any of the clustering method like K-means, hierarchical clustering etc.
#Using the gap statistic, one can compare the total intracluster variation for different 
#values of k along with their expected values under the null reference distribution of data

set.seed(123)
gap_stat=clusGap(cust[,3:5],FUN=kmeans,nstart=20,
                 K.max = 10,B=50)

fviz_gap_stat(gap_stat)





k6<-kmeans(cust[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6


#Visualizing the Clustering Results using the First Two Principle Components



pcluster=prcomp(cust[,3:5],scale=FALSE)
summary(pcluster)

pcluster$rotation[,1:2]





set.seed(1)
ggplot(cust, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



#Cluster 1 and 5 - These clusters represent the customer_data with the medium income salary as well as the medium annual spend of salary.

#Cluster 4 - This cluster represents the customer_data having a high annual income as well as a high annual spend.

#Cluster 2 - This cluster denotes the customer_data with low annual income as well as low yearly spend of income.

#Cluster 3 - This cluster denotes a high annual income and low yearly spend.

#Cluster 6 - This cluster represents a low annual income but its high yearly expenditure.





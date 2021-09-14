# petrol price dataset

library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(tidyverse)
library(scales)
print(getwd)


# read and view the dataset:
d<-read.csv("C:/Users/harisankar k/Desktop/PetrolPriceIndia.csv")
print(d)

# check the first and last rows 
print(head(d))
print(tail(d))

# check the no.of rows and columns of the dataset:
print(dim(d))

# check the summary:
print(summary(d))

# check the structure of dataset:
print(str(d))

# check the glimpse of the dataset:
print(glimpse(d))

# check the column names:
print(names(d))

# check the details of column opening:
print(d$Opening)

# check the length of the dataset:
print(length(d))

# check unique values:
print(unique(d))

# statistical values:
print(is.na(d))
print(is.data.frame(d))
print(is.name(d))
print(ncol(d))
print(nrow(d))
print(max(d$Opening))
print(min(d$Opening))
print(sort(d$Opening))
print(which.max(d$Opening))
print(which.min(d$Opening))
print(mean(d$Opening))
print(mean(d$Opening,trim=0.10))
print(var(d$Opening))
print(median(d$Opening))
print(mad(d$Opening))
print(sd(d$Opening))
print(mode(d$Opening))
print(range(d$Opening))
print(scale(d$Opening))
print(sd(d$Opening/sqrt(length(d$Closing))))
print(max(d$Opening-min(d$Closing)))
print(quantile(d$Opening))
print(quantile(d$Opening,c(0.75)))
print(IQR(d$Opening))
print(t.test(d$Opening))

# monthwise price analysis

monthwise=d %>% group_by(Month.Year) %>% summarise(Lowest)
View(monthwise)

# monthwise price analysis in desecnding order
monthwise=d %>% group_by(Month.Year) %>% summarise(Lowest) %>% arrange((desc
                                                                        (Lowest)))
view(monthwise)


# geographic plot of monthwise
gsplot=d %>% group_by(Month.Year) %>% summarise(Lowest)
View(gsplot)



# MONTHWISE PRICE IN HIGHEST 
monthwisehighest=d %>% group_by(Month.Year) %>% summarise(Highest) %>% arrange((desc
                                                                                (Highest)))
View(monthwisehighest)


# highest vs lowest price analysis
highestvslowest=d %>% group_by(Highest) %>% summarise(Lowest) %>% arrange((desc
                                                                           (Lowest)))
View(highestvslowest)


# datavisualisation

#ploting highest price in  monthwise
monthwise=ggplot(d,aes(x=Highest,y=Month.Year,fill=Highest))+geom_col()
print(monthwise)


# plotting the lowest price in monthwise
monthwise=ggplot(d,aes(x=Lowest,y=Month.Year,fill=Lowest))+geom_col()
print(monthwise)



# highest vs lowest price using bargraph:
highestvslowest1=ggplot(d,aes(x=Lowest,y=Highest,fill=Closing))+geom_col()
print(highestvslowest1)


highestvslowest2=ggplot(d,aes(x=Lowest,y=Month.Year,fill=Lowest))+geom_col()
print(highestvslowest2)


# highest price using scatterplot:
highestprice=ggplot(d,aes(x=Highest,y=Month.Year))+geom_point()
print(highestprice)


# opening price using dotplot:
openingprice=ggplot(d,aes(x=Opening,y=Month.Year,fill=Opening))+geom_jitter()
print(openingprice)



# lowest vs closing price using lineplot:
highestprice=ggplot(d,aes(x=Highest,y=Closing))+geom_line()
print(highestprice)


# highest closing price using boxplot:
highestclosing=ggplot(d,aes(x=Highest,y=Closing,fill=Closing),size=1.0)+geom_boxplot()
print(highestclosing)
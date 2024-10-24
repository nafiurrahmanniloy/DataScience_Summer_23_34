library(dplyr)


mydata <- read.csv("D:/Study Metarials/8th Semester/Data Science/Project/Dataset_midterm.csv", header = TRUE, sep = ",")

View(mydata)
str(mydata)
summary(mydata)
num_instances <- nrow(mydata) 
num_attributes <- ncol(mydata) 
print(paste("Number of instances (rows):", num_instances))
print(paste("Number of Columns:", num_attributes))



missing_values_indices <- lapply(mydata, function(x) {
  if (is.integer(x) | is.character(x)) {
    return(which(is.na(x) | x == ""))
  } else {
    return(NULL)
  }
})
print(missing_values_indices)

#barplot

na_counts <- colSums(is.na(mydata))
print(na_counts)


barplot(na_counts, names.arg = names(na_counts),
        ylab = "Number of Missing Values", col = "red",cex.names = 0.9,
        main = "Missing Values per Attribute", las =2)



actualData <- mydata[-c((num_instances - 1):num_instances), ]

num_instances <- nrow(actualData)
num_attributes <- ncol(actualData)
print(paste("Number of instances (rows):", num_instances))
print(paste("Number of Columns:", num_attributes))


#Gender

unique_values <- unique(actualData$Gender)
print(unique_values)

actualData$Gender <- factor(actualData$Gender,
                        levels = c("male", "female"),
                        labels = c(1, 2))
View(actualData)

mode_gender <- names(which.max(table(actualData$Gender)))

actualData$Gender[is.na(actualData$Gender)] <- mode_gender

View(actual)

#Age

age_median <- round(median(actualData$age, na.rm = TRUE))
actualData$age[is.na(actualData$age)] <- age_median

#boxplot
boxplot(actualData$age, main = "Age" ) 

Q1 <- quantile(actualData$age, 0.25)
Q3 <- quantile(actualData$age, 0.75)


IQR_value <- Q3 - Q1

threshold <- 1.5

outlier_condition <- (actualData$age < (Q1 - threshold * IQR_value)) | (actualData$age > (Q3 + threshold * IQR_value))


actualData <- actualData %>% 
  filter(!outlier_condition) %>% 
  arrange(row_number())
boxplot(actualData$age, main = "Age" )


View(actualData)

#sibsp
missing_values_sibsp <- sum(is.na(actualData$sibsp)) 
print(paste("Total Missing Values:",missing_values_sibsp))


#parch
missing_values_parch <- sum(is.na(actualData$parch)) 
print(paste("Total Missing Values:",missing_values_parch))


#embarked 

unique_embarked <- unique(actualData$embarked)
print(unique_values)

actualData$embarked <- factor(actualData$embarked,
                            levels = c("S","Q","C"),
                            labels = c(1, 2,3))
missing_values_embarked <- sum(is.na(actualData$embarked)) 
print(missing_values_embarked)
print(paste("Total Missing Values:",missing_values_embarked))

View(actualData)


#class 

unique_class <- unique(actualData$class)
print(unique_class)

actualData$class<- factor(actualData$class,
                          levels=c("First","Second","Third"),
                          labels=c(1,2,3))
missing_values_class<-sum(is.na(actualData$class))
print(paste("Total Missing Values:",missing_values_class))


mode_class <- names(which.max(table(actualData$class)))
actualData$class[is.na(actualData$class)] <- mode_class
View(actualData)






#who 
unique_values <- unique(actualData$who) 
print(unique_values)

actualData$who <- gsub("mannn", "man", actualData$who) 

actualData$who <- factor(actualData$who,
                         levels = c("man", "woman", "child"),
                         labels = c(1, 2, 3))

View(actualData)
missing_values_who <- sum(is.na(actualData$who)) 
print(paste("Total Missing Values:", missing_values_who))

#alone

unique_values<-unique(actualData$alone)
print(unique_values)

actualData$alone<- factor(actualData$alone,
                          levels=c("TRUE","FALSE"),
                          labels=c(1,2))
missing_values_alone <- sum(is.na(actualData$alone)) 
print(paste("Total Missing Values:",missing_values_alone))

View(actualData)


#survived
missing_values_survived <- sum(is.na(actualData$survived)) 
noisy_values_survived <- sum(actualData$survived < 0 | actualData$survived > 1 ) 
print(paste("Total Missing Values:",missing_values_survived))
print(paste("Total Noisy Values:",noisy_values_survived))

#fare
actualData$fare <- as.numeric(actualData$fare)
fare_median <- round(median(actualData$fare, na.rm = TRUE))
actualData$fare[is.na(actualData$fare)] <- fare_median

#Vizualization

  getMode <- function(v) { 
    tabulated <- table(v) 
    mode_value <- names(sort(tabulated, decreasing = TRUE))[1] 
    return(as.numeric(mode_value))  
  } 
means <- sapply(actualData, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA) 
medians <- sapply(actualData, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA) 
modes <- sapply(actualData, function(x) if(is.numeric(x) || is.factor(x) || is.character(x)) getMode(x) 
                else NA) 
stat_values <- rbind(means, medians, modes) 
row_names <- c("Mean", "Median", "Mode") 
rownames(stat_values) <- row_names

barplot(stat_values, beside = TRUE,  
        col = c("green", "orange", "brown"), 
        legend.text = row_names, 
        args.legend = list(x = "topright", cex = 0.9), 
        cex.names = 0.9,
        ylim = c(0, 50))

View(actualData)




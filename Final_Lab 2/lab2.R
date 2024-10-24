library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(fmsb)

mydata <- read.csv("D:/Study Metarials/8th Semester/Data Science/Final/Lab 2/WHO COVID-19 cases.csv")
View(mydata)
str(mydata)

missing_values_New_cases <- sum(is.na(mydata$New_cases))
missing_values_Cumulative_cases <- sum(is.na(mydata$Cumulative_cases))
missing_values_New_deaths <- sum(is.na(mydata$New_deaths))
missing_values_Cumulative_deaths <- sum(is.na(mydata$Cumulative_deaths))

print(paste("Total missing values in New Cases:",missing_values_New_cases))
print(paste("Total missing values in Cumulative Cases:",missing_values_Cumulative_cases))
print(paste("Total missing values in New Deaths:",missing_values_New_cases))
print(paste("Total missing values in Cumulative Deaths:",missing_values_Cumulative_deaths))

mydata$New_cases[is.na(mydata$New_cases)] <- 0
mydata$New_deaths[is.na(mydata$New_deaths)] <- 0
View(mydata)



unique(mydata$Continent)
deaths_by_Continent <- mydata %>%
  group_by(Continent) %>%  
  summarize(Total_New_Deaths = sum(New_deaths))
View(deaths_by_Continent)
str(deaths_by_Continent)
summary(deaths_by_Continent$Total_New_Deaths)

ggplot(deaths_by_Continent, aes(x = Continent, y = Total_New_Deaths,fill = Continent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  labs(title = "Total New Deaths by Continent", 
       x = "Continent", 
       y = "Total New Deaths")

ggplot(deaths_by_Continent, aes(x = Continent, y = Total_New_Deaths)) +
  geom_point(aes(color = Continent), size = 3, show.legend = FALSE) + 
  labs(
    title = "Scatter Plot of Total New Deaths by Continent",
    x = "Continent",
    y = "Total New Deaths"
  )

mydata_filtered <- mydata %>% 
  filter(New_deaths > 5000)
ggplot(mydata_filtered, aes(x = Continent, y = New_deaths)) +
  geom_violin(trim = FALSE, fill = "skyblue", color = "darkblue") +
    stat_summary(fun = median, geom = "point", color = "red", size = 2) +
  labs(title = "Distribution of New Deaths in Violine Plot", x = "Continent", y = "New Deaths") 
mydata_filtered <- mydata %>%
  filter(New_deaths > 3000)



deaths_by_Country_code <- mydata %>%
  group_by(Country_code) %>%
  summarize(Total_New_Deaths = sum(New_deaths, na.rm = TRUE))
str(deaths_by_Country_code)
summary(deaths_by_Country_code$Total_New_Deaths)

ggplot(deaths_by_Country_code, aes(x = Total_New_Deaths)) +
  geom_histogram(binwidth = 30000, fill = "red", color = "black") +  
  labs(
    title = "Histogram of Total New Deaths by Country",  
    x = "Total New Deaths",  
    y = "Count of Countries"  
  ) +
  xlim(0, max(deaths_by_Country_code$Total_New_Deaths)) +
  ylim(0, 40)
mean_deaths <- mean(deaths_by_Country_code$Total_New_Deaths, na.rm = TRUE)
median_deaths <- median(deaths_by_Country_code$Total_New_Deaths, na.rm = TRUE)
print(paste("Mean:", mean_deaths))
print(paste("Median:", median_deaths))

if (mean_deaths > median_deaths) {
  skewness_direction <- "Positively Skewed"
} else if (mean_deaths < median_deaths) {
  skewness_direction <- "Negatively Skewed"
} else {
  skewness_direction <- "Normal Distribution"
}
print(paste("Distribution:", skewness_direction))


plot <- ggplot(deaths_by_Country_code, aes(x = Country_code, y = Total_New_Deaths)) +
  geom_bar(stat = "identity", aes(fill = Country_code), show.legend = FALSE) +  
  labs(title = "Total New Deaths by Country Code", 
       x = "Country Code", 
       y = "Total New Deaths") +
  scale_x_discrete(labels = NULL)  

p_plotly <- ggplotly(plot, tooltip = c("Country_code", "Total_New_Deaths")) 
p_plotly <- layout(p_plotly, showlegend = FALSE)
p_plotly



new_cases_by_who_region <- mydata %>%
  filter(!is.na(WHO_region)) %>%
  group_by(WHO_region) %>%
  summarize(Total_New_Cases = sum(New_cases, na.rm = TRUE))
print(new_cases_by_who_region)

plot <- ggplot(new_cases_by_who_region, aes(x = WHO_region, y = Total_New_Cases, fill = WHO_region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  labs(title = "Total New Cases by WHO Region", 
       x = "WHO Region", 
       y = "Total New Cases")
p_plotly <- ggplotly(plot, tooltip = c("Total_New_Cases"))
p_plotly <- layout(p_plotly, showlegend = FALSE)
p_plotly

ggplot(new_cases_by_who_region, aes(x = WHO_region, y = Total_New_Cases)) +
  geom_point(aes(color = WHO_region), size = 4, alpha = 0.7) +  
  labs(
    title = "Scatter Plot of Total New Cases by WHO Region",
    x = "WHO Region",
    y = "Total New Cases"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "none"  
  )



max_cumulative_deaths <- max(mydata$Cumulative_deaths, na.rm = TRUE)
print(max_cumulative_deaths)
ggplot(mydata, aes(x = Cumulative_deaths)) +
  geom_histogram(binwidth = 100000,fill="blue",color="black") +
  labs(
    title = "Histogram of Cumulative Deaths",
    x = "Cumulative Deaths",
    y = "Frequency"
  )+
  xlim(0, 1300000) +
  ylim(0,3000)

mean_deaths <- mean(mydata$Cumulative_deaths, na.rm = TRUE)
median_deaths <- median(mydata$Cumulative_deaths, na.rm = TRUE)
print(paste("Mean:", mean_deaths))
print(paste("Median:", median_deaths))

if (mean_deaths > median_deaths) {
  skewness_direction <- "Positively Skewed"
} else if (mean_deaths < median_deaths) {
  skewness_direction <- "Negatively Skewed"
} else {
  skewness_direction <- "Normal Distribution"
}
print(paste("Distribution:", skewness_direction))



max_Cumulative_cases <- max(mydata$Cumulative_cases, na.rm = TRUE)
print(max_Cumulative_cases)
ggplot(mydata, aes(x = Cumulative_cases)) +
  geom_histogram(binwidth = 3000000,fill="blue",color="black") +
  labs(
    title = "Histogram of Cumulative Deaths",
    x = "Cumulative Cases",
    y = "Frequency"
  )+
  xlim(0, 110000000) +
  ylim(0,7500)

mean_deaths <- mean(mydata$Cumulative_cases, na.rm = TRUE)
median_deaths <- median(mydata$Cumulative_cases, na.rm = TRUE)
print(paste("Mean:", mean_deaths))
print(paste("Median:", median_deaths))

if (mean_deaths > median_deaths) {
  skewness_direction <- "Positively Skewed"
} else if (mean_deaths < median_deaths) {
  skewness_direction <- "Negatively Skewed"
} else {
  skewness_direction <- "Normal Distribution"
}
print(paste("Distribution:", skewness_direction))



boxplot(deaths_by_Country_code$Total_New_Deaths,
        main = "Boxplot of Total New Deaths",
        ylab = "Total New Deaths",
        xlab = "Countries",  
        col = "lightblue",
        outline = TRUE, 
        horizontal = FALSE,  
        las = 1)  
















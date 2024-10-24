library(dplyr)
library(ggplot2)

mydata <- read.csv("D:/Series/climate_change.csv", header = TRUE, sep = ",")
summary(mydata)
str(mydata)
View(mydata)

colnames(mydata) <- make.names(colnames(mydata))
print(names(mydata))


#correlation between units sold and discount
missing_values_units_sold <- sum(is.na(mydata$Units.Sold))
missing_values_discount <- sum(is.na(mydata$Discount.Percentage))
print(paste("Total Missing Values in Units Sold:", missing_values_units_sold))
print(paste("Total Missing Values in Discount Percentage:", missing_values_discount))

correlation_value <- cor(mydata$Units.Sold, mydata$Discount.Percentage, method = "pearson", use = "complete.obs")
print(paste("The Correlation value between Discount Percentage and Units Sold is:",correlation_value))
correlation_text <- paste("Correlation 1 =", round(correlation_value, 2))

#Scatter plot vizualization
ggplot(mydata, aes(x = Discount.Percentage, y = Units.Sold)) +
  geom_point(alpha = 0.5, size = 2, color = "blue") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of Units Sold vs Discount Percentage", 
       subtitle = paste("Correlation (R) =", round(correlation_value, 2)), 
       x = "Discount Percentage", 
       y = "Units Sold") +  
  theme_bw()



#correlation between revenue and unit sold
missing_values_sales_revenue <- sum(is.na(mydata$Sales.Revenue..USD.))
missing_values_units_sold <- sum(is.na(mydata$Units.Sold))
print(paste("Total Missing Values in Sales Revenue:", missing_values_sales_revenue))
print(paste("Total Missing Values in Units Sold:", missing_values_units_sold))

# Calculate the Pearson correlation between 'Sales.Revenue..USD.' and 'Units.Sold'
correlation_value <- cor(mydata$Sales.Revenue..USD., mydata$Units.Sold, method = "pearson", use = "complete.obs")
print(paste("The Correlation value between Sales Revenue and Units Sold is:",correlation_value))
correlation_text <- paste("Correlation 2 =", round(correlation_value, 2))

#Scatter plot vizualization
ggplot(mydata, aes(x = Sales.Revenue..USD., y = Units.Sold)) +
  geom_point(alpha = 0.5, size = 2, color = "black") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Scatterplot of Sales Revenue vs Units Sold", 
       subtitle = paste("Correlation (R) =", round(correlation_value, 2)),  
       x = "Sales Revenue (USD)", 
       y = "Units Sold") + 
  theme_bw()  



#Correlation between Day of the week and sales 
sales_by_day <- aggregate(Units.Sold ~ Day.of.the.Week, data = mydata, sum)
print(sales_by_day)
sales_day_sorted <- sales_by_day[order(-sales_by_day$Units.Sold), ]

print(sales_day_sorted)

#scatter plot to see which day has most sales
ggplot(data = sales_by_day, aes(x = Day.of.the.Week, y = Units.Sold)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Total Units Sold by Day of the Week",
       x = "Day of the Week",
       y = "Total Units Sold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for units sold by day of the week
ggplot(data = mydata, aes(x = Day.of.the.Week, y = Units.Sold)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Units Sold by Day of the Week",
       x = "Day of the Week",
       y = "Units Sold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


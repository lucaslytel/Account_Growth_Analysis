library(tidyverse)


data <- read_csv(file.choose())

head(data)
data

p <- ggplot(data = data, mapping = aes(x = X1, y = West))

p + geom_bar(stat = "identity")

data %>% gather()

colnames(data) <- c("Month_Year", "West", "Midwest", "South", "Northeast")

first_dollar <- data %>% pivot_longer(-"Month_Year", names_to = "Region", values_to = "count")
tail(first_dollar)  

p <- ggplot(first_dollar, mapping = aes(x = count, y = Month_Year))
p + geom_bar(stat = "identity") + facet_wrap(~Region)

first_dollar <- first_dollar[complete.cases(first_dollar),]
tail(first_dollar)
summary(first_dollar)

t <- subset(first_dollar, Month_Year != "Totals 2020") %>% subset(Month_Year != "Totals 2019") %>% 
  subset(Month_Year != "Totals 2018") %>% subset(Month_Year != "Totals 2017")
str(t)
t$Region <- as.factor(t$Region)
t$Month_Year <- factor(t$Month_Year, levels = unique(t$Month_Year))

p <- ggplot(data = t, mapping = aes(x = Month_Year, y = count))
p + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~Region)

library(lubridate)

# Subetting Data Frame based off condition
result <- subset(t,  grepl(glob2rx("*2017*") , Month_Year) )
result$Month_Year <- as_date(result$Month_Year)
result$Month_Year <- factor(result$Month_Year, levels = unique(result$Month_Year))
result$count <- as.numeric(result$count)

?as_date()
p <- ggplot(data = result, mapping = aes(x = Month_Year, y = count, fill = Region))
new_p <- p + geom_bar(stat = "identity", position = "dodge", color = "gray70") + 
  facet_wrap(~Region, ncol = 2) +
  guides(fill = F) + #Removes Legend 
  labs(x = "Month",
       y = "Number of Accounts",
       title = "Actively Raising Accounts by Month 2017")

new_p + theme(axis.text.x = element_text(angle = 60, hjust = 1))





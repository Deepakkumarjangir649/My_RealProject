## Import Liberies

library(readr)
library(data.table)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)
library(Amelia)
library(caTools)
library(class)
library(scales)
library(gbm)

options(scipen=999)
set.seed(321)


setwd("C:/Users/DEEPAK KUMAR/Desktop/Python Project/Predict Future Sale")

dataset_sales_train = read.csv("sales_train.csv")
head(dataset_sales_train)



dataset_items = read.csv("items.csv")
head(dataset_items)


item_categories = read.csv("item_categories.csv")
head(item_categories)



dim(dataset_sales_train)

dim(dataset_items)

dataset_sales <- dataset_sales_train %>% left_join(dataset_items, by = c("item_id"))
dataset_sales$item_name <- NULL
head(dataset_sales)


dataset_sales <- as.data.frame(dataset_sales)
str(dataset_sales)
head(dataset_sales)

dataset_sales$date <- dmy(dataset_sales$date)

dataset_sales$year <- year(dataset_sales$date)
dataset_sales$month <- month(dataset_sales$date)
dataset_sales$day <- day(dataset_sales$date)
dataset_sales$weekday <- weekdays(dataset_sales$date)


dataset_sales$year = as.factor(dataset_sales$year)
dataset_sales$weekday = as.factor(dataset_sales$weekday)

str(dataset_sales)


dataset_sales_item_cnt_month <-
  dataset_sales %>% group_by(year, month, shop_id, item_id) %>% summarise(item_cnt_month = sum(item_cnt_day)) %>% ungroup()
dataset_sales <-
  dataset_sales %>% left_join(dataset_sales_item_cnt_month,
                              by = c("year", "month", "shop_id", "item_id"))



head(dataset_sales)
rm(dataset_sales_item_cnt_month)


glimpse(dataset_sales)

str(dataset_sales)


summary(dataset_sales)


colSums(is.na(dataset_sales))


is.null(dataset_sales)

# DATA EXPLORATION  # 
#-----------------------------------------#

# Check the correlation between numeric columns
 num.cols = sapply(dataset_sales,is.numeric)
dataset_sales_numcols = dataset_sales[,num.cols] 
dataset_sales_numcols$data_block_num = NULL
dataset_sales_numcols$month = NULL
dataset_sales_numcols$day = NULL
cor(dataset_sales_numcols)


melted_corr <- melt(cor(dataset_sales_numcols))
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="darkred") + 
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), size = 4) + 
  labs(title = "Correlation Matrix", x = "Numeric Column(s)", y = "Numeric Column(s)", fill = "Coefficient Range") + 
  theme(axis.text.x=element_text(angle=45, vjust=0.5))

rm(num.cols)
rm(dataset_sales_numcols)
rm(melted_corr)

# How many shops are there?
dataset_sales %>% select(shop_id) %>% distinct() %>%
  count()


## Which shop is most popular and what is total sales by the shop?
most.popular.shop <- 
  dataset_sales %>% group_by(shop_id) %>% summarise(total.sales.by.shop = sum(item_cnt_day)) %>%
  arrange(desc(total.sales.by.shop)) %>% ungroup()



ggplot(data = most.popular.shop, aes(x = reorder(as.factor(shop_id), total.sales.by.shop), y = total.sales.by.shop, fill = as.factor(shop_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Most popular shop with most sales", x = "Shop(s)", y = "Total sales", fill = "Shop Id")

rm(most.popular.shop)


# How Many unique items are there in various shops ?

dataset_sales %>% select(item_id) %>% distinct() %>% 
  count()


## Which shop has items in the shop ?
most.items.in.shop <- 
  dataset_sales %>% group_by(shop_id) %>% summarise(total.items.per.shop = n_distinct(item_id)) %>%
  arrange(desc(total.items.per.shop)) %>% ungroup()


ggplot(data = most.items.in.shop,aes(x=reorder(as.factor(shop_id),total.items.per.shop),y = total.items.per.shop, fill = as.factor(shop_id))) +
  geom_bar(stat = 'identity') + coord_flip() +
  labs(title="Most items available at shop(s)",x = "Shop(s)",y = "Total number of items at shop",fill = 
         "Shop Id")



rm(most.items.in.shop)



## Which item is the most popular and most sold in each shop?
most.sold.item.at.shop <- 
  dataset_sales %>% group_by(shop_id,item_id) %>% 
  summarise(most.sold.item.count = sum(item_cnt_day)) %>%
  filter(most.sold.item.count == max(most.sold.item.count)) %>% 
  arrange(desc(most.sold.item.count)) %>% ungroup()


ggplot(data = most.sold.item.at.shop,aes(x = reorder(as.factor(shop_id),most.sold.item.count),y = most.sold.item.count,
                                         fill = as.factor(item_id))) + 
geom_bar(stat = "identity") + 
coord_flip() +
  labs(title = "Most Popular/sold item at shop(s)",x = "Shop(s)",y = "Most sold item at shop", fill = "Item Id")


rm(most.sold.item.at.shop)



## How MAny unique categories are there ?
dataset_sales %>% select(item_category_id) %>% distinct() %>% count()


## Which shop has most items categories i the shop ?


most.categories.in.shop <- 
  dataset_sales %>% group_by(shop_id) %>% summarise(total.categories.per.shop = n_distinct(category_id)) %>% arrange(desc(total.categories.per.shop)) %>% ungroup()

ggplot(data = most.categories.in.shop, aes(x = reorder(as.factor(shop_id), total.categories.per.shop), y = total.categories.per.shop, fill = as.factor(shop_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Most item categories at shop(s)", x = "Shop(s)", y = "Total item categories at shop", fill = "Shop Id")



# Which item category is the most popular and most sold in each shop?


most.sold.category.at.shop <- 
  dataset_sales %>% group_by(shop_id, category_id) %>% summarise(most.sold.category.count = sum(item_cnt_day)) %>% filter(most.sold.category.count == max(most.sold.category.count)) %>% arrange(desc(most.sold.category.count)) %>% ungroup()

ggplot(data = most.sold.category.at.shop, aes(x = reorder(as.factor(shop_id), most.sold.category.count), y = most.sold.category.count, fill = as.factor(category_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Most item categories at shop", x = "Shop(s)", y = "Most item categories sold count", fill = "Item Category Id")

rm(most.sold.category.at.shop)



 ## Which product category is highest sales grossing across all shops?


highest.grossing.category <- 
  dataset_sales %>% group_by(category_id) %>% summarise(total.grossing = sum(item_price * item_cnt_day)) %>% arrange(desc(total.grossing)) %>% ungroup()

ggplot(data = highest.grossing.category, aes(x = reorder(as.factor(category_id), total.grossing), y = total.grossing, fill = as.factor(category_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Highest sales grossing product category", x = "Item Category Id(s)", y = "Total sales", fill = "Item Category Id")


rm(highest.grossing.category)


## Which store keeps which product categories?

product.categories.sold.by.shop <- 
  dataset_sales %>% group_by(shop_id) %>% summarise(product.categories.list = paste(sort(unique(category_id)), collapse = ", ")) %>% ungroup()
product.categories.sold.by.shop

rm(product.categories.sold.by.shop)


## Which items gets sold the most under which product category?

most.sold.item.under.category <- 
  dataset_sales %>% group_by(category_id, item_id) %>% summarise(total.grossing = sum(item_price * item_cnt_day)) %>% filter(total.grossing == max(total.grossing)) %>% arrange(desc(total.grossing)) %>% ungroup()

ggplot(data = most.sold.item.under.category, aes(x = reorder(as.factor(category_id), total.grossing), y = total.grossing, fill = as.factor(item_id))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Highest sold item in product category", x = "Item Category Id(s)", y = "Total sales", fill = "Item Id")

rm(most.sold.item.under.category)


# What Are the Day and Month Wise total sales ?


month.day.wise.total.sales <- 
  dataset_sales %>% group_by(month, day) %>% summarise(total.sales.everyday = sum(item_price * item_cnt_day)) %>% arrange(month, day) %>% ungroup()

ggplot(data = month.day.wise.total.sales, aes(x = day, y = total.sales.everyday, group = month, color = as.factor(month))) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = seq(min(0), max(31), by = 1)) + 
  labs(title = "Month-Day-wise total sales", x = "Day(s) of Month", y = "Total sales everyday", fill = "Month")




ggplot(data = month.day.wise.total.sales, aes(x = day, y = total.sales.everyday, fill = as.factor(day))) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks = seq(min(0), max(31), by = 1)) + 
  facet_wrap( ~ month, ncol = 2) + 
  labs(title = "Month-Day-wise total sales", x = "Day(s) of Month", y = "Total sales everyday", fill = "Day")

rm(month.day.wise.total.sales)



## What are the year wise total sales?


year.wise.total.sales <- 
  dataset_sales %>% group_by(year) %>% summarise(total.sales.per.year = sum(item_price * item_cnt_day)) %>% arrange(year) %>% ungroup()

ggplot(data = year.wise.total.sales, aes(x = year, y = total.sales.per.year, fill = as.factor(year))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total sales per year", x = "Year", y = "Total sales per year", fill = "Year")

rm(year.wise.total.sales)



## What are the Year_Month wise total sales?

year.month.wise.total.sales <- 
  dataset_sales %>% group_by(year, month) %>% summarise(total.sales.per.year = sum(item_price * item_cnt_day)) %>% arrange(year) %>% ungroup()

ggplot(data = year.month.wise.total.sales, aes(x = year, y = total.sales.per.year, fill = as.factor(month))) + 
  geom_bar(stat = "identity") + 
  labs(title = "Total sales per year-month", x = "Year", y = "Total sales per year", fill = "Month")

rm(year.month.wise.total.sales)



# What Percentage fpr sold each month?

dataset_sales$sale_price <- dataset_sales$item_price * dataset_sales$item_cnt_day

total.no.of.items.sold <- sum(dataset_sales$item_cnt_day)
total.revenue <- sum(dataset_sales$sale_price)

monthly.items.sales <-
  dataset_sales %>% group_by(date_block_num) %>% summarise(monthly.items.sales.freqeuncy = round(sum(item_cnt_day) / total.no.of.items.sold, digit = 3))

ggplot(data = monthly.items.sales, aes(x = "", y = monthly.items.sales.freqeuncy, fill = factor(date_block_num) )) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar(theta = "y", start = 0) +
  geom_col(position = 'fill') +
  geom_label(aes(label = paste0(monthly.items.sales.freqeuncy * 100, "%")), position = position_fill(vjust = 0.5)) + 
  labs(title = "% of items sold per month", x = "", y = "Monthly item sale frequency", fill = "Months")

rm(total.no.of.items.sold, total.revenue, monthly.items.sales)




# How Many Items got Sold Each day ?

items.sold.per.day <- 
  dataset_sales %>% group_by(date) %>% summarise(items.per.day = sum(item_cnt_day)) %>% ungroup()

head(items.sold.per.day)

ggplot(data = items.sold.per.day, aes(x = date, y = items.per.day, colour = items.per.day)) + 
  geom_line() + 
  geom_point(size=0.25) + 
  labs(title = "No of items sold per day", x = "Date(s)", y = "Total items sold per day", fill = "No. of items per day")

rm(items.sold.per.day)



## Which data Highest sale got recorded during the dataset time period?

date.of.high.sale <- 
  dataset_sales %>% group_by(date) %>% summarise(total.sales.of.day = sum(item_price * item_cnt_day)) %>% arrange(desc(total.sales.of.day)) %>% ungroup()

head(date.of.high.sale, 10)

ggplot(date.of.high.sale, aes(date, total.sales.of.day)) + 
  geom_point(na.rm=TRUE, color="darkred", size=0.5) + 
  (scale_x_date(breaks=date_breaks("9 months"), labels=date_format("%b %y"))) + 
  labs(title = "No of items sold per day", x = "Date(s)", y = "Total sale of the day")

rm(date.of.high.sale)


## How MAny items (total) sold on particular weekday ?

items.sold.per.weekday <- 
  dataset_sales %>% group_by(weekday) %>% summarise(most.items.sold = sum(item_cnt_day)) %>% arrange(desc(most.items.sold)) %>% ungroup()

ggplot(data = items.sold.per.weekday, aes(x = reorder(weekday, most.items.sold), y = most.items.sold, fill = weekday)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Items sold per weekday", x = "Weekday", y = "Most items sold", fill = "Weekday")

rm(items.sold.per.weekday)



#We can see that at week-end (Friday, Saturday, Sunday) most 
#items got sold. During week-days the sale remains flat.

#What were the total sales revenue on particular weekday?

total.sales.per.weekday <- 
  dataset_sales %>% group_by(weekday) %>% summarise(total.sales = sum(item_price * item_cnt_day)) %>% arrange(desc(total.sales)) %>% ungroup()

ggplot(data = total.sales.per.weekday, aes(x = reorder(weekday, total.sales), y = total.sales, fill = weekday)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Items sales revenue per weekday", x = "Weekday", y = "Most sales", fill = "Weekday")

rm(total.sales.per.weekday)



## Modeling

test <- read_csv("test.csv")
head(test)


dataset_sales_train <- merge(dataset_sales_train, dataset_items[,c("item_id", "category_id")], by = "item_id", all.x = T)



# Prediction
# Use the linear model
sales_lm <- lm(item_cnt_day ~ item_id + shop_id + I(item_id^2) + I(shop_id^2), data = dataset_sales_train)

sales_lm_pred <- predict(sales_lm, test[, c("item_id", "shop_id")])

submission_lm <- data.frame(ID = test$ID,
                            item_cnt_month = sales_lm_pred)
head(submission_lm)

write.csv(submission_lm, "submission.csv", row.names = F)





summary(submission_lm)


res.aov <- aov(ID ~ item_cnt_month, data = submission_lm)
summary(res.aov)



plot(res.aov,1)



plot(res.aov,2)




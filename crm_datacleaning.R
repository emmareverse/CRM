###########DATA CLEANING PROCESS########
library(readr)
olist_order_items_dataset <- read_csv("Desktop/archive/olist_order_items_dataset.csv")
olist_order_payments_dataset <- read_csv("Desktop/archive/olist_order_payments_dataset.csv")
olist_order_reviews_dataset <- read_csv("Desktop/archive/olist_order_reviews_dataset.csv")
olist_orders_dataset <- read_csv("Desktop/archive/olist_orders_dataset.csv")
olist_products_dataset <- read_csv("Desktop/archive/olist_products_dataset.csv")
###Create dataframe for each dataset
items <- data.frame(olist_order_items_dataset)
payments <- data.frame(olist_order_payments_dataset)
reviews <- data.frame(olist_order_reviews_dataset)
orders<- data.frame(olist_orders_dataset)
products <- data.frame(olist_products_dataset)

###Inner join
ds1<-merge(orders,reviews, by="order_id",all=FALSE)
ds2<-merge(ds1, payments, by="order_id", all=FALSE)
ds3 <-merge(ds2, items, by="order_id",all=FALSE)
ds4 <-merge(ds3, products, by="product_id",all=FALSE)
#####So now ds4 contains all the variables that we need to use

###Drop useless colunms
ds5 = subset(ds4, select = -c(order_status, order_delivered_carrier_date,
                              review_id, review_comment_title,
                              review_comment_message,
                              payment_sequential,
                              payment_installments,
                              seller_id,
                              shipping_limit_date,
                              product_category_name,
                              product_weight_g,
                              product_length_cm,
                              product_height_cm,
                              product_width_cm))
print('Modified dataframe:-')
ds5

##########################
###Calculate each variable
##########################

#1. Create "late - dummy"
delivery_time<-(ds5$order_estimated_delivery_date-ds5$order_delivered_customer_date)
delivery_time
ds5 <- cbind(ds5, delivery_time)
ds5$late<- ifelse(ds5$delivery_time<0, 1, 0)

#2. Create"seller_response_time - numerical"
ds5$seller_response_time <- (ds5$order_approved_at-ds5$order_purchase_timestamp)
ds5$seller_response_time

#3. Create "purchase_day_of_week - dummy" 
##Calculate difference for weekday variable
ds5$weekday <- weekdays(ds5$order_purchase_timestamp)               
ds5$weekday 
##Create dummy variables using weekday
ds5$monday<- ifelse(ds5$weekday  == '星期一', 1, 0)
ds5$tuesday<- ifelse(ds5$weekday  == '星期二', 1, 0)
ds5$wednesday<- ifelse(ds5$weekday  == '星期三', 1, 0)
ds5$thursday<- ifelse(ds5$weekday  == '星期四', 1, 0)
ds5$friday<- ifelse(ds5$weekday  == '星期五', 1, 0)
ds5$satureday<- ifelse(ds5$weekday  == '星期六', 1, 0)
ds5$tuesday
ds5$wednesday
ds5$thursday
ds5$friday
ds5$satureday

#4. Calculate "customer_response_time - numerical"
ds5$customer_response_time <-(ds5$review_answer_timestamp-ds5$review_creation_date)
ds5$customer_response_time

#5. Calcualte "freight_value_by_order - numerical"
ds5$freight_value_by_order <- (ds5$freight_value*ds5$order_item_id)
ds5$freight_value_by_order 

#6. Calculate "total_product_value_by_order - numerical"
ds5$total_product_value_by_order<-(ds5$price*ds5$order_item_id)
ds5$total_product_value_by_order

#7. Create "payment type - dummy"
ds5$boleto<- ifelse(ds5$payment_type  == 'boleto', 1, 0)
ds5$credit_card<- ifelse(ds5$payment_type  == 'credit_card', 1, 0)
ds5$debit_card<- ifelse(ds5$payment_type  == 'debit_card', 1, 0)
ds5$voucher<- ifelse(ds5$payment_type  == 'voucher', 1, 0)
ds5$boleto
ds5$credit_card
ds5$debit_card
ds5$voucher

###So far, all the variables needed are created. Now we need to drop
###the columns that we are not going to model

#Drop useless columns
ds6 = subset(ds5, select = -c(product_id, customer_id, 
                              order_purchase_timestamp,
                              order_approved_at,
                              order_delivered_customer_date,
                              order_estimated_delivery_date, 
                              review_creation_date,
                              review_answer_timestamp,
                              payment_type,
                              price,
                              freight_value,
                              product_name_lenght,
                              delivery_time,
                              weekday))
str(ds6)

#Drop null values
ds7<-na.omit(ds6)
summary(ds7)


#Aggregate duplicated records
library("dplyr")
ds8<- ds7 %>% group_by(order_id) %>% slice(which.max(order_item_id))
ds8

#####So ds8 is our final dataset for modeling. The dataset is fully cleaned!

##Export the final dataset ds8 as cleaned_final.csv
write.csv (ds8, 'Desktop\\cleaned_final.csv', row.names = FALSE)




setwd('C:/Users/Jacky Yang/Desktop/Marketing Analytics/Segmentation')
library(data.table)
library(dplyr)
transaction = fread("transaction_table.csv",header = T)
product = fread("product_table.csv",header = T)
transaction_product = merge(transaction, product, by = "prod_id",all.x = T)
#transaction_product$tran_dt = as.Date(transaction_product$tran_dt,"%y/%m/%d")
transaction2017 = transaction_product[tran_dt>"2017-01-01" & tran_dt<"2017-12-31"]
transaction2017_sum = transaction2017[,total:=lapply(.SD,sum),by = "cust_id",.SDcol = "tran_prod_paid_amt"]
target_customer = transaction2017_sum[total>=5000,]
length(unique(target_customer$cust_id))
length(unique(target_customer$brand_desc))
target_customer[,.N,by = "brand_desc"]

#combine cust_id, tran_id,store_id
target_customer[,id:=paste0(cust_id,tran_id,store_id)]

#get the product price after discount
target_customer[, dis_unitprice:=(prod_unit_price+tran_prod_discount_amt/tran_prod_sale_qty), by = 'prod_id']
#find out the lowest price by product id as cost to calculate profit
target_customer[, min_price:=min(dis_unitprice), by = 'prod_id']
#get profit for each product
target_customer[,profit:=(dis_unitprice-min_price)*tran_prod_sale_qty]
#get profit and sale by transaction level
target_customer[,tran_profit:=sum(profit), by = c('tran_id','cust_id','store_id')]
target_customer[,total_tran_sale:=sum(tran_prod_paid_amt), by = c('tran_id','cust_id','store_id')]
#get weighted percentage 
target_customer[,tran_profit_percent:=tran_profit/total_tran_sale]


#delete the bad instance that has negative tran_prod_paid_amt
target_customer<-target_customer[!which(dis_unitprice==-0.55),]

#sanity check from a high level
summary(target_customer$min_price)
summary(target_customer$dis_unitprice)
summary(target_customer$profit)
summary(target_customer$tran_profit)
summary(target_customer$tran_profit_percent)
target_customer

#sanity check from a low level to see if the minimum price of a product looks right
target_customer[target_customer$min_price==0.89]
target_customer[target_customer$dis_unitprice==1.19]
target_customer[target_customer$prod_id==999191212]
#there are about 500 products 
zero<-target_customer[target_customer$tran_profit_percent==Inf]
zero<-rbind(zero,target_customer[target_customer$tran_profit_percent=='NaN'])
count(zero,id,sort=TRUE)
count_zero<-as.data.table()

target_customer[target_customer$tran_prod_paid_amt==0]
target_customer[target_customer$total_tran_sale==0]
target_customer[target_customer$id=='11797952.017061e+18336']
target_customer[target_customer$tran_profit_percent>1.000001]




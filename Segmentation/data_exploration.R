rm(list = ls(all = TRUE))
setwd("C:/Users/VIP/Desktop/Marketing Analytics/Projects/Segmentation")
getwd()
list.files()

if(!require("data.table")) { install.packages("data.table"); require("data.table") }
if(!require("ggplot2")) { install.packages("ggplot2"); require("ggplot2") }

Data  <- fread("product_table.csv"    ,sep=",",header=T, stringsAsFactors=F)
Data2 <- fread("transaction_table.csv",sep=",",header=T, stringsAsFactors=F)

length(unique(Data2$cust_id))
length(unique(Data2$store_id))

data <- merge(Data, Data2, by = 'prod_id')

summary(Data2)
summary(as.Date(Data2$tran_dt))
nrow(data)
length(unique(Data$prod_id))

#data[is.na(data)] <- 0

#write.table(data, file = "prodandtrans.csv",row.names=TRUE, na="",col.names=FALSE, sep=",")

#summary(data)
#length(unique(data$cust_id))
#length(unique(data$store_id))

# Who are the best customers in terms of revenues, profits, transactions/store visits, number of products, etc.?-----------------------------------------------------------------------


#funfacts
data <- data[, uniqueprod := length(unique(prod_id)), by = .(cust_id)]
data <- data[, uniquecat := length(unique(category_id)), by = .(cust_id)]
data <- data[, mosttrans:= length(unique(tran_id)) , by = .(cust_id)]
data <- data[, productspertrans := mean(length(unique(prod_id))), by =.(cust_id, tran_id)]
data <- data[, salestotal := sum(tran_prod_sale_amt), by =.(cust_id)]
data <- data[, salesqty := sum(tran_prod_sale_qty), by =.(cust_id)]

subset <- subset(data, !duplicated(cust_id))
subset <- subset[order(-salestotal),]
subset$index <- 1:nrow(subset)
subset[order(-salestotal),]
subset[order(-uniqueprod),]
subset[order(-uniquecat),]
subset[order(-mosttrans),]
subset[order(-productspertrans),]

subset[order(-salesqty),]




#prep for plots 

min(data$avgprod)
data1 <- data[, lapply(.SD,sum), by = .(cust_id),.SDcol = c("tran_prod_sale_amt", "tran_prod_sale_qty")]
data1
data1 <- data1[order(-tran_prod_sale_amt),]
data1$index <- 1:nrow(data1)
data1
data1 <- data1[, cumulative := cumsum(tran_prod_sale_amt)]
data1 <- data1[, salespercentage := cumulative/ sum(data1$tran_prod_sale_amt)]
data1 <- data1[order(-tran_prod_sale_amt),]
data1
names(data1)[names(data1) == 'index'] <- 'customers'
data1

y= 25000

ggplot(data1) + 
  geom_point(aes(x = customers,y= data1$salespercentage*y),color = 'red') + 
  geom_point(aes(x= customers, y = data1$tran_prod_sale_amt),color = 'turquoise') + 
  scale_y_continuous("Sales Volume", sec.axis=sec_axis(~./y, breaks = seq(0,1,0.2), name="% of Sales")) + geom_hline(yintercept = 22500) +
  geom_text(aes( 6000, 22500, label = "90 % of Sales", vjust = -1), size = 6) + geom_vline(xintercept = 6800) + theme_light()

write.csv(data1, file = "customers.csv")

# What are the products and product groups with the best volumes, revenues, profits, transactions, customers, etc.?-----------------------------------------------------------------------

#data2 <- data[,lapply(.SD,cumsum), by = .(category_desc_eng),.SDcol = c("tran_prod_sale_amt", "tran_prod_sale_qty")]
data2 <- data[, lapply(.SD,sum), by = .(category_desc_eng),.SDcol = c("tran_prod_sale_amt", "tran_prod_sale_qty")]
data2 <- data2[order(-tran_prod_sale_amt),]
data2$index <- 1:nrow(data2)
data2 <- data2[, cumulative := cumsum(tran_prod_sale_amt)]
data2 <- data2[, salespercentage := cumulative/ sum(data2$tran_prod_sale_amt)]
data2 <- data2[order(-tran_prod_sale_amt),]
names(data2)[names(data2) == 'index'] <- 'products'
#write.csv(file = data2, "dataproduct.csv" )

a <- sum(data2$tran_prod_sale_amt)
data2[1:25,]

head(data2, 30)
tail(data2, 10, desc)
data2[order(-tran_prod_sale_qty),]


#salespercentage
ggplot(data2) + 
  geom_point(aes(x = products,y= data2$salespercentage*2000000),color = 'red') + 
  geom_point(aes(x= products, y = data2$tran_prod_sale_amt),color = 'turquoise') + 
  scale_y_continuous("Sales Volume", sec.axis=sec_axis(~./2000000, breaks = seq(0,1,0.2), name="% of Sales")) + geom_hline(yintercept = 1800000) +
  geom_text(aes(105, 1900000, label = "90 % of Sales", vjust = -1), size = 6) + geom_vline(xintercept = 150)+ theme_light()


ggplot(data2) + 
  geom_point(aes(x = products,y= data2$tran_prod_sale_qty),color = 'red') + 
  geom_line(aes(x= products, y = data2$tran_prod_sale_amt),color = 'turquoise') + theme_light() + scale_y_continuous("Sales VOlume and Quantity")
  #scale_y_continuous("Sales Volume", sec.axis=sec_axis(~./2000000, breaks = seq(0,1,0.2), name="% of Sales")) + geom_hline(yintercept = 1800000) +
  #geom_text(aes(105, 1900000, label = "90 % of Sales", vjust = -1), size = 6) + geom_vline(xintercept = 150)+ theme_light()

#write.csv(data2, file = "products.csv")

# stores ------------------------------------------------------------------
 #funfacts

data <- data[, storemostcust := length(unique(cust_id)), by = .(store_id)]
data <- data[, storeuniquecat := length(unique(category_id)), by = .(store_id)]
data <- data[, storemosttrans:= length(unique(tran_id)) , by = .(store_id)]
#data <- data[, productspertrans := mean(length(unique(prod_id))), by =.(cust_id, tran_id)]
data <- data[, salestotalstores := sum(tran_prod_sale_amt), by =.(store_id)]
data <- data[, salesqtystores := sum(tran_prod_sale_qty), by =.(store_id)]

subset2 <- subset(data, !duplicated(store_id))
subset2 <- subset2[order(-salestotalstores),]
subset2$index <- 1:nrow(subset2)
subset2[order(-salestotalstores),]
subset2[order(-storeuniquecat),]
subset2[order(-uniquecat),]
subset2[order(-storemostcust),]
subset2[order(-productspertrans),]

subset2[order(-salesqty),]


data3 <- data[, lapply(.SD,sum), by = .(store_id),.SDcol = c("tran_prod_sale_amt", "tran_prod_sale_qty")]
data3 <- data3[order(-tran_prod_sale_qty),]
data3$stores <- 1:nrow(data3)
data3 <- data3[, cumulative := cumsum(tran_prod_sale_amt)]
data3 <- data3[, salespercentage := cumulative/ sum(data2$tran_prod_sale_amt)]
data3 <- data3[order(-tran_prod_sale_amt),]

library(scales)
ggplot(data3) + 
  geom_point(aes(x = stores,y= data3$salespercentage*1000000),color = 'red') + 
  geom_point(aes(x= stores, y = data3$tran_prod_sale_amt),color = 'turquoise') + 
  scale_y_continuous("Sales Volume", labels = comma, breaks = seq(0,1000000, 100000),sec.axis=sec_axis(~./1000000, breaks = seq(0,1,0.2), name="% of Sales")) + geom_hline(yintercept = 900065.9) +
  geom_text(aes( 350, 800050, label = "90 % of Sales", vjust = -1), size = 6) + geom_vline(xintercept = 313) + theme_light()
data3

data3[data3$salespercentage==round(0.8, digits = 4)]
your.number=0.9
data3[which(abs(data3$salespercentage-your.number)==min(abs(data3$salespercentage-your.number)))]

#write.csv(data3, file = "stores.csv")

sum(data3$tran_prod_sale_amt)

# Which stores rank the highest in volumes, revenues, profits, transactions, customers, etc.?-----------------------------------------------------------------------

data[, srev := sum(tran_prod_sale_amt), by = .(store_id)]
data[, squant := sum(tran_prod_sale_qty), by = .(store_id)]
data[, scust := sum(length(unique(cust_id))), by = .(store_id)]
#?
data[, strans := sum(length(unique(tran_id))), by = .(cust_id, store_id)]

#plots
ggplot(data[unique(store_id)], aes(reorder(data[unique(store_id)]$store_id, -data[unique(store_id)]$srev),data[unique(store_id)]$srev)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1)
ggplot(data[unique(store_id)], aes(reorder(data[unique(store_id)]$store_id, -data[unique(store_id)]$squant),data[unique(store_id)]$squant)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1)
ggplot(data[unique(store_id)], aes(reorder(data[unique(store_id)]$store_id, -data[unique(store_id)]$scust),data[unique(store_id)]$scust)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1)

#?
ggplot(data[unique(store_id)], aes(reorder(data[unique(store_id)]$store_id, -data[unique(store_id)]$strans),data[unique(store_id)]$strans)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1)


data3
summary(data3)

# miscellaneous -----------------------------------------------------------



data[, cvol := sum(tran_prod_sale_qty), by = .(category_id)]
data[, crev := sum(tran_prod_sale_amt), by = .(category_id)]
data[, ctrans := sum(length(tran_id)), by = .(category_id)]
data[, ccust := sum(length(unique(cust_id))), by = .(category_id)]
summary(data[,crev])
unique(data[,category_id])

#plots
data[unique(category_id)]
unique(data$category_id)
ggplot(data[unique(category_id)], aes(reorder(data[unique(category_id)]$category_id, -data[unique(category_id)]$cvol),data[unique(category_id)]$cvol)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1 )
ggplot(data[unique(category_id)], aes(reorder(data[unique(category_id)]$category_id, -data[unique(category_id)]$crev),data[unique(category_id)]$crev)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1) 
ggplot(data[unique(category_id)], aes(reorder(data[unique(category_id)]$category_id, -data[unique(category_id)]$totalrev),data[unique(category_id)]$totalrev)) + 
  geom_bar(color = 'orange',stat="identity", position = "identity", width = 0.1)

ggplot(data[unique(category_id)]) + geom_bar(aes(reorder(data[unique(category_id)]$category_id, -data[unique(category_id)]$crev),data[unique(category_id)]$crev),color = '#F4BB0E',stat="identity", position = "identity") +
  scale_y_continuous(limits = range(0,2600000,50000)) + theme_classic() + labs(y="Total Sales", x = "Product")
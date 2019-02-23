library(data.table)
transaction = fread("transaction_table.csv",header = T)
product = fread("product_table.csv",header = T)

# combine all the transactions with category information
transaction_product = merge(transaction, product, by = "prod_id",all.x = T)

# extract applicable columns
customers = (transaction_product[,c("prod_id","cust_id","tran_id","store_id","tran_prod_sale_amt","tran_prod_discount_amt","tran_prod_paid_amt","category_id","category_desc_eng")])

head(customers)
# add identifier for products that had promotion
customers$if_discount = ifelse(customers$tran_prod_discount_amt<0,1,0)

customers <- setDT(customers)

# product basket diversity
# # of unique products bought
customers <- customers[, uniq_prod:=uniqueN(prod_id), by="cust_id"]

# promotion sensitivity
# sales of each customer
customers <- customers[, sales_cus:=sum(tran_prod_sale_amt), by="cust_id"]

# % of sales got discounted
customers <- customers[, disc_per:=sum(tran_prod_discount_amt*-1)/sales_cus, by="cust_id"]

# % of products bought on promotion
customers <- customers[, promo_per:=sum(if_discount)/.N, by="cust_id"]

# % of sales got discounted for each products
customers <- customers[, disc_prod:=(tran_prod_discount_amt*-1)/tran_prod_sale_amt]

# store loyalty
# # of unique stores visited
customers <- customers[, unique_store:=uniqueN(store_id), by="cust_id"]

# purchase power
# total sales per customer, not included in the model

# # of transactions per customer, not included in the model
#customers <- customers[, n_tran:=uniqueN(tran_id), by="cust_id"]

# sales per transaction
customers <- customers[, sales_tran:=sum(tran_prod_paid_amt), by=c("cust_id","tran_id")]

head(customers)

# unique customer table
customers_list <- setDT(unique(customers[,c("cust_id","promo_per","unique_store","sales_cus","uniq_prod","n_tran","disc_per")]))

# assess reasonability on values
summary(customers_list)

# create table with cust_id, tran_id, sales per transaction to calculate quartiles
sales_transaction <- setDT(unique(customers[,c("cust_id","tran_id","sales_tran")]))

# create quatiles for sales amount in a transaction for a given customer
customer_list_amt_quntile = sales_transaction[,lapply(.SD,quantile),by = "cust_id",.SDcols = "sales_tran"]
customer_list_amt_quntile$quantile = rep(1:5,7920)
customers_list$Q1 = customer_list_amt_quntile[quantile == 1,c(sales_tran)]
customers_list$Q2 = customer_list_amt_quntile[quantile == 2,c(sales_tran)]
customers_list$Q3 = customer_list_amt_quntile[quantile == 3,c(sales_tran)]
customers_list$Q4 = customer_list_amt_quntile[quantile == 4,c(sales_tran)]
customers_list$Q5 = customer_list_amt_quntile[quantile == 5,c(sales_tran)]
customer_list_amt_mean = sales_transaction[,lapply(.SD,mean),by = "cust_id",.SDcols = "sales_tran"]
customers_list$mean = customer_list_amt_mean$sales_tran

# create table with cust_id, tran_id, unique products per transaction to calculate quartiles
prod_discount <- setDT(customers[,c("cust_id","prod_id","disc_prod")])

# create quatiles for unique products purchased in a transaction for a given customer
customer_list_prod_quntile = prod_discount[,lapply(.SD,quantile),by = "cust_id",.SDcols = "disc_prod"]
customer_list_prod_quntile$quantile = rep(1:5,7920)
customers_list$c.Q1 = customer_list_prod_quntile[quantile == 1,c(disc_prod)]
customers_list$c.Q2 = customer_list_prod_quntile[quantile == 2,c(disc_prod)]
customers_list$c.Q3 = customer_list_prod_quntile[quantile == 3,c(disc_prod)]
customers_list$c.Q4 = customer_list_prod_quntile[quantile == 4,c(disc_prod)]
customers_list$c.Q5 = customer_list_prod_quntile[quantile == 5,c(disc_prod)]
customer_list_prod_mean = prod_discount[,lapply(.SD,mean),by = "cust_id",.SDcols = "disc_prod"]
customers_list$c.mean = customer_list_prod_mean$disc_prod

# write output file to input to the kmeans model in Python
write.csv(customers_list,"customers_list.csv")

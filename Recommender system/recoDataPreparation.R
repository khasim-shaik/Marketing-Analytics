library(data.table)
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
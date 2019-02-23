library(data.table)
transaction = fread("transaction_table.csv",header = T)
product = fread("product_table.csv",header = T)
transaction_product = merge(transaction, product, by = "prod_id",all.x = T)

#
length(unique(transaction_product$prod_id))
transaction = transaction[,.SD,by = c("cust_id","tran_id")]

# product attribute table
transaction_product$disc_percent = -transaction_product$tran_prod_discount_amt/transaction_product$tran_prod_sale_amt
product_table = transaction_product[,.SD,by = "prod_id"]
product_table = transaction_product[,lapply(.SD,sum),by = "prod_id",.SDcols = c("tran_prod_paid_amt")]
product_quantity = transaction_product[,lapply(.SD,mean),by = "prod_id",.SDcols = c("tran_prod_sale_qty")]
product_quantity_quntile = transaction_product[,lapply(.SD,quantile),by = "prod_id",.SDcols = c("tran_prod_sale_qty")]
product_quantity_quntile$quantile = rep(1:5,10770)
product_quantity$Q1 = product_quantity_quntile[quantile == 1,c(tran_prod_sale_qty)]
product_quantity$Q2 = product_quantity_quntile[quantile == 2,c(tran_prod_sale_qty)]
product_quantity$Q3 = product_quantity_quntile[quantile == 3,c(tran_prod_sale_qty)]
product_quantity$Q4 = product_quantity_quntile[quantile == 4,c(tran_prod_sale_qty)]
product_quantity$Q5 = product_quantity_quntile[quantile == 5,c(tran_prod_sale_qty)]
product_traffic = transaction_product[,.N,by = "prod_id"]
setnames(product_traffic,old = "N",new = "num_transactions")

product_disc = transaction_product[,.N,by = "prod_id"]
product_num_disc = transaction_product[tran_prod_offer_cts == 1,.N,by = "prod_id"]
product_disc=merge(product_disc,product_num_disc,by = "prod_id",all.x = T)
product_disc = product_disc[,-"num_disc"]
product_disc[is.na(N.y)]$N.y = 0
product_disc$disc_perc = product_disc$N.y/product_disc$N.x

product_disc_amt_quntile = transaction_product[,lapply(.SD,quantile),by = "prod_id",.SDcols = "disc_percent"]
product_disc_amt_quntile$quantile = rep(1:5,10770)
product_disc$Q1 = product_disc_amt_quntile[quantile == 1,c(disc_percent)]
product_disc$Q2 = product_disc_amt_quntile[quantile == 2,c(disc_percent)]
product_disc$Q3 = product_disc_amt_quntile[quantile == 3,c(disc_percent)]
product_disc$Q4 = product_disc_amt_quntile[quantile == 4,c(disc_percent)]
product_disc$Q5 = product_disc_amt_quntile[quantile == 5,c(disc_percent)]
product_disc_amt_mean = transaction_product[,lapply(.SD,mean),by = "prod_id",.SDcols = "disc_percent"]
product_disc$mean = product_disc_amt_mean$disc_percent

product_table_full = merge(product_table, product_traffic,by = "prod_id")
product_table_full = merge(product_table_full,product_quantity,by = "prod_id")
product_table_full = merge(product_table_full,product_disc,by = "prod_id")
product_table_full = product_table_full[,-c("N.x","N.y")]

setnames(product_table_full,old = "tran_prod_paid_amt",new = "sales_avg")
setnames(product_table_full,old = "mean",new = "discount_avg")
setnames(product_table_full,old = "disc_perc",new = "discounted_transaction_p")

write.csv(product_table_full,file = "product_table_full.csv")

store_loyalty = unique(transaction_product[,c("store_id","cust_id","tran_id")])
store_loyalty = store_loyalty[,.N,by = c("store_id","cust_id")]
store_loyalty_avg = store_loyalty_avg[,lapply(.SD,mean),by = "store_id"]

store_loyalty_quantile = store_loyalty[,lapply(.SD,quantile),by = "store_id",.SDcols = "N"]

store_loyalty_quantile$quantile = rep(1:5,2105)

store_loyalty = unique(store_loyalty[,"store_id"])
store_loyalty$loyal_avg = store_loyalty_avg$loyal_avg
store_loyalty$Q1 = store_loyalty_quantile[quantile == 1,"N"]
store_loyalty$Q2 = store_loyalty_quantile[quantile == 2,"N"]
store_loyalty$Q3 = store_loyalty_quantile[quantile == 3,"N"]
store_loyalty$Q4 = store_loyalty_quantile[quantile == 4,"N"]
store_loyalty$Q5 = store_loyalty_quantile[quantile == 5,"N"]

write.csv(store_loyalty,file = "store_loyalty.csv")

# cluster evaluation
product_cluster = fread("product_clustered.csv",header = T)
product_cluster_eval = product_cluster[,lapply(.SD,mean),by = "cluster"]

product_center = fread("product_centers.csv",header = T)
product_center = merge(product_center, product[,c("prod_id","category_desc_eng")],by = "prod_id")



cluster1_product_category = merge(product_cluster[cluster == 1,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster1_product_category = cluster1_product_category[,.N,by = "category_desc_eng"]
cluster1_product_category = cluster1_product_category[order(N,decreasing = T)]

cluster2_product_category = merge(product_cluster[cluster == 2,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster2_product_category = cluster2_product_category[,.N,by = "category_desc_eng"]
cluster2_product_category = cluster2_product_category[order(N,decreasing = T)]

cluster3_product_category = merge(product_cluster[cluster == 3,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster3_product_category = cluster3_product_category[,.N,by = "category_desc_eng"]
cluster3_product_category = cluster3_product_category[order(N,decreasing = T)]

cluster4_product_category = merge(product_cluster[cluster == 4,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster4_product_category = cluster4_product_category[,.N,by = "category_desc_eng"]
cluster4_product_category = cluster4_product_category[order(N,decreasing = T)]

cluster5_product_category = merge(product_cluster[cluster == 5,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster5_product_category = cluster5_product_category[,.N,by = "category_desc_eng"]
cluster5_product_category = cluster5_product_category[order(N,decreasing = T)]

cluster0_product_category = merge(product_cluster[cluster == 0,"prod_id"],product[,c("prod_id","category_desc_eng")])
cluster0_product_category = cluster0_product_category[,.N,by = "category_desc_eng"]
cluster0_product_category = cluster0_product_category[order(N,decreasing = T)]



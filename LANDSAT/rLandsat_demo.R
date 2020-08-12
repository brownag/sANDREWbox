devtools::install_github('atlanhq/rLandsat')

library(rLandsat)

# get all the product IDs for path and row
result <- landsat_search(min_date = "2018-01-01", max_date = "2018-12-31", 
                         path_master = 42,	row_master=34)

# inputting espa creds
espa_creds("andrew.g.brown@usda.gov", "th1sisaplaintextpassword")

# getting available products
prods <- espa_products(result$product_id)
prods <- prods$master

# placing an espa order
result_order <- espa_order(result$product_id[11], product = c("sr_ndvi"),
                          projection = "lonlat",
                          order_note = "HUC10_WillowCreek_2018")
order_id <- result_order$order_details$orderid

# getting order status
durl <- espa_status(order_id = order_id, getSize = F)
downurl <- durl$order_details

# download; after the order is complete
landsat_download(download_url = downurl$product_dload_url, dest_file = getwd())

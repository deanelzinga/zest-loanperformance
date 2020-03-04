library(data.table)
library(magrittr)
library(bit64)
library(stringi)
library(ggplot2)
library("pheatmap")
options(scipen=10)
options(digits = 1)


# library(h2o)

setwd("~/zest-homework/")

# ----------------
# Loan APPLICATION
# ----------------
cat("Reading application data... ")
app <- fread("data/application_data.txt", sep = "\t",
             colClasses = c(customer_id = "character", amount_requested = "integer", 
                           birth_date = "character", status = "character", residence_rent_or_own = "integer", 
                           monthly_rent_amount = "integer", bank_account_direct_deposit = "integer", 
                           application_when = "character", loan_duration = "integer", 
                           payment_ach = "integer", num_payments = "integer", payment_amount = "double", 
                           amount_approved = "integer", duration_approved = "integer", 
                           payment_amount_approved = "double", address_zip = "character", 
                           email = "character", bank_routing_number = "character", email_duration = "character", 
                           residence_duration = "character", bank_account_duration = "character", 
                           payment_frequency = "character", home_phone_type = "character", 
                           other_phone_type = "character", how_use_money = "character", 
                           monthly_income_amount = "integer", raw_l2c_score = "integer", 
                           raw_FICO_telecom = "integer", raw_FICO_retail = "integer", 
                           raw_FICO_bank_card = "integer", raw_FICO_money = "integer"))
cat("done.\n")

# -----------------------------------------------------------------
# DEPRECATE in favor of tables: Dev-time only: write ZIP Code table for median income data-entry:
# -----------------------------------------------------------------
# fwrite(app[, .N, keyby = address_zip], "data/zipcode_median_income_pad.txt", sep = "\t") 
# app[stri_length(bank_routing_number) %in% 
#      c("021000021", "011401533", "091000019", "21000021", "11401533", "91000019"), ]
# app[, bank_routing_number_pad := sprintf("%09s", bank_routing_number) ]

# fwrite(app[, .(bank_routing_citystate = "", .N), keyby = bank_routing_number_pad],
#       "data/bank_routing_citystate_pad.txt", sep = "\t")

cat("Creating column for email host:\n")
app[, email_host := tstrsplit(email, "@", fixed=TRUE)[2]]
app[, customer_id9 := customer_id]
cat("Creating column for 0-padded, 10-hex-digit customer ID:\n")
app[, customer_id := 
      sprintf("%010s", customer_id9)]
app[customer_id != customer_id9, sprintf("%s ==> %s", customer_id9, customer_id)]
app[, birth_date_std := as.Date(app$birth_date, format = "%m/%d/%Y")]
app[, application_when_std := as.POSIXct(app$application_when, format = "%m/%d/%Y %H:%M",tz = "GMT")]

app[, age := floor(as.numeric(difftime(application_when_std, birth_date_std, units = "days")/365.25))]
app[, monthly_payment_amount := payment_amount * num_payments / loan_duration]

# ----------------
# Loan PERFORMANCE
# ----------------
cat("Reading loan performance data...\n")
perf <- fread("data/loan_performance.txt", sep = "\t")
dput(as.vector(lapply(X = perf, FUN = typeof)))
cat("Splitting loan-performance `idLoan` column into customer id and loan number:\n")
perf[, c("id_upper", "loan") := tstrsplit(idLoan, "-", fixed = TRUE)]
cat("Creating lowercase loan-performance customer_id column:\n")
perf[, customer_id := tolower(id_upper)]
perf[, good01 := as.integer(flgGood == "Good")]
app2 <- app %>% merge(perf, by = c("customer_id"), all.x = TRUE)

# ---------------------
# Troubleshooting the join on customer ID, between loan application (mangled IDs!) 
# and loan performance
# ---------------------
cat("Checking join misses:\n")
cat("Customer IDs in application table and NOT in performance table:\n")
unique(app[!(customer_id %in% perf$customer_id), customer_id])
length(unique(perf[!(customer_id %in% app$customer_id), customer_id]))

# ----------------------------------------------------------------------------------
# Warning about Excel mangling Customer IDs (originally causing join miss in error!)
# ----------------------------------------------------------------------------------
cat("NOTE: Attention to data-intake procedures required!\n")
cat("Defect found in prior data-load.\n")
cat("Probably caused by Excel `General` cell format.\n")
mangled_ids <- c("00413198E8", "2101486E84","0902431691")
cat("These Customer IDs were mangled:\n")
print(mangled_ids)
cat("They became something like this:\n")
print(as.numeric(mangled_ids))
# During development: output column types to provide template for fread(...) above:
# dput(as.vector(lapply(X = app, FUN = typeof)))

# --------------------------------------------------------
# ROUTING NUMBER check. Manual work at...
# https://www.usbanklocations.com/check-routing-number.php
# --------------------------------------------------------
routing <- fread("data/bank_routing_citystate_filled.txt", sep = "\t",
                 colClasses = 
                   c(bank_routing_number = "character", fedach_name = "character", 
                     fedach_citystate = "character", loans_per_routing_number = "integer", 
                     comment = "character"))
routing[, bank_routing_number_found := as.integer(!is.na(fedach_name))]
print(head(routing))
# During development: output column types to provide template for fread(...) above:
# dput(as.vector(lapply(X = routing, FUN = typeof)))
app3 <- app2 %>%
  merge(routing, by=c("bank_routing_number"), all.x = TRUE)

# ------------------------------------------------------------------
# IRS.gov
# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-2017-zip-code-data-soi
# 
# DEVELOPMENT ONLY... deprecated in favor of full Census community survey table
# ------------------------------------------------------------------
irs_soi_tax_stats2017zipcode <- fread("data/zipcode2017/17zpallagi.csv", sep = ",", integer64 = "integer64",
                 colClasses =  c(STATEFIPS = "integer", STATE = "character", 
                                 zipcode = "integer", agi_stub = "integer"))
# During development: output column types to provide template for fread(...) above:
# dput(as.vector(lapply(X = zipcode, FUN = typeof)))

# ------------------------------------------------------------------
# CENSUS median income per ZIP Code, plus histogram of income ranges
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1901&prodType=table
# ------------------------------------------------------------------
acs_17_path <- "data/ACS_17_5YR_S1901 transposed annotations-separate/ACS_17_5YR_S1901.csv"
acs_17 <- fread(acs_17_path,
                header = TRUE,
             )

names(acs_17) <- tolower(names(acs_17))
# Column descriptions are in row 2 of the file, row 1 of the data.table:
acs_17.col_desc <- acs_17[1, lapply(.SD, tolower)]

# Remove column description row:
acs_17 <- acs_17[2:.N,]

if (length(unique(acs_17$geo.id2)) == nrow(acs_17)) {
  cat("ZIP Code designation area, geo.id2, is unique per row and so works as a key.\n")
  setkey(acs_17, geo.id2)
}
hc_cols <- names(acs_17)[names(acs_17) %like% "hc.*"]
# Find all columns with "median" in the description:
acs_17.col_desc[, .SD, .SDcols = acs_17.col_desc[1] %like% ".*median.*"] %>%
  t %>% 
  print
# ==> 
# hc01_est_vc13 "households; estimate; median income (dollars)"                    
# hc01_moe_vc13 "households; margin of error; median income (dollars)" ....
acs_17[, zip_median_hh_income := as.numeric(hc01_est_vc13)]

acs_17_zip_median_income <- acs_17[, .(address_zip = geo.id2, 
                                        zip_median_hh_income = as.numeric(hc01_est_vc13), 
                                        zip_median_hh_income_moe = as.numeric(hc01_moe_vc13))]

dput(as.vector(lapply(X = acs_17_zip_median_income, FUN = typeof))) # Dev only
app4 <- app3 %>% 
  merge(acs_17_zip_median_income, by = c("address_zip"), all.x = TRUE)
cat("Mean rate of good01 for all cases:\n")
mean_good_rate <- sum(app4$good01==1, na.rm = T)/nrow(app4)
print(mean_good_rate)
pgood <- c(1-mean_good_rate, mean_good_rate)
# ==>  0.4555053

# ---------------------------
# Dev roughcheck whether email-host hunch jumps out...
# --------------------------
print(app4[, .N, by = .(email_host, good01)][order(email_host, good01)])
# ==> Modest. It doesn't look TOO different from random.
# -------------------------------------------------------------------
# Chi-Squared tests, rough and ready to check for outlier proportions:
# -------------------------------------------------------------------
chi.sq.tests <- list(
  aol.com = chisq.test(x = c(73, 64), p = pgood), # aol.com # 
  bing.com = chisq.test(x = c(75, 60), p = pgood), # bing.com
  gmail.com = chisq.test(x = c(62, 56), p = pgood), # gmail.com
  hotmail.com = chisq.test(x = c(69, 68), p = pgood), # hotmail.com
  yahoo.com = chisq.test(x = c(78, 54), p = pgood)) # yahoo.com
app4[,.N, by = email_host]
email_good <- data.table(email_host = c("aol.com", "bing.com", "gmail.com", "hotmail.com", "yahoo.com"),
           good = c(64, 60, 56, 68, 54),
           N = c(140, 136, 118, 137, 132))
email_good[, pgood:=good/N]
email_good[, email_zgood := scale(pgood)]

app5 <- app4 %>% merge(email_good[, .(email_host, email_zgood)], by = c("email_host"), all.x = T)
app5[, monthly_income_surplus := monthly_income_amount - monthly_rent_amount - monthly_payment_amount]
app5_num_cols <- names(app5)[sapply(app5, is.numeric)]
pred_cols <- c("amount_requested", "residence_rent_or_own", "monthly_rent_amount", 
               "bank_account_direct_deposit", "loan_duration", 
               # "payment_ach", # ???
               "num_payments", "payment_amount", 
               "monthly_income_amount", 
               "raw_l2c_score", 
               "raw_FICO_telecom", "raw_FICO_retail", "raw_FICO_bank_card", "raw_FICO_money", 
               "age", "monthly_payment_amount", "good01", # "loans_per_routing_number", 
               "bank_routing_number_found", 
               "zip_median_hh_income", 
               "email_zgood", "monthly_income_surplus", "email_")
app5_pred_cols <- intersect(app5_num_cols, pred_cols)


# --------------------------
# `how_use_money`: any good?
# --------------------------
cat("Inspect these totals for `how_use_money`:\n")
data.table::dcast(app5[, .N, by = .(how_use_money, flgGood)], 
                  how_use_money ~ flgGood, value.var = "N") %>%
  .[, .(how_use_money, Bad, Good)] %>%
  .[order(-Good)]
cat("Hmm, `Bills (General)` looks really good at over 50% Good, but...\n")
cat("...to me, `Bills` means the same as `Bills (General)`.\n")
cat("Add these Good & Bad totals together, and it's nothing special.\n")

# ----------------------------------
# Correlation heatmap and clustering
# ----------------------------------
app5_num <- app5[, ..app5_pred_cols]
app5_num_comp <- na.omit(app5_num)
app5_num_norm <- scale(app5_num_comp)
cols_cor <- cor(app5_num_norm, use = "pairwise.complete.obs", method = "pearson")

# ----------
# HEAT MAP
# ----------
set.seed(424)
cols_cor %>% pheatmap(scale = "column",
  clustering_distance_cols = as.dist(asin((1-cols.cor)/2)),
  cluster_rows = FALSE,
  # clustering_distance_rows = as.dist(1 - cols.cor),
  clustering_method = "complete"
)

# -------------------------------------
# RANK by correlation with GOOD outcome
# -------------------------------------
cordt <- as.data.table(cols.cor, keep.rownames = "name")
cordt[name != "good01", .(name, good01, goodrank = frank(-good01))][order(goodrank)]
app5[, goodc := 2*good01 - 1]
app5[, FICO_sumlog := log(raw_FICO_money) + 
       # log(raw_FICO_bank_card) +
       # log(raw_FICO_retail) +
             log(raw_FICO_telecom)
     ]
app5[, raw_FICO_money_norm := scale(log(raw_FICO_money))]

app5[, FICO_sumlog_norm := scale(FICO_sumlog)]

app5[raw_FICO_money > 500, ] %>%
  ggplot(aes(x=raw_FICO_money,y = goodc)) + 
  labs(title="Loess-smoothed plot of loan outcome",
       y = "Loan performance: Good=1, Bad=-1") +
  geom_line() + 
  geom_smooth(method='loess') + xlim(550, 660)
  
app5 %>%
  ggplot(aes(x=age,y = goodc)) + 
  geom_line() + 
  geom_smooth(method='loess') 
app5 %>%
  ggplot(aes(x=age,y = goodc)) + 
  geom_line() + 
  geom_smooth(method='loess')
app5[, .(bank_account_direct_deposit, flgGood)] %>%
  table %>%
  addmargins 
app5[, .(age = mean(age)), by = .(email_host)]
plot(density(app5[raw_FICO_telecom > 500, scale(raw_FICO_money)]))
app4_cor_mat <- stats::cor(app4_num, use="complete.obs")

length(unique(app5$address_zip))
# app4_cor_mat[app4_cor_mat == 1.0] <- 0
# app4_cor_mat2 <- lessR::corReorder(app4_cor_mat, order = "chain", chain_first = 1, heat_map = TRUE)
# options(theme = "dodgerblue")

# Dev atttempt 1 headmap with ggplot2

# library(reshape2)
# app4_cor_mat_melt <- app4_cor_mat %>% melt
# print(head(app4_cor_mat_melt))
# app4_cor_mat_melt %>%
# ggplot(aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()

library(pheatmap)

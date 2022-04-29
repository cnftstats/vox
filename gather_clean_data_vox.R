# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(httr)

# Variables ----------------------------------------------------------------------------------------
policy_id <- "a8c68ff1e130c79cd0cad73357670b3b4fd11a0708319a99be14e860"
project <- "Boss Planet Vox Cat"
project_label <- "vox"
time_now <- as_datetime(now())


# Databases ----------------------------------------------------------------------------------------
RAR <- readRDS(sprintf("data/RAR_%s.rds", project_label))


# Functions ----------------------------------------------------------------------------------------
extract_num <- function(x) as.numeric(gsub("[^0-9\\-]+","",as.character(x)))

loj <- function (X = NULL, Y = NULL, onCol = NULL) {
  if (truelength(X) == 0 | truelength(Y) == 0) 
    stop("setDT(X) and setDT(Y) first")
  n <- names(Y)
  X[Y, `:=`((n), mget(paste0("i.", n))), on = onCol]
}


# CNFT listings ------------------------------------------------------------------------------------
api_link_cnft <- "https://api.cnft.io/market/listings"

query <- function(page, url, project, sold) {
  httr::content(httr::POST(
    url = url, 
    body = list(
      search = "", 
      types = c("listing", "offer", "bundle"),
      project = project, 
      sort = list(`_id` = -1L), 
      priceMin = NULL, 
      priceMax = NULL, 
      page = page, 
      verified = TRUE, 
      nsfw = FALSE, 
      sold = sold, 
      smartContract = NULL
    ), 
    encode = "json"
  ), simplifyVector = TRUE)
}

query_n <- function(url, project, sold, n = "all") {
  if (n == "all") n <- query(1L, url, project, sold)[["count"]]
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- query(i, url, project, sold)[["results"]]
    if (length(out[[i]]) < 1L) return(out[seq_len(i - 1L)])
  }
  out
}

.CNFT <- query_n(api_link_cnft, project, sold = FALSE) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

if (nrow(.CNFT) > 0) {
  .CNFT[, link := paste0("https://cnft.io/token/", ifelse(is.na(X_id), `_id`, X_id))]
  
  # Initialize data.table
  CNFT <- data.table(asset = NA, asset_number = NA, type = NA, price = NA,
                     last_offer = NA, sc = NA, market = NA, link = NA)
  
  for (i in 1:nrow(.CNFT)) {
    CNFT <- rbind(CNFT, data.table(asset        = .CNFT[i, assets[[1]]$metadata$name],
                                   asset_number = as.numeric(NA),
                                   type         = .CNFT[i, type],
                                   price        = .CNFT[i, price],
                                   last_offer   = .CNFT[i, offers],
                                   sc           = .CNFT[i, smartContractTxid],
                                   market       = "cnft.io",
                                   link         = .CNFT[i, link]))
  }
  
  CNFT <- CNFT[2:nrow(CNFT)] # Clear first row from initialization
  CNFT[, asset_number := extract_num(asset)]
  CNFT[, price        := price/10**6]
  CNFT[, sc           := ifelse(is.na(sc), "no", "yes")]
  
  for (i in 1:nrow(CNFT)) {
    .offers <- CNFT[i, last_offer[[1]]]
    if (nrow(.offers) == 0) {
      CNFT[i, last_offer := NA]
    } else {
      CNFT[i, last_offer := max(.offers$offer/10**6)]
    }
  }
}


# CNFT sales ---------------------------------------------------------------------------------------
.CNFTS <- query_n(api_link_cnft, project, sold = TRUE, n = 11) |>
  lapply(data.table) |> rbindlist(fill = TRUE)

if (nrow(.CNFTS) > 0) {
  .CNFTS <- .CNFTS[!is.na(soldAt)]
  
  # Initialize data.table
  CNFTS <- data.table(asset = NA, asset_number = NA, price = NA,
                      market = NA, sold_at = NA)
  
  for (i in 1:nrow(.CNFTS)) {
    CNFTS <- rbind(CNFTS, data.table(asset        = .CNFTS[i, assets[[1]]$metadata$name],
                                     asset_number = as.numeric(NA),
                                     price        = .CNFTS[i, price],
                                     market       = "cnft.io",
                                     sold_at      = .CNFTS[i, soldAt]))
  }
  
  CNFTS <- CNFTS[2:nrow(CNFTS)] # Clear first row from initialization
  CNFTS[, asset_number  := extract_num(asset)]
  CNFTS[, price         := price/10**6]
  CNFTS[, market        := "cnft.io"]
  CNFTS[, sold_at       := as_datetime(sold_at)]
  CNFTS[, sold_at_hours := difftime(time_now, sold_at, units = "hours")]
  CNFTS[, sold_at_days  := difftime(time_now, sold_at, units = "days")]
  
  CNFTS <- CNFTS[order(-sold_at), .(asset, asset_number, price, sold_at, sold_at_hours, 
                                    sold_at_days, market)]
  CNFTS <- CNFTS[sold_at_hours <= 24*3]
}


# JPG listings -------------------------------------------------------------------------------------
JPG_list <- list()
p <- 1
while (TRUE) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/listings?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  if (nrow(X) == 0) break
  JPG_list[[p]] <- X
  p <- p + 1
}

JPG <- rbindlist(JPG_list)

JPG[, link           := paste0("https://www.jpg.store/asset/", asset_id)]
JPG[, asset          := display_name]
JPG[, price          := price_lovelace/10**6]
JPG[, sc             := "yes"]
JPG[, market         := "jpg.store"]
JPG[, asset_number   := extract_num(asset)]

JPG <- JPG[, .(asset, asset_number, type = "listing", price, last_offer = NA, sc, market, link)]


# JPG sales ----------------------------------------------------------------------------------------
JPGS_list <- lapply(1:7, function(p) {
  api_link <- sprintf("https://server.jpgstoreapis.com/policy/%s/sales?page=%d", policy_id, p)
  X <- data.table(fromJSON(rawToChar(GET(api_link)$content)))
  return(X)
})

JPGS <- rbindlist(JPGS_list)

JPGS[, asset          := display_name]
JPGS[, price          := price_lovelace/10**6]
JPGS[, market         := "jpg.store"]
JPGS[, asset_number   := extract_num(asset)]
JPGS[, sold_at        := as_datetime(confirmed_at)]
JPGS[, sold_at_hours  := difftime(time_now, sold_at, units = "hours")]
JPGS[, sold_at_days   := difftime(time_now, sold_at, units = "days")]

JPGS <- JPGS[order(-sold_at), .(asset, asset_number, price, sold_at, sold_at_hours,
                                sold_at_days, market)]
JPGS <- JPGS[sold_at_hours <= 24*3]


# Merge markets data -------------------------------------------------------------------------------
# Listings
if (exists("CNFT")) {
  DT <- rbindlist(list(CNFT, JPG), fill = TRUE, use.names = TRUE)
} else {
  DT <- copy(JPG)
}

# Sales
if (exists("CNFTS")) {
  DTS <- rbindlist(list(CNFTS, JPGS), fill = TRUE, use.names = TRUE)
} else {
  DTS <- copy(JPGS)
}

# Add data collection timestamp
DT[, data_date := time_now]
DTS[, data_date := time_now]


# Rarity and ranking -------------------------------------------------------------------------------
setDT(DT); setDT(RAR)
loj(DT, RAR, "asset_number")

setDT(DTS); setDT(RAR)
loj(DTS, RAR, "asset_number")

DT[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                         asset_rank %between% c(11,   100),  "11-100",
                         asset_rank %between% c(101,  1000), "101-1000",
                         asset_rank %between% c(1001, 1500), "1001-2000",
                         asset_rank %between% c(2001, 3000), "2001-3000",
                         asset_rank %between% c(3001, 4000), "3001-4000",
                         asset_rank %between% c(4001, 5000), "4001-5000",
                         asset_rank %between% c(5001, 6000), "5001-6000",
                         asset_rank %between% c(6001, 7000), "6001-7000",
                         asset_rank %between% c(7001, 8000), "7001-8000",
                         asset_rank %between% c(8001, 9000), "8001-9000",
                         asset_rank %between% c(9001, 9999), "9001-9999") %>% 
     factor(levels = c("1-10",
                       "11-100",
                       "101-1000",
                       "1001-2000",
                       "2001-3000",
                       "3001-4000",
                       "4001-5000",
                       "5001-6000",
                       "6001-7000",
                       "7001-8000",
                       "8001-9000",
                       "9001-9999"))]

DTS[, rank_range := fcase(asset_rank %between% c(1,    10),   "1-10",
                          asset_rank %between% c(11,   100),  "11-100",
                          asset_rank %between% c(101,  1000), "101-1000",
                          asset_rank %between% c(1001, 1500), "1001-2000",
                          asset_rank %between% c(2001, 3000), "2001-3000",
                          asset_rank %between% c(3001, 4000), "3001-4000",
                          asset_rank %between% c(4001, 5000), "4001-5000",
                          asset_rank %between% c(5001, 6000), "5001-6000",
                          asset_rank %between% c(6001, 7000), "6001-7000",
                          asset_rank %between% c(7001, 8000), "7001-8000",
                          asset_rank %between% c(8001, 9000), "8001-9000",
                          asset_rank %between% c(9001, 9999), "9001-9999") %>% 
      factor(levels = c("1-10",
                        "11-100",
                        "101-1000",
                        "1001-2000",
                        "2001-3000",
                        "3001-4000",
                        "4001-5000",
                        "5001-6000",
                        "6001-7000",
                        "7001-8000",
                        "8001-9000",
                        "9001-9999"))]


# Large format -------------------------------------------------------------------------------------
.cols <- names(DT)[names(DT) %like% "^asset_trait_"]
.cols <- .cols[.cols != "asset_trait_num"]
DTL <- data.table(gather(DT, key, value, all_of(.cols)))

DTL[, trait_category := strsplit(value, "_")[[1]][1], 1:nrow(DTL)]
DTL[, trait_category := gsub("\\.", " ", trait_category)]
DTL[, trait          := strsplit(value, "_")[[1]][2], 1:nrow(DTL)]


# Save ---------------------------------------------------------------------------------------------
saveRDS(DT, file = "data/DT.rds")
saveRDS(DTL, file = "data/DTL.rds")
saveRDS(DTS, file = "data/DTS.rds")


# Database evolution -------------------------------------------------------------------------------
DTE <- copy(DT)
.file_name <- sprintf("data/DTE_%s.rds", project_label)
if (file.exists(.file_name)) {
  cat("File data/DTE exists:", file.exists(.file_name), "\n")
  DTE_old <- readRDS(.file_name)
  DTE <- rbindlist(list(DTE, DTE_old), fill = TRUE)
  DTE <- DTE[difftime(time_now, data_date, units = "hours") <= 24] # Only retain last 24 hours
}
saveRDS(DTE, file = .file_name)

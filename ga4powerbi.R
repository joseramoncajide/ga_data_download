#devtools::install_github("MarkEdmondson1234/googleAnalyticsR", force = T)
Sys.setenv(GA_AUTH_FILE = "C:/Program Files/R/R-3.3.0/SM-SAVIA-5f1079c39bdd.json")
library(googleAnalyticsR)
library(lubridate)
library(parallel)
library(doParallel)
library(data.table)

numCores <- detectCores()
cl <- makeCluster(numCores - 2) 
registerDoParallel(cl) 

options(googleAuthR.client_id = "707893024048-9jl1rljaegll19nfim1q6k2jbt7phu6s.apps.googleusercontent.com")
options(googleAuthR.client_secret = "An4D9bd2u-bAGNmal9VRgQJ5")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")

account_list <- google_analytics_account_list()

ga_id <- account_list$viewId[1]

gadata <- google_analytics(id = ga_id, 
                           start="2016-09-10", end="2016-09-19", 
                           metrics = c("productDetailViews"), 
                           dimensions = c("date", "productSku", "dimension4"))

head(gadata)


##### PARALLEL

date_range <- c('2016-09-10' ,'yesterday')

sessions.ga <- c(
  metrics = "sessions,users,pageviews,avgSessionDuration,percentNewSessions",
  dimensions = "date"
)

bounces.ga <- c(
  metrics = 'bounces',
  dimensions = 'date'
)
getGaData4 <- function(query, ga_id, date_range) {
  require(data.table)
  #service_token <- gar_auth_service(json_file="./ga-api-r-30a82c2d29e7.json")
  # googleAuthR::gar_auth()
  # ga_id <- view
  ga <- google_analytics_4(
    ga_id,
    date_range = as.character(date_range),
    metrics = as.vector(unlist(strsplit(query['metrics'], ","), use.names = F)),
    dimensions = as.vector(unlist(strsplit(query['dimensions'], ","), use.names = F))
  )
  return(as.data.table(ga))
}

ga_queries <- list(bounces.ga, sessions.ga)

res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate", "data.table"), .combine='list', .multicombine=TRUE, .inorder = T) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

merge.by.time <- function(a, b) {  
  merge(a, b, by='date', suffixes=c('', ncol(a)))
}

res5 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate", "data.table"), .combine='merge.by.time', .multicombine=TRUE, .inorder = T) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)


sessions.df <- res4[[1]]
sessions.df$target <- mean(sessions.df$bounces)


bounces.df <- res4[[2]]
merged.df <- res5

library(googleAnalyticsR)
ga_auth()
google_analytics_account_list()


options(googleAuthR.client_id = "1012693244539-c4sec4hjq0ufcgc5dpt4ebcn20050okf.apps.googleusercontent.com")
options(googleAuthR.client_secret = "hpNmMQ9qpKsEjrjCeDPvQRt8")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")

account_list <- google_analytics_account_list()
ga_id <- account_list[23,'viewId']


ga_id <- '83321718' #EAM
start.date <- '2016-05-01'
end.date <- '2016-05-31'



gadata <- google_analytics(id = ga_id, 
                           start="2015-08-01", end="2015-08-02", 
                           metrics = "ga:sessions,ga:bounceRate", 
                           dimensions = c("source", "medium"))


# sessions.ga <- c(
#   id = ga_id,
#   start = start.date,
#   end = end.date,
#   metrics = "sessions",
#   dimensions = "date"
#   )
# 
# bounces.ga <- c(
#   id = ga_id,
#   start = start.date,
#   end = end.date,
#   metrics = "ga:bounces, ga:bounceRate",
#   dimensions = "date"
# )
# sessions.ga['metrics']


sessions.ga <- c(
  metrics = "sessions",
  dimensions = "date"
)

bounces.ga <- c(
  metrics = 'sessions,bounces',
  dimensions = 'date'
)
bounces.ga['metrics']
bounces.ga[4]

getGaData <- function(query) {
  service_token <- gar_auth_service(json_file="./ga-api-r-30a82c2d29e7.json")
  # googleAuthR::gar_auth()
  ga_id <- query['id']
  google_analytics(
    ga_id,
    start = query['start'],
    end = query['end'],
    metrics = query['metrics'],
    dimensions = query['dimensions']
  )
}

getGaData(bounces.ga)

getGaData4 <- function(query, ga_id, date_range) {
  service_token <- gar_auth_service(json_file="./ga-api-r-30a82c2d29e7.json")
  # googleAuthR::gar_auth()
  # ga_id <- view
  ga <- google_analytics_4(
    ga_id,
    date_range,
    metrics = as.vector(unlist(strsplit(query['metrics'], ","), use.names = F)),
    dimensions = as.vector(unlist(strsplit(query['dimensions'], ","), use.names = F))
  )
  return(ga)
}

bounces.ga
sessions.ga

# as.vector(unlist(strsplit(bounces.ga['metrics'], ","), use.names = F))


date_range = c("2015-07-30","2015-10-01")
getGaData4(bounces.ga, ga_id, date_range)
getGaData4(sessions.ga, ga_id, date_range)

ga_queries = ls()[grepl("*.ga$", ls())]

ga_queries <- list(bounces.ga, sessions.ga)



# library(googleAuthR)
# service_token <- gar_auth_service(json_file="./ga-api-r-30a82c2d29e7.json")
# googleAuthR::gar_auth()

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

merge.by.time <- function(a, b) {  
  merge(a, b, by='date', suffixes=c('', ncol(a)))
}

res<- system.time(
  foreach(i=1:length(ga_queries),.combine='merge.by.time') %do% getGaData(ga_queries[[i]])
)

system.time(
  foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR"), .combine='merge.by.time') %dopar% getGaData(ga_queries[[i]])
)

# res.df <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR"), .combine='merge.by.time') %dopar% getGaData(ga_queries[[i]])

#Google Analytics API v4
# res4.df <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR"), .combine='merge.by.time') %dopar% getGaData4(ga_queries[[i]])

#OK
res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR"), .combine='list', .multicombine=TRUE) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

res4





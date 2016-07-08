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
  metrics = "sessions,users",
  dimensions = "yearWeek"
)

bounces.ga <- c(
  metrics = 'bounces',
  dimensions = 'yearWeek'
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
    date_range = as.character(date_range),
    metrics = as.vector(unlist(strsplit(query['metrics'], ","), use.names = F)),
    dimensions = as.vector(unlist(strsplit(query['dimensions'], ","), use.names = F))
  )
  return(ga)
}

bounces.ga
sessions.ga

# as.vector(unlist(strsplit(bounces.ga['metrics'], ","), use.names = F))

to.date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
library(lubridate)

# start.Date <- function(last_Day){
#   
#   days <- seq(last_Day-6,last_Day,by='day')
#   from.date <- days[weekdays(days)=='lunes']
#   return(from.date)
#   
# }

start.Date <- function(last_Day){
  
  days <- floor_date(seq(last_Day-365,last_Day,by='week'), 'week') # Ojo. Tiene que devolver una semana más.
  from.date <- days - 6
  # from.date <- days[weekdays(days+1)=='lunes']
  return(min(from.date))
  
}



last_Day <- floor_date(to.date, "week")

first_Day <- start.Date(last_Day)

# last_previous_week_Day <- first_Day-1
# first_previous_week_Day <- start.Date(last_previous_week_Day)

# date_range <- c(first_Day ,last_Day,first_previous_week_Day,last_previous_week_Day)


# prev.days <- seq(previous_sunday-6,previous_sunday,by='day')
# from.date <- prev.days[weekdays(prev.days)=='lunes']
# 
# previous_sunday - 1

# date_range <- c(from.date ,previous_sunday)

date_range <- c(first_Day ,last_Day)



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
  merge(a, b, by='yearWeek', suffixes=c('', ncol(a)))
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
res4 <- foreach(i=1:length(ga_queries),.export=c(''), .packages = c("googleAnalyticsR","googleAuthR","lubridate"), .combine='list', .multicombine=TRUE) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

head(res4)
ga_queries[[1]]
previous_period



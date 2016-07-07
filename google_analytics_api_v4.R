library(googleAnalyticsR)
library(googleAuthR)
library(lubridate)
library(parallel)
library(doParallel)

ga_id <- '83321718' #EAM

numCores <- detectCores()
cl <- makeCluster(numCores - 6) 
registerDoParallel(cl) 

sessions.ga <- c(
  metrics = "sessions,users",
  dimensions = "yearWeek"
)

bounces.ga <- c(
  metrics = 'bounces',
  dimensions = 'yearWeek'
)
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



start.Date <- function(last_Day){
  
  days <- floor_date(seq(last_Day-365,last_Day,by='week'), 'week') # Ojo. Tiene que devolver una semana más.
  from.date <- days - 6
  # from.date <- days[weekdays(days+1)=='lunes']
  return(min(from.date))
  
}

today <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

last_Day <- floor_date(today, "week")

first_Day <- start.Date(last_Day)

date_range <- c(first_Day ,last_Day)

getGaData4(bounces.ga, ga_id, date_range)
getGaData4(sessions.ga, ga_id, date_range)

ga_queries <- list(bounces.ga, sessions.ga)

res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate"), .combine='list', .multicombine=TRUE) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

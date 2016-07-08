library(googleAnalyticsR)
library(googleAuthR)
library(lubridate)
library(parallel)
library(doParallel)
library(data.table)

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
  return(as.data.table(ga))
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

res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate", "data.table"), .combine='list', .multicombine=TRUE, .inorder = T) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

# names(res4) <- as.vector(ga_queries)

class(res4[[2]])

foo <- as.data.table(res4[[2]])
class(foo)
foo[,c('sessions'), with=F]
# row.names(foo)
# rowid(foo$yearWeek)
# foo2 <- dcast.data.table(foo, "ssss" ~ yearWeek , value.var = c("sessions"), margins="yearWeek")
# 
# foo3 <- melt(foo, id.vars = "yearWeek")
# dcast.data.table(melt(foo, id.vars = "yearWeek"), value ~ yearWeek, value.var = c("value"), subset = .(variable == "sessions"))

head(foo2)
rownames(foo2) <- c("sessions")

library(openxlsx)
wb <- loadWorkbook('excel/dash.xlsx')
addWorksheet(wb, "test", zoom = 110)
writeData(wb, sheet = "test", foo2)
saveWorkbook(wb, 'excel/dash.xlsx', overwrite = TRUE)

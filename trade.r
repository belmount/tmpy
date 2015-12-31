library(quantmod)
rm(list=ls())
data <- getSymbols('600660.SS', auto.assign = F) # 福耀玻璃
data2 <- getSymbols('002022.SZ', auto.assign = F) # 科华生物
data3 <- getSymbols('000001.SS', auto.assign = F) # 上证指数
data4 <- getSymbols('600754.SS', auto.assign = F) # 锦江股份
data5 <- getSymbols('000568.SZ', auto.assign = F) # 泸州老叫
# generate indicators need to trade
gen.indicators <- function(data, short.p, long.p){
  ma.s <- SMA(Ad(data), short.p)
  ma.l <- SMA(Ad(data), long.p)
  atr <- ATR(data, n = short.p)
  indicators <- cbind(ma.s = ma.s, ma.l=ma.l , atr= atr)
  # generate trade signatures
  indicators$sig <- 0
  indicators$sig[Ad(data) > ma.s & ma.s > ma.l & rollmax(data, )] <-1
  indicators <- na.omit(indicators)
  return(indicators)
}


gen.trade_range<- function(indicators) {
  entry_pts <- indicators[indicators$sig - Lag(indicators$sig)>0]
  exit_pts <- indicators[indicators$sig - Lag(indicators$sig)<0]
  trade.range <- rbind(entry_pts, exit_pts)
  return(trade.range)
}


stop.loss <- function(data, buy.price, stop.loss.pct) {
   sig <- data[Ad(data)/buy.price < stop.loss.pct]
   if (nrow(sig) == 0 || stop.loss.pct == 1)
     return(NA)
   else
     return(index(first(sig)))
}

trail.stop <- function(data, trails.threshold){
  cummax <- cummax(Ad(data))
  sig <- data[Ad(data) <cummax - trails.threshold ]
  if (nrow(sig) == 0 )
    return(NA)
  else
    return(index(first(sig)))
}

performance.analytics<- function(data){
  invest.return <- cumprod(na.fill(Ad(data)/Ad(lag(data)), 1))
  max.gain <- max(invest.return)
  final.return <- last(invest.return)
  max.drowndown <- 1- min ( invest.return / cumsum(invest.return))
  return(data.frame(max.gain = max.gain, max.dd = max.drowndown, final.return=final.return))
}


short.p <- 50
long.p <- 150
#holding<- indicators$sig
#holding$sig<- 0
#holding$price <- 0

stop.loss.pct <- 0.96
trail.factor <- 3


back_test <- function(data, trade.range, stop.loss.pct, trail.factor, indicators) {
  pa <- data.frame()
  retcurve <- xts(order.by=index(data))
  retcurve$t <- 0
  for(i in 1:nrow(trade.range)){
    idx.day <- index(trade.range[i])
    if (trade.range$sig[idx.day] == 0){ 
      next 
    } else {
      if(i + 1 > nrow(trade.range)){
        end.day<- last(index(data))  
      } else {
        end.day <- index(trade.range[i+1])
      }
      date.range<-paste(idx.day, end.day, sep='/')
      trade <- data[date.range]
      #print(date.range)
      buy.price <- as.numeric(Op(data)[[data[idx.day,which.i=T] + 1]])
      factor <- as.numeric(first(Ad(trade)) / first(Cl(trade)))
      buy.price <- buy.price * factor
      trail.threshold <- as.numeric(trail.factor * factor * indicators$atr[idx.day])
      
      stoploss.day <- stop.loss(trade, buy.price, stop.loss.pct)
      trail.stop.day <- trail.stop(trade, trail.threshold)
      exit.day <- as.Date(min(stoploss.day, end.day, trail.stop.day, na.rm=T))
      #if (!is.na(stoploss.day) && (stoploss.day == exit.day)) print('stop loss triggered')
      #if (!is.na(trail.stop.day) && (trail.stop.day == exit.day)) print('trail stop triggered')
      date.range<-paste(idx.day, exit.day, sep='/')
      retcurve$t[date.range] <- Delt(Ad(trade))
      #holding$sig[date.range] <- 1
      #holding$price[date.range] <- Ad(data)[date.range]
      pa <- rbind(pa, performance.analytics(data[date.range]))
    }
  }
  return (pa)
}

disp.performance <- function (pa){
  print(paste('trade count', nrow(pa)))
  print(paste('trade return avg.', mean(pa[,3]), 'trade final return', last(cumprod(pa[,3]))))
  print(summary(pa))
  print("\n\n")
}

do.param.test <- function(data, short.p, long.p, stop.loss.pct, trail.factor) {
  indicators <- gen.indicators(data, short.p, long.p)
  trade.range <- gen.trade_range(indicators)
  back_test(data, trade.range, stop.loss.pct, trail.factor, indicators)
}

gen.paramgrid <- function(){
  s <-  c(15)#seq(15, 30, 5)
  l <- seq(3, 5)
  stopl <-  seq(0.96, 1.0, 0.01)
  trail.f <-  seq(0, 3)
  params <- expand.grid(s=s, l=l, stop.l=stopl, trail.f=trail.f)
  params$l <- params$l * params$s
  return(params)
}


params <- gen.paramgrid()
for(id in 1:nrow(params)){
  i <- params[id,]
  print(paste(i$s, i$l, i$stop.l, i$trail.f ))
  pa <- do.param.test(data4, i$s, i$l, i$stop.l, i$trail.f)
  disp.performance(pa)
}

pa <- do.param.test(data, 15, 60, 0.96, 3)

pa$t <-na.fill(pa$t, 0)
pa$s <- cumprod(pa$t+1)
plot(pa$s)




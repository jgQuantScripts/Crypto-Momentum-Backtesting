require("lubridate");require("quantmod");require("pbapply");require("data.table")
# ********************************************************************************************************************************************
#                                                         ** CHOOSE 4 ASSETS ** updated : 8/20/16....works
# ********************************************************************************************************************************************
# COINS <- c("ALGO","ZRX","LINK","ETC","XLM","DASH","EOS","BCH","LTC","XTZ","BTC","ETH")
# PRC <- lapply(as.list(COINS),function(x) Cl(readRDS(paste0("/Volumes/3TB/CoinbasePro/",x,"_15_20191001.rds"))))
# PRC <- do.call(merge,PRC)
# PRC <- na.locf(PRC)
# PRC <- na.locf(PRC,fromLast = TRUE)
# colnames(PRC) <- gsub(".Close","",names(PRC))
# saveRDS(PRC,"CRYPTO_CLOSES_15min.rds")
PRC <- readRDS("CRYPTO_CLOSES_15min.rds")

# Extract column names
NOM <- names(PRC)
# get momentum for every 60 bars , round to 4 decimal places
MOMO60 <- round(ROC(PRC,n=60,"discrete"),4)

# data actually starts 2019-09-28 12:00 AM
MOMO60 <- MOMO60["20190928T12:00:00::"] 
PRC <- PRC["20190928T12:00:00::"]

# sequence of hourly intervals... will be used to extract the asset with the best momentum at that time
indx <- seq.POSIXt(from = as.POSIXct("2019-09-28 12:00:00"), to = Sys.time(), by = "1 hour")
# use the index to subset the momentum & price series
SELECT <- MOMO60[indx]
PRC2 <- PRC[indx]
# make combinations of assets: groups of 4
ASSETS4 <- combn(NOM,4)
MAX <- dim(ASSETS4)[2]
# getStrat extracts the Performance of the grouped assets
getStrat = function(x){
  y <- ASSETS4[,x]
  S <- SELECT[,y]
  # Contains the Column index for the MAXIMUM VALUE
  SEQ <- as.numeric(apply(S,1,which.max))
  # CALCULATE PRICE RETURN OVER HOLDING PERIOD
  prc2 <- PRC2[,y]
  RETS <- CalculateReturns(prc2,"discrete")
  
  # APPLY STRATEGY FOR THE HOLDING PERIOD
  ALL <- do.call(merge.xts,lapply(as.list(1:dim(RETS)[2]), function(x){
    Lag(reclass(ifelse(SEQ==x,1,0),match.to=S))*RETS[,x]
  }))
  
  colnames(ALL) <- names(prc2)
  ALL[is.na(ALL)]<-0
  # RETURNS INDIVIDUAL RETURNS FOR THE STRATEGY
  #charts.PerformanceSummary(ALL)
  
  # RETURNS Chart for the strategy
  EQT <- reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
  colnames(EQT) <- paste(names(prc2), collapse = '-')
  EQT
}
# Will apply the strategy for all combinations
STRAT <- pblapply(as.list(1:MAX),function(x){
  tmp <- try(getStrat(x))
  if (!inherits(tmp, 'try-error')) tmp
})
# eliminates empty lists caused by any errors in the pblapply()
STRAT <- STRAT[lapply(STRAT,length)>0]
# saveRDS(STRAT,"CRYPTO_STRAT.rds")
# extract the column sums of each combination
AAA <- pblapply(STRAT,colSums)
# SORT least to greatest
TOP <- STRAT[order(sapply(AAA,'[[',1))]
# TOP 10 best performers
TOP<- TOP[(length(TOP)-9):length(TOP)]
# extract the column sums
AAA <- pblapply(TOP,colSums)
# store best combination
AAA[[which.max(AAA)]]
EQT <- TOP[[which.max(AAA)]]
# plot EQT curve of the best performer
charts.PerformanceSummary(EQT, ylog=TRUE,geometric = FALSE)
# get % returns
last(cumsum(EQT))*100
# plot all TOP 10
TOP10 <- do.call(merge,TOP)
charts.PerformanceSummary(TOP10, ylog=TRUE,geometric = FALSE, colorset=rich10equal)
charts.PerformanceSummary(TOP10["2021"], ylog=TRUE,geometric = FALSE, colorset=rich10equal)

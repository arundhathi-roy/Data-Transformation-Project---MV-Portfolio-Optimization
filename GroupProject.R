
rm(list=ls(all=T)) # this just removes everything from memory

# Connect to PostgreSQL ---------------------------------------------------

# Make sure you have created the reader role for our PostgreSQL database
# and granted that role SELECT rights to all tables
# Also, make sure that you have completed (or restored) Part 3b db

# ONLY IF YOU STILL AN AUTHENTICATION ERROR:
# Try changing the authentication method from scram-sha-256 to md5 or trust (note: trust is not a secure connection, use only for the purpose of completing the class)
# this is done by editing the last lines of the pg_hba.conf file,
# which is stored in C:\Program Files\PostgreSQL\16\data (for version 16)
# Restart the computer after the change


require(RPostgres) # did you install this package?
require(DBI)
conn <- dbConnect(RPostgres::Postgres()
                  ,user="stockmarketreader"
                  ,password="read123"
                  ,host="localhost"
                  ,port=5432
                  ,dbname="stockmarket_HW2"
)

#custom calendar
qry<-"SELECT * FROM custom_calendar WHERE date BETWEEN '2015-12-31' AND '2021-03-26' ORDER by date"
ccal<-dbGetQuery(conn,qry)
#eod prices and indices
qry1="SELECT symbol,date,adj_close FROM eod_indices WHERE date BETWEEN '2015-12-31' AND '2021-03-26'"
qry2="SELECT ticker,date,adj_close FROM eod_quotes WHERE date BETWEEN '2015-12-31' AND '2021-03-26'" 
eod<-dbGetQuery(conn,paste(qry1,'UNION',qry2))
dbDisconnect(conn)
rm(conn)



# Use Calendar --------------------------------------------------------

tdays<-ccal[which(ccal$trading==1),,drop=F]

# Completeness ----------------------------------------------------------
# Percentage of completeness
pct<-table(eod$symbol)/(nrow(tdays)-1)
selected_symbols_daily<-names(pct)[which(pct>=0.99)]
eod_complete<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]


#YOUR TURN: perform all these operations for monthly data
#Create eom and eom_complete
#Hint: which(ccal$trading==1 & ccal$eom==1)

# Transform (Pivot) -------------------------------------------------------

require(reshape2) #did you install this package?
eod_pvt<-dcast(eod_complete, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)


# YOUR TURN: Perform the same set of tasks for monthly prices (create eom_pvt)

# Merge with Calendar -----------------------------------------------------
eod_pvt_complete<-merge.data.frame(x=tdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)


#use dates as row names and remove the date column
rownames(eod_pvt_complete)<-eod_pvt_complete$date
eod_pvt_complete$date<-NULL #remove the "date" column


# Missing Data Imputation -----------------------------------------------------
# We can replace a few missing (NA or NaN) data items with previous data
# Let's say no more than 3 in a row...
require(zoo)
eod_pvt_complete<-na.locf(eod_pvt_complete,na.rm=F,fromLast=F,maxgap=3)

# Calculating Returns -----------------------------------------------------
require(PerformanceAnalytics)
eod_ret<-CalculateReturns(eod_pvt_complete)

#remove the first row
eod_ret<-tail(eod_ret,-1) #use tail with a negative value


# YOUR TURN: calculate eom_ret (monthly returns)

# Check for extreme returns -------------------------------------------
# There is colSums, colMeans but no colMax so we need to create it
colMax <- function(data) sapply(data, max, na.rm = TRUE)
# Apply it
max_daily_ret<-colMax(eod_ret)

# And proceed just like we did with percentage (completeness)
selected_symbols_daily<-names(max_daily_ret)[which(max_daily_ret<=1.00)]
length(selected_symbols_daily)

#subset eod_ret
eod_ret<-eod_ret[,which(colnames(eod_ret) %in% selected_symbols_daily),drop=F]


# Tabular Return Data Analytics -------------------------------------------

# We will select 'SP500TR' and 12 RANDOM TICKERS
set.seed(100) # seed can be any number, it will ensure repeatability
tickers <-c('BXP.PB', 'AGRO', 'SRI','TU', 'FISI', 'SFM', 'POST', 'BKNG', 'VBTX', 'OFC', 'CKX', 'KT', 'LSI', 'CUZ','AX')


# We need to convert data frames to xts (extensible time series)
Ra<-as.xts(eod_ret[,tickers,drop=F])
Rb<-as.xts(eod_ret[,'SP500TR',drop=F]) #benchmark


# MV Portfolio Optimization -----------------------------------------------

# withhold the last 58 trading days
Ra_training<-head(Ra,-58)
Rb_training<-head(Rb,-58)

# use the last 58 trading days for testing
Ra_testing<-tail(Ra,58)
Rb_testing<-tail(Rb,58)

# Cumulative returns chart
chart.CumReturns(cbind(Ra_training,Rb_training),legend.loc = 'topleft')



#optimize the MV (Markowitz 1950s) portfolio weights based on training
table.AnnualizedReturns(Rb_training)
mar<-mean(Rb_training) #we need daily minimum acceptable return

require(PortfolioAnalytics)
require(ROI) # make sure to install it
require(ROI.plugin.quadprog)  # make sure to install it
pspec<-portfolio.spec(assets=colnames(Ra_training))
pspec<-add.objective(portfolio=pspec,type="risk",name='StdDev')
pspec<-add.constraint(portfolio=pspec,type="full_investment")
pspec<-add.constraint(portfolio=pspec,type="return",return_target=mar)

#optimize portfolio
opt_p<-optimize.portfolio(R=Ra_training,portfolio=pspec,optimize_method = 'ROI')

#extract weights (negative weights means shorting)
opt_w<-opt_p$weights

# YOUR TURN: try adding the long-only constraint and re-optimize the portfolio

#apply weights to test returns
Rp<-Rb_testing # easier to apply the existing structure
#define new column that is the dot product of the two vectors
Rp$ptf<-Ra_testing %*% opt_w

#check
head(Rp)
tail(Rp)

#Compare basic metrics
table.AnnualizedReturns(Rp)

# Chart Hypothetical Portfolio Returns ------------------------------------


chart.CumReturns(Rp,legend.loc = 'bottomright')

# End of Part 3c
# End of Stock Market Case Study 
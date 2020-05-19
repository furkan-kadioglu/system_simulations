# Install packages
#install.packages("xlsx")

# Load Data
library("xlsx") # This package is used to read data from .xls file.
data <- read.xlsx("Assignment2-Interarrival_Data-S2020.xls",
                 sheetIndex = 1)
day1=data[,1]
day2=data[,2]

    
#------------------------1 Kolmogorov-Smirnovtest-------------------#
uniformity_test = function(x){
    x = sort(x)
    x = punif(x, min = 0, max = 400)
    x.length = length(x)

    D.plus = max(((1:x.length) / x.length) - x)
    D.minus = max(x - ((1:x.length - 1) / x.length))
    D = max(D.minus, D.plus)

    print(D)
    print(1.36 / sqrt(x.length))
    print(D < (1.36 / sqrt(x.length)))
}
#uniformity_test(day1)
#uniformity_test(day2)

#-------------------------2 Sample Statistics-----------------------#
append = function(x,y) {
    paste(c(x, y), collapse = "")
}

statistics = summary(data)
statistics = rbind(statistics, 
                    c(append("Std.   : ", round(sd(data[,1]),3)), 
                      append("Std.   : ", round(sd(data[,2]),3))))
                      
#-------------------------3 Histogram------------------------------#

## DAY 1 Histograms
hist(data[,1], xlab='Day 1 for 5 secs', xlim=c(0,400), breaks=400/5)
hist(data[,1], xlab='Day 1 for 10 secs', xlim=c(0,400), breaks=400/10)
hist(data[,1], xlab='Day 1 for 20 secs', xlim=c(0,400), breaks=400/20)

## DAY 2 Histograms
hist(data[,2], xlab='Day 2 for 5 secs', xlim=c(0,400), breaks=400/5)
hist(data[,2], xlab='Day 2 for 10 secs', xlim=c(0,400), breaks=400/10)
hist(data[,2], xlab='Day 2 for 20 secs', xlim=c(0,400), breaks=400/20)

#----------------------4 Chi-Square Test---------------------------#
frequency = function (x) {
    #x.mean = mean(data[,1]) # day 1
    x.mean = mean(data[,2]) # day 2
    pexp(x+10, rate=1/x.mean) - pexp(x, rate=1/x.mean)
}

chi_square_test = function(O, interval = 10){
    N = length(O)
    O.hist = hist(O, breaks=400/interval)
    end = O.hist$breaks[length(O.hist$breaks)-1] / interval 
    O.frequency = O.hist$count

    E.frequency = unlist(lapply(0:end * interval, frequency)) * N


    diff = O.frequency - E.frequency
    X = sum(((diff)**2) / E.frequency)

    print(X)
    #print(X < 58.142)  # day 1
    print(X < 46.194)  # day 2
}

#chi_square_test(day1)
#chi_square_test(day2)

#----------------------5 QQ Plot----------------------------------#

qq_plot = function(x){
    x.length = length(x)
    x.mean = mean(x)
    theoretical = qexp(ppoints(x.length),rate=1/x.mean)
    qqplot(x, theoretical, xlab="Interarrival Times", ylab="Exponential Data Quantiles")
    a = x
    lines(x,a)
}

#qq_plot(day1)
#qq_plot(day2)


#--------------6 inter-arrival vs observation times---------------#
plot_inter = function(x){
    plot(cumsum(x), x, type="h", xlab="Observation Times", ylab="Interarrival Times")
}

#plot_inter(day1)
#plot_inter(day2)

#------------------------7 Auto correlation-----------------------#

autocorrelation = function(arr, lag){
    arr.length = length(arr)
    arr = unlist(lapply(arr, function(k){pexp(k, rate=1/mean(arr))})) # Convert to U[0, 1] distribution
    M = floor((arr.length-1)/lag-1)
    ro = sum(unlist(lapply(0:M, function(k){arr[1+k*lag]*arr[1+(k+1)*lag]})))/(M+1)-0.25
    sigma = sqrt(13*M+7)/(12*(M+1))
    z0 = ro/sigma
    if (z0 >= -1.96 && z0 <= 1.96) 
        sprintf("Data seems independent at alpha=0.05. |%f|<1.96", z0)
    else
        sprintf("Data seems not independent at alpha=0.05. |%f|>1.96", z0)
}

#autocorrelation(day1)
#autocorrelation(day2)


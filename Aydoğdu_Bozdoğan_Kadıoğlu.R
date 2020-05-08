# Install packages
#install.packages("xlsx")

# Load Data
library("xlsx")
data <- read.xlsx("Assignment2-Interarrival_Data-S2020.xls",
                 sheetIndex = 1)
    
#------------------------1 Kolmogorov-Smirnovtest-------------------#
uniformity_test = function(x){
    x.sorted = sort(x)
    x.length = length(x)

    D.plus = max(((1:x.length) / x.length) - x)
    D.minus = max(x - ((1:x.length - 1) / x.length))
    D = max(D.minus, D.plus)

    print(D < (1.36 / sqrt(x.length)))
}

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
# Problems with task 4
# 1- E.frequency length is smaller than O.frequency.
# 2- number of samples expectations for some intervals is 0 
# so inf and nan value are possible.
# 3- degree of freedom for 488 - 1 = 477 has to be calculated.


chi_square_test = function(O, interval = 10){
    N = length(O)
    E = rexp(n=N, rate=(1/mean(O)))
    O.frequency = hist(O, breaks=400/interval)$count
    E.frequency = hist(E, breaks=400/interval)$count

    diff = O.frequency[1:length(E.frequency)] - E.frequency
    X = sum(((diff)**2) / E.frequency)
    print(X < 553.127) # degree of freedom 500
}

#----------------------5 QQ Plot----------------------------------#

qq_plot = function(x){
    x.length = length(x)
    x.mean = mean(x)
    theoretical = qexp(ppoints(x.length),rate=1/x.mean)
    qqplot(x, theoretical)
}

#--------------6 inter-arrival vs observation times---------------#
plot_inter = function(x){
    plot(cumsum(x), x)
}


#------------------------7 Auto correlation-----------------------#


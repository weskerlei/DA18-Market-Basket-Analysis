##load library##
library(arules)
library(arulesViz)

##Parallel Computing##
#--- for Win ---#
library(doParallel) 

# Check number of cores and workers available 
detectCores()
getDoParWorkers()
cl <- makeCluster(detectCores()-1, type='PSOCK')
registerDoParallel(cl)

##load and inspect transaction data##
trans_data <- read.transactions("ElectronidexTransactions2017.csv", format = c("basket"), sep = ",", rm.duplicates = TRUE)
summary(trans_data)
##looks like there are 2 empty rows in the sparse matrix

##Visualize dataset##
##plot item frequency with support >0.1##
itemFrequencyPlot(trans_data, type = c("relative"), support =0.1)
##plot item frequency - top 20 items##
itemFrequencyPlot(trans_data, type = c("relative"), topN=20)
##binary map of items purchased for 1st 100 transactions##
image(trans_data[1:100])
##binary map of items purchased for randomly selected 500 transactions##
image(sample(trans_data, 500))

##Apply the apriori function##
trans_rules <- apriori(trans_data, parameter = list(supp = 0.0015, conf = 0.90, minlen = 2))
inspect(sort(trans_rules, by = "lift"))

##remove redundant rules##
redundant <- is.redundant(trans_rules)
which(redundant)
trans_rules <- trans_rules[!redundant]
inspect(sort(trans_rules, by = "lift"))

##Visualize rules##
plot(trans_rules)
plot(trans_rules, method ="graph", control=list(type="items"))


##item specific rules##
imac_rules <-subset(trans_rules, items %in% "iMac")
inspect(sort(imac_rules, by ="lift"))

hpLaptop_rules <-subset(trans_rules, items %in% "HP Laptop")
inspect(sort(hpLaptop_rules, by = "lift"))

cygamer_rules <-subset(trans_rules, items %in% "CYBERPOWER Gamer Desktop")
inspect(sort(cygamer_rules, by ="lift"))

earpods_rules <-subset(trans_rules, items %in% "Apple Earpods")
inspect(sort(earpods_rules, by ="lift"))

mbair_rules <-subset(trans_rules, items %in% "Apple MacBook Air")
inspect(sort(mbair_rules, by = "lift"))

mbpro_rules <-subset(trans_rules, items %in% "Apple MacBook Pro")
inspect(sort(mbpro_rules, by = "lift"))



# to stop cluster/parallel:
stopCluster(cl) 
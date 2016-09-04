#count of type of populations
#takes out "Other"
populationSum <- appic_dataframe[,67:84]
popVarList <- colnames(populationSum)
popData <- data.frame()
for (i in 1:18) {
  popTable <- table(populationSum[,i])  %>% data.frame()
  popData <- rbind(popData, data.frame(subset(popTable, Var1 == "Yes")))
}
popData <- cbind(popVarList, popData)
popData$type <- "population"

#count of treatment modalities
treatmentSum <- appic_dataframe[,86:100]
treatVarList <- colnames(treatmentSum)
treatData <- data.frame()
for (i in 1:15) {
  treatTable <- table(treatmentSum[,i])  %>% data.frame()
  treatData <- rbind(treatData, data.frame(subset(treatTable, Var1 == "Yes")))
}
treatData <- cbind(treatVarList, treatData)
treatData$type <- "treatment"

#count of expereinces
experSum <- appic_dataframe[,102:137]
experVarList <- colnames(experSum)
experData <- data.frame()
for (i in 1:36) {
  experTable <- table(experSum[,i])  %>% data.frame()
  experData <- rbind(experData, data.frame(subset(experTable, Var1 == "Yes")))
}
experData <- cbind(experVarList, experData)
experData$type <- "experience"

#renaming coloums
colnames(popData) <- c("category", "var", "count", "type")
colnames(treatData) <- c("category", "var", "count", "type")
colnames(experData) <- c("category", "var", "count", "type")
factor(x$name, levels = x$name[order(x$val)])

#removing period and trailing white space
popData$category <- gsub("\\.", " ", popData$category)
popData$category <- gsub(' +$', '', popData$category)
treatData$category <- gsub("\\.", " ", treatData$category)
treatData$category <- gsub(' +$', '', treatData$category)
experData$category <- gsub("\\.", " ", experData$category)
experData$category <- gsub(' +$', '', experData$category)

#sorting the factor level by decending order
popData$category <- factor(popData$category, levels = popData$category[order(popData$count)])
treatData$category <- factor(treatData$category, levels = treatData$category[order(treatData$count)])
experData$category <- factor(experData$category, levels = experData$category[order(experData$count)])

typeOfExp <- rbind(popData, treatData, experData)

#CC6666
popPlot <- ggplot(popData, aes(category, count)) + 
  geom_bar(stat = "identity", fill = "#CC6666") +
  coord_flip() +
  guides(colour = FALSE) +
  ggtitle("What is the most common population?") +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = pretty(0:700, n = 7)) +
  themeBrettrics

treatPlot <- ggplot(treatData, aes(category, count)) + 
  geom_bar(stat = "identity",  fill = "#66CC99") +
  coord_flip() +
  guides(colour = FALSE) +
  ggtitle("What is the most common type of treatment?") +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = pretty(0:700, n = 7)) +
  themeBrettrics

experPlot <- ggplot(experData, aes(category, count)) + 
  geom_bar(stat = "identity", fill = "#9999CC") +
  coord_flip() +
  guides(colour = FALSE) +
  ggtitle("What is the most common experience need?") +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = pretty(0:700, n = 7)) +
  themeBrettrics

grid.arrange(popPlot, treatPlot, experPlot, ncol = 1)

h = 4.5

svg(filename = "APPIC_data/Plots/pop.svg", 
    width = 8, 
    height = length(popData$category)/h, 
    pointsize = 5)
print(popPlot)
dev.off()

svg(filename = "APPIC_data/Plots/treat.svg", 
    width = 8, 
    height = length(treatData$category)/h, 
    pointsize = 5)
print(treatPlot)
dev.off()

svg(filename = "APPIC_data/Plots/exper.svg", 
    width = 8, 
    height = length(experData$category)/h, 
    pointsize = 5)
print(experPlot)
dev.off()

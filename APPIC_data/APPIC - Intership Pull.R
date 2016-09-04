library('rvest')
library('magrittr')
library('dplyr')
library('reshape2')
library('ggmap')
library('ggplot2')
library("grid")
library("gridExtra")
library("scales")
library("plyr")

#Web Scrapper to retrieve information off of APPIC Directory
appic_dataframe <- appic_dataframe[-1, ]
intership_site_display_id <- read.csv("APPIC_data/site_url_list.csv", header = TRUE)

appic_dataframe <- data.frame()
intership_stats <- data.frame()
postdocs_stats <- data.frame()

length(intership_site_display_id$url_number)

for (i in 1:length(intership_site_display_id$url_number)) {
  tryCatch({
card <- read_html(paste("https://membership.appic.org/directory/display/", intership_site_display_id$url_number[i], sep = ""))
all_tables <- html_nodes(card, "table") 

#Invidual tables of data
site_info <- html_table(all_tables[[1]])
accred <- html_table(all_tables[[2]])
staff_info <- html_table(all_tables[[3]])
star_date <- html_table(all_tables[[4]])
num_of_slots <- html_table(all_tables[[5]])
colnames(num_of_slots) <- c("X1", "X2")
benefits <- html_table(all_tables[[7]])

work_day_descript <- html_table(all_tables[[8]])
fully_aff <- html_table(all_tables[[9]])
part_aff <- html_table(all_tables[[10]])

app_process <- html_table(all_tables[[11]])
app_req <- html_table(all_tables[[12]])
program_type <- html_table(all_tables[[13]])

other_req <- html_table(all_tables[[14]])
population <- html_table(all_tables[[15]])
treatment_meth <- html_table(all_tables[[16]])
experience <- html_table(all_tables[[17]])

# site description & display_id
description_info <- html_node(card, "#main > div.row.directory-display.page-sectioned > div > div:nth-child(9) > fieldset > div > p:nth-child(2)") %>% html_text()
description <- c("Description", description_info)
display_id <- c("Display Id", intership_site_display_id$url_number[i])

#stiped info flattend to fit into row by row database
stipend <- html_table(all_tables[[6]])
low_stip <- c(unlist(stipend[2]))
high_stip <- c(unlist(stipend[3]))
stip_info <- data.frame(unlist(t(data.frame(
  v1 <- c("Full Time - High", high_stip[[1]]),
  v2 <- c("Full Time - Low", low_stip[[1]]),
  v3 <- c("Part Time - High", high_stip[[2]]),
  v4 <- c("Part Time - Low", low_stip[[2]]))))
  )

#rolled up for parsing
roll_up_text <- rbind(display_id, site_info, accred, staff_info, star_date, num_of_slots, benefits, work_day_descript, fully_aff, part_aff, app_process, app_req, program_type, other_req, population, treatment_meth, experience, description, stip_info)

#transposed for adding to data.frame             
roll_up_TRAN <- unlist(c(t(roll_up_text[2])))
appic_dataframe <- rbind(appic_dataframe, data.frame(t(roll_up_TRAN)))

# intership stats / records from previous three years - 4x
intership_stats <- rbind(intership_stats, cbind(intership_site_display_id$url_number[i], site_info[[2]][[4]], html_table(all_tables[[18]])))
postdocs_stats <- rbind(postdocs_stats, cbind(intership_site_display_id$url_number[i], site_info[[2]][[4]], html_table(all_tables[[19]])))

print(round((i/length(intership_site_display_id$url_number)*100), digits = 2))
 }, error = function(e){"ERROR"})
}

roll_up_NAME <- unlist(c(t(roll_up_text[1])))   
colnames(appic_dataframe) <- roll_up_NAME
colnames(intership_stats) <- c("display_id", "site_name", "desciption", "2014-2015", "2015-2016", "2016-2017")
colnames(postdocs_stats) <- c("display_id", "site_name", "desciption", "2013-2014", "2014-2015", "2015-2016")

write.csv(appic_dataframe, file = "APPIC_data/appic_data_extra.csv")
write.csv(intership_stats, file = "APPIC_data/appic_intership_data_extra.csv")
write.csv(postdocs_stats, file = "APPIC_data/appic_postintership_data_extra.csv")

# ----- data analyis of APPIC ----- 

# - - - Questions to be Answered in the Blog Post - - - 
# How likey are you to get an interview?
# how likey are you to get placed?
# Do they take Ph. D's, Psy. D's or both?
# What state has the high interview rate?
# Do you have a good chance at being placed for a post-doc?
# What does a "competeative" site look like?
# What is the "ideal" canidate based on experience? 

appic_dataframe <- read.csv(file = "APPIC_data/appic_data_extra.csv", stringsAsFactors = FALSE)
var_list <- data.frame(colnames(appic_dataframe))

themeBrettrics <- theme(plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
                        axis.ticks = element_line(color = "black"),
                        axis.text = element_text(size = 11),
                        axis.title = element_text(size = 13, face = "bold"))


# Analysis of the type of experiences wanted, types of treatment and the type of expereince

#Population 66-84
#Treatment Modalities - 85-100
#Experience 101-137

#Using Melt and Cast to create heatmap of poputions, treatment types, and expereinces
#population
populationM <- appic_dataframe[, c(17,67:84)]
pop.m <- melt(populationM, id = 1)
pop.m$value <- gsub("Yes", as.numeric(1), pop.m$value) %>% as.numeric()
pop.c <- dcast(pop.m, variable ~ Primary.Agency.Type., fun.aggregate = sum, na.rm = TRUE)
pop.cm <- melt(pop.c, id = 1) 
colnames(pop.cm) <- c("pop", "type", "value")
pop.cm <- ddply(pop.cm, .(type), transform, rescale = rescale(value))
pop.cm$pop <-gsub("\\.", " ", pop.cm$pop)
pop.cm$pop <-gsub(' +$', '', pop.cm$pop)

#treatment
treatM <- appic_dataframe[, c(17,86:100)]
treat.m <- melt(treatM, id = 1)
treat.m$value <- gsub("Yes", as.numeric(1), treat.m$value) %>% as.numeric()
treat.c <- dcast(treat.m, variable ~ Primary.Agency.Type., fun.aggregate = sum, na.rm = TRUE)
treat.cm <- melt(treat.c, id = 1) 
colnames(treat.cm) <- c("treat", "type", "value")
treat.cm <- ddply(treat.cm, .(type), transform, rescale = rescale(value))
treat.cm$treat <-gsub("\\.", " ", treat.cm$treat)
treat.cm$treat <-gsub(' +$', '', treat.cm$treat)

#expereince
experM <- appic_dataframe[, c(17,102:137)]
exper.m <- melt(experM, id = 1)
exper.m$value <- gsub("Yes", as.numeric(1), exper.m$value) %>% as.numeric()
exper.c <- dcast(exper.m, variable ~ Primary.Agency.Type., fun.aggregate = sum, na.rm = TRUE)
exper.cm <- melt(exper.c, id = 1) 
colnames(exper.cm) <- c("exper", "type", "value")
exper.cm <- ddply(exper.cm, .(type), transform, rescale = rescale(value))
exper.cm$exper <-gsub("\\.", " ", exper.cm$exper)
exper.cm$exper <-gsub(' +$', '', exper.cm$exper)

#plots and creating heatmaps
popPlot <- ggplot(pop.cm, aes(type, pop)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "white", high = "#756bb1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("What is the most common population?") +
  ylab("Population") +
  xlab("Agency Type") +
  themeBrettrics

#creating SVG
svg(filename = "APPIC_data/Plots/pop.svg", 
    width = 8, 
    height = 7, 
    pointsize = 5)
print(popPlot)
dev.off()

treatPlot <- ggplot(treat.cm, aes(type, treat)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "white", high = "#31a354") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("What is the most common type of treatment?") +
  ylab("Treatments") +
  xlab("Agency Type") +
  themeBrettrics

svg(filename = "APPIC_data/Plots/treat.svg", 
    width = 8, 
    height = 7, 
    pointsize = 5)
print(treatPlot)
dev.off()

experPlot <- ggplot(exper.cm, aes(type, exper)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "white", high = "#f03b20") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("What is the most common experience need?") +
  ylab("Experience") +
  xlab("Agency Type") +
  themeBrettrics

svg(filename = "APPIC_data/Plots/exper.svg", 
    width = 8, 
    height = 10, 
    pointsize = 5)
print(experPlot)
dev.off()


#plotting a box whisker plot of the salaries by agency type
money_type <- data.frame(
  appic_dataframe['Primary.Agency.Type.'],
  appic_dataframe['Full.Time...High']
)

# ggplot of box and whisker plot of salaries by Agency Type

moneyPlot <- ggplot(money_type, aes(Primary.Agency.Type., Full.Time...High)) + 
            geom_boxplot(color = "#CC6666") + 
            coord_flip() + 
            #geom_jitter(width = 0.1, color = "#587272") + 
            scale_y_continuous(limits = c(10000,90000), breaks = pretty(10000:90000, n = 9), labels = paste0("$", pretty(10000:90000, n = 9))) +
            labs(y = "Salary", x = "Agency Type") +
            ggtitle("How much can you expect to get paid?") +
            guides(colour = FALSE, fill = FALSE) +
            themeBrettrics

svg(filename = "APPIC_data/Plots/money.svg", 
    width = 9, 
    height = 6, 
    pointsize = 5)
print(moneyPlot)
dev.off()

#hour needed to get internship
hours_type <- data.frame(
  appic_dataframe['Primary.Agency.Type.'],
  appic_dataframe['Minimum.Number.of.AAPI.Intervention.Hours.'],
  appic_dataframe['Minimum.Number.of.AAPI.Assessment.Hours.']
)

hourPlot <- ggplot(hours_type, aes(Primary.Agency.Type.,Minimum.Number.of.AAPI.Intervention.Hours. )) + 
  geom_boxplot(color = "red") + 
  coord_flip() + 
  scale_y_continuous(limits = c(0,1000), breaks = pretty(0:1000, n = 9), labels = paste0(pretty(0:1000, n = 9))) +
  labs(y = "Hours", x = "Agency Type") +
  ggtitle("How many hours do you need?") +
  guides(colour = FALSE, fill = FALSE) +
  themeBrettrics

svg(filename = "APPIC_data/Plots/hour.svg", 
    width = 9, 
    height = 6, 
    pointsize = 5)
print(hourPlot)
dev.off()

#What is your chances of getting an internship?
intership_stats <- read.csv(file = "APPIC_data/appic_intership_data_extra.csv", stringsAsFactors = FALSE)
mod_inter_stats <- intership_stats
mod_inter_stats$X2014.2015 <- as.numeric(mod_inter_stats$X2014.2015)
mod_inter_stats$X2015.2016 <- as.numeric(mod_inter_stats$X2015.2016)
mod_inter_stats$X2016.2017 <- as.numeric(mod_inter_stats$X2016.2017)

mod_appic_stats <- appic_dataframe
mod_appic_stats$Display.Id <- as.integer(as.character(mod_appic_stats$Display.Id))

super_join <- left_join(mod_inter_stats, mod_appic_stats, by = c("display_id" = "Display.Id"))
var_list_super_join <- data.frame(colnames(super_join))

#creating data.frame for analysising app/interview/intern conversion rate
inter_data <- data.frame(
  super_join['display_id'],
  super_join['site_name'],
  super_join['Primary.Agency.Type.'],
  super_join['desciption'],
  super_join['X2014.2015'],
  super_join['X2015.2016'],
  super_join['X2016.2017'],
  super_join['Address.'],
  super_join['Web.Address.'],
  super_join['APA.Accreditation'],
  super_join['Number.of.Full.Time.Slots.Expected.Next.Class.'],
  super_join['Full.Time...High'],
  super_join['Minimum.Number.of.AAPI.Intervention.Hours.'],
  super_join['Minimum.Number.of.AAPI.Assessment.Hours.']
)

#resetting the NA to 0 to calculate mean
inter_data[is.na(inter_data)] <- 0 
inter_data$yoy <- apply(inter_data[,c("X2014.2015", "X2015.2016", "X2016.2017")], 1, nzmean) %>% round(0)

#reshaping data to calculate on row by row level
interview <- subset(inter_data, desciption == "Number of applicants invited for interviews:")
app <- subset(inter_data, desciption == "Number of Completed Applications:")
interns <- subset(inter_data, desciption == "Total number of interns:")
phDcount <- subset(inter_data, desciption == "Total number of interns from Ph.D. programs:")
psyDcount <- subset(inter_data, desciption == "Total number of interns from Psy.D. programs:")

#determining which agency is more friendly for phd or psyd

phDcount$phdpercent <- phDcount$yoy / phDcount$Number.of.Full.Time.Slots.Expected.Next.Class.
psyDcount$psydpercent <- psyDcount$yoy / psyDcount$Number.of.Full.Time.Slots.Expected.Next.Class.

phDvpsyD <- data.frame(phDcount, psyDcount$yoy, psyDcount$Number.of.Full.Time.Slots.Expected.Next.Class.)
phDvpsyD.g <- group_by(phDvpsyD, Primary.Agency.Type.) %>%
  summarise(psyd_sum = sum(psyDcount.yoy), phd_sum = sum(yoy))

phDvpsyD.g$psyD_diff <- round(phDvpsyD.g$psyd_sum / (phDvpsyD.g$phd_sum + phDvpsyD.g$psyd_sum),2)
phDvpsyD.g$phD_diff <- round(phDvpsyD.g$phd_sum / (phDvpsyD.g$psyd_sum + phDvpsyD.g$phd_sum),2) 

phDvpsyD.m <- melt(phDvpsyD.g, id.vars = "Primary.Agency.Type.", measure.vars = c("psyD_diff", "phD_diff"))
phDvpsyD.m <- ddply(phDvpsyD.m, .(Primary.Agency.Type.), transform, pos = cumsum(value) - (0.5 * value))

ggplot(phDvpsyD.m, aes(x = Primary.Agency.Type., y = value*100, fill = variable, label = value*100)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x = Primary.Agency.Type., y = pos*100, label = paste0(value*100, "%"))) +
  coord_flip() +
  labs(title = "How do Psy.Ds compare to Ph.D?", y = NULL, x = "Agency Type") +
  scale_fill_manual(name = NULL, labels = c("Psy. D", "Ph. D"), values = c("#1f84d4", "#d4ca1f")) +
  theme(legend.position="bottom", legend.direction="horizontal") + 
  themeBrettrics

#creating exporting files for downloads
exportAppic <- data.frame(app, interview$yoy, interns$yoy)
exportAppic$app_cr <- round(exportAppic$interview.yoy/exportAppic$yoy, 2)
exportAppic$inter_cr <- round(exportAppic$interns.yoy/exportAppic$interview.yoy, 2)
exportAppic[is.na(exportAppic)] <- 0

exportAppicCSV <- exportAppic[c(1:3,8:14,18:19)]
colnames(exportAppicCSV) <- c("display_id","site_name", "primary_agency_type", "address", "web_url", "apa_accred", "number_of_slots", "high_stipend","appi_intervention_hours","appi_assess_hours","chance_of_interview","chance_of_intership")
exportAppicCSV$address <- gsub("\\s\\s", "", as.character(exportAppicCSV$address))
write.csv(exportAppicCSV, "APPIC_data/intership_site_data.csv")

#new dataframe with site.name and conversion rates
app_conversion_rate <- data.frame(app$site_name, app$Primary.Agency.Type., app$yoy, interview$yoy, interns$yoy)

app_conversion_rate$app_cr <- round(app_conversion_rate$interview.yoy/app_conversion_rate$app.yoy, 2)
app_conversion_rate$inter_cr <- round(app_conversion_rate$interns.yoy/app_conversion_rate$interview.yoy, 2)

#resetting values for calculating mean
app_conversion_rate[is.na(app_conversion_rate)] <- 0

#creating piviot table of average and sums
agency_sum <- app_conversion_rate %>% 
              group_by(app.Primary.Agency.Type.) %>%
              summarise(num_of_site = n(), app_sum = sum(app.yoy), interview_sum = sum(interview.yoy), intern_sum = sum(interns.yoy))

agency_sum$app_cr <- round(agency_sum$interview_sum / agency_sum$app_sum, 2)
agency_sum$inter_cr <- round(agency_sum$intern_sum / agency_sum$interview_sum, 2)

agency_sum.m <- melt(agency_sum[c("app.Primary.Agency.Type.", "app_cr", "inter_cr")], id.vars = 'app.Primary.Agency.Type.')
agency_sum_m_s <- agency_sum.m[ order(-xtfrm(agency_sum.m$variable), -agency_sum.m$value), ]

agency_sum_m_s$variable <- factor(agency_sum_m_s$variable, levels = agency_sum_m_s$variable[order(-agency_sum_m_s$variable)])
agency_sum_m_s$app.Primary.Agency.Type. <- factor(agency_sum_m_s$app.Primary.Agency.Type., levels = agency_sum_m_s$app.Primary.Agency.Type.[order(agency_sum_m_s$value)])

#ploting chances
chanceInterview <- ggplot(agency_sum_m_s, aes(app.Primary.Agency.Type., value*100)) +   
                        geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + 
                        coord_flip() +
                        labs(title = "How likely are you to get an internship?", y = "% chance", x = "Agency Type") +
                        scale_y_continuous(limits = c(0, 100), breaks = pretty(0:100, n = 5), labels = paste0(pretty(0:100, n = 5), "%")) +
                        scale_fill_manual(name = NULL,labels = c("% chance to get into internship", "% to get interview"), values = c("#599ad3", "#f9a65a"), guide = guide_legend(ncol = 1)) +
                        themeBrettrics + 
                        theme(legend.position = "bottom")

svg(filename = "APPIC_data/Plots/chance.svg", 
    width = 8, 
    height = 6, 
    pointsize = 5)
print(chanceInterview)
dev.off()

# ---- Strucutre of Blog Post ----
# What is the purpose of this analysis? What was the inspiration?
# How did I get the data?
# What I will be sumarrizing with it?
# What the most common site types? What are they looking for?
# What is the average salary for each of the site type?
# What are your chances of actually getting in?
# Scripts for scrapping APPIC
# File Download

#functions 
nzmean <- function(x) {
  if (all(x == 0)) 0 else mean(x[x != 0])
}

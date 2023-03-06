library("likert")
library("dplyr")
library("RColorBrewer")
`%notin%` <- Negate(`%in%`)

##### Activities #####

#data <- read.csv("./data/strategies and activities_October 18, 2021_10.20.csv")
data <- read.csv("./data/strategies and activities_February 2, 2022_11.38.csv")
#colnames(data) <- c("The majority of the developers and I want the same thing","I don't feel at home in the group","A majority of developers in the group know me","If I have a problem, I know members in the group who I can ask for help","I want to contribute more but I do not feel valued",
#                    "I feel that I belong to the group", "Gender")

#data_filtered<- filter(data,Gender!= "Did not Inform" & "The majority of the developers and I want the same thing"!= "Did not Inform" & "I don't feel at home in the group"!= "Did not Inform" & "A majority of developers in the group know me" != "Did not Inform" & "If I have a problem, I know members in the group who I can ask for help" != "Did not Inform" & "I want to contribute more but I do not feel valued" != "Did not Inform")
#data_filtered_act<-data[!(data$Q66=="") | !(data$Q74==""),]
data_filtered_act<-data[!(data$Q66=="") & !(data$Q74=="") & (data$Q70=="1 - Unimportant"),]

#head(data_filtered_act)
nrow(data_filtered_act)
ncol(data_filtered_act)

#data_filtered_act_main <- data_filtered_act[(data_filtered_act$Q97=="Maintainer/Core Member/Owner/Community builder"),]
#data_filtered_act_freq <- data_filtered_act[(data_filtered_act$Q97=="Frequent Contributor"),]
#data_filtered_act_new <- data_filtered_act[(data_filtered_act$Q97=="New contributor/Possible contributor in the future"),]

keep <- c("Q66",	"Q98",	"Q67",	"Q68",	"Q69",	"Q74", "Q78", "Q7", "Q12", "Q77", "Q93", "Q97", "Q37_6",	"Q37_7",	"Q37_8",	"Q37_9",	"Q37_10")


activities <- data_filtered_act[-c(1,2), (names(data_filtered_act) %in% keep)]
head(activities)
#activities_main <- data_filtered_act_main[-c(1,2), (names(data_filtered_act_main) %in% keep)]
#activities_freq <- data_filtered_act_freq[-c(1,2), (names(data_filtered_act_freq) %in% keep)]
#activities_new <- data_filtered_act_new[-c(1,2), (names(data_filtered_act_new) %in% keep)]

activities[activities$Q66 == "5 - Essential", "Q66"] <- "5"
#activities[activities$Q70 == "5 - Essential", "Q70"] <- "5"
activities[activities$Q98 == "5 - Essential", "Q98"] <- "5"
activities[activities$Q67 == "5 - Essential", "Q67"] <- "5"
activities[activities$Q68 == "5 - Essential", "Q68"] <- "5"
activities[activities$Q69 == "5 - Essential", "Q69"] <- "5"

activities[activities$Q66 == "1 - Unimportant", "Q66"] <- "1"
#activities[activities$Q70 == "1 - Unimportant", "Q70"] <- "1"
activities[activities$Q98 == "1 - Unimportant", "Q98"] <- "1"
activities[activities$Q67 == "1 - Unimportant", "Q67"] <- "1"
activities[activities$Q68 == "1 - Unimportant", "Q68"] <- "1"
activities[activities$Q69 == "1 - Unimportant", "Q69"] <- "1"


activities[activities$Q97 == "Maintainer/Core Member/Owner/Community builder", "Q97"] <- "Maintainer"
activities[activities$Q97 == "New contributor/Possible contributor in the future", "Q97"] <- "New contributor"

activities_main <- activities[(activities$Q97=="Maintainer"),]
activities_freq <- activities[(activities$Q97=="Frequent Contributor"),]
activities_new <- activities[(activities$Q97=="New contributor"),]

head(activities)

#df <- within(df, {
#  f <- Name == 'John Smith' & State == 'WI'
#  Name[f] <- 'John Smith1'
#  State[f] <- 'CA'
#})
#activities_filtered<-activities[!is.na(activities$Q66) | is.na(activities$Q74),]


#activities %>% replace_na(list(Q74 = "unknown", Q78 = "unknown", Q7 = "unknown", Q12 = "unknown", Q77 = "unknown", Q93 = "unknown"))


# all participants
colnames(activities)
#colnames(activities) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed", "1st", "2nd", "3rd", "4th", "5th","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities)
str(activities)
#cols <- c("Understand the issue","Feel confident to contribute","Setup environment","Understand the Context","Understand what needs to be changed") 
cols <- c("Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed")
activities[cols] <- lapply(activities[cols], factor, levels=c("5","4","3","2","1"), order=TRUE)
#activities[cols] <- lapply(activities[cols], factor, levels=c("5","4","3","2","1"))
#activities_filtered<- filter(activities,Gender!= "Did not Inform" & "The majority of the developers and I want the same thing"!= "Did not Inform" & "I don't feel at home in the group"!= "Did not Inform" & "A majority of developers in the group know me" != "Did not Inform" & "If I have a problem, I know members in the group who I can ask for help" != "Did not Inform" & "I want to contribute more but I do not feel valued" != "Did not Inform")

plot(likert(activities[,2:6])) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes


#plot(likert(activities[,2:6], grouping=activities$years.of.programming),col = brewer.pal(n=5,"RdYlBu"))
plot(likert(activities[,2:6], grouping=activities$years.of.programming)) +
scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities[,2:6], grouping=activities$years.contributing)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities[,2:6], grouping=activities$frequent)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities[,2:6], grouping=activities$age.group)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities[,2:6], grouping=activities$gender)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities[,2:6], grouping=activities$Country)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

plot(likert(activities[,2:6], grouping=activities$role)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

#maintainers

colnames(activities_main)
#colnames(activities) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_main) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed", "1st", "2nd", "3rd", "4th", "5th","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_main)
str(activities_main)
#cols <- c("Understand the issue","Feel confident to contribute","Setup environment","Understand the Context","Understand what needs to be changed") 
cols <- c("Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed")
# must keep the string instead of number (5,4,3,2,1)
#activities_main[cols] <- lapply(activities_main[cols], factor, levels=c("Essential","Important","Moderately Important","Slightly Important","Unimportant"), order=TRUE)
activities_main[cols] <- lapply(activities_main[cols], factor, levels=c("5","4","3","2","1"))
#activities_filtered<- filter(activities,Gender!= "Did not Inform" & "The majority of the developers and I want the same thing"!= "Did not Inform" & "I don't feel at home in the group"!= "Did not Inform" & "A majority of developers in the group know me" != "Did not Inform" & "If I have a problem, I know members in the group who I can ask for help" != "Did not Inform" & "I want to contribute more but I do not feel valued" != "Did not Inform")

plot(likert(activities_main[,2:6]),legend.position = "right") +
  #scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("Unimportant", "Slightly Important", "Moderately Important", "Important", "Essential")) #order and color the likert boxes
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes


#plot(likert(activities[,2:6], grouping=activities$years.of.programming),col = brewer.pal(n=5,"RdYlBu"))
plot(likert(activities_main[,2:6], grouping=activities_main$years.of.programming)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_main[,2:6], grouping=activities_main$years.contributing)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_main[,2:6], grouping=activities_main$frequent)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_main[,2:6], grouping=activities_main$age.group)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_main[,2:6], grouping=activities_main$gender)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_main[,2:6], grouping=activities_main$Country)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

plot(likert(activities_main[,2:6], grouping=activities_main$role)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

#frequent contributors

colnames(activities_freq)
#colnames(activities) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_freq) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed", "1st", "2nd", "3rd", "4th", "5th","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_freq)
str(activities_freq)
#cols <- c("Understand the issue","Feel confident to contribute","Setup environment","Understand the Context","Understand what needs to be changed") 
cols <- c("Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed")
activities_freq[cols] <- lapply(activities_freq[cols], factor, levels=c("5","4","3","2","1"), order=TRUE)
#activities[cols] <- lapply(activities[cols], factor, levels=c("5","4","3","2","1"))
#activities_filtered<- filter(activities,Gender!= "Did not Inform" & "The majority of the developers and I want the same thing"!= "Did not Inform" & "I don't feel at home in the group"!= "Did not Inform" & "A majority of developers in the group know me" != "Did not Inform" & "If I have a problem, I know members in the group who I can ask for help" != "Did not Inform" & "I want to contribute more but I do not feel valued" != "Did not Inform")

plot(likert(activities_freq[,2:6])) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes



#plot(likert(activities[,2:6], grouping=activities$years.of.programming),col = brewer.pal(n=5,"RdYlBu"))
plot(likert(activities_freq[,2:6], grouping=activities_freq$years.of.programming)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_freq[,2:6], grouping=activities_freq$years.contributing)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_freq[,2:6], grouping=activities_freq$frequent)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_freq[,2:6], grouping=activities_freq$age.group)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_freq[,2:6], grouping=activities_freq$gender)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_freq[,2:6], grouping=activities_freq$Country)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

plot(likert(activities_freq[,2:6], grouping=activities_freq$role)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

#new contributors

colnames(activities_new)
#colnames(activities) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_new) <- c("role","Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed", "1st", "2nd", "3rd", "4th", "5th","years.of.programming", "years.contributing", "frequent", "age.group", "gender", "Country") 
colnames(activities_new)
str(activities_new)
#cols <- c("Understand the issue","Feel confident to contribute","Setup environment","Understand the Context","Understand what needs to be changed") 
cols <- c("Understand.the.issue","Feel.confident.to.contribute","Setup.environment","Understand.the.Context","Understand.what.needs.to.be.changed")
activities_new[cols] <- lapply(activities_new[cols], factor, levels=c("5","4","3","2","1"), order=TRUE)
#activities[cols] <- lapply(activities[cols], factor, levels=c("5","4","3","2","1"))
#activities_filtered<- filter(activities,Gender!= "Did not Inform" & "The majority of the developers and I want the same thing"!= "Did not Inform" & "I don't feel at home in the group"!= "Did not Inform" & "A majority of developers in the group know me" != "Did not Inform" & "If I have a problem, I know members in the group who I can ask for help" != "Did not Inform" & "I want to contribute more but I do not feel valued" != "Did not Inform")

plot(likert(activities_new[,2:6])) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes



#plot(likert(activities[,2:6], grouping=activities$years.of.programming),col = brewer.pal(n=5,"RdYlBu"))
plot(likert(activities_new[,2:6], grouping=activities_new$years.of.programming)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_new[,2:6], grouping=activities_new$years.contributing)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_new[,2:6], grouping=activities_new$frequent)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_new[,2:6], grouping=activities_new$age.group)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_new[,2:6], grouping=activities_new$gender)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes
plot(likert(activities_new[,2:6], grouping=activities_new$Country)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

plot(likert(activities_new[,2:6], grouping=activities_new$role)) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = c("1", "2", "3", "4", "5")) #order and color the likert boxes

#alternative
p <- likert(activities_main[,2:6]) 

a <- likert.bar.plot(p, legend.position = "right", text.size = 4) +
  
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  
  theme_update(legend.text = element_text(size = rel(0.7))) +
  
  theme_classic()

plot(a)


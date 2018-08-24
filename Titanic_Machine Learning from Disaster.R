#The whole point of this dataset is to build a machine learning model that can predict whether a passenger survived or not.

#Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Add a "survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#combine data sets
data.combined <- rbind(train, test.survived)

#This is how R interpret the data which we have downloaded from kaggel
str(data.combined)     #structure of this data set

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

#Take a look at gross survival rates
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

# Load up ggplot2 package to use for visualizations. Before this line we have install package ggplot2
library(ggplot2)


#in ggplot function fir we pass the data we are interested in plotting(more often than not that's going to be a data frame)
#train variable is a data frame. Next we have to tell ggplot some aspects of the aesthetic(i.e aes()).
#aes() generates aesthetic mappings. Means is that you are controlling the way the graph or plot is going to look.
#And what we are saying is that hey the aesthetic will have an x-axis corresponding to the Pclass variable of the train dataframe.
#and we are also telling ggplot that we would also like a fill color to be used this optional you have to specify an X at 
#a minimum right you have to have an x-axis at a minimum otherwise you are not gonna have a plot but the filled variable
#is optional but its gonna be helpful bcoz we are going to say is look color code the resulting plot based on the survived
#variable and survived variable was not a factor we changed it to factor on the go using factor().
#lastly we are telling ggplot that you got the data, u got some aesthetics now plot a histogram of the given width.
#then x-label, y-label labs() is saying i want to assign to the fill a label of survived.

#Hypothesis - Rich Folks survived at a higher rate.

train$Pclass = as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


#fill = factor(Survived))  has generated the color green and orange in the plot

#From data analysis perspective it seems that Pclass is important for our problem at hand which is determining the pattern
#of who survived and who did not on the Titanic.

#Examine the first few names in the training data set.
head(as.character(train$Name))

#How many unique names are there in combined dataset
length(unique(as.character(data.combined$Name)))

#Two duplicate names
#First, Get the duplicate names and store them as a vector.
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"]) 


#Next take a look at the records of the duplicated value in data.combined
data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$title = as.factor(titles)

# Since we only have survived lables for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$Sex)

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)     #We have a lot of missing values. Which is not good.

#Lets check how many missing values are there in training data.
summary(data.combined[1:891, "Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)   #Here it shows only 8 missing values are there out of 263 missing values.
#Master seems to be at this point to be a very good proxy for male children, which means reflexively Mr. is a good proxy 
#for adult males. So we dont need to worry about age variable for males in this data set. 


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)   #50 missing values are here

#You can see here that no. of women in 3rd class are more. And in 3rd class as the miss. gets older her chances of survuival
#gets bad.
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age of Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
#It is filtering out misses who is travelling with no siblings or spouse and with no parents or children.
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]  
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))   #Here we took 14.5 because when we did summary(boys$Age) the max age was 14.5
#Summary() and length() also confirms that if miss. is travelling alone then most probably they are an adult.

#So from now on we can say that if you are a miss. then and you are travelling alone we are just going to say that you 
#are an adult female. Because, in misses.alone total 150 observations was there (which you can see on top-right corner)
#and out of 150 only 4 are there with age less than 14.5. We will be wrong for some number of time but not too bad.
#Will work for hypothesis.
  

# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)

#can we treat it as a factor
length(unique(data.combined$SibSp))
#Across the entire data set only 7 unique values are there so we can change it to a factor
#with 7 unique values means someone is travelling with 1 sibling, 2 siblings, 3 siblings, and so on.. 

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#In the plot x-axis has 0,1,2,3,4,5,8   meaning 7 unique values of siblings. either 1 sibling, 2 siblings, 3 siblings....
#so in the plot if u will see 3rd class Master data... the survival rate decreases if u have more siblings. U survive if
#in a smaller family.

length(unique(data.combined$Parch))
length(data.combined$Parch)

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


temp.sibsp = c(train$SibSp, test$SibSp)  #Reason we are taking these values from test and train data and not from
#data.combined because  we transformed the values to factor in data.combined and here we need its original datatype.
temp.parch = c(train$Parch, test$Parch)
#c() is the combine function.

data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)
#Here we have created a new column family size by combining sibsp and parch and now we will see what is the survival 
#rate based on the family size.

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Take a look at the ticket variable
str(data.combined$Ticket)    #str means structure

# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$Ticket = as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

# OK, we can make a factor for analysis purposes and visualize
data.combined$Ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")
#we didnt see anything unique here. The survival rates are as expected.


# Next up - the fares Titanic passengers paid
summary(data.combined$Fare)
#some folks didnt pay at all, median is 14.45 that means 50% of the passengers paid less than 14.5.
#But the mean is more than double the median which means that distribution we would expect to be skewed to the high end.
length(unique(data.combined$Fare))


# Can't make fare a factor, treat as numeric because no. of unique values are largeer & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

#Value near 500 on x-axis shows that there are not many people paying 500.

# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")

# This R script will run on our backend. You can write arbitrary code here!


# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)


# Take a look at gross survival rates
table(data.combined$survived)


# Distribution across classes
table(data.combined$pclass)


# Load up ggplot2 package to use for visualizations
library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$sex == "male"), ]
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)

# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# What's the distribution of females to males across train & test?
table(data.combined$sex)


# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$age)
summary(data.combined[1:891,"age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None" & !is.na(misses$age),], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# Move on to the sibsp variable, summarize the variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Take a look at the ticket variable
str(data.combined$ticket)

# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


# Next up - the fares Titanic passengers paid
summary(data.combined$fare)
length(unique(data.combined$fare))


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")


# Analysis of the cabin variable
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]    #I have put 100 just to see clearly how data looks like


# Replace empty cabins with a "U"
#Below line is telling grab all the indices of the cabin variables then grab the cabin variable and add U to the blank space
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

#Add to the combined data set and plot
data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivbility by cabin first char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")


ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivbility by cabin first char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Survivbility by cabin first char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
str(data.combined$cabin.multiple)

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


library(randomForest)
# Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]    #Now i got my training data
rf.label <- as.factor(train$Survived)  #And here i got my labels

set.seed(1234) #if you want to have some sort of reproduciblility or some ways to evaluate different runs of the random
#forest algorithm setting the seed is really imprtnt otherwise you are going to get slightly different results every single
#time that's by design, the way you counteract that is by set.seed and this basically says look this is how you can set
#the state for the random number generator . so by setting it to 1234(classic eg.) we can get some reproducibility.
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
#here we call the randomForest funtion which actually trains an instance of a random forest for us
rf.1   #it will show us the confusion matrix
#OOB stands for "out of bag" you are seeing this OOB in the output. it is saying error rate 20% which is pretty good 
#because we are taking two variables Pclass and title and for these two variables the accuracy is 80% which is extremely
#good for predicting whether you lived or died.
varImpPlot(rf.1)
#it will draw a graph. the farther the point is the more predictive that variable is. Here title is more farther and 
#hence is way more predictive that Pclass.

# Train a Random Forest using pclass, title, & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)
#Here, when we add a third variable we gor the error rate 19.53 % < earlier 20% 
#also see confusion matrix

# Train a Random Forest using pclass, title, & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


# Train a Random Forest using pclass, title, sibsp & Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#we are using the same seed everytime so that we can actually compare the models legitiately and say okay we know that
#the  random seed kis going to be the same so the processing of the algo. is going to start at exactly the same point every 
#time so that we can know that the only thing that varies between the results are the features i.e "Pclass", "title", "SibSp", "Parch"
#that we are plugging in


# Train a Random Forest using pclass, title, & family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)
table(rf.label)
#though family size is combination of sibsp and parch but sometimes it better to say total size suppose 4 instead of
#saying 2 + 2 because it might happen that in the tree it first goes to sibsp to 2 then check parch 2. So sometimes 
#deliberately enginerring a feature may be potentially helpful.

# Train a Random Forest using pclass, title, sibsp & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)
#Here error rate increased. So better not to add sibsp with  family size


# Train a Random Forest using pclass, title, Parch & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)
#Here error rate increased.

#so our most accurate result is from rf.5
rf.5



# Before we jump into features engineering we need to establish a methodology
# for estimating our error rate on the test set (i.e., unseen data). This is
# critical, for without this we are more likely to overfit. Let's start with a 
# submission of rf.5 to Kaggle to see if our OOB error estimate is accurate.

# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "family.size")]
rf.5.preds <- predict(rf.5, test.submit.df)  #Make some predicions using rf.5(the object we created by training random forest)
#and use test.submit.df (df means dataframe)  as input, create those predictions and store them in the given variable.
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
#Most thigd in R, it all starts from a dataframe. Give it a structure. Give two columns one is passengerId and other 
#is Survived. We gave passengerId number from 892 to 1309. So rep() means replicate elements of vectors and list.
#and in survived column it will have 0's and 1's

write.csv(submit.df, file = "RF_Titanic_1.csv", row.names = FALSE)
#By default row.names is True, so if you will not put it false it will add a column called row name in your file which 
#you dont want because we have to make a file with only 2 columns named passengerid and survived.


# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
#Caret means classification and regression training.
#help(package = "caret")   write this below and see.
library(doSNOW)
#It parralalize the work for caret.


# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# however there are no hard and fast rules - this is where the experience of the 
# Data Scientist (i.e., the "art") comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.

# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.

#we need 100 distinct slices of the data in such a way that we can use those over and over again because what we want
#to do is we can't use the same 10 folds 10 different times because this is going to give us the same result. we actually
#need randomize it so we actually need 100 folds 10 collections of 10 folds where each ideally the ten groups of folds 
#are different from each other. So caret to the rescue, createMultiFolds() will do it.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
#caretMultiFolds() is the part of family of functions for data splitting operations and caret.
#caret not only handle randomization for you but can also take care of stratification. Stratification is imp. because 
#as we know in the titanic dataset more people died than survived. we know actually the proportion got worsed based on 
#pclass eg. males in 3rd class died in much much higher rates. so we can slice this data into 100 folds by 10 groups and 
#10 folds. and as we did this randomly we can get some folds where everyone is perished. Data is not skewed very well so
#what we can do is stratify Which means that take the proportion of your labels and aggregates and makes sure every folds
#that is randomly genarated has the same proportion approximately (with same proportion i mean we know that 2/3rd people 
#died and 1/3rd survived. so folds should have same proportion approx.) so this is taken care by caret automatically.
#so anytime we talk about any sort of randomzation in R, if we want to make sure that we can  get reproducibility we need 
#to set the random seed.
#in createMultiFolds() we gave rf.label, which is our labels that's what we trained our rf.5 with.
#so it says i can do two things: i can figure out the proportion so i can make sure the statification works correctly 
#and it also gives me the indexing as well right because what i will do is i will just pull back a list of indexes 
#this is what createMultiFolds returns is just a bunch of indexes into  your training set because why give you the full
#records thats just that's just a bunch of data that you will have to store memory thats redundant if we will give 
#you the indexes you can pull directy from the training set.
#K says how many folds and how many times.


# Check stratification
table(rf.label)
342 / 549
table(rf.label[cv.10.folds[[33]]]) #grab the 33rd entry in the list of 100 from cv ten folds
308 / 495
#proportion is almost same. So startification works.


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)
#What it basically tells: hey look i want you to train my model using repeatedcv(i.e repeated cross validation). I want you to
#use 10 folds per iteration, and i want you to do 10 iterations  and by the way here are 100 collections of indexes 100 lists
#of the indexes to use for each of those iterations caret is smart enough to use them all so now what we have got is you know
# ideal 10-fold cross validation repeated 10 times with stratified data in each of the folds.


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
#How doSnow works is that it sets up esstetially child processes that are subordinate to the main thread here in R and uses
#sockets to connect to them. It ensures that you can  totally just stree out your CPU to all get out which is great because 
#when you are starting to do some of these large-scla competition lots and lots of cores is a good thing

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
#First thong we are going to do is basically telling hey how many subprocesses should i set up to help me do my training 
#faster in parallel.
#So it says, look creat a cluster and i want 6 child processes and i want type socket server. Choose this number carefully. 
#My computer is quadcore so i can write 6 here. So create the cluster object in cl variable we then have to register, because if
#we dont use it then caret will not recognize that you set up a cluster to do work in parallel so you have to register it first.
#once you do that caret is smart enough to do training in parallel.

#Now we are ready to do my training.
#set seed for reproducability and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1 )
#method = "rf" tells that use the random forest method to train
#with tunelength  we are saying try we 3 different values and see for which valu it works well means which gives u the best score
#for 10 fold cross validation repeated 1o times

#shut doen cluster
stopCluster(cl)

#check out results
rf.5.cv.1



# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
#Lets try with smaller number of folds Which means that i actually get less data to actually tráin my algorithm this also means 
#that my test set is larger
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)

#shut doen cluster
stopCluster(cl)

#check out results
rf.5.cv.2

#The accuracy does get better. But  5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3
#The accuracy is not good compared to others.




# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.

# Install and load packages
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#let's use 3-fold CV repeated 10 times 

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  rpart.cv <- train(training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)  #here we have changed method from randomforest to rpart
  stopCluster(cl)
  return(rpart.cv)
}

# Grab features
features <- c("Pclass", "title", "family.size")  #combine values into a vector and craming that int a variable called features.
#this allows me to do slice and dice a dataframe like this in below line (where i have used features).
rpart.train.1 <- data.combined[1:891,features]

rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)   #Calling user defined function above to run my 3 folds cross validation 
#repeated 10 times
rpart.1.cv.1
#Accuracy is better than random forest. Here we are making only a single tree which is fitting a little bit better.
#While Random forest was trying to make many trees which tends to overfit.

#plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# some interesting facts about this plot:
# IF your title was Mr or other you most certainly perished with an accuracy of 83.2%
# If you were among Mrs, Miss or Master and you were not in 3rd class, you survived with an accuracy of 94.4%
# If you were in 3rd class and had a family size of 5,6, 8,11 you perished 100% of the time and 
# if you were a smaller family you had a greater than 50% chance of survival.
#in the plot the first number is perished and the 2nd number is survived number.
#and 1 and 0 in circles means survived or not.

#BOTH rf and rpart confirms that title is important. Lets look into it deeper.
table(data.combined$title)
data.combined[1:25, "Name"]

name.splits <- strsplit(as.character(data.combined$Name), ",")
name.splits[1]
last.name <- sapply(name.splits, "[", 1)
#spapply is the prefered way to itereate over list or vector.
#sapply function is taking the list i.e name.splits  and then apply the indexing operator to it i.e "["
#indexing operator means each iteration of sapply will get like this [1] "Braund"   "Mr. Owen Harris"  which is basically
#the output of name.splits[1]. So in each iteration sapply will get this and then we wrote 1 which means grab me the 1st element
#which is the last name i.e Braund . and so on for other names.
last.name[1:10]

#Add last names to dataframe in case we find it useful later.
data.combined$last.name <- last.name

# Now splitting the titles
name.splits <- strsplit(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)   # this analysis includes most of the title are used for designation of royal classes and nothing exceptional 
#can be extracted

#Whts up with the title of "THE"
data.combined[which(titles == "the"),]

#remap the titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
unique(titles)
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles %in% c("Mme.")] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer."
titles[titles %in% c("Lady")] <- "Lady."
table(titles)


#Make title a factor
data.combined$new.title <- as.factor(titles)

#visualize the new version of the title 
ggplot(data.combined[1:891,], aes(x =  new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival rate for new.title by pclass")

#collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer.")
data.combined$new.title[indexes] <- "Mr."

#visualize
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar()+
  facet_wrap(~Pclass) +
  ggtitle("Survival rate for new.title by pclass")


#grab features
features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

#Run CV and check the result
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Lets see 1st class Mr.
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)
#In sex there is one female. How is this possible. We will explore it. This is the prime example of doing data analysis in depth

first.mr.df[first.mr.df$Sex == "female",]


#update new.title feature
indexes <- which(data.combined$new.title == "Mr." &
        data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#Any other gender slip-ups?
length(which(data.combined$Sex == "female" &
        (data.combined$new.title == "Master." |
        data.combined$new.title == "Mr.")))

#Refresh the data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

#let's look at survivng 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])   #this will open the file.

#Take a look at some of the high prices
indexes <- which(data.combined$Ticket == "PC 17755" |
        data.combined$Ticket == "PC 17611" |
        data.combined$Ticket == "113760")

View(data.combined[indexes,])

#here every person is servived. So visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class Mr. Survival Rates by fare")
#density plot is useful for numeric data  to visualize over the range of values. How they fluctuate in terms of density.
#0 means perished, 1 = survived, None = data from test set which are none.

# this analysis provides us with the insight that, the ticket fare column is provided for an entire party size or family size. 
#We can divide the fare by the family size to get an avg fare, which will help us improve our model accuracy. 

#Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)
#we use rep() to create a vector of 1309(i.e total no. of records)  0's.

for(i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1: length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

#Refresh 1st class Mr. data frame
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# visualize the new features added 
ggplot (first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size , fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("first class mr with avg fare and survival")

ggplot (first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare , fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("first class mr with avg fare and survival")

# hypothesis - ticket party size and avg fare is highly corelated , so we use only one feature
summary(data.combined$avg.fare)
#one missing value. So we will look.
data.combined[is.na(data.combined$avg.fare),]


#Get records for similar passengers and summarize avg. fare
indexes <- with(data.combined, which(Pclass == 3 & title == "Mr." &
                                      family.size == 1 & Ticket != 3701))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

#use median since close to mean and a little higher than mean 
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# normalizing the data using preprocess function in caret
preproc.data.combined <- data.combined[, c("avg.fare","ticket.party.size")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))
#method = "center" subtracts the mean of the predictor's data (again from the data in x) from the predictor values while
#method = "scale" divides by the standard deviation.
postproc.data.combined <- predict(preProc, preproc.data.combined)
#now we see a much more normalzied data

#hypothesis refuted for all data
# for machine learning algos like SVM, THEY require the data to be normalized 
# using the cor function, we can get the corelation between any two variables 
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)
#this shows that these two values are highly uncorelated. Which is good which means we have potentially two new features that we can 
#use to extract predictive power out of our data set .
#1 means highly corelated.


# to check the corelation between first class and avg fare 
indexes <- which(data.combined$Pclass == 1)
cor(postproc.data.combined$avg.fare[indexes], 
    postproc.data.combined$ticket.party.size[indexes])
#hypothesis refuted again
# this result of 0.25 indicates the data is only slightly corelated and we refute the hypothesis that first class and
#avg fares are corelated


#ok, let's see if our feature engineering has made any difference
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

#RUN CV and see the results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

#plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)
#family size is no longer here. We now have more generalized decisions.



# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")]
kaggel.predict <- predict(rpart.3.cv.1, test.submit.df)  #Make some predicions using rf.5(the object we created by training random forest)
#and use test.submit.df (df means dataframe)  as input, create those predictions and store them in the given variable.
table(kaggel.predict)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = kaggel.predict)
#Most thigd in R, it all starts from a dataframe. Give it a structure. Give two columns one is passengerId and other 
#is Survived. We gave passengerId number from 892 to 1309. So rep() means replicate elements of vectors and list.
#and in survived column it will have 0's and 1's

write.csv(submit.df, file = "crt_Titanic_2.csv", row.names = FALSE)
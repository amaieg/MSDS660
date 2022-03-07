# highlight a section and press ctrl+enter to run,
# or click run above.  Always better to use
# keyboard shortcuts because they're faster

# if you forgot, this will check if the 
# data.table package is installed
# If not, it will install it
if (!require(data.table)) {
  install.packages('data.table')
  require(data.table)
}
library(data.table)
# set the filepath and file name
filepath = 'C:/Users/ksorauf/OneDrive - Regis University/MSDS 660/22S8W2/Week 1/'
filename = 'WV3_Data_x_v_2015-04-18.rdata'


# this is how we read in a .rdata file
load(paste(filepath, filename, sep=''))

# this converts to a data.table, which is easier
# to work with than other formats in R
wv3 = as.data.table(`WV3_Data_R_v_2015-04-18`)

# shows number of rows and columns, always good
# to check first and look at the data using
# summary and head
dim(wv3)
summary(wv3)
head(wv3)

# check the .pdf file to see column definitions
# Respondents interest in interview is V231

# google 'data.table cheat sheet' for a quick
# summary of how to manipulate data.tables
# basically, we do dt[row, columns, groupby]

#How interested was the respondent? Problem?
hist(wv3[, V231])

# seems like there were some erros.
hist(wv3[V231 > 0, V231])

# How long did interview take?
hist(wv3[, V230])
summary(wv3$V230)
#Looks like some errors

# I like to look at box plots
# It really helps highlight outliers.
boxplot(wv3$V230)

#Sort the values and count the frequency
sort(table(wv3[, V230]), decreasing=T)

# Remove the outliers
dim(wv3)
wv3_duration <- wv3[V230 <= 250, ]
wv3_duration <- wv3_duration[V230 > 0, ]
dim(wv3_duration)
hist(wv3_duration[, V230])
boxplot(wv3_duration[, V230])

# Some the data is cleaned.  Now we will propose a hypothosis and use the WV3 survey data
# to either support or oppose the hypothosis

# Hypothesis: As a persons education level increases they are more 
# likely to believe women are equitable political leaders.

# Question V101: On the whole, men make better political leaders than women do. 
# 1 = Strongly agree; 4 = Stongly Disagree'

table(wv3_duration[, V101])
hist(wv3_duration[, V101])
hist(wv3_duration[V101 > 0, V101])

#Boxplot is not very useful because of the catagorial nature of the respose.
boxplot(wv3_duration[V101 > 0, V101])

# Question V217: What is the highest educational level that you have attained? 
# 1 = No formal Education; 9 = University level with degree
table(wv3_duration[, V217])
hist(wv3_duration[V217 > 0, V217])
boxplot(wv3_duration[V217 > 0, V217])

if (!require(plyr)) {
  install.packages("plyr")
  require(plyr)
}

library(ggplot2)


# Two good pages to help plot histograms with multiple groups
# https://uc-r.github.io/histograms#groups 
# https://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group


wv3_filtered <- wv3_duration[V217 > 0 & V101 > 0]

wv3_filtered$Education <- as.factor(wv3_filtered$V217)
wv3_filtered$Education <- plyr::revalue(wv3_filtered$Education,
                                        c("1" = "No formal education",
                                          "2" = "Incomplete primary school",
                                          "3" = "Complete primary school",
                                          "4" = "Incomplete secondary school: tech/vocational",
                                          "5" = "Complete secondary school: tech/vocational",
                                          "6" = "Incomplete secondary school: preparatory",
                                          "7" = "Complete primary school: preparatory",
                                          "8" = "Some university without a degree",
                                          "9" = " University with degree"))


ggplot(data=wv3_filtered, aes(x=V101, fill=Education)) + 
  geom_histogram(aes(y=..density.. * 0.25), binwidth=0.5) +
  facet_wrap(~Education, nrow=4) + 
  scale_x_continuous(breaks = 1:4, labels=c("1" = "Stongly agree", "2" = "Agree",
                                            "3" = "Disagree", "4" = "Strongly disagree")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Q: On the whole, men make better political leaders than women do',
       y = 'Noramlized Response',
       title = 'How education changes peoples opinion of women in poltics' ,
       subtitle = 'As education increases people are more accecpting of women in politics')



# It appears there is a small trend indicating the more educated a person becomes
# the more they dissagree that men make better polticians. 

# Let us combine the stongly agree and agree resutls and combine the 
# strongly disagree and disagree results.

# Convert V101 to 0 if the respondents agree or stongly agree and convert to 1 if respondeds disagree
# or strongly disagree.

wv3_combined <- wv3_duration[V217 > 0 & V101 > 0]
wv3_combined$V101[wv3_combined$V101 <= 2] <- 0
wv3_combined$V101[wv3_combined$V101 >= 3] <- 1
wv3_combined$V101

# Plot the new data
wv3_combined$Education <- as.factor(wv3_combined$V217)
wv3_combined$Education <- plyr::revalue(wv3_combined$Education,
                                        c("1" = "No formal education",
                                          "2" = "Incomplete primary school",
                                          "3" = "Complete primary school",
                                          "4" = "Incomplete secondary school: tech/vocational",
                                          "5" = "Complete secondary school: tech/vocational",
                                          "6" = "Incomplete secondary school: preparatory",
                                          "7" = "Complete primary school: preparatory",
                                          "8" = "Some university without a degree",
                                          "9" = " University with degree"))


ggplot(data=wv3_combined, aes(x=V101, fill=Education)) + 
  geom_histogram(aes(y=..density.. * 0.5), binwidth=0.5) +
  facet_wrap(~Education, nrow=4) + 
  scale_x_continuous(breaks = 0:1, labels=c("0" = "Stongly agree/Agree", "1" = "Disagree/Strongly Disagree")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  labs(x = 'Q: On the whole, men make better political leaders than women do',
       y = 'Noramlized Response',
       title = 'How education changes peoples opinion of women in poltics' ,
       subtitle = 'As education increases people are more accecpting of women in politics')

# It appears there is a small trend indicating the more educated a person becomes
# the more they dissagree that men make better polticians. 



##### Continue using this R file to complete Week 1 Disucssion Questions#####

# Continuing with the Week 1 World_Values_Survey.R file in Week 1 Contents, 
# create a discussion post that contains the following:
  
# an hypothesis regarding the effect of tv on income and happiness
# problems or concerns you have with the data
# a histogram plot – include axis label the axis and title plot
# interpretation of your results




# ---------------------------------------------------------Header--------------------------------------------------------------- 
# Name: Maaz Siddiqui
# Year: Summer 2020
# Assignment: Data Project #6
# Description: Dataset was obtained from problem 42 in Section 10.3 (Supplementary Exercises) from Probability and Statistics
#              for Engineering and the Sciences, 9th Ed, by Jay L. Devore. Cengage Learning, 2016. The data was to be analyzed
#              numerically and graphically. In-depth analysis can be found in the Word document submitted along with this code.
#              This code includes the data set, statistcal description of the data (such as mean, trimmed mean, etc.), ANOVA 
#              analysis and graphical representation of the data in the form of a qqplot and TBD.
# ------------------------------------------------------End Header--------------------------------------------------------------

# --------------Overview--------------
# An investigation is conducted to determine whether true mean critical flicker frequency (cff) is related to iris color. There
# were three iris colors that were studied within the investigation. These were brown, green, and blue.
# Nomenclature:
#    - xbar, ybar, gbar => sample means
#    - s => sample standard deviation
#    - sample1, sample2, sample3 => data for iris colors respectively.
# ***IMPORTANT: Install and load package "onewaytests" into library before running the rest of the code
install.packages("onewaytests")
library("onewaytests")

# --------------Data Organization, ANOVA, & Descriptive Stats--------------
# Brown Iris
sample1 = c(26.8,27.9,23.7,25.0,26.3,24.8,25.7,24.5)
xbar = 25.59
s1 = sd(sample1)

# Green Iris
sample2 = c(26.4,24.2,28.0,26.9,29.1)
ybar = 26.92
s2 = sd(sample2)

# Blue Iris
sample3 = c(25.7,27.2,29.9,28.5,29.4,28.3)
gbar = 28.17
s3 = sd(sample3)

# describe(collectionOfCFF~irisNames, data=my_data)                  # descriptive statistics

# Formatting of data
collectionOfCFF = c(sample1, sample2, sample3)
irisNames = as.factor(c(rep("Brown",length(sample1)), rep("Green",length(sample2)), rep("Blue",length(sample3))))
my_data = data.frame(collectionOfCFF, irisNames)

# Assumptions
sds = c(s1,s2,s3)
max(sds)/min(sds)
normCheck = c(sample1-xbar, sample2-ybar, sample3-gbar)
qqnorm(normCheck)                                                    # Normality check
qqline(normCheck)
bartlett.test(collectionOfCFF~irisNames, data=my_data)               # test to affirm homogeneity of variances of groups

# ANOVA
results = aov.test(collectionOfCFF~irisNames,data=my_data)
summary(results)

# post hoc testing
paircomp(results, adjust.method = "holm")       # Holm

# --------------Plots--------------
boxplot(sample1,sample2,sample3,                                     # boxplot
        names = c("Brown","Green","Blue"),       
        main = "Comparison of cff for Iris Colors",                 
        xlab = "Iris Color",
        ylab = "Critical Flicker Frequency (cycles/sec)")

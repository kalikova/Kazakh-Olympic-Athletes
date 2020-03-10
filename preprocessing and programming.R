#import the "athlete_events" data 
athlete_events <- read.csv("~/Desktop/CS544Final_Kalikova/athlete_events.csv")
#subset the data only for team from Kazakhstan, and export it into "kazAthlete" csv file
athletesKazakhstan = subset(athlete_events, Team == "Kazakhstan")
write.csv(athletesKazakhstan, "kazAthlete.csv")


#import the "kazAthlete" data
kazAthlete <- read.csv("~/Desktop/CS544Final_Kalikova/kazAthlete.csv")




#Part A) categorical variables
#analysing and showing barplot and pie for Sex variable
gender = table(kazAthlete$Sex, exclude = FALSE)
gender
gender/length(kazAthlete$Sex)
barplot(gender, 
        col = c("red", "blue"), ylim = c(0, 1000),
        main = "Athlete's Gender",
        xlab = "Gender", ylab = "Frequency")

slice.labels = names(gender)
slice.percents = round(gender/sum(gender) *100)
slice.labels = paste(slice.labels, slice.percents)
slice.labels = paste(slice.labels, "%", sep = "")
pie(gender, labels = slice.labels,
    col = c("red", "blue"))

#analysing and showing barplot for Sport variable
sports = table(kazAthlete$Sport)
sports
barplot(sports, col = rainbow(36), xlim = c(0, 250), 
        horiz = TRUE, las = 2, cex.names=0.3,
        main = "Type of Sports", xlab = "Frequency")

#Part A) numerical variables
#analysing and showing barplot for variable Year
table(kazAthlete$Year)
#analyzing which year had the higher number of athletes
which(table(kazAthlete$Year) == max(table(kazAthlete$Year)))
barplot(table(kazAthlete$Year), horiz = TRUE,
        col = rainbow(12), xlim = c(0, 200),
        main = "Olympic Year", las = 2, cex.names=0.7,
        ylab = "Year", xlab = "Numbers of Athletes")

#analysing and showing barplot for variable Height
table(kazAthlete$Height, exclude = FALSE)
#analyzing the higher number of athletes' height
which(table(kazAthlete$Height) == max(table(kazAthlete$Height)))
barplot(table(kazAthlete$Height),
        col = "cyan", ylim = c(0, 100),
        main = "Athlete's Height", las = 2,
        xlab = "Height", ylab = "Frequency")

#analysing and showing barplot for variable Weight
table(kazAthlete$Weight, exclude = FALSE)
#analyzing the higher number of athletes' weight
which(table(kazAthlete$Weight) == max(table(kazAthlete$Weight)))
barplot(table(kazAthlete$Weight),
        col = "cyan", ylim = c(0, 100),
        main = "Athlete's Weight", las = 2,
        xlab = "Weight", ylab = "Frequency")




#Part B) set of two or more variables
attach(kazAthlete)

table(Medal, exclude = FALSE)
medals = table(Sex, ordered(Medal, levels = c("Gold", "Silver", "Bronze")))
medals
addmargins(medals)
prop.table(medals)

mosaicplot(medals, col= c("yellow", "grey", "brown"),
           main = "Medals by Gender",
           ylab = "Medal", xlab = "Gender")

par(mfrow = c(1,2))
barplot(medals, beside = TRUE, ylim = c(0, 25),
        legend.text = TRUE, args.legend = list(x = "top"),
        col = c("red", "blue"), xlab = "Medal")

barplot(t(medals), beside = TRUE, ylim = c(0, 30),
        legend.text = TRUE, args.legend = list(x = "top"),
        col = c("yellow", "grey", "brown"), xlab = "Gender")
par(mfrow = c(1,1))

detach(kazAthlete)




#Part C) distribution of the numerical data
age = kazAthlete$Age

mean(age)
median(age)
var(age)
sd(age)
fivenum(age)
summary(age)

f = fivenum(age)
c(f[2] - 1.5*(f[4] - f[2]),
  f[4] + 1.5*(f[4] - f[2]))

boxplot(age, horizontal = TRUE, xaxt = "n", notch = TRUE, col = "cyan")
axis(side = 1, at = fivenum(age), labels = TRUE)

table(kazAthlete$Age)
which(table(kazAthlete$Age) == max(table(kazAthlete$Age)))
barplot(table(kazAthlete$Age),
        col = "cyan", ylim = c(0, 160),
        main = "Age",
        xlab = "Age", ylab = "Frequency")




#Part D) various random samples of the data and the applicability of the Central Limit Theorem for this variable
age = kazAthlete$Age
mean(age)
sd(age)
hist(age, prob = TRUE, 
     xlim = c(10, 60), ylim = c(0, 0.1), 
     breaks = 15, las = 2)

samples<- 9000
xbar <- numeric(samples)
par(mfrow = c(2,2))
for (size in c(5, 10, 20, 30)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(age, size = size, replace = FALSE))
  }
  hist(xbar, prob = TRUE,
       breaks = 15, las = 2, ylim = c(0, 0.5),
       main = paste("Sample Size =", size))
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}
par(mfrow = c(1,1))





#Part E) how various sampling methods can be used on data
library(sampling)
dataSample = kazAthlete
head(dataSample[c(2,3,4,13)])
nrow(dataSample)

#Simple sampling
s = srswr(70, nrow(dataSample))
s [s !=0]
rows = (1:nrow(dataSample))[s!=0]
rows = rep(rows, s[s !=0])
rows
sample.1 = dataSample[rows, ]
head(sample.1[c(2,3,4,13)])
table(sample.1$Age)

#Systematic sampling
N = nrow(dataSample)
N
n = 70
k = ceiling(N/n)
k
r = sample(k,1)
r
s = seq(r, by = k , length = n)
s
sample.2 = dataSample[s, ]
head(sample.2[c(2,3,4,13)])
table(sample.2$Age)

#Stratified Sampling with Two Variables
data = data.frame(
  Season = kazAthlete$Season,
  Gender = kazAthlete$Sex)
head(data)
data = data[order(data$Season, data$Gender), ]
freq <- table(data$Season, data$Gender)
freq
st.sizes <- 70 * freq / sum(freq)
st.sizes = as.vector(t(st.sizes))
st.sizes = st.sizes[st.sizes != 0]
st.sizes
st.1 <- strata(data, stratanames = c("Season", "Gender"),
             size = st.sizes, method = "srswor")
sample.3 <- getdata(data, st.1)
head(sample.3)





#Implementation of any feature(s) not mentioned in the specification
library(tidyverse)
athletes_kaz = as_tibble(kazAthlete)
athletes_kaz
athletes_kaz %>% summary

#showing the maximum age recorded for each sport category
maxAgeSport = athletes_kaz %>% 
  group_by(Sport) %>%
  summarise(maxAge = max(Age))
maxAgeSport
# showing the plot for these values
plot(maxAgeSport$Sport, maxAgeSport$maxAge, 
     ylab = "max age", pch = 16, 
     las = 2, cex.axis = 0.4,
     main = "Maximum Age of Athletes \n in Each Sport Category")


# filter the Summer Season into athletesSummer
athletesSummer = filter(athletes_kaz, Season == "Summer")
athletesSummer

#e showing the average athletes' heights of Summer Season
avgHeightSummer = athletesSummer %>%
  filter(Height > 0) %>%
  group_by(Age) %>%
  summarise(avgHeight = mean(Height))
avgHeightSummer
# showing the plot for these values
plot(avgHeightSummer$avgHeight, ylim = c(150, 190),
     xlab = "age", ylab = "average height", pch = 16)
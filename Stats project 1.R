install.packages("readxl")   
library(readxl)

library(readxl)
data <- read_excel("astronauts.xlsx")

names(data)

astro1 <- data

astro1$Military_Status <- ifelse(
  is.na(astro1$`Military Rank`) & is.na(astro1$`Military Branch`),
  "Civilian",
  "Military"
)
table(astro1$Military_Status)

astro1 <- astro1[, c("Military_Status", "Space Flights")]
astro1 <- na.omit(astro1)

aggregate(`Space Flights` ~ Military_Status, data = astro1, mean)
aggregate(`Space Flights` ~ Military_Status, data = astro1, median)
aggregate(`Space Flights` ~ Military_Status, data = astro1, sd)

table(astro1$Military_Status)

## Question 1! do military pilots fly more than civilians

boxplot(`Space Flights` ~ Military_Status,
        data = astro1,
        main = "Space Flights by Background",
        xlab = "Astronaut Background",
        ylab = "Number of Space Flights")

means <- tapply(astro1$`Space Flights`, astro1$Military_Status, mean)

points(1:2, means, col = "red", pch = 19, cex = 1.5)

means <- tapply(astro1$`Space Flights`, astro1$Military_Status, mean)
sds <- tapply(astro1$`Space Flights`, astro1$Military_Status, sd)
ns <- tapply(astro1$`Space Flights`, astro1$Military_Status, length)

se <- sds / sqrt(ns)  # standard error

bar_centers <- barplot(means,
                       ylim = c(0, max(means + se) + 0.5),
                       main = "Average Space Flights by Background",
                       ylab = "Mean Number of Flights",
                       col = c("lightblue", "lightgreen"))

t.test(`Space Flights` ~ Military_Status, data = astro1)


-------- trouble shooting for q1
  table(astro1$Military_Status)

sum(astro1$Military_Status == "Civilian")

tapply(astro1$`Space Flights`, astro1$Military_Status, mean)

summary(astro1$`Space Flights`)
unique(astro1$`Space Flights`)

## question 2: How has age of astronauts changed over time?

names(data)

head(data$`Birth Date`)

install.packages("lubridate")

library(lubridate)

birth_year <- year(ymd(data$`Birth Date`))
data$Age_at_Selection <- data$Year - birth_year

astro2 <- data[, c("Year", "Age_at_Selection")]
astro2 <- na.omit(astro2)

##scatter plot 

plot(astro2$Year, astro2$Age_at_Selection,
     main = "Astronaut Age at Selection Over Time",
     xlab = "Selection Year",
     ylab = "Age at Selection",
     pch = 19)

abline(lm(Age_at_Selection ~ Year, data = astro2), lwd = 2)

## linear regression 

model2 <- lm(Age_at_Selection ~ Year, data = astro2)
summary(model2)

## Question 3: Q3: Has the gender composition of astronaut classes become more balanced? 

astro3 <- astronauts[, c("Year", "Gender")]
astro3 <- na.omit(astro3)

astro3$Gender <- trimws(tolower(as.character(astro3$Gender)))

table(astro3$Gender)

gender_counts <- table(astro3$Year, astro3$Gender)
gender_props <- prop.table(gender_counts, margin = 1)

colnames(gender_props)

years <- as.numeric(rownames(gender_props))

## plot of females vs males over time. 

plot(years, gender_props[, "female"],
     type = "b",
     pch = 19,
     ylim = c(0, 1),
     main = "Proportion of Female Astronauts Over Time",
     xlab = "Selection Year",
     ylab = "Proportion Female")

## regression 
astro3$Female <- ifelse(astro3$Gender == "female", 1, 0)

model3 <- lm(Female ~ Year, data = astro3)
summary(model3)


------ troubleshooting for q3
added to main code 


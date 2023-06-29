library(readxl)
library(ggplot2)
library(dplyr)

# Load data for productivity
productivity <- as.data.frame(t(read_excel("productivityVSemployment/productivity/productivity.xlsx", 
                           sheet = "national-modified", range = "S14:AC14", 
                           col_names = FALSE)))

# Load data for rates
year <- 2008:2018
total_population <- c(57848, 59237, 60718, 61882, 62985, 64093, 62189, 64939, 68125, 69896, 71339)
participation <- c(63.6, 64.0, 64.1, 64.6, 64.2, 63.9, 64.4, 63.7, 63.4, 61.2, 60.9)
employment <- c(92.6, 92.5, 92.7, 93.0, 93.0, 92.7, 93.2, 93.7, 94.5, 94.3, 94.7)
unemployment <- c(7.4, 7.5, 7.3, 7.0, 7.0, 7.3, 6.8, 6.3, 5.5, 5.7, 5.3)
underemployment <- c(19.3, 19.1, 18.7, 19.3, 20.0, 19.8, 18.4, 18.5, 18.3, 16.1, 16.4)

# Combine data frames
# Note: population is by '000s
df <- data.frame(year, total_population, participation, employment, unemployment, underemployment, productivity=productivity$V1)

# Add workforce population and other population
df <- df %>%
        mutate(workforce=total_population*participation/100) %>% 
        mutate(pop_e=workforce*employment/100) %>% 
        mutate(pop_une=workforce*unemployment/100) %>% 
        mutate(pop_undere=workforce*underemployment/100)

# Is there a correlation between the employment status of the workers and the labor productivity?
# NOTE: Based on https://towardsdatascience.com/eveything-you-need-to-know-about-interpreting-correlations-2c485841c0b8

# Linear model for workforce population against labor productivity
test <- cor.test(df$workforce, df$productivity)
ggplot(df, aes(x=workforce, y=productivity)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title=paste("Correlation Coefficient:", test$estimate),
       subtitle=paste("p-value:", test$p.value)) +
  xlab("Workforce Population")

# Linear model for employment population against labor productivity
test <- cor.test(df$pop_e, df$productivity)
ggplot(df, aes(x=pop_e, y=productivity)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title=paste("Correlation Coefficient:", test$estimate),
       subtitle=paste("p-value:", test$p.value)) +
  xlab("Employment Population")

# Linear model for unemployment population against labor productivity
test <- cor.test(df$pop_une, df$productivity)
ggplot(df, aes(x=pop_une, y=productivity)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title=paste("Correlation Coefficient:", test$estimate),
       subtitle=paste("p-value:", test$p.value)) +
  xlab("Unemployment Population")

# Linear model for underemployment population against labor productivity
test <- cor.test(df$pop_undere, df$productivity)
ggplot(df, aes(x=pop_undere, y=productivity)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title=paste("Correlation Coefficient:", test$estimate),
       subtitle=paste("p-value:", test$p.value)) +
  xlab("Underemployment Population")





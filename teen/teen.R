library(readxl)
library(ggplot2)
library(dplyr)

col_names <- c("age_of_father", "freq", "year")

data_2016 <- read_excel("teen/teen_data.xlsx", 
                        sheet = "Table 3", range = "A8:B20", 
                        col_names = FALSE, na = "*")
data_2016$year <- as.Date(rep("2016", nrow(data_2016)), format='%Y')

data_2017 <- read_excel("teen/teen_data.xlsx", 
                        sheet = "Table 3", range = "A24:B36", 
                        col_names = FALSE, na = "*")
data_2017$year <- as.Date(rep("2017", nrow(data_2017)), format='%Y')

data_2018 <- read_excel("teen/teen_data.xlsx", 
                        sheet = "Table 3", range = "A40:B52", 
                        col_names = FALSE, na = "*")
data_2018$year <- as.Date(rep("2018", nrow(data_2018)), format='%Y')

data_2019 <- read_excel("teen/teen_data.xlsx", 
                        sheet = "Table 3", range = "A56:B68", 
                        col_names = FALSE, na = "*")
data_2019$year <- as.Date(rep("2019", nrow(data_2019)), format='%Y')

data_2020 <- read_excel("teen/teen_data.xlsx", 
                        sheet = "Table 3", range = "A72:B84", 
                        col_names = FALSE, na = "*")
data_2020$year <- as.Date(rep("2020", nrow(data_2020)), format='%Y')

colnames(data_2016) <- col_names
colnames(data_2017) <- col_names
colnames(data_2018) <- col_names
colnames(data_2019) <- col_names
colnames(data_2020) <- col_names

teen_data <- rbind(data_2016, data_2017, data_2018, data_2019, data_2020)

small_legend <- theme(legend.key.size = unit(0.25, 'cm'),
                      legend.title = element_text(size=7),
                      legend.text = element_text(size=5))


# Overall
ggplot(data=teen_data,
       mapping=aes(x=year,
                   y=freq,
                   fill=age_of_father)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  small_legend


# By total
ggplot(data=teen_data) +
  geom_bar(mapping=aes(x=year,
                       y=freq,
                       fill=age_of_father),
           stat="identity") +
  geom_smooth(data=group_by(teen_data, year) %>%
                    summarise(total=sum(freq)),
              mapping=aes(x=year,
                          y=total),
              method="lm",
              se=FALSE,
              color="red") +
  theme_bw() +
  small_legend


# By age of father (overall)
ggplot(data=teen_data,
       mapping=aes(x=year,
                   y=freq,
                   fill=age_of_father)) + 
  geom_bar(stat="identity") +
  facet_wrap(~age_of_father) +
  theme_bw() +
  small_legend


# By age of father (specific)
age_of_father_vector <- unique(teen_data$age_of_father)
age_of_father_vector # for quick printing
i <- 3
ggplot(data=filter(teen_data, age_of_father==age_of_father_vector[i]),
       mapping=aes(x=year,y=freq)) +
  geom_bar(stat="identity") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  theme_bw() +
  labs(title=paste("Bar graph with trend of", age_of_father_vector[i], sep=" "))


















#        legend.box.background = element_rect(fill="black"),
#        plot.background = element_rect(fill="black"),
#        panel.background = element_rect(fill="black", color="gray"),
#        panel.grid.major = element_line(color = "navy"),
#        panel.grid.minor = element_line(color = "navy"))



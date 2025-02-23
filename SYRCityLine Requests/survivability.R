library(survival)
library(ggplot2)

data <- read.csv('/Users/rafeburns/Downloads/SYRCityline_Requests_(2021-Present).csv')

data <- data %>%
  filter(Rating %in% c(1,2,3,4,5))

survData <- data %>%
  select(ObjectId, Summary, Rating, Agency_Name, Request_type, Minutes_to_Close, Created_at_local, Assignee_name, Sla_in_hours) %>%
  filter(!is.na(Minutes_to_Close))
survData <- survData %>%
  mutate(internal = ifelse(str_detect(Summary, "(?i)internal"), TRUE, FALSE),
         start = 0, time = Minutes_to_Close/1440, event = 1) %>%
  filter(time != 0,
         time < 14,
         !is.na(Sla_in_hours))
survData$Rating <- as.numeric(survData$Rating)
survData$Sla_in_hours <- as.numeric(survData$Sla_in_hours)

histData <- survData
firstQ <- quantile(histData$time, 0.25)
thirdQ <- quantile(histData$time, 0.95)
histData <- histData %>%
  filter(time < thirdQ, time > firstQ)
ggplot(histData, aes(x = time)) + 
  geom_histogram(bins = 50) + labs(title = "Histogram of Time to Completion", y = "Frequency", x = "Time")

ggplot(histData, aes(x = time)) + 
  geom_histogram(bins = 50) + labs(title = "Histogram of Time to Completion", y = "Frequency", x = "Time") + 
  facet_wrap(~ Agency_Name)

#Agency_Name
avgmodel <- coxph(Surv(start, time, event) ~ Agency_Name, data = survData)
avgSurvFit <- survfit(avgmodel, data = survData)
plot(avgSurvFit, xlab = "Time", ylab = "Probability Issue is Unresilved", main = "Survival Curve - All Agencies")


model <- coxph(Surv(start, time, event) ~ strata(Agency_Name), data = survData)
survFit <- survfit(model, data = survData)
surv_df <- broom::tidy(survFit)
ggplot(surv_df, aes(x = time, y = estimate, color = strata)) +
  geom_smooth(se = FALSE, size = 1, span = 0.05) +
  labs(title = "Time to Response by Agency",
       x = "Time",
       y = "Probability Issue is Unresolved",
       color = "Agency") +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(size = 14))








#Assignee Surv
assigneeData <- survData %>%
  filter(Assignee_name %in% c('Commissioner- Water',
                              'Cityline Operator - KW', 'Sanitation_KT',
                              'Community Police'))
assigneeModel <- coxph(Surv(start, time, event) ~ strata(Assignee_name), data = assigneeData)
assigneeSurvFit <- survfit(assigneeModel, data = assigneeData)
assigneeSurv_df <- broom::tidy(assigneeSurvFit)
ggplot(assigneeSurv_df, aes(x = time, y = estimate, color = strata)) +
  geom_smooth(se = FALSE, size = 1, span = 0.3) +
  labs(title = "Time to Response by Assignee",
       x = "Time",
       y = "Probability Issue is Unresolved",
       color = "Assignee") +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(size = 14))

assigneeDataB <- survData %>%
  filter(Assignee_name %in% c('Acting Assistant Superintendent- Street Cleaning/Snow & Ice',
                              'Transportation:Traffic Signals Foreman'))
assigneeModelB <- coxph(Surv(start, time, event) ~ strata(Assignee_name), data = assigneeDataB)
assigneeSurvFitB <- survfit(assigneeModelB, data = assigneeDataB)
assigneeSurv_dfB <- broom::tidy(assigneeSurvFitB)
ggplot(assigneeSurv_dfB, aes(x = time, y = estimate, color = strata)) +
  geom_smooth(se = FALSE, size = 1, span = 0.3) + 
  labs(title = "Time to Response by Assignee",
       x = "Time",
       y = "Probability Issue is Unresolved",
       color = "Assignee") +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(size = 14))




#install packages 
install.packages("readxl")
install.packages("Hmisc")
install.packages("cowplot")
install.packages("car")
install.packages("stargazer")
install.packages("visreg")
install.packages("rmarkdown")


### PREPARING THE DATA
# Load packages
library(tidyverse)

#Imoport the data
library(readxl)
WVS_org <- read_excel("WVS_sweden.xlsx")

#Selecting the relevant variabels
WVS <- WVS_org %>% 
  select(Life_Satisfaction = `V23: Satisfaction with your life`,
         Financial_Satisfaction = `V59: Satisfaction with financial situation of household`,
         Happiness = `V10: Feeling of happiness`,
         Health = `V11: State of health (subjective)`,
         Marital_status = `V57: Marital status`,
        Children_number = `V58: How many children do you have`,
      Religion = `V147: Religious person`,
        Employment = `V229: Employment status`,
        Social_class = `V238: Social class (subjective)`,
       Gender = `V240: Sex`,
       Age = `V242: Age`,
        Mother_immigrant = `V243: Mother immigrant`,
        Father_immigrants = `V244: Father immigrant`,
      Immigrant_background = `V245: Respondent immigrant`,
       Education = `V248: Highest educational level attained`)


#coding the missing values
WVS <- WVS %>%
  mutate(Life_Satisfaction = if_else(
    Life_Satisfaction %in% c("No answer", "Don´t know"),
    NA_character_,Life_Satisfaction), Financial_Satisfaction = if_else(
      Financial_Satisfaction %in% 
        c("No answer", "Don´t know",  "HT: Dropped out survey;DE,SE:Inapplicable ; RU:Inappropriate response"),
      NA_character_, Financial_Satisfaction), 
    Happiness = na_if(Happiness, "No answer"), 
     Health= if_else(
      Health %in% c("No answer", "Don´t know"), NA_character_,Health),
    Marital_status= if_else(
        Marital_status %in% c("No answer", "Don´t know"), NA_character_,Marital_status),  
    Children_number = na_if(Children_number, "No answer"),
    Religion= if_else(
      Religion %in% c("No answer", "Don´t know", "DE,SE:Inapplicable ; RU:Inappropriate response; HT: Dropped out survey"), 
      NA_character_,Religion), 
    Employment= if_else(
      Employment %in% c("No answer;SG: Refused", "Other"), 
      NA_character_,Employment),
    Social_class= if_else(
      Social_class %in% c("No answer", "Don´t know", "SE:Inapplicable ; RU:Inappropriate response; BH: Missing; HT: Dropped out survey"), 
      NA_character_,Social_class), 
    Mother_immigrant= if_else(
      Mother_immigrant %in% c("No answer", "AM,DE,SE:Inapplicable ; RU:Inappropriate response; BH: Missing"), 
      NA_character_,Mother_immigrant),
    Father_immigrants= if_else(
      Father_immigrants %in% c("No answer", "AM,DE,SE:Inapplicable ; RU:Inappropriate response; BH: Missing"), 
      NA_character_,Father_immigrants),
    Immigrant_background= if_else(
      Immigrant_background %in% c("No answer", "SE:Inapplicable ; RU:Inappropriate response; BH: Missing"), 
      NA_character_,Immigrant_background),
    Education= if_else(
      Education %in% c("No answer", "Don´t know", "AU: Inapplicable (No-school education) DE,SE:Inapplicable ; SG: Refused; ZA:Other; Missing"), 
      NA_character_,Education))

#Checking that everything went well
unique(WVS$Life_Satisfaction)
unique(WVS$Financial_Satisfaction)
unique(WVS$Happiness)
unique(WVS$Health)
unique(WVS$Marital_status)
unique(WVS$Children_number)
unique(WVS$Pray)
unique(WVS$Religion)
unique(WVS$Believing_God)
unique(WVS$Employment)
unique(WVS$Social_class)
unique(WVS$Mother_immigrant)
unique(WVS$Father_immigrants)
unique(WVS$Immigrant_background)
unique(WVS$Education)

#Changing variables types
WVS <- WVS %>% mutate_if(is.character, as.factor) %>% 
  mutate(Life_Satisfaction = as.numeric(Life_Satisfaction),
               Financial_Satisfaction = as.numeric(Financial_Satisfaction))
str(WVS)

# Recoding the variables
WVS <- WVS %>%
  mutate(Children_number = fct_collapse(Children_number,
                                        Have_children = c("2 children", "1 child", "3 children", "4 children", "5 children",        
                                                          "6 children", "7 children", "8 or more children"),
                                        Not_Children = c("Not_Children")), 
         Happiness = fct_collapse(Happiness, 
                                  Happy = c("Quite happy","Very happy"),
                                  Not_happy = c("Not at all happy","Not very happy")), 
         Marital_status = fct_collapse(Marital_status, 
                                       Married = c( "Living together as married","Married"),
                                       Not_Married = c("Single","Widowed","Separated","Divorced")),
         Education = fct_collapse(Education,
                                  "Primary_school" = c("Complete primary school", "Incomplete primary school"),
                                  "Secondary_school" = c("Complete secondary school: technical/ vocational type",
                                                         "Complete secondary school: university-preparatory type",
                                                         "Incomplete secondary school: technical/ vocational type",
                                                         "Incomplete secondary school: university-preparatory type"),
                                  "No_formal_education" = "No formal education",
                                  "University_education" = c("Some university-level education, without degree",
                                                             "University - level education, with degree")),
         Social_class = fct_collapse(Social_class,
                                     "Lower" = c("Lower class", "Working class"),
                                     "Middle" = c("Lower middle class", "Upper middle class"),
                                     "Upper" = "Upper class"),
         Employment = fct_collapse(Employment,
                                   Others = c("Full time", "Housewife","Part time","Retired","Self employed","Students"),
                                   Unemployment = c("Unemployed")),
         Religion = fct_collapse(Religion,
                                 Religious = c("A religious person"),
                                 Not_Religious = c( "An atheist","Not a religious person")),
         Age = fct_collapse(Age,
                            Older_than50 = c("50 and more"),
                            Up_to50 = c("Up to 29", "30-49")),
         Health = fct_collapse(Health, 
                               Good_health = c("Very good",  "Good"),
                               Not_good_Health = c("Poor", "Fair")))

WVS <- WVS %>%
  mutate (Immigrant_Parents = factor (case_when (Father_immigrants == "Immigrant" | Mother_immigrant == "Immigrant" ~ "Has Immigrant Parents",
                                                 TRUE ~ "No Immigrant Parents")))

#Chaging the referens category 
WVS <- WVS %>% 
  mutate(Education = fct_relevel(Education, "No_formal_education"), 
         Gender = fct_relevel(Gender, "Male"), 
         Religion = fct_relevel(Religion,"Not_Religious"),
         Immigrant_Parents = fct_relevel(Immigrant_Parents, "No Immigrant Parents"),
         Immigrant_background = fct_relevel(Immigrant_background, "I am born in this country"),
         Children_number = fct_relevel(Children_number,"No children"))

### DESCRIPTIVE STAT
#Descriptivi Stat for categrorical variables
library(Hmisc)
describe(WVS$Gender)
describe(WVS$Age)
describe(WVS$Religion)
describe (WVS$Health)
describe (WVS$Immigrant_background)
describe (WVS$Immigrant_Parents)
describe (WVS$Education)
describe(WVS$Social_class)
describe(WVS$Marital_status)
describe(WVS$Children_number)
describe(WVS$Happiness)
describe(WVS$Employment)

# summary stat for the data
library(skimr)
skim(WVS)

#summary stat for X
summary(WVS$Financial_Satisfaction)

#summary stat for Y
summary(WVS$Life_Satisfaction)

### VISUALAISE FOCAL VARIABLES
library(cowplot)

# Plot for Life Satisfaction (Focal Y)
plot_life <- ggplot(WVS, aes(x = Life_Satisfaction)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  xlab("Life Satisfaction") + ylab("Frequency") +
  ggtitle("The distribution of Life Satisfaction") +
  theme_classic()

# Plot for Financial Satisfaction (Focal X)
plot_financial <- ggplot(WVS, aes(x = Financial_Satisfaction)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  xlab("Financial Satisfaction") + ylab("Frequency") +
  ggtitle("The distribution of Financial Satisfaction") +
  theme_classic()

# Combine plots using plot_grid() from cowplot
combined_plot <- plot_grid(plot_life, plot_financial, ncol = 1) 

combined_plot


### ANALYSIS 

#Bivariata analysis (Model 1)
mod1 <- lm(Life_Satisfaction ~ Financial_Satisfaction, data = WVS)

summary(mod1)

#multivariata analysis with controll variables (Model 2)

mod2 <- lm(Life_Satisfaction ~ Financial_Satisfaction + Happiness + Health + Marital_status + Children_number +
             Religion + Employment + Social_class + Gender + Age + Immigrant_Parents + Immigrant_background +
             Education, data = WVS)

summary(mod2)                      

#multivariata analysis with with interaction term (Model 3)
mod3 <- lm(Life_Satisfaction ~ Financial_Satisfaction + Happiness + Health + Marital_status + Children_number +
             Religion + Employment + Social_class + Age + Gender + Immigrant_Parents + Immigrant_background +
             Education+ Financial_Satisfaction*Gender, data = WVS)
summary(mod3)


### ASSUMPTION 

#plot the linearity ( Residuals vs Fitted Values)
plot(mod3, wich = 1) #looks good

#plot the normality of the residuals (Normal Q-Q Plot)
plot(mod3, wich = 2) #looks good

# plot the resiudals and homoscedastic (Scale-Location Plot)
plot(mod3, which = 3) #looks good

# plot Cook's Distance (Checks for influential observations) (Residuals vs leverage)
plot(mod3, wich = 4) #Looks good

# multicollinearity
library(car)
vif(mod3, type="predictor")


#VISUALIZE REGRESSION MODELS#

#Multivariata analysis with controll variables (Model 2)
library(visreg)
visreg(mod2, "Financial_Satisfaction", 
       ylab= "Life_Satisfaction",      
         xlab= "Financial_Satisfaction",
         gg = TRUE, 
       band = TRUE) + 
  theme_classic() +
  ggtitle("Figure 2: Multiple linear regression model") +
  labs(
    x = "Financial Satisfaction",
    y = "Life Satisfaction"
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10))

# Visualize Multivariata analysis with interaction term (Model 3)
visreg(mod3, "Financial_Satisfaction", by = "Gender", 
       overlay = TRUE,
       ylab = "Life_Satisfaction", 
       xlab = "Financial_Satisfaction",
       legend = FALSE,
       gg = TRUE, 
       band = FALSE) + 
  theme_classic() +
  ggtitle("Figure 3: Gender-Based Multiple linear regression") +
  labs(
    x = "Financial Satisfaction",
    y = "Life Satisfaction"
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10 ),
    axis.title = element_text(size = 10))

  
#TABLES  
library(modelsummary)

my_summary <- msummary(
  list("Model 1"=mod1,"Model 2"= mod2,"Model 3"= mod3), 
  stars = TRUE, 
  fmt = "%.2f", 
  coef_map = c(
    '(Intercept)' = '(Intercept)',
    'Financial_Satisfaction' = 'Financial Satisfaction',
    'GenderFemale' = 'Gender: Female',
    'AgeOlder_than50' = 'Age: older than 50 years',
    'EducationPrimary_school' = 'Education: Primary school',
    'EducationSecondary_school' = 'Secondary school',
    'EducationUniversity_education' = 'University education',
    'Social_classMiddle' = 'Social class: Middle class',
    'Social_classUpper' = 'Social class: Upper class',
    'Marital_statusMarried' = 'Marital status: Married',
    'Children_numberHave_children' = 'Children: Have children',
    'HappinessHappy' = 'Happiness: Happy',
    'HealthGood_health' = 'Health: Good health',
    'EmploymentUnemployment' = 'Unemployment',
    'ReligionReligious' = 'Religion: Religious',
    'Immigrant_backgroundI am an immigrant to this country' = 'First generation immigrant',
    'Immigrant_ParentsHas Immigrant Parents' = 'Second generation immigrant',
    'Financial_Satisfaction:GenderFemale' = 'Interaction: Financial Satisfaction*Gender'
  ), 
  title = 'Table 1. Linear regression models', 
  notes = ('Ref: Male, Younger than 50 years, No education, Lower class, Not married, No children, Not happy, Poor health, Not religious, Not immigranys'),
  statistic = NULL,
  gof_omit = "AIC|BIC|Lik")


summary_df <- as.data.frame(my_summary)

# Save as a CSV file
write.csv(summary_df, file = "summary_table.csv", row.names = FALSE)






#----------------PART 1: INSTALLING PACKAGE & CALLING BY LIBRARY()--------------
library(readxl)
library(dplyr)       # data wrangling
library(ggplot2)
library(plotly)
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(lattice)
library(caret) 
library(splitstackshape)
library(e1071)
library(ROCR)

#----------------PART 2: LOADING DATASET---------------------------------------
air_france <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls",
                         sheet = "DoubleClick")


#----------------PART 3: MASSAGING & ANALYSIS DATA------------------------------------------

#MASSAGE DATA
#Modified missing value in Bid Strategy variable
air_france$`Bid Strategy`[is.na(air_france$`Bid Strategy`)] = "No Strategy"

#Checking the NA of air_france 
sum(is.na(air_france)) #we have 0 missing values now

#Exclude 0 from Total cost
air_france <- air_france[which(air_france$`Total Cost` !=0), ] 

#ANALYZE DATA
#1. EXTRA COLUMNS 
#Create the binary based on Total volume of bookings
air_france$flag_booking <- ifelse(air_france$`Total Volume of Bookings` > 0, 1,0)

#Net Revenue
air_france$net_revenue <- air_france$Amount - air_france$`Total Cost`

#ROA
air_france$ROA <- air_france$net_revenue/air_france$`Total Cost`

#Avg Revenue/bookings
air_france$`avg_revenue/booking` <- air_france$Amount/air_france$`Total Volume of Bookings`

#Probability of bookings
air_france$probs_booking <- (air_france$`Engine Click Thru %`*air_france$`Trans. Conv. %`)/10000

#Cost/booking
air_france$`cost/booking` <- air_france$`Total Cost`/air_france$`Total Volume of Bookings`

#2. PIVOT TABLE
#Group_by Publisher Name

#2.1. Pivot table of average 
publisher_pivot <- group_by(air_france, `Publisher Name`)
publisher_pivot_1 <- summarise(publisher_pivot, 
                               avg_ROA = mean(ROA),
                              `avg_cost/click` = mean(`Avg. Cost per Click`),
                              `avg_net_revenue` = mean(net_revenue))

View(publisher_pivot_1)

#2.2.Pivot table of sum
publisher_pivot_2 <- summarise(publisher_pivot, 
                             `sum total bookings` = sum(`Total Volume of Bookings`),
                             `sum net revenue` = sum(net_revenue),
                             `sum click charge`= sum(`Click Charges`),
                             `sum ROA` = sum(ROA),
                             `sum probs of booking`= sum(probs_booking))

View(publisher_pivot_2)


#2.3. Pivot table 3
Campaign_pivot <- group_by(air_france, Campaign, `Publisher Name`)

Campaign_pivot <- summarise(Campaign_pivot, 
                             Revenue = sum(net_revenue),
                             Count_Match_Type = n(),
                             Cost = sum(`Total Cost`),
                             Total_ROA = sum(ROA))
View(Campaign_pivot)          
                

#----------------PART 4: PLOTS AND CHARTS---------------------------------------
#I. GGLOT (all charts)
#Click Charges
team_click_charges <- ggplot() + 
                      geom_histogram(data=as.data.frame(air_france),
                                     aes(x= `Click Charges`),
                                     bins =50, fill="blue", 
                                     color="white", na.rm = TRUE) +
                      xlim(200,10000) + ylim(0,50)+
                      labs(title= "Click Charges histogram", x= "Click Charges")+
                      theme_light()
#Clicks
team_clicks <- ggplot(data=air_france) + 
                geom_histogram(aes(x=Clicks),bins =50, fill="dark green", color="white", na.rm = TRUE) +
                xlim(100,5000) + ylim(0,30) +
                labs(title= "Clicks histogram", x= "Clicks")+
                theme_light()

#Box plot Match Type vs Publisher Name
match_type <- subset(x=air_france, subset = ((air_france$ROA >0)&(air_france$`Match Type` != 'N/A')))
ggplot(match_type, aes(`Match Type`, ROA), color="blue") + 
  geom_boxplot() + 
  ylim(0,3000)+
  labs(title= "Match Type boxplot", x= "Match Types")+
  theme_minimal()+
  stat_boxplot(na.rm = TRUE)

#ROA
team_ROA <- ggplot(data=air_france, aes(ROA)) + 
            geom_histogram(fill="blue", color="white", na.rm = TRUE, bins = 30) + 
            xlim(0,500) + ylim(0,70)

#Campaign and Publisher Bar Chart
barchartData <- Campaign_pivot[which(Campaign_pivot$Total_ROA>7),]
barchartData <- barchartData[-which(barchartData$Campaign=='Unassigned'),]


team_campaign_PN <- ggplot(barchartData, aes(x=Campaign, y=Total_ROA, fill=`Publisher Name`)) +   
                    geom_bar(position = "Dodge", stat="identity", size=10) +
                    coord_flip()

#5. Scatter plot Average cost per click and ROA by Publisher Name

ScatterData<- subset(x=air_france, subset = (between(air_france$ROA,1,200)))

team_scatter <- ggplot(ScatterData, aes(y=ROA, x=`Avg. Cost per Click`, color= `Publisher Name`)) + geom_point()

#6. Probability scatter plot
Scatter_probs<- subset(x=air_france, subset = (between(air_france$probs_booking,-0.02,0.5)))

team_scatter_2 <- ggplot(Scatter_probs, aes(y=`Avg. Cost per Click`, x=`probs_booking`, color= `Publisher Name`)) + 
                  geom_point(alpha = 0.5, na.rm = TRUE) +
                  scale_size(range=c(0.5,12), name="AirFrance")


#7. Clicks vs total volume of bookings
team_scatter_3 <- ggplot(air_france, aes(y=`Total Volume of Bookings`, x=`Clicks`, color= `Publisher Name`)) + geom_point()

#II. PLOTLY
#convert ggplot to plotly by ggplotly()
ggplotly(team_click_charges)
ggplotly(team_clicks,  originalData = TRUE)
ggplotly(team_ROA)
ggplotly(team_campaign_PN)
ggplotly(team_scatter)
ggplotly(team_scatter_2)
ggplotly(team_scatter_3)

#-----------------PART 5: LOGISTIC REGRESSION-----------------------------------

##NORMALIZE ALL COLUMNS IN AIRFRACE
#Function of normalized data
my_normal <- function(x){
  my_min <- min(x, na.rm=TRUE)
  my_max <- max(x, na.rm=TRUE)
  normalized <- (x-my_min)/(my_max-my_min)
  return(normalized)
}

#Normalize columns in AirFrance
air_france$Clicks_norm <- my_normal(x=air_france$Clicks)
air_france$Click_Charges_norm<- my_normal(x=air_france$`Click Charges`)
air_france$Avg_cost_click_norm <- my_normal(x=air_france$`Avg. Cost per Click`)
air_france$Impressions_norm <- my_normal(x=air_france$Impressions)
air_france$Engine_click_thru_norm<- my_normal(x=air_france$`Engine Click Thru %`)
air_france$Avg_pos_norm <- my_normal(x=air_france$`Avg. Pos.`)
air_france$Trans_conv_norm<- my_normal(x=air_france$`Trans. Conv. %`)
air_france$Total_cost_trans_norm<- my_normal(x=air_france$`Total Cost/ Trans.`)
air_france$Amount_norm<- my_normal(x=air_france$Amount)
air_france$Total_cost_norm<- my_normal(x=air_france$`Total Cost`)
air_france$Total_booking_norm<- my_normal(x=air_france$`Total Volume of Bookings`)
air_france$Net_revenue_norm <- my_normal(x=air_france$net_revenue)
air_france$ROA_norm<- my_normal(x=air_france$ROA)
air_france$Avg_revenue_booking_norm<- my_normal(x=air_france$`avg_revenue/booking`)
air_france$Probs_booking_norm<- my_normal(x=air_france$probs_booking)
air_france$Cost_booking_norm<- my_normal(x=air_france$`cost/booking`)

#STRATIFIED dataset (80-20)
training_testing <- stratified(as.data.frame(air_france),
                               group=24, 
                               size=0.8, #size training (80%)
                               bothSets = TRUE)

training_stratified <- training_testing$SAMP1
testing_stratified <- training_testing$SAMP2

#I. LOGISTIC REGRESSION
#A.ORIGINAL DATA
#Flag_booking binary vs Click charge, Avg Cost/click, impression
team_logit <- glm(flag_booking~ `Clicks` + `Avg. Cost per Click` + `Impressions` , 
              data=training_stratified,
              family = "binomial")

summary(team_logit)

#Coefficient of the model
my_coeff <- team_logit$coefficients

#Odd ratio
insight <- exp(my_coeff)-1
insight

#B. RESCALED DATA
#which factor have the most impact on business success (Answer: Clicks)
#Logistic regression for RESCALE DATA
team_logit_norm <- glm(flag_booking ~ Clicks_norm + Avg_cost_click_norm + Impressions_norm,
                          data= training_stratified,
                          family ="binomial")

summary(team_logit_norm)

#II. CONFUSION MATRIX FOR ACTUAL DATA
#A. Training stratified data
team_prediction <- predict(team_logit, training_stratified, type="response")
confusionMatrix(data=as.factor(as.numeric(team_prediction>0.5)), 
                reference = as.factor(as.numeric(training_stratified$flag_booking)))


#B. Testing stratified data
team_prediction_testing <- predict(team_logit, testing_stratified, type="response")

confusionMatrix(data=as.factor(as.numeric(team_prediction_testing>0.5)), 
                reference = as.factor(as.numeric(testing_stratified$flag_booking)))

#III. ROC 
#For logistic regression
#Predict the testing_strat vs logistic regression
team_prediction_strat <- predict(team_logit, testing_stratified, type="response")

#Using prediction between predict above vs testing_strat for flag_booking column
pred_team_logit <- prediction(team_prediction_strat, testing_stratified$flag_booking)

#performance ROC of logistic regression
perf_team_logit <- performance(pred_team_logit, "tpr", "fpr")

#plot the ROC
plot(perf_team_logit, col="blue", lty=3, lwd=3)

#------------------------PART 6: DECISION TREE----------------------------------
#create the tree decision
team_tree <- rpart(flag_booking~ `Clicks` + `Avg. Cost per Click` + `Impressions`,
                    data=training_stratified,
                    method="class", cp=0.012)

#plot the tree
rpart.plot(team_tree, type=1, extra=1,
           box.palette = c("pink", "green"),
           branch.lty = 3,
           shadow.col = "gray")

plotcp(team_tree)

#Predict the tree vs testing stratified
team_tree_predict <- predict(team_tree, testing_stratified, type="prob")

#the predict for the tree will give 2 column(p(0) and p(1))- choosing the 2nd column p(1)
team_tree_prediction <- prediction(team_tree_predict[,2],
                                   testing_stratified$flag_booking)

#do the ROC performance for the tree
team_tree_performance <- performance(team_tree_prediction, "tpr", "fpr")


plot(team_tree_performance, col="black",lty=3, lwd=3)
plot(perf_team_logit, col="blue", lty=3, lwd=3, add=T)








  
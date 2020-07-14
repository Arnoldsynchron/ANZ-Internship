#1 ----
#Salary payment frequency of customers 
ANZ_cust_freq = data.frame(customer_id = unique(Csmp$customer_id))

# A mode function to determine salary payment frequency
Mode <- function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x,ux)))]
}

#Loop through all salary payment for each customer

for (i in seq(nrow(ANZ_cust_freq))){
    trans_data <- ANZdata[ANZdata$customer_id == ANZ_cust_freq$customer_id[i] &
                              ANZdata$txn_description == 'PAY/SALARY', c('amount', 'date')]
    %>% group_by(date) %>% summarise (amount = sum(amount))
    
    total_s <-  sum(trans_data$amount)
    count = dim(trans_data)[1]
    
    if ( count == 0){
        ANZ_cust_freq$freq[i] = NA
        ANZ_cust_freq$level[i] = NA
    } else {
        s = c()
        lvl = c()
        for (j in seq(count-1)) {
            s = c(s, (trans_data$date[j+1]-trans_data$date[j]))
            lvl = c(lvl, trans_data$amount[j])
        }
        lvl = c(lvl, tail(trans_data$amount, n =1))
        ANZ_cust_freq$freq[i] = Mode(s)
        ANZ_cust_freq$level[i] = Mode(lvl)
    }
}


ANZ_cust_freq$annual_salary = ANZ_cust_freq$level/ANZ_cust_freq$freq * 365.25

#Visualize distribution of customers' annual salary
hist(ANZ_cust_freq$annual_salary[!is.na(ANZ_cust_freq$annual_salary)], breaks = c(seq(28000, 140000, by=10000)),
     main = "Histograms of customers' annual salary", xlab = 'Income($)')

#2 ----
#Create a dataframe to store relevant features for customers
cus <- Csmp %>% select (customer_id, gender, age, amount, date, balance) %>%
    group_by(customer_id) %>% mutate(avg_no_weekly_trans = round(7*n()/length(unique(ANZ$date)),0), max_amt =max(amount),
                                     no_large_trans = sum(amount>100),
                                     use_no_day = length(unique(date)),
                                     avg_trans_amt = mean(amount, na.rm = TRUE),
                                     med_bal = median(balance, na.rm = TRUE)) %>%
    select(-c('amount','date','balance')) %>% unique()

# create additional features
cus$age_below20 <- ifelse(cus$age<=20,1,0)
cus$age_btw20n40 <- ifelse(cus$age>=20 & cus$age<40, 1,0)
cus$age_btw40n60 <- ifelse(cus$age>= 40 $ cus$age<60,1,0)

# return state where customers live where most transactions occur
cus_region <- Csmp %>% group_by(customer_id,merchant_state) %>% summarize(trans_count=n())%>%
    group_by(customer_id) %>% mutate(no_state = n()) %>% filter(trans_count == max(trans_count))

#For equal number of transactions between multiple states, pick most_likely state
n_occur = data.frame(table(cus_region$customer_id))
cus_id_rep = n_occur$Var1[n_occur$Freq>1]

state_by_cust_no <- rev(names(sort(table(cus_region$merchant_state), rev = TRUE)))
t = data.frame(customer_id = cus_id_rep, merchant_state = NA)

for (i in seq(length(cus_id_rep))){
    s = cus_region$merchant_state[cus_region$customer_id == cus_id_rep[i]]
    for (state in state_by_cust_no){
        if (state %in% s){
            t[i,2] = state
            break
        }
    }
}

cus_region <-  cus_region[!(cus_region$customer_id %in% cus_id_rep), c(1,2)] %>% as.data.frame() %>%
    rbind(t) %>%
    rename(State = merchant_state)

#merge all the features into single dataframe
cus <- cus %>% merge (ANZ_cust_freq) %>% merge(cus_region)

#Extracting relevant features
cus_attr <- cus %>% select('gender', 'annual_salary', 'age', 'avg_no_weekly_trans', 'max_amt', 'no_large_trans',
                           'use_no_day','avg_trans_amt', 'med_bal', 'State')

plot(cus_attr)

#Regression Predictive Modelling-----

first_fit = glm(annual_salary~.-customer_id-level-freq, data = cus)
summary(first_fit)
MASS::stepAIC(first_fit)
BIC(first_fit)

#Improving the model by backward selection using An Information Correlation 
second_fit = glm(annual_salary~age+avg_trans_amt + med_bal + age_below20 +age_btw20n40 +age_btw40n60, data = cus)
summary(second_fit)

#Plot of the coefficients
library(coefplot)
coefplot::coefplot(second_fit)

#Plot residuals to see if any missed relationship exists
plot(second_fit$residuals)

#Model accuracy
rmse(second_fit,cus)

#Decision Tree Modelling ----
library(rpart)
sample_size <- floor(0.75*nrow(cus))

set.seed(123)
train_ind <- sample(seq_len(nrow(cus)), size = sample_size)

cus_train <- cus[train_ind, ]
cus_test <- cus[-train_ind, ]

decision_fit <- rpart(annual_salary~gender+age+avg_no_weekly_trans +max_amt +no_large_trans+
                          use_no_day +avg_trans_amt + med_bal +age_below20+age_btw20n40+age_btw40n60+
                          State, method = 'anova', data = cus_train)
rpart.plot(decision_fit, extra = 6) #plot tree

#or -
plot(decision_fit, uniform = TRUE, main = 'Regression Tree for Annual Salary')
text(fit3, use.n = TRUE, all = TRUE, cex = 0.8)


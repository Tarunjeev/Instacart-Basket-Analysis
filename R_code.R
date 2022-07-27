# INSTACART MARKET BASED ANLYSIS

# This problem can be solved using three different approaches

# 1. Standard Market Based Analysis

# 2. Using a Predictive model to estimate the demand for a particular product
 
# 3. Product recommendation engine using collaborative filtering

library(ggplot2)
library(dplyr)
library(readr)
library(arules)
library(Hmisc)
library(arulesViz)
library(stringr)
library(data.table)
library(methods)
library(RColorBrewer)

# lets say we have some transactions
#1. Dry Fruits
#2. Coffee
#1. soups
#2. milk
#3. vegetables

# If a customer buys potatoes and onions from a retail outlet, how likely will he 
# buy chicken from the retail outlet, that is one rule
# rules can be what the customer will buy next that will be the function of what he has already bought
# so these are called assosiation rules or arules

# Transaction Set

#Invoice 1 = {apple, oranges, rice, wheat, milk}
#Invoice 2 = {rice, milk, butter, bread, fruits, apple}
.
.
#Invoice 20 = {....}

# Total transaction is 20, that means 20 invoices

# ARULES
# Support = proportion of transactions in the invoice data which contains an item set
# X = {milk, rice}
# Support of X = 2/20        ( combination of milk and bread present in invoices)

# Confidence = Confidence of a rule
# Rules of buying apple(Y) fgiven that the user has already added X
# Confidence of (X > Y) = Sup(X U Y) / Sup(X)
# X = LHS and Y = RHS
# Confidence = 2/2 = 100% that is Sup( X UNION Y) = 2/20, Sup(X) = 2/20

# Lift = lift of a specific rule
# Lift ( X > Y) = Sup(X U Y) / Sup(X) * Sup(Y)
# Lift = 2/(2*2) = 2/4 = 0.5

# So these are 3 rules in association rules

# Steps in creating Association rules
# Apply the minimum support criteria to identify most frequent item set
# these frequent item sets and the minimum confidence constraints are used to form rules
# these frequesnt item sets and the minimum confidence constraint are used to form rules
# So we will use two algortihms in creating rules
# eclat algorithm
# apriori algorithm - Let say we have products A, B, D, E, K, J, P, and lets call
# it a set. If a set of products are most frequent in a dataset, then the constituents
# of the most frequent set are also called most frequent.
# Now that is a property of apriori algorithm


orders <- fread("orders.csv")
orders
# The variable orders_dow in orders, the dow stands for date of week
dim(orders)
names(orders)
summary(orders)

products <- fread("products.csv")
products
names(products)

order_products_prior <- fread("order_products_prior.csv")
names(order_products_prior)

order_products <- fread("order_products_train.csv")
names(order_products_train)

aisles <- fread("aisles.csv")
names(aisles)

departments <- fread("departments.csv")
names(departments)

#Now which day of the week the company received most orders
# So in order to find that let's compute a new column in orders
orders$day_week_name <- 
  ifelse(orders$order_dow == 0,
         'Sunday',
         ifelse(orders$order_dow == 1,
                'Monday',
                ifelse(orders$order_dow == 2,
                       'Tuesday',
                       ifelse(orders$order_dow == 3,
                              'Wednesday',
                              ifelse(orders$order_dow == 4,
                                     'Thursday',
                                     ifelse(orders$order_dow == 5,
                                            'Friday',
                                            ifelse(orders$order_dow == 6,
                                                   'Saturday',"")))))))
orders$day_week_name
glimpse(orders)
# Since day_week_name is an character variable in order, so in order to avoid problems
# let's convert it into factor
orders$day_ordered <- factor(orders$day_week_name, levels = c("Sunday",
                                                              "Monday", "Tuesday","Wednesday",
                                                              "Thursday", "Friday","Saturday"))
# Lets perform visualization of orders placed by different days of the week
dow_graph <- barplot(
  table(orders$day_ordered),
  main = "Total Orders by Day",
  xlab = "Days",
  ylab = "Number of Orders",
  col = "blue"
)
# Lets change the graph a little bit by adding text on top of each observations for days
text(
  x = dow_graph,
  y = table(orders$day_ordered),
  labels = table(orders$day_ordered),
  pos = 1,
  cex = 1.0,
  col = "white"
)
# Now let's find which department is mostly purchased by different days of the week
# As we see department_id is the present in both tables
# Therefore, we join the products dataset with the departments dataset
products_dept <-
  merge(products,departments, by='department_id')
sku_orders <- left_join(
  right_join(orders,order_products_train[,c('order_id','product_id')],
             by='order_id'),
  products_dept,
  by = 'product_id'
)
View(sku_orders)
# so now we get department purchases by day of week
dept_purchase_dow <- 
  aggregate(product_id~department + order_dow, sku_orders, FUN = length)
names(dept_purchase_dow) <- c('department','order_dow','num_of_orders')
# dept_purchase dow will give us all the information about when the order was placed
# and how many order were placed in a week by each department

library(knitr)
#Now the function kable() is a very simple table generator and is simple by design
kable(head(orders,12))

kable(head(order_products,10))

kable(head(products,10))

kable(head(order_products_prior,10))

kable(head(aisles,10))

kable(head(departments,10))

# Recoding the variables
# Converting the variables with character datatypes into factor and into numeric

orders <- orders %>% 
  mutate(order_hour_of_day = as.numeric(order_hour_of_day),
         eval_set = as.factor(eval_set))

products <- products %>% 
  mutate(product_name = as.factor(product_name))

aisles <- aisles %>%
  mutate(aisle = as.factor(aisle))

departments <- departments %>%
  mutate(department = as.factor(department))

# GRAPHS
# 1). Visualization for orders by hour of the day
orders %>%
  ggplot(aes(x=order_hour_of_day)) +
  geom_histogram(stat = 'count', fill='blue') +
  ggtitle("Visualization for orders by hour of the day")

# Orders by the day of the week

#Hypothesis
#Is there any effect of day of the week on orders

orders %>%
  ggplot(aes(x=order_dow)) +
  geom_histogram(stat = 'count',fill='red') +
  ggtitle("Visualizationn for orders by day of week")
  
#Coclusion: Most orders are placed on Sunday==0, and Monday==1

#Hypothesis
# Do people order more often after exactly 1 week?

orders %>%
  ggplot(aes(x=days_since_prior_order)) +
  geom_histogram(stat = 'count', fill='orange')

#Conclusion : yes they do order more often after exactly 1 week

#Hypothesis: How many prior orders placed

orders %>% filter(eval_set=='prior') %>% count_(orders,'order_number') %>%
  ggplot(aes(order_number,n)) + geom_line(color='red', size=1)

# Now lets from the training set
# Lets find number of items ordered
order_products %>%
  group_by(order_id) %>%
  summarise(n_items=last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items)) + 
  geom_histogram(stat = 'count', fill = 'red') +
  geom_rug() +
  coord_cartesian(xlim = c(0,80))

# Now lets use the same thing for prior
order_products _ prior %>%
  group_by(order_id) %>%
  summarise(n_items=last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items)) + 
  geom_histogram(stat = 'count', fill = 'red') +
  geom_rug() +
  coord_cartesian(xlim = c(0,80))

# Association rules Mining
order_products_prior[1:10,]
products[1:10,]

mydata <- order_products_prior[,1:2]
mydata <- merge(mydata,products,by='product_id')

mydata <- arrange(mydata,order_id)
head(mydata)

# now on only considering column 2 and 3
mydata <- mydata[,c(2,3)]
head(mydata)

# dataset is disjoint
# for arules or market basket analysos we would need transactional dataset
# how to convert the available information to a transactional dataset

dt <- split(mydata$product_name, mydata$order_id)
# Converting dt dataset into an transactional dataset
dt2 = as(dt,'transactions')
summary(dt2)
# By checking out the summary we come across that consumers have bought some frequent items
# like Bananas, Strawberries, etc. and alse we see the people have bought 1 item = 239 transactions
# 2 items= 305 transactions, upto 51 items = 2 transactions
# summary is of number of transactions

inspect(dt2)[[5]]

# Now lets visualize the most frequent item sets in this dataset
itemFrequency(dt2,type='relative')
itemFrequencyPlot(dt2,topN=10,type='absolute')

#Create rules
rule_1 = apriori(dt2,parameter = list(support=0.00001,
                                      confidence = 0.90))
plot(rule_1,control = list(col=brewer.pal(11,"Spectral")),main="")
# Note iyou take a very small value of support, we are taking the rarest products
# That means the lower the support percentage the more the rules


rule_2 = apriori(dt2,parameter = list(support=0.0001,
                                      confidence = 0.90))
plot(rule_2,control = list(col=brewer.pal(11,"Spectral")),main="")

rule_3 = apriori(dt2,parameter = list(support=0.001,
                                      confidence = 0.90))
plot(rule_3,control = list(col=brewer.pal(11,"Spectral")),main="")
# rule_3 supports in total 25 set of rules
# Reducing the support level gives us more rules
# plot show the relation between support, and confidence in one chart

rule_4 = apriori(dt2,parameter = list(support=0.001,
                                      confidence = 0.80, minlen=3))
# So with rule_4 we have 40 different rules as we have reduced the confidence
# that is why we have more rules as compared to rule_3
plot(rule_4,control = list(col=brewer.pal(11,"Spectral")),main="")

rule_5 = apriori(dt2,parameter = list(support=0.001,
                                      confidence = 0.80, maxlen=4 ))
# With rule 5 we get 57 rules in total 
plot(rule_5,control = list(col=brewer.pal(11,"Spectral")),main="")

inspect(rule_3)
summary(rule_3)
# The summary tells us that for 2 products in LHS we have 4 rules,
# for 3 products in LHS we have 20 rules and then for 4 products in Lhs we have 1 rule

# Converting the rules into a Data Frame
rules_3 = as(rule_3, 'data.frame')
# lets inspect data frame, if we need to find products with banana atleast in rhs
inspect(subset(rule_3,subset=rhs %pin% 'Banana'))

# Before recommending the products to the company you can sort the rules
inspect(head(sort(rule_3, by='lift'),5))
# If we generate smaller set of rules its easy to implement
# So the sorting can also be done on lift values, support values and confidence values

plot(rule_3,method = 'graph',control = list(type='items',main=''))
# Since rule 3 is messy let take top 10 rules from the set of rule_3
subrule3 <- head(sort(rule_3,by='lift'),10)
plot(subrule3,method = 'graph',control = list(type='items',main=''))


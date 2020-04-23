library(mgcv)

germany_train <- train %>% filter(country == "Germany" & days_since_first_case > 0 )
germany_test <- test %>% filter(country == "Germany" & days_since_first_case > 0 )


#
gam1 <- gam(confirmed ~ s(days_since_first_case, k = 50),data = germany, method = "REML")

germany_test$pred <- predict(gam1,germany_test)


ggplot(germany_test,aes(x=date, y =confirmed)) + 
  geom_line()+
  geom_line(aes(x=date, y =pred, color ="red"))

###################################################
train$country_f <- as.factor(train$country)
test$country_f <- as.factor(test$country)

gam2 <- gam(confirmed ~ s(days_since_first_case, by = country_f) + country_f + s(SP.POP.TOTL),
            data = train %>% filter(region =="Europe & Central Asia"), method = "REML")

test$pred <- predict(gam2,test%>% filter(region =="Europe & Central Asia")

test %>% filter(country =="Germany") %>%
ggplot(aes(x=date, y =confirmed)) + 
  geom_line()+
  geom_line(aes(x=date, y =pred, color ="red"))

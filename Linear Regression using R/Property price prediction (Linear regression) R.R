### PROPERTY PRICE PREDICTION - LINEAR REGRESSION ###
my_data = read.csv('C:/download/R Datasets/Property_Price_Train.csv',
                stringsAsFactors = F)
head(my_data)
str(my_data)
dim(my_data)
summary(my_data)
NA_values = data.frame(na_no = colSums(is.na(my_data)))
NA_values #generate missing values for each col

#generate correlations
nums <- unlist(lapply(data, is.numeric))
num_data <- my_data[,nums]
str(num_data)
dim(num_data)

library(ggplot2)

cor_mat = cor(num_data, method = 'pearson', use='complete.obs')
ggplot(data=my_data, aes(x=Lot_Size, y=Sale_Price)) +
  geom_jitter() + geom_smooth(method='lm', se=F)+
  labs(title = 'Scatter lot of Lot_Size and price')

names(my_data)
names(num_data)
(cor_mat['Sale_Price'])
Overall_Material
Total_Basement_Area
First_Floor_Area
Grade_Living_Area

par(mfrow=c(1,4))
with(my_data, {
  xy<-ggplot(data=my_data, aes(x=Overall_Material, y=Sale_Price)) +
    geom_jitter() + geom_smooth(method='lm', se=F)+
    labs(title = 'Scatter lot of Overall_Material and price')
  #plot(Overall_Material, Sale_Price, main='Overall_Material and price')
  #lm1<-lm(Overall_Material~Sale_Price, data=my_data)
  #abline(lm1,lwd=4)
  ggplot(data=my_data, aes(x=Total_Basement_Area, y=Sale_Price)) +
    geom_jitter() + geom_smooth(method='lm', se=F)+
    labs(title = 'Scatter lot of Total_Basement_Area and price')
  #plot(Total_Basement_Area, Sale_Price, main='Total_Basement_Area and price')
  #lm2<-lm(Total_Basement_Area~Sale_Price, data=my_data)
  #abline(lm2,lwd=4)
  ggplot(data=my_data, aes(x=First_Floor_Area, y=Sale_Price)) +
    geom_jitter() + geom_smooth(method='lm', se=F)+
    labs(title = 'Scatter lot of First_Floor_Area and price')
  #plot(First_Floor_Area, Sale_Price, main='First_Floor_Area and price')
  #lm3<-lm(First_Floor_Area~Sale_Price, data=my_data)
  #abline(lm3,lwd=4)
  ggplot(data=my_data, aes(x=Grade_Living_Area, y=Sale_Price)) +
    geom_jitter() + geom_smooth(method='lm', se=F)+
    labs(title = 'Scatter lot of Grade_Living_Area and price')
  #plot(Grade_Living_Area, Sale_Price, main='Grade_Living_Area and price')
  #lm4<-lm(Grade_Living_Area~Sale_Price, data=my_data)
  #abline(lm4,lwd=4)
  
})

install.packages('GGally')
library(GGally)
ggpairs(my_data, columns = c('Overall_Material',
                             'Total_Basement_Area',
                             'First_Floor_Area',
                             'Grade_Living_Area',
                             'Full_Bathroom_Above_Grade',
                             'Rooms_Above_Grade',
                             'Garage_Size'))

my_data2 = my_data[,c('Overall_Material',
                      'Total_Basement_Area',
                      'First_Floor_Area',
                      'Grade_Living_Area',
                      'Full_Bathroom_Above_Grade',
                      'Rooms_Above_Grade',
                      'Garage_Size',
                      'Sale_Price')]

str(my_data2)

NA_values = data.frame(na_no = colSums(is.na(my_data2)))
NA_values #generate missing values for each col

model = lm(data=my_data2,
           Sale_Price ~ Overall_Material +
             Total_Basement_Area +
             First_Floor_Area +
             Grade_Living_Area +
             Full_Bathroom_Above_Grade +
             Rooms_Above_Grade +
             Garage_Size )
summary(model)

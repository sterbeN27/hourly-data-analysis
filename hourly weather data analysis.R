#Author: STEVEN

#load packages and import data
library("ggplot2")
library("dplyr")
full_data = read.csv("hourly weather data.csv")

#pre-processing
#-add season column based on day and month
full_data=mutate(full_data, season = 
            if_else((month==3&day>=20)|month==4|month==5|(month==6&day<=19),1,
            if_else((month==6&day>=20)|month==7|month==8|(month==9&day<=21),2,
            if_else((month==9&day>=22)|month==10|month==11|(month==12&day<=20),3,4))))


#Analysis
#1: Finding the best season for holiday, based on the temperature
#-manipulation
full_data %>% group_by(season) %>% summarise(avg_temp=mean(temp),
                                             max_temp=max(temp),
                                             min_temp=min(temp), .groups='drop')
#-visualization
ggplot(full_data, aes(season, temp, fill=season)) + geom_boxplot(aes(group=season))


#2: Analyzing average humidity at both airports for each month
#-manipulation
jfk_humid=full_data %>% filter(origin=="JFK") %>% group_by(month) %>% #JFK
  summarise(humid=mean(humid),origin, .groups='drop')
lga_humid=full_data %>% filter(origin=="LGA") %>% group_by(month) %>% #LGA
  summarise(humid=mean(humid),origin, .groups='drop')

#-visualization
ggplot() + 
  geom_line(data = jfk_humid, aes(x = month, y = humid, color=origin)) +
  geom_line(data = lga_humid, aes(x = month, y = humid, color=origin)) + 
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun", #change month format into character
                                          "jul","aug","sep","oct","nov","dec")) + 
  labs(title="LGA and JFK Humidity in a year", y="Humidity (%)",x="Month") 

                                        
#3: Analyzing Rainy Days
#-manipulation
#--assumed that humid = 100 is indicating a rain
jfk_rain=full_data %>% filter(humid==100, origin=="JFK") %>% group_by(month) %>% #JFK
  summarise(rain=n_distinct(day),origin,.groups='drop') #multiple rain in a day will be counted as once
lga_rain=full_data %>% filter(humid==100, origin=="LGA") %>% group_by(month) %>% #LGA
  summarise(rain=n_distinct(day),origin,.groups='drop') #multiple rain in a day will be counted as once

#-visualization
ggplot() + 
  geom_point(data=jfk_rain, aes(x = month, y = rain, color=origin),size=3) + #JFK plot
  geom_point(data=lga_rain, aes(x = month, y = rain, color=origin),size=3) + #LGA plot
  annotate("rect", xmin=3.6, xmax=6.6, ymin=1 , ymax=6, alpha=0.1, fill="green")+ #Summer time
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                          "jul","aug","sep","oct","nov","dec"))+ 
  scale_y_continuous(breaks=1:7) + labs(title="Number of Rainy Days in A Year",
                                        x="Month", y="Rainy Days") 


#4: Analyzing Queens Temperature in Summer
#-manipulation
summer_temp=full_data %>% filter(season==2) %>% group_by(day) %>% 
  summarise(temp=max(temp),.groups='drop')

#-visualization
ggplot(summer_temp, aes(day, temp))+geom_line(aes(color=temp))+geom_point(aes(color=temp))+
  scale_x_continuous(breaks=1:31)+geom_hline(yintercept=90,color="orange")+ #show 31 days and make a warning line
  labs(title="Queens Temperature in Summer 2013",subtitle="Average of maximum temperature per day",
       y="Temperature(F)",x="Day",color="Temperature") 


#5: Relationship between Temperature and Wind Speed in Winter
#-manipulation
winter_wind=full_data%>%filter(season==4)%>%select(temp,wind_speed) 
summary(winter_wind) #show summary for temperature and wind speed in winter

#-visualization
ggplot(winter_wind, aes(temp,wind_speed)) + geom_point(na.rm=TRUE)+ #remove any NA value
  geom_vline(xintercept=10,color="blue",alpha=0.3)+ #frostbite potential temperature
  geom_hline(yintercept=55,color="blue",alpha=0.3)+ #frostbite potential wind speed
  annotate("pointrange", x=10, y=55, ymin=55, ymax=55,colour ="blue", size=0.77, alpha=0.6)+
  labs(title="Relationship between Temperature and Wind Speed",subtitle="Winter 2013",
       x="Temperature(F)",y="Wind Speed(mph)")+
  geom_text(label="Frostbite potential", x=16,y=56.5,color="blue")
 
 
#6: Queens Temperature per-season
#-manipulation
full_data%>%select(season,temp)%>%group_by(season)%>%summarise(avg_temp=mean(temp),.groups='drop')
#-visualization
ggplot(full_data, aes(x = temp, y=frequency(temp), fill = factor(season))) + 
  geom_bar(width=0.18,stat = "identity")+ theme_bw() + labs(fill="Season", 
                                                            x="Temperature(F)",
                                                            y="count",
                                                            title="Queens Temperature per season",
                                                            subtitle = "2013")


#7: Average temperature comparison between LGA and JFK in 2013
#-manipulation
jfk_temp=full_data %>% filter(origin=="JFK") %>% group_by(month) %>% 
  summarise(temp=mean(temp),origin, .groups='drop') #find average temp for JFK
lga_temp=full_data %>% filter(origin=="LGA") %>% group_by(month) %>% 
  summarise(temp=mean(temp),origin, .groups='drop') #find average temp for LGA
#-visualization
ggplot() + 
  geom_line(data = jfk_temp, aes(x = month, y = temp, color=origin)) +
  geom_line(data = lga_temp, aes(x = month, y = temp, color=origin)) + 
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                          "jul","aug","sep","oct","nov","dec")) + 
  labs(title="LGA and JFK Average Temperature in a year", y="Temperature(F)",x="Month") 


#8: Analyzing temperature for energy saving
#-manipulation
#--show temperature summary for each month
full_data%>%select(month,temp)%>%group_by(month)%>%summarise(avg_temp=mean(temp),
                                                             max_temp=max(temp),
                                                             min_temp=min(temp),.groups='drop')
#--for labeling purpose
monlab=c("1"="jan","2"="feb","3"="mar","4"="apr","5"="may","6"="jun","7"="jul",
         "8"="aug","9"="sep","10"="oct","11"="nov","12"="dec")

#-visualization
ggplot(full_data, aes(x=temp))+geom_histogram(bins=30,aes(fill = factor(season)))+
  facet_wrap(~month,labeller = labeller(month = monlab)) + #changing label name for each table
  geom_vline(xintercept=64,color="blue",alpha=0.5) + #safe and well-balanced indoor temperature line
  labs(title="LGA and JFK Temperature", y="Count",x="Temperature(F)",fill="Season",
       subtitle="2013") 


#9: Effect of Wind Speed and Wind Direction towards Flight at LGA
#-manipulation
lga_hwind=full_data%>%filter(origin=="LGA")%>%
  #convert mph into Knot & calculate headwind
  mutate(knot_speed=wind_speed/1.151,headwind=knot_speed*cos(wind_dir-45))%>% 
  select(month,knot_speed,wind_dir,headwind)%>%filter(headwind>=0)
#--show average and maximum headwind per month
lga_hwind%>%group_by(month)%>%summarise(hwind_avg=mean(headwind),
                                       hwind_max=max(headwind),.groups='drop')
#-visualization
ggplot(lga_hwind, aes(month,headwind))+geom_boxplot(aes(group=month))+
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                          "jul","aug","sep","oct","nov","dec"))+
  labs(title="LGA Airport Headwind in 2013", y="Headwind",x="Month",subtitle="Runway 4") 


#10: Finding possibility of heat stroke in Summer
#-manipulation
#--temp >= 86 because heat stroke potential occurs from temperature 86F
summer_dewp=full_data%>%filter(season==2, temp>=86)%>%select(time_hour,dewp,temp)
#--data frame for heat index which indicates heat stroke potential (>104)
heatindex=data.frame(temp=c(90:100),dewp=c(78,77,76,75,76,73,72,71,70,68,68))

#-visualization
ggplot() +
  geom_point(data=summer_dewp,aes(x=dewp,y=temp))+
  geom_line(data=heatindex, aes(x=dewp,y=temp),color="red",size=2,alpha=0.2)+ #heatstroke potential line
  scale_x_continuous(breaks=53:80)+scale_y_continuous(breaks=86:100)+
  labs(title="Heat Stroke Potential in Summer", y="Temperature(F)",x="Dew Point(F)")

 
#11: Relationship between temperature and dew point
#-manipulation
#--check the correlation between temperature and dew point
cor(full_data$temp,full_data$dewp)
#-visualization
ggplot(full_data,aes(dewp,temp))+geom_jitter()+geom_smooth(method="lm")+
  labs(title="Correlation Between Temperature and Dew Point", y="Temperature(F)",x="Dew Point(F)")


#12: Analyzing precipitation and temperature in Winter
#-manipulation
winter=full_data%>%filter(season==4,precip>0)
#--to show the days
full_data%>%filter(season==4,precip>0,temp<=32)
#-visualization
ggplot(winter, aes(temp,precip))+geom_count(alpha=0.2,color="blue") + facet_wrap(~hour)+
  #--this rectangle area indicates snowfalls
  annotate("rect", xmin=15, xmax=32, ymin=0.0 , ymax=0.6, alpha=0.1, fill="cyan")+
  labs(title="Hourly Temperature and Precipitation", 
       y="Precipitation(in)",x="Temperature(F)",subtitle = "Winter, 2013")


#13: Analyzing pressure
#-manipulation
#--show pressure summary for both airports
full_data%>%filter(origin=="JFK")%>%select(jfk_pressure=pressure)%>%summary()
full_data%>%filter(origin=="LGA")%>%select(lga_pressure=pressure)%>%summary()
#-visualization
ggplot(full_data,aes(pressure))+geom_histogram(bins=20,aes(group=origin,fill=origin),na.rm=T)+
  labs(title="Histogram for Pressure", 
       y="count",x="Pressure",subtitle = "2013")

#14: Analyzing Crosswind
#-manipulation
#--LGA
lga_cwind=full_data%>%filter(origin=="LGA")%>%
  #convert mph into Knot & calculate crosswind for Runway 4
  mutate(knot_speed=wind_speed/1.151,crosswind=knot_speed*sin(wind_dir-45))%>% 
  filter(crosswind>0)%>%select(origin,month,knot_speed,wind_dir,crosswind)
#--show average and maximum crosswind at LGA
lga_cwind%>%summarise(cwind_avg=mean(crosswind,na.rm=T),cwind_max=max(crosswind,na.rm=T),.groups='drop')
#--JFK
jfk_cwind=full_data%>%filter(origin=="JFK")%>%
  #convert mph into Knot & calculate crosswind for Runway 13R
  mutate(knot_speed=wind_speed/1.151,crosswind=knot_speed*sin(wind_dir-133))%>% 
  filter(crosswind>0)%>%select(origin,month,knot_speed,wind_dir,crosswind)
#--show average and maximum crosswind at JFK
jfk_cwind%>%summarise(cwind_avg=mean(crosswind,na.rm=T),cwind_max=max(crosswind,na.rm=T),.groups='drop')

#-visualization
ggplot()+
  geom_jitter(data = jfk_cwind, aes(y=crosswind,x=month,color=origin)) +
  geom_jitter(data = lga_cwind, aes(y=crosswind,x=month,color=origin)) +
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                          "jul","aug","sep","oct","nov","dec"))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35)) +
  geom_hline(yintercept=30,color="darkred",size=2,alpha=0.2)+
  labs(title = "Jitterplot for Crosswind in 2013", subtitle="JFK: Runway 13R | LGA: Runway 4")

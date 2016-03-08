#Seprate in test and train
test<-df_final[df_final$Year==2015,c(1,2,4:169)]
train<-df_final[!df_final$Year==2015,c(1,2,4:169)]

x<-paste("Team_2_Win ~",paste(names(df_final[c(1,2,5:169)]),collapse=" + "))

# Training the Dataset
library(randomForest)
model<-randomForest(Team_2_Win ~ Team_1 + Team_2 + Margin_Runs + Margin_Wickets + Date + Year + Sydney + Melbourne + Hobart + Perth + Auckland + Adelaide + Belfast + Southampton + Lords + Manchester + Leeds + Moratuwa + Kandy + Chittagong + Dhaka + Sharjah + Chandigarh + Kolkata + Colombo_RPS + Colombo_SSC + Nairobi_Gym + Nairobi_Aga + Mohali + Hyderabad_Deccan + Chennai + Mumbai + Chelmsford + Dublin + Edinburgh + ChesterleStreet + Northampton + Harare + Bulawayo + Potchefstroom + Benoni + Kimberley + Durban + Pietermaritzburg + Bloemfontein + Johannesburg + Cairns + Darwin + Multan + Faisalabad + Lahore + Rawalpindi + Karachi + Kingstown + St_Georges + Birmingham + The_Oval + Cardiff + Nottingham + Canterbury + Bogra + Khulna + Fatullah + Jaipur + St_Johns + Port_of_Spain + North_Sound + Providence + Bridgetown + Colombo_PSS + Napier + Queenstown + Roseau + Basseterre + Dunedin + Christchurch + Dambulla + Bristol + Glasgow + Hambantota + Pallekele + Canberra + Nelson + Hamilton + Swansea + Wellington + Scarborough + Sahiwal + Sialkot + Brisbane + Albion + Ahmedabad + Jalandhar + Cuttack + Taunton + Pune + Bangalore + Nagpur + Kingston + Devonport + Gujranwala + Peshawar + Delhi + Kanpur + Gwalior + Georgetown + Ballarat + Albury + Jamshedpur + Cape_Town + Centurion + East_London + Port_Elizabeth + Gros_Islet + Faridabad + Margao + Kochi + Indore + Rajkot + Abu_Dhabi + Dubai_DSC + Ranchi + Dharamsala + Dublin_Malahide + Aberdeen + Quetta + Amritsar + Leicester + Tunbridge_Wells + Srinagar + Vadodara + Guwahati + New_Delhi + Thiruvananthapuram + Launceston + Visakhapatnam + Mackay + Mumbai_BS + Singapore + Toronto + Hyderabad_Sind + Taupo + Hove + Galle + Jodhpur + Paarl + Vijayawada + Amstelveen + Kuala_Lumpur + Derby + Melbourne_Docklands + Whangarei + Mount_Maunganui + Lucknow + Sargodha + Sheikhupura + Tangier + Nairobi_Club + New_Plymouth + Berri + Patna + Worcester + Castries + Nairobi + King_City_NW + Kwekwe + Mombasa, data=train, ntree=500, mtry=15, importance=TRUE)
pred<-predict(model,test,type="response")
tb<-table(test$Team_2_Win,pred)
print(tb)
#Successful Cases
s<-sum(diag(tb))
print(s)
# Efficiy of RF Algorithm
(sum(diag(tb))/nrow(test))*100
#prop.table(tb,1)

# Hypothetical Betting Game
# If correct Guess the money gets double otherwise you loose everything
# Amount Invested in each match
Amount<-1000

#Matches Played in 2015 240 with TOP 10 Teams

# Total Amount Invested
Total_Amount<-Amount*240

# Total Amount Earned By Predicting the Correct Result
Amount_Earned<-s*2*Amount

# %Profit in one Year
(Amount_Earned/Total_Amount)*100-100



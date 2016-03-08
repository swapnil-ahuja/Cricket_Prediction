setwd("~/Documents/R/R PROGRAMS")
counteries<-c('Australia','Bangladesh','England','India','New Zealand','Pakistan','South Africa','Sri Lanka','West Indies','Zimbawe')
counteries

for (i in counteries[])# Loop for Counteries
{ for(j in 1971:2015){#Loop for Years
  add11<-paste('/Users/Swapnil/Documents/R/Cricket Data/',i,'/match_results/',j,'/',j,'.csv',sep="")
  print(add11)
  print(j)
  df<-read.csv(file= add11,head=TRUE,sep=",",col.names = c("Team_1","Team_2","Winner","Margin","Venue","Date","Year"),row.names = NULL)
  df[,'Year']<-NULL
  if(!is.na(df[1,'Team_1'])){
    
  df$Year<-j
  }
  print(df[1:3,])
 
  
  if(i=='Australia'){
  df_final<-df
  }
  else
  {
  df_final<-rbind(df_final,df)
  }
}}
df_final$Date<-as.Date(df_final$Date, "%B %d- %Y")#Convert Date to required format
df_final$Date<-as.numeric(as.POSIXct(df_final$Date, format="%Y-%m-%d"))# Convert Date to Unix Time




#Cleaning Data

for (i in df_final$Margin){
  if((grepl(pattern="(\\w\\w\\w \\d\\d)",i))){df_final$Margin[df_final$Margin==i]<-NA } 
  if((grepl(pattern="(\\w\\w\\w \\d)",i))){df_final$Margin[df_final$Margin==i]<-NA }
  
  }
df_final<-df_final[complete.cases(df_final),]# remove rows with NA
rownames(df_final)<-NULL #resetting rows index numbers




# Dummy Variables for Venue
for(level in unique(df_final$Venue)){
      df_final[paste(level)] <- ifelse(df_final$Venue == level, 1, 0)
}




# Taking care of tie matches
df_final$Margin<- as.character(df_final$Margin)
df_final$Margin[df_final$Margin==""]<-"0"
df_final$Margin<-as.factor(df_final$Margin)



#Margin Wickets and Runs
df_final$Margin<-gsub("( wickets)","",df_final$Margin)
df_final$Margin<-gsub("( wicket)","",df_final$Margin)




# Seprating Margin Runs and Wickets
df_final$Margin_Wickets<-ifelse(!grepl(pattern="(run)",df_final$Margin),df_final$Margin,0)
df_final<-df_final[,c(1:4,169,5:168)]
names(df_final)[names(df_final) == 'Margin'] <- 'Margin_Runs'
df_final$Margin_Runs<-ifelse(grepl(pattern="(run)",df_final$Margin_Runs),df_final$Margin_Runs,0)
df_final$Margin_Runs<-gsub("( runs)","",df_final$Margin_Runs)
df_final$Margin_Runs<-gsub("( run)","",df_final$Margin_Runs)
df_final$Venue<-NULL
df_final$Margin_Runs<-as.numeric(df_final$Margin_Runs)
df_final$Margin_Wickets<-as.numeric(df_final$Margin_Wickets)
df_final$Team_1<-as.character(K$Team_1)
df_final$Team_2<-as.character(df_final$Team_2)
df_final$Winner<-as.character(df_final$Winner)


#Creating a New Column
df_final$Team_2_Win<-0


# Shifting Columns
df_final<-df_final[,c(1:3,169,4:168)]


#Winner/Looser/DRAW
df_final$Team_2_Win[df_final$Team_2==df_final$Winner]<-1
df_final$Team_2_Win[df_final$Winner=="no result"]<-2


# Factor in Character
df_final$Team_2_Win<-as.character(df_final$Team_2_Win)
df_final$Team_2_Win[df_final$Team_2_Win=="2"]<-"DRAW"
df_final$Team_2_Win[df_final$Team_2_Win=="1"]<-"WIN"
df_final$Team_2_Win[df_final$Team_2_Win=="0"]<-"LOOSE"
df_final$Team_2_Win<-as.factor(df_final$Team_2_Win)


names(df_final)[names(df_final) == 'Lord\'s'] <- 'Lords'
names(df_final)[names(df_final) == 'Chester-le-Street'] <- 'ChesterleStreet'



df_final$Team_1<-as.factor(df_final$Team_1)
df_final$Team_2<-as.factor(df_final$Team_2)
df_final$Winner<-as.factor(df_final$Winner)


#Change every into factor
df_final[,9:169]<-lapply(df_final[,9:169],as.factor)

#Removing Brackets from Column Names
names(df_final)<-gsub("\\(|\\)","",names(df_final))

#removing ' from Column Names
names(df_final)<-gsub("\\'","",names(df_final))

# removing space from Column Names
names(df_final)<-gsub("\\s","_",names(df_final))

# saving file to csv
write.csv(df_final,file= "Final_DATA.csv")








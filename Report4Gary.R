library(tidyverse)
library(lubridate)
library(jpeg)
library(grid)
library(gridExtra)
library(readxl)


#Farm_Dams_Toowoomba_Data.xlsx ontains field info and single methane files contain emission data
#Join them by Site (use syntax of farmers FirstName_LastName):

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/FarmDams/DATA_Toowoomba")

# Read the data
field_data <- read_excel("Farm_Dams_Toowoomba_Data.xlsx", sheet = "FarmDams") #continuously updated on TEAMs (see TealCarbonGroup)
unique(field_data$FarmDam_ID)#see  all farm dams IDs

water_data <- read_excel("Farm_Dams_Toowoomba_Data.xlsx", sheet = "WATER_DATA_CLEAN") #continuously updated on TEAMs (see TealCarbonGroup)

#PLOT EMISSION for MyFarmDam "Gary_Swinbourne_01" ============
MyFarmDam <- "Gary_Swinbourne_01"  #Choose one farm dam to visualize

#Load backgrond image
img <- readJPEG("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/FarmDams/DATA_Toowoomba/PHOTOS/PHOTOS_FarmDams/SunshineCoast FarmDamCampaignPHOTOS/Gary_Swinbourne _01_20240716_001348315.jpg")
img_grob <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))# Create a grob for the background image

#Load emission data:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/FarmDams/DATA_Toowoomba/PONDI/PONDI_DATA")
emission_data <- read.csv("RAW_MethaneData_Gary_Swinbourne_All.csv") #also on Teams downloaded from deployed Pondi.
names(emission_data)
emission_data$Date.Time.UTC.10 <- as.POSIXct(emission_data$Date.Time.UTC.10 ,"%Y-%m-%d %H:%M", tz="UTC")#convert to time format that R can  work with

start_time <- select(field_data,TimeStart,FarmDam_ID) %>% filter(FarmDam_ID == MyFarmDam )
emission_start_time <-  as.POSIXct(strptime(start_time$TimeStart, "%d/%m/%Y %H:%M", tz="UTC"))
emission_start_time
start <-  as.Date(emission_start_time)
start

end_time <- select (field_data,TimeEnd,FarmDam_ID) %>% filter(FarmDam_ID == MyFarmDam) 
emission_end_time = as.POSIXct(strptime(end_time$TimeEnd,"%d/%m/%Y %H:%M",  tz="UTC"))
emission_end_time
end<- as.Date(emission_end_time)
end

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/BCL_R/FarmDams/DATA_Toowoomba") #back to working directory of your R script location

#CO2_filtered_data =#GET Average emission stats for plotting:
CO2_filtered_data <- emission_data %>% 
                 filter(Date.Time.UTC.10 >= emission_start_time & Date.Time.UTC.10 <= emission_end_time) %>% 
                 filter(Total.CO2.Est...mg.m.2.day. > 0)     %>% ##Remove negative emission logs
                   
                  mutate(Total.CO2.Est...mg.m.2.day. =  Total.CO2.Est...mg.m.2.day./1000) %>% #get emissions in g (not mg)

                group_by(Site) %>%
                summarise(Latitude = mean(Latitude..DD., na.rm=T),
                          Longitude = mean (Longitude..DD., na.rm=T),
                  
                          AV=mean   (Total.CO2.Est...mg.m.2.day. , na.rm = T),
                          SD=sd     (Total.CO2.Est...mg.m.2.day.),
                          N = length(Total.CO2.Est...mg.m.2.day.),
                          SE= SD / sqrt(N)) %>%
                mutate (Gas = "CO2")

CO2_filtered_data


#CH4_filtered_data:
#CH4_filtered_data (84 x – methane (CH4) = CO2 equivalent):
CH4_filtered_data <- emission_data %>% 
               filter(Date.Time.UTC.10 >= emission_start_time & Date.Time.UTC.10 <= emission_end_time) %>% 
                filter(Total.Methane.Est...mg.m.2.day. > 0) %>% #Remove negative emission logs
  
                mutate(Total.Methane.Est...mg.m.2.day. =  Total.Methane.Est...mg.m.2.day./1000) %>% #get emissions in g (not mg)

              group_by(Site) %>% 
              summarise(Latitude = mean(Latitude..DD., na.rm=T),

                
                           Longitude = mean (Longitude..DD., na.rm=T),
                
                                  AV=mean   (Total.Methane.Est...mg.m.2.day. *84 , na.rm = T),
                                  SD=sd     (Total.Methane.Est...mg.m.2.day. *84),
                                 N = length(Total.Methane.Est...mg.m.2.day.),
                                SE= SD / sqrt(N)) %>%         mutate (Gas = "CH4")

CH4_filtered_data

filtered_data <- rbind(CO2_filtered_data, CH4_filtered_data)#join CH4 and CO2 emission stats data
max(filtered_data$AV) #2.055708 grams
filtered_data
filtered_data$Emission <-ifelse(filtered_data$AV >100, "High", "Low")#high values would be 80-100 g CO2 m-2 day-1 whereas low would be 0-20 g CO2 m-2 day-1 for emissions.
  
  
p1 <- ggplot(filtered_data, aes(x = Gas, y = AV)) +
   annotation_custom(img_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  # Full background image
   geom_bar(aes(fill = Emission), position="identity", stat="identity", alpha = 0.6) +
   geom_errorbar(aes(ymin = AV - SE, ymax = AV + SE), width = 0.5, size = 0.9, alpha = 0.6) +
   
   labs(title = paste("Farm Dam ID:" , MyFarmDam),
        subtitle = paste("Sampling period: ", start, " to" , end),
        fill= "Emission level: ", 
        x= "", 
        y = bquote("CO"[2] ~ "equivalent flux " ~(g*~m^-2 ~day^-1))) +
   
  
     #ADJUST ACCORDINGLY TO EMISSION AVERAGE LEVELS +- SE:  
     scale_y_continuous(limits = c(0,3), breaks = c(0, 1, 2, 2.8), labels= c(0,  1, 2, 100)) +


   # Add a red horizontal line at y = 100
   geom_hline(yintercept = 2.8, color = "red", size = 1, linetype="dashed") +
   
   # Add text label for the line
   annotate("text", x = Inf, y = 2.8, label = "Maximum emission", vjust = -0.5, hjust = 1.5, color = "red", size = 5) +
   
   scale_fill_manual(values = c("grey")) +
   
   theme_bw() +
   theme(
         axis.text.y = element_text(size = 10, colour = c("black","black","black", "red")),
         axis.title.x = element_text(size = 14),
         axis.text.x = element_text(size = 14, colour = "black"),
         axis.title.y = element_text(size = 14),
         strip.text = element_text(size = 16),
         plot.background = element_blank(),  # Ensure no background color hides the image
         legend.position = c(.8,.8),  # Change to named position first
         legend.text = element_text(size = 12),  # Increase legend text size
         legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
         legend.background = element_rect(fill = "white", color = "black"),  # Box around the legend
         panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Soft grid lines
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
         panel.grid.minor = element_blank())  # Remove minor grid lines

p1

#PLOT WATER:==========
# Select rows 7 and 9 (Total N and Total P)
#Lakes and reservoirs: 5-50 micrograms /L (0.000005 to 0.00005 g/L) (as total-phosphorus) or in mg/L = 50/1000
#link: https://www.dcceew.gov.au/environment/protection/npi/substances/fact-sheets/total-phosphorus
 
MyFarmDam_water <- water_data %>%  select(contains("Gary_Swinbourne") | contains("sample"))
MyFarmDam_water

select_Phosphate <-  MyFarmDam_water [7, ]
P <- gather(select_Phosphate,  FarmDamID, Content_mgL,-Client_SampleID)%>% 
  mutate(NP_Level = ifelse(as.numeric(Content_mgL) > 50/1000, "High_P","Low_P"))
P$Content_mgL <- as.numeric(P$Content_mgL)
P

Pplot <- ggplot(data = P, aes(x = FarmDamID, y = Content_mgL)) +
  # Add points with NP_Level color mapping
  geom_point(aes(fill = NP_Level), size = 4, alpha = 0.8, shape = 21, stroke = 1.2) +
  
  scale_fill_manual(values = c("green"))+
  
     scale_y_continuous(limits = c(0,0.06), breaks = c(0.01, 0.02, 0.03, 0.04, 0.05)) +

  
  # Add red horizontal dashed line at y = 0.05
  geom_hline(aes(yintercept = 0.05), linetype = "dashed", color = "red", size = 1) +
  
  # Annotate red text at the center of the plot
  annotate("text", x = mean(as.numeric(factor(P$FarmDamID))), y = 0.051, 
           label = "Guideline level", color = "red", size = 5, hjust = 0.5, vjust = -0.5) +
  
  # Add titles and labels
  ggtitle("Total Phosphate") +
  labs(col = "Level:", x = "Farm Dam ID", y = "Content (mg/L)") +
  

  # Rotate x-axis text and make other aesthetic adjustments
  theme_minimal(base_size = 14) +  # Minimal theme with larger base font size
  theme(
    axis.text.x = element_text(angle = 35, hjust = 0.9, size = 10),  # Rotate x-axis labels 90 degrees
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),  # Bold x-axis title with spacing
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),  # Bold y-axis title with spacing
    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),  # Centered plot title
    legend.position = "top",  # Position the legend on the right
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.background = element_rect(fill = "white", color = "black"),  # Box around the legend
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Soft grid lines
    panel.grid.minor = element_blank())  # Remove minor grid lines

Pplot


#Lakes and reservoirs: 100-500 micrograms /L (0.0001 to 0.0005 g/L) (as total-nitrogen)
#link:https://www.dcceew.gov.au/environment/protection/npi/substances/fact-sheets/total-nitrogen
#dcceew-main

select_Nitrogen<-  MyFarmDam_water [9, ]
N <- gather(select_Nitrogen,  FarmDamID, Content_mgL,-Client_SampleID)%>% 
  mutate(NP_Level = ifelse(as.numeric(Content_mgL) > 500/1000, "High_N","Low_N"))
N$Content_mgL <- as.numeric(N$Content_mgL)
N

Nplot <- ggplot(data = N, aes(x = FarmDamID, y = Content_mgL)) +
  # Add points with NP_Level color mapping
  geom_point(aes(fill = NP_Level), size = 4, alpha = 0.8, shape = 21, stroke = 1.2) +
  
    scale_fill_manual(values = c("green"))+
  
   scale_y_continuous(limits = c(0,0.6), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +


  
  # Add red horizontal dashed line at y = 0.05
  geom_hline(aes(yintercept = 0.5), linetype = "dashed", color = "red", size = 1) +
  
  # Annotate red text at the center of the plot
  annotate("text", x = mean(as.numeric(factor(P$FarmDamID))), y = 0.51, 
           label = "Guideline level", color = "red", size = 5, hjust = 0.5, vjust = -0.5) +
  
  # Add titles and labels
  ggtitle("Total Nitrogen") +
  labs(col = "Level:", x = "Farm Dam ID", y = "Content (mg/L)") +
  

  # Rotate x-axis text and make other aesthetic adjustments
  theme_minimal(base_size = 14) +  # Minimal theme with larger base font size
  theme(
    axis.text.x = element_text(angle = 35, hjust = 0.9, size = 10),  # Rotate x-axis labels 90 degrees
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),  # Bold x-axis title with spacing
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),  # Bold y-axis title with spacing
    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10)),  # Centered plot title
    legend.position = "top",  # Position the legend on the right
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.background = element_rect(fill = "white", color = "black"),  # Box around the legend
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Soft grid lines
    panel.grid.minor = element_blank())  # Remove minor grid lines

Nplot


#Combined PLOT:===========
grid.arrange(p1, Pplot, Nplot, ncol = 3, nrow = 1)
grid_plot <- grid.arrange(p1, Pplot, Nplot, ncol = 3, nrow = 1)

ggsave("combined_Plot4Gary.png", grid_plot, width = 19, height = 10, units = "in")

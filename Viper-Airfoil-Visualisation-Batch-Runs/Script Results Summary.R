#---------------------------->
#--- Summarise Results
#--- Alwin Wang MAE3401
#============================>

savedata = "Output_Data"
savesummary = "Summary_Data"

filelist = list.files(savedata, pattern = "*.rds")

filecontents <- 
  data.frame(filename = filelist, 
             temp = gsub("Re", "_", gsub("AoA", "_", gsub(".rds", "_", filelist)))) %>% 
  separate(temp, c("temp1", "Re", "AoA", "Data", "temp2"), sep = "_") %>%
  select(-temp1, - temp2) %>%
  mutate(Re = as.numeric(Re)) %>%
  mutate(AoA = as.numeric(AoA))

dataname = "blplot"

datasum = lapply(
  filter(filecontents, Data == dataname),
  function(filename) {
    
  }
)

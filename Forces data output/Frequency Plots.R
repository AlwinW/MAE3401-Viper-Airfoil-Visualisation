library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

my_files = list.files(pattern ="\\.dat$")
my_data <- lapply(my_files, 
                  function(name)
                    read.table(name, header=TRUE)
)
names(my_data) <- gsub("\\.dat$", "", my_files)
big_data = bind_rows(my_data, .id="id")
ggplot() + geom_line(data=big_data, aes(x=big_data$t, y=big_data$Ftotal_y)) + 
  ylim(-1, 1) + xlim(0, 150) + facet_wrap(~id, scales="free")

### Libraries
library(readr)
library(stringr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
# To properly render the Chinese characters:
Sys.setlocale(category = "LC_ALL", locale = "Chinese")

solution.file.name <- here("prob v12 provs baseline 2019.txt")
# other file names/paths
helperfiles.path <- here("helper files")
output.file.all <- here(str_replace(solution.file.name, ".txt", " all.xlsx"))
output.file.imp <- here(str_replace(solution.file.name, ".txt", " imp.xlsx"))

# get solution, read with csv
df.solutions <- read.csv(solution.file.name, sep = "'", header=FALSE, stringsAsFactors=FALSE)
#suppliers
df.suppliers <- df.solutions %>% 
  select(V1, V2, V4, V6, V7) %>% 
  rename(var_type = V1,
         orig_node_id = V2,
         dest_node_id = V4,
         coal_type = V6,
         volume_Mt = V7)
# clean up char vars
df.suppliers$var_type <- substr(df.suppliers$var_type,1,nchar(df.suppliers$var_type)-2)
df.suppliers$var_type <- str_trim(df.suppliers$var_type, side = c("both"))
df.suppliers$orig_node_id <- str_trim(df.suppliers$orig_node_id, side = c("both"))
df.suppliers$dest_node_id <- str_trim(df.suppliers$dest_node_id, side = c("both"))
df.suppliers$coal_type <- str_trim(df.suppliers$coal_type, side = c("both"))
# fix volumes
df.suppliers$volume_Mt <- substr(df.suppliers$volume_Mt, 4, 99)
df.suppliers$volume_Mt <- as.numeric(df.suppliers$volume_Mt)
df.suppliers$orig_node_type <- substr(df.suppliers$orig_node_id, 1, 4)
df.suppliers$dest_node_type <- substr(df.suppliers$dest_node_id, 1, 4)  

# keep mines only (supplies)
df.suppliers <- df.suppliers %>% 
  filter(var_type=="flow_by_coaltype") %>% 
  filter(orig_node_type=="mine")
# Hook up mine info
node.info.file <- file.path(helperfiles.path, "node to node id helper latest v12.xlsx")
df.node.info <- read_excel(node.info.file) 
mine.loc.info.file <- file.path(helperfiles.path, "mine name to location.xlsx")
df.mine.loc.info <- read_excel(mine.loc.info.file) %>% select(node_name, country, location) %>% distinct()
# Hook up mine names full
df.suppliers <- left_join(df.suppliers, df.node.info, by = c("orig_node_id" = "node_id"))
# Hook up mine locations
df.suppliers <- left_join(df.suppliers, df.mine.loc.info, by = "node_name") 
df.suppliers <- df.suppliers %>% rename(orig_node_name = node_name)
# Hook up destination names full
df.suppliers <- left_join(df.suppliers, df.node.info, by = c("dest_node_id" = "node_id")) %>% rename(dest_node_name = node_name)
# Russian imports split into overland and seaborne
df.suppliers$location <- ifelse(df.suppliers$location=="Russia" & df.suppliers$dest_node_type=="ovld", "Russia - overland", df.suppliers$location)
df.suppliers$location <- ifelse(df.suppliers$location=="Russia" & df.suppliers$dest_node_type=="navo", "Russia - seaborne", df.suppliers$location)

### Summarize all
df.suppliers$coal_group <- substr(df.suppliers$coal_type, 1, 7)  
df.supplier.summ.all <- df.suppliers %>% 
  group_by(location, coal_group) %>% 
  summarise(volume_Mt = sum(volume_Mt))
df.supplier.summ.imp <- df.suppliers %>% 
  filter(country!="China") %>% 
  group_by(location, coal_group) %>% 
  summarise(volume_Mt = sum(volume_Mt))
############################    Save to file   ##############################################
# Save with date to prevent overwriting
write.xlsx(df.supplier.summ.all, output.file.all)
write.xlsx(df.supplier.summ.imp, output.file.imp)
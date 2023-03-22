

library(dplyr)
library(purrr)

# setwd("C:/Users/ftw712/Desktop/griis-compendium/")


xml2::read_xml("https://cloud.gbif.org/griis/rss.do") %>% 
XML::xmlParse() %>%
XML::xmlToList() %>%
pluck("channel") %>%
`[`(15:500) %>%
map(~ tibble(title=.x$title,dwca=.x$dwca,link=.x$link)) %>%
bind_rows() %>% 
filter(!grepl("Reef Life Survey",title)) %>%
transpose() %>%
map(~ httr::GET(.x$dwca, httr::write_disk(paste0("data/zip/",.x$title,".zip"), overwrite=TRUE)))


list.files("data/zip/") %>%
map(~ unzip(paste0("data/zip/",.x),exdir=paste0("data/dwca/",gsub(".zip","",.x)))) 

list.files("data/dwca/",full.names=TRUE)


d = list.files("data/dwca/",full.names=TRUE) %>%
map(~ {
file_name = .x %>% gsub("data/dwca/","",.) %>% gsub("/taxon.txt","",.)
taxon = readr::read_tsv(paste0(.x,"/taxon.txt"),col_types=readr::cols(.default = "c")) 
distribution = readr::read_tsv(paste0(.x,"/distribution.txt"),col_types=readr::cols(.default = "c"))
speciesprofile = readr::read_tsv(paste0(.x,"/speciesprofile.txt"),col_types=readr::cols(.default = "c"))

out = merge(taxon,distribution,by="id") %>% 
merge(speciesprofile,by="id") %>%
mutate(file_name = file_name) 
if(!nrow(out) == nrow(taxon)) warning("possible bad merge")
out
}) %>%
bind_rows() %>% 
setNames(.,paste0("source_",colnames(.))) %>%
mutate(m_id = source_scientificName) %>%
mutate(source_file_name = gsub("Protected Areas - Global Register of Introduced and Invasive Species","",source_file_name)) %>%
mutate(source_file_name = gsub("Global Register of Introduced and Invasive Species","",source_file_name)) %>%
mutate(source_file_name = gsub("GRIIS Checklist of Introduced and Invasive Species","",source_file_name)) %>%
glimpse() 

d %>% pull(source_file_name) %>% unique()

d %>% 
pull(source_scientificName) %>% 
unique() %>% 
rgbif::name_backbone_checklist() %>% 
setNames(.,paste0("gbif_",colnames(.))) %>%
mutate(m_id = gbif_verbatim_name) %>% 
merge(d, by="m_id") %>%
mutate(source_establishmentMeans = tolower(source_establishmentMeans)) %>%
mutate(source_isInvasive = tolower(source_isInvasive)) %>%
mutate(source_isInvasive = 
case_when(
source_isInvasive == "true" ~ "invasive",
source_isInvasive == "yes" ~ "invasive",
grepl("invasive",source_isInvasive) ~ "invasive",
source_isInvasive == "false" ~ NA_character_,
source_isInvasive == "to be evaluated" ~ NA_character_,
source_isInvasive == "not specified" ~ NA_character_,
source_isInvasive == "not evaluated" ~ NA_character_,
source_isInvasive == "null" ~ NA_character_,
TRUE ~ as.character(source_isInvasive)
)) %>% 
mutate(source_establishmentMeans = tolower(source_establishmentMeans)) %>%
mutate(source_establishmentMeans = 
case_when(
source_establishmentMeans == "alien" ~ "alien",
source_establishmentMeans == "native|alien" ~ NA_character_,
source_establishmentMeans == "present" ~ "present",             
source_establishmentMeans == "cryptogenic|uncertain" ~ "uncertain",
source_establishmentMeans == "native/alien" ~ NA_character_,       
source_establishmentMeans == "uncertain" ~ "uncertain",            
source_establishmentMeans == "native|invasive" ~  NA_character_,      
source_establishmentMeans == "cryptogenic|unknown" ~ "uncertain",
source_establishmentMeans == "alien/native" ~ NA_character_,         
source_establishmentMeans == "cryptogenic|uncerain" ~  "uncertain",
source_establishmentMeans == "cryptogeni|uncertain" ~ "uncertain",
source_establishmentMeans == "cryptogenic/uncertain" ~ "uncertain",
TRUE ~ source_establishmentMeans
)) %>%
glimpse() %>%
select(
-source_acceptedNameUsage,
-source_originalNameUsage,
-source_vernacularName,
-source_locality,
-source_locationID,
-source_occurrenceRemarks,
-source_habitat,
-source_id,
-source_taxonID,
-source_acceptedNameUsage,
-source_kingdom,
-source_phylum,
-source_class,
-source_order,
-source_family,
-source_taxonRank,
-source_taxonomicStatus,
-source_occurrenceStatus
) %>% 
readr::write_tsv("exports/griis-compendium.tsv") 

# }


d = readr::read_tsv("exports/griis-compendium.tsv") 

lapply(d, function(x) summary(x))

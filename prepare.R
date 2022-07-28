library(xml2)
library(tidyverse)

#### Functions ####

get_metadata <- function(x) {
  
  ## get the date of the first edition in teiHeader
  t_date=xml_find_first(x, xpath = "d1:teiHeader/d1:fileDesc/d1:sourceDesc/d1:bibl[@type='firstEdition']/d1:date",ns=xml_ns(x)) %>% xml_text()
  
  ## if there is no firstedition date, grab the first one in source description 
  if(is.na(t_date)) {
    t_date=xml_find_first(x, xpath = "d1:teiHeader/d1:fileDesc/d1:sourceDesc//d1:date",ns=xml_ns(x)) %>% xml_text()
  }
  
  ## grab first author in teiHeader
  t_aut=xml_find_first(x, xpath = "d1:teiHeader//d1:author",ns=xml_ns(x)) %>% xml_text()
  # grab first novel title in teiHeader
  t_title=xml_find_first(x, xpath = "d1:teiHeader//d1:title",ns=xml_ns(x)) %>% xml_text()
  
  ## combine this into table
  df=tibble(year=t_date,author=t_aut, title=t_title)
  return(df)
}

parse_paragraphs <- function(x) {
  ## extract paragraph nodes within chapters (<div> elements)
  ## d1 references the TEI namespace
  
  text=xml_find_all(x, xpath = "//d1:text/d1:body//d1:p",ns=xml_ns(x))
  #text=xml_find_all(x, xpath = "//d1:div[@type='chapter']//d1:p",ns=xml_ns(x))
  
  ## if there is no chapter subdivision, just grab all paragraphs from the <text> 
  #if (length(text) == 0) {
  #  text=xml_find_all(x, xpath = "//d1:text//d1:p",ns=xml_ns(x))
  #}
  ## get text, collapse into one 
  plain=text %>% xml_text() %>% paste(collapse = "\n")
  return(plain)
  
}


#### Managing files ####

## Czech
cze_files <-  tibble(file=list.files("ELTeC_originals/Cze/level1",full.names = T),lang="cz")

## Polish
pol_files <- tibble(file=list.files("ELTeC_originals/Pol/level1",full.names = T),lang="pl")

# remove first README row from SRP
c <- bind_rows(cze_files,pol_files)
langs <- unique(c$lang) %>% sort()

## get language ID for folder names
langs_id <- str_replace(c$file, ".*?//([A-Z]{3}).*", "\\1") %>% unique() %>% sort()

## create directories for languages in 'plain_corpus/' if not there
for (l in langs_id) {
  dir.create(paste0("plain_corpus/",l,"/"),showWarnings = F)
}


## empty variable for future meta table
meta_df <- NULL


for(l in 1:length(langs)) {
  ## create vector of files per language
  files <- c %>% filter(lang==langs[l]) %>% pull(file)

for (i in 1:length(files)) {
  # create filename
  p=str_extract(files[i], paste0(langs_id[l],".*\\.")) %>% paste0("txt")
  
  # read xml 
  f=read_xml(files[i])
  
  # extract text (paragraphs in chapters)
  plain=parse_paragraphs(f)
  
  # write plain file
  write_lines(plain,paste0("plain_corpus/",langs_id[l], "/",p))
  
  }
 } # end of main loop

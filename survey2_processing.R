# Eric J. Earley
# RTI International
# September 2023

# This code will extract Research SITE x Entity collaborations from survey 2 section 6. 
# This is the section that almost destroyed the CREID CC

# create an edge list:
#   RC, RS, Country, EntityX
# This will be added to the master RC-RS relationship table for Social Network Analysis (SNA)

# Once a complete edge list is created, convert to an adjacency matrix using iGraph
# get.adjacency(graph.edgelist(as.matrix(dat), directed=FALSE))





#setwd("C:/Users/eearley/OneDrive - Research Triangle Institute/Documents/CREID/DCH-WG/Social Network Analysis/")
setwd("C:/Users/eearley/OneDrive - Research Triangle Institute/Network Analysis/Social Network Analysis/")

OUTFILE = "data/survey2_table_2023_10_05.txt"




# read.table will have issues with ' and special characters.
# 1. convert the text file to UTF-8 instead of ANSI
# 2. remove all double quotes from the file with sed
# 3. quote="" will stop treating ' as a special character
# 4. comment.char = "" will stop treating # as a comment character

#survey2 = read.table("../../survey2/processed_results/survey2_section6_tall_2023_UTF8.txt",
survey2 = read.table("data/survey2_section6_tall_2023_UTF8.txt",
                     sep="|",
                     header=F, 
                     stringsAsFactors = F, 
                     quote="",
                     comment.char = "")


# this function will take a data.frame and pre-defined entity name
# it will clean out unused rows/columns
# and format lines to be added to a growing output table
clean.tab = function(tmp, entity = "") {
  tmp = tmp[!is.na(tmp[,2]),]
  tmp = tmp[2:dim(tmp)[1],]
  colnames(tmp) = c("RC","RS","Country","Entity")
  tmp = as.data.frame(tmp)
  tmp = tmp[!is.na(tmp$Country) & !is.na(tmp$Entity),]
  if (entity != "") {
    tmp$Entity = ifelse(!tmp$Entity %in% c(NA, "Never","Not applicable"),entity,NA)
    tmp = tmp[!is.na(tmp$Entity),]  
  }
  
  
  return(tmp)
}

#### INITIALIZE ####
out = NULL


#### COUNTRY 1 ####

# every column is a unique RCxRS
# 1628 = RC name
# 4 = RS name
# 28 = country #1
# 40 =  in-country collaborators for country #1 (free text)
out = clean.tab(t(survey2[c(1628,4,28,40),]))

# 43 = freq. Ministry of Health
tmp = clean.tab(t(survey2[c(1628,4,28,43),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

# 44 = freq. Ministry of Agriculture
tmp = clean.tab(t(survey2[c(1628,4,28,44),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)



# 45 = ... Livestock
tmp = clean.tab(t(survey2[c(1628,4,28,45),]),
                "Ministry of Livestock")

out = rbind(out, tmp)


# 46 = ... Forestry
tmp = clean.tab(t(survey2[c(1628,4,28,46),]),
                "Ministry of Forestry")
out = rbind(out, tmp)


# 47 = ...Wildlife
tmp = clean.tab(t(survey2[c(1628,4,28,47),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

# 55 = WHO Regional office
tmp = clean.tab(t(survey2[c(1628,4,28,55),]),
                "WHO Regional Office")
out = rbind(out, tmp)


# 56 = WHO Country office
tmp = clean.tab(t(survey2[c(1628,4,28,56),]),
                "WHO Country Office")
out = rbind(out, tmp)


# 57 = WHO Global office
tmp = clean.tab(t(survey2[c(1628,4,28,57),]),
                "WHO Global Office")
out = rbind(out, tmp)

# 58 = National Public Health Institute
tmp = clean.tab(t(survey2[c(1628,4,28,58),]),
                "National Public Health Institute")
out = rbind(out, tmp)

# 59 = MSF (doctors without borders)
tmp = clean.tab(t(survey2[c(1628,4,28,59),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

# 60 = UNICEF
tmp = clean.tab(t(survey2[c(1628,4,28,60),]),
                "UNICEF")
out = rbind(out, tmp)

# 61 = CDC
tmp = clean.tab(t(survey2[c(1628,4,28,61),]),
                "CDC")
out = rbind(out, tmp)

# 62 = US Embassy
tmp = clean.tab(t(survey2[c(1628,4,28,62),]),
                "US Embassy")
out = rbind(out, tmp)

# 63 = International Research Institute
tmp = clean.tab(t(survey2[c(1628,4,28,63),]),
                "International Research Institute")
out = rbind(out, tmp)

# 101 = Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,28,101),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,28,108),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #3
tmp = clean.tab(t(survey2[c(1628,4,28,115),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #4
tmp = clean.tab(t(survey2[c(1628,4,28,122),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #5
tmp = clean.tab(t(survey2[c(1628,4,28,129),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #6
tmp = clean.tab(t(survey2[c(1628,4,28,136),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #7
tmp = clean.tab(t(survey2[c(1628,4,28,143),]),
                entity = "")
out = rbind(out, tmp)


#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,28,152),]),
                entity = "")
out = rbind(out, tmp)

#Local organization #2
tmp = clean.tab(t(survey2[c(1628,4,28,155),]),
                entity = "")
out = rbind(out, tmp)

#Local organization #3
tmp = clean.tab(t(survey2[c(1628,4,28,158),]),
                entity = "")
out = rbind(out, tmp)

#Local organization #4
tmp = clean.tab(t(survey2[c(1628,4,28,161),]),
                entity = "")
out = rbind(out, tmp)

#Local organization #5
# tmp = clean.tab(t(survey2[c(1628,4,28,164),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Local organization #6
# tmp = clean.tab(t(survey2[c(1628,4,28,167),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Local organization #7
# tmp = clean.tab(t(survey2[c(1628,4,28,170),]),
#                 entity = "")
# out = rbind(out, tmp)



#### COUNTRY 2 ####
#29 = country name 2
# 199 = country #2 in-country collaborators (free text)
tmp = clean.tab(t(survey2[c(1628,4,29,199),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,202),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,203),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,204),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,205),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,206),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,214),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,29,215),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,29,216),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,217),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,218),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,219),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,220),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,221),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,29,222),]),
                "International Research Institute")
out = rbind(out, tmp)

# 101 = Academic institution
tmp = clean.tab(t(survey2[c(1628,4,29,260),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,29,267),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #3
tmp = clean.tab(t(survey2[c(1628,4,29,274),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #4
tmp = clean.tab(t(survey2[c(1628,4,29,281),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #5
tmp = clean.tab(t(survey2[c(1628,4,29,288),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #6
tmp = clean.tab(t(survey2[c(1628,4,29,295),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #7
tmp = clean.tab(t(survey2[c(1628,4,29,302),]),
                entity = "")
out = rbind(out, tmp)


#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,29,311),]),
                entity = "")
out = rbind(out, tmp)

# #Local organization #2
# tmp = clean.tab(t(survey2[c(1628,4,29,314),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Local organization #3
# tmp = clean.tab(t(survey2[c(1628,4,28,158),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Local organization #4
# tmp = clean.tab(t(survey2[c(1628,4,28,161),]),
#                 entity = "")
# out = rbind(out, tmp)


















#### COUNTRY 3 ####
#30 = country name 3
# 199 = country #2 in-country collaborators (free text)
tmp = clean.tab(t(survey2[c(1628,4,30,358),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,361),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,362),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,363),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,364),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,365),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,373),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,30,374),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,30,375),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,376),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,377),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,378),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,379),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,380),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,30,381),]),
                "International Research Institute")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,30,419),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,30,426),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #3
tmp = clean.tab(t(survey2[c(1628,4,30,433),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #4
tmp = clean.tab(t(survey2[c(1628,4,30,440),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #5
tmp = clean.tab(t(survey2[c(1628,4,30,447),]),
                entity = "")
out = rbind(out, tmp)



#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,30,470),]),
                entity = "")
out = rbind(out, tmp)

#Local organization #2
tmp = clean.tab(t(survey2[c(1628,4,30,473),]),
                entity = "")
out = rbind(out, tmp)















#### COUNTRY 4 ####
#31 = country name 4
# 199 = country #2 in-country collaborators (free text)
tmp = clean.tab(t(survey2[c(1628,4,31,517),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,520),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,521),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,522),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,523),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,524),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,532),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,31,533),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,31,534),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,535),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,536),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,537),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,538),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,539),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,31,540),]),
                "International Research Institute")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,31,578),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,31,585),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #3
#tmp = clean.tab(t(survey2[c(1628,4,31,592),]),
#                entity = "")
#out = rbind(out, tmp)

#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,31,629),]),
                entity = "")
out = rbind(out, tmp)










#### COUNTRY 5 ####
tmp = clean.tab(t(survey2[c(1628,4,32,676),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,679),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,680),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,681),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,682),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,683),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,691),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,32,692),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,32,693),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,694),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,695),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,696),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,697),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,698),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,32,699),]),
                "International Research Institute")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,32,737),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,32,744),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #3
tmp = clean.tab(t(survey2[c(1628,4,32,751),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #4
tmp = clean.tab(t(survey2[c(1628,4,32,758),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #5
tmp = clean.tab(t(survey2[c(1628,4,32,765),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #6
#tmp = clean.tab(t(survey2[c(1628,4,32,772),]),
#                entity = "")
#out = rbind(out, tmp)


#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,32,788),]),
                entity = "")
out = rbind(out, tmp)









#### COUNTRY 6 ####
tmp = clean.tab(t(survey2[c(1628,4,33,835),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,838),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,839),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,840),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,841),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,842),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,850),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,33,851),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,33,852),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,853),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,854),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,855),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,856),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,857),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,33,858),]),
                "International Research Institute")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,33,896),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
tmp = clean.tab(t(survey2[c(1628,4,33,903),]),
                entity = "")
out = rbind(out, tmp)

# #Academic institution #3
# tmp = clean.tab(t(survey2[c(1628,4,32,751),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #4
# tmp = clean.tab(t(survey2[c(1628,4,32,758),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #5
# tmp = clean.tab(t(survey2[c(1628,4,32,765),]),
#                 entity = "")
# out = rbind(out, tmp)

#Academic institution #6
#tmp = clean.tab(t(survey2[c(1628,4,32,772),]),
#                entity = "")
#out = rbind(out, tmp)


#Local organization #1
tmp = clean.tab(t(survey2[c(1628,4,33,947),]),
                entity = "")
out = rbind(out, tmp)







#### COUNTRY 7 ####
tmp = clean.tab(t(survey2[c(1628,4,34,994),]),
                entity="")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,997),]),
                entity = "Ministry of Health")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,998),]),
                entity = "Ministry of Agriculture")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,999),]),
                "Ministry of Livestock")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1000),]),
                "Ministry of Forestry")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1001),]),
                "Ministry of Wildlife")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1009),]),
                "WHO Regional Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,34,1010),]),
                "WHO Country Office")
out = rbind(out, tmp)


tmp = clean.tab(t(survey2[c(1628,4,34,1011),]),
                "WHO Global Office")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1012),]),
                "National Public Health Institute")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1013),]),
                "Doctor's Without Borders")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1014),]),
                "UNICEF")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1015),]),
                "CDC")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1016),]),
                "US Embassy")
out = rbind(out, tmp)

tmp = clean.tab(t(survey2[c(1628,4,34,1017),]),
                "International Research Institute")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,34,1055),]),
                entity = "")
out = rbind(out, tmp)

#Academic institution #2
#tmp = clean.tab(t(survey2[c(1628,4,33,1062),]),
#                entity = "")
#out = rbind(out, tmp)

# #Academic institution #3
# tmp = clean.tab(t(survey2[c(1628,4,32,751),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #4
# tmp = clean.tab(t(survey2[c(1628,4,32,758),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #5
# tmp = clean.tab(t(survey2[c(1628,4,32,765),]),
#                 entity = "")
# out = rbind(out, tmp)

#Academic institution #6
#tmp = clean.tab(t(survey2[c(1628,4,32,772),]),
#                entity = "")
#out = rbind(out, tmp)


#Local organization #1
#tmp = clean.tab(t(survey2[c(1628,4,34,1106),]),
#                entity = "")
#out = rbind(out, tmp)




#### COUNTRY 8 ####
tmp = clean.tab(t(survey2[c(1628,4,35,1153),]),
                entity="")
out = rbind(out, tmp)

# # tmp = clean.tab(t(survey2[c(1628,4,34,997),]),
# #                 entity = "Ministry of Health")
# # out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,998),]),
#                 entity = "Ministry of Agriculture")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,999),]),
#                 "Ministry of Livestock")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1000),]),
#                 "Ministry of Forestry")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1001),]),
#                 "Ministry of Wildlife")
# out = rbind(out, tmp)

# tmp = clean.tab(t(survey2[c(1628,4,34,1009),]),
#                 "WHO Regional Office")
# out = rbind(out, tmp)
# 
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1010),]),
#                 "WHO Country Office")
# out = rbind(out, tmp)
# 
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1011),]),
#                 "WHO Global Office")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1012),]),
#                 "National Public Health Institute")
# out = rbind(out, tmp)

# tmp = clean.tab(t(survey2[c(1628,4,34,1013),]),
#                 "Doctor's Without Borders")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1014),]),
#                 "UNICEF")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1015),]),
#                 "CDC")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1016),]),
#                 "US Embassy")
# out = rbind(out, tmp)
# 
# tmp = clean.tab(t(survey2[c(1628,4,34,1017),]),
#                 "International Research Institute")
# out = rbind(out, tmp)

#Academic institution #1
#tmp = clean.tab(t(survey2[c(1628,4,34,1214),]),
#                entity = "")
#out = rbind(out, tmp)

#Academic institution #2
#tmp = clean.tab(t(survey2[c(1628,4,33,1062),]),
#                entity = "")
#out = rbind(out, tmp)

# #Academic institution #3
# tmp = clean.tab(t(survey2[c(1628,4,32,751),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #4
# tmp = clean.tab(t(survey2[c(1628,4,32,758),]),
#                 entity = "")
# out = rbind(out, tmp)
# 
# #Academic institution #5
# tmp = clean.tab(t(survey2[c(1628,4,32,765),]),
#                 entity = "")
# out = rbind(out, tmp)

#Academic institution #6
#tmp = clean.tab(t(survey2[c(1628,4,32,772),]),
#                entity = "")
#out = rbind(out, tmp)


#Local organization #1
#tmp = clean.tab(t(survey2[c(1628,4,34,1106),]),
#                entity = "")
#out = rbind(out, tmp)



#### COUNTRY 9 ####
# very few answers for country 9. No WHO nor any ministry collaborations
tmp = clean.tab(t(survey2[c(1628,4,36,1312),]),
                entity="")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,36,1373),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #2
#tmp = clean.tab(t(survey2[c(1628,4,36,1380),]),
#                entity = "")
#out = rbind(out, tmp)



#### COUNTRY 10 ####
# very few answers for country 10. No WHO nor any ministry collaborations
tmp = clean.tab(t(survey2[c(1628,4,37,1471),]),
                entity="")
out = rbind(out, tmp)

#Academic institution #1
tmp = clean.tab(t(survey2[c(1628,4,37,1532),]),
                entity = "")
out = rbind(out, tmp)


#Academic institution #2
#tmp = clean.tab(t(survey2[c(1628,4,36,1380),]),
#                entity = "")
#out = rbind(out, tmp)



#### write output ####
write.table(out, file=OUTFILE, sep="\t", col.names=T, row.names=F, quote=F)









#### CUT ####
# record.id.tab = data.frame("record_id" = c(1,2,3,4,5,6,7,8,9,10),
#                            "RC_name" = c("CREATE-NEO",
#                                          "WAC-EID",
#                                          "EEIDI",
#                                          "UWARN",
#                                          "PICREID",
#                                          "A2CARES",
#                                          "CREID-ESP",
#                                          "EID-SEARCH",
#                                          "WARN-ID",
#                                          "CREID-ECA"))


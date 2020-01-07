options(stringsAsFactors = FALSE)
library(stringr)
library(stringr)

fifa_data <- read.csv2("https://raw.githubusercontent.com/JaseZiv/FIFA19-Analysis/master/data/data.csv",sep=',') 
fifa_data<-fifa_data[-1]

positions <- unique(fifa_data$Position)
gk <- "GK"
defs <- positions[str_detect(positions, "B$")]
mids <- positions[str_detect(positions, "M$")]
f1 <- positions[str_detect(positions, "F$")]
f2 <- positions[str_detect(positions, "S$")]
f3 <- positions[str_detect(positions, "T$")]
f4 <- positions[str_detect(positions, "W$")]
fwds <- c(f1, f2, f3, f4)

fifa_data <- fifa_data %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% defs, "DEF", ifelse(Position %in% mids, "MID", ifelse(Position %in% fwds, "FWD", "Unknown")))))

fifa_data_final<-fifa_data %>%
  select_if(is.numeric) %>% 
  select(-Special,-International.Reputation,-Weak.Foot,-Skill.Moves,-Jersey.Number,-Potential) %>% 
  cbind(fifa_data %>% select(Name,Club,PositionGroup)) %>%
  mutate(ID=as.character(ID))

fifa_sample<-sample_n(fifa_data_final,3000) 

fifa_sample<-fifa_sample[complete.cases(fifa_sample), ]
s1<-fifa_sample %>% sample_n(50) %>% mutate(Age=NA)
s2<-fifa_sample %>% sample_n(50) %>% mutate(Overall=NA)

fifa_sample<-fifa_sample %>% anti_join(s1,by="ID") %>% anti_join(s2,by="ID") %>% rbind(s1,s2)

write.csv2(fifa_sample,"./fifa_sample.csv", row.names = FALSE)

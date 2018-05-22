library(tidyverse)
library(stringr)
library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

url = "http://games.espn.com/ffl/api/v2/leagueSettings?leagueId=1314476&seasonId=2017"

raw.result = GET(url = url)
this.raw.content = rawToChar(raw.result$content)
this.content = fromJSON(this.raw.content)


raw.result$headers

games<-function(n)
{
  #home and away team scores
  scoreaway = lapply(n, "[",c(2))
  scorehome = lapply(n, "[",c(9))
  #who the away team is
  test = lapply(n, "[[",3)
  awayteam1 = lapply(test,"[",4)
  awayteam2 = lapply(test,"[",6)
  #who the home team is
  test2 = lapply(n, "[[",12)
  hometeam1 = lapply(test2,"[",4)
  hometeam2 = lapply(test2,"[",6)
  
  fhelper = data.frame(Away = paste(unlist(awayteam2), unlist(awayteam1)), Home = paste(unlist(hometeam2), unlist(hometeam1)), awayscore = unlist(scoreaway), homescore = unlist(scorehome), week = 1:13)
  return(fhelper)
}
fantasystart = games(
  c(
    this.content$leaguesettings$teams$`1`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`2`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`3`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`4`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`5`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`6`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`7`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`8`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`9`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`10`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`11`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`12`$scheduleItems$matchups,
    this.content$leaguesettings$teams$`13`$scheduleItems$matchups
    # this.content$leaguesettings$teams$`14`$scheduleItems$matchups,
    # this.content$leaguesettings$teams$`15`$scheduleItems$matchups,
    # this.content$leaguesettings$teams$`16`$scheduleItems$matchups
  )
)

fantasy =
fantasystart %>%
select(Away, awayscore, week) %>%
  mutate(Team = Away) %>%
  mutate(Score = awayscore) %>%
  mutate(Who = "For") %>%
  select(Team, Score, Who, week) %>%
  bind_rows(
    fantasystart %>%
      select(Home, homescore, week) %>%
      mutate(Team = Home) %>%
      mutate(Score = homescore) %>%
      mutate(Who = "For") %>%
      select(Team, Score, Who, week)
  ) %>%
  bind_rows(
    fantasystart %>%
      select(Away, homescore, week) %>%
      mutate(Team = Away) %>%
      mutate(Score = homescore) %>%
      mutate(Who = "Against") %>%
      select(Team, Score, Who, week)
   ) %>%
  bind_rows(
    fantasystart %>%
      select(Home, awayscore, week) %>%
      mutate(Team = Home) %>%
      mutate(Score = awayscore) %>%
      mutate(Who = "Against") %>%
      select(Team, Score, Who, week)
  )

#find plunger winner
plung =
fantasy %>%
  group_by(week) %>%
  summarise(Plunger = min(Score))

fantasy =
  fantasy %>%
  left_join(plung) %>%
  mutate(Plunger = ifelse(Plunger==Score,1,0))

fantasy =
fantasy %>%
  filter(Score > 0) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  mutate(Team = ifelse(Team == "5 Guns N' Rosas", "Team Guns N' Rosas", Team)) %>%
  mutate(Team = ifelse(Team == "3 Baye's Theorem", "Green Baye's Theorem", Team)) %>%
  mutate(Team = ifelse(Team == "16 Grymes", "Team Grymes", Team)) %>%
  group_by(Team, Who) %>%
  summarise(AverageScore = mean(Score),
            STDEV = sd(Score),
            max = max(Score),
            min = min(Score),
            Plunger = sum(Plunger/2)
            ) %>%
  mutate(PlungerLuck = ifelse(Who == "Against",Plunger,""), Plunger = ifelse(Who == "For", Plunger, "")) %>%
  ungroup() %>%
  arrange(desc(Who), AverageScore)

fantasy =
fantasy %>%
  mutate(dummy = 0) %>%
  mutate(ord = row_number())

fantasy$Team = factor(fantasy$Team, levels=unique(fantasy$Team[order(fantasy$ord)]), ordered=TRUE)
# 
# a=.5
# ggplot(data = fantasy, aes(y = AverageScore, x = dummy,ymin = AverageScore-2*STDEV, ymax = AverageScore+2*STDEV)) +
# # ggplot(data = fantasy, aes(y = AverageScore, x = dummy,ymin = min, ymax = max)) +
#   geom_point(aes(alpha = Who, shape = Who, size = a)) +
#   geom_point(aes(y = max, x = dummy, color = "red", alpha = Who, shape = Who, size = a)) +
#   geom_point(aes(y = min, x = dummy, color = "blue", alpha = Who, shape = Who, size = a)) +
#   guides(colour=FALSE, size = FALSE) +
#   geom_errorbar(aes(alpha = Who)) +
#   scale_alpha_discrete(range=c(0.3, 1)) +
#   scale_radius() +
#   geom_text(aes(y = 35, x = 0, label = Plunger, fontface = "bold")) +
#   geom_text(aes(y = 175, x = 0, label = PlungerLuck, fontface = "bold")) +
#   facet_grid(Team~.) +
#   # geom_text(hjust = 0, aes(label = Plunger)) +
#   theme(strip.text.y = element_text(angle = 0),
#         # axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         # axis.text.y=Plunger,
#         axis.ticks.y =element_blank()) +
#   xlab("Left - Plungers Earned : Right - Plungers Played Against") + ylab("Average Weekly Score") +
#   coord_flip() +
#   ggtitle("Is Your Team As Bad (or Good) As You Think It Is?") +
#   labs(caption = "Through Week 10") 

standing = data.frame(
  Team = c(paste(this.content$leaguesettings$teams$`1`$teamLocation, this.content$leaguesettings$teams$`1`$teamNickname),
           paste(this.content$leaguesettings$teams$`2`$teamLocation, this.content$leaguesettings$teams$`2`$teamNickname),
           paste(this.content$leaguesettings$teams$`3`$teamLocation, this.content$leaguesettings$teams$`3`$teamNickname),
           paste(this.content$leaguesettings$teams$`4`$teamLocation, this.content$leaguesettings$teams$`4`$teamNickname),
           paste(this.content$leaguesettings$teams$`5`$teamLocation, this.content$leaguesettings$teams$`5`$teamNickname),
           paste(this.content$leaguesettings$teams$`6`$teamLocation, this.content$leaguesettings$teams$`6`$teamNickname),
           paste(this.content$leaguesettings$teams$`7`$teamLocation, this.content$leaguesettings$teams$`7`$teamNickname),
           paste(this.content$leaguesettings$teams$`8`$teamLocation, this.content$leaguesettings$teams$`8`$teamNickname),
           paste(this.content$leaguesettings$teams$`9`$teamLocation, this.content$leaguesettings$teams$`9`$teamNickname),
           paste(this.content$leaguesettings$teams$`10`$teamLocation, this.content$leaguesettings$teams$`10`$teamNickname),
           paste(this.content$leaguesettings$teams$`11`$teamLocation, this.content$leaguesettings$teams$`11`$teamNickname),
           paste(this.content$leaguesettings$teams$`12`$teamLocation, this.content$leaguesettings$teams$`12`$teamNickname),
           paste(this.content$leaguesettings$teams$`13`$teamLocation, this.content$leaguesettings$teams$`13`$teamNickname),
           paste(this.content$leaguesettings$teams$`14`$teamLocation, this.content$leaguesettings$teams$`14`$teamNickname),
           paste(this.content$leaguesettings$teams$`15`$teamLocation, this.content$leaguesettings$teams$`15`$teamNickname),
           paste(this.content$leaguesettings$teams$`16`$teamLocation, this.content$leaguesettings$teams$`16`$teamNickname)),
  
  rank = c(this.content$leaguesettings$teams$`1`$record$overallStanding,
           this.content$leaguesettings$teams$`2`$record$overallStanding,
           this.content$leaguesettings$teams$`3`$record$overallStanding,
           this.content$leaguesettings$teams$`4`$record$overallStanding,
           this.content$leaguesettings$teams$`5`$record$overallStanding,
           this.content$leaguesettings$teams$`6`$record$overallStanding,
           this.content$leaguesettings$teams$`7`$record$overallStanding,
           this.content$leaguesettings$teams$`8`$record$overallStanding,
           this.content$leaguesettings$teams$`9`$record$overallStanding,
           this.content$leaguesettings$teams$`10`$record$overallStanding,
           this.content$leaguesettings$teams$`11`$record$overallStanding,
           this.content$leaguesettings$teams$`12`$record$overallStanding,
           this.content$leaguesettings$teams$`13`$record$overallStanding,
           this.content$leaguesettings$teams$`14`$record$overallStanding,
           this.content$leaguesettings$teams$`15`$record$overallStanding,
           this.content$leaguesettings$teams$`16`$record$overallStanding))

fantasy2 = fantasy %>%
  filter(Who == "For") %>%
  left_join(standing, by = "Team")

# fantasy$actualrank = c(16:1)

fantasy2 =
fantasy2 %>%
  mutate(actualrank = c(16:1)) %>%
  mutate(Luck = actualrank-rank)

library(scales)
# rescale(fantasy2$Luck, to = c(-100,100))

library(knitr)

# kable(fantasy[,c(1,5,6,7,8)])

fantasyfor = fantasy %>%
  filter(Who == "For") %>%
  mutate(MaxPointsScored = max, MinPointsScored = min, PlungersPlayedAgainst = PlungerLuck) %>%
  select(Team, AverageScore, MaxPointsScored, MinPointsScored, Plunger)
fantasyagainst = fantasy %>%
  filter(Who == "Against")%>%
  mutate(MaxPointsAgainst = max, MinPointsAgainst = min, PlungersPlayedAgainst = PlungerLuck) %>%
  select(Team, MaxPointsAgainst, MinPointsAgainst, PlungersPlayedAgainst) 
FantasyChartstart = fantasyfor %>%
  left_join(fantasyagainst, by = "Team")

FantasyChart = FantasyChartstart %>%
  mutate(Scored = AverageScore) %>%
  mutate(Max = MaxPointsScored) %>%
  mutate(Min = MinPointsScored) %>%
  mutate(MaxFoe = MaxPointsAgainst) %>%
  mutate(MinFoe = MinPointsAgainst) %>%
  mutate(PlungerFoe = PlungersPlayedAgainst)%>%
  select(Scored, Max,Min,Plunger,MaxFoe,MinFoe,PlungerFoe)

fantasystart

ggplot(data = fantasy2, aes(x = actualrank, y = rank)) +
  geom_point() +
  geom_text(aes(label = paste(Team,":" ,Luck), color = Luck), vjust =1.5, size=4) +
  scale_colour_gradient(low = "red", high = "black") +
  geom_text(aes(x = 8, y = 8, label = "Line of Luck"), angle = 43, nudge_y = .6) +
  geom_abline(slope = 1, intercept = 0) +
  ylim(c(1,17)) + xlim(c(1,17)) +
  xlab("Points Scored Rank") + ylab("Actual Standings") + ggtitle("Are You Over-Performing or Under-Performing?") +
  labs(caption = "The Luck Index measures how much better (or worse) your team is doing than what you should expect")

module Data where

import           Data.List (find)
import           Data.Text (Text)

data Team = Team { tId   :: Int
                 , tName :: Text
                 , tYear :: Text
                 , tDesc :: Text }

findTeam :: Int -> Maybe Team
findTeam n =
  find (\(Team i _ _ _) -> i == n) teamDatabase

teamDatabase :: [Team]
teamDatabase =
  [ Team 1
         "Gotham Girls Roller Derby"
         "2003"
         "Gotham Girls Roller Derby (GGRD) is a flat track roller derby league \
         \ based in New York City, New York. Founded in late 2003, Gotham      \
         \ Girls Roller Derby is also a founding member league of the Women's  \
         \ Flat Track Derby Association (WFTDA). The first flat track roller   \
         \ derby league in the metropolitan New York area, GGRD is one of the  \
         \ preeminent leagues in roller derby, having won five WFTDA           \
         \ Championships."
  , Team 2
         "Rose City Rollers"
         "2004"
         "The Rose City Rollers is a women's flat track roller derby league   \
         \ based in Portland, Oregon, operating as a 501(c)3 non-profit       \
         \ organization, and is a founding member of the Women's Flat Track   \
         \ Derby Association (WFTDA).[1] Established in 2004, the Rose City   \
         \ Rollers consists of four adult local home teams and two all-star   \
         \ travel teams that represent the league in competition with others  \
         \ as well as junior skaters on six home teams with a travel team for \
         \ Rose Petals (7-12) and a travel team for Rosebuds (12-17). Rose    \
         \ City's all-star travel team \"Wheels of Justice\" won the WFTDA    \
         \ Championships in 2015 and 2016."
  , Team 3
         "Steel City Roller Derby"
         "2006"
         "Steel City Roller Derby or SCRD, is a women's flat-track roller    \
         \ derby league based in Pittsburgh, Pennsylvania. Founded in 2006,  \
         \ the league celebrated its tenth anniversary in 2016. Steel City   \
         \ is a member of the Women's Flat Track Derby Association (WFTDA)." ]

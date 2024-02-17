url_string <- "https://www.sportsbookreview.com/betting-odds/ncaa-basketball/"
website <- read_html(url_string)
scoreboard <- html_elements(website, '#tbody-ncaab')


odds
away_tm <- xmlToDataFrame(xmlParse(xml_child(xml_child(xml_child(scoreboard[[1]], 1), 1), 2)))[1]
home_tm <- xmlToDataFrame(xmlParse(xml_child(xml_child(xml_child(scoreboard[[1]], 1), 1), 2)))[2]

away_odds <- xmlToDataFrame(xmlParse(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(scoreboard[[1]], 1), 3), 1), 3), 1), 1), 1), 2)))[1]
home_odds <- xmlToDataFrame(xmlParse(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(scoreboard[[1]], 1), 3), 1), 3), 1), 1), 1), 2)))[2]

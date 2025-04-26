##############################################
## Implemented by Suhyeon Jo                ##
## Title: Crawling_practice.R               ##
## Date : 2025.04.25                        ##
##                                          ##
## This project was created for practice.   ##
##                                          ##
##############################################

## Turn on R Selenium
## -------------------------------------
## For Windows
## cd "C:/r_selenium"
## java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
##
## For MacOS
## cd /Users/jjos/r_selenium
## java -Dwebdriver.gecko.driver="geckodriver" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

## Set Repositories
setRepositories(ind = 1:7)

## Import Library
## -------------------------------------
library(tidyverse)
library(rvest)
library(robotstxt)
library(stringr)
library(polite)
library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(writexl)
library(RSelenium)
library(purrr)
library(data.table)
library(readr)
library(tictoc)

setwd('/Users/jjos/Desktop/25-R01/Lectures/Data Mining/Code/Midterm_Practice')
getwd()


## Start!
## -------------------------------------
link <- "https://9db.jp/pokemongo/data/2734?id=81&genre=dmax&lv=5&o=1&pl=50&highdps_only=1&maxlv3_only=1"

## Pokemon DB Xpath: /html/body/div[4]/div[1]/div[1]/div[1]/div[4]/table[1]/tbody/tr[2]/td/span[2]/span/select
html <- read_html(link) %>% 
  html_node('body') %>% 
  html_node('table') %>% 
  html_nodes('tbody') %>% 
  html_nodes('tr:nth-child(2)') %>% 
  html_node('td') %>% 
  html_nodes('span')

View(html)

#### 포켓몬 정보를 추출할 수 없음. 주어진 페이지는 동적 페이지이기 때문

## Dynamic Web Crawling
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

url <- "https://9db.jp/pokemongo/data/2734?id=81&genre=dmax&lv=5&o=1&pl=50&highdps_only=1&maxlv3_only=1"
remDr$navigate(url)

html <- remDr$getPageSource()[[1]] %>% read_html()

## /html/body/div[4]/div[1]/div[1]/div[1]/div[4]/table[1]/tbody/tr[2]/td/span[2]
## /html/body/div[4]/div[1]/div[1]/div[1]/div[4]/table[1]/tbody/tr[2]/td/span[2]/span/select/option[1]
pokemon_db <- html %>% 
  html_node('table') %>% 
  html_nodes('tbody') %>% 
  html_nodes('tr:nth-child(2)') %>% ## nth-child()는 태그에 관계없이 n번째 하위 태그를 선택함
  html_nodes('td') %>% 
  html_nodes('span:nth-of-type(2)') %>% ## 여기서 nth-child() 쓰면, 이상한거 잡혀서 못긁음. 따라서, Xpath 구조를 알 때는, nth-of-type을 사용해야 함.
  html_node('span') %>% 
  html_node('select') %>% 
  html_nodes('option')

# View(pokemon_db)

df_pokemon <- tibble(
  ID       = pokemon_db %>% html_attr("value"),
  JAP_Name = pokemon_db %>% html_text()
)

View(df_pokemon) 

remDr$close()

## Load Dictionary
poke_dict <- as.tibble(fread("pokeNameDict.txt", sep = '\t', col.names = c("ID", "Kor", "Jap", "Eng")))
skill_dict <- as.tibble(fread("SkillNameDict.txt", sep = '\t', col.names = c("Num", "Jap", "Eng", "Kor")))

head(poke_dict) ; tail(poke_dict)
head(skill_dict) ; tail(skill_dict)

## 데이터 난리났음. 인코딩 해야 함.
poke_dict <- poke_dict %>%
  mutate(
    Kor = iconv(Kor, from="CP949", to="UTF-8"),
    Jap = iconv(Jap, from="CP949", to="UTF-8")
  )

skill_dict <- skill_dict %>%
  mutate(
    Kor = iconv(Kor, from="CP949", to="UTF-8"),
    Jap = iconv(Jap, from="CP949", to="UTF-8")
  )

head(poke_dict) ; tail(poke_dict)
head(skill_dict) ; tail(skill_dict)

## 여기서 그냥 끝내면 허수임. 일본어에 ?가 있네?
## ?가 있는거 함 찾아보자.
poke_dict %>% slice(1:50) %>% 
  filter(if_any(
    where(is.character), ~str_detect(., fixed("?"))
  ))

skill_dict %>% slice(1:50) %>% 
  filter(if_any(
    where(is.character), ~str_detect(., fixed("?"))
  ))

## 이게 뭔지 알아야하는데, 이건 노가다 해야지. 구글신을 활용하자!
## 포켓몬에서는 ー (한자 한 일)이 문제임.
## 스킬에서도 ー (한자 한 일)이 문제임.
## 추가로, 반각 -> 전각 작업도 함.

poke_dict <- poke_dict %>% 
  mutate(
    Jap = if_else(
      str_starts(Jap, fixed("カプ")),        
      str_replace(Jap, fixed("?"), "・"), 
      Jap
    ),
    Jap = str_replace_all(Jap, fixed("?"), "ー")
  )

skill_dict <- skill_dict %>% 
  mutate(Jap = str_replace_all(Jap, fixed("?"), "ー"))

poke_dict %>% slice(1:50) %>% filter(if_any(where(is.character), ~str_detect(., fixed("?"))))
skill_dict %>% slice(1:50) %>% filter(if_any(where(is.character), ~str_detect(., fixed("?"))))

poke_dict <- poke_dict %>% 
  mutate(across(
    where(is.character),
    ~ str_replace_all(., c("２" = "2", "Ｚ" = "Z", "Ｘ" = "X", "Ｙ" = "Y"))
  ))

## 클랜징 완료!
## 이제 df_pokemon에 한국어, 영어 이름 추가하기
head(df_pokemon)

aug_pokemon_df <- df_pokemon %>% 
  left_join(
    poke_dict %>% select(Jap, Kor, Eng),
    by = c("JAP_Name" = "Jap")
  ) %>% 
  select(ID, JAP_Name, Kor, Eng)

View(aug_pokemon_df)

## 결측치가 또 있음. 확인해봐야지
NA_poke_df <- aug_pokemon_df[rowSums(is.na(aug_pokemon_df)) > 0, ]
View(NA_poke_df)

## 이건 솔직히 포켓몬 사전 지식이 필요함
## 메가진화, 다이맥스, 테라스탈, 폼 형태 등의 요소들이 교수님이 주신 파일에 전혀 반영되지 않음.
## Notes 열 만들어서 형태 반영해야됨. 버려지는 포켓몬이 존재해서는 안 됨. 전부 다 처리함.

## キョダイマックスサダイシャDB에 오타가 있음.
## キョダイマックスサダイジャ 로 수정.
### シ -> ジ

#### 참고 페이지
## https://bulbapedia.bulbagarden.net/wiki/Main_Page
## https://pokemon.gameinfo.io/ko/pokemon/25-pikachu/pop-star
## https://pokemon.fandom.com/ko/wiki/%EB%8C%80%EB%AC%B8

final_pokemon_df <- aug_pokemon_df %>%
  mutate(
    Forms = case_when(
      str_starts(JAP_Name, "メガ") & str_detect(JAP_Name, fixed("X")) ~ "Mega_X",
      str_starts(JAP_Name, "メガ") & str_detect(JAP_Name, fixed("Y")) ~ "Mega_Y",
      str_starts(JAP_Name, "メガ") ~ "Mega",
      str_starts(JAP_Name, "ゲンシ") ~ "Primal",
      str_starts(JAP_Name, "キョダイマックス") & str_detect(JAP_Name, fixed("(いちげきのかた)")) ~ "Dynamax_Single_Strike_Style",
      str_starts(JAP_Name, "キョダイマックス") & str_detect(JAP_Name, fixed("(れんげきのかた)")) ~ "Dynamax_Rapid_Strike_Style",
      str_starts(JAP_Name, "キョダイマックス") ~ "Dynamax",
      str_starts(JAP_Name, "アーマード") ~ "Armored",
      str_starts(JAP_Name, "ホワイト") ~ "White",
      str_starts(JAP_Name, "ブラック") ~ "Black",
      str_detect(JAP_Name, fixed("(アローラ)")) ~ "Alola",
      str_detect(JAP_Name, fixed("(ガラル)")) ~ "Galar",
      str_detect(JAP_Name, fixed("(ヒスイ)")) ~ "Hisui",
      str_detect(JAP_Name, fixed("(オリジン)")) ~ "Origin",
      str_detect(JAP_Name, fixed("そらをとぶ")) ~ "Costume_2020",
      str_detect(JAP_Name, fixed("(パルデア単)")) ~ "Paldea_Combat",
      str_detect(JAP_Name, fixed("(パルデア炎)")) ~ "Paldea_Blaze",
      str_detect(JAP_Name, fixed("(パルデア水)")) ~ "Paldea_Aqua",
      str_detect(JAP_Name, fixed("かりゆし")) ~ "Kariyushi",
      str_detect(JAP_Name, fixed("キャプテン")) ~ "Horizons",
      str_detect(JAP_Name, fixed("マスクド・")) ~ "Vs_2019",
      str_detect(JAP_Name, fixed("ドクター・")) ~ "Doctor",
      str_detect(JAP_Name, fixed("(ハードロック)")) ~ "Rock_Star",
      str_detect(JAP_Name, fixed("(アイドル)")) ~ "Pop_Star",
      str_detect(JAP_Name, fixed("(パルデア)")) ~ "Paldea",
      str_detect(JAP_Name, fixed("(すなち)")) ~ "Sandy_Cloak",
      str_detect(JAP_Name, fixed("(くさき)")) ~ "Plant_Cloak",
      str_detect(JAP_Name, fixed("(ゴミ)")) ~ "Trash_Cloak",
      str_detect(JAP_Name, fixed("(にし)")) ~ "West",
      str_detect(JAP_Name, fixed("(ひがし)")) ~ "East",
      str_detect(JAP_Name, fixed("(春)")) ~ "Spring",
      str_detect(JAP_Name, fixed("(夏)")) ~ "Summer",
      str_detect(JAP_Name, fixed("(秋)")) ~ "Autumn",
      str_detect(JAP_Name, fixed("(冬)")) ~ "Winter",
      str_detect(JAP_Name, fixed("(たいよう)")) ~ "",
      str_detect(JAP_Name, fixed("(あまみず)")) ~ "Sunny",
      str_detect(JAP_Name, fixed("(ゆきぐも)")) ~ "Rainy",
      str_detect(JAP_Name, fixed("(ゆきぐも)")) ~ "Snowy",
      str_detect(JAP_Name, fixed("(ノーマル)")) ~ "Normal",
      str_detect(JAP_Name, fixed("(アタック)")) ~ "Attack",
      str_detect(JAP_Name, fixed("(ディフェンス)")) ~ "Defense",
      str_detect(JAP_Name, fixed("(スピード)")) ~ "Speed",
      str_detect(JAP_Name, fixed("(ネガ)")) ~ "Overcast",
      str_detect(JAP_Name, fixed("(ポジ)")) ~ "Sunshine",
      str_detect(JAP_Name, fixed("(ヒート)")) ~ "Heat",
      str_detect(JAP_Name, fixed("(ウォッシュ)")) ~ "Wash",
      str_detect(JAP_Name, fixed("(フロスト)")) ~ "Frost",
      str_detect(JAP_Name, fixed("(スピン)")) ~ "Fan",
      str_detect(JAP_Name, fixed("(カット)")) ~ "Mow",
      str_detect(JAP_Name, fixed("(アナザー)")) ~ "Altered ",
      str_detect(JAP_Name, fixed("(スカイ)")) ~ "Sky",
      str_detect(JAP_Name, fixed("(ランド)")) ~ "Land",
      str_detect(JAP_Name, fixed("(ほのお)")) ~ "Fire",
      str_detect(JAP_Name, fixed("(みず)")) ~ "Water",
      str_detect(JAP_Name, fixed("(くさ)")) ~ "Grass",
      str_detect(JAP_Name, fixed("(でんき)")) ~ "Electric",
      str_detect(JAP_Name, fixed("(こおり)")) ~ "Ice",
      str_detect(JAP_Name, fixed("(かくとう)")) ~ "Fighting",
      str_detect(JAP_Name, fixed("(どく)")) ~ "Poison",
      str_detect(JAP_Name, fixed("(じめん)")) ~ "Ground",
      str_detect(JAP_Name, fixed("(ひこう)")) ~ "Flying",
      str_detect(JAP_Name, fixed("(エスパー)")) ~ "Psychic",
      str_detect(JAP_Name, fixed("(むし)")) ~ "Bug",
      str_detect(JAP_Name, fixed("(いわ)")) ~ "Rock",
      str_detect(JAP_Name, fixed("(ゴースト)")) ~ "Ghost",
      str_detect(JAP_Name, fixed("(ドラゴン)")) ~ "Dragon",
      str_detect(JAP_Name, fixed("(あく)")) ~ "Dark",
      str_detect(JAP_Name, fixed("(はがね)")) ~ "Steel",
      str_detect(JAP_Name, fixed("(フェアリー)")) ~ "Fairy",
      str_detect(JAP_Name, fixed("(あかすじ)")) ~ "Red-Striped",
      str_detect(JAP_Name, fixed("(あおすじ)")) ~ "Blue-Striped",
      str_detect(JAP_Name, fixed("(しろすじ)")) ~ "White-Striped",
      str_detect(JAP_Name, fixed("(ダルマ)")) ~ "Zen_Mode",
      str_detect(JAP_Name, fixed("(ガラル・ノーマル)")) ~ "Galarian_Standard_Mode",
      str_detect(JAP_Name, fixed("(ガラル・ダルマ)")) ~ "Galarian_Zen_Mode",
      str_detect(JAP_Name, fixed("(オス)")) ~ "Male",
      str_detect(JAP_Name, fixed("(メス)")) ~ "Female",
      str_detect(JAP_Name, fixed("(けしん)")) ~ "Incarnate_Forme",
      str_detect(JAP_Name, fixed("(れいじゅう)")) ~ "Therian_Forme",
      str_detect(JAP_Name, fixed("(いつも)")) ~ "Ordinary",
      str_detect(JAP_Name, fixed("(かくご)")) ~ "Resolute",
      str_detect(JAP_Name, fixed("(ボイス)")) ~ "Aria",
      str_detect(JAP_Name, fixed("(ステップ)")) ~ "Pirouette",
      str_detect(JAP_Name, fixed("(ブレイズ)")) ~ "Burn_Drive",
      str_detect(JAP_Name, fixed("(フリーズ)")) ~ "Chill_Drive",
      str_detect(JAP_Name, fixed("(アクア)")) ~ "Douse_Drive",
      str_detect(JAP_Name, fixed("(イナズマ)")) ~ "Shock_Drive",
      str_detect(JAP_Name, fixed("(シールド)")) ~ "Shield",
      str_detect(JAP_Name, fixed("(ブレード)")) ~ "Blade",
      str_detect(JAP_Name, fixed("(ちいさい)")) ~ "Small",
      str_detect(JAP_Name, fixed("(おおきい)")) ~ "Large",
      str_detect(JAP_Name, fixed("(とくだい)")) ~ "Super",
      str_detect(JAP_Name, fixed("(50％)")) ~ "50%",
      str_detect(JAP_Name, fixed("(10％)")) ~ "10%",
      str_detect(JAP_Name, fixed("(パーフェクト)")) ~ "Complete",
      str_detect(JAP_Name, fixed("(戒)")) ~ "Confined",
      str_detect(JAP_Name, fixed("(解)")) ~ "Unbound",
      str_detect(JAP_Name, fixed("(めらめら)")) ~ "Baile",
      str_detect(JAP_Name, fixed("(ぱちぱち)")) ~ "Pom-Pom",
      str_detect(JAP_Name, fixed("(ふらふら)")) ~ "Pa'u",
      str_detect(JAP_Name, fixed("(まいまい)")) ~ "Sensu",
      str_detect(JAP_Name, fixed("(たそがれに進化)")) ~ "One_Tempo",
      str_detect(JAP_Name, fixed("(まひる)")) ~ "Midday",
      str_detect(JAP_Name, fixed("(まよなか)")) ~ "Midnight",
      str_detect(JAP_Name, fixed("(たそがれ)")) ~ "Dusk",
      str_detect(JAP_Name, fixed("(単独)")) ~ "Solo",
      str_detect(JAP_Name, fixed("(魚群)")) ~ "School",
      str_detect(JAP_Name, fixed("(ハイ)")) ~ "Amped",
      str_detect(JAP_Name, fixed("(ロー)")) ~ "Low_Key",
      str_detect(JAP_Name, fixed("(アイス)")) ~ "Ice_Face",
      str_detect(JAP_Name, fixed("(ナイス)")) ~ "Noice_Face",
      str_detect(JAP_Name, fixed("(まんぷく)")) ~ "Full_Belly",
      str_detect(JAP_Name, fixed("(はらぺこ)")) ~ "Hangry",
      str_detect(JAP_Name, fixed("(れきせんのゆうしゃ)")) ~ "Hero_of_Many_Battles",
      str_detect(JAP_Name, fixed("(けんのおう)")) ~ "Crowned_Sword",
      str_detect(JAP_Name, fixed("(たてのおう)")) ~ "Crowned_Shield",
      str_detect(JAP_Name, fixed("(いちげきのかた)")) ~ "Single_Strike_Style",
      str_detect(JAP_Name, fixed("(れんげきのかた)")) ~ "Rapid_Strike_Style",
      str_detect(JAP_Name, fixed("(はくばじょうのすがた)")) ~ "Ice_Rider",
      str_detect(JAP_Name, fixed("(こくばじょうのすがた)")) ~ "Shadow_Rider",
      str_detect(JAP_Name, fixed("(アカツキ)")) ~ "Bloodmoon",
      str_detect(JAP_Name, fixed("(りゅうせい)")) ~ "Meteor",
      str_detect(JAP_Name, fixed("(コア)")) ~ "Red_Core",
      str_detect(JAP_Name, fixed("(たそがれのたてがみ)")) ~ "Dusk_Mane",
      str_detect(JAP_Name, fixed("(あかつきのつばさ)")) ~ "Dawn_Wings",
      str_detect(JAP_Name, fixed("(ウルトラネクロズマ)")) ~ "Ultra",
      str_detect(JAP_Name, fixed("(ナイーブ)")) ~ "Zero",
      str_detect(JAP_Name, fixed("(マイティ)")) ~ "Hero",
      str_detect(JAP_Name, fixed("(とほ)")) ~ "Roaming",
      str_detect(JAP_Name, fixed("(はこ)")) ~ "Chest",
      str_detect(JAP_Name, fixed("(がんさく)")) ~ "Counterfeit",
      str_detect(JAP_Name, fixed("(しんさく)")) ~ "Artisan",
      str_detect(JAP_Name, fixed("(がんさく)")) ~ "Unmarkable",
      str_detect(JAP_Name, fixed("(しんさく)")) ~ "Masterpiece",
      str_detect(JAP_Name, fixed("(いどのめん)")) ~ "Wellspring_Mask",
      str_detect(JAP_Name, fixed("(かまどのめん)")) ~ "Hearthflame_Mask",
      str_detect(JAP_Name, fixed("(いしずえのめん)")) ~ "Cornerstone_Mask",
      str_detect(JAP_Name, fixed("(みどりのめん)")) ~ "Teal_Mask",
      TRUE ~ NA_character_
    ),
    JAP_Name = case_when(
      str_detect(JAP_Name, fixed("パフュートン♂")) ~ "パフュートン",
      str_detect(JAP_Name, fixed("パフュートン♀")) ~ "パフュートン",
      str_detect(JAP_Name, fixed("サダイシャ")) ~ "サダイジャ",
      TRUE ~ JAP_Name
    ),
    JAP_Name = JAP_Name %>% 
      str_remove("^メガ") %>% 
      str_remove("^キョダイマックス") %>% 
      str_remove("^アーマード") %>% 
      str_remove("^ゲンシ") %>% 
      str_remove("^ホワイト") %>% 
      str_remove("^ブラック") %>% 
      str_remove("\\(.*$") %>%   
      str_remove_all(fixed("マスクド・")) %>% 
      str_remove_all(fixed("ドクター・")) %>% 
      str_remove_all(fixed("X")) %>% 
      str_remove_all(fixed("Y")) %>% 
      str_remove_all(fixed("そらをとぶ")) %>% 
      str_remove_all(fixed("かりゆし"))
  )

NA_poke_df <- final_pokemon_df %>%
  filter(
    if_all(3:5, is.na)
  )

head(NA_poke_df)
View(final_pokemon_df)

## 모든 포켓몬 데이터 수정 완료
## 수정 후, 후처리 작업이 필요함 (빠진 부분 다시 채우기 등)

filled_pokemon_df <- final_pokemon_df %>%
  left_join(
    poke_dict %>% 
      select(Jap, Kor, Eng) %>% 
      rename(
        JAP_Name  = Jap,
        Kor_dict  = Kor,
        Eng_dict  = Eng
      ),
    by = "JAP_Name"
  ) %>%
  mutate(
    Kor = coalesce(Kor, Kor_dict),
    Eng = coalesce(Eng, Eng_dict)
  ) %>%
  select(-Kor_dict, -Eng_dict)

## 후처리 작업 후 결측치 다시 확인
NA_poke_df <- filled_pokemon_df[rowSums(is.na(filled_pokemon_df[1:4])) > 0, ]
head(NA_poke_df)

head(filled_pokemon_df) ; tail(filled_pokemon_df)
View(filled_pokemon_df)

## DB에 존재하지 않는 포켓몬
missing_poke <- poke_dict %>%
  select(Eng) %>%                                  
  distinct() %>%                                   
  anti_join(
    filled_pokemon_df %>% select(Eng) %>% distinct(),
    by = "Eng"
  )
missing_poke

## 이제 각 포켓몬마다 카운터 찾아야함.
## 재미있는 사실 하나 더 -> DB에 존재하지만 접근할 수 없는 포켓몬들이 많음. 이것들도 수집 못 함.
base <- 'https://9db.jp/pokemongo/data/2734?id='
query <- '&genre=dmax&lv=5&o=1&pl=50&highdps_only=1&maxlv3_only=1'

## filled_pokemon_df의 ID를 기반으로 페이지 돈다.
## API 기반으로 수행 가능. Network / Fetch/XHR
## https://9db.jp/pokemongo/data/2734?id={base_id}&genre=dmax&lv=5&o=1&pl=50&highdps_only=1&maxlv3_only=1

## API를 보니, 포켓몬 기술이 id로 저장되어 있음. 기술을 빼오는 페이지가 있으니까 여기서 크롤링, 셀레니움은 너무 느려서 못써!
## 페이지에서 추출할 수 있는 기술은 311개로, 제공된 기술보다 적다. 하지만, 이 데이터만으로 완벽한 결과를 만들 수 있으니 사용 가능
## 포켓몬 페이지 DB에 존재하지 않는 기술이 크롤링하는 페이지에서 나타날 수 없기 때문
## https://9db.jp/pokemongo/data/1427

remDr$open()
skill_url <- "https://9db.jp/pokemongo/data/1427"
remDr$navigate(skill_url)

## /html/body/div[3]/div[1]/div[1]/div[1]/div[3]/div[5]/div/div/table/tbody
skill1_html <- remDr$getPageSource()[[1]] %>% read_html()
skill_node <- skill1_html %>% 
  html_nodes('table') %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_node("td:nth-child(1)")

normal_skills <- tibble(
  jp_name = skill_node %>% html_node("a") %>% html_text(),
  ID      = skill_node %>% html_node("a") %>% html_attr("href") %>% str_extract("\\d+$"),
  en_name = skill_node %>% html_node("span.wiki_hint") %>% html_text(trim = TRUE)
)

btn_charged <- remDr$findElement(
  using = "css selector",
  value = 'td.wiki_select a[data-id="class"][data-val="2"]'
)
btn_charged$clickElement()

skill2_html <- remDr$getPageSource()[[1]] %>% read_html()

skill_node <- skill2_html %>% 
  html_nodes('table') %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_node("td:nth-child(1)")

charged_skills <- tibble(
  jp_name = skill_node %>% html_node("a")        %>% html_text(),
  ID      = skill_node  %>% html_node("a") %>% html_attr("href") %>% str_extract("\\d+$"),
  en_name = skill_node  %>% html_node("span.wiki_hint") %>% html_text(trim = TRUE)
)

normal_trimmed  <- normal_skills  %>% slice(-1:-16)
charged_trimmed <- charged_skills %>% slice(-1:-16)

skills_df <- bind_rows(
  normal_trimmed  %>% mutate(type = "normal"),
  charged_trimmed %>% mutate(type = "charged")
) %>%
  arrange(as.integer(ID))

View(skills_df)


remDr$close()

## pokemon ID로 페이지를 순회하면서, API로 정보를 가져온다. API의 counter_list의 최상단 정보만 가져오면 된다.
## 이후, skill id는 저장한 df에서 찾아 붙인다. 에러 발생 가능성이 있기 때문에, err_list에 에러 쿼리를 저장한 후, 다음으로 진행한다.

DATA_DIR <- '/Users/jjos/Desktop/25-R01/Lectures/Data Mining/Code/Midterm_Practice/Poke_Counter'
err_list <- character()
tic.clearlog()

## 실행시간 약 40분
tic('Query Timer')
for (i in filled_pokemon_df$ID) {
  
  cat('Crawling ', i, '…')

  tryCatch({
    link <- paste0(base, i, query)
    
    res <- GET(link,
               add_headers(
                 `X-Requested-With` = "XMLHttpRequest",
                 Accept             = "application/json, text/javascript, */*; q=0.01"
               ))
    
    payload      <- fromJSON(content(res, "text", encoding = "UTF-8"))
    counter_list <- as_tibble(payload$data_list$counter_list)
    
    processed <- counter_list %>% 
      distinct(at_id, .keep_all = TRUE) %>% 
      slice(1:20) %>% 
      rename(
        Poke_ID   = at_id,
        Skill1_ID = at_qk_id,
        Skill2_ID = at_sp_id,
        DPS       = dps_max,
        Shield_1  = dwall0,
        Shield_2  = dwall3,
        MaxDPS    = max_damage
      ) %>% 
      select(Poke_ID, Skill1_ID, Skill2_ID, DPS, Shield_1, Shield_2, MaxDPS) %>% 
      mutate(
        Rank      = row_number(),
        Poke_ID   = as.character(Poke_ID),
        Skill1_ID = as.character(Skill1_ID),
        Skill2_ID = as.character(Skill2_ID),
        Shield_1  = round(Shield_1, 1),
        Shield_2  = round(Shield_2, 1)
      ) %>%
      left_join(filled_pokemon_df %>% select(ID, Eng), 
                by = c("Poke_ID"   = "ID")) %>%
      rename(Poke_Name = Eng) %>%
      left_join(skills_df %>% select(ID, en_name), 
                by = c("Skill1_ID" = "ID")) %>%
      rename(Skill1 = en_name) %>%
      left_join(skills_df %>% select(ID, en_name), 
                by = c("Skill2_ID" = "ID")) %>%
      rename(Skill2 = en_name) %>%
      select(Rank, Poke_Name, Skill1, Skill2, MaxDPS, DPS, Shield_1, Shield_2)
    
    eng  <- filled_pokemon_df %>% 
      filter(ID == as.character(i)) %>% 
      pull(Eng)
    form <- filled_pokemon_df %>% 
      filter(ID == as.character(i)) %>% 
      pull(Forms)
    
    if (!is.na(form) && nzchar(form)) {
      output_name <- paste0(eng, "_", form)
    } else {
      output_name <- eng
    }
    output_name <- str_replace_all(output_name, "\\s+", "_")
    
    out_file <- file.path(DATA_DIR, paste0(output_name, ".txt"))
    write_tsv(processed, out_file)
    
    cat(' Done!\n')
    Sys.sleep(0.5)
    
  }, error = function(e) {
    cat(' ERROR: ', e$message, '\n')
    err_list <<- c(err_list, as.character(i))
  })
}

toc('Query Timer')
print(tic.logformat())

## Error list 확인
print(err_list)












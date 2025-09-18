# new버전
# /AED_MAP/global.R (최종 수정본)

# --- 1. 필수 패키지 로드 ---
library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinycssloaders)
library(cicerone)
library(rclipboard)
library(sf)
library(leaflet)
library(leaflet.extras)
library(echarts4r)
library(DT)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(commonmark)
library(waiter)
library(readr)
library(rmarkdown)
library(tidyr)


# --- 2. 앱 공통 변수 및 객체 정의 ---
today <- Sys.Date()

# Docker 환경 경로 접두사 (필요 시 수정)
DOCKER_PREFIX <- if (Sys.getenv("IS_DOCKER") == "true") "/app/" else ""

status_color_map <- tibble::tibble(
  status_detail = factor(
    c("배터리 만료", "패치 만료", "장비 만료", "점검 필요", "24시간 사용가능", "정상", "기타(사용불가)", "위치정보 누락"),
    levels = c("배터리 만료", "패치 만료", "장비 만료", "점검 필요", "24시간 사용가능", "정상", "기타(사용불가)", "위치정보 누락")
  ),
  color_hex = c("#E53935", "#FDD835", "#8E24AA", "#FB8C00", "#43A047", "#1E88E5", "#BDBDBD", "#757575")
)

keyword_rules <- tibble::tribble(
  ~keyword,      ~target_class_pattern, ~boost_score,
  "시청",        "시도청사",               5, "도청", "시도청사", 5,
  "구청",        "시군구청사",             5, "군청", "시군구청사", 5,
  "경찰서",      "경찰관서",               5, "파출소", "경찰관서", 5, "지구대", "경찰관서", 5,
  "소방서",      "소방서",                 5, "119", "소방서", 5, "안전센터", "소방서", 5,
  "보건소",      "보건소",                 5,
  "아파트",      "공동주택",               5,
  "지하철",      "철도시설",               5, "역", "철도시설", 4,
  "대학교",      "교육시설",               4, "초등학교", "교육시설", 4, "중학교", "교육시설", 4, "고등학교", "교육시설", 4,
  "아파트",      "시도청사",              -100, "아파트", "철도시설", -100,
  "역",          "시도청사",              -100, "역", "공동주택", -100,
  "학교",        "공동주택",              -100, "학교", "시도청사", -100
)

checkbox_filter_map <- tibble::tribble(
  ~choice,                ~filter_pattern,
  "공공/행정기관",        "기관|청사",
  "의료기관",             "의료기관|보건",
  "교육시설",             "교육시설|학교",
  "공동주택/주거시설",    "주택|주거",
  "교통시설",             "교통|철도|공항|선박|터미널",
  "사업장",               "사업장",
  "기타 다중이용시설",   "다중이용시설|문화|체육|숙박|판매"
)

# global.R 파일의 '--- 3. 외부 데이터 및 모델 로드 ---' 섹션을 아래 코드로 교체하세요.

# --- 3. 외부 데이터 및 모델 로드 (안정성 강화) ---

# 3-1. AI 분류 모델
# [수정] 프로젝트 폴더 내의 파일을 직접 참조하도록 경로 수정
model_path <- "aed_classification_model.RData" 
if (file.exists(model_path)) {
  load(model_path)
  print("✅ AI 분류 모델 로딩 성공.")
} else {
  print("⚠️ 경고: AI 분류 모델 파일을 찾을 수 없습니다.")
  aed_classification_model <- NULL
  preprocess_text <- function(text) { text } # 임시 함수
}

# 3-2. FAQ 데이터
faq_data <- tryCatch({
  # [수정] 프로젝트 폴더 내의 파일을 직접 참조하도록 경로 수정
  read.csv("./faq_data.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
}, error = function(e) {
  print(paste("⚠️ [오류] faq_data.csv 파일 로딩 실패:", e$message))
  return(data.frame(keywords = character(), answer = character())) # 빈 데이터프레임 반환
})

# 3-3. AED 매뉴얼 텍스트
aed_manual_paragraphs <- tryCatch({
  # [수정] 프로젝트 폴더 내의 파일을 직접 참조하도록 경로 수정
  manual_text <- readLines("aed_manual.txt", encoding = "UTF-8")
  unlist(strsplit(paste(manual_text, collapse = "\n"), "\n\n+"))
}, error = function(e) {
  print(paste("⚠️ [오류] aed_manual.txt 파일 로딩 실패:", e$message))
  "매뉴얼 파일을 찾을 수 없습니다." # 오류 발생 시 기본 텍스트 반환
})


# --- 4. 공용 UI 객체 정의 ---
chat_modal_ui <- modalDialog(
  title = tagList(bs_icon("robot"), "AED 지침 기반 Q&A 챗봇"),
  size = "l",
  easyClose = TRUE,
  div(
    id = "modal-chat-container",
    style = "max-height: 60vh; overflow-y: auto; background-color: var(--light-bg-1); border: 1px solid var(--light-border); border-radius: 8px; padding: 15px;",
    div(
      id = "modal-chat-history-content",
      div(
        id = "welcome-message-modal",
        class = "chat-message bot-message",
        div(class = "message-bubble",
            tagList(
              p("안녕하세요! AED 설치 및 관리 지침에 대해 무엇이든 물어보세요."),
              p("예시:", style="font-weight:bold; margin-bottom: 5px;"),
              tags$ul(style="margin-top: 0; padding-left: 20px;",
                      tags$li("설치 의무 대상은?"),
                      tags$li("매월 1회 점검 방법 알려줘"),
                      tags$li("관리자 교육은 어떻게 해?")
              )
            )
        )
      )
    )
  ),
  div(
    class = "chat-input-area",
    style = "margin-top: 15px; display: flex; gap: 10px; align-items: center;",
    textAreaInput("modal_chat_query_input",
                  label = NULL,
                  placeholder = "질문을 입력하세요... (Shift+Enter로 줄바꿈)",
                  rows = 2,
                  width = "100%"
    ),
    actionButton("modal_chat_submit_btn",
                 label = "전송",
                 icon = icon("paper-plane"),
                 class = "btn-primary",
                 style = "height: 58px; white-space: nowrap;"
    )
  ),
  footer = modalButton("닫기")
)

# --- 5. 공용 함수 정의 ---

# 5-1. 스마트 분류 어시스턴트 함수
get_classification_info <- function(query, aed_data) {
  
  query_lower <- tolower(query)
  
  # 1. 키워드 규칙 우선 검사
  for (i in 1:nrow(keyword_rules)) {
    rule <- keyword_rules[i, ]
    if (grepl(rule$keyword, query_lower)) {
      
      best_match_class_df <- aed_classification_model$class_examples %>%
        filter(grepl(rule$target_class_pattern, full_class))
      
      if (nrow(best_match_class_df) > 0) {
        best_match_class <- best_match_class_df %>% slice(1) %>% pull(full_class)
        duty <- grepl("구비의무기관", best_match_class)
        duty_text <- if(duty) rule$target_class_pattern else "응급의료에 관한 법률 시행령 제2조에 해당하지 않음"
        
        classification_parts <- unlist(str_split(best_match_class, " > "))
        return(
          list(
            duty = duty, duty_text = duty_text,
            c1 = ifelse(length(classification_parts) >= 1, classification_parts[1], NA_character_),
            c2 = ifelse(length(classification_parts) >= 2, classification_parts[2], NA_character_),
            c3 = ifelse(length(classification_parts) >= 3, classification_parts[3], NA_character_),
            c4 = ifelse(length(classification_parts) >= 4, classification_parts[4], NA_character_),
            suggestion_text = paste0("'", rule$keyword, "' 키워드 규칙에 따라 자동 분류되었습니다.")
          )
        )
      }
    }
  }
  
  # 2. 규칙에 맞지 않을 때만 AI 유사도 분석 실행
  duty <- FALSE
  duty_text <- "응급의료에 관한 법률 시행령 제2조에 해당하지 않음"
  
  if (grepl("500세대|아파트", query_lower)) { duty <- TRUE; duty_text <- "500세대 이상의 공동주택" }
  else if (grepl("공항|철도|터미널|여객선", query_lower)) { duty <- TRUE; duty_text <- "공항, 철도역사, 여객선 등 운송시설" }
  else if (grepl("300인|사업장", query_lower)) { duty <- TRUE; duty_text <- "상시근로자 300명 이상의 사업장" }
  else if (grepl("5천명|운동장|경기장|체육관", query_lower)) { duty <- TRUE; duty_text <- "5천명 이상 수용 가능한 운동장/체육관" }
  else if (grepl("시청|도청|구청|군청|경찰서|소방서|보건소|공공기관", query_lower)) { duty <- TRUE; duty_text <- "공공보건의료기관, 국가 및 지자체 청사" }
  
  processed_query <- preprocess_text(query)
  query_words <- unlist(str_split(processed_query, " "))
  
  best_match <- aed_classification_model$class_examples %>%
    rowwise() %>%
    mutate(
      union_size = length(union(unlist(examples), query_words)),
      intersect_size = length(intersect(unlist(examples), query_words)),
      similarity = if (union_size == 0) 0 else intersect_size / union_size
    ) %>%
    ungroup() %>%
    filter(similarity == max(similarity)) %>%
    slice(1)
  
  similar_entries <- aed_data %>%
    filter(grepl(paste(query_words, collapse = "|"), `설치기관명`)) %>%
    count(`분류2`, sort = TRUE) %>%
    slice(1)
  
  suggestion_text <- if (nrow(similar_entries) > 0 && !is.na(similar_entries$분류2)) {
    paste0("유사한 명칭의 장소는 주로 '", similar_entries$분류2, "'로 분류되었습니다.")
  } else { NA_character_ }
  
  classification_parts <- unlist(str_split(best_match$full_class, " > "))
  
  return(
    list(
      duty = duty, duty_text = duty_text,
      c1 = ifelse(length(classification_parts) >= 1, classification_parts[1], NA_character_),
      c2 = ifelse(length(classification_parts) >= 2, classification_parts[2], NA_character_),
      c3 = ifelse(length(classification_parts) >= 3, classification_parts[3], NA_character_),
      c4 = ifelse(length(classification_parts) >= 4, classification_parts[4], NA_character_),
      suggestion_text = suggestion_text
    )
  )
}

# 5-2. Q&A 답변 생성 함수
generate_bot_response <- function(query) {
  if (!exists("faq_data") || is.null(faq_data) || nrow(faq_data) == 0) {
    return("죄송합니다. 답변 데이터베이스(faq_data.csv)를 불러오는 데 문제가 발생했습니다.")
  }
  
  query_clean <- tolower(trimws(query))
  if (nchar(query_clean) < 2) { return("검색어를 최소 두 글자 이상 입력해주세요.") }
  
  for (i in 1:nrow(faq_data)) {
    keywords <- tolower(faq_data$keywords[i])
    if (any(sapply(strsplit(keywords, "\\|")[[1]], function(k) grepl(trimws(k), query_clean)))) {
      return(commonmark::markdown_html(faq_data$answer[i]))
    }
  }
  
  found_indices <- which(grepl(query_clean, tolower(aed_manual_paragraphs)))
  if (length(found_indices) > 0) {
    results_to_show <- head(found_indices, 2)
    response_parts <- lapply(results_to_show, function(idx) {
      paragraph <- aed_manual_paragraphs[idx]
      highlighted_text <- gsub(paste0("(", query_clean, ")"), "**\\1**", paragraph, ignore.case = TRUE)
      return(paste0("📄 **문서 내용:**\n\n", highlighted_text))
    })
    final_response <- paste(
      paste0("'", query, "'에 대한 일반 지침 내용을 찾았습니다."),
      paste(response_parts, collapse = "\n\n---\n\n"),
      sep = "\n\n"
    )
    return(commonmark::markdown_html(final_response))
  }
  
  return("죄송합니다. 입력하신 질문에 대한 답변을 찾을 수 없습니다. 키워드를 바꿔서 다시 질문해주세요.")
}


print("✅ global.R 파일 실행 완료.")
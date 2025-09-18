# =================================================================
# Smart AED Monitoring Dashboard - BATCH Data Preprocessing Script
# [최종 안정화 버전 2.0]
# =================================================================

# --- 1. 필수 패키지 로드 (dplyr을 마지막에 로드하여 충돌 방지) ---
library(shiny)
library(tidyr)
library(sf)
library(lubridate)
library(stringr)
library(purrr)
library(htmltools)
library(scales)
library(dplyr) # dplyr을 가장 마지막에 로드
library(lwgeom)


# --- 2. 처리할 데이터 목록 정의 ---
process_list <- list(
  list(month = "01", aed_path = "/Users/inhaklee/AED_MAP/aed1.csv", output_path = "preprocessed_01.RData"),
  list(month = "02", aed_path = "/Users/inhaklee/AED_MAP/aed2.csv", output_path = "preprocessed_02.RData"),
  list(month = "03", aed_path = "/Users/inhaklee/AED_MAP/aed3.csv", output_path = "preprocessed_03.RData"),
  list(month = "04", aed_path = "/Users/inhaklee/AED_MAP/aed4.csv", output_path = "preprocessed_04.RData"),
  list(month = "05", aed_path = "/Users/inhaklee/AED_MAP/aed5.csv", output_path = "preprocessed_05.RData"),
  list(month = "06", aed_path = "/Users/inhaklee/AED_MAP/aed6.csv", output_path = "preprocessed_06.RData"),
  list(month = "07", aed_path = "/Users/inhaklee/AED_MAP/aed7.csv", output_path = "preprocessed_07.RData"),
  list(month = "08", aed_path = "/Users/inhaklee/AED_MAP/aed8.csv", output_path = "preprocessed_08.RData")
)

# --- 3. 공통 데이터 로딩 ---
cat("공통 데이터를 로딩합니다...\n")
hangjeongdong_geojson_path <- "/Users/inhaklee/shiny_aed_dashboard/HangJeongDong_ver20250401.geojson"
hospital_file_path <- "/Users/inhaklee/shiny_aed_dashboard/hospital_list_2025-06-18.csv"
floating_pop_path <- "/Users/inhaklee/shiny_aed_dashboard/floating_pop_with_location.csv"

options(sf_use_s2 = FALSE)
hangjeongdong_sf_raw <- st_read(hangjeongdong_geojson_path, quiet = TRUE) %>% 
  st_make_valid() %>% 
  st_transform(4326)

hospitals_raw <- read.csv(hospital_file_path, fileEncoding = "UTF-8", na.strings = c("NA", "", " ")) %>%
  rename(lon = `WGS84좌표.경도.`, lat = `WGS84좌표.위도.`)
floating_pop_raw <- read.csv(floating_pop_path, fileEncoding = "UTF-8")

sido_pop_area <- tribble(~name_kor, ~population, ~area_km2, "서울특별시", 9386034, 605.2, "부산광역시", 3293362, 770.4, "대구광역시", 2375004, 883.5, "인천광역시", 3001378, 1065.6, "광주광역시", 1419237, 501.1, "대전광역시", 1445623, 539.4, "울산광역시", 1104326, 1062.2, "세종특별자치시", 386525, 465.0, "경기도", 13630821, 10195.6, "강원특별자치도", 1527807, 16874.6, "충청북도", 1634796, 7407.2, "충청남도", 2130119, 8246.1, "전북특별자치도", 1754757, 8071.4, "전라남도", 1804217, 12348.3, "경상북도", 2554324, 19034.4, "경상남도", 3247545, 10542.4, "제주특별자치도", 672948, 1850.3)

# [최종 수정] 모든 데이터 소스와 형식을 통일하기 위해, 모든 시군구 이름의 띄어쓰기를 제거합니다.
sigungu_pop_area <- tribble(~sido_name, ~sigungu_name_raw, ~population, ~area_km2,
                            "세종특별자치시", "세종시", 386525, 465.0,
                            "서울특별시", "종로구", 139945, 23.91, "서울특별시", "중구", 121312, 9.96, "서울특별시", "용산구", 213151, 21.87, "서울특별시", "성동구", 277352, 16.86, "서울특별시", "광진구", 334166, 17.06, "서울특별시", "동대문구", 337422, 14.22, "서울특별시", "중랑구", 381093, 18.5, "서울특별시", "성북구", 422553, 24.58, "서울특별시", "강북구", 290013, 23.6, "서울특별시", "도봉구", 309339, 20.65, "서울특별시", "노원구", 498311, 35.44, "서울특별시", "은평구", 462835, 29.71, "서울특별시", "서대문구", 306339, 17.63, "서울특별시", "마포구", 365286, 23.85, "서울특별시", "양천구", 436928, 17.41, "서울특별시", "강서구", 568886, 41.44, "서울특별시", "구로구", 395833, 20.12, "서울특별시", "금천구", 231762, 13.02, "서울특별시", "영등포구", 376073, 24.55, "서울특별시", "동작구", 381047, 16.35, "서울특별시", "관악구", 493494, 29.57, "서울특별시", "서초구", 400006, 46.98, "서울특별시", "강남구", 535483, 39.5, "서울특별시", "송파구", 658000, 33.88, "서울특별시", "강동구", 459933, 24.59,
                            "부산광역시", "중구", 40029, 2.83, "부산광역시", "서구", 104332, 13.98, "부산광역시", "동구", 86778, 9.73, "부산광역시", "영도구", 107477, 14.2, "부산광역시", "부산진구", 350916, 29.7, "부산광역시", "동래구", 270712, 16.63, "부산광역시", "남구", 257583, 17.27, "부산광역시", "북구", 274402, 39.37, "부산광역시", "해운대구", 382165, 51.47, "부산광역시", "사하구", 300536, 41.77, "부산광역시", "금정구", 218634, 65.28, "부산광역시", "강서구", 143532, 181.5, "부산광역시", "연제구", 207175, 12.08, "부산광역시", "수영구", 172709, 10.21, "부산광역시", "사상구", 201847, 36.09, "부산광역시", "기장군", 179325, 218.32,
                            "대구광역시", "중구", 76016, 7.06, "대구광역시", "동구", 338982, 182.16, "대구광역시", "서구", 159348, 17.34, "대구광역시", "남구", 140551, 17.44, "대구광역시", "북구", 412476, 93.99, "대구광역시", "수성구", 409837, 76.55, "대구광역시", "달서구", 533714, 62.34, "대구광역시", "달성군", 263332, 426.63, "대구광역시", "군위군", 23242, 614.32,
                            "인천광역시", "중구", 157488, 140.3, "인천광역시", "동구", 57858, 7.2, "인천광역시", "미추홀구", 401961, 24.8, "인천광역시", "연수구", 390953, 56.0, "인천광역시", "남동구", 500432, 57.1, "인천광역시", "부평구", 481257, 32.0, "인천광역시", "계양구", 282107, 45.5, "인천광역시", "서구", 606535, 119.1, "인천광역시", "강화군", 71237, 411.4, "인천광역시", "옹진군", 20410, 172.5,
                            "광주광역시", "동구", 101836, 49.21, "광주광역시", "서구", 289339, 47.88, "광주광역시", "남구", 212393, 17.55, "광주광역시", "북구", 423755, 121.76, "광주광역시", "광산구", 401952, 222.88,
                            "대전광역시", "동구", 218633, 136.6, "대전광역시", "중구", 223892, 62.1, "대전광역시", "서구", 468137, 95.5, "대전광역시", "유성구", 353135, 177.2, "대전광역시", "대덕구", 169320, 68.4,
                            "울산광역시", "중구", 203501, 37.0, "울산광역시", "남구", 306948, 73.0, "울산광역시", "동구", 149940, 36.0, "울산광역시", "북구", 219391, 157.4, "울산광역시", "울주군", 221491, 757.4,
                            "경기도", "가평군", 62397, 843.6, "경기도", "고양시덕양구", 491845, 165.5, "경기도", "고양시일산동구", 298453, 59.9, "경기도", "고양시일산서구", 296603, 42.8, "경기도", "과천시", 78827, 35.9, "경기도", "광명시", 283211, 38.5, "경기도", "광주시", 393296, 431.0, "경기도", "구리시", 187414, 33.3, "경기도", "군포시", 262442, 36.4, "경기도", "김포시", 488443, 276.6, "경기도", "남양주시", 734534, 458.1, "경기도", "동두천시", 88436, 95.7, "경기도", "부천시소사구", 780248, 53.4, "경기도", "부천시오정구", 780248, 53.4, "경기도", "부천시원미구", 780248, 53.4, "경기도", "성남시분당구", 478411, 69.4, "경기도", "성남시수정구", 229602, 46.1, "경기도", "성남시중원구", 208754, 26.9, "경기도", "수원시권선구", 367894, 47.3, "경기도", "수원시영통구", 369400, 27.5, "경기도", "수원시장안구", 269469, 33.1, "경기도", "수원시팔달구", 192534, 13.1, "경기도", "시흥시", 521452, 135.8, "경기도", "안산시단원구", 290606, 91.2, "경기도", "안산시상록구", 336499, 57.9, "경기도", "안성시", 190086, 553.5, "경기도", "안양시동안구", 309857, 28.2, "경기도", "안양시만안구", 233765, 30.3, "경기도", "양주시", 243330, 310.4, "경기도", "양평군", 125790, 877.8, "경기도", "여주시", 114354, 608.6, "경기도", "연천군", 41751, 675.2, "경기도", "오산시", 233184, 42.7, "경기도", "용인시기흥구", 439019, 81.7, "경기도", "용인시수지구", 378121, 42.1, "경기도", "용인시처인구", 262391, 467.5, "경기도", "의왕시", 157690, 54.0, "경기도", "의정부시", 463516, 81.5, "경기도", "이천시", 223281, 461.3, "경기도", "파주시", 498993, 672.4, "경기도", "평택시", 592009, 458.2, "경기도", "포천시", 146852, 826.5, "경기도", "하남시", 331194, 93.0, "경기도", "화성시", 948081, 693.8,
                            "강원특별자치도", "춘천시", 286812, 1116.4, "강원특별자치도", "원주시", 361846, 867.7, "강원특별자치도", "강릉시", 210028, 1040.4, "강원특별자치도", "동해시", 88451, 180.2, "강원특별자치도", "태백시", 38582, 303.5, "강원특별자치도", "속초시", 81962, 105.7, "강원특별자치도", "삼척시", 62499, 1186.6, "강원특별자치도", "홍천군", 67613, 1820.3, "강원특별자치도", "횡성군", 46749, 997.8, "강원특별자치도", "영월군", 36922, 1127.5, "강원특별자치도", "평창군", 40323, 1464.2, "강원특별자치도", "정선군", 34704, 1220.6, "강원특별자치도", "철원군", 41691, 899.8, "강원특별자치도", "화천군", 22865, 909.1, "강원특별자치도", "양구군", 20951, 702.3, "강원특별자치도", "인제군", 31437, 1646.1, "강원특별자치도", "고성군", 26381, 664.3, "강원특별자치도", "양양군", 27871, 628.9,
                            "충청북도", "괴산군", 37429, 842.1, "충청북도", "단양군", 27247, 780.6, "충청북도", "보은군", 31114, 584.2, "충청북도", "영동군", 44228, 846.5, "충청북도", "옥천군", 48866, 537.1, "충청북도", "음성군", 91525, 520.2, "충청북도", "제천시", 130480, 883.1, "충청북도", "증평군", 37210, 81.8, "충청북도", "진천군", 86605, 407.3, "충청북도", "청주시상당구", 175780, 404.4, "충청북도", "청주시서원구", 209875, 122.6, "충청북도", "청주시청원구", 197729, 214.9, "충청북도", "청주시흥덕구", 267329, 198.3, "충청북도", "충주시", 208633, 983.5,
                            "충청남도", "천안시동남구", 263309, 456.0, "충청남도", "천안시서북구", 396150, 197.8, "충청남도", "공주시", 102154, 864.4, "충청남도", "보령시", 95785, 569.4, "충청남도", "아산시", 341257, 542.2, "충청남도", "서산시", 179061, 741.3, "충청남도", "논산시", 110954, 554.8, "충청남도", "계룡시", 44910, 60.7, "충청남도", "당진시", 171118, 704.1, "충청남도", "금산군", 50016, 576.4, "충청남도", "부여군", 60788, 624.5, "충청남도", "서천군", 48775, 368.1, "충청남도", "청양군", 29886, 479.6, "충청남도", "홍성군", 98392, 444.0, "충청남도", "예산군", 76826, 542.1, "충청남도", "태안군", 61161, 514.9,
                            "전북특별자치도", "전주시완산구", 344781, 95.1, "전북특별자치도", "전주시덕진구", 309849, 111.1, "전북특별자치도", "군산시", 259572, 675.4, "전북특별자치도", "익산시", 270147, 506.7, "전북특별자치도", "정읍시", 102689, 692.9, "전북특별자치도", "남원시", 76573, 752.5, "전북특별자치도", "김제시", 80747, 545.2, "전북특별자치도", "완주군", 97973, 821.0, "전북특별자치도", "진안군", 24534, 789.1, "전북특별자치도", "무주군", 23083, 631.8, "전북특별자치도", "장수군", 21117, 789.8, "전북특별자치도", "임실군", 26178, 597.2, "전북특별자치도", "순창군", 26388, 495.9, "전북특별자치도", "고창군", 51939, 607.7, "전북특별자치도", "부안군", 48939, 493.3,
                            "전라남도", "목포시", 213382, 51.6, "전라남도", "여수시", 271326, 512.3, "전라남도", "순천시", 277987, 910.9, "전라남도", "나주시", 117217, 608.6, "전라남도", "광양시", 152098, 458.8, "전라남도", "담양군", 45618, 457.0, "전라남도", "곡성군", 26458, 547.4, "전라남도", "구례군", 23887, 443.2, "전라남도", "고흥군", 59510, 807.4, "전라남도", "보성군", 37978, 661.2, "전라남도", "화순군", 61380, 786.9, "전라남도", "장흥군", 34767, 618.3, "전라남도", "강진군", 32159, 500.4, "전라남도", "해남군", 64883, 1013.8, "전라남도", "영암군", 52190, 604.3, "전라남도", "무안군", 90814, 449.0, "전라남도", "함평군", 29974, 392.4, "전라남도", "영광군", 49602, 475.0, "전라남도", "장성군", 42476, 518.5, "전라남도", "완도군", 46269, 396.9, "전라남도", "진도군", 29058, 439.9, "전라남도", "신안군", 36926, 655.5,
                            "경상북도", "경산시", 267838, 411.7, "경상북도", "경주시", 248393, 1324.4, "경상북도", "고령군", 30178, 384.1, "경상북도", "구미시", 405699, 615.5, "경상북도", "김천시", 137798, 1009.5, "경상북도", "문경시", 68914, 911.2, "경상북도", "봉화군", 29679, 1201.0, "경상북도", "상주시", 93874, 1254.8, "경상북도", "성주군", 42001, 616.2, "경상북도", "안동시", 152780, 1522.0, "경상북도", "영덕군", 34167, 741.1, "경상북도", "영양군", 15744, 815.1, "경상북도", "영주시", 100296, 669.1, "경상북도", "영천시", 100283, 920.3, "경상북도", "예천군", 41942, 661.5, "경상북도", "울릉군", 8829, 72.9, "경상북도", "울진군", 46745, 989.2, "경상북도", "의성군", 49045, 1175.8, "경상북도", "청도군", 41200, 696.5, "경상북도", "청송군", 23737, 846.1, "경상북도", "칠곡군", 112046, 450.9, "경상북도", "포항시남구", 228399, 393.3, "경상북도", "포항시북구", 263445, 736.6,
                            "경상남도", "거제시", 234781, 403.0, "경상남도", "거창군", 60096, 803.9, "경상남도", "고성군", 49603, 517.3, "경상남도", "김해시", 532154, 463.3, "경상남도", "남해군", 40940, 357.6, "경상남도", "밀양시", 102467, 799.0, "경상남도", "사천시", 109305, 398.6, "경상남도", "산청군", 33656, 794.6, "경상남도", "양산시", 356868, 485.6, "경상남도", "의령군", 25607, 483.0, "경상남도", "진주시", 341234, 712.9, "경상남도", "창녕군", 59512, 532.8, "경상남도", "창원시의창구", 212579, 211.2, "경상남도", "창원시성산구", 223405, 82.2, "경상남도", "창원시마산합포구", 172605, 240.2, "경상남도", "창원시마산회원구", 193026, 90.6, "경상남도", "창원시진해구", 188633, 122.9, "경상남도", "통영시", 120410, 239.5, "경상남도", "하동군", 42143, 675.5, "경상남도", "함안군", 60789, 400.4, "경상남도", "함양군", 37272, 725.0, "경상남도", "합천군", 41238, 993.4,
                            "제주특별자치도", "제주시", 492829, 978.5, "제주특별자치도", "서귀포시", 184334, 871.2
)

# --- 4. 데이터 처리 함수 정의 ---
run_preprocessing <- function(aed_file_path,
                              data_month_str, 
                              common_hangjeongdong_sf,
                              common_hospitals_raw,
                              common_floating_pop_raw,
                              common_sido_pop,
                              common_sigungu_pop) {
  
  # --- 4-1. 헬퍼 함수 정의 ---
  standardize_sido_name <- function(name_vector) {
    name_vector <- str_replace(name_vector, "^서울(특별시)?$", "서울특별시"); name_vector <- str_replace(name_vector, "^부산(광역시)?$", "부산광역시"); name_vector <- str_replace(name_vector, "^대구(광역시)?$", "대구광역시"); name_vector <- str_replace(name_vector, "^인천(광역시)?$", "인천광역시"); name_vector <- str_replace(name_vector, "^광주(광역시)?$", "광주광역시"); name_vector <- str_replace(name_vector, "^대전(광역시)?$", "대전광역시"); name_vector <- str_replace(name_vector, "^울산(광역시)?$", "울산광역시"); name_vector <- str_replace(name_vector, "^세종(특별자치시)?$", "세종특별자치시"); name_vector <- str_replace(name_vector, "^경기(도)?$", "경기도"); name_vector <- str_replace(name_vector, "^강원(도|특별자치도)?$", "강원특별자치도"); name_vector <- str_replace(name_vector, "^충(청)?북(도)?$", "충청북도"); name_vector <- str_replace(name_vector, "^충(청)?남(도)?$", "충청남도"); name_vector <- str_replace(name_vector, "^전(라)?북(도|특별자치도)?$", "전북특별자치도"); name_vector <- str_replace(name_vector, "^전(라)?남(도)?$", "전라남도"); name_vector <- str_replace(name_vector, "^경(상)?북(도)?$", "경상북도"); name_vector <- str_replace(name_vector, "^경(상)?남(도)?$", "경상남도"); name_vector <- str_replace(name_vector, "^제주(특별자치도)?$", "제주특별자치도"); return(name_vector)
  }
  
  # --- 4-2. 월별 AED 데이터 처리 ---
  cat("AED 데이터 전처리를 시작합니다. (시간 소요)\n")
  
  aed_data_raw <- read.csv(aed_file_path, fileEncoding = "UTF-8", na.strings = c("NA", "", " ")) %>%
    as_tibble()
  
  original_rows <- nrow(aed_data_raw)
  
  aed_data_processed <- aed_data_raw %>%
    filter(!is.na(장비연번) & 장비연번 != "") %>%
    mutate(
      신고일자 = as.Date(str_replace_all(trimws(신고일자), "[./]", "-"), format = "%Y-%m-%d")
    ) %>%
    group_by(장비연번) %>%
    filter(신고일자 == max(신고일자, na.rm = TRUE) | is.na(신고일자)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(unique_id = row_number())
  
  deduped_rows <- nrow(aed_data_processed)
  cat("AED 데이터 중복 제거:", original_rows, "개 ->", deduped_rows, "개\n")
  
  data_with_coords <- aed_data_processed %>%
    filter(!is.na(경도) & !is.na(위도) & 경도 != 0 & 위도 != 0)
  
  spatial_join_results <- tibble()
  if(nrow(data_with_coords) > 0){
    spatial_join_sf <- st_as_sf(data_with_coords, coords = c("경도", "위도"), crs = 4326, na.fail = FALSE)
    joined_sf <- st_join(spatial_join_sf, common_hangjeongdong_sf %>% select(sidonm, sggnm), join = st_intersects, left = TRUE)
    spatial_join_results <- joined_sf %>%
      st_drop_geometry() %>%
      select(unique_id, sido_name_spatial = sidonm, sigungu_name_spatial = sggnm) %>%
      as_tibble()
  }
  cat("공간 정보 결합 완료:", nrow(spatial_join_results), "건 처리\n")
  
  aed_data <- aed_data_processed %>%
    left_join(spatial_join_results, by = "unique_id") %>%
    mutate(
      has_location = !is.na(경도) & !is.na(위도) & 경도 != 0 & 위도 != 0,
      sido_name = standardize_sido_name(`시.도`),
      sigungu_name_raw = str_replace_all(trimws(`구.군`), " ", ""), # AED 데이터의 시군구 이름도 띄어쓰기 제거
      sigungu_name_display = if_else(!is.na(sido_name) & !is.na(sigungu_name_raw), paste0(sido_name, " ", sigungu_name_raw), NA_character_),
      address_needs_verification = if_else(
        has_location & !is.na(sigungu_name_spatial) & (str_replace_all(trimws(`구.군`), " ", "") != sigungu_name_spatial),
        TRUE, FALSE, missing = FALSE
      )
    )
  cat("기본 컬럼 생성 완료\n")
  
  date_cols <- c("제조일", "교체.예정일", "배터리.유효기간", "패치.유효기간", "최근사용일", "최근점검일")
  aed_data <- aed_data %>%
    mutate(across(all_of(date_cols), ~ as_date(., format = "%Y-%m-%d")))
  cat("날짜 컬럼 변환 완료\n")
  
  reference_date <- as.Date(paste0("2025-", data_month_str, "-01")) %>%
    ceiling_date("month") - days(1)
  
  aed_data <- aed_data %>%
    mutate(
      is_expired_overall = case_when(
        !is.na(교체.예정일) & 교체.예정일 < reference_date ~ TRUE,
        !is.na(제조일) & (제조일 + years(10)) < reference_date ~ TRUE,
        TRUE ~ FALSE
      ),
      is_battery_expired = if_else(is.na(배터리.유효기간), TRUE, 배터리.유효기간 < reference_date),
      is_patch_expired = if_else(is.na(패치.유효기간), TRUE, 패치.유효기간 < reference_date),
      inspection_status = case_when(
        is.na(최근점검일) ~ "점검 필요",
        floor_date(최근점검일, "month") %in% c(floor_date(reference_date, "month"), floor_date(reference_date, "month") + months(1)) ~ "최근 점검",
        TRUE ~ "점검 필요"
      ),
      is_ready_for_use = !is_expired_overall & !is_battery_expired & !is_patch_expired,
      is_24_hour_available = grepl("24", 월요일운영시간) & grepl("24", 화요일운영시간) & grepl("24", 수요일운영시간) & grepl("24", 목요일운영시간) & grepl("24", 금요일운영시간) & grepl("24", 토요일운영시간) & grepl("24", 일요일운영시간),
      status_detail = factor(case_when(
        is_battery_expired ~ "배터리 만료", is_patch_expired ~ "패치 만료", is_expired_overall ~ "장비 만료",
        !has_location ~ "위치정보 누락", inspection_status == "점검 필요" ~ "점검 필요",
        is_ready_for_use & is_24_hour_available ~ "24시간 사용가능", is_ready_for_use ~ "정상",
        TRUE ~ "기타(사용불가)"
      ), levels = c("배터리 만료", "패치 만료", "장비 만료", "위치정보 누락", "점검 필요", "24시간 사용가능", "정상", "기타(사용불가)")),
      issue_level = case_when(
        !has_location | is_expired_overall | is_battery_expired | is_patch_expired ~ "즉시 조치 필요",
        !is.na(배터리.유효기간) & 배터리.유효기간 < (reference_date + months(2)) ~ "주의 필요",
        !is.na(패치.유효기간) & 패치.유효기간 < (reference_date + months(2)) ~ "주의 필요",
        inspection_status == "점검 필요" ~ "점검 필요",
        TRUE ~ "정상"
      )
    )
  cat("상태 평가 완료\n")
  
  aed_data <- aed_data %>%
    mutate(
      popup_html_content = pmap_chr(
        .l = list(id = unique_id, 기관명 = `설치기관명`, 설치위치 = `설치위치`, 주소 = `설치장소.주소`, 사용가능 = is_ready_for_use, 평일 = 월요일운영시간, 주말 = 토요일운영시간),
        .f = function(id, 기관명, 설치위치, 주소, 사용가능, 평일, 주말) {
          status_icon_html <- as.character(if (사용가능) icon("check-circle", style = "color:green;") else icon("times-circle", style = "color:red;"))
          report_button_html <- as.character(tags$button(
            class = "report-aed-btn btn btn-danger", `data-id` = id,
            style = "padding: 1px 5px; font-size: 0.75rem; width: 100%; margin-top: 5px;",
            "오류/사용 신고"
          ))
          as.character(tagList(
            tags$b(기관명), tags$small(paste0(" (", 설치위치, ")")), br(),
            "주소: ", 주소, br(),
            "운영(평일): ", if_else(is.na(평일) | 평일=="", "정보없음", 평일), br(),
            "운영(주말): ", if_else(is.na(주말) | 주말=="", "정보없음", 주말), br(),
            tags$b("사용 가능: "), HTML(status_icon_html),
            HTML(report_button_html)
          ))
        }
      )
    )
  cat("팝업 콘텐츠 생성 완료\n")
  
  
  # --- 4-3. 공간 데이터(sf) 생성 및 통계 결합 ---
  cat("공간 데이터 처리 및 결합을 시작합니다.\n")
  
  # [수정] sido_sf 생성 시, 미터 단위 좌표계에서 단순화 후 원래 좌표계로 복귀
  sido_sf_raw <- common_hangjeongdong_sf %>% 
    group_by(sidonm) %>% 
    summarise(geometry = st_union(geometry), .groups = 'drop') %>% 
    rename(name_kor = sidonm) %>%
    mutate(
      name_kor = standardize_sido_name(name_kor), 
      
      # --- 속도 개선을 위한 핵심 수정 부분 (시/도) ---
      geometry = st_transform(geometry, 5186) %>% # 1. 미터 단위 좌표계(EPSG:5186)로 변환
        st_simplify(dTolerance = 200) %>%         # 2. 200미터 기준으로 단순화 (이 값을 조절하며 테스트)
        st_transform(4326)                         # 3. 다시 Leaflet 표준 좌표계(EPSG:4326)로 복귀
      # --- 수정 끝 ---
    )
  
  aed_counts_by_sido <- aed_data %>% 
    filter(!is.na(sido_name)) %>% 
    group_by(sido_name) %>% 
    summarise(count = n(), .groups = 'drop')
  
  sido_sf <- left_join(sido_sf_raw, aed_counts_by_sido, by = c("name_kor" = "sido_name")) %>% 
    left_join(common_sido_pop, by = "name_kor") %>%
    mutate(
      across(c(count, population, area_km2), ~replace_na(.x, 0)),
      count_per_100k = ifelse(population > 0, round((count / population) * 100000, 1), 0),
      count_per_area = ifelse(area_km2 > 0, round(count / area_km2, 1), 0)
    )
  
  # [수정] sigungu_sf 생성 시, 미터 단위 좌표계에서 단순화 후 원래 좌표계로 복귀
  sigungu_sf_base <- common_hangjeongdong_sf %>% 
    group_by(sidonm, sggnm) %>% 
    summarise(geometry = st_union(geometry), .groups = 'drop') %>% 
    rename(sido_name = sidonm, sigungu_name_raw = sggnm) %>%
    mutate(
      sido_name = standardize_sido_name(sido_name),
      
      # --- 속도 개선을 위한 핵심 수정 부분 (시/군/구) ---
      geometry = st_transform(geometry, 5186) %>% # 1. 미터 단위 좌표계(EPSG:5186)로 변환
        st_simplify(dTolerance = 200) %>%         # 2. 200미터 기준으로 단순화 (이 값을 조절하며 테스트)
        st_transform(4326)                         # 3. 다시 Leaflet 표준 좌표계(EPSG:4326)로 복귀
      # --- 수정 끝 ---
    )
  
  aed_counts_by_sigungu <- aed_data %>% 
    filter(!is.na(sido_name), !is.na(sigungu_name_raw)) %>%
    group_by(sido_name, sigungu_name_raw) %>% 
    summarise(count = n(), .groups = 'drop')
  
  sigungu_sf <- left_join(sigungu_sf_base, common_sigungu_pop, by = c("sido_name", "sigungu_name_raw")) %>%
    left_join(aed_counts_by_sigungu, by = c("sido_name", "sigungu_name_raw")) %>%
    mutate(
      count = ifelse(is.na(count), 0, count), 
      population = ifelse(is.na(population), 0, population), 
      area_km2 = ifelse(is.na(area_km2), 0, area_km2),
      sigungu_name_display = paste0(sido_name, " ", sigungu_name_raw), 
      area = st_area(geometry),
      count_per_100k = ifelse(population > 0, round((count / population) * 100000, 1), 0),
      count_per_area = ifelse(area_km2 > 0, round(count / area_km2, 1), 0)
    )
  
  sido_order <- c("서울특별시","부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시","세종특별자치시","경기도","강원특별자치도","충청북도","충청남도","전북특별자치도","전라남도","경상북도","경상남도","제주특별자치도")
  sido_list <- c("전체", sido_order)
  korea_boundary <- st_union(sido_sf)
  
  # --- 4-4. 응급의료기관 데이터 처리 ---
  hospital_data <- tibble()
  if (!is.null(common_hospitals_raw) && nrow(common_hospitals_raw) > 0) {
    hospital_data_sf <- common_hospitals_raw %>%
      filter(!is.na(lon) & !is.na(lat) & lon != 0 & lat != 0) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    
    hospital_data <- st_join(hospital_data_sf, sigungu_sf %>% select(sido_name, sigungu_name_raw, sigungu_name_display), join = st_intersects, left = TRUE) %>%
      mutate(
        popup_html_content = pmap_chr(
          .l = list(기관명 = `기관명`, 분류 = `의료기관분류`, 주소 = `기관주소.도로명.`, 응급실전화 = `응급실.직통전화.`),
          .f = function(기관명, 분류, 주소, 응급실전화) { as.character(tagList(
            tags$b(paste0(기관명, " (", 분류, ")")), br(), "주소: ", 주소,
            br(), "응급실: ", if_else(is.na(응급실전화), "정보없음", as.character(응급실전화))
          ))}
        )
      )
  }
  
  # --- 4-5. 유동인구 데이터 처리 ---
  cat("유동인구 데이터를 처리합니다...\n")
  floating_pop_data <- tibble()
  
  tryCatch({
    if (!is.null(common_floating_pop_raw) && nrow(common_floating_pop_raw) > 0) {
      cat(" -> 원본 유동인구 데이터 로드 성공 (", nrow(common_floating_pop_raw), " 행)\n")
      
      base_pop_data <- common_floating_pop_raw %>%
        select(-any_of(c("PLST_NM", "YMD", "RPRT_CNT"))) %>%
        mutate(sido_name = standardize_sido_name(시도),
               sigungu_name_raw = str_replace_all(시군구, " ", "")) %>%
        select(sido_name, sigungu_name_raw, everything(), -시도, -시군구)
      
      time_data <- base_pop_data %>%
        select(sido_name, sigungu_name_raw, matches("FUDPUL_CNT_[0-9]{2}_[0-9]{2}_TIZN")) %>%
        pivot_longer(
          cols = -c(sido_name, sigungu_name_raw),
          names_to = "group", values_to = "population"
        ) %>%
        filter(!is.na(population) & population > 0) %>%
        mutate(
          type = "시간대",
          group = group %>% str_remove_all("FUDPUL_CNT_|_TIZN") %>% str_replace("_", "-") %>% paste0("시")
        )
      
      age_data <- base_pop_data %>%
        select(sido_name, sigungu_name_raw, starts_with("AGRDE_FUDPUL_CNT_GN")) %>%
        pivot_longer(
          cols = -c(sido_name, sigungu_name_raw),
          names_to = "group", values_to = "population"
        ) %>%
        filter(!is.na(population) & population > 0) %>%
        mutate(
          type = "연령대",
          group = if_else(str_detect(group, "ABOVE"),
                          paste0(str_extract(group, "[0-9]+"), "대 이상"),
                          paste0(str_extract(group, "[0-9]+"), "대"))
        )
      
      floating_pop_data <- bind_rows(time_data, age_data)
      cat(" -> ✅ 심층 분석용 유동인구 데이터 생성 완료 (", nrow(floating_pop_data), " 행)\n")
      
      sigungu_total_pop <- base_pop_data %>%
        mutate(total_daily_pop = rowSums(select(., starts_with("FUDPUL_CNT")), na.rm = TRUE)) %>%
        group_by(sido_name, sigungu_name_raw) %>%
        summarise(avg_floating_pop = mean(total_daily_pop, na.rm = TRUE), .groups = 'drop')
      
      sigungu_sf <- sigungu_sf %>%
        left_join(sigungu_total_pop, by = c("sido_name", "sigungu_name_raw")) %>%
        mutate(total_floating_pop = ifelse(is.na(avg_floating_pop), 0, avg_floating_pop)) %>%
        select(-any_of("avg_floating_pop")) 
      
      cat(" -> ✅ 지도용 유동인구 데이터 결합 완료\n")
      
    } else {
      sigungu_sf$total_floating_pop <- 0
      warning("원본 유동인구 데이터(common_floating_pop_raw)가 비어있습니다.")
    }
  }, error = function(e) {
    sigungu_sf$total_floating_pop <<- 0
    cat(" -> ❌ [오류] 유동인구 데이터 처리 중 심각한 오류 발생:", e$message, "\n")
  })
  
  cat("누락된 유동인구 데이터를 수동으로 추가합니다 (최종 버전)...\n")
  sigungu_sf <- sigungu_sf %>%
    mutate(
      total_floating_pop = case_when(
        # --- 기존 코드 (생략) ---
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경기도" & sigungu_name_raw == "용인시수지구" ~ 600000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경기도" & sigungu_name_raw == "수원시팔달구" ~ 450000,
        
        # [추가] 대구광역시 군위군 (상주인구 약 23,000명)
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "대구광역시" & sigungu_name_raw == "군위군" ~ 25000,
        
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "강원특별자치도" & sigungu_name_raw == "양양군" ~ 46000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "충청북도" & sigungu_name_raw == "청주시서원구" ~ 250000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "충청북도" & sigungu_name_raw == "증평군" ~ 50000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "충청남도" & sigungu_name_raw == "계룡시" ~ 60000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "충청남도" & sigungu_name_raw == "태안군" ~ 30000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "전라남도" & sigungu_name_raw == "신안군" ~ 10000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경상북도" & sigungu_name_raw == "포항시남구" ~ 250000,
        
        # [추가] 경상남도 고성군 (상주인구 약 50,000명)
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경상남도" & sigungu_name_raw == "고성군" ~ 60000,
        
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경상남도" & sigungu_name_raw == "창원시마산회원구" ~ 200000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "경상남도" & sigungu_name_raw == "창원시성산구" ~ 300000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "부산광역시" & sigungu_name_raw == "부산진구" ~ 1000000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "부산광역시" & sigungu_name_raw == "수영구" ~ 350000,
        
        # [추가] 울산광역시 북구 (상주인구 약 220,000명)
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "울산광역시" & sigungu_name_raw == "북구" ~ 220000,
        
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "인천광역시" & sigungu_name_raw == "옹진군" ~ 15000,
        (is.na(total_floating_pop) | total_floating_pop == 0) & sido_name == "세종특별자치시" & sigungu_name_raw == "세종시" ~ 500000,
        TRUE ~ total_floating_pop
      )
    )
  cat(" -> ✅ 누락된 유동인구 데이터 추가 완료.\n")
  
  # --- 4-6. 심정지 취약지수 및 서비스 미보장 지역 분석 ---
  cat("심정지 취약지수 및 서비스 미보장 지역 분석을 시작합니다... (시간이 많이 소요될 수 있습니다)\n")
  if (nrow(hospital_data) > 0) {
    hospital_sf <- hospital_data %>% st_as_sf()
    sigungu_centroids_4326 <- st_centroid(sigungu_sf) %>% st_transform(4326)
    
    dist_to_er_m <- map_dbl(1:nrow(sigungu_centroids_4326), ~{ min(st_distance(sigungu_centroids_4326[.x,], hospital_sf)) })
    sigungu_sf$nearest_er_dist_km <- dist_to_er_m / 1000
    
    sigungu_sf <- sigungu_sf %>%
      mutate(
        dist_score = if_else(is.infinite(nearest_er_dist_km), 1, scales::rescale(log(nearest_er_dist_km + 1))),
        aed_score = scales::rescale(-log(count_per_100k + 1)),
        vulnerability_score = round(dist_score + aed_score, 2)
      ) %>%
      select(-any_of(c("dist_score", "aed_score")))
    cat("취약지수 계산 완료.\n")
    
    cat("서비스 커버리지 분석 중...\n")
    crs_proj <- 5186  
    aed_points_sf <- aed_data %>% filter(has_location == TRUE) %>% st_as_sf(coords = c("경도", "위도"), crs = 4326) %>% st_transform(crs_proj)
    hospital_points_sf <- hospital_data %>% st_as_sf() %>% st_transform(crs_proj)
    aed_coverage <- st_buffer(aed_points_sf, dist = 200) %>% st_union()
    
    er_coverage_list <- list()
    if ("권역응급의료센터" %in% hospital_points_sf$의료기관분류) { er_coverage_list$level1 <- hospital_points_sf %>% filter(의료기관분류 == "권역응급의료센터") %>% st_buffer(dist = 3000) }
    if ("지역응급의료센터" %in% hospital_points_sf$의료기관분류) { er_coverage_list$level2 <- hospital_points_sf %>% filter(의료기관분류 == "지역응급의료센터") %>% st_buffer(dist = 2000) }
    if ("지역응급의료기관" %in% hospital_points_sf$의료기관분류) { er_coverage_list$level3 <- hospital_points_sf %>% filter(의료기관분류 == "지역응급의료기관") %>% st_buffer(dist = 1000) }
    
    if (length(er_coverage_list) > 0) {
      all_er_buffers <- do.call(rbind, er_coverage_list)
      er_coverage <- st_union(all_er_buffers)
      total_coverage <- st_union(aed_coverage, er_coverage)
    } else {
      total_coverage <- aed_coverage
    }
    
    sigungu_sf_proj <- st_transform(sigungu_sf, crs_proj)
    uncovered_pct <- map_dbl(1:nrow(sigungu_sf_proj), ~{
      sgg_geom <- st_geometry(sigungu_sf_proj[.x,])
      intersection <- suppressWarnings(st_intersection(sgg_geom, total_coverage))
      if (length(intersection) == 0 || st_is_empty(intersection)) { 100 }  
      else { round((1 - as.numeric(st_area(intersection) / st_area(sgg_geom))) * 100, 1) }
    })
    sigungu_sf$uncovered_pct <- uncovered_pct
    cat("서비스 커버리지 분석 완료.\n")
    
  } else {
    sigungu_sf$vulnerability_score <- NA
    sigungu_sf$nearest_er_dist_km <- NA
    sigungu_sf$uncovered_pct <- NA
    cat("병원 데이터가 없어 분석 기능을 건너뜁니다.\n")
  }
  
  # --- 4-7. 최종 결과 반환 ---
  cat("모든 전처리 완료. 데이터를 반환합니다.\n")
  return(
    list(
      aed_data = as.data.frame(aed_data),
      sido_sf = sido_sf,
      sigungu_sf = sigungu_sf,
      sido_list = sido_list,
      sido_order = sido_order,
      korea_boundary = korea_boundary,
      hospital_data = hospital_data,
      floating_pop_data = floating_pop_data
    )
  )
}

# --- 5. 배치 작업 실행 ---
for (item in process_list) {
  cat(paste0("\n===== ", item$month, "월 데이터 처리를 시작합니다 (입력: ", basename(item$aed_path), ") =====\n"))
  
  all_data <- run_preprocessing(
    aed_file_path = item$aed_path,
    data_month_str = item$month,
    common_hangjeongdong_sf = hangjeongdong_sf_raw,
    common_hospitals_raw = hospitals_raw,
    common_floating_pop_raw = floating_pop_raw,
    common_sido_pop = sido_pop_area,
    common_sigungu_pop = sigungu_pop_area
  )
  
  if (!is.null(all_data$aed_data) && is.data.frame(all_data$aed_data)) {
    temp_env <- new.env()
    list2env(all_data, envir = temp_env)
    save(
      list = ls(temp_env),
      file = item$output_path,
      envir = temp_env
    )
    cat(paste0("===== ", item$month, "월 데이터 처리 완료 (결과: ", item$output_path, ") =====\n"))
  } else {
    cat(paste0("!!!!! ", item$month, "월 데이터 처리 중 심각한 오류 발생. RData 파일이 생성되지 않았습니다. !!!!!\n"))
  }
}

cat("\n모든 데이터 전처리 작업이 완료되었습니다.\n")


# --- 6. AI 분류 모델 학습 및 저장 ---
cat("\n===== AI 분류 모델 학습을 시작합니다 (유사도 분석용 데이터 추가) =====\n")

preprocess_text <- function(text) {
  text_processed <- text %>%
    tolower() %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
    str_replace_all(pattern = "[0-9]+", replacement = " ") %>%
    str_squish()
  return(text_processed)
}
cat("텍스트 전처리 함수(preprocess_text)가 정의되었습니다.\n")

if(exists("all_data") && !is.null(all_data$aed_data)){
  training_data <- all_data$aed_data %>%
    filter(!is.na(분류1), 분류1 != "", !is.na(설치기관명)) %>%
    unite(
      "full_class", c(분류1, 분류2, 분류3, 분류4),
      sep = " > ", na.rm = TRUE, remove = FALSE
    ) %>%
    filter(full_class != "") %>%
    mutate(processed_text = preprocess_text(설치기관명)) %>%
    select(full_class, processed_text)
  
  cat(paste("총", nrow(training_data), "개의 데이터로 학습을 준비합니다.\n"))
  
  class_examples <- training_data %>%
    group_by(full_class) %>%
    summarise(examples = list(unique(processed_text)))
  cat("분류별 텍스트 예시 목록 생성을 완료했습니다.\n")
  
  all_words <- training_data %>%
    pull(processed_text) %>%
    str_split(" ") %>%
    unlist() %>%
    unique()
  vocab_size <- length(all_words)
  cat(paste("학습에 사용될 고유 단어의 수:", vocab_size, "개\n"))
  
  word_probs <- training_data %>%
    separate_rows(processed_text, sep = " ") %>%
    rename(word = processed_text) %>%
    filter(word != "") %>%
    count(full_class, word, name = "word_count") %>%
    add_count(full_class, wt = word_count, name = "total_words_in_class") %>%
    mutate(probability = (word_count + 1) / (total_words_in_class + vocab_size))
  
  class_priors <- training_data %>%
    count(full_class, name = "class_count") %>%
    mutate(prior = class_count / sum(class_count))
  
  class_total_words <- word_probs %>%
    distinct(full_class, total_words_in_class) %>%
    rename(total_words = total_words_in_class)
  
  aed_classification_model <- list(
    word_probs = word_probs,
    class_priors = class_priors,
    class_total_words = class_total_words,
    vocab_size = vocab_size,
    class_examples = class_examples
  )
  cat("AI 모델 구성이 완료되었습니다.\n")
  
  save(
    aed_classification_model,
    preprocess_text,
    file = "aed_classification_model.RData"
  )
  
  cat("===== AI 분류 모델이 'aed_classification_model.RData' 파일로 성공적으로 저장되었습니다 =====\n")
} else {
  cat("!!!!! AI 모델 학습을 위한 데이터가 없어 모델 생성을 건너뜁니다. !!!!!\n")
}
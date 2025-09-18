# new버전
# server.R (오류 최종 수정 버전)

# --- 1. 앱 시작 전 라이브러리 로딩 ---
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(cicerone)
library(bsicons)
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


# ===================================================================
# 서버 로직 시작
# ===================================================================
server <- function(input, output, session) {
  
  # [수정된 최종 코드] 모바일 접속 시, 사이드바를 확실하게 닫는 로직
  # 앱이 시작되고 UI가 완전히 그려진 후(250ms 지연) 실행하여 안정성을 확보합니다.
  shinyjs::delay(250, {
    shinyjs::runjs(
      "if (window.innerWidth < 768) {
         // UI.R에 정의된 숨김용 체크박스를 '체크된' 상태로 만들어 사이드바를 닫습니다.
         $('#sidebar-toggle-checkbox').prop('checked', true);
       }"
    )
  })
  
  # [핵심 추가] 시작 시 안내 화면(로딩 스크린) 로직
  # 1. 안내 화면에 표시할 HTML 내용 만들기
  welcome_screen_spinner <- tagList(
    tags$div(
      style = "color: #333; text-align: center;",
      tags$h2("AED MAP 대시보드", style = "font-weight: 600;"),
      tags$br(),
      p("왼쪽 사이드바 메뉴를 열어", style = "font-size: 1.1em;"),
      p("다양한 데이터 시각화를 확인해 보세요.", style = "font-size: 1.1em;"),
      br(),
      p(tags$i(class = "fa fa-bars"), " 버튼으로 메뉴를 열고 닫을 수 있습니다."),
      p(tags$i(class = "fa fa-map-marker-alt"), " 지도 위의 마커를 클릭하면 상세 정보가 표시됩니다."),
      br(),
      spin_3() # 로딩 스피너 추가
    )
  )
  
  
  
  
  

  
  # 2. 앱이 처음 시작될 때 바로 안내 화면을 보여줌
  waiter_show_on_load(
    html = welcome_screen_spinner,
    color = "rgba(255, 255, 255, 0.95)" # 거의 불투명한 흰색 배경
  )
  
  # 3. 모든 데이터와 UI가 준비될 시간을 고려하여 3.5초 후 안내 화면을 자동으로 숨김
  # Sys.sleep() 대신 delay()를 사용하여 다른 프로세스를 방해하지 않도록 함
  shinyjs::delay(3500, {
    waiter_hide()
  })
  # [추가] 현재 지도에 범례가 표시되고 있는지 상태를 저장하는 변수
  is_legend_active <- reactiveVal(FALSE)
  # --- 1. 초기 설정 및 공통 객체 ---
  
  # 1-1. 로딩 화면(Waiter) 설정
  waiter <- Waiter$new(html = tagList(
    spin_3(),
    h4("분석 데이터를 처리 중입니다...", style = "color:white; margin-top:20px;")
  ), color = "rgba(0,0,0,0.8)")
  
  golden_time_waiter <- Waiter$new(id = "golden_time_map",
                                   html = spin_3(),
                                   color = "rgba(0,0,0,0.3)")
  
  # 1-2. 공통 리액티브 객체
  data_trigger <- reactiveVal(FALSE)
  is_heatmap_active <- reactiveVal(FALSE)
  aed_to_report <- reactiveVal(NULL)
  map_view_state <- reactiveVal(list(lng = 127.7669, lat = 36.3, zoom = 7))
  last_clicked_shape <- reactiveVal(NULL)
  is_first_message <- reactiveVal(TRUE)
  
  # <<< ### 테이블 전용 데이터 객체 추가 ### >>>
  data_for_table <- reactiveVal(data.frame())
  
  # 1-3. 시뮬레이션 관련 리액티브 객체
  simulation_mode <- reactiveVal(FALSE)
  simulated_aeds <- reactiveVal(data.frame(lng = numeric(), lat = numeric()))
  
  # 1-4. 자동 리포트 관련 리액티브 객체
  report_scheduler <- reactiveVal(NULL)
  
  # 1-5. 디바운스 리액티브
  search_term <- reactive(input$place_search) %>% debounce(500)
  
  
  # --- 2. 기본 상호작용 로직 ---
  # 2-1. 시작 화면 처리 (지도 렌더링 오류 최종 수정)
  # server.R
  
  # '대시보드 시작하기' 버튼 로직을 찾아서 아래 내용으로 교체하세요.
  observeEvent(input$start_app_btn, {
    shinyjs::hide("start_screen")
    shinyjs::show("main_dashboard")
    data_trigger(TRUE)
    
    # [핵심 수정] 팝업 대신 하단 배너를 보여줍니다.
    shinyjs::show("loading_banner")
    
    # 지도 크기 재계산 로직 (기존 코드 유지)
    shinyjs::delay(250, {
      shinyjs::runjs("if (window.Leaflet && $('#map').is(':visible')) {
                    var map = $('#map').data('leaflet-map');
                    if (map) map.invalidateSize();
                  }")
    })
  })
  
  # 2-2. 도움말 버튼
  observeEvent(input$help_btn, {
    showModal(
      modalDialog(
        title = h4(icon("book-open"), "Smart AED 대시보드 둘러보기"),
        "안녕하세요! 이 가이드는 대시보드의 주요 기능을 빠르게 익힐 수 있도록 도와줍니다.",
        br(),
        br(),
        tags$h5("주요 기능 안내"),
        tags$ul(
          style = "padding-left: 20px;",
          tags$li(strong("지도 현황:"), " 전국 AED 분포를 지도에서 확인하고 필터링합니다."),
          tags$li(strong("세부 현황 분석:"), " 선택한 지역의 장비 상태, 점검률 등 상세 통계를 봅니다."),
          tags$li(strong("상세 분석 (탭 메뉴):"), " 유동인구, 접근성 등 고급 분석을 수행합니다."),
          tags$li(strong("통계 리포트:"), " 지역별/유형별 통계 데이터를 표 형태로 다운로드합니다."),
          tags$li(strong("AED 범례:"), " 범례는 PC환경에서만 확인할 수 있으며, AED의 상태에 따라 색상 진하기로 표현되거나 색상별로 상태를 나타냅니다."),
        ),
        easyClose = TRUE,
        footer = modalButton("확인")
      )
    )
  })
  
  # 2-3. 챗봇 모달 실행
  observeEvent(input$launch_chat_modal_btn, {
    showModal(chat_modal_ui)
    is_first_message(TRUE)
  })
  
  # 2-4. 챗봇 메시지 전송
  observeEvent(input$modal_chat_submit_btn, {
    query <- isolate(input$modal_chat_query_input)
    if (is.null(query) || nchar(trimws(query)) == 0)
      return()
    
    updateTextInput(session, "modal_chat_query_input", value = "")
    
    if (is_first_message()) {
      removeUI(selector = "#welcome-message-modal", immediate = TRUE)
      is_first_message(FALSE)
    }
    
    insertUI(
      selector = "#modal-chat-history-content",
      where = "beforeEnd",
      ui = div(class = "chat-message user-message", div(class = "message-bubble", query))
    )
    
    bot_response <- generate_bot_response(query)
    
    insertUI(
      selector = "#modal-chat-history-content",
      where = "beforeEnd",
      ui = div(class = "chat-message bot-message", div(class = "message-bubble", HTML(bot_response)))
    )
    
    shinyjs::delay(100, shinyjs::runjs("shinyjs.scrollChat();"))
  })
  
  # 2-5. 홈 로고 클릭 시 초기화
  observeEvent(input$reset_to_home, {
    updateSelectInput(session, "sido_filter", selected = "전체")
    updateTextInput(session, "place_search", value = "")
    updateRadioGroupButtons(session, "map_display_type", selected = "boundaries_only")
    data_for_table(data.frame()) # 테이블 데이터 초기화
    leafletProxy("map") %>% flyTo(lng = 127.7669,
                                  lat = 36.3,
                                  zoom = 7)
  })
  
  
  # --- 3. 데이터 로딩 및 필터링 로직 ---
  
  # 3-1. 월별 데이터 로딩 함수
  load_data_env <- function(month_code) {
    file_to_load <- switch(
      month_code,
      "01" = "preprocessed_01.RData", "02" = "preprocessed_02.RData",
      "03" = "preprocessed_03.RData", "04" = "preprocessed_04.RData",
      "05" = "preprocessed_05.RData", "06" = "preprocessed_06.RData",
      "07" = "preprocessed_07.RData", "08" = "preprocessed_08.RData"
    )
    
    if (!file.exists(file_to_load)) {
      showModal(modalDialog(
        title = "데이터 파일 오류",
        paste("필요한 데이터 파일(", file_to_load, ")이 없습니다."),
        footer = modalButton("닫기")
      ))
      return(NULL)
    }
    
    env <- new.env()
    load(file_to_load, envir = env)
    
    if ("aed_data" %in% ls(env)) {
      if (!is.data.table(env$aed_data)) { setDT(env$aed_data) }
      if ("장비연번" %in% names(env$aed_data) && "신고일자" %in% names(env$aed_data)) {
        env$aed_data[, 신고일자 := as.Date(신고일자)]
        setorder(env$aed_data, 장비연번, -신고일자)
        env$aed_data <- unique(env$aed_data, by = "장비연번")
      }
    }
    return(env)
  }
  
  # 3-2. 메인 데이터 리액티브
  data_env <- reactive({
    req(data_trigger(), cancelOutput = TRUE)
    req(input$data_month_select)
    
    showNotification(paste(input$data_month_select, "월 데이터를 불러옵니다..."), type = "message", duration = 2)
    
    env <- load_data_env(input$data_month_select)
    gc()
    return(env)
  })
  
  # 3-3. 필터 UI 동적 업데이트
  output$sigungu_filter_ui <- renderUI({
    req(input$sido_filter, data_env())
    choices <- if (input$sido_filter == "전체") {
      c("전체")
    } else {
      c("전체", sort(unique(data_env()$aed_data[sido_name == input$sido_filter & !is.na(sigungu_name_display), sigungu_name_display])))
    }
    selectInput("sigungu_filter", "시/군/구:", choices = choices, selected = isolate(input$sigungu_filter))
  })
  
  output$status_filter_ui <- renderUI({
    req(data_env())
    choices <- levels(data_env()$aed_data$status_detail)[levels(data_env()$aed_data$status_detail) != "위치정보 누락"]
    checkboxGroupInput("status_checkboxes", label = NULL, choices = choices, selected = choices)
  })
  
  output$stats_sigungu_filter_ui <- renderUI({
    req(input$stats_sido_filter, data_env())
    if (input$stats_sido_filter == "전국") return(NULL)
    choices <- c("전체", unique(data_env()$aed_data[sido_name == input$stats_sido_filter & !is.na(sigungu_name_raw), sigungu_name_raw]))
    selectInput("stats_sigungu_filter", "시/군/구 선택:", choices = choices, selected = "전체")
  })
  
  observe({
    env <- data_env()
    req(env)
    sido_choices <- env$sido_list
    sido_order_choices <- c("전국", env$sido_order)
    
    updateSelectInput(session, "sido_filter", choices = sido_choices)
    updateSelectInput(session, "stats_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "pred_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "issue_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "golden_time_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "coverage_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "optim_sido_filter", choices = sido_order_choices)
    updateSelectInput(session, "report_sido_filter", choices = sido_order_choices)
  })
  
  observe({
    env <- data_env()
    req(env, "floating_pop_data" %in% names(env))
    if (nrow(env$floating_pop_data) > 0) {
      age_choices <- c("전체", unique(env$floating_pop_data$group[env$floating_pop_data$type == "연령대"]))
      time_choices <- c("전체", unique(env$floating_pop_data$group[env$floating_pop_data$type == "시간대"]))
      updateSelectInput(session, "pop_age_filter", choices = age_choices)
      updateSelectInput(session, "pop_time_filter", choices = time_choices)
    } else {
      updateSelectInput(session, "pop_age_filter", choices = c("데이터 없음"))
      updateSelectInput(session, "pop_time_filter", choices = c("데이터 없음"))
    }
  })
  
  # 3-4. 필터링된 데이터 리액티브 (수정된 부분)
  debounced_place_search <- reactive(input$place_search) %>% debounce(500)
  
  # [수정] eventReactive를 reactive로 변경하여 필터 입력에 즉시 반응하도록 수정
  # 이제 지도 표시는 필터 변경 시 바로 업데이트되며, '필터 적용' 버튼은 주로 '장비 상세 목록' 탭에 데이터를 전달하는 역할을 합니다.
  filtered_data <- reactive({
    env <- data_env()
    req(env)
    
    # data_env()가 로드될 때까지 기다립니다.
    # 앱 시작 시 또는 월 변경 시 env$aed_data가 NULL이 아니도록 보장합니다.
    req(exists("aed_data", envir = env) && !is.null(env$aed_data))
    
    data <- env$aed_data
    sido_val <- input$sido_filter
    sigungu_val <- input$sigungu_filter
    search_val <- debounced_place_search()
    mandatory_val <- input$map_mandatory_filter
    
    # sido_filter가 아직 UI에서 렌더링되지 않았을 수 있으므로 NULL 체크 강화
    if (!is.null(sido_val) && sido_val != "전체") {
      data <- data[sido_name == sido_val]
      # sigungu_filter는 sido_filter에 따라 동적으로 생성되므로, 의존성 체크
      if (!is.null(sigungu_val) && sigungu_val != "전체") {
        data <- data[sigungu_name_display == sigungu_val]
      }
    }
    
    if (!is.null(search_val) && search_val != "") {
      data <- data[grepl(search_val, `설치기관명`, ignore.case = TRUE) | grepl(search_val, `장비연번`)]
    }
    
    if (!is.null(mandatory_val) && mandatory_val != "전체") {
      is_mandatory_filter <- if (mandatory_val == "의무") TRUE else FALSE
      data <- data[is_mandatory == is_mandatory_filter]
    }
    
    return(data)
  })
  
  dashboard_data <- reactive({
    env <- data_env()
    req(env)
    data <- copy(env$aed_data)
    
    sido_val <- input$sido_filter
    sigungu_val <- input$sigungu_filter
    
    if (!is.null(sido_val) && sido_val != "전체") {
      data <- data[sido_name == sido_val]
      if (!is.null(sigungu_val) && sigungu_val != "전체") {
        data <- data[sigungu_name_display == sigungu_val]
      }
    }
    return(data)
  })
  
  
  # --- 4. 지도 탭 로직 ---
  create_base_map <- function() {
    leaflet(options = leafletOptions(minZoom = 7)) %>%
      addMapPane("choroplethPane", zIndex = 390) %>% addMapPane("floatingPopPane", zIndex = 395) %>%
      addMapPane("labelPane", zIndex = 416) %>% addMapPane("maskPane", zIndex = 420) %>%
      addMapPane("aedBufferPane", zIndex = 425) %>% addMapPane("erBufferPane", zIndex = 426) %>%
      addMapPane("intersectionPane", zIndex = 427) %>% addMapPane("markerPane", zIndex = 430) %>%
      addMapPane("highlightPane", zIndex = 440) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = map_view_state()$lng, lat = map_view_state()$lat, zoom = map_view_state()$zoom) %>%
      setMaxBounds(lng1 = 122, lat1 = 32, lng2 = 132, lat2 = 39)
  }
  
  create_analysis_map <- function() {
    leaflet(options = leafletOptions(minZoom = 7)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 127.7669, lat = 36.3, zoom = 7) %>%
      setMaxBounds(lng1 = 122, lat1 = 32, lng2 = 132, lat2 = 39)
  }
  
  output$map <- renderLeaflet({ create_base_map() })
  
  # (이하 지도 관련 로직은 기존과 동일)
  observe({
    env <- data_env()
    req(env)
    metric <- input$choropleth_metric
    zoom <- input$map_zoom
    
    req(metric, zoom)
    
    proxy <- leafletProxy("map") %>%
      clearGroup(c("sido_boundary", "sigungu_boundary", "map_labels")) %>%
      removeControl("choropleth_legend")
    is_legend_active(FALSE) # [추가]
    
    # 'is_heatmap_active()' 대신 input 값을 직접 확인하도록 변경
    if (isTRUE(input$map_display_type == "heatmap")) {
      return()
    }
    
    if (metric == "none") {
      if (zoom < 10) {
        proxy %>% addPolygons(
          data = env$sido_sf,
          group = "sido_boundary",
          layerId = ~ name_kor,
          fillColor = "transparent",
          weight = 2,
          opacity = 0.8,
          color = "#4A637F",
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#4A90E2",
            fillOpacity = 0.3,
            bringToFront = TRUE
          ),
          label = ~ lapply(
            sprintf("<strong>%s</strong>", name_kor),
            htmltools::HTML
          ),
          options = pathOptions(pane = "choroplethPane")
        ) %>% addLabelOnlyMarkers(
          data = env$sido_sf,
          group = "map_labels",
          lng = ~ st_coordinates(st_point_on_surface(geometry))[, 1],
          lat = ~ st_coordinates(st_point_on_surface(geometry))[, 2],
          label = ~ name_kor,
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            className = 'sido-label',
            pane = "labelPane"
          )
        )
      } else {
        proxy %>% addPolygons(
          data = env$sigungu_sf,
          group = "sigungu_boundary",
          layerId = ~ sigungu_name_display,
          fillColor = "transparent",
          weight = 1,
          opacity = 0.8,
          color = "#4A637F",
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#4A90E2",
            fillOpacity = 0.3,
            bringToFront = TRUE
          ),
          label = ~ lapply(
            sprintf("<strong>%s</strong>", sigungu_name_display),
            htmltools::HTML
          ),
          options = pathOptions(pane = "choroplethPane")
        ) %>% addLabelOnlyMarkers(
          data = env$sigungu_sf,
          group = "map_labels",
          lng = ~ st_coordinates(st_point_on_surface(geometry))[, 1],
          lat = ~ st_coordinates(st_point_on_surface(geometry))[, 2],
          label = ~ sigungu_name_raw,
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            className = 'sigungu-label',
            pane = "labelPane"
          )
        )
      }
    } else {
      if (zoom < 10) {
        pal_sido <- colorNumeric("Blues", domain = env$sido_sf[[metric]])
        proxy %>% addPolygons(
          data = env$sido_sf,
          group = "sido_boundary",
          layerId = ~ name_kor,
          fillColor = ~ pal_sido(env$sido_sf[[metric]]),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "white",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~ lapply(
            sprintf("<strong>%s</strong>", name_kor),
            htmltools::HTML
          ),
          options = pathOptions(pane = "choroplethPane")
        ) %>% addLabelOnlyMarkers(
          data = env$sido_sf,
          group = "map_labels",
          lng = ~ st_coordinates(st_point_on_surface(geometry))[, 1],
          lat = ~ st_coordinates(st_point_on_surface(geometry))[, 2],
          label = ~ name_kor,
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            className = 'sido-label',
            pane = "labelPane"
          )
        )
        
        metric_labels <- c(
          "count" = "총 AED 수",
          "count_per_100k" = "인구 10만명 당 AED",
          "count_per_area" = "면적(km²) 당 AED"
        )
        legend_title <- as.character(metric_labels[metric])
        proxy %>% addLegend(
          layerId = "choropleth_legend",
          position = "bottomright",
          pal = pal_sido,
          values = env$sido_sf[[metric]],
          title = legend_title,
          opacity = 1,
          className = "info legend desktop-legend" # <<< 이렇게 추가/수정
        )
      } else {
        pal_sigungu <- colorNumeric("Blues", domain = env$sigungu_sf$count)
        proxy %>% addPolygons(
          data = env$sigungu_sf,
          group = "sigungu_boundary",
          layerId = ~ sigungu_name_display,
          fillColor = ~ pal_sigungu(count),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.6,
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#FFFFFF",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~ lapply(
            sprintf("<strong>%s</strong>", sigungu_name_display),
            htmltools::HTML
          ),
          options = pathOptions(pane = "choroplethPane")
        ) %>% addLabelOnlyMarkers(
          data = env$sigungu_sf,
          group = "map_labels",
          lng = ~ st_coordinates(st_point_on_surface(geometry))[, 1],
          lat = ~ st_coordinates(st_point_on_surface(geometry))[, 2],
          label = ~ sigungu_name_raw,
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            className = 'sigungu-label',
            pane = "labelPane"
          )
        )
        
        proxy %>% addLegend(
          layerId = "choropleth_legend",
          position = "bottomright",
          pal = pal_sigungu,
          values = env$sigungu_sf$count,
          title = "총 AED 수",
          opacity = 1,
          className = "info legend desktop-legend"  # <<< 이 줄을 추가하세요!
        )
        is_legend_active(TRUE) # [추가] 범례 활성 상태로 변경
      }
    }
  })
  
  selected_area_geom <- reactive({
    env <- data_env()
    req(env)
    sido <- input$sido_filter
    sigungu <- input$sigungu_filter
    if (!is.null(sigungu) && sigungu != "전체") {
      return(env$sigungu_sf %>% filter(sigungu_name_display == sigungu))
    } else if (sido != "전체") {
      return(env$sido_sf %>% filter(name_kor == sido))
    } else {
      return(NULL)
    }
  })
  
  observeEvent(selected_area_geom(), {
    proxy <- leafletProxy("map") %>% clearGroup("mask_layer")
    selected_geom <- selected_area_geom()
    if (!is.null(selected_geom) && nrow(selected_geom) > 0) {
      req(data_env()$korea_boundary)
      mask <- st_difference(data_env()$korea_boundary, st_union(selected_geom))
      proxy %>% addPolygons(
        data = mask,
        group = "mask_layer",
        fillColor = "black",
        fillOpacity = 0.4,
        weight = 0,
        options = pathOptions(pane = "maskPane")
      )
    } else {
      proxy %>% clearGroup("mask_layer")
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$map_display_type, {
    if (isTRUE(input$sido_filter == "전체") &&
        isTRUE(input$map_display_type %in% c("markers", "dots"))) {
      showModal(
        modalDialog(
          title = "성능 안내",
          "전국 단위의 클러스터 또는 상태별 보기는 많은 데이터를 처리하여 웹 브라우저가 느려질 수 있습니다. 정확하고 빠른 분석을 위해 지역 필터를 사용하시는 것을 권장합니다.",
          footer = modalButton("확인"),
          easyClose = TRUE
        )
      )
    }
  })
  
  observe({
    env <- data_env()
    req(env)
    data_to_draw <- filtered_data()
    display_type <- input$map_display_type
    show_hospitals_flag <- input$show_hospitals
    toggle_buffers_flag <- input$toggle_buffers
    current_sido <- input$sido_filter
    current_sigungu <- input$sigungu_filter
    
    proxy <- leafletProxy("map") %>%
      clearGroup(
        c(
          "AED 마커",
          "AED 점",
          "AED 히트맵",
          "hospitals",
          "aed_buffers",
          "er_buffers",
          "intersection_buffers"
        )
      ) %>%
      removeControl("status_legend")
    
    if (isTRUE(show_hospitals_flag)) {
      hospitals_to_draw <- env$hospital_data
      if (current_sido != "전체") {
        hospitals_to_draw <- hospitals_to_draw %>% filter(sido_name == current_sido)
        if (!is.null(current_sigungu) && current_sigungu != "전체") {
          hospitals_to_draw <- hospitals_to_draw %>% filter(sigungu_name_display == current_sigungu)
        }
      }
      if (nrow(hospitals_to_draw) > 0) {
        icons_by_level <- list(
          "권역응급의료센터" = awesomeIcons(icon = 'h-square', library = 'fa', markerColor = 'red', iconColor = '#FFF'),
          "지역응급의료센터" = awesomeIcons(icon = 'h-square', library = 'fa', markerColor = 'orange', iconColor = '#FFF'),
          "지역응급의료기관" = awesomeIcons(icon = 'h-square', library = 'fa', markerColor = 'blue', iconColor = '#FFF')
        )
        cluster_opts <- if (current_sido == "전체") markerClusterOptions() else NULL
        types_to_draw <- intersect(names(icons_by_level), unique(hospitals_to_draw$의료기관분류))
        if (length(types_to_draw) > 0) {
          for (type in types_to_draw) {
            data_subset <- hospitals_to_draw %>% filter(의료기관분류 == type)
            proxy %>% addAwesomeMarkers(
              data = data_subset, lng = ~ lon, lat = ~ lat, group = "hospitals",
              layerId = ~ paste0("hosp_", 기관코드), icon = icons_by_level[[type]],
              popup = ~ popup_html_content, clusterOptions = cluster_opts
            )
          }
        }
      }
    }
    
    if (isTRUE(toggle_buffers_flag)) {
      withProgress(message = "서비스 반경 계산 중...", value = 0, {
        incProgress(0.1, detail = "데이터 준비 중...")
        aed_sf <- data_to_draw %>% filter(has_location) %>% st_as_sf(coords = c("경도", "위도"), crs = 4326)
        hosp_sf_filtered <- env$hospital_data
        
        if (current_sido != "전체") {
          hosp_sf_filtered <- hosp_sf_filtered %>% filter(sido_name == current_sido)
          if (!is.null(current_sigungu) && current_sigungu != "전체") {
            hosp_sf_filtered <- hosp_sf_filtered %>% filter(sigungu_name_display == current_sigungu)
          }
        }
        hosp_sf <- hosp_sf_filtered %>% filter(!is.na(lon) & !is.na(lat)) %>% st_as_sf()
        
        if (nrow(aed_sf) > 0 || nrow(hosp_sf) > 0) {
          crs_proj <- 5186
          incProgress(0.3, detail = "AED 반경 생성 중...")
          aed_buffers <- if (nrow(aed_sf) > 0) st_transform(aed_sf, crs_proj) %>% st_buffer(dist = 500) %>% st_union() else NULL
          incProgress(0.3, detail = "응급실 반경 생성 중...")
          er_buffers <- if (nrow(hosp_sf) > 0) st_transform(hosp_sf, crs_proj) %>% st_buffer(dist = 5000) %>% st_union() else NULL
          incProgress(0.1, detail = "교차 영역 계산 중...")
          intersection_buffers <- if (!is.null(aed_buffers) && !is.null(er_buffers)) st_intersection(aed_buffers, er_buffers) else NULL
          incProgress(0.2, detail = "지도에 표시 중...")
          if (!is.null(aed_buffers)) proxy %>% addPolygons(data = st_transform(aed_buffers, 4326), group = "aed_buffers", color = NA, fillColor = "#3B82F6", fillOpacity = 0.3, options = pathOptions(pane = "aedBufferPane"))
          if (!is.null(er_buffers)) proxy %>% addPolygons(data = st_transform(er_buffers, 4326), group = "er_buffers", color = NA, fillColor = "#DC3545", fillOpacity = 0.3, options = pathOptions(pane = "erBufferPane"))
          if (!is.null(intersection_buffers) && !st_is_empty(intersection_buffers)) proxy %>% addPolygons(data = st_transform(intersection_buffers, 4326), group = "intersection_buffers", color = NA, fillColor = "#6b21a8", fillOpacity = 0.6, options = pathOptions(pane = "intersectionPane"))
        } else {
          showNotification("반경을 표시할 데이터가 없습니다.", type = "warning")
        }
      })
    }
    
    if (display_type == "markers") {
      if (nrow(data_to_draw) > 0) {
        icons <- awesomeIcons(icon = 'heartbeat', iconColor = 'white', library = 'fa', markerColor = ifelse(data_to_draw$is_ready_for_use, "green", "red"))
        proxy %>% addAwesomeMarkers(
          data = data_to_draw, lng = ~ 경도, lat = ~ 위도, group = "AED 마커",
          layerId = ~ paste0("aed_", unique_id), 
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 17),
          icon = icons, popup = ~ popup_html_content, options = markerOptions(pane = "markerPane")
        )
      }
    } else if (display_type == "dots") {
      if (nrow(data_to_draw) > 15000) {
        showNotification("표시할 데이터가 너무 많습니다 (15,000개 초과). 지도를 확대하거나 필터를 사용해 지역을 좁혀주세요.", type = "warning", duration = 10)
        data_to_draw <- data_to_draw %>% slice(0)
      }
      
      if (!is.null(input$status_checkboxes) && length(input$status_checkboxes) > 0) {
        data_to_draw <- data_to_draw %>% filter(status_detail %in% input$status_checkboxes)
      } else {
        data_to_draw <- data_to_draw %>% slice(0)
      }
      
      if (nrow(data_to_draw) > 0) {
        data_to_draw <- data_to_draw %>% left_join(status_color_map, by = "status_detail")
      }
      
      if (nrow(data_to_draw) > 0) {
        proxy %>% addCircleMarkers(
          data = data_to_draw, lng = ~ 경도, lat = ~ 위도, group = "AED 점",
          layerId = ~ paste0("dot_", unique_id), radius = 5, stroke = TRUE, color = "white",
          weight = 1.5, fillColor = ~ color_hex, fillOpacity = 0.9,
          popup = ~ popup_html_content, options = pathOptions(pane = "markerPane")
        )
      }
      
      statuses_for_legend <- status_color_map %>%
        filter(status_detail %in% levels(data_env()$aed_data$status_detail)) %>%
        filter(status_detail != "위치정보 누락")
      
      if (nrow(statuses_for_legend) > 0) {
        proxy %>% addLegend(
          layerId = "status_legend", position = "bottomright",
          colors = statuses_for_legend$color_hex, labels = statuses_for_legend$status_detail,
          title = "AED 상태", opacity = 1,
          className = "info legend desktop-legend" # className 확인
        )
        is_legend_active(TRUE) # [추가] 범례 활성 상태로 변경
      } else {
        is_legend_active(FALSE) # [추가] 범례가 없으면 비활성 상태로 변경
      }
      
    } else if (display_type == "heatmap") {
      # 위치 정보가 있는 데이터만 필터링
      data_for_heatmap <- data_to_draw %>% filter(has_location == TRUE)
      
      if (nrow(data_for_heatmap) > 0) {
        proxy %>% addHeatmap(
          data = data_for_heatmap, lng = ~ 경도, lat = ~ 위도, group = "AED 히트맵",
          blur = 20, max = 0.05, radius = 15
        )
      }
    }
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    req(click$id)
    env <- data_env()
    req(env)
    
    proxy <- leafletProxy("map", session) %>% clearGroup(c("aed_buffers", "er_buffers"))
    
    click_type <- str_split_fixed(click$id, "_", 2)[1, 1]
    click_real_id <- str_split_fixed(click$id, "_", 2)[1, 2]
    
    if (click_type %in% c("aed", "dot")) {
      aed_info <- env$aed_data %>% filter(unique_id == click_real_id)
      if (nrow(aed_info) > 0) {
        proxy %>% addCircles(lng = aed_info$경도, lat = aed_info$위도, group = "aed_buffers", radius = 500, color = "#3B82F6", stroke = FALSE, fillOpacity = 0.4, options = pathOptions(pane = "aedBufferPane"))
      }
    } else if (click_type == "hosp") {
      hospital_info <- env$hospital_data %>% filter(기관코드 == click_real_id)
      if (nrow(hospital_info) > 0) {
        proxy %>% addCircles(lng = hospital_info$lon, lat = hospital_info$lat, group = "er_buffers", radius = 5000, color = "#DC3545", stroke = FALSE, fillOpacity = 0.4, options = pathOptions(pane = "erBufferPane"))
      }
    }
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(click, click$id)
    
    current_last_click <- last_clicked_shape()
    
    # 이전에 클릭한 도형의 ID와 현재 클릭한 도형의 ID가 같으면 확대
    if (!is.null(current_last_click) && !is.na(current_last_click$id) && current_last_click$id == click$id) {
      leafletProxy("map") %>% flyTo(lng = click$lng, lat = click$lat, zoom = input$map_zoom + 2)
      # 확대 후에는 마지막 클릭 기록 초기화
      last_clicked_shape(NULL) 
    } else {
      # 다른 도형을 클릭했으면, 클릭 정보 저장 및 필터 업데이트
      last_clicked_shape(click)
      
      zoom_level <- isolate(input$map_zoom)
      req(zoom_level) # 현재 줌 레벨 확인
      
      if (zoom_level < 10) {
        # 줌 레벨이 10 미만이면 시/도 단위로 필터링
        updateSelectInput(session, "sigungu_filter", selected = "전체")
        updateSelectInput(session, "sido_filter", selected = click$id)
      } else {
        # 줌 레벨이 10 이상이면 시/군/구 단위로 필터링
        # sido_filter는 현재 선택된 상태를 유지해야 함
        updateSelectInput(session, "sigungu_filter", selected = click$id)
      }
    }
  })
  
  observeEvent(input$sido_filter, {
    req(input$sido_filter)
    if (!is.null(input$sigungu_filter) && input$sigungu_filter != "전체") return()
    env <- data_env()
    req(env)
    proxy <- leafletProxy("map")
    if (input$sido_filter == "전체") {
      proxy %>% flyTo(lng = 127.7669, lat = 36.3, zoom = 7)
    } else {
      area <- env$sido_sf %>% filter(name_kor == input$sido_filter)
      if (nrow(area) > 0) {
        bbox <- st_bbox(area)
        proxy %>% flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sigungu_filter, {
    req(input$sigungu_filter, input$sigungu_filter != "전체")
    env <- data_env()
    req(env)
    area <- env$sigungu_sf %>% filter(sigungu_name_display == input$sigungu_filter)
    if (nrow(area) > 0) {
      bbox <- st_bbox(area)
      leafletProxy("map") %>% flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
  }, ignoreInit = TRUE)
  
  # 'search_term()'을 'debounced_place_search()'로 변경
  observeEvent(debounced_place_search(), {
    # 검색어가 비어있지 않은지 확인
    search_val <- debounced_place_search()
    req(nzchar(search_val))
    
    # Sys.sleep() 제거하여 반응성 향상
    found_data <- filtered_data()
    
    if (nrow(found_data) == 1) {
      # 단일 결과: 해당 위치로 크게 확대하여 개별 마커 표시
      leafletProxy("map") %>% flyTo(lng = found_data$경도, lat = found_data$위도, zoom = 18)
    } else if (nrow(found_data) > 1) {
      # 다중 결과: 모든 결과가 보이도록 지도 범위 조정
      leafletProxy("map") %>% flyToBounds(lng1 = min(found_data$경도),
                                          lat1 = min(found_data$위도),
                                          lng2 = max(found_data$경도),
                                          lat2 = max(found_data$위도))
    }
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "sido_filter", selected = "전체")
    updateTextInput(session, "place_search", value = "")
    updateRadioGroupButtons(session, "map_display_type", selected = "boundaries_only")
    data_for_table(data.frame()) # 테이블 데이터 초기화
    leafletProxy("map") %>% clearGroup("nearby_results_group") %>% flyTo(lng = 127.7669, lat = 36.3, zoom = 7)
  })
  
  observeEvent(input$show_floating_pop, {
    proxy <- leafletProxy("map")
    env <- data_env()
    req(env)
    
    if (isTRUE(input$show_floating_pop) &&
        "total_floating_pop" %in% colnames(env$sigungu_sf)) {
      data_to_draw <- env$sigungu_sf %>%
        filter(!is.na(total_floating_pop) & total_floating_pop > 0)
      
      if (nrow(data_to_draw) == 0) {
        showNotification("표시할 유동인구 데이터가 없습니다.", type = "warning")
        return()
      }
      
      pal_floating_pop <- colorNumeric(
        "YlOrRd",
        domain = data_to_draw$total_floating_pop,
        na.color = "#808080"
      )
      
      proxy %>%
        clearGroup("floating_population_layer") %>%
        removeControl("floating_pop_legend") %>%
        addPolygons(
          data = data_to_draw,
          group = "floating_population_layer",
          fillColor = ~ pal_floating_pop(total_floating_pop),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          label = ~ lapply(
            sprintf(
              "<strong>%s</strong><br/>평균 유동인구: %s명",
              sigungu_name_display,
              prettyNum(round(total_floating_pop, 0), big.mark = ",")
            ),
            htmltools::HTML
          ),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "white",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          options = pathOptions(pane = "floatingPopPane")
        ) %>%
        addLegend(
          position = "topright",
          pal = pal_floating_pop,
          values = data_to_draw$total_floating_pop,
          title = "평균 일일 유동인구",
          opacity = 1,
          layerId = "floating_pop_legend",
          className = "info legend desktop-legend" # <<< 이렇게 추가/수정
        )
    } else {
      proxy %>%
        clearGroup("floating_population_layer") %>%
        removeControl("floating_pop_legend")
    }
  }, ignoreInit = TRUE)
  
  
  # --- 5. 통계 리포트 탭 로직 ---
  get_stylish_dt_options <- function(pageLength = 10) {
    list(
      pageLength = pageLength,
      dom = "B<'row'<'col-sm-12 col-md-6'l><'col-sm-12 col-md-6'f>>" %>%
        paste0("<'row'<'col-sm-12'tr>>") %>%
        paste0(
          "<'row'<'col-sm-12 col-md-5'i><'col-sm-12 col-md-7'p>>"
        ),
      buttons = list(
        list(
          extend = 'excel',
          text = '엑셀 저장',
          className = 'btn-sm btn-success'
        ),
        list(
          extend = 'csv',
          text = 'CSV 저장',
          className = 'btn-sm btn-secondary'
        )
      ),
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Korean.json"),
      scrollX = TRUE
    )
  }
  
  output$stats_table_density <- renderDT({
    env <- data_env()
    req(env)
    regional_data <- stats_regional_data()
    group_var <- stats_group_by_var()
    
    total_pop_area <- if (input$stats_sido_filter == "전국") {
      env$sido_sf %>% st_drop_geometry() %>%
        summarise(
          인구 = sum(population, na.rm = TRUE),
          면적_km2 = sum(area_km2, na.rm = TRUE)
        )
    } else {
      env$sigungu_sf %>% st_drop_geometry() %>%
        filter(sido_name == input$stats_sido_filter) %>%
        summarise(
          인구 = sum(population, na.rm = TRUE),
          면적_km2 = sum(area_km2, na.rm = TRUE)
        )
    }
    
    total_row <- tibble(
      지역 = if (input$stats_sido_filter == "전국")
        "전국 전체"
      else
        paste(input$stats_sido_filter, "전체"),
      `총 설치대수` = nrow(regional_data),
      `인구` = total_pop_area$인구,
      `면적(km²)` = total_pop_area$면적_km2
    ) %>%
      mutate(
        `인구 10만명당` = ifelse(인구 > 0, round((`총 설치대수` / 인구) * 100000, 1), 0),
        `면적 km²당` = ifelse(`면적(km²)` > 0, round(`총 설치대수` / `면적(km²)`, 1), 0)
      )
    
    summary_data <- regional_data %>%
      filter(!is.na(.data[[group_var]]) &
               .data[[group_var]] != "") %>%
      group_by(group = .data[[group_var]]) %>%
      summarise(AED_설치대수 = n(), .groups = 'drop')
    
    join_data <- if (group_var == "sido_name") {
      env$sido_sf %>% st_drop_geometry() %>% select(group = name_kor,
                                                    인구 = population,
                                                    면적_km2 = area_km2)
    } else {
      env$sigungu_sf %>% st_drop_geometry() %>% filter(sido_name == input$stats_sido_filter) %>% select(group = sigungu_name_raw,
                                                                                                        인구 = population,
                                                                                                        면적_km2 = area_km2)
    }
    
    final_data_grouped <- left_join(summary_data, join_data, by = "group") %>%
      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
      mutate(인구십만명당 = ifelse(인구 > 0, round((AED_설치대수 / 인구) * 100000, 1), 0),
             면적당 = ifelse(면적_km2 > 0, round(AED_설치대수 / 면적_km2, 1), 0)) %>%
      select(
        지역 = group,
        `총 설치대수` = AED_설치대수,
        `인구`,
        `면적(km²)` = 면적_km2,
        `인구 10만명당` = 인구십만명당,
        `면적 km²당` = 면적당
      )
    
    final_data <- bind_rows(total_row, final_data_grouped)
    
    datatable(
      final_data,
      class = 'cell-border stripe hover',
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_stylish_dt_options()
    ) %>%
      formatCurrency(
        columns = c('총 설치대수', '인구'),
        currency = "",
        digits = 0
      ) %>%
      formatRound(columns = c('인구 10만명당', '면적 km²당', '면적(km²)'),
                  digits = 1)
  }, server = FALSE)
  
  stats_regional_data <- reactive({
    env <- data_env()
    req(env)
    sido_val <- input$stats_sido_filter
    sigungu_val <- input$stats_sigungu_filter
    data <- env$aed_data
    if (sido_val != "전국") {
      data <- data %>% filter(sido_name == sido_val)
      if (!is.null(sigungu_val) &&
          sigungu_val != "전체") {
        data <- data %>% filter(sigungu_name_raw == sigungu_val)
      }
    }
    return(data)
  })
  
  stats_group_by_var <- reactive({
    if (input$stats_sido_filter == "전국")
      "sido_name"
    else
      "sigungu_name_raw"
  })
  
  output$stats_title <- renderText({
    sido_val <- input$stats_sido_filter
    sigungu_val <- input$stats_sigungu_filter
    region_text <- if (sido_val == "전국") {
      "전국"
    } else {
      if (!is.null(sigungu_val) &&
          sigungu_val != "전체") {
        paste(sido_val, sigungu_val)
      } else {
        paste(sido_val, "전체")
      }
    }
    paste(region_text, "AED 현황 리포트")
  })
  
  output$stats_table_inspection <- renderDT({
    group_var <- stats_group_by_var()
    data_to_process_full <- stats_regional_data()
    data_to_process_grouped <- data_to_process_full %>%
      filter(!is.na(.data[[group_var]]) & .data[[group_var]] != "")
    
    summarize_inspection_data <- function(df) {
      df %>%
        summarise(
          `총 설치대수` = n(),
          `점검 대수` = sum(inspection_status == "최근 점검", na.rm = TRUE),
          `의무_총` = sum(분류1 == "구비의무기관", na.rm = TRUE),
          `의무_점검` = sum(inspection_status == "최근 점검" &
                          분류1 == "구비의무기관", na.rm = TRUE),
          `의무외_총` = sum(분류1 != "구비의무기관", na.rm = TRUE),
          `의무외_점검` = sum(inspection_status == "최근 점검" &
                           분류1 != "구비의무기관", na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          `점검률` = if_else(`총 설치대수` > 0, `점검 대수` / `총 설치대수`, 0),
          `의무기관 점검률` = if_else(`의무_총` > 0, `의무_점검` / `의무_총`, 0),
          `의무 외 점검률` = if_else(`의무외_총` > 0, `의무외_점검` / `의무외_총`, 0)
        )
    }
    
    total_row <- summarize_inspection_data(data_to_process_full) %>%
      mutate(지역 = if (input$stats_sido_filter == "전국")
        "전국 전체"
        else
          paste(input$stats_sido_filter, "전체"))
    
    summary_data <- data_to_process_grouped %>%
      group_by(지역 = .data[[group_var]]) %>%
      summarize_inspection_data()
    
    final_data <- bind_rows(total_row, summary_data) %>%
      mutate(
        `점검률` = scales::percent(`점검률`, accuracy = 0.1),
        `의무기관 점검률` = scales::percent(`의무기관 점검률`, accuracy = 0.1),
        `의무 외 점검률` = scales::percent(`의무 외 점검률`, accuracy = 0.1)
      ) %>%
      select(지역, `총 설치대수`, `점검 대수`, `점검률`, `의무기관 점검률`, `의무 외 점검률`)
    
    datatable(
      final_data,
      class = 'cell-border stripe hover',
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_stylish_dt_options()
    ) %>%
      formatCurrency(
        columns = c('총 설치대수', '점검 대수'),
        currency = "",
        digits = 0
      )
  }, server = FALSE)
  
  output$stats_table_management <- renderDT({
    group_var <- stats_group_by_var()
    regional_data <- stats_regional_data()
    data_for_grouping <- regional_data %>%
      filter(!is.na(.data[[group_var]]) & .data[[group_var]] != "")
    
    summarize_management_data <- function(df) {
      df %>%
        summarise(
          `10년 이상 장비` = sum(is_expired_overall, na.rm = TRUE),
          `배터리 만료` = sum(is_battery_expired, na.rm = TRUE),
          `패치 만료` = sum(is_patch_expired, na.rm = TRUE),
          `24시간 이용가능` = sum(is_24_hour_available, na.rm = TRUE),
          .groups = 'drop'
        )
    }
    
    total_row <- summarize_management_data(regional_data) %>%
      mutate(지역 = if (input$stats_sido_filter == "전국")
        "전국 전체"
        else
          paste(input$stats_sido_filter, "전체")) %>%
      relocate(지역)
    
    grouped_data <- data_for_grouping %>%
      group_by(지역 = .data[[group_var]]) %>%
      summarize_management_data()
    
    final_data <- bind_rows(total_row, grouped_data)
    numeric_cols <- colnames(final_data)[sapply(final_data, is.numeric)]
    
    datatable(
      final_data,
      class = 'cell-border stripe hover',
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_stylish_dt_options()
    ) %>%
      formatCurrency(columns = numeric_cols,
                     currency = "",
                     digits = 0)
  }, server = FALSE)
  
  output$stats_table_by_class <- renderDT({
    data <- stats_regional_data() %>%
      filter(분류1 != "" & !is.na(분류1)) %>%
      group_by(`대분류` = 분류1, `중분류` = 분류2) %>%
      summarise(
        `설치대수` = n(),
        `점검률` = scales::percent(mean(inspection_status == "최근 점검", na.rm = TRUE), accuracy = 0.1),
        `노후 장비 수` = sum(is_expired_overall, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(`설치대수`))
    
    datatable(
      data,
      class = 'cell-border stripe hover',
      rownames = FALSE,
      extensions = 'Buttons',
      options = get_stylish_dt_options(pageLength = 10)
    ) %>%
      formatCurrency(
        columns = c('설치대수', '노후 장비 수'),
        currency = "",
        digits = 0
      )
  }, server = FALSE)
  
  
  # --- 6. 세부 현황 분석 탭 로직 ---
  selected_region_name <- reactive({
    sido <- input$sido_filter
    sigungu <- input$sigungu_filter
    
    if (sido == "전체") {
      "전국"
    } else {
      if (!is.null(sigungu) && sigungu != "전체") {
        sigungu
      } else {
        sido
      }
    }
  })
  
  output$detail_dashboard_title <- renderText({
    paste(selected_region_name(), "세부 현황 분석")
  })
  
  output$total_aed_box <- renderUI({
    value_box(
      title = "총 AED 수",
      value = prettyNum(nrow(dashboard_data()), big.mark = ","),
      showcase = icon("heart-pulse"),
      class = "value-box-total"
    )
  })
  
  output$ready_aed_box <- renderUI({
    value_box(
      title = "즉시 사용 가능",
      value = prettyNum(
        sum(dashboard_data()$is_ready_for_use, na.rm = TRUE),
        big.mark = ","
      ),
      showcase = icon("check-circle"),
      class = "value-box-ready"
    )
  })
  
  output$expired_aed_box <- renderUI({
    value_box(
      title = "장비 만료",
      value = prettyNum(
        sum(dashboard_data()$is_expired_overall, na.rm = TRUE),
        big.mark = ","
      ),
      showcase = icon("triangle-exclamation"),
      class = "value-box-expired"
    )
  })
  
  output$inspection_needed_box <- renderUI({
    value_box(
      title = "점검 필요",
      value = prettyNum(
        sum(dashboard_data()$inspection_status == '점검 필요'),
        big.mark = ','
      ),
      showcase = icon("search"),
      class = "value-box-inspection"
    )
  })
  
  output$inspection_chart_overall <- renderEcharts4r({
    data <- dashboard_data()
    req(nrow(data) > 0)
    pct <- round(sum(data$inspection_status == "최근 점검", na.rm = TRUE) / nrow(data) * 100,
                 1)
    data %>%
      count(inspection_status) %>%
      e_charts(inspection_status) %>%
      e_pie(n, name = "수량", radius = c("55%", "75%")) %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c}대 ({d}%)") %>%
      e_title(
        text = paste0(pct, "%"),
        subtext = "최근 점검률",
        left = 'center',
        top = '40%',
        textStyle = list(fontSize = 22, fontWeight = 'bold')
      ) %>%
      e_legend(bottom = 0)
  })
  
  output$inspection_chart_mandatory <- renderEcharts4r({
    data <- dashboard_data() %>% filter(분류1 == "구비의무기관")
    req(nrow(data) > 0)
    pct <- round(sum(data$inspection_status == "최근 점검", na.rm = TRUE) / nrow(data) * 100,
                 1)
    data %>%
      count(inspection_status) %>%
      e_charts(inspection_status) %>%
      e_pie(n, name = "수량", radius = c("55%", "75%")) %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c}대 ({d}%)") %>%
      e_title(
        text = paste0(pct, "%"),
        subtext = "최근 점검률",
        left = 'center',
        top = '40%',
        textStyle = list(fontSize = 22, fontWeight = 'bold')
      ) %>%
      e_legend(bottom = 0)
  })
  
  output$battery_expiry_chart <- renderEcharts4r({
    data <- dashboard_data()
    req(nrow(data) > 0)
    data <- data %>% mutate(`배터리.유효기간` = lubridate::as_date(`배터리.유효기간`))
    
    data <- data %>% mutate(
      battery_status = case_when(
        is.na(`배터리.유효기간`) ~ "정보 없음",
        `배터리.유효기간` < today ~ "만료",
        `배터리.유효기간` < (today + months(6)) ~ "6개월 내 만료",
        TRUE ~ "정상"
      )
    )
    
    pct <- round(sum(data$battery_status == "정상", na.rm = TRUE) / nrow(data) * 100,
                 1)
    
    data %>% count(battery_status) %>% e_charts(battery_status) %>%
      e_pie(n, name = "수량", radius = c("55%", "75%")) %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c}대 ({d}%)") %>%
      e_title(
        text = paste0(pct, "%"),
        subtext = "정상 비율",
        left = 'center',
        top = '40%',
        textStyle = list(fontSize = 22, fontWeight = 'bold')
      ) %>%
      e_legend(bottom = 0)
  })
  
  output$patch_expiry_chart <- renderEcharts4r({
    data <- dashboard_data()
    req(nrow(data) > 0)
    data <- data %>% mutate(`패치.유효기간` = lubridate::as_date(`패치.유효기간`))
    
    data <- data %>% mutate(
      patch_status = case_when(
        is.na(`패치.유효기간`) ~ "정보 없음",
        `패치.유효기간` < today ~ "만료",
        `패치.유효기간` < (today + months(6)) ~ "6개월 내 만료",
        TRUE ~ "정상"
      )
    )
    
    pct <- round(sum(data$patch_status == "정상", na.rm = TRUE) / nrow(data) * 100,
                 1)
    
    data %>% count(patch_status) %>% e_charts(patch_status) %>%
      e_pie(n, name = "수량", radius = c("55%", "75%")) %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c}대 ({d}%)") %>%
      e_title(
        text = paste0(pct, "%"),
        subtext = "정상 비율",
        left = 'center',
        top = '40%',
        textStyle = list(fontSize = 22, fontWeight = 'bold')
      ) %>%
      e_legend(bottom = 0)
  })
  
  output$facility_type_chart_1 <- renderEcharts4r({
    dashboard_data() %>%
      filter(!is.na(분류1) &
               분류1 != "") %>% count(분류1, sort = TRUE) %>% top_n(10, n) %>%
      e_charts(분류1) %>% e_bar(n, name = "대수") %>% e_tooltip(trigger = 'axis') %>%
      e_grid(containLabel = TRUE) %>% e_flip_coords()
  })
  
  output$facility_type_chart_2 <- renderEcharts4r({
    dashboard_data() %>%
      filter(!is.na(분류2) &
               분류2 != "") %>% count(분류2, sort = TRUE) %>% top_n(10, n) %>%
      e_charts(분류2) %>% e_bar(n, name = "대수") %>% e_tooltip(trigger = 'axis') %>%
      e_grid(containLabel = TRUE) %>% e_flip_coords()
  })
  
  
  # --- 7. 장비 상세 목록 탭 로직 (수정됨) ---
  
  # [새로 추가] 목록 탭의 시/도 필터 채우기
  observe({
    env <- data_env()
    req(env)
    sido_choices <- env$sido_list
    updateSelectInput(session, "list_sido_filter", choices = sido_choices, selected = isolate(input$sido_filter))
  })
  
  # [새로 추가] 목록 탭의 시/군/구 필터 UI 렌더링
  output$list_sigungu_filter_ui <- renderUI({
    req(input$list_sido_filter, data_env())
    choices <- if (input$list_sido_filter == "전체") {
      c("전체")
    } else {
      c("전체", sort(unique(data_env()$aed_data[sido_name == input$list_sido_filter & !is.na(sigungu_name_display), sigungu_name_display])))
    }
    selectInput("list_sigungu_filter", "시/군/구:", choices = choices, selected = "전체")
  })
  
  # <<< ### `apply_filters_btn` 클릭 시 테이블 데이터 및 목록 필터 업데이트 ### >>>
  observeEvent(input$apply_filters_btn, {
    data_for_table(filtered_data())
    
    # [수정/추가] 지도 필터 값을 목록 탭 필터에 동기화
    updateSelectInput(session, "list_sido_filter", selected = input$sido_filter)
    # list_sigungu_filter는 list_sido_filter 변경에 따라 자동으로 업데이트되므로, 여기서는 sido만 업데이트
  })
  
  # <<< ### 체크박스 및 지역 필터링을 위한 '반응형' 리액티브 수정 ### >>>
  table_data_filtered <- reactive({
    # 필터 UI가 준비될 때까지 기다립니다.
    req(input$list_sido_filter)
    
    # [수정] 전국 단위의 과도한 데이터 조회를 막기 위해 시/도 선택을 필수로 만듭니다.
    if (input$list_sido_filter == "전체") {
      return(data.frame()) # 시/도가 '전체'일 경우 빈 테이블 반환
    }
    
    # [수정] data_for_table() 대신 전체 데이터(data_env()$aed_data)에서 직접 필터링합니다.
    env <- data_env()
    req(env, !is.null(env$aed_data))
    data <- env$aed_data
    
    # 목록 탭의 지역 필터 적용
    sido_val <- input$list_sido_filter
    sigungu_val <- input$list_sigungu_filter
    
    data <- data[sido_name == sido_val]
    if (!is.null(sigungu_val) && sigungu_val != "전체") {
      data <- data[sigungu_name_display == sigungu_val]
    }
    
    # 기존 문제 유형 필터 적용
    issue_filters <- input$issue_filter
    if (!is.null(issue_filters) && length(issue_filters) > 0) {
      conditions <- lapply(issue_filters, function(cond) {
        switch(
          cond,
          "no_location" = quote(!has_location),
          "is_expired_overall" = quote(is_expired_overall == TRUE),
          "is_battery_expired" = quote(is_battery_expired == TRUE),
          "is_patch_expired" = quote(is_patch_expired == TRUE),
          "is_inspection_needed" = quote(inspection_status == "점검 필요")
        )
      })
      final_expr <- Reduce(function(a, b) call("|", a, b), conditions)
      data <- data[eval(final_expr), ]
    }
    return(data)
  })
  
  output$table <- DT::renderDT({
    data_to_display <- table_data_filtered()
    
    # [수정] 사용자 안내 메시지 강화
    validate(
      need(input$list_sido_filter != "전체", "먼저 조회할 시/도를 선택해주세요. (전국 단위 조회는 제공되지 않습니다)"),
      need(nrow(data_to_display) > 0, "선택하신 조건에 해당하는 장비가 없습니다.")
    )
    
    data_to_display <- data_to_display %>%
      mutate(
        위치보기 = if_else(
          has_location,
          paste0('<div style="text-align:center;"><button class="location-btn" data-id="', unique_id, '" style="background-color:#3B82F6; color:white; border:none; border-radius:4px; padding: 5px 15px; font-weight:bold; cursor:pointer;">지도</button></div>'),
          '<div style="text-align:center;">-</div>'
        ),
        신고하기 = paste0('<div style="text-align:center;"><button class="report-aed-btn" data-id="', unique_id, '" style="background-color:#dc3545; color:white; border:none; border-radius:4px; padding: 5px 15px; font-weight:bold; cursor:pointer;">신고</button></div>')
      ) %>%
      select(
        "위치보기", "신고하기", "관리번호", "장비연번", "설치기관명", "설치위치",
        "시/도" = "sido_name", "시/군/구" = "sigungu_name_raw", "비고" = "issue_level",
        "최근 점검일" = "최근점검일", "배터리유효기간" = "배터리.유효기간", "패치유효기간" = "패치.유효기간",
        "교체예정일" = "교체.예정일", "분류1", "분류2", "분류3", "분류4"
      )
    
    datatable(
      data_to_display,
      escape = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(list(extend = 'excel', text = '엑셀 저장'), list(extend = 'csv', text = 'CSV 저장')),
        pageLength = 15, autoWidth = TRUE, scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Korean.json")
      ),
      class = "cell-border stripe nowrap"
    )
  }, server = FALSE)
  
  
  
  
  # server.R의 server 함수 최상단 또는 적절한 위치에 추가
  
  # [수정 및 통합] '지도' 버튼 클릭 시 위치로 이동하는 최종 코드
  observeEvent(input$goto_location, {
    req(input$goto_location)
    
    # 1. 클릭된 장비의 ID를 가져옴
    target_id <- input$goto_location
    
    # 2. 현재 월 데이터(data_env()$aed_data)에서 해당 ID의 장비 정보를 찾음
    target_aed <- data_env()$aed_data[unique_id == target_id, ]
    
    # 3. 해당 장비 정보가 있을 경우에만 실행
    if (nrow(target_aed) > 0) {
      
      # leafletProxy를 사용하여 지도를 제어
      leafletProxy("map") %>%
        # 4. flyTo 함수로 부드럽게 해당 위치로 이동 및 확대
        flyTo(lng = target_aed$경도, lat = target_aed$위도, zoom = 17) %>%
        
        # 5. 기존 마커 팝업을 모두 닫고, 새로운 팝업을 띄움
        clearPopups() %>%
        addPopups(
          lng = target_aed$경도,
          lat = target_aed$위도,
          # 팝업에 표시될 내용을 HTML로 생성
          popup = paste(
            "<strong>장소:</strong>", target_aed$설치기관명, "<br>",
            "<strong>주소:</strong>", target_aed$`설치장소.주소`, "<br>",
            "<strong>상태:</strong>", target_aed$issue_level
          )
        )
    }
  })
  
  
  
  
  # server.R 파일 안 적당한 곳에 아래 코드를 새로 추가하세요.
  
  # [핵심 추가] 하단 배너의 '확인' 버튼 클릭 시 배너 숨기기
  observeEvent(input$close_banner_btn, {
    shinyjs::hide("loading_banner", anim = TRUE, animType = "fade") # 부드럽게 사라지는 효과
  })
  
  
  
  
  
  
  observeEvent(input$generate_report_btn, {
    selected_rows <- input$table_rows_selected
    if (length(selected_rows) == 0) {
      showNotification("메시지를 생성할 장비를 목록에서 먼저 선택해주세요.", type = "warning")
      return()
    }
    
    # <<< ### `table_data_filtered()` 사용 ### >>>
    report_data <- table_data_filtered()[selected_rows, ] %>% filter(issue_level != "정상")
    if (nrow(report_data) == 0) {
      showModal(modalDialog(title = "알림", "선택하신 장비 중 점검이 필요한 항목이 없습니다.", footer = modalButton("닫기")))
      return()
    }
    msg_body <- report_data %>%
      rowwise() %>%
      mutate(
        battery_text = if_else(is.na(배터리.유효기간), "정보없음", as.character(배터리.유효기간)),
        patch_text = if_else(is.na(패치.유효기간), "정보없음", as.character(패치.유효기간)),
        inspect_text = if_else(is.na(최근점검일), "정보없음", as.character(최근점검일)),
        text = paste0(
          "- 기관/위치: ", `설치기관명`, " / ", `설치위치`, "\n",
          "  (주소: ", `설치장소.주소`, ")\n",
          "  문제사항: ", issue_level, "\n",
          "  (배터리: ", battery_text, ", 패치: ", patch_text, ", 최근점검: ", inspect_text, ")"
        )
      ) %>%
      ungroup() %>% pull(text) %>% paste(collapse = "\n\n")
    
    full_message <- paste0(
      "※ 자동심장충격기(AED) 점검 요청 ※\n\n",
      "아래 장비에 대한 조속한 점검 및 조치를 요청드립니다.\n\n",
      "------------------------------------\n", msg_body, "\n------------------------------------\n",
      "총 ", nrow(report_data), "대 점검 필요"
    )
    
    showModal(modalDialog(
      title = "점검 요청 메시지", size = "l",
      textAreaInput("report_text", label = NULL, value = full_message, rows = 15, width = "100%"),
      footer = tagList(modalButton("닫기"), rclipButton("copy_btn", "메시지 복사", "report_text", icon = icon("copy"), class = "btn-success"))
    ))
  })
  
  
  # --- 8. 상세 분석 탭 로직 ---
  
  # 8-1. 월별 비교 분석
  data_env_A <- reactive({
    req(input$month_a_select)
    load_data_env(input$month_a_select)
  })
  
  data_env_B <- reactive({
    req(input$month_b_select)
    load_data_env(input$month_b_select)
  })
  
  comparison_summary <- eventReactive(list(
    data_env_A(),
    data_env_B(),
    input$month_a_select,
    input$month_b_select
  ),
  {
    env_a <- data_env_A()
    env_b <- data_env_B()
    req(env_a, env_b)
    
    if (input$month_a_select == input$month_b_select) {
      showNotification("서로 다른 월을 선택해주세요.", type = "warning")
      return(NULL)
    }
    
    aed_data_a <- tibble::as_tibble(env_a$aed_data) %>%
      filter(!is.na(sido_name) & sido_name != "")
    
    aed_data_b <- tibble::as_tibble(env_b$aed_data) %>%
      filter(!is.na(sido_name) & sido_name != "")
    
    summary_a <- aed_data_a %>% summarise(
      total = n(),
      ready = sum(is_ready_for_use, na.rm = TRUE),
      inspected = sum(inspection_status == "최근 점검", na.rm = TRUE),
      expired = sum(is_expired_overall, na.rm = TRUE)
    )
    summary_b <- aed_data_b %>% summarise(
      total = n(),
      ready = sum(is_ready_for_use, na.rm = TRUE),
      inspected = sum(inspection_status == "최근 점검", na.rm = TRUE),
      expired = sum(is_expired_overall, na.rm = TRUE)
    )
    
    sido_summary_a <- aed_data_a %>% group_by(sido_name) %>%
      summarise(
        total_a = n(),
        inspected_a = sum(inspection_status == "최근 점검", na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(inspection_pct_a = round(inspected_a / total_a * 100, 1))
    
    sido_summary_b <- aed_data_b %>% group_by(sido_name) %>%
      summarise(
        total_b = n(),
        inspected_b = sum(inspection_status == "최근 점검", na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(inspection_pct_b = round(inspected_b / total_b * 100, 1))
    
    sido_comparison <- full_join(sido_summary_a, sido_summary_b, by = "sido_name") %>%
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
      mutate(
        total_change = total_b - total_a,
        inspection_pct_change = inspection_pct_b - inspection_pct_a
      )
    
    list(
      summary_a = summary_a,
      summary_b = summary_b,
      sido_comparison = sido_comparison
    )
  })
  
  output$comp_total_aed_box <- renderUI({
    res <- comparison_summary()
    req(res)
    change <- res$summary_b$total - res$summary_a$total
    value_box(
      title = "총 AED 수",
      value = prettyNum(res$summary_b$total, big.mark = ","),
      showcase = bs_icon("heart-pulse-fill"),
      class = "value-box-total",
      p(paste0(
        "변화: ",
        if (change >= 0)
          "+"
        else
          "",
        prettyNum(change, big.mark = ","),
        "대"
      ))
    )
  })
  
  output$comp_ready_aed_box <- renderUI({
    res <- comparison_summary()
    req(res)
    pct_a <- round(res$summary_a$ready / res$summary_a$total * 100, 1)
    pct_b <- round(res$summary_b$ready / res$summary_b$total * 100, 1)
    change <- pct_b - pct_a
    value_box(
      title = "사용 가능률",
      value = paste0(pct_b, "%"),
      showcase = bs_icon("shield-fill-check"),
      class = "value-box-ready",
      p(paste0(
        "변화: ", if (change >= 0)
          "+"
        else
          "", round(change, 1), "%p"
      ))
    )
  })
  
  output$comp_inspection_box <- renderUI({
    res <- comparison_summary()
    req(res)
    pct_a <- round(res$summary_a$inspected / res$summary_a$total * 100, 1)
    pct_b <- round(res$summary_b$inspected / res$summary_b$total * 100, 1)
    change <- pct_b - pct_a
    value_box(
      title = "최근 점검률",
      value = paste0(pct_b, "%"),
      showcase = bs_icon("check-circle-fill"),
      class = "value-box-inspection",
      p(paste0(
        "변화: ", if (change >= 0)
          "+"
        else
          "", round(change, 1), "%p"
      ))
    )
  })
  
  output$comp_expired_box <- renderUI({
    res <- comparison_summary()
    req(res)
    change <- res$summary_b$expired - res$summary_a$expired
    value_box(
      title = "장비 만료 대수",
      value = prettyNum(res$summary_b$expired, big.mark = ","),
      showcase = bs_icon("exclamation-triangle-fill"),
      class = "value-box-expired",
      p(paste0(
        "변화: ",
        if (change >= 0)
          "+"
        else
          "",
        prettyNum(change, big.mark = ","),
        "대"
      ))
    )
  })
  
  output$comparison_change_chart <- renderEcharts4r({
    res <- comparison_summary()
    req(res)
    res$sido_comparison %>%
      arrange(desc(total_b)) %>%
      e_charts(sido_name) %>%
      e_bar(total_a, name = paste(input$month_a_select, "월 총 대수")) %>%
      e_bar(total_b, name = paste(input$month_b_select, "월 총 대수")) %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(bottom = 0) %>%
      e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
      e_grid(containLabel = TRUE)
  })
  
  output$comparison_inspection_chart <- renderEcharts4r({
    res <- comparison_summary()
    req(res)
    res$sido_comparison %>% arrange(desc(inspection_pct_b)) %>% e_charts(sido_name) %>%
      e_bar(inspection_pct_a,
            name = paste(input$month_a_select, "월 점검률(%)")) %>%
      e_bar(inspection_pct_b,
            name = paste(input$month_b_select, "월 점검률(%)")) %>%
      e_tooltip(trigger = "axis") %>% e_legend(bottom = 0) %>%
      e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>% e_grid(containLabel = TRUE)
  })
  
  output$comparison_detail_table <- renderDT({
    res <- comparison_summary()
    req(res)
    display_data <- res$sido_comparison %>%
      select(
        "시도" = sido_name,
        "총 대수(A)" = total_a,
        "총 대수(B)" = total_b,
        "증감" = total_change,
        "점검률(A)" = inspection_pct_a,
        "점검률(B)" = inspection_pct_b,
        "점검률 변화(%p)" = inspection_pct_change
      )
    datatable(display_data,
              rownames = FALSE,
              options = get_stylish_dt_options())
  }, server = FALSE)
  
  
  # --- 9. 심층 분석 탭 로직 (이하 기존과 동일) ---
  
  # 9-1. 유동인구 기반
  output$pop_analysis_summary_ui <- renderUI({
    data <- pop_analysis_data()
    req(data)
    top_region <- data %>% st_drop_geometry() %>% filter(!is.na(vulnerability_index)) %>%
      filter(vulnerability_index == max(vulnerability_index, na.rm = TRUE)) %>% slice(1)
    tagList(
      value_box(
        title = "평균 AED 필요지수",
        value = round(mean(data$vulnerability_index, na.rm = TRUE), 3),
        showcase = bsicons::bs_icon("geo-alt-fill"),
        class = "bg-light border"
      ),
      p(
        strong("최우선 필요 지역: "),
        top_region$sigungu_name_display,
        " (",
        round(top_region$vulnerability_index, 3),
        "점)",
        class = "mt-3"
      )
    )
  })
  
  output$pop_analysis_table <- DT::renderDT({
    pop_analysis_data() %>% st_drop_geometry() %>%
      select(
        "지역명" = sigungu_name_display,
        "필요지수" = vulnerability_index,
        "유동인구" = filtered_floating_pop,
        "AED밀도" = aed_per_area
      ) %>%
      arrange(desc(필요지수)) %>% head(10) %>% mutate(across(where(is.numeric), ~
                                                           round(.x, 3))) %>%
      datatable(rownames = FALSE,
                options = list(dom = 't', ordering = FALSE))
  })
  
  pop_analysis_data <- eventReactive(input$run_pop_analysis, {
    waiter$show()
    on.exit(waiter$hide())
    env <- data_env()
    req(env, "floating_pop_data" %in% names(env), env$sigungu_sf)
    
    if (nrow(env$floating_pop_data) == 0) {
      showNotification("분석할 유동인구 데이터가 없습니다. 데이터 전처리 과정을 확인해주세요.",
                       type = "error",
                       duration = 7)
      return(NULL)
    }
    
    req(
      input$pop_age_filter,
      input$pop_time_filter,
      input$pop_weight,
      input$aed_density_weight_pop
    )
    
    filtered_pop <- env$floating_pop_data
    if (input$pop_age_filter != "전체") {
      filtered_pop <- filtered_pop %>% filter(type == "연령대", group == input$pop_age_filter)
    }
    if (input$pop_time_filter != "전체") {
      filtered_pop <- filtered_pop %>% filter(type == "시간대", group == input$pop_time_filter)
    }
    
    sigungu_pop_filtered <- filtered_pop %>%
      group_by(sido_name, sigungu_name_raw) %>%
      summarise(
        filtered_floating_pop = sum(population, na.rm = TRUE),
        .groups = 'drop'
      )
    
    if (input$pop_age_filter == "전체" &&
        input$pop_time_filter == "전체") {
      num_time_slots <- length(unique(env$floating_pop_data$group[env$floating_pop_data$type == "시간대"]))
      if (num_time_slots > 0) {
        sigungu_pop_filtered <- sigungu_pop_filtered %>% mutate(filtered_floating_pop = filtered_floating_pop / num_time_slots)
      }
    }
    
    env$sigungu_sf %>%
      left_join(sigungu_pop_filtered,
                by = c("sido_name", "sigungu_name_raw")) %>%
      mutate(filtered_floating_pop = ifelse(is.na(filtered_floating_pop), 0, filtered_floating_pop)) %>%
      mutate(aed_per_area = ifelse(area_km2 > 0, count / area_km2, 0)) %>%
      mutate(
        pop_norm = if (max(filtered_floating_pop, na.rm = T) > 0)
          scales::rescale(filtered_floating_pop)
        else
          0,
        aed_density_norm = if (max(aed_per_area, na.rm = T) > 0)
          scales::rescale(aed_per_area)
        else
          0
      ) %>%
      mutate(
        vulnerability_index = (pop_norm * input$pop_weight) - (aed_density_norm * input$aed_density_weight_pop)
      )
  })
  
  output$pop_analysis_map <- renderLeaflet({
    create_analysis_map()
  })
  
  observeEvent(pop_analysis_data(), {
    data <- pop_analysis_data()
    req(data, nrow(data) > 0)
    
    pal <- colorNumeric("OrRd", domain = data$vulnerability_index)
    proxy <- leafletProxy("pop_analysis_map", data = data) %>% clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor = ~ pal(vulnerability_index),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~ paste0(
          sigungu_name_display,
          ": ",
          round(vulnerability_index, 3)
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ vulnerability_index,
        opacity = 0.7,
        title = "AED 필요지수",
        position = "bottomright",
        className = "info legend desktop-legend" # <<< 이 줄을 추가하세요
      )
    is_legend_active(TRUE) # [추가]
    
    proxy %>% flyToBounds(
      lng1 = min(st_bbox(data)[1]),
      lat1 = min(st_bbox(data)[2]),
      lng2 = max(st_bbox(data)[3]),
      lat2 = max(st_bbox(data)[4])
    )
  })
  
  # 9-2. 관리 취약 지역
  issue_data_reac <- eventReactive(input$run_issue_analysis, {
    waiter$show()
    on.exit(waiter$hide())
    env <- data_env()
    req(env)
    req(input$issue_type_filter)
    
    ninety_days_later <- today() + days(90)
    data_to_filter <- env$aed_data
    if (input$issue_sido_filter != "전국") {
      data_to_filter <- data_to_filter %>% filter(sido_name == input$issue_sido_filter)
    }
    
    data_to_filter %>%
      filter(
        (
          "exp_imminent" %in% input$issue_type_filter &
            !is.na(교체.예정일) & 교체.예정일 <= ninety_days_later
        ) |
          (
            "batt_imminent" %in% input$issue_type_filter &
              !is.na(배터리.유효기간) & 배터리.유효기간 <= ninety_days_later
          ) |
          (
            "patch_imminent" %in% input$issue_type_filter &
              !is.na(패치.유효기간) & 패치.유효기간 <= ninety_days_later
          ) |
          (
            "insp_needed" %in% input$issue_type_filter &
              inspection_status == "점검 필요"
          )
      ) %>%
      filter(has_location == TRUE)
  })
  
  output$issue_heatmap <- renderLeaflet({
    create_analysis_map()
  })
  
  observeEvent(issue_data_reac(), {
    data <- issue_data_reac()
    proxy <- leafletProxy("issue_heatmap", data = data) %>% clearHeatmap()
    if (nrow(data) > 0) {
      proxy %>% addHeatmap(
        lng = ~ 경도,
        lat = ~ 위도,
        intensity = 1,
        blur = 20,
        max = 0.05,
        radius = 15
      )
      bbox <- st_bbox(st_as_sf(data, coords = c("경도", "위도"), crs = 4326))
      proxy %>% flyToBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    } else {
      showNotification("선택된 조건에 해당하는 관리 필요 AED가 없습니다.", type = "warning")
    }
  })
  
  output$issue_summary_ui <- renderUI({
    data <- issue_data_reac()
    req(nrow(data) > 0)
    top_region <- data %>% filter(!is.na(sigungu_name_display)) %>% count(sigungu_name_display, sort = TRUE) %>% slice(1)
    tagList(
      value_box(
        title = "총 관리 필요 AED",
        value = prettyNum(nrow(data), big.mark = ","),
        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
        theme = "bg-warning"
      ),
      p(
        strong("최다 발생 지역: "),
        top_region$sigungu_name_display,
        " (",
        top_region$n,
        "건)",
        class = "mt-3"
      )
    )
  })
  
  output$issue_table <- DT::renderDT({
    issue_data_reac() %>% filter(!is.na(sigungu_name_display)) %>%
      count(sigungu_name_display, name = "관리필요_수") %>% arrange(desc(관리필요_수)) %>% head(10) %>%
      rename("지역명" = sigungu_name_display, "건수" = 관리필요_수) %>%
      datatable(rownames = FALSE,
                options = list(dom = 't', ordering = FALSE))
  })
  
  # 9-3. 설치 최적 입지
  optimization_data_reac <- eventReactive(input$run_optimization_analysis, {
    waiter$show()
    on.exit(waiter$hide())
    env <- data_env()
    req(env)
    req(input$er_dist_weight, input$aed_density_weight_optim)
    data <- env$sigungu_sf
    if (input$optim_sido_filter != "전국") {
      data <- data %>% filter(sido_name == input$optim_sido_filter)
      if (!is.null(input$optim_sigungu_filter) &&
          input$optim_sigungu_filter != "전체") {
        data <- data %>% filter(sigungu_name_display == input$optim_sigungu_filter)
      }
    }
    data %>%
      mutate(
        dist_score_norm = scales::rescale(log(nearest_er_dist_km + 1)),
        aed_score_norm = scales::rescale(-log(count_per_100k + 1)),
        dist_score_norm = ifelse(is.na(dist_score_norm), 0, dist_score_norm),
        aed_score_norm = ifelse(is.na(aed_score_norm), 0, aed_score_norm),
        final_vulnerability = (dist_score_norm * input$er_dist_weight) + (aed_score_norm * input$aed_density_weight_optim)
      )
  })
  
  output$optimization_map <- renderLeaflet({
    create_analysis_map()
  })
  
  observeEvent(optimization_data_reac(), {
    data <- optimization_data_reac()
    req(data,
        nrow(data) > 0,
        "final_vulnerability" %in% names(data))
    pal <- colorNumeric("Reds",
                        domain = data$final_vulnerability,
                        na.color = "#808080")
    proxy <- leafletProxy("optimization_map", data = data) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor = ~ pal(final_vulnerability),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~ paste0(
          sigungu_name_display,
          ": ",
          round(final_vulnerability, 2)
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ final_vulnerability,
        opacity = 0.7,
        title = "최종 취약지수",
        position = "bottomright",
        className = "info legend desktop-legend" # <<< 이 줄을 추가하세요
      )
    is_legend_active(TRUE) # [추가]
    proxy %>% flyToBounds(
      lng1 = min(st_bbox(data)[1]),
      lat1 = min(st_bbox(data)[2]),
      lng2 = max(st_bbox(data)[3]),
      lat2 = max(st_bbox(data)[4])
    )
  })
  
  output$optimization_summary_ui <- renderUI({
    data <- optimization_data_reac()
    req(data)
    top_region <- data %>% st_drop_geometry() %>% filter(final_vulnerability == max(final_vulnerability, na.rm = TRUE)) %>% slice(1)
    tagList(
      value_box(
        title = "평균 취약지수",
        value = round(mean(data$final_vulnerability, na.rm = TRUE), 2),
        showcase = bsicons::bs_icon("clipboard2-pulse"),
        theme = "bg-success"
      ),
      p(
        strong("최우선 확충 필요 지역: "),
        top_region$sigungu_name_display,
        " (",
        round(top_region$final_vulnerability, 2),
        "점)",
        class = "mt-3"
      )
    )
  })
  
  output$optimization_table <- DT::renderDT({
    optimization_data_reac() %>% st_drop_geometry() %>%
      select(sigungu_name_display, final_vulnerability) %>% arrange(desc(final_vulnerability)) %>% head(10) %>%
      mutate(final_vulnerability = round(final_vulnerability, 2)) %>%
      rename("지역명" = sigungu_name_display, "취약지수" = final_vulnerability) %>%
      datatable(rownames = FALSE,
                options = list(dom = 't', ordering = FALSE))
  })
  
  # 9-4. 서비스 커버리지
  # [수정] 아래 함수 전체를 복사하여 기존 코드를 덮어쓰세요.
  # 8-7. 심층 분석 - 서비스 커버리지
  # [1. coverage_analysis_data 함수 전체를 이 코드로 교체하십시오]
  coverage_analysis_data <- eventReactive(input$run_coverage_analysis, {
    waiter$show()
    sido_choice <- isolate(input$coverage_sido_filter)
    if (sido_choice == "전국") {
      showNotification("이 분석은 '전국' 단위로 실행할 수 없습니다. 시/도를 선택해주세요.", type = "error", duration = 10)
      waiter$hide()
      return(NULL)
    }
    
    on.exit(waiter$hide())
    env <- data_env()
    req(env)
    
    sgg_to_analyze_raw <- env$sigungu_sf
    if (input$coverage_sido_filter != "전국") {
      sgg_to_analyze_raw <- sgg_to_analyze_raw %>% filter(sido_name == input$coverage_sido_filter)
    }
    req(nrow(sgg_to_analyze_raw) > 0)
    
    aed_to_analyze <- env$aed_data %>% semi_join(st_drop_geometry(sgg_to_analyze_raw), by = "sigungu_name_display")
    hospital_to_analyze <- env$hospital_data %>% semi_join(st_drop_geometry(sgg_to_analyze_raw), by = "sigungu_name_display")
    
    crs_proj <- 5186
    aed_buffer_dist <- input$aed_buffer_dist
    er_buffer_factor <- input$er_buffer_factor
    
    aed_points_sf <- aed_to_analyze %>% filter(has_location == TRUE) %>% st_as_sf(coords = c("경도", "위도"), crs = 4326)
    hospital_points_sf <- hospital_to_analyze %>% st_as_sf()
    
    coverage_geometries <- list()
    if (nrow(aed_points_sf) > 0) {
      coverage_geometries$aed <- st_transform(aed_points_sf, crs_proj) %>% st_buffer(dist = aed_buffer_dist) %>% st_geometry()
    }
    if (nrow(hospital_points_sf) > 0) {
      hospital_points_proj <- st_transform(hospital_points_sf, crs_proj)
      if ("권역응급의료센터" %in% hospital_points_proj$의료기관분류) {
        coverage_geometries$er1 <- hospital_points_proj %>% filter(의료기관분류 == "권역응급의료센터") %>% st_buffer(dist = 5000 * er_buffer_factor) %>% st_geometry()
      }
      if ("지역응급의료센터" %in% hospital_points_proj$의료기관분류) {
        coverage_geometries$er2 <- hospital_points_proj %>% filter(의료기관분류 == "지역응급의료센터") %>% st_buffer(dist = 5000 * er_buffer_factor) %>% st_geometry()
      }
      if ("지역응급의료기관" %in% hospital_points_proj$의료기관분류) {
        coverage_geometries$er3 <- hospital_points_proj %>% filter(의료기관분류 == "지역응급의료기관") %>% st_buffer(dist = 5000 * er_buffer_factor) %>% st_geometry()
      }
    }
    
    if (length(coverage_geometries) == 0) {
      showNotification("선택된 지역에 분석할 AED 또는 응급의료기관 데이터가 없습니다.", type = "warning")
      sgg_to_analyze_raw$uncovered_pct_service <- 100
      return(sgg_to_analyze_raw)
    }
    
    total_coverage_proj <- st_union(do.call(c, coverage_geometries)) %>% st_make_valid()
    sgg_proj <- st_transform(st_make_valid(sgg_to_analyze_raw), crs_proj)
    
    uncovered_percentages <- purrr::map_dbl(1:nrow(sgg_proj), function(i) {
      sgg_geom <- st_geometry(sgg_proj[i, ])
      total_area <- st_area(sgg_geom)
      
      if (as.numeric(total_area) == 0) return(100)
      
      intersection_geom <- suppressWarnings(st_intersection(sgg_geom, total_coverage_proj))
      
      covered_area <- if (length(intersection_geom) == 0 || st_is_empty(intersection_geom)) {
        0
      } else {
        st_area(intersection_geom)
      }
      
      uncovered_pct <- 100 * (1 - (as.numeric(covered_area) / as.numeric(total_area)))
      return(max(0, min(100, round(uncovered_pct, 1))))
    })
    
    final_data <- sgg_to_analyze_raw %>%
      mutate(uncovered_pct_service = uncovered_percentages)
    
    return(final_data)
  })
  
  output$coverage_map <- renderLeaflet({
    create_analysis_map()
  })
  
  observeEvent(coverage_analysis_data(), {
    data <- coverage_analysis_data()
    req(data, nrow(data) > 0)
    pal <- colorNumeric(
      "YlOrRd",
      domain = data$uncovered_pct_service,
      na.color = "#808080"
    )
    proxy <- leafletProxy("coverage_map", data = data) %>% clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor = ~ pal(uncovered_pct_service),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~ paste0(
          sigungu_name_display,
          ": ",
          uncovered_pct_service,
          "% 미보장"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ uncovered_pct_service,
        opacity = 0.7,
        title = "서비스 미보장 비율(%)",
        position = "bottomright",
        layerId = "coverage_legend",
        className = "info legend desktop-legend" # <<< 이 줄을 추가하세요
      )
    
    proxy %>% flyToBounds(
      lng1 = min(st_bbox(data)[1]),
      lat1 = min(st_bbox(data)[2]),
      lng2 = max(st_bbox(data)[3]),
      lat2 = max(st_bbox(data)[4])
    )
  })
  
  output$coverage_summary_ui <- renderUI({
    data <- coverage_analysis_data()
    req(data)
    top_region <- data %>% st_drop_geometry() %>% filter(uncovered_pct_service == max(uncovered_pct_service, na.rm = TRUE)) %>% slice(1)
    tagList(
      value_box(
        title = "평균 서비스 미보장율",
        value = paste0(round(
          mean(data$uncovered_pct_service, na.rm = TRUE), 1
        ), "%"),
        showcase = bsicons::bs_icon("shield-slash"),
        theme = "bg-info"
      ),
      p(
        strong("가장 소외된 지역: "),
        top_region$sigungu_name_display,
        " (",
        top_region$uncovered_pct_service,
        "%)",
        class = "mt-3"
      )
    )
  })
  
  output$coverage_table <- DT::renderDT({
    data <- coverage_analysis_data()
    req(data)
    
    data %>% st_drop_geometry() %>%
      select("지역명" = sigungu_name_display, "미보장 비율(%)" = uncovered_pct_service) %>%
      arrange(desc(`미보장 비율(%)`)) %>% head(10) %>%
      datatable(rownames = FALSE,
                options = list(dom = 't', ordering = FALSE))
  })
  
  # 9-5. 골든아워 접근성
  observeEvent(input$toggle_simulation_mode, {
    current_mode <- simulation_mode()
    simulation_mode(!current_mode)
    
    if (!current_mode) {
      showNotification("시뮬레이션 모드 활성화. 지도에 가상 AED를 추가할 위치를 클릭하세요.", type = "message")
    } else {
      showNotification("시뮬레이션 모드 비활성화.", type = "warning")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$golden_time_map_click, {
    if (simulation_mode()) {
      click <- input$golden_time_map_click
      new_aed <- data.frame(lng = click$lng, lat = click$lat)
      updated_aeds <- rbind(simulated_aeds(), new_aed)
      simulated_aeds(updated_aeds)
      showNotification(paste("가상 AED 추가:", nrow(updated_aeds), "개"), type = "message")
    }
  })
  
  observe({
    proxy <- leafletProxy("golden_time_map")
    sim_data <- simulated_aeds()
    
    proxy %>% clearGroup("simulated_aeds_group")
    
    if (nrow(sim_data) > 0) {
      proxy %>% addAwesomeMarkers(
        data = sim_data,
        lng = ~ lng,
        lat = ~ lat,
        group = "simulated_aeds_group",
        icon = awesomeIcons(
          icon = 'plus',
          library = 'fa',
          markerColor = 'blue',
          iconColor = '#FFF'
        ),
        popup = "가상 AED 위치"
      )
    }
  })
  
  observeEvent(input$reset_simulation, {
    simulated_aeds(data.frame(lng = numeric(), lat = numeric()))
    showNotification("가상 AED가 모두 삭제되었습니다.", type = "warning")
  })
  
  # [수정] 아래 함수 전체를 복사하여 기존 코드를 덮어쓰세요.
  # [2. golden_time_data_reac 함수 전체를 이 코드로 교체하십시오]
  golden_time_data_reac <- reactive({
    req(input$run_golden_time_analysis > 0 | nrow(simulated_aeds()) > 0)
    if (isolate(input$run_golden_time_analysis) > 0) {
      golden_time_waiter$show()
    }
    
    sido_choice <- isolate(input$golden_time_sido_filter)
    if (sido_choice == "전국") {
      showNotification("이 분석은 '전국' 단위로 실행할 수 없습니다. 시/도를 선택해주세요.", type = "error", duration = 10)
      golden_time_waiter$hide()
      return(NULL)
    }
    
    on.exit(golden_time_waiter$hide())
    sim_aeds <- simulated_aeds()
    env <- data_env()
    req(env, input$walking_speed_kmh)
    
    sgg_data_raw <- env$sigungu_sf
    aed_data <- env$aed_data
    if (input$golden_time_sido_filter != "전국") {
      sgg_data_raw <- sgg_data_raw %>% filter(sido_name == input$golden_time_sido_filter)
      aed_data <- aed_data %>% filter(sido_name == input$golden_time_sido_filter)
    }
    
    walking_dist_m <- (input$walking_speed_kmh * 1000 / 60) * 4
    crs_proj <- 5186
    
    usable_aed_sf_orig <- aed_data %>%
      filter(is_ready_for_use == TRUE, has_location == TRUE) %>%
      st_as_sf(coords = c("경도", "위도"), crs = 4326)
    
    sim_aed_sf <- if (nrow(sim_aeds) > 0) {
      st_as_sf(sim_aeds, coords = c("lng", "lat"), crs = 4326)
    } else {
      NULL
    }
    
    all_aeds_sf <- if (!is.null(sim_aed_sf)) {
      rbind(usable_aed_sf_orig %>% select(geometry), sim_aed_sf %>% select(geometry))
    } else {
      usable_aed_sf_orig
    }
    
    if (nrow(all_aeds_sf) == 0) {
      showNotification("분석할 사용가능 AED가 없습니다.", type="warning")
      sgg_data_raw$uncovered_pct_golden <- 100
      return(sgg_data_raw)
    }
    
    aed_coverage_proj <- st_transform(all_aeds_sf, crs_proj) %>% 
      st_buffer(dist = walking_dist_m) %>% 
      st_union() %>% 
      st_make_valid()
    
    sgg_proj <- st_transform(st_make_valid(sgg_data_raw), crs_proj)
    
    uncovered_percentages <- purrr::map_dbl(1:nrow(sgg_proj), function(i) {
      sgg_geom <- st_geometry(sgg_proj[i, ])
      total_area <- st_area(sgg_geom)
      
      if (as.numeric(total_area) == 0) return(100)
      
      intersection_geom <- suppressWarnings(st_intersection(sgg_geom, aed_coverage_proj))
      
      covered_area <- if (length(intersection_geom) == 0 || st_is_empty(intersection_geom)) {
        0
      } else {
        st_area(intersection_geom)
      }
      
      uncovered_pct <- 100 * (1 - (as.numeric(covered_area) / as.numeric(total_area)))
      return(max(0, min(100, round(uncovered_pct, 1))))
    })
    
    final_data <- sgg_data_raw %>%
      mutate(uncovered_pct_golden = uncovered_percentages)
    
    return(final_data)
  })
  
  output$golden_time_map <- renderLeaflet({
    create_analysis_map()
  })
  
  observeEvent(golden_time_data_reac(),
               {
                 golden_time_waiter$hide()
                 data <- golden_time_data_reac()
                 req(data)
                 
                 pal <- colorNumeric(
                   "YlOrRd",
                   domain = data$uncovered_pct_golden,
                   na.color = "#808080"
                 )
                 proxy <- leafletProxy("golden_time_map", data = data) %>%
                   clearShapes() %>% clearControls() %>%
                   addPolygons(
                     fillColor = ~ pal(uncovered_pct_golden),
                     weight = 1,
                     color = "white",
                     fillOpacity = 0.8,
                     label = ~ paste0(
                       sigungu_name_display,
                       ": ",
                       uncovered_pct_golden,
                       "% 접근 불가"
                     ),
                     highlightOptions = highlightOptions(
                       weight = 3,
                       color = "#333",
                       bringToFront = TRUE
                     ),
                     layerId = ~ sigungu_name_display
                   ) %>%
                   addLegend(
                     pal = pal,
                     values = ~ uncovered_pct_golden,
                     opacity = 0.7,
                     title = "접근 불가 지역(%)",
                     position = "bottomright",
                     className = "info legend desktop-legend" # <<< 이 줄을 추가하세요
                   )
                 
                 if (isolate(input$run_golden_time_analysis) <= 1) {
                   proxy %>% flyToBounds(
                     lng1 = min(st_bbox(data)[1]),
                     lat1 = min(st_bbox(data)[2]),
                     lng2 = max(st_bbox(data)[3]),
                     lat2 = max(st_bbox(data)[4])
                   )
                 }
               },
               ignoreNULL = FALSE,
               ignoreInit = TRUE)
  
  output$golden_time_summary_ui <- renderUI({
    data <- golden_time_data_reac()
    req(data)
    
    sim_count <- nrow(simulated_aeds())
    sim_text <- if (sim_count > 0)
      paste0(" (가상 AED ", sim_count, "개 추가)")
    else
      ""
    
    top_region <- data %>% st_drop_geometry() %>%
      filter(!is.na(uncovered_pct_golden)) %>%
      filter(uncovered_pct_golden == max(uncovered_pct_golden, na.rm = TRUE)) %>%
      slice(1)
    
    tagList(
      value_box(
        title = paste0("평균 접근불가율", sim_text),
        value = paste0(round(
          mean(data$uncovered_pct_golden, na.rm = TRUE), 1
        ), "%"),
        showcase = bsicons::bs_icon("person-walking"),
        class = "bg-light border"
      ),
      if (nrow(top_region) > 0) {
        p(
          strong("가장 취약한 지역: "),
          top_region$sigungu_name_display,
          " (",
          top_region$uncovered_pct_golden,
          "%)",
          class = "mt-3"
        )
      } else {
        p(strong("모든 지역이 양호합니다."), class = "mt-3")
      }
    )
  })
  
  output$golden_time_table <- DT::renderDT({
    data <- golden_time_data_reac()
    req(data)
    
    data %>% st_drop_geometry() %>%
      filter(!is.na(uncovered_pct_golden)) %>%
      select(sigungu_name_display, uncovered_pct_golden) %>%
      arrange(desc(uncovered_pct_golden)) %>% head(10) %>%
      rename("지역명" = sigungu_name_display, "접근불가율(%)" = uncovered_pct_golden) %>%
      datatable(rownames = FALSE,
                options = list(dom = 't', ordering = FALSE))
  })
  
  
  # --- 10. 예측 기반 관리 탭 로직 ---
  prediction_data_reac <- reactive({
    env <- data_env()
    req(env)
    req(input$prediction_period, input$pred_sido_filter)
    days_to_predict <- as.numeric(gsub("일", "", input$prediction_period))
    target_date <- Sys.Date() + days(days_to_predict)
    data <- env$aed_data
    if (input$pred_sido_filter != "전국") {
      data <- data %>% filter(sido_name == input$pred_sido_filter)
      if (!is.null(input$pred_sigungu_filter) &&
          input$pred_sigungu_filter != "전체") {
        data <- data %>% filter(sigungu_name_display == input$pred_sigungu_filter)
      }
    }
    data %>% filter((!is.na(배터리.유효기간) &
                       배터리.유효기간 <= target_date) |
                      (!is.na(패치.유효기간) & 패치.유효기간 <= target_date))
  })
  
  output$battery_replacement_box_pred <- renderUI({
    data <- prediction_data_reac()
    target_date <- Sys.Date() + days(as.numeric(gsub("일", "", input$prediction_period)))
    count <- data %>% filter(!is.na(배터리.유효기간) &
                               배터리.유효기간 <= target_date) %>% nrow()
    value_box(
      title = paste0("교체 필요 배터리 (", input$prediction_period, " 내)"),
      value = prettyNum(count, big.mark = ","),
      showcase = icon("car-battery"),
      theme = if (count > 0)
        "danger"
      else
        "success"
    )
  })
  
  output$patch_replacement_box_pred <- renderUI({
    data <- prediction_data_reac()
    target_date <- Sys.Date() + days(as.numeric(gsub("일", "", input$prediction_period)))
    count <- data %>% filter(!is.na(패치.유효기간) &
                               패치.유효기간 <= target_date) %>% nrow()
    value_box(
      title = paste0("교체 필요 패치 (", input$prediction_period, " 내)"),
      value = prettyNum(count, big.mark = ","),
      showcase = icon("band-aid"),
      theme = if (count > 0)
        "danger"
      else
        "success"
    )
  })
  
  output$prediction_table <- renderDT({
    data <- prediction_data_reac() %>%
      select(
        "설치기관명",
        "설치위치",
        "시/도" = "sido_name",
        "배터리 유효기간" = "배터리.유효기간",
        "패치 유효기간" = "패치.유효기간"
      )
    
    datatable(
      data,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        order = list(list(3, 'asc')),
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'excel', text = '엑셀 저장'),
          list(extend = 'csv', text = 'CSV 저장')
        )
      )
    )
  }, server = FALSE)
  
  
  # --- 11. 품질 관리 탭 로직 ---
  observeEvent(input$goto_report_form, {
    req(input$goto_report_form)
    env <- data_env()
    req(env)
    
    aed_id_to_find <- as.numeric(input$goto_report_form)
    aed_info <- env$aed_data %>% filter(unique_id == aed_id_to_find)
    
    aed_to_report(aed_info)
    updateTabsetPanel(session, "navbar", selected = "dqm_tab")
    updateNavlistPanel(session, "dqm_sub_tabs", selected = "dqm_report_sub_tab")
  })
  
  output$dqm_form_ui <- renderUI({
    aed_info <- aed_to_report()
    
    if (is.null(aed_info) || nrow(aed_info) == 0) {
      return(div(
        class = "text-center py-5",
        icon("info-circle", class = "text-muted", style = "font-size: 3em;"),
        h4("데이터를 신고할 AED를", br(), "'지도' 또는 '장비 목록'에서 선택해주세요.", class =
             "text-secondary mt-3")
      ))
    }
    
    tagList(
      h3(icon("triangle-exclamation"), "AED 품질 관리 리포트"),
      hr(),
      h5(strong("장비 정보")),
      p(strong("관리번호: "), aed_info$관리번호),
      p(strong("설치 기관명: "), aed_info$설치기관명),
      p(strong("설치 장소: "), aed_info$설치위치),
      p(strong("주소: "), aed_info$`설치장소.주소`),
      hr(),
      h5(strong("신고 내용 입력")),
      radioButtons(
        "report_type",
        "신고 유형:",
        choices = c("장비 오류 (작동 불능, 소모품 만료 등)", "정보 오류 (위치, 주소 불일치 등)", "장비 사용", "기타"),
        inline = FALSE
      ),
      textAreaInput(
        "report_details",
        "상세 내용:",
        placeholder = "오류나 사용 상황에 대해 구체적으로 작성해주세요.",
        rows = 4
      ),
      actionButton(
        "submit_report",
        "신고 제출",
        class = "btn-danger w-100",
        icon = icon("paper-plane")
      )
    )
  })
  
  observeEvent(input$submit_report, {
    aed_info <- aed_to_report()
    req(aed_info)
    
    report_log <- tibble(
      timestamp = Sys.time(),
      report_type = input$report_type,
      report_details = input$report_details,
      unique_id = aed_info$unique_id,
      관리번호 = aed_info$관리번호,
      설치기관명 = aed_info$설치기관명,
      주소 = aed_info$`설치장소.주소`
    )
    
    log_file <- "report_log.csv"
    tryCatch({
      write_csv(report_log, log_file, append = file.exists(log_file))
      showNotification("신고가 성공적으로 접수 및 저장되었습니다.",
                       type = "message",
                       duration = 5)
    }, error = function(e) {
      showNotification(paste("신고 저장에 실패했습니다:", e$message),
                       type = "error",
                       duration = 8)
    })
    
    aed_to_report(NULL)
  })
  
  
  
  
  # --- 12. 스마트 분류 어시스턴트 탭 로직 ---
  debounced_query <- reactive(input$assistant_query) %>% debounce(500)
  
  assistant_result <- reactiveVal(NULL)
  
  run_assistant_analysis <- function(query) {
    env <- data_env()
    req(env)
    info <- get_classification_info(query, env$aed_data)
    assistant_result(info)
    if (query != isolate(input$assistant_query)) {
      updateTextInput(session, "assistant_query", value = query)
    }
  }
  
  observeEvent(input$assistant_search_btn, {
    req(nchar(trimws(input$assistant_query)) > 0)
    run_assistant_analysis(input$assistant_query)
  })
  
  observeEvent(debounced_query(), {
    req(nchar(trimws(debounced_query())) > 0)
    run_assistant_analysis(debounced_query())
  })
  
  observeEvent(input$btn_apt, {
    run_assistant_analysis("500세대 아파트")
  })
  observeEvent(input$btn_gov, {
    run_assistant_analysis("공공보건의료기관")
  })
  observeEvent(input$btn_transport, {
    run_assistant_analysis("공항")
  })
  observeEvent(input$btn_biz, {
    run_assistant_analysis("300인 사업장")
  })
  observeEvent(input$btn_stadium, {
    run_assistant_analysis("5천석 경기장")
  })
  
  output$duty_result_ui <- renderUI({
    res <- assistant_result()
    if (is.null(res)) {
      return(
        bslib::card(
          height = "25vh",
          class = "d-flex justify-content-center align-items-center text-muted",
          p("검색어를 입력하거나 버튼을 누르면 결과가 표시됩니다.")
        )
      )
    }
    
    card_theme <- if (res$duty)
      "bg-success"
    else
      "bg-warning"
    card_icon <- if (res$duty)
      "check-circle-fill"
    else
      "exclamation-triangle-fill"
    
    bslib::value_box(
      title = "구비 의무 여부",
      value = if (res$duty)
        "의무 대상입니다"
      else
        "의무 대상이 아닙니다",
      showcase = bsicons::bs_icon(card_icon),
      theme = card_theme,
      p(strong("판단 근거: "), res$duty_text)
    )
  })
  
  output$detail_result_ui <- renderUI({
    res <- assistant_result()
    if (is.null(res))
      return(NULL)
    bslib::card(height = "30vh",
                bslib::card_header("상세 분류 추천"),
                bslib::card_body(if (!res$duty && is.na(res$c2)) {
                  p("법적 구비 의무 대상이 아니므로, '구비의무기관 외'로 신고 후 기관의 특성에 맞게 세부 분류를 입력하는 것을 권장합니다.")
                } else {
                  tagList(p("아래 내용을 참고하여 관할 보건소에 신고하세요."),
                          if (!is.null(res$suggestion_text) &&
                              !is.na(res$suggestion_text)) {
                            div(
                              class = "alert alert-info",
                              bsicons::bs_icon("lightbulb"),
                              strong("빅데이터 추천: "),
                              res$suggestion_text
                            )
                          },
                          tags$table(
                            class = "table",
                            tags$tbody(
                              tags$tr(tags$th("분류1 (의무)"), tags$td(res$c1)),
                              tags$tr(tags$th("분류2 (대분류)"), tags$td(res$c2)),
                              tags$tr(tags$th("분류3 (중분류)"), tags$td(res$c3)),
                              tags$tr(tags$th("분류4 (소분류)"), tags$td(
                                ifelse(is.na(res$c4), "(해당 시 직접 입력)", res$c4)
                              ))
                            )
                          ))
                }))
  })
  
  
  # --- 13. 내 주변 찾기 로직 ---
  run_nearby_search <- function(lng, lat) {
    env <- data_env()
    req(env)
    
    withProgress(message = '주변 정보를 계산 중입니다...', value = 0.1, {
      clicked_point <- st_sfc(st_point(c(lng, lat)), crs = 4326)
      incProgress(0.2, detail = "가까운 AED 분석 중...")
      usable_aed <- env$aed_data %>% filter(has_location == TRUE, is_ready_for_use == TRUE) %>%
        st_as_sf(
          coords = c("경도", "위도"),
          crs = 4326,
          remove = FALSE
        )
      
      nearest_aeds <- if (nrow(usable_aed) > 0) {
        distances_aed <- st_distance(clicked_point, usable_aed)
        usable_aed$distance_m <- as.numeric(distances_aed[1, ])
        usable_aed %>% st_drop_geometry() %>% arrange(distance_m) %>% head(2) %>%
          mutate(walk_time_min = round(distance_m / 70, 1))
      } else {
        data.frame()
      }
      
      incProgress(0.3, detail = "가까운 응급실 분석 중...")
      hospitals_with_loc <- env$hospital_data %>% filter(!is.na(lon) &
                                                           !is.na(lat)) %>%
        st_as_sf(
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        )
      
      nearest_hospitals <- if (nrow(hospitals_with_loc) > 0) {
        distances_hosp <- st_distance(clicked_point, hospitals_with_loc)
        hospitals_with_loc$distance_m <- as.numeric(distances_hosp[1, ])
        hospitals_with_loc %>% st_drop_geometry() %>% arrange(distance_m) %>% head(2) %>%
          mutate(drive_time_min = round((distance_m / 1000) / 30 * 60, 1))
      } else {
        data.frame()
      }
      
      incProgress(0.4, detail = "지도에 결과 표시 중...")
      proxy <- leafletProxy("map") %>% clearGroup("nearby_results_group")
      proxy %>% addAwesomeMarkers(
        lng = lng,
        lat = lat,
        group = "nearby_results_group",
        icon = awesomeIcons(
          icon = 'street-view',
          library = 'fa',
          markerColor = 'purple'
        ),
        popup = "선택한 위치"
      )
      
      if (nrow(nearest_aeds) > 0) {
        for (i in 1:nrow(nearest_aeds)) {
          aed <- nearest_aeds[i, ]
          proxy %>% addAwesomeMarkers(
            lng = aed$경도,
            lat = aed$위도,
            group = "nearby_results_group",
            icon = awesomeIcons(
              icon = 'heartbeat',
              library = 'fa',
              markerColor = 'green'
            ),
            popup = paste0(
              "<b>[AED ",
              i,
              "순위] ",
              aed$설치기관명,
              "</b><br>거리: 약 ",
              round(aed$distance_m),
              "m<br>예상 도보 시간: 약 ",
              aed$walk_time_min,
              "분"
            )
          ) %>% addPolylines(
            lng = c(lng, aed$경도),
            lat = c(lat, aed$위도),
            group = "nearby_results_group",
            color = "#28a745",
            weight = 2,
            opacity = 0.9,
            dashArray = "5, 10"
          )
        }
      }
      
      if (nrow(nearest_hospitals) > 0) {
        for (i in 1:nrow(nearest_hospitals)) {
          hosp <- nearest_hospitals[i, ]
          proxy %>% addAwesomeMarkers(
            lng = hosp$lon,
            lat = hosp$lat,
            group = "nearby_results_group",
            icon = awesomeIcons(
              icon = 'hospital',
              library = 'fa',
              markerColor = 'red'
            ),
            popup = paste0(
              "<b>[응급실 ",
              i,
              "순위] ",
              hosp$기관명,
              "</b><br>거리: 약 ",
              round(hosp$distance_m / 1000, 1),
              "km<br>예상 차량 시간: 약 ",
              hosp$drive_time_min,
              "분"
            )
          ) %>% addPolylines(
            lng = c(lng, hosp$lon),
            lat = c(lat, hosp$lat),
            group = "nearby_results_group",
            color = "#dc3545",
            weight = 2,
            opacity = 0.9
          )
        }
      }
      
      incProgress(0.1, detail = "결과 요약창 생성 중...")
      
      create_info_card <- function(type,
                                   rank,
                                   name,
                                   location,
                                   time_val,
                                   time_unit,
                                   golden_hour_check_html,
                                   dest_lat,
                                   dest_lng) {
        directions_url <- sprintf(
          "https://map.kakao.com/link/to/%s,%f,%f",
          URLencode(name),
          dest_lat,
          dest_lng
        )
        directions_button <- sprintf(
          "<a href='%s' target='_blank' style='text-decoration: none; background-color: #0d6efd; color: white; padding: 4px 8px; border-radius: 4px; font-size: 0.9em;'>카카오맵 길찾기</a>",
          directions_url
        )
        paste0(
          "<div style='border: 1px solid #e9ecef; border-radius: 6px; padding: 8px; margin-bottom: 6px; font-size: 0.85em;'>",
          "<div style='display: flex; justify-content: space-between; align-items: center;'>",
          "<span style='font-weight: bold; color: #0d6efd;'>",
          rank,
          "순위: ",
          name,
          "</span>",
          directions_button,
          "</div>",
          "<div style='font-size: 0.9em; color: #6c757d; margin: 2px 0 6px 0;'>",
          location,
          "</div>",
          "<div style='border-top: 1px solid #e9ecef; padding-top: 6px; display: flex; justify-content: space-between; align-items: center;'>",
          "<span><i class='fa fa-clock'></i> 약 ",
          time_val,
          time_unit,
          "</span>",
          golden_hour_check_html,
          "</div></div>"
        )
      }
      
      modal_content_html <- ""
      if (nrow(nearest_aeds) > 0) {
        aed_html_list <- lapply(1:nrow(nearest_aeds), function(i) {
          aed <- nearest_aeds[i, ]
          golden_check <- if (aed$walk_time_min <= 4)
            "<span style='color: #198754; font-weight: bold; font-size: 0.9em;'><i class='fa fa-check-circle'></i> 골든아워 내</span>"
          else
            "<span style='color: #ffc107; font-weight: bold; font-size: 0.9em;'><i class='fa fa-exclamation-triangle'></i> 4분 초과</span>"
          create_info_card(
            "AED",
            i,
            aed$설치기관명,
            aed$설치위치,
            aed$walk_time_min,
            "분",
            golden_check,
            aed$위도,
            aed$경도
          )
        })
        modal_content_html <- paste0(
          "<h5><i class='fa fa-heartbeat' style='color:#dc3545;'></i> 주변 AED 정보</h5>",
          paste(aed_html_list, collapse = "")
        )
      }
      
      if (nrow(nearest_hospitals) > 0) {
        hosp_html_list <- lapply(1:nrow(nearest_hospitals), function(i) {
          hosp <- nearest_hospitals[i, ]
          golden_check <- if (hosp$drive_time_min <= 10)
            "<span style='color: #198754; font-weight: bold; font-size: 0.9em;'><i class='fa fa-check-circle'></i> 골든아워 내</span>"
          else
            "<span style='color: #ffc107; font-weight: bold; font-size: 0.9em;'><i class='fa fa-exclamation-triangle'></i> 10분 초과</span>"
          create_info_card(
            "ER",
            i,
            hosp$기관명,
            hosp$의료기관분류,
            hosp$drive_time_min,
            "분",
            golden_check,
            hosp$lat,
            hosp$lon
          )
        })
        modal_content_html <- paste0(
          modal_content_html,
          "<h5 style='margin-top: 12px;'><i class='fa fa-hospital' style='color:#dc3545;'></i> 주변 응급의료기관 정보</h5>",
          paste(hosp_html_list, collapse = "")
        )
      }
      
      if (nchar(modal_content_html) == 0) {
        modal_content_html <- "<div class='text-center' style='padding: 20px; color: #6c757d;'><i class='fa fa-info-circle'></i><br>주변에서 사용 가능한 AED나<br>응급의료기관을 찾지 못했습니다.</div>"
      }
      
      all_points <- bind_rows(
        data.frame(lng = lng, lat = lat),
        nearest_aeds %>% select(lng = 경도, lat = 위도),
        nearest_hospitals %>% select(lng = lon, lat = lat)
      ) %>% filter(!is.na(lng) & !is.na(lat))
      
      if (nrow(all_points) > 0) {
        proxy %>% flyToBounds(
          lng1 = min(all_points$lng),
          lat1 = min(all_points$lat),
          lng2 = max(all_points$lng),
          lat2 = max(all_points$lat),
          options = list(padding = c(50, 50))
        )
      }
      
      showModal(
        modalDialog(
          title = HTML("<div style='font-weight:bold;'>📍 내 주변 응급 자원 분석</div>"),
          HTML(modal_content_html),
          easyClose = TRUE,
          footer = modalButton("확인"),
          size = "m"
        )
      )
    })
  }
  
  find_nearby_mode <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_buffers, {
    if (isTRUE(input$toggle_buffers) &&
        isTRUE(isolate(input$sido_filter) == "전체")) {
      showNotification(
        "전국 단위의 서비스 반경 분석은 시간이 오래 걸릴 수 있습니다. 시/도 필터를 먼저 선택하는 것을 권장합니다.",
        type = "warning",
        duration = 8
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$find_nearby_btn, {
    find_nearby_mode(TRUE)
    showNotification("GPS로 현재 위치를 가져오거나, 지도에서 원하는 위치를 클릭하세요.",
                     type = "message",
                     duration = 10)
    session$sendCustomMessage(type = "get_geolocation", message = list())
  })
  
  observeEvent(input$client_location, {
    if (find_nearby_mode() == TRUE) {
      coords <- input$client_location
      run_nearby_search(lng = coords$lng, lat = coords$lat)
      find_nearby_mode(FALSE)
    }
  })
  
  observeEvent(input$geolocation_error, {
    showNotification(paste("GPS 위치를 가져올 수 없습니다:", input$geolocation_error$message),
                     type = "error")
    # find_nearby_mode(FALSE) <- 이 라인을 삭제 또는 주석 처리
  })
  
  observeEvent(input$map_click, {
    if (find_nearby_mode() == TRUE) {
      click <- input$map_click
      run_nearby_search(lng = click$lng, lat = click$lat)
      find_nearby_mode(FALSE)
    }
  })
  
  
  # --- 14. 자동 리포트 탭 로직 ---
  observeEvent(input$activate_report_btn, {
    is_valid_email <- grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",
                            input$report_email)
    if (!is_valid_email) {
      showModal(modalDialog(
        title = "오류",
        "유효한 이메일 주소를 입력해주세요.",
        footer = modalButton("닫기")
      ))
      return()
    }
    
    if (!is.null(report_scheduler())) {
      report_scheduler()$destroy()
    }
    
    interval_ms <- switch(
      input$report_schedule,
      "daily" = 24 * 60 * 60 * 1000,
      "weekly" = 7 * 24 * 60 * 60 * 1000,
      "monthly" = 30 * 24 * 60 * 60 * 1000,
      "test" = 5 * 60 * 1000
    )
    
    showNotification(paste(input$report_schedule, "주기로 리포트 자동 생성을 시작합니다."),
                     type = "message")
    
    new_scheduler <- observe({
      invalidateLater(interval_ms)
      
      isolate({
        report_sido <- input$report_sido_filter
        report_email <- input$report_email
        report_format <- input$report_format
      })
      
      showNotification(paste(report_sido, "지역 리포트 생성 중..."), type = "message")
      
      temp_report_path <- tempfile(fileext = paste0(".", report_format))
      
      tryCatch({
        rmarkdown::render(
          "/app/report_template.Rmd",
          output_file = temp_report_path,
          params = list(
            target_sido = report_sido,
            data_month = isolate(input$data_month_select)
          ),
          envir = new.env(parent = globalenv())
        )
        
        showNotification(
          paste0(
            report_sido,
            " 리포트가 성공적으로 생성되어 '",
            report_email,
            "' 주소로 발송되었습니다. (현재는 시뮬레이션)"
          ),
          type = "message",
          duration = 10
        )
        
      }, error = function(e) {
        showNotification(paste("리포트 생성 중 오류 발생:", e$message),
                         type = "error",
                         duration = 15)
      })
    })
    
    report_scheduler(new_scheduler)
  })
  
  observeEvent(input$deactivate_report_btn, {
    if (!is.null(report_scheduler())) {
      report_scheduler()$destroy()
      report_scheduler(NULL)
      showNotification("리포트 자동 생성이 중지되었습니다.", type = "warning")
    }
  })
  
  session$onSessionEnded(function() {
    scheduler <- isolate(report_scheduler())
    if (!is.null(scheduler)) {
      scheduler$destroy()
    }
  })
  
  
  

  
  
  
}
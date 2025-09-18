# new5
# --- 1. 필수 라이브러리 로드 ---
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
library(echarts4r)
library(DT)
library(waiter)
library(shinymanager)
library(fs)




# ui.R 파일 상단에 이 코드를 추가하세요.

# 지도 위에 표시될 안내창(로딩 스크린) HTML
map_loading_screen <- tagList(
  tags$div(
    style = "color: #333; text-align: center; padding: 20px;",
    tags$h3("Smart AED 대시보드 활용법", style = "font-weight: 600;"),
    tags$br(),
    tags$div(
      style = "text-align: left; font-size: 1.1em; max-width: 500px; margin: auto;",
      p(tags$b("지도 제어:"), " 왼쪽 사이드바 메뉴로 원하는 지역을 필터링하거나 표시 유형(클러스터, 히트맵 등)을 변경할 수 있습니다."),
      p(tags$b("상세 정보:"), " 지도 위의 마커나 지역(폴리곤)을 클릭하면 상세 정보를 보거나 해당 지역으로 확대할 수 있습니다."),
      p(tags$b("모바일:"), " 스마트폰에서는 두 손가락으로 지도를 확대/축소하고, 좌우로 스와이프하여 메뉴를 열고 닫을 수 있습니다.")
    ),
    br(),
    actionButton("close_map_cover_btn", "확인하고 지도 보기", icon = icon("check"), class = "btn-primary btn-lg")
  )
)

# --- 2. UI 에셋 정의 ---

# JavaScript 코드 (지도 렌더링 개선)
js_code <- "
shinyjs.init = function() {
  // 모바일 감지
  window.isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);


  
  

  // 홈 로고 클릭 시 지도 탭으로 이동하고 앱 상태 초기화
  $('#home-logo').on('click', function() {
    Shiny.setInputValue('reset_to_home', true, {priority: 'event'});
    var tab = new bootstrap.Tab($('.nav-link[data-value=\"map_tab\"]'));
    tab.show();
  });

  // DT 테이블 내의 버튼 클릭 이벤트 위임(delegation)
  $('body').on('click', '.location-btn', function() {
    Shiny.setInputValue('goto_location', $(this).data('id'), {priority: 'event'});
  });

  $('body').on('click', '.report-aed-btn', function() {
    if ($('.leaflet-popup').length) {
      $('.leaflet-popup-close-button').click();
    }
    Shiny.setInputValue('goto_report_form', $(this).data('id'), {priority: 'event'});
  });
  


  // 터치 스와이프로 사이드바 제어 (모바일)
  if (window.isMobile) {
    let startX = 0;
    let currentX = 0;
    let isDragging = false;

    document.addEventListener('touchstart', function(e) {
      startX = e.touches[0].clientX;
      isDragging = true;
    });

    document.addEventListener('touchmove', function(e) {
      if (!isDragging) return;
      currentX = e.touches[0].clientX;
    });

    document.addEventListener('touchend', function(e) {
      if (!isDragging) return;

      const diffX = currentX - startX;
      const isOpen = !$('#sidebar-toggle-checkbox').is(':checked');

      // 오른쪽 스와이프로 사이드바 열기 (화면 왼쪽 끝에서 시작)
      if (startX < 20 && diffX > 50 && !isOpen) {
        $('#sidebar-toggle-checkbox').prop('checked', false);
      }
      // 왼쪽 스와이프로 사이드바 닫기
      else if (diffX < -50 && isOpen) {
        $('#sidebar-toggle-checkbox').prop('checked', true);
      }

      isDragging = false;
    });
  }

  // 탭 전환 시 지도 크기 재계산 (핵심 수정사항)
  $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"]', function (e) {
    if ($(e.target).attr('data-value') === 'map_tab') {
      setTimeout(function() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('map_resize_trigger', Math.random(), {priority: 'event'});
        }
      }, 100);
    }
  });

  // 화면 회전/크기 변경 대응 (개선된 버전)
  $(window).on('resize orientationchange', function() {
    setTimeout(function() {
      // 지도 크기 재조정
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('map_resize_trigger', Math.random(), {priority: 'event'});
      }

      // 모바일에서 가로 모드 시 사이드바 자동 닫기
      if (window.isMobile && window.innerWidth < 768) {
        $('#sidebar-toggle-checkbox').prop('checked', true);
      }
    }, 100);
  });
};

// 시작 화면 제거 개선
$(document).on('click', '#start_app_btn', function() {
  $('#start_screen').fadeOut(500, function() {
    $(this).remove(); // 완전히 DOM에서 제거
    $('#main_dashboard').show();
    // 지도 초기화 트리거
    setTimeout(function() {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('app_started', true, {priority: 'event'});
        Shiny.setInputValue('map_resize_trigger', Math.random(), {priority: 'event'});
      }
    }, 600);
  });
});

// 모달 대화창 자동 스크롤 함수
shinyjs.scrollChat = function() {
  var container = document.getElementById('modal-chat-container');
  if (container) {
    var lastMessage = container.querySelector('.chat-message:last-child');
    if (lastMessage) {
      lastMessage.scrollIntoView({ behavior: 'auto', block: 'end' });
    }
  }
}

// 모달 대화창에서 Enter 키를 누르면 전송 버튼 클릭
$(document).on('keypress', '#modal_chat_query_input', function(e) {
  if (e.which == 13 && !e.shiftKey) {
    e.preventDefault();
    $('#modal_chat_submit_btn').click();
  }
});

// 모바일 최적화: 더블 탭 방지
$(document).ready(function() {
  if (window.isMobile) {
    $('button, .btn').css('touch-action', 'manipulation');
  }
});
"

# CSS 코드 (지도 렌더링 개선)
css_code <- "
:root {
  /* --- 디자인 시스템: 색상, 폰트, 레이아웃 변수 정의 --- */
  --sidebar-width: 300px;
  --sidebar-width-mobile: 280px;
  --header-height: 65px;
  --header-height-mobile: 55px;
  --footer-height: 50px;
  --footer-height-mobile: 40px;

  /* 색상 팔레트 */
  --primary-color: #4A90E2;
  --primary-hover: #5DAAFE;
  --gradient-start: #1A2E47;
  --gradient-end: #2C3E50;

  --dark-bg-1: #1A2E47;
  --dark-bg-2: #2C3E50;
  --dark-border: #4A637F;

  --light-bg-1: #F4F7FA;
  --light-bg-2: #FFFFFF;
  --light-border: #DEE2E6;

  --text-dark-primary: #E0E6EB;
  --text-dark-secondary: #A7B4C2;
  --text-light-primary: #34495E;
  --text-light-secondary: #7F8C8D;

  /* 컴포넌트 스타일 */
  --card-shadow: 0 6px 20px rgba(0,0,0,0.08);
  --card-shadow-mobile: 0 2px 8px rgba(0,0,0,0.1);
  --card-radius: 10px;
  --card-radius-mobile: 8px;

  /* 폰트 */
  --font-primary: 'Poppins', 'Noto Sans KR', sans-serif;
  --font-secondary: 'Noto Sans KR', sans-serif;

  /* 폰트 크기 변수 */
  --font-size-base: 0.8125rem; /* 약 13px */
  --font-size-base-mobile: 0.875rem; /* 모바일에서 약간 더 큼 */
  --font-size-sm: 0.75rem;
  --font-size-sm-mobile: 0.8125rem;
  --font-size-lg: 0.9375rem;
  --font-size-lg-mobile: 1rem;
  --line-height-base: 1.6;
}

/* --- [수정] 사이드바 폰트 크기 통일 --- */
.sidebar .form-control, .sidebar .selectize-input {
  font-size: 0.9rem !important;
}
.selectize-dropdown-content .option {
  font-size: 0.9rem !important;
}


/* --- 기본 레이아웃 및 전체 스타일 --- */
html {
  font-size: 16px;
  -webkit-text-size-adjust: 100%; /* iOS 폰트 크기 조정 방지 */
}

html, body {
  height: 100%;
  width: 100%;
  margin: 0;
  padding: 0;
  overflow: hidden;
  font-family: var(--font-secondary);
  background-color: var(--light-bg-1);
  color: var(--text-light-primary);
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  font-size: var(--font-size-base);
  line-height: var(--line-height-base);
  /* 모바일 최적화: 탭 하이라이트 제거 */
  -webkit-tap-highlight-color: transparent;
  -webkit-touch-callout: none;
  -webkit-user-select: none;
  -khtml-user-select: none;
  -moz-user-select: none;
  -ms-user-select: none;
  user-select: none;
}

/* 텍스트 선택 허용 영역 */
.selectable, input, textarea, .form-control {
  -webkit-user-select: text;
  -moz-user-select: text;
  -ms-user-select: text;
  user-select: text;
}

/* --- 타이포그래피 시스템 --- */
h1, h2, h3, h4, h5, h6 {
  font-family: var(--font-primary);
  font-weight: 700;
  line-height: 1.3;
  color: var(--text-light-primary);
  margin-top: 0;
  margin-bottom: 0.75rem;
}
h3 { font-size: 1.3rem; }
h4 { font-size: 1.1rem; }
h5 { font-size: 1.0rem; }
h6 { font-size: 0.9rem; }

.sidebar h5 {
    color: #FFFFFF;
}
.layer-item-desc {
    color: var(--text-dark-primary);
}

.page-container {
  display: flex;
  width: 100%;
  height: 100%;
}

.main-content {
  flex-grow: 1;
  height: 100vh;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  background-color: var(--light-bg-1);
}

.tab-content-container {
  padding: 25px;
  flex-grow: 1;
  overflow-y: auto;
}

/* --- 사이드바 --- */
.sidebar {
  width: var(--sidebar-width);
  height: 100vh;
  background-color: var(--dark-bg-1);
  color: var(--text-dark-primary);
  display: flex;
  flex-direction: column;
  box-shadow: 5px 0 20px rgba(0,0,0,0.2);
  flex-shrink: 0;
  transition: width 0.3s ease, padding 0.3s ease, min-width 0.3s ease;
  overflow-x: hidden;
  z-index: 1010;
  position: relative;
}

#sidebar-toggle-checkbox:checked ~ .page-container .sidebar {
  width: 0;
  padding: 0;
  min-width: 0;
}

.sidebar-content-wrapper {
  padding: 20px 25px;
  overflow-y: auto;
  flex-grow: 1;
  scrollbar-width: none;
  -ms-overflow-style: none;
  /* 모바일에서 스크롤 개선 */
  -webkit-overflow-scrolling: touch;
}
.sidebar-content-wrapper::-webkit-scrollbar { display: none; }

.sidebar-header {
  text-align: center;
  margin: 20px 0 25px 0;
  cursor: pointer;
  padding-bottom: 20px;
  border-bottom: 1px solid var(--dark-border);
  transition: opacity 0.3s ease;
}
.sidebar-header:hover { opacity: 0.9; }
.sidebar-header h2 {
  font-family: var(--font-primary);
  color: #FFFFFF;
  font-size: 2.5rem;
  font-weight: 800;
  margin: 0;
  letter-spacing: 1.5px;
  text-shadow: 1px 1px 3px rgba(0,0,0,0.3);
}
.sidebar-header p {
  color: var(--text-dark-secondary);
  font-size: 0.85rem;
  margin-top: 8px;
  font-weight: 500;
  text-transform: uppercase;
  letter-spacing: 0.8px;
}



/* --- [수정 및 추가] 모바일 반응형 범례 및 폰트 크기 조정 CSS --- */

/* '범례 보기' 버튼 스타일 (기존과 동일) */
#show-legend-modal-btn {
  display: none; 
  background-color: white;
  padding: 8px 12px;
  border: 2px solid rgba(0,0,0,0.2);
  border-radius: 4px;
  font-weight: bold;
  cursor: pointer;
}

/* [추가] 범례 내부 텍스트 스타일 조정 */
.leaflet-control.legend {
  font-size: 12px !important; /* 범례 기본 글자 크기 줄이기 */
}
.leaflet-control.legend .legend-title {
  font-size: 14px !important; /* 범례 제목 글자 크기 */
  margin-bottom: 5px;
}
.leaflet-control.legend i {
  width: 15px !important; /* 색상 사각형 크기 살짝 줄이기 */
  height: 15px !important;
}

/* 데스크톱/모바일 화면 크기에 따라 범례와 버튼을 선택적으로 표시 (기존과 동일) */
@media (max-width: 768px) {
  .desktop-legend {
    display: none !important; 
  }
  #show-legend-modal-btn {
    display: block !important;
  }
}

/* --- [추가] 모바일 전용 플로팅 버튼 스타일 --- */
#mobile-sidebar-toggle {
  display: flex; /* 아이콘을 중앙에 배치하기 위함 */
  justify-content: center;
  align-items: center;
  position: fixed; /* 화면에 고정 */
  bottom: 25px;    /* 하단에서 25px */
  right: 25px;     /* 우측에서 25px */
  width: 55px;
  height: 55px;
  background-color: var(--primary-color);
  color: white;
  border-radius: 50%; /* 원형 모양 */
  box-shadow: 0 4px 12px rgba(0,0,0,0.25);
  z-index: 1030; /* 다른 요소들보다 위에 표시 */
  cursor: pointer;
  transition: background-color 0.2s ease, transform 0.2s ease;
  font-size: 1.4rem; /* 아이콘 크기 */
}

#mobile-sidebar-toggle:hover {
  background-color: var(--primary-hover);
  transform: scale(1.05);
}


.main-header::before { display: none !important; }



/* -------- ✅ 이 코드를 CSS에 새로 추가하세요 ✅ -------- */
/* 데스크탑 화면(769px 이상)일 때 버튼 스타일 */
@media (min-width: 769px) {
  .main-header .sidebar-toggle-btn {
    display: flex; /* 데스크탑에서는 헤더에 버튼을 항상 표시 */
  }
  #mobile-sidebar-toggle {
    display: none; /* 데스크탑에서는 플로팅 버튼 숨기기 */
  }
}

/* www/styles.css 파일에 추가 */

/* 화면 너비가 768px 미만일 때 (모바일 환경) */
@media (max-width: 767.98px) {
  /* 데스크톱용 범례(.desktop-legend)를 완전히 숨김 처리 */
  .leaflet-control-container .info.legend.desktop-legend {
    display: none !important;
  }
}



/* --- 헤더 & 푸터 --- */
.main-header {
  height: var(--header-height);
  flex-shrink: 0;
  background: var(--light-bg-2);
  padding: 0 25px;
  display: flex;
  align-items: center;
  gap: 15px;
  box-shadow: var(--card-shadow);
  z-index: 1000;
}
/* --- 헤더 & 푸터 --- 섹션의 .page-footer 부분을 수정합니다. */
.page-footer {
  flex-shrink: 0;
  background-color: var(--light-bg-2);
  border-top: 1px solid var(--light-border);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: var(--font-size-sm);
  color: var(--text-light-secondary);
  box-shadow: 0 -3px 10px rgba(0,0,0,0.05);
  padding: 10px 15px; /* 높이를 제거하고 패딩으로 대체 */
  text-align: center; /* 텍스트 중앙 정렬 보장 */
}

/* --- 사이드바 토글 버튼 --- */
.sidebar-toggle-btn {
  cursor: pointer;
  color: var(--text-light-secondary);
  font-size: 1.2rem;
  padding: 10px 12px;
  border-radius: 8px;
  transition: all 0.2s ease;
  display: flex;
  align-items: center;
  white-space: nowrap;
  background-color: var(--light-bg-1);
  border: 1px solid var(--light-border);
  /* 모바일 터치 최적화 */
  min-width: 44px;
  min-height: 44px;
  touch-action: manipulation;
}
.sidebar-toggle-btn:hover {
  color: var(--primary-color);
  background-color: #F0F4F8;
  border-color: var(--primary-color);
}
.sidebar-toggle-btn .toggle-text {
  font-size: 0.9rem;
  font-weight: 600;
  margin-left: 10px;
  max-width: 0;
  overflow: hidden;
  transition: max-width 0.3s ease-in-out;
}
.sidebar-toggle-btn:hover .toggle-text { max-width: 150px; }

/* --- bslib 탭 & 카드 스타일 오버라이드 --- */
.main-tabs-container .card-header {
  background: linear-gradient(90deg, var(--gradient-end), var(--gradient-start)) !important;
  border-bottom: none !important;
  padding: 0 1.5rem !important;
  border-radius: var(--card-radius) var(--card-radius) 0 0 !important;
}
.main-tabs-container .nav-tabs { border-bottom: none !important; }
.main-tabs-container .nav-tabs .nav-link {
  color: var(--text-dark-primary) !important;
  opacity: 0.8;
  border: none !important;
  border-bottom: 3px solid transparent !important;
  background-color: transparent !important;
  padding: 1rem 1.2rem !important;
  font-weight: 500;
  font-size: 0.95rem;
  transition: all 0.3s ease-in-out;
  /* 모바일 터치 최적화 */
  min-height: 44px;
  touch-action: manipulation;
}
.main-tabs-container .nav-tabs .nav-link:hover {
  color: white !important;
  opacity: 1;
  border-bottom-color: rgba(255, 255, 255, 0.4) !important;
}
.main-tabs-container .nav-tabs .nav-link.active {
  color: white !important;
  opacity: 1;
  font-weight: 700;
  border-bottom-color: var(--primary-color) !important;
  background-color: rgba(255, 255, 255, 0.05) !important;
}

/* --- [수정] 사이드바 닫기 버튼 (모바일용) --- */
.sidebar-close-btn {
  display: none; /* 기본적으로 숨김 */
  color: var(--text-dark-secondary);
  font-size: 1.5rem;
  cursor: pointer;
  transition: color 0.2s ease;
  padding: 5px; /* 터치 영역 확보 */
}

.sidebar-close-btn:hover {
  color: #FFFFFF;
}

@media (max-width: 768px) {
  .sidebar-close-btn {
    display: block; /* 모바일에서만 표시 */
  }
}

/* =================================================================== */
/* ### 지도 높이 문제 해결을 위한 핵심 CSS 코드 (수정 버전) ### */
/* =================================================================== */

/* 1. bslib 탭 레이아웃(.navset-card-tab)이 남은 공간을 모두 차지하도록 설정합니다. */
.main-content > .navset-card-tab {
    flex-grow: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
}

/* 2. 탭을 감싸는 card와 card-body도 flex 컨테이너로 만들어 높이를 채웁니다. */
.navset-card-tab > .card,
.navset-card-tab > .card > .card-body.tab-content {
    flex-grow: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
}

/* 3. 개별 탭(.tab-pane)이 card-body 영역을 꽉 채우도록 설정합니다. (개선된 버전) */
.tab-content > .tab-pane {
    height: 100%;
    /* 탭 전환 시 렌더링 최적화 */
    opacity: 0;
    transition: opacity 0.1s ease-in-out;
}

.tab-content > .tab-pane.active {
    opacity: 1;
}

/* 4. 지도가 포함된 탭(#nav-navbar-map_tab)의 불필요한 안쪽 여백을 제거합니다. */
.tab-content > #nav-navbar-map_tab {
    padding: 0 !important;
}

/* 5. 최종적으로 leaflet 지도 객체(#map)가 컨테이너를 100% 채우도록 보장합니다. */
#map {
    height: 100% !important;
    width: 100% !important;
    min-height: 400px; /* 최소 높이 보장 */
    position: relative;
    z-index: 1;
    /* 지도 로딩 개선 */
    background-color: #f8f9fa;
}

/* 지도 로딩 중일 때 표시할 스타일 */
#map:empty::before {
    content: '지도를 불러오는 중...';
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    color: #666;
    font-size: 1rem;
}

.navset-card-tab > .card > .card-body.tab-content {
    flex-grow: 1;
    overflow-y: auto;
    padding: 25px;
    /* 모바일에서 스크롤 개선 */
    -webkit-overflow-scrolling: touch;
}

/* 지도 탭만 패딩 제거 */
.tab-content > #nav-navbar-map_tab {
    padding: 0 !important;
    overflow: hidden;
}

/* --- 카드 스타일 --- */
.card {
  background-color: var(--light-bg-2);
  border: 1px solid var(--light-border);
  border-radius: var(--card-radius);
  box-shadow: var(--card-shadow);
  overflow: hidden;
  margin-bottom: 20px;
}
.card .card-header {
  background-color: var(--dark-bg-2);
  color: var(--text-dark-primary);
  font-weight: 600;
  border-bottom: 1px solid var(--dark-border);
  padding: 12px 20px;
  font-size: 0.95rem;
}
.card .card-body { padding: 20px; }

.card .card-header h1,
.card .card-header h2,
.card .card-header h3,
.card .card-header h4,
.card .card-header h5,
.card .card-header h6 {
    color: var(--text-dark-primary);
    margin-bottom: 0;
}

/* --- 폼 컨트롤 & 버튼 --- */
.form-control, .selectize-input, .btn {
  border-radius: 8px;
  font-size: var(--font-size-base);
  /* 모바일 터치 최적화 */
  min-height: 44px;
  touch-action: manipulation;
}

.btn {
  /* 모바일에서 버튼 크기 최적화 */
  min-width: 44px;
  min-height: 44px;
  padding: 0.5rem 1rem;
}

.btn-sidebar-action {
  font-size: 0.95rem;
  padding-top: 0.6rem;
  padding-bottom: 0.6rem;
}
.btn-primary {
  background-color: var(--primary-color);
  border-color: var(--primary-color);
  transition: background-color 0.2s ease, border-color 0.2s ease;
}
.btn-primary:hover {
  background-color: var(--primary-hover);
  border-color: var(--primary-hover);
}
.btn-outline-light {
  color: var(--text-dark-primary);
  border-color: var(--dark-border);
}
.btn-outline-light:hover {
  background-color: var(--dark-bg-2);
  color: white;
}
.radio-group-buttons .btn.active,
.radio-group-buttons .btn:active,
.radio-group-buttons .btn:focus {
  background-color: var(--primary-color);
  border-color: var(--primary-color);
  color: white;
  box-shadow: none;
}
.radio-group-buttons.btn-group-sm > .btn {
    font-size: var(--font-size-sm);
}

/* --- 지도 & 레이어 옵션 --- */
.layer-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
  padding: 4px 0;
}
.layer-item .form-label {
  font-weight: 500;
  white-space: nowrap;
  text-align: left;
}
.layer-item .material-switch {
  flex-shrink: 0;
}
.layer-item .shiny-input-container {
  margin-bottom: 0;
}
.layer-item-desc {
  font-size: 0.85rem;
  color: var(--text-dark-secondary);
  white-space: nowrap;
  text-align: right;
}

/* --- 세부 현황 분석 탭의 값 상자(Value Box) --- */
.bs-value-box {
  background-color: var(--light-bg-2) !important;
  color: var(--text-light-primary) !important;
  border: 1px solid var(--light-border) !important;
  border-top-width: 4px;
  border-top-style: solid;
  transition: all 0.2s ease-in-out;
}
.bs-value-box:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0,0,0,0.1);
}
.bs-value-box .value-box-area > p {
  color: var(--text-light-secondary) !important;
  opacity: 1 !important;
}
.bs-value-box .value-box-area .value {
  font-size: 1.7rem !important;
  font-weight: 700;
  color: var(--text-light-primary) !important;
}
.bs-value-box .showcase-area .bs-icon {
  opacity: 1 !important;
}
.value-box-total { border-top-color: #4A90E2 !important; }
.value-box-total .showcase-area .bs-icon { color: #4A90E2 !important; }
.value-box-ready { border-top-color: #28a745 !important; }
.value-box-ready .showcase-area .bs-icon { color: #28a745 !important; }
.value-box-expired { border-top-color: #dc3545 !important; }
.value-box-expired .showcase-area .bs-icon { color: #dc3545 !important; }
.value-box-inspection { border-top-color: #ffc107 !important; }
.value-box-inspection .showcase-area .bs-icon { color: #ffc107 !important; }

/* --- Q&A 채팅 UI --- */
.chat-wrapper {
  display: flex;
  flex-direction: column;
  height: 100%;
}
#chat-container {
  overflow-y: auto;
  padding: 15px;
  border: 1px solid var(--light-border);
  border-radius: var(--card-radius);
  background-color: var(--light-bg-1);
  flex-grow: 1;
  max-height: 400px;
  /* 모바일에서 스크롤 개선 */
  -webkit-overflow-scrolling: touch;
}
.chat-message {
  margin-bottom: 12px;
  display: flex;
}
.user-message {
  justify-content: flex-end;
}
.user-message .message-bubble {
  background-color: var(--primary-color);
  color: white;
  border-radius: 15px 15px 0 15px;
  padding: 10px 15px;
  display: inline-block;
  max-width: 80%;
  word-wrap: break-word;
}
.bot-message {
  justify-content: flex-start;
}
.bot-message .message-bubble {
  background-color: var(--light-bg-2);
  color: var(--text-light-primary);
  border: 1px solid var(--light-border);
  border-radius: 15px 15px 15px 0;
  padding: 10px 15px;
  display: inline-block;
  max-width: 80%;
  word-wrap: break-word;
}
.chat-input-area {
  display: flex;
  gap: 10px;
  margin-top: 15px;
  flex-shrink: 0;
}
.chat-input-area .form-control { flex-grow: 1; }
.selectize-dropdown { z-index: 9999 !important; }


    
    
    
    
    









































/* =================================================================== */
/* 모바일 최적화 미디어 쿼리 */
/* =================================================================== */

/* 태블릿 (768px 이하) */
@media (max-width: 768px) {
  :root {
    --sidebar-width: var(--sidebar-width-mobile);
    --header-height: var(--header-height-mobile);
    --footer-height: var(--footer-height-mobile);
    --font-size-base: var(--font-size-base-mobile);
    --font-size-sm: var(--font-size-sm-mobile);
    --font-size-lg: var(--font-size-lg-mobile);
    --card-shadow: var(--card-shadow-mobile);
    --card-radius: var(--card-radius-mobile);
  }

  /* 사이드바를 기본적으로 숨김 */
  #sidebar-toggle-checkbox:not(:checked) ~ .page-container .sidebar {
    width: 0;
    padding: 0;
    min-width: 0;
  }

  /* 사이드바가 열릴 때 오버레이 효과 */
  #sidebar-toggle-checkbox:checked ~ .page-container .sidebar {
    position: fixed;
    width: var(--sidebar-width-mobile);
    z-index: 1020;
    box-shadow: 5px 0 25px rgba(0,0,0,0.3);
  }



  /* 메인 헤더 패딩 조정 */
  .main-header {
    padding: 0 15px;
    gap: 10px;
  }

  /* 탭 콘텐츠 패딩 조정 */
  .navset-card-tab > .card > .card-body.tab-content {
    padding: 15px;
  }

  /* 카드 패딩 조정 */
  .card .card-body {
    padding: 15px;
  }

  /* 탭 링크 패딩 조정 */
  .main-tabs-container .nav-tabs .nav-link {
    padding: 0.75rem 0.8rem !important;
    font-size: 0.85rem;
  }

  /* 버튼 크기 조정 */
  .btn-sidebar-action {
    font-size: 0.85rem;
  }

  /* 사이드바 헤더 크기 조정 */
  .sidebar-header h2 {
    font-size: 2rem;
  }

  /* 값 상자 폰트 크기 조정 */
  .bs-value-box .value-box-area .value {
    font-size: 1.4rem !important;
  }
}

/* 모바일 (576px 이하) */
@media (max-width: 576px) {
  /* 더 작은 패딩 */
  .navset-card-tab > .card > .card-body.tab-content {
    padding: 10px;
  }

  .card .card-body {
    padding: 12px;
  }

  /* 더 작은 탭 링크 */
  .main-tabs-container .nav-tabs .nav-link {
    padding: 0.5rem 0.6rem !important;
    font-size: 0.8rem;
  }

  /* 헤더 높이 더 줄이기 */
  :root {
    --header-height-mobile: 50px;
    --footer-height-mobile: 35px;
  }

  .main-header {
    padding: 0 10px;
  }

  /* 사이드바 폭 줄이기 */
  :root {
    --sidebar-width-mobile: 260px;
  }

  /* 사이드바 헤더 더 작게 */
  .sidebar-header h2 {
    font-size: 1.8rem;
    letter-spacing: 1px;
  }

  .sidebar-header p {
    font-size: 0.75rem;
  }

  /* 사이드바 콘텐츠 패딩 조정 */
  .sidebar-content-wrapper {
    padding: 15px 20px;
  }

  /* 토글 버튼 텍스트 숨기기 */
  .sidebar-toggle-btn .toggle-text {
    display: none;
  }

  /* 값 상자 더 작게 */
  .bs-value-box .value-box-area .value {
    font-size: 1.2rem !important;
  }

  /* 차트 높이 조정 */
  .echarts4r {
    min-height: 200px !important;
  }
}

/* 가로 모드 최적화 (모바일) */
@media (max-width: 768px) and (orientation: landscape) {
  /* 사이드바를 더 좁게 */
  :root {
    --sidebar-width-mobile: 240px;
  }

  /* 헤더 높이 줄이기 */
  :root {
    --header-height-mobile: 45px;
  }

  .sidebar-header h2 {
    font-size: 1.6rem;
  }

  .sidebar-content-wrapper {
    padding: 10px 15px;
  }
}

/* 터치 인터페이스 최적화 */
@media (pointer: coarse) {
  /* 모든 클릭 가능한 요소의 최소 크기 보장 */
  .btn, .nav-link, .form-control, .selectize-input {
    min-height: 44px;
    min-width: 44px;
  }

  /* 터치 피드백 개선 */
  .btn:active, .nav-link:active {
    transform: scale(0.98);
    transition: transform 0.1s ease;
  }

  /* 스크롤 개선 */
  .sidebar-content-wrapper,
  .navset-card-tab > .card > .card-body.tab-content,
  #chat-container {
    scroll-behavior: smooth;
    -webkit-overflow-scrolling: touch;
  }
}

/* 높은 DPI 디스플레이 최적화 */
@media (-webkit-min-device-pixel-ratio: 2), (min-resolution: 192dpi) {
  /* 아이콘과 텍스트의 선명도 개선 */
  .sidebar-header h2 {
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }

  /* 그림자 효과 세밀하게 조정 */
  .card {
    box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
  }
}

/* 다크 모드 지원 (시스템 설정 기반) */
@media (prefers-color-scheme: dark) {
  /* 현재는 라이트 모드만 지원하지만, 필요시 다크 모드 변수 추가 가능 */
}

/* 접근성 개선 */
@media (prefers-reduced-motion: reduce) {
  /* 애니메이션 최소화 */
  *, *::before, *::after {
    animation-duration: 0.01ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.01ms !important;
  }
}

/* 인쇄 최적화 */
@media print {
  .sidebar, .main-header, .page-footer {
    display: none !important;
  }

  .main-content {
    width: 100% !important;
    height: auto !important;
    overflow: visible !important;
  }

  .card {
    break-inside: avoid;
    box-shadow: none;
    border: 1px solid #ccc;
  }
}

/* iOS Safari 특별 최적화 */
@supports (-webkit-touch-callout: none) {
  /* iOS에서 고정 요소의 스크롤 문제 해결 */
  .sidebar {
    -webkit-transform: translate3d(0, 0, 0);
    transform: translate3d(0, 0, 0);
  }

  /* iOS에서 입력 확대 방지 */
  .form-control, .selectize-input {
    font-size: 16px;
  }
}

/* 매우 작은 화면 (320px 이하) - 구형 모바일 지원 */
@media (max-width: 320px) {
  .sidebar-header h2 {
    font-size: 1.5rem;
  }

  .main-tabs-container .nav-tabs .nav-link {
    padding: 0.4rem 0.5rem !important;
    font-size: 0.75rem;
  }

  .card .card-body {
    padding: 8px;
  }

  :root {
    --sidebar-width-mobile: 240px;
  }
}

/* 모바일용 추가 스타일 */
.mobile-radio-group .radio-group-buttons {
  font-size: 0.8rem;
}

@media (max-width: 576px) {
  .mobile-radio-group .radio-group-buttons .btn {
    padding: 0.4rem 0.6rem;
    font-size: 0.75rem;
  }
}

/* =================================================================== */
/* ### 지도 높이 문제 해결을 위한 핵심 CSS 코드 (높이 개선 버전) ### */
/* =================================================================== */

/* 1. 전체 페이지의 최소 높이를 뷰포트 높이로 설정 */
html, body {
    height: 100vh;
    min-height: 100vh;
}

/* 2. main-content가 적절한 높이를 가지도록 설정 */
.main-content {
    min-height: 80vh; /* 뷰포트 높이의 80% 최소 보장 */
    display: flex;
    flex-direction: column;
}

/* 3. bslib 탭 레이아웃(.navset-card-tab)이 남은 공간을 모두 차지하도록 설정 */
.main-content > .navset-card-tab {
    flex-grow: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
}

/* 4. 탭을 감싸는 card와 card-body도 flex 컨테이너로 만들어 높이를 채움 */
.navset-card-tab > .card,
.navset-card-tab > .card > .card-body.tab-content {
    flex-grow: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
}

/* 5. 개별 탭(.tab-pane)이 적절한 높이를 가지도록 설정 (개선된 버전) */
.tab-content > .tab-pane {
    height: 100%;
    min-height: 60vh; /* 뷰포트 높이의 60% 최소 보장 */
    /* 탭 전환 시 렌더링 최적화 */
    opacity: 0;
    transition: opacity 0.1s ease-in-out;
}

.tab-content > .tab-pane.active {
    opacity: 1;
}

/* 6. 지도가 포함된 탭(#nav-navbar-map_tab)의 특별 설정 */
.tab-content > #nav-navbar-map_tab {
    padding: 0 !important;
    min-height: 70vh; /* 지도 탭은 더 큰 최소 높이 */
}

/* 7. 최종적으로 leaflet 지도 객체(#map)가 컨테이너를 적절히 채우도록 보장 */
#map {
    height: 100% !important;
    width: 100% !important;
    min-height: 60vh; /* 뷰포트 높이의 60% 최소 보장 */
    position: relative;
    z-index: 1;
    /* 지도 로딩 개선 */
    background-color: #f8f9fa;
}

/* 8. 반응형 디자인 - 모바일에서는 높이를 조정 */
@media (max-width: 768px) {
    .main-content {
        min-height: 90vh; /* 모바일에서는 더 많은 공간 활용 */
    }
    
    .tab-content > .tab-pane {
        min-height: 50vh;
    }
    
    .tab-content > #nav-navbar-map_tab {
        min-height: 60vh;
    }
    
    #map {
        min-height: 50vh;
    }
}

/* 9. 큰 화면에서는 더 넉넉한 높이 제공 */
@media (min-width: 1200px) {
    .main-content {
        min-height: 85vh;
    }
    
    .tab-content > #nav-navbar-map_tab {
        min-height: 75vh;
    }
    
    #map {
        min-height: 70vh;
    }
}

"

# --- 3. UI 모듈 및 객체 정의 ---

# 시작 화면 UI
# [핵심 수정] 시작 화면 UI를 더 세련된 안내 화면으로 변경
start_screen_ui <- div(id = "start_screen",
                       style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: #2C3E50; z-index: 9999; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                       div(
                         # [수정된 부분] h1 태그 안에 span으로 v2 추가
                         h1("Smart AED Dashboard ", 
                            tags$span("v2.2", style = "font-size: 0.6em; vertical-align: middle; color: #ffc107;"), 
                            style = "color: white; text-align: center;"),
                         br(),
                         actionButton("start_app_btn", "대시보드 시작하기", class = "btn-primary btn-lg")
                       )
)


# 사이드바 UI (버튼 위치 수정됨)
sidebar_ui <- div(class = "sidebar",
                  div(class = "sidebar-content-wrapper",
                      div(class = "sidebar-header", id = "home-logo",
                          # 'X' 버튼 코드를 여기서 삭제합니다.
                          h2("AED"),
                          p("Smart Dashboard")
                      ),
                      
                      # [수정] 제목과 버튼을 한 줄에 배치하기 위한 div 추가
                      div(style = "display: flex; justify-content: space-between; align-items: center;",
                          h5("지도 필터", style = "margin: 0;"), # h5의 기본 마진 제거
                          # [새로운 위치] 'X' 버튼을 이곳으로 이동
                          tags$label(`for`="sidebar-toggle-checkbox", class="sidebar-close-btn", icon("times"))
                      ),
                      
                      selectInput("sido_filter", "시/도:", choices = NULL),
                      uiOutput("sigungu_filter_ui"),
                      textInput("place_search", "장소/장비번호 검색:", placeholder = "예: 인천시청"),
                      
                      actionButton("apply_filters_btn", "필터 적용", icon = icon("filter"), class = "btn-primary w-100 mt-3 mb-2 btn-sidebar-action"),
                      actionButton("reset_filters", "필터 초기화", icon = icon("undo"), class = "btn-secondary w-100 mb-2 btn-sidebar-action"),
                      actionButton("find_nearby_btn", "내 주변 응급자원 찾기", icon = icon("street-view"), class = "btn-info w-100 btn-sidebar-action"),
                      
                      hr(),
                      
                      h5("지도 표시 옵션"),
                      
                      radioGroupButtons(
                        inputId = "map_display_type",
                        label = "표시 유형",
                        choices = c("경계만" = "boundaries_only", "클러스터" = "markers", "상태별" = "dots", "히트맵" = "heatmap"),
                        selected = "markers",
                        justified = TRUE,
                        size = "sm"
                      ),
                      
                      conditionalPanel(
                        condition = "input.map_display_type == 'dots'",
                        uiOutput("status_filter_ui")
                      ),
                      
                      selectInput("choropleth_metric", "통계 지도 (단계 구분도)",
                                  choices = c("표시 안함" = "none",
                                              "총 AED 수" = "count",
                                              "인구 10만명 당 AED" = "count_per_100k",
                                              "면적(km²) 당 AED" = "count_per_area"),
                                  selected = "count"),
                      
                      radioGroupButtons(
                        inputId = "map_mandatory_filter",
                        label = "의무설치기관 필터",
                        choices = c("전체", "의무", "의무 외"),
                        selected = "전체",
                        justified = TRUE, size = "sm"
                      ),
                      
                      hr(),
                      
                      h5("레이어 및 분석"),
                      
                      div(class="layer-item",
                          tags$label("응급의료기관 표시", class="form-label"),
                          materialSwitch("show_hospitals", value = FALSE, status = "primary")
                      ),
                      div(class="layer-item",
                          tags$label("유동인구(시군구) 표시", class="form-label"),
                          materialSwitch("show_floating_pop", value = FALSE, status = "primary")
                      ),
                      div(class="layer-item",
                          tags$label("서비스 반경 표시", class="form-label"),
                          materialSwitch("toggle_buffers", value = FALSE, status = "primary")
                      )
                  )
)

# 탭: 지도 현황 (올바르게 수정된 버전)
# ui.R 파일의 tab_map_ui 부분을 아래 코드로 교체하세요.

tab_map_ui <- tabPanel("지도 현황", value = "map_tab", icon = icon("map-location-dot"),
                       # 지도를 감싸는 부모 div를 만들고 position: relative를 부여합니다.
                       div(style = "position: relative; height: 100%; width: 100%;",
                           leafletOutput("map", height = "100%"),
                           
                           # [핵심 추가] 지도 위에 떠 있는 하단 안내 배너 UI
                           shinyjs::hidden(
                             div(id = "loading_banner",
                                 style = "position: absolute; bottom: 0; left: 0; width: 100%; background: rgba(255, 255, 255, 0.95);
                         padding: 20px; box-shadow: 0 -5px 15px rgba(0,0,0,0.1); border-top: 1px solid #ddd;
                         z-index: 1001; text-align: left; font-size: 1.0em; max-height: 80vh; overflow-y: auto;", # 스타일 수정
                                 
                                 tags$h4("Smart AED 대시보드 활용법 🗺️", style = "font-weight: 600; text-align: center; margin-bottom: 15px;"),
                                 
                                 # --- 기본 기능 ---
                                 p(tags$b("지도 제어:"), " 왼쪽 사이드바 메뉴로 원하는 지역을 필터링하거나 표시 유형(클러스터, 히트맵 등)을 변경할 수 있습니다."),
                                 p(tags$b("상세 정보:"), " 지도 위의 마커나 지역을 클릭하면 상세 정보를 보거나 해당 지역으로 확대할 수 있습니다."),
                                 p(tags$b("내 주변 자원 찾기:"), " '내 주변 찾기' 버튼을 누른 후 지도에 원하는 위치를 클릭하면, 가장 가까운 AED와 응급의료기관 정보를 거리, 예상 시간과 함께 보여줍니다."),
                                 
                                 
                                 div(style="text-align: center; margin-top: 20px;",
                                     actionButton("close_banner_btn", "확인하고 지도 보기", icon = icon("check"), class = "btn-primary")
                                 )
                             )
                           )
                       )
)

# 탭: 세부 현황 분석
tab_detail_dashboard_ui <- tabPanel("세부 현황 분석", value = "dashboard_tab", icon = icon("dashboard"),
                                    h3(textOutput("detail_dashboard_title")),
                                    fluidRow(
                                      column(12, md = 3, uiOutput("total_aed_box")),
                                      column(12, md = 3, uiOutput("ready_aed_box")),
                                      column(12, md = 3, uiOutput("expired_aed_box")),
                                      column(12, md = 3, uiOutput("inspection_needed_box"))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(12, md = 6,
                                             card(card_header("전체 점검률"), echarts4rOutput("inspection_chart_overall", height="250px") %>% withSpinner())
                                      ),
                                      column(12, md = 6,
                                             card(card_header("의무기관 점검률"), echarts4rOutput("inspection_chart_mandatory", height="250px") %>% withSpinner())
                                      )
                                    ),
                                    fluidRow(
                                      column(12, md = 6,
                                             card(card_header("배터리 유효기간 현황"), echarts4rOutput("battery_expiry_chart", height="250px") %>% withSpinner())
                                      ),
                                      column(12, md = 6,
                                             card(card_header("패치 유효기간 현황"), echarts4rOutput("patch_expiry_chart", height="250px") %>% withSpinner())
                                      )
                                    ),
                                    fluidRow(
                                      column(12, md = 6,
                                             card(card_header("설치장소 대분류 Top 10"), echarts4rOutput("facility_type_chart_1", height="300px") %>% withSpinner())
                                      ),
                                      column(12, md = 6,
                                             card(card_header("설치장소 중분류 Top 10"), echarts4rOutput("facility_type_chart_2", height="300px") %>% withSpinner())
                                      )
                                    )
)

# 탭: 장비 상세 목록 (체크박스 필터 추가)
# 탭: 장비 상세 목록 (체크박스 및 지역 필터 추가)
tab_detail_list_ui <- tabPanel("장비 상세 목록", value = "list_tab", icon = icon("list"),
                               div(class="container-fluid",
                                   p("지도 탭에서 필터를 적용하거나, 아래에서 직접 지역을 선택하여 조회할 수 있습니다."),
                                   
                                   # --- [추가된 부분 시작] ---
                                   fluidRow(
                                     column(4, selectInput("list_sido_filter", "시/도 선택:", choices = NULL)),
                                     column(4, uiOutput("list_sigungu_filter_ui"))
                                   ),
                                   # --- [추가된 부분 끝] ---
                                   
                                   hr(),
                                   checkboxGroupInput("issue_filter", "문제 유형 필터:",
                                                      choices = c(
                                                        "배터리 만료" = "is_battery_expired",
                                                        "패치 만료" = "is_patch_expired",
                                                        "장비 기한 만료" = "is_expired_overall",
                                                        "점검 필요" = "is_inspection_needed",
                                                        "위치정보 누락" = "no_location"
                                                      ),
                                                      selected = c("is_battery_expired", "is_patch_expired", "is_expired_overall", "is_inspection_needed"),
                                                      inline = TRUE),
                                   actionButton("generate_report_btn", "필터링된 항목 점검요청 메시지 생성", icon = icon("comment-dots"), class="btn-warning mb-3"),
                                   DT::DTOutput("table") %>% withSpinner()
                               )
)

# 탭: 월별 비교 분석
tab_monthly_comparison_ui <- tabPanel("월별 비교 분석", value = "comparison_tab", icon = icon("chart-line"),
                                      fluidRow(
                                        column(6, selectInput("month_a_select", "비교 월(A)", choices = c("01", "02", "03", "04", "05", "06", "07", "08"), selected = "07")),
                                        column(6, selectInput("month_b_select", "비교 월(B)", choices = c("01", "02", "03", "04", "05", "06", "07", "08"), selected = "08"))
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(12, md=3, uiOutput("comp_total_aed_box")),
                                        column(12, md=3, uiOutput("comp_ready_aed_box")),
                                        column(12, md=3, uiOutput("comp_inspection_box")),
                                        column(12, md=3, uiOutput("comp_expired_box"))
                                      ),
                                      br(),
                                      fluidRow(
                                        column(12, md=6, card(card_header("월별 총 대수 비교"), echarts4rOutput("comparison_change_chart") %>% withSpinner())),
                                        column(12, md=6, card(card_header("월별 점검률 비교"), echarts4rOutput("comparison_inspection_chart") %>% withSpinner()))
                                      ),
                                      card(
                                        card_header("시도별 상세 비교 데이터"),
                                        DTOutput("comparison_detail_table") %>% withSpinner()
                                      )
)

# 탭: 심층 분석
menu_analysis_ui <- nav_menu("심층 분석", icon = icon("magnifying-glass-chart"),
                             # 1. 유동인구 기반 분석
                             tabPanel("유동인구 기반 분석", value = "pop_analysis_tab",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h5("분석 조건 설정"),
                                                     selectInput("pop_age_filter", "연령대 필터", choices=c("전체")),
                                                     selectInput("pop_time_filter", "시간대 필터", choices=c("전체")),
                                                     sliderInput("pop_weight", "유동인구 가중치", min = 0, max = 2, value = 1, step = 0.1),
                                                     sliderInput("aed_density_weight_pop", "AED 밀도 가중치", min = 0, max = 2, value = 1, step = 0.1),
                                                     actionButton("run_pop_analysis", "분석 실행", icon=icon("play"), class="btn-primary w-100")
                                        ),
                                        mainPanel(width = 9,
                                                  fluidRow(
                                                    column(8, card(card_header("AED 필요지수 지도"), leafletOutput("pop_analysis_map", height="60vh") %>% withSpinner())),
                                                    column(4,
                                                           uiOutput("pop_analysis_summary_ui"),
                                                           card(card_header("필요지수 상위 10개 지역"), DTOutput("pop_analysis_table"))
                                                    )
                                                  )
                                        )
                                      )
                             ),
                             # 2. 관리 취약 지역 분석
                             tabPanel("관리 취약 지역 분석", value = "issue_analysis_tab",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h5("분석 조건 설정"),
                                                     selectInput("issue_sido_filter", "지역 선택", choices=NULL),
                                                     checkboxGroupInput("issue_type_filter", "취약점 유형 선택",
                                                                        choices = c("장비 만료 임박(90일 내)" = "exp_imminent",
                                                                                    "배터리 교체 임박(90일 내)" = "batt_imminent",
                                                                                    "패치 교체 임박(90일 내)" = "patch_imminent",
                                                                                    "점검 필요" = "insp_needed"),
                                                                        selected = c("exp_imminent", "insp_needed")),
                                                     actionButton("run_issue_analysis", "분석 실행", icon=icon("play"), class="btn-primary w-100")
                                        ),
                                        mainPanel(width = 9,
                                                  fluidRow(
                                                    column(8, card(card_header("관리 취약 지역 히트맵"), leafletOutput("issue_heatmap", height="60vh") %>% withSpinner())),
                                                    column(4,
                                                           uiOutput("issue_summary_ui"),
                                                           card(card_header("취약 건수 상위 10개 지역"), DTOutput("issue_table"))
                                                    )
                                                  )
                                        )
                                      )
                             ),
                             # 3. 설치 최적 입지 분석
                             tabPanel("설치 최적 입지 분석", value = "optim_analysis_tab",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h5("분석 조건 설정"),
                                                     selectInput("optim_sido_filter", "지역 선택", choices=NULL),
                                                     uiOutput("optim_sigungu_filter_ui"),
                                                     sliderInput("er_dist_weight", "응급실 거리 가중치", min = 0, max = 2, value = 1, step = 0.1),
                                                     sliderInput("aed_density_weight_optim", "AED 밀도 가중치", min = 0, max = 2, value = 1, step = 0.1),
                                                     actionButton("run_optimization_analysis", "분석 실행", icon=icon("play"), class="btn-primary w-100")
                                        ),
                                        mainPanel(width = 9,
                                                  fluidRow(
                                                    column(8, card(card_header("AED 설치 필요지역 지도"), leafletOutput("optimization_map", height="60vh") %>% withSpinner())),
                                                    column(4,
                                                           uiOutput("optimization_summary_ui"),
                                                           card(card_header("설치 필요 상위 10개 지역"), DTOutput("optimization_table"))
                                                    )
                                                  )
                                        )
                                      )
                             ),
                             # 4. 서비스 커버리지 분석
                             tabPanel("서비스 커버리지 분석", value = "coverage_analysis_tab",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h5("분석 조건 설정"),
                                                     selectInput("coverage_sido_filter", "분석 지역 (시/도 단위)", choices=NULL),
                                                     numericInput("aed_buffer_dist", "AED 서비스 반경(m)", value=500, min=100, max=2000, step=50),
                                                     numericInput("er_buffer_factor", "응급실 반경 계수", value=1.0, min=0.5, max=3.0, step=0.1),
                                                     actionButton("run_coverage_analysis", "분석 실행", icon=icon("play"), class="btn-primary w-100")
                                        ),
                                        mainPanel(width = 9,
                                                  fluidRow(
                                                    column(8, card(card_header("서비스 미보장 지역 지도"), leafletOutput("coverage_map", height="60vh") %>% withSpinner())),
                                                    column(4,
                                                           uiOutput("coverage_summary_ui"),
                                                           card(card_header("미보장률 상위 10개 지역"), DTOutput("coverage_table"))
                                                    )
                                                  )
                                        )
                                      )
                             ),
                             # 5. 골든 아워 접근성 분석
                             tabPanel("골든 아워 접근성 분석", value = "golden_hour_tab",
                                      sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     h5("분석 조건 설정"),
                                                     selectInput("golden_time_sido_filter", "분석 지역 (시/도 단위)", choices=NULL),
                                                     numericInput("walking_speed_kmh", "도보 속도(km/h)", value = 4.2, min=2, max=8, step=0.1),
                                                     p("4분(골든 타임) 이동 가능 거리가 계산됩니다.", style="font-size:0.9em; color: #6c757d;"),
  actionButton("run_golden_time_analysis", "분석 실행", icon=icon("play"), class="btn-primary w-100 mb-2"),
hr(),
h5("가상 AED 시뮬레이션"),
materialSwitch("toggle_simulation_mode", label="시뮬레이션 모드 활성화", status="warning"),
p("활성화 후 지도 클릭 시 가상 AED가 추가됩니다.", style="font-size:0.9em; color: #6c757d;"),
actionButton("reset_simulation", "가상 AED 초기화", icon=icon("undo"), class="btn-danger w-100")
),
mainPanel(width = 9,
          fluidRow(
            column(8, card(card_header("4분 내 접근 불가 지역 지도"), leafletOutput("golden_time_map", height="60vh") %>% withSpinner())),
            column(4,
                   uiOutput("golden_time_summary_ui"),
                   card(card_header("접근 불가율 상위 10개 지역"), DTOutput("golden_time_table"))
            )
          )
)
)
)
)

# 탭: 통계 리포트
# [교체] 아래 코드로 기존 tab_stats_ui 부분을 덮어쓰세요.
tab_stats_ui <- tabPanel("통계 리포트", value = "stats_tab", icon = icon("table"),
                         # [수정] div로 감싸고 높이를 지정하여 테이블 잘림 문제 해결
                         div(style = "height: calc(100vh - 250px);", 
                             fluidRow(
                               column(4, selectInput("stats_sido_filter", "시/도 선택", choices = NULL)),
                               column(4, uiOutput("stats_sigungu_filter_ui"))
                             ),
                             h4(textOutput("stats_title")),
                             br(),
                             navset_card_pill(
                               nav_panel("설치 밀도 통계", DTOutput("stats_table_density") %>% withSpinner()),
                               nav_panel("점검 현황 통계", DTOutput("stats_table_inspection") %>% withSpinner()),
                               nav_panel("관리 상태 통계", DTOutput("stats_table_management") %>% withSpinner()),
                               nav_panel("설치 장소별 통계", DTOutput("stats_table_by_class") %>% withSpinner())
                             )
                         )
)

# 탭: 자동 리포트
tab_auto_report_ui <- tabPanel("자동 리포트", value = "auto_report_tab", icon = icon("robot"),
                               div(class="container", style="max-width: 800px; margin-top: 30px;",
                                   card(
                                     card_header(h4(icon("robot"), " 정기 리포트 생성 및 발송")),
                                     p("선택한 지역의 월간 현황 리포트를 주기적으로 생성하여 이메일로 받아볼 수 있습니다. (현재는 시뮬레이션 기능)"),
                                     hr(),
                                     selectInput("report_sido_filter", "리포트 대상 지역", choices = NULL),
                                     textInput("report_email", "리포트 수신 이메일:", placeholder="example@email.com"),
                                     radioButtons("report_schedule", "발송 주기", choices = c("매일"="daily", "매주"="weekly", "매월"="monthly", "테스트(5분)"="test"), inline = TRUE),
                                     radioButtons("report_format", "파일 형식", choices = c("html", "pdf", "docx"), inline = TRUE),
                                     div(class="d-flex justify-content-end gap-2 mt-3",
                                         actionButton("activate_report_btn", "자동 리포트 활성화", icon=icon("check"), class="btn-success"),
                                         actionButton("deactivate_report_btn", "자동 리포트 비활성화", icon=icon("stop"), class="btn-danger")
                                     )
                                   )
                               )
)

# 탭: 예측 기반 관리
tab_prediction_ui <- tabPanel(
  title = "예측 기반 관리", value = "prediction_tab", icon = icon("gears"),
  div(style = "height: calc(100vh - 180px); overflow-y: auto; -webkit-overflow-scrolling: touch;",
      div(class = "container-fluid px-2 px-md-3 py-3",
          div(class = "d-flex flex-column flex-md-row align-items-start align-items-md-center gap-3 mb-3",
              h3(strong("소모품 교체주기 예측"), style = "font-size: clamp(1.2rem, 4vw, 1.5rem);"),
              div(class = "d-flex flex-column flex-sm-row gap-2",
                  selectInput("pred_sido_filter", "시/도 선택:", choices = NULL, width = "100%"),
                  uiOutput("pred_sigungu_filter_ui")
              )
          ),
          hr(),
          
          div(class = "d-flex flex-column flex-md-row align-items-start align-items-md-center gap-2 mb-3",
              h5("교체까지 남은 기간:", style="margin:0; font-size: clamp(0.9rem, 2.5vw, 1.1rem);"),
              radioGroupButtons(inputId = "prediction_period",
                                label = NULL,
                                justified = TRUE,
                                choices = c("30일", "60일", "90일"),
                                selected = "30일",
                                size = "sm",
                                individual = TRUE)
          ),
          br(),
          
          fluidRow(class = "g-2 g-md-3",
                   column(12, md = 6, uiOutput("battery_replacement_box_pred")),
                   column(12, md = 6, uiOutput("patch_replacement_box_pred"))
          ),
          br(),
          
          card(card_header("교체 필요 장비 상세 목록"),
               div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
                   DT::DTOutput("prediction_table")))
      )
  )
)

# 탭: 스마트 분류 어시스턴트
tab_assistant_ui <- tabPanel(
  title = "스마트 분류 어시스턴트", value = "assistant_tab", icon = bs_icon("robot"),
  div(
    style = "height: calc(100vh - 185px); overflow-y: auto; -webkit-overflow-scrolling: touch;",
    div(class = "container-fluid px-2 px-md-3 py-2",
        
        # 1. AI 분류 추천 카드
        card(
          card_header(h5(bs_icon("search"), " AI 분류 추천",
                         style = "font-size: clamp(1rem, 2.5vw, 1.1rem);")),
          p("장소명을 입력하면 AI가 법적 의무 여부와 표준 분류를 추천합니다.",
            style = "font-size: clamp(0.85rem, 2vw, 0.95rem);"),
          textInput("assistant_query", label = NULL,
                    placeholder = "예: 500세대 아파트, 인천공항, 가평군청"),
          actionButton("assistant_search_btn", "분석 실행",
                       icon = icon("play"), class = "btn-primary w-100 mb-3"),
          hr(),
          h6(strong("주요 장소 유형 (클릭)"),
             style = "font-size: clamp(0.9rem, 2vw, 1rem);"),
          div(class = "d-grid gap-2",
              actionButton("btn_apt", "공동주택 (아파트 등)",
                           class = "btn-light text-start"),
              actionButton("btn_gov", "공공보건의료기관 / 청사",
                           class = "btn-light text-start"),
              actionButton("btn_transport", "터미널 / 철도역사 / 공항",
                           class = "btn-light text-start"),
              actionButton("btn_stadium", "다중이용시설 (운동장/마트 등)",
                           class = "btn-light text-start")
          )
        ),
        
        # 2. 분류 결과 카드
        card(
          card_header(h5(bs_icon("clipboard2-check"), " 분류 결과",
                         style = "font-size: clamp(1rem, 2.5vw, 1.1rem);")),
          p("AI 분석과 빅데이터 추천을 바탕으로 한 분류 결과입니다.",
            style="font-size: clamp(0.8rem, 1.8vw, 0.85rem); color: var(--text-light-secondary);"),
          hr(style="margin-top:0;"),
          uiOutput("duty_result_ui"),
          uiOutput("detail_result_ui")
        ),
        
        # 3. Q&A 카드
        card(
          card_header(h5(bs_icon("patch-question"), " AED 지침 기반 Q&A",
                         style = "font-size: clamp(1rem, 2.5vw, 1.1rem);")),
          card_body(
            p("AED 설치 및 관리 지침에 대한 궁금한 점을 챗봇에게 물어보세요.",
              style = "font-size: clamp(0.85rem, 2vw, 0.95rem);"),
            actionButton("launch_chat_modal_btn", "Q&A 전체보기",
                         icon = icon("expand"), class = "btn-primary w-100")
          )
        )
    )
  )
)

# 탭: 품질 관리
tab_dqm_ui <- tabPanel(
  title = "품질 관리", value = "dqm_tab", icon = icon("check-to-slot"),
  div(style = "height: calc(100vh - 180px); overflow-y: auto; -webkit-overflow-scrolling: touch;",
      div(class = "container-fluid px-2 px-md-3 py-3",
          navset_card_pill(
            id = "dqm_sub_tabs",
            
            nav_panel(
              title = "분류 품질 검증",
              value = "dqm_validation_sub_tab",
              div(class = "container-fluid",
                  div(class = "row g-2 g-md-3 mb-3",
                      div(class = "col-12 col-md-4",
                          selectInput("dqm_rule_select",
                                      label = h5(strong("1. 검증 규칙 선택"),
                                                 style = "font-size: clamp(0.9rem, 2.5vw, 1.1rem);"),
                                      choices = c(
                                        "규칙 선택..." = "",
                                        "[분류1] 보건소/지소/진료소 검증" = "rule_health_center",
                                        "[분류1] 500세대 이상 공동주택 검증" = "rule_apartment",
                                        "[분류1] 공공기관/청사 검증" = "rule_government",
                                        "[분류1] 철도/공항/터미널 검증" = "rule_transport",
                                        "전체 미분류 데이터 조회" = "rule_unclassified"
                                      ))
                      ),
                      div(class = "col-12 col-md-3",
                          actionButton("run_dqm_check", "검증 실행",
                                       icon = icon("play"),
                                       class = "btn-primary w-100",
                                       style="margin-top: 32px;")
                      ),
                      div(class = "col-12 col-md-5",
                          uiOutput("dqm_summary_ui")
                      )
                  ),
                  hr(),
                  
                  card(
                    card_header(
                      div(class = "d-flex flex-column flex-sm-row justify-content-between align-items-start align-items-sm-center gap-2",
                          h5(strong(textOutput("dqm_table_title")),
                             style="margin:0; font-size: clamp(0.95rem, 2.5vw, 1.1rem);"),
                          downloadButton("download_dqm_list", "목록 엑셀 저장",
                                         icon=icon("file-excel"),
                                         class="btn-success btn-sm")
                      )
                    ),
                    div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
                        uiOutput("dqm_table_area_ui"))
                  )
              )
            ),
            
            nav_panel(
              title = "신규/오류 신고",
              value = "dqm_report_sub_tab",
              div(class = "container-fluid",
                  uiOutput("dqm_form_ui")
              )
            )
          )
      )
  )
)

# --- 4. UI 최종 조립 ---
ui <- page_fillable(
  # 기본 설정
  useShinyjs(),
  use_waiter(),
  extendShinyjs(text = js_code, functions = c("scrollChat")),
  
  # HTML head 태그
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=5.0, user-scalable=yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "format-detection", content = "telephone=no"),
    
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=""),
    tags$link(href="https://fonts.googleapis.com/css2?family=Noto+Sans+KR:wght@300;400;500;700&family=Poppins:wght@400;600;800&display=swap", rel="stylesheet"),
    
    cicerone::use_cicerone(),
    rclipboard::rclipboardSetup(),
    
    tags$script('
      Shiny.addCustomMessageHandler("get_geolocation", function(message) {
        if (navigator.geolocation) {
          var options = {
            enableHighAccuracy: true,
            timeout: 10000,
            maximumAge: 300000
          };

          navigator.geolocation.getCurrentPosition(
            function(position) {
              Shiny.setInputValue("client_location", {
                lat: position.coords.latitude,
                lng: position.coords.longitude,
                accuracy: position.coords.accuracy
              }, {priority: "event"});
            },
            function(error) {
              Shiny.setInputValue("geolocation_error", {
                code: error.code,
                message: error.message
              }, {priority: "event"});
            },
            options
          );
        } else {
          Shiny.setInputValue("geolocation_error", {
            code: -1,
            message: "Geolocation is not supported by this browser."
          }, {priority: "event"});
        }
      });

      $(document).ready(function() {
        if (window.isMobile) {
          document.body.classList.add("mobile-device");

          setTimeout(function() {
            window.scrollTo(0, 1);
          }, 100);
        }

        $(window).on("orientationchange", function() {
          setTimeout(function() {
            $(window).trigger("resize");
          }, 500);
        });
      });
    '),
    
    tags$style(HTML(css_code))
  ),
  
  start_screen_ui,
  
  shinyjs::hidden(
    div(id = "main_dashboard",
        tags$input(type = "checkbox", id = "sidebar-toggle-checkbox", style="display:none;"),
        
        # --- [추가] 모바일 전용 플로팅 버튼 ---
        tags$label(`for`="sidebar-toggle-checkbox",
                   id="mobile-sidebar-toggle",
                   icon("bars")),
        # ------------------------------------
        
        div(class = "page-container",
            sidebar_ui,
            
            div(class = "main-content",
                div(class = "main-header",
                    tags$label(`for`="sidebar-toggle-checkbox", class="sidebar-toggle-btn", # <-- 여기에 추가
                               icon("bars"),
                               tags$span(class="toggle-text", "사이드바")
                    ),
                    
                    tags$div(style = "flex-grow: 1; text-align: center; font-weight: bold; font-size: clamp(1rem, 3vw, 1.2rem);",
                             "Smart AED Dashboard"),
                    
                    div(id="data-selector",
                        style = "width: clamp(140px, 20vw, 160px);",
                        selectInput("data_month_select",
                                    label = NULL,
                                    choices = c("2025년 8월" = "08",
                                                "2025년 7월" = "07",
                                                "2025년 6월" = "06",
                                                "2025년 5월" = "05",
                                                "2025년 4월" = "04",
                                                "2025년 3월" = "03",
                                                "2025년 2월" = "02",
                                                "2025년 1월" = "01"),
                                    selected = "08")
                    ),
                    
                    actionButton("help_btn", label=NULL, icon=icon("question-circle"),
                                 style="background:none; border:none; font-size: 1.4rem; color: #555; min-width: 44px; min-height: 44px;")
                ),
                
                navset_card_tab(
                  id = "navbar", selected = "map_tab",
                  
                  tab_map_ui,
                  tab_detail_dashboard_ui,
                  tab_detail_list_ui,
                  tab_monthly_comparison_ui,
                  menu_analysis_ui,
                  tab_stats_ui,
                  tab_auto_report_ui,
                  tab_prediction_ui,
                  tab_assistant_ui,
                  tab_dqm_ui
                ),
                
                div(class = "page-footer",
                    div(
                      style = "font-size: clamp(0.7rem, 1.5vw, 0.8rem); text-align: center;",
                      HTML(
                        paste0(
                          "&copy; ", format(Sys.Date(), "%Y"), " Emergency Medical Support Center (Daegu, Incheon, Sejong). All Rights Reserved.",
                          "<br>",
                          "Data Source: National Emergency Medical Center (NEMC)"
                        )
                      )
                    )
                )
                
            )
        )
    )
  ),
  
  div(id = "mobile-debug", style = "display: none;",
      textOutput("screen_size_debug")
  )
)
























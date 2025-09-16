⏺ SAP ABAP SY-UNAME Tracker 사용 가이드 📋

> 🚀 **빠른 배포를 원하시나요?** → [초간단 배포 가이드](DEPLOY_GUIDE.md) | [상세 한국어 가이드](README_KR.md) | [SAP GUI 수동 배포](SAP_MANUAL_DEPLOY.md)를 참조하세요!

  프로젝트 소개

  이 도구는 SAP ABAP 프로그램에서 sy-uname(사용자 ID) 데이터가 어떻게 흐르는지 추적하여, 사용자 정보가 데이터베이스 테이블, RFC 호출, 기타 작업에 미치는 영향을
  식별하는 정적 분석 도구입니다.

  🎯 주요 기능

  - Taint Analysis: sy-uname으로부터 시작된 데이터 흐름을 추적
  - 데이터베이스 작업 감지: INSERT, UPDATE, DELETE, MODIFY 문 분석
  - RFC/BAPI 호출 추적: 사용자 정보가 전달되는 외부 호출 감지
  - CSV 리포트 생성: 분석 결과를 구조화된 형식으로 출력

  📦 설치 방법

  1. SAP 시스템에 프로그램 업로드
  SE80 → 로컬 객체 → 우클릭 → Import
  또는
  SAPLINK를 사용하여 전체 패키지 가져오기
  2. 필요한 컴포넌트 확인
    - 메인 프로그램: ZSYUNAME_ANALYZER
    - 클래스: ZCL_SYUNAME_SCANNER, ZCL_SYUNAME_TAINT, ZCL_SYUNAME_PARSER, ZCL_SYUNAME_REPORT
    - 예외 클래스: ZCX_SYUNAME_ERROR

  🚀 사용 방법

  1. 기본 실행

  " SE38에서 실행
  ZSYUNAME_ANALYZER

  " 파라미터 입력:
  - Program Name: ZTEST_PROGRAM  (분석할 프로그램명)
  - Analysis Mode: L              (L=로컬, S=서버)
  - Output File: /tmp/results.csv (결과 파일 경로)
  - Debug Mode: [ ]               (디버그 로그 활성화)

  2. 명령줄 실행

  SUBMIT ZSYUNAME_ANALYZER
    WITH p_prog = 'ZTEST_PROGRAM'
    WITH p_mode = 'L'
    WITH p_output = '/usr/sap/tmp/analysis.csv'
    AND RETURN.

  3. 프로그래밍 방식 호출

  DATA: lo_factory TYPE REF TO zcl_syuname_factory,
        lo_scanner TYPE REF TO zif_syuname_scanner,
        lo_taint   TYPE REF TO zif_syuname_taint,
        lo_parser  TYPE REF TO zif_syuname_parser,
        lo_report  TYPE REF TO zif_syuname_report.

  " 팩토리를 통한 객체 생성
  CREATE OBJECT lo_factory.
  lo_scanner = lo_factory->create_scanner( ).
  lo_taint = lo_factory->create_taint_engine( ).
  lo_parser = lo_factory->create_parser( ).
  lo_report = lo_factory->create_reporter( ).

  " 프로그램 분석
  lo_scanner->read_program( 'ZTEST_PROGRAM' ).
  lo_taint->analyze( lo_scanner->get_source( ) ).
  lo_report->generate( lo_taint->get_results( ) ).

  📊 출력 형식

  CSV 파일 구조:
  순번,프로그램명,라인번호,작업유형,대상,필드명,추적경로
  1,ZTEST_PROG,45,INSERT,ZTABLE,CREATED_BY,sy-uname→lv_user→ls_data-created_by
  2,ZTEST_PROG,78,UPDATE,ZLOG,CHANGED_BY,sy-uname→lv_current_user→update_fields
  3,ZINCLUDE1,120,CALL_FUNCTION,Z_RFC_UPDATE,IV_USER,sy-uname→lv_user→parameter

  🔍 분석 모드

  1. 로컬 모드 (L): 단일 프로그램만 분석
  2. 서버 모드 (S): Include 파일까지 포함하여 전체 분석
  3. 패키지 모드 (P): 전체 패키지 분석 (향후 구현 예정)

  ⚙️ 고급 옵션

  특정 프로그램 제외

  " Selection Screen에서 Exclude Programs 사용
  s_excl-sign = 'I'.
  s_excl-option = 'EQ'.
  s_excl-low = 'ZTEST_EXCLUDE'.
  APPEND s_excl.

  디버그 모드 활성화

  p_debug = abap_true.
  " SLG1에서 로그 확인 가능

  📈 성능 최적화 팁

  1. 대용량 프로그램 분석 시:
    - 먼저 작은 테스트 프로그램으로 동작 확인
    - Include 제외 옵션 활용
    - 서버 백그라운드 작업으로 실행
  2. 메모리 관리:
    - 10만 라인 이상 프로그램은 배치 작업 권장
    - 결과 파일을 서버 경로에 저장

  🐛 문제 해결

  자주 발생하는 오류:

  1. Authority Check 실패: S_DEVELOP 권한 필요
  2. 파일 쓰기 오류: AL11에서 디렉토리 권한 확인
  3. 메모리 부족: 프로그램을 작은 단위로 분할 분석

  로그 확인:

  트랜잭션 SLG1 실행
  Object: ZSYUNAME
  Subobject: ANALYZER

  🔧 확장 및 커스터마이징

  새로운 패턴 추가:

  " ZCL_SYUNAME_PARSER 클래스 확장
  METHOD add_custom_pattern.
    " 커스텀 데이터 흐름 패턴 추가
  ENDMETHOD.

  출력 형식 변경:

  " ZCL_SYUNAME_REPORT 클래스 수정
  " JSON, XML 등 다른 형식 지원 추가 가능

  📚 사용 사례

  1. 보안 감사: 사용자 정보 노출 위험 식별
  2. GDPR 준수: 개인정보 처리 위치 파악
  3. 코드 리뷰: 자동화된 데이터 흐름 분석
  4. 마이그레이션: S/4HANA 전환 시 영향도 분석

  💡 베스트 프랙티스

  1. 정기적인 분석 스케줄링 (주간/월간)
  2. 중요 트랜잭션 코드 우선 분석
  3. 결과를 팀과 공유하여 코드 품질 개선
  4. 새로운 개발 전 기존 패턴 분석

  이 도구를 통해 SAP 시스템의 사용자 데이터 흐름을 체계적으로 관리하고 보안 취약점을 사전에 발견할 수 있습니다!

# SAP ABAP SY-UNAME Tracker

## Overview
Static analysis tool for SAP ABAP that tracks sy-uname (user ID) data flow through code to identify where user information impacts database tables, RFC calls, and other operations.

## Features
- Tracks sy-uname assignments and propagation through variables
- Identifies database operations (INSERT, UPDATE, DELETE, MODIFY) with tainted data
- Detects RFC calls with tainted parameters
- Supports structure field tracking and MOVE-CORRESPONDING
- Handles nested includes and complex program structures
- CSV output for easy analysis
- Debug mode with detailed logging

## Installation

### Prerequisites
- SAP NetWeaver 7.40 or higher
- Developer access to SE80/SE38
- Authorization for creating Z* objects

### Setup Steps
1. Create package Z_SYUNAME in SE80
2. Import all ABAP objects from src/abap/ directory
3. Activate all objects in dependency order:
   - Exception classes first
   - Interfaces
   - Implementation classes
   - Factory class
   - Main program

## 🚀 SAP 서버 배포 가이드 (한국어)

### 📦 단일 파일 배포 방법 (가장 간단함)

이 프로젝트는 **ZSYUNAME_ANLZ.abap** 단일 파일로 모든 기능이 통합되어 있어 배포가 매우 간단합니다.

#### 1단계: 프로그램 생성
```
1. SAP 로그온
2. T-Code: SE38 또는 SE80 실행
3. 프로그램명 입력: ZSYUNAME_ANLZ
4. "생성" 버튼 클릭
5. 속성 입력:
   - Title: SY-UNAME Tracker
   - Type: Executable Program (1)
   - Package: $TMP (로컬) 또는 Z 패키지
```

#### 2단계: 소스 코드 복사
```
1. GitHub의 ZSYUNAME_ANLZ.abap 파일 전체 내용 복사
2. SE38 편집기에 붙여넣기
3. "저장" (Ctrl+S)
4. "구문 검사" (Ctrl+F2)
5. "활성화" (Ctrl+F3)
```

#### 3단계: 실행 및 테스트
```
1. SE38에서 ZSYUNAME_ANLZ 실행 (F8)
2. 파라미터 입력:
   - Program Name: 분석할 프로그램명
   - Output Path: /tmp/results.csv (또는 원하는 경로)
3. "실행" (F8)
```

### 📋 배포 전 체크리스트

- [ ] SAP 개발 권한 확인 (S_DEVELOP)
- [ ] 파일 쓰기 권한 확인 (S_DATASET)
- [ ] AL11에서 출력 디렉토리 권한 확인

### 🔧 Transport Request 생성 (운영 서버 이관용)

#### 방법 1: 직접 생성
```
1. T-Code: SE09 또는 SE10
2. "생성" 버튼 클릭
3. Request 유형: Workbench Request
4. 설명: SY-UNAME Tracker 배포
5. 프로그램 ZSYUNAME_ANLZ 추가
```

#### 방법 2: 프로그램 저장 시 자동 생성
```
1. SE38에서 프로그램 저장 시
2. Package 지정 (Z로 시작하는 개발 패키지)
3. Transport Request 자동 생성 또는 기존 Request 선택
```

### 🎯 사용 예제

#### 기본 실행
```abap
" SE38에서 직접 실행
ZSYUNAME_ANLZ

" 파라미터:
P_PROG = 'ZTEST_PROGRAM'     " 분석할 프로그램
P_OUTPUT = '/tmp/result.csv'  " 결과 파일
```

#### 프로그램에서 호출
```abap
SUBMIT ZSYUNAME_ANLZ
  WITH p_prog = 'Z_MY_PROGRAM'
  WITH p_output = '/usr/sap/tmp/analysis.csv'
  AND RETURN.
```

### 📊 결과 확인

1. **파일 확인 (AL11)**
```
T-Code: AL11
디렉토리 선택: /tmp 또는 지정한 경로
파일명: results.csv
```

2. **CSV 형식**
```csv
Seq,Program,Line,Operation,Target,Field,TaintPath
1,ZTEST,45,INSERT,ZTABLE,USER_ID,SY-UNAME:45
2,ZTEST,78,UPDATE,ZLOG,CHANGED_BY,SY-UNAME:10->LV_USER:78
```

### ⚠️ 주의사항

1. **프로그램명 길이**: ABAP는 30자 제한이 있음 (ZSYUNAME_ANLZ는 OK)
2. **권한**: 분석 대상 프로그램 읽기 권한 필요
3. **출력 경로**: 서버 경로 쓰기 권한 확인 필요

### 🔍 문제 해결

#### 권한 오류
```
해결: Basis 팀에 S_DEVELOP, S_DATASET 권한 요청
```

#### 파일 쓰기 실패
```
1. AL11에서 디렉토리 권한 확인
2. 대안 경로: /usr/sap/tmp/ 사용
```

#### 프로그램을 찾을 수 없음
```
1. 프로그램명 정확히 확인 (대문자)
2. 프로그램 활성화 상태 확인
```

### 💡 팁

1. **로컬 테스트 먼저**: $TMP 패키지로 먼저 테스트
2. **작은 프로그램부터**: 간단한 프로그램으로 동작 확인
3. **배치 작업**: 대용량 프로그램은 SM36으로 백그라운드 실행

### 📞 지원

문제 발생 시:
1. ST22에서 덤프 확인
2. SLG1에서 Application Log 확인
3. GitHub Issues에 문제 제보

## Usage

### Basic Analysis
```abap
ZSYUNAME_ANALYZER p_prog='ZTEST_PROGRAM' p_mode='L' p_output='/tmp/results.csv'
```

### Full Analysis with Includes
```abap
ZSYUNAME_ANALYZER p_prog='ZTEST_PROGRAM' p_mode='F' p_output='/tmp/results.csv'
```

### Debug Mode
```abap
ZSYUNAME_ANALYZER p_prog='ZTEST_PROGRAM' p_debug='X'
```

## Output Format

CSV columns:
- **A**: Sequence number
- **B**: Program/Include name
- **C**: sy-uname line number
- **D**: Operation (INSERT/UPDATE/CALL_FUNCTION/etc)
- **E**: Target table/RFC name
- **F**: Field name
- **G+**: Taint path steps

Example output:
```csv
Seq,Program,Include,Line,Operation,Table,RFC,Field,Path
1,ZTEST,ZTEST,10,INSERT,ZTABLE,,,sy-uname -> lv_user -> INSERT
2,ZTEST,ZTEST_INC,25,CALL_FUNCTION,,Z_UPDATE_USER,,sy-uname -> ls_data-user -> RFC
```

## Architecture

### Core Components
- **ZCL_SYUNAME_SCANNER**: Reads ABAP source code and resolves includes
- **ZCL_SYUNAME_TAINT**: Tracks tainted variables and propagation
- **ZCL_SYUNAME_PARSER**: Parses ABAP statements using SCAN ABAP-SOURCE
- **ZCL_SYUNAME_REPORT**: Generates CSV output and statistics
- **ZCL_SYUNAME_FACTORY**: Creates component instances

### Interfaces
- **ZIF_SYUNAME_SCANNER**: Scanner contract
- **ZIF_SYUNAME_TAINT**: Taint engine contract
- **ZIF_SYUNAME_PARSER**: Parser contract
- **ZIF_SYUNAME_REPORT**: Reporter contract

## Testing

### Unit Tests
Each implementation class includes embedded ABAP Unit tests:
```abap
SE80 → [Class Name] → Test → Execute Unit Test
```

### Integration Tests
Run the integration test program:
```abap
SE38 → ZSYUNAME_TEST_INTEGRATION → Execute
```

## Performance
- Processes 10,000+ lines per minute
- Handles programs up to 100,000 lines
- Progress indicator for large programs (>5000 lines)

## Limitations
- Requires SAP NetWeaver 7.40+
- Dynamic SQL table names marked as medium confidence
- Field symbols partially supported
- Performance methods not fully tracked

## Development Status
✅ Phase 3.1: Foundation Objects - Complete
✅ Phase 3.2: TDD Tests - Complete
✅ Phase 3.3: Core Implementation - Complete
✅ Phase 3.4: Main Program - Complete
⏳ Phase 3.5: Integration & Validation - Pending
⏳ Phase 3.6: Polish & Documentation - Pending

## License
Internal use only - SAP proprietary

## Support
For issues or questions, contact the development team.
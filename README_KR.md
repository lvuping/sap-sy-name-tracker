# 🔍 SAP ABAP SY-UNAME 추적기

## 📌 프로젝트 소개
이 도구는 SAP ABAP 코드에서 사용자 ID(sy-uname)가 어떻게 사용되는지 추적하는 정적 분석 도구입니다. 데이터베이스 테이블, RFC 호출 등에서 사용자 정보가 어떤 영향을 미치는지 분석합니다.

## 🚀 간단한 배포 방법

### 방법 1: 자동 배포 (권장) ⭐

#### Windows 사용자:
```batch
# 1. 명령 프롬프트 열기
# 2. 프로젝트 폴더로 이동
cd C:\your_project_path\sy-name

# 3. 배포 스크립트 실행
deploy\deploy.bat
```

#### Mac/Linux 사용자:
```bash
# 1. 터미널 열기
# 2. 프로젝트 폴더로 이동
cd /your_project_path/sy-name

# 3. 실행 권한 부여
chmod +x deploy/deploy.sh

# 4. 배포 스크립트 실행
./deploy/deploy.sh
```

### 방법 2: SAP GUI 수동 배포 📋

#### 단계 1: 패키지 생성
1. SAP GUI 로그인
2. 트랜잭션 코드 `SE80` 실행
3. 패키지 생성: `Z_SYUNAME_TRACKER`
   - Repository Browser에서 "Package" 선택
   - 패키지 이름 입력: `Z_SYUNAME_TRACKER`
   - 마우스 오른쪽 클릭 → Create

#### 단계 2: 객체 생성 및 코드 복사
다음 순서대로 객체를 생성하고 코드를 복사합니다:

##### 2.1 인터페이스 생성 (SE24)
```
ZIF_SYUNAME_SCANNER
ZIF_SYUNAME_TAINT
ZIF_SYUNAME_PARSER
ZIF_SYUNAME_REPORT
```

##### 2.2 예외 클래스 생성 (SE24)
```
ZCX_SYUNAME_ERROR (Exception Class로 생성)
```

##### 2.3 구현 클래스 생성 (SE24)
```
ZCL_SYUNAME_SCANNER
ZCL_SYUNAME_TAINT
ZCL_SYUNAME_PARSER
ZCL_SYUNAME_REPORT
ZCL_SYUNAME_FACTORY
```

##### 2.4 프로그램 생성 (SE38)
```
ZSYUNAME_ANALYZER (실행 프로그램)
```

#### 단계 3: 코드 복사
1. 각 파일의 내용을 해당 객체에 복사
2. 문법 체크 (Ctrl+F2)
3. 활성화 (Ctrl+F3)

### 방법 3: Transport Request 사용 🚛

1. 트랜잭션 `SE09` 또는 `SE10` 실행
2. 새 Workbench Request 생성
3. `deploy/transport_request.txt` 파일의 객체 목록 추가
4. Transport 릴리즈 및 이동

## 📖 사용 방법

### 프로그램 실행
```abap
" 트랜잭션 SE38에서 실행
ZSYUNAME_ANALYZER

" 파라미터 입력
- 프로그램 이름: 분석할 프로그램 이름
- 모드: L (로컬)
- 출력 파일: /tmp/results.csv
```

### 결과 확인
결과는 CSV 파일로 저장되며 다음 정보를 포함합니다:
- A열: 순번
- B열: 프로그램/Include 이름
- C열: sy-uname 라인 번호
- D열: 작업 유형 (INSERT/UPDATE/CALL_FUNCTION 등)
- E열: 대상 테이블/RFC 이름
- F열: 필드 이름
- G열 이후: Taint 경로

## 🛠️ 문제 해결

### 자주 발생하는 문제

#### 1. Python이 설치되지 않음
```
해결: Python 3.x 설치
https://www.python.org/downloads/
```

#### 2. 권한 문제
```
해결: SAP 개발 권한 확인
- 개발 클래스 생성 권한
- 프로그램 생성 권한
```

#### 3. Transport 오류
```
해결:
- Transport Request 상태 확인
- 대상 시스템 연결 상태 확인
```

## 📁 프로젝트 구조
```
sy-name/
├── deploy/              # 배포 관련 파일
│   ├── deploy.sh       # Unix/Linux 배포 스크립트
│   ├── deploy.bat      # Windows 배포 스크립트
│   └── deploy_to_sap.py # Python 배포 생성기
├── src/                # 소스 코드
│   ├── abap/          # ABAP 소스 파일
│   │   ├── classes/   # 클래스 구현
│   │   ├── interfaces/# 인터페이스 정의
│   │   ├── programs/  # 실행 프로그램
│   │   └── tests/     # 테스트 코드
├── CLAUDE.md          # 프로젝트 컨텍스트
└── README_KR.md       # 한국어 설명서 (현재 파일)
```

## 🎯 주요 기능

1. **SY-UNAME 추적**: 사용자 ID가 코드에서 어떻게 흐르는지 추적
2. **데이터베이스 영향 분석**: INSERT/UPDATE/DELETE 문에서 사용자 ID 사용 감지
3. **RFC 호출 분석**: 원격 함수 호출에서 사용자 ID 전달 추적
4. **CSV 리포트**: 분석 결과를 Excel에서 열 수 있는 CSV로 출력

## 💡 팁

### 빠른 시작을 위한 체크리스트
- [ ] SAP 개발 시스템 접근 권한 확인
- [ ] Python 3.x 설치 확인
- [ ] 배포 스크립트 실행
- [ ] SAP GUI에서 패키지 생성
- [ ] 생성된 XML 파일 import 또는 수동 복사
- [ ] 테스트 프로그램 실행

### 성능 최적화
- 10,000줄 이하 프로그램: 기본 설정 사용
- 10,000줄 이상 프로그램: 배치 모드 사용 권장
- 매우 큰 프로그램: Include 단위로 분할 분석

## 📞 지원

문제가 발생하면:
1. `validation-report.md` 파일 확인
2. 에러 로그 확인 (트랜잭션 SLG1)
3. GitHub Issues에 문제 보고

## 📄 라이선스

내부 사용 전용 도구입니다.

---

**참고**: 이 도구는 SAP NetWeaver 7.40 이상에서 동작합니다.
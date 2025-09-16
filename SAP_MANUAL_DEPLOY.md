# 📚 SAP 수동 배포 가이드 (스크립트 없이)

## 🎯 이 가이드는 SAP GUI에서 직접 하나씩 객체를 생성하는 방법입니다

---

## 📦 Step 1: 패키지 생성

### 1.1 SE80 트랜잭션 실행
```
SAP 로그인 → 명령창에 SE80 입력 → Enter
```

### 1.2 패키지 생성
1. Repository Browser에서 **"Package"** 선택
2. 입력창에 `Z_SYUNAME_TRACKER` 입력
3. Enter 키 누르기
4. "패키지가 존재하지 않습니다. 생성하시겠습니까?" → **예(Yes)**
5. 패키지 속성 입력:
   - Short Description: `SY-UNAME Tracker Tool`
   - Application Component: 선택 안함
   - Software Component: HOME
6. **저장(Save)** 클릭
7. Transport Request 선택 또는 로컬 객체($TMP)로 저장

---

## 🔧 Step 2: 인터페이스 생성 (4개)

### 2.1 ZIF_SYUNAME_SCANNER 인터페이스
```
1. SE80 → 패키지 Z_SYUNAME_TRACKER 선택
2. 마우스 오른쪽 클릭 → Create → Class Library → Interface
3. 이름: ZIF_SYUNAME_SCANNER
4. 설명: Scanner Interface for SY-UNAME
5. 저장 → 로컬 객체
```

**인터페이스 코드 복사:**
1. 더블클릭으로 인터페이스 열기
2. Change 모드 (Ctrl+F1)
3. 아래 위치에서 코드 복사: `src/abap/interfaces/zif_syuname_scanner.abap`
4. 전체 코드 붙여넣기
5. 저장 (Ctrl+S)
6. 활성화 (Ctrl+F3)

### 2.2 ZIF_SYUNAME_TAINT 인터페이스
```
동일한 방법으로:
- 이름: ZIF_SYUNAME_TAINT
- 설명: Taint Tracking Interface
- 코드: src/abap/interfaces/zif_syuname_taint.abap
```

### 2.3 ZIF_SYUNAME_PARSER 인터페이스
```
동일한 방법으로:
- 이름: ZIF_SYUNAME_PARSER
- 설명: Parser Interface
- 코드: src/abap/interfaces/zif_syuname_parser.abap
```

### 2.4 ZIF_SYUNAME_REPORT 인터페이스
```
동일한 방법으로:
- 이름: ZIF_SYUNAME_REPORT
- 설명: Report Interface
- 코드: src/abap/interfaces/zif_syuname_report.abap
```

---

## ⚠️ Step 3: 예외 클래스 생성

### 3.1 ZCX_SYUNAME_ERROR 생성
```
1. SE80 → 패키지 선택 → 마우스 오른쪽 클릭
2. Create → Class Library → Exception Class
3. 이름: ZCX_SYUNAME_ERROR
4. 설명: Exception Class for SY-UNAME Tracker
5. Superclass: CX_STATIC_CHECK
6. 저장
```

**예외 클래스 코드:**
1. 더블클릭으로 클래스 열기
2. `src/abap/exceptions/zcx_syuname_error.abap` 코드 복사
3. 붙여넣기 → 저장 → 활성화

---

## 🏗️ Step 4: 구현 클래스 생성 (순서 중요!)

### 4.1 ZCL_SYUNAME_SCANNER 클래스
```
1. SE80 → 패키지 선택 → 마우스 오른쪽 클릭
2. Create → Class Library → Class
3. 이름: ZCL_SYUNAME_SCANNER
4. 설명: Scanner Implementation
5. 저장
```

**클래스 구현:**
1. Interfaces 탭 → Add → `ZIF_SYUNAME_SCANNER` 입력
2. Methods 탭으로 이동
3. `src/abap/classes/zcl_syuname_scanner.abap` 코드 복사
4. 각 메소드에 코드 붙여넣기
5. 저장 → 활성화

### 4.2 ZCL_SYUNAME_TAINT 클래스
```
동일한 방법으로:
- 이름: ZCL_SYUNAME_TAINT
- 인터페이스: ZIF_SYUNAME_TAINT
- 코드: src/abap/classes/zcl_syuname_taint.abap
```

### 4.3 ZCL_SYUNAME_PARSER 클래스
```
동일한 방법으로:
- 이름: ZCL_SYUNAME_PARSER
- 인터페이스: ZIF_SYUNAME_PARSER
- 코드: src/abap/classes/zcl_syuname_parser.abap
```

### 4.4 ZCL_SYUNAME_REPORT 클래스
```
동일한 방법으로:
- 이름: ZCL_SYUNAME_REPORT
- 인터페이스: ZIF_SYUNAME_REPORT
- 코드: src/abap/classes/zcl_syuname_report.abap
```

### 4.5 ZCL_SYUNAME_FACTORY 클래스
```
- 이름: ZCL_SYUNAME_FACTORY
- 설명: Factory Class
- 코드: src/abap/classes/zcl_syuname_factory.abap
```

---

## 🎨 Step 5: 추가 클래스 생성

다음 클래스들을 동일한 방법으로 생성:

1. **ZCL_SYUNAME_SCOPE_MANAGER**
   - 코드: `src/zcl_syuname_scope_manager.abap`

2. **ZCL_SYUNAME_FIELDSYMBOL_TRACK**
   - 코드: `src/zcl_syuname_fieldsymbol_track.abap`

3. **ZCL_SYUNAME_ENHANCED_PARSER**
   - 코드: `src/zcl_syuname_enhanced_parser.abap`

4. **ZCL_SYUNAME_REPORT_ENHANCED**
   - 코드: `src/zcl_syuname_report_enhanced.abap`

5. **ZCL_SYUNAME_MOVE_CORR_HANDLER**
   - 코드: `src/zcl_syuname_move_corr_handler.abap`

6. **ZCL_SYUNAME_PERFORMANCE_OPT**
   - 코드: `src/zcl_syuname_performance_opt.abap`

---

## 📝 Step 6: 메인 프로그램 생성

### 6.1 ZSYUNAME_ANALYZER 프로그램
```
1. SE38 트랜잭션 실행
2. Program: ZSYUNAME_ANALYZER
3. Create 버튼 클릭
4. Title: SY-UNAME Analyzer Main Program
5. Type: Executable Program
6. 저장
```

**프로그램 코드:**
1. `src/abap/programs/zsyuname_analyzer.abap` 파일 열기
2. 전체 코드 복사
3. SE38에 붙여넣기
4. 저장 (Ctrl+S)
5. 문법 체크 (Ctrl+F2)
6. 활성화 (Ctrl+F3)

### 6.2 ZSYUNAME_ANLZ 프로그램 (선택사항)
```
동일한 방법으로:
- 이름: ZSYUNAME_ANLZ
- 코드: ZSYUNAME_ANLZ.abap
```

---

## ✅ Step 7: 테스트 프로그램 생성

### 7.1 통합 테스트 프로그램
```
1. SE38 → Program: ZSYUNAME_TEST_INTEGRATION
2. Create → Type: Executable Program
3. 코드: src/abap/tests/zsyuname_test_integration.abap
4. 저장 → 활성화
```

### 7.2 검증 테스트 클래스
```
1. SE80 → Class: ZCL_SYUNAME_TEST_VALIDATION
2. 코드: src/zcl_syuname_test_validation.abap
3. 저장 → 활성화
```

---

## 🔍 Step 8: 활성화 확인

### 8.1 전체 객체 활성화 확인
```
1. SE80 → 패키지 Z_SYUNAME_TRACKER 선택
2. 모든 객체가 활성화(Active) 상태인지 확인
3. 비활성 객체가 있으면 → 마우스 오른쪽 클릭 → Activate
```

### 8.2 문법 체크
```
각 객체별로:
1. 열기
2. Ctrl+F2 (문법 체크)
3. 오류가 있으면 수정
4. Ctrl+F3 (활성화)
```

---

## 🚀 Step 9: 프로그램 실행

### 9.1 첫 실행
```
1. SE38 트랜잭션
2. Program: ZSYUNAME_ANALYZER
3. F8 (실행)
```

### 9.2 파라미터 입력
```
- Program Name: 분석할 프로그램 이름
- Analysis Mode: L (로컬)
- Output File: /tmp/results.csv
- Debug Mode: [ ] (체크 안함)
```

### 9.3 실행 및 결과 확인
```
1. F8 키로 실행
2. 결과 메시지 확인
3. AL11 트랜잭션에서 결과 파일 확인
```

---

## 🔧 문제 해결

### 오류 1: 인터페이스를 찾을 수 없음
```
해결: 인터페이스를 먼저 생성하고 활성화
```

### 오류 2: 클래스 활성화 실패
```
해결:
1. 인터페이스가 모두 활성화되었는지 확인
2. 예외 클래스가 활성화되었는지 확인
3. 순서대로 다시 활성화
```

### 오류 3: 권한 오류
```
해결:
1. 개발 권한 확인
2. 패키지 생성 권한 확인
3. BASIS 팀에 권한 요청
```

---

## 📌 체크리스트

생성 순서를 꼭 지켜주세요:

- [ ] 1. 패키지 생성 (Z_SYUNAME_TRACKER)
- [ ] 2. 인터페이스 4개 생성 및 활성화
- [ ] 3. 예외 클래스 생성 및 활성화
- [ ] 4. 구현 클래스 생성 및 활성화
- [ ] 5. 추가 클래스 생성 및 활성화
- [ ] 6. 메인 프로그램 생성 및 활성화
- [ ] 7. 테스트 프로그램 생성
- [ ] 8. 전체 활성화 확인
- [ ] 9. 프로그램 실행 테스트

---

## 💡 팁

1. **코드 복사 시**: 들여쓰기가 깨지면 Pretty Printer (Shift+F1) 사용
2. **활성화 오류 시**: 종속성 순서대로 활성화 (인터페이스 → 예외 → 클래스 → 프로그램)
3. **저장 위치**: 로컬 객체($TMP) 또는 Transport Request 선택

---

**작성일**: 2024년
**버전**: 1.0
**지원**: SAP NetWeaver 7.40+
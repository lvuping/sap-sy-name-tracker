# 🚀 초간단 배포 가이드 (Super Simple Deployment Guide)

## 1️⃣ 원클릭 배포 (One-Click Deploy)

### Windows 사용자
1. `deploy` 폴더 열기
2. `deploy.bat` 더블클릭
3. 생성된 파일을 SAP에 업로드

### Mac/Linux 사용자
1. 터미널에서:
```bash
cd sy-name
./deploy/deploy.sh
```

## 2️⃣ SAP 업로드 (Upload to SAP)

### 옵션 A: SAPLink 사용 (쉬움)
1. SAP GUI 로그인
2. 트랜잭션: `ZSAPLINK` 실행
3. Import Nugget 선택
4. `deploy/output/syuname_tracker_*.xml` 파일 선택
5. Import 클릭

### 옵션 B: 수동 복사 (단순함)
1. SAP GUI 로그인
2. 트랜잭션: `SE80` 실행
3. 각 `.abap` 파일 내용을 복사하여 붙여넣기

## 3️⃣ 실행 (Run)

```
트랜잭션: SE38
프로그램: ZSYUNAME_ANALYZER
실행 (F8)
```

## ❓ 문제 해결

| 문제 | 해결 |
|------|------|
| Python 없음 | [Python 다운로드](https://www.python.org) |
| 권한 없음 | SAP 관리자에게 개발 권한 요청 |
| SAPLink 없음 | 수동 복사 방법 사용 |

## 📝 체크리스트

- [ ] Python 설치됨
- [ ] SAP 개발 권한 있음
- [ ] 배포 스크립트 실행함
- [ ] SAP에 업로드함
- [ ] 테스트 완료

끝! 🎉
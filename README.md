# Cobol-like-Haskell  
COBOL 문법 감성을 Haskell의 함수형 의미 모델 위에서 재구현하는 언어 실험 프로젝트

---

## 📌 개요
**Cobol-like-Haskell**은 COBOL 특유의 서술적·절차적 구조를  
Haskell의 **타입 안정성**, **순수 함수형 모델**, **의미 기반 실행(meaning-level semantics)** 위에서 재해석하는 실험적 언어 리포지토리다.

Freeing-the-Lang 생태계의 하나로, ‘언어를 언어로 만드는 실험실’ 컨셉 속에서  
COBOL → Haskell 스타일의 하이브리드 언어 모델을 구축하는 것이 핵심이다.

---

## 🎯 목표
### 1. COBOL 구문 파서(Parser)
다음과 같은 COBOL 스타일 입력을:




IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE.


PROCEDURE DIVISION.
DISPLAY "HELLO WORLD".
STOP RUN.



아래와 같은 Haskell AST 구조로 변환하는 것이 목표:

```haskell
Program
  { identification = "SAMPLE"
  , body =
      [ Display "HELLO WORLD"
      , Stop
      ]
  }




🧠 의미 모델 (Haskell 기반)




COBOL의 순차적 실행 흐름을 유지하되

→ 내부 의미 처리는 Haskell의 함수형 방식으로 변환


사이드 이펙트는 IO 모나드로 안전하게 처리


Division / Section / Paragraph 구조를

재귀적 AST + 의미 트리로 구성





🏗️ 향후 구현 로드맵


✔ 1단계 — 기본 파서




IDENTIFICATION / PROCEDURE DIVISION 지원


DISPLAY / MOVE / STOP RUN 등 주요 명령어 우선 지원




✔ 2단계 — 의미 기반 실행기




AST → Haskell 의사 코드 변환


“절차형 → 함수형” 패턴 정립


간단한 해석기(interpreter) 제공




✔ 3단계 — 고급 기능




변수 테이블


SECTION / PARAGRAPH 점프 처리


조건문(IF) / 반복(PERFORM) 추가


오류 감지 및 타입 검증





🔧 기술 스택




Haskell (GHC 9.x+)


Cabal 프로젝트 구조


Freeing-the-Lang 생태계에서 사용하는

Meaning-Level Engine, AST Framework 일부 공유 예정





🧪 예제 (미니 DSL)


향후 목표 예제:


IDENTIFICATION DIVISION.
PROGRAM-ID. ADDER.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 A PIC 9 VALUE 2.
01 B PIC 9 VALUE 3.

PROCEDURE DIVISION.
DISPLAY A + B.
STOP RUN.



→ 변환 후 Haskell:


main :: IO ()
main = do
  let a = 2
      b = 3
  print (a + b)




📂 리포 구조 (예정)


/
├── src/
│   ├── Lexer.hs
│   ├── Parser.hs
│   ├── AST.hs
│   ├── Evaluator.hs
│   └── CodeGen.hs
├── example/
│   └── sample.cobhs
├── README.md
└── LICENSE




🔄 앞으로 들어갈 기능




Multi-OS 3OS Build (Ubuntu / macOS / Windows)


ProofLedger 자동 생성


artifact 출력 (AST, 중간 코드, Haskell 변환본)


Grammar 문서 자동 생성





📜 라이선스


MIT License



🤝 기여


언어 실험 생태계에 관심 있다면 누구든지 기여 가능.

Parser/AST/CodeGen 개선 제안 환영.



---


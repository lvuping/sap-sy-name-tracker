# Implementation Plan: SAP ABAP SY-UNAME Tracker


**Branch**: `001-sap-abap-sy` | **Date**: 2025-09-15 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-sap-abap-sy/spec.md`

## Execution Flow (/plan command scope)
```
1. Load feature spec from Input path
   → If not found: ERROR "No feature spec at {path}"
2. Fill Technical Context (scan for NEEDS CLARIFICATION)
   → Detect Project Type from context (web=frontend+backend, mobile=app+api)
   → Set Structure Decision based on project type
3. Evaluate Constitution Check section below
   → If violations exist: Document in Complexity Tracking
   → If no justification possible: ERROR "Simplify approach first"
   → Update Progress Tracking: Initial Constitution Check
4. Execute Phase 0 → research.md
   → If NEEDS CLARIFICATION remain: ERROR "Resolve unknowns"
5. Execute Phase 1 → contracts, data-model.md, quickstart.md, agent-specific template file (e.g., `CLAUDE.md` for Claude Code, `.github/copilot-instructions.md` for GitHub Copilot, or `GEMINI.md` for Gemini CLI).
6. Re-evaluate Constitution Check section
   → If new violations: Refactor design, return to Phase 1
   → Update Progress Tracking: Post-Design Constitution Check
7. Plan Phase 2 → Describe task generation approach (DO NOT create tasks.md)
8. STOP - Ready for /tasks command
```

**IMPORTANT**: The /plan command STOPS at step 7. Phases 2-4 are executed by other commands:
- Phase 2: /tasks command creates tasks.md
- Phase 3-4: Implementation execution (manual or via tools)

## Summary
A static analysis tool for SAP ABAP code that tracks the data flow of sy-uname (user information) through assignments, structures, internal tables, and ultimately to database operations (INSERT/UPDATE/MODIFY/DELETE on Z*/Y* tables), RFC calls, and method invocations. The tool provides taint analysis with confidence levels and complete path tracking from source to sink.

## Technical Context
**Language/Version**: ABAP (SAP NetWeaver 7.40+)
**Primary Dependencies**: SAP kernel scanner (SCAN ABAP-SOURCE), READ REPORT API
**Storage**: CSV output files for analysis results
**Testing**: ABAP Unit Test Framework
**Target Platform**: SAP NetWeaver Application Server (on-premise)
**Project Type**: single - ABAP report/program with CLI interface
**Performance Goals**: Process 10,000+ lines of ABAP code per minute
**Constraints**: Must run within SAP system, handle recursive includes, support both LOCAL_ONLY and FOLLOW_CALLS modes
**Scale/Scope**: Analyze programs up to 100,000 lines with nested includes, track 1000+ taint paths

## Constitution Check
*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Simplicity**:
- Projects: 1 (single ABAP program with CLI interface)
- Using framework directly? YES (native ABAP, no wrappers)
- Single data model? YES (taint tracking state model)
- Avoiding patterns? YES (direct implementation, no unnecessary abstraction)

**Architecture**:
- EVERY feature as library? YES (modular ABAP includes)
- Libraries listed:
  - ZCL_SYUNAME_SCANNER: Core scanning and tokenization
  - ZCL_SYUNAME_TAINT: Taint tracking engine
  - ZCL_SYUNAME_PARSER: Statement parsing (DML/RFC/PERFORM)
  - ZCL_SYUNAME_REPORT: Result formatting and CSV output
- CLI per library: REPORT ZSYUNAME_ANALYZER with parameters
- Library docs: Technical documentation in ABAP comments + external MD

**Testing (NON-NEGOTIABLE)**:
- RED-GREEN-Refactor cycle enforced? YES
- Git commits show tests before implementation? YES
- Order: Contract→Integration→E2E→Unit strictly followed? YES
- Real dependencies used? YES (actual SAP system, real ABAP programs)
- Integration tests for: new libraries, contract changes, shared schemas? YES
- FORBIDDEN: Implementation before test, skipping RED phase ✓

**Observability**:
- Structured logging included? YES (SAP Application Log - SLG1)
- Frontend logs → backend? N/A (single-tier ABAP program)
- Error context sufficient? YES (line numbers, statement context, taint path)

**Versioning**:
- Version number assigned? 1.0.0
- BUILD increments on every change? YES
- Breaking changes handled? YES (transport management)

## Project Structure

### Documentation (this feature)
```
specs/[###-feature]/
├── plan.md              # This file (/plan command output)
├── research.md          # Phase 0 output (/plan command)
├── data-model.md        # Phase 1 output (/plan command)
├── quickstart.md        # Phase 1 output (/plan command)
├── contracts/           # Phase 1 output (/plan command)
└── tasks.md             # Phase 2 output (/tasks command - NOT created by /plan)
```

### Source Code (repository root)
```
# Option 1: Single project (DEFAULT)
src/
├── models/
├── services/
├── cli/
└── lib/

tests/
├── contract/
├── integration/
└── unit/

# Option 2: Web application (when "frontend" + "backend" detected)
backend/
├── src/
│   ├── models/
│   ├── services/
│   └── api/
└── tests/

frontend/
├── src/
│   ├── components/
│   ├── pages/
│   └── services/
└── tests/

# Option 3: Mobile + API (when "iOS/Android" detected)
api/
└── [same as backend above]

ios/ or android/
└── [platform-specific structure]
```

**Structure Decision**: Option 1 (Single project) - ABAP program with modular includes

## Phase 0: Outline & Research
1. **Extract unknowns from Technical Context** above:
   - For each NEEDS CLARIFICATION → research task
   - For each dependency → best practices task
   - For each integration → patterns task

2. **Generate and dispatch research agents**:
   ```
   For each unknown in Technical Context:
     Task: "Research {unknown} for {feature context}"
   For each technology choice:
     Task: "Find best practices for {tech} in {domain}"
   ```

3. **Consolidate findings** in `research.md` using format:
   - Decision: [what was chosen]
   - Rationale: [why chosen]
   - Alternatives considered: [what else evaluated]

**Output**: research.md with all NEEDS CLARIFICATION resolved

## Phase 1: Design & Contracts
*Prerequisites: research.md complete*

1. **Extract entities from feature spec** → `data-model.md`:
   - Entity name, fields, relationships
   - Validation rules from requirements
   - State transitions if applicable

2. **Generate API contracts** from functional requirements:
   - For each user action → endpoint
   - Use standard REST/GraphQL patterns
   - Output OpenAPI/GraphQL schema to `/contracts/`

3. **Generate contract tests** from contracts:
   - One test file per endpoint
   - Assert request/response schemas
   - Tests must fail (no implementation yet)

4. **Extract test scenarios** from user stories:
   - Each story → integration test scenario
   - Quickstart test = story validation steps

5. **Update agent file incrementally** (O(1) operation):
   - Run `/scripts/bash/update-agent-context.sh claude` for your AI assistant
   - If exists: Add only NEW tech from current plan
   - Preserve manual additions between markers
   - Update recent changes (keep last 3)
   - Keep under 150 lines for token efficiency
   - Output to repository root

**Output**: data-model.md, /contracts/*, failing tests, quickstart.md, agent-specific file

## Phase 2: Task Planning Approach
*This section describes what the /tasks command will do - DO NOT execute during /plan*

**Task Generation Strategy**:
- Load `/templates/tasks-template.md` as base
- Generate tasks from Phase 1 design docs (contracts, data model, quickstart)
- Each ABAP interface → interface definition task
- Each ABAP class → test class creation task (TDD)
- Each ABAP class → implementation task
- Main program implementation
- Integration test scenarios from quickstart
- Performance validation tasks

**Ordering Strategy**:
- TDD order: Tests before implementation
- Dependency order:
  1. Exception class first
  2. Interfaces next
  3. Test classes (failing)
  4. Implementation classes
  5. Main program
  6. Integration tests
- Mark [P] for parallel execution where possible

**Estimated Output**: 20-25 numbered, ordered tasks in tasks.md

**Task Categories**:
1. Foundation (exceptions, interfaces) - Tasks 1-5
2. Test Creation (TDD red phase) - Tasks 6-10
3. Core Implementation - Tasks 11-15
4. Main Program - Tasks 16-17
5. Integration & Validation - Tasks 18-20
6. Documentation - Tasks 21-22

**IMPORTANT**: This phase is executed by the /tasks command, NOT by /plan

## Phase 3+: Future Implementation
*These phases are beyond the scope of the /plan command*

**Phase 3**: Task execution (/tasks command creates tasks.md)  
**Phase 4**: Implementation (execute tasks.md following constitutional principles)  
**Phase 5**: Validation (run tests, execute quickstart.md, performance validation)

## Complexity Tracking
*Fill ONLY if Constitution Check has violations that must be justified*

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |


## Progress Tracking
*This checklist is updated during execution flow*

**Phase Status**:
- [x] Phase 0: Research complete (/plan command)
- [x] Phase 1: Design complete (/plan command)
- [x] Phase 2: Task planning complete (/plan command - describe approach only)
- [ ] Phase 3: Tasks generated (/tasks command)
- [ ] Phase 4: Implementation complete
- [ ] Phase 5: Validation passed

**Gate Status**:
- [x] Initial Constitution Check: PASS
- [x] Post-Design Constitution Check: PASS
- [x] All NEEDS CLARIFICATION resolved
- [x] Complexity deviations documented (none required)

---
*Based on Constitution v2.1.1 - See `/memory/constitution.md`*
# Tasks: SAP ABAP SY-UNAME Tracker

**Input**: Design documents from `/specs/001-sap-abap-sy/`
**Prerequisites**: plan.md (required), research.md, data-model.md, contracts/

## Execution Flow (main)
```
1. Load plan.md from feature directory
   → Extract: ABAP classes, interfaces, main program
2. Load design documents:
   → data-model.md: TaintedVariable, Finding, AnalysisScope entities
   → contracts/: CLI interface, ABAP API specifications
   → research.md: Technical decisions on parsing approach
3. Generate tasks by category:
   → Foundation: Exception class, interfaces
   → Tests: Unit test classes (TDD red phase)
   → Core: Implementation classes
   → Main: Program and selection screen
   → Integration: End-to-end test scenarios
4. Apply TDD rules:
   → Test classes before implementation
   → Tests must fail first
5. Number tasks sequentially (T001, T002...)
6. Return: SUCCESS (tasks ready for ABAP development)
```

## Format: `[ID] [P?] Description`
- **[P]**: Can run in parallel (different ABAP objects)
- Include exact ABAP object names in descriptions

## Path Conventions
- All ABAP objects created in SE80/SE38
- Test classes embedded in main classes (ABAP Unit)
- Exception classes in package Z_SYUNAME

## Phase 3.1: Foundation Objects
- [x] T001 Create exception class ZCX_SYUNAME_ERROR with message constants for program_not_found, file_write_error, parse_error
- [x] T002 [P] Create interface ZIF_SYUNAME_SCANNER with methods read_program and get_includes
- [x] T003 [P] Create interface ZIF_SYUNAME_TAINT with methods mark_tainted, is_tainted, get_taint_path, propagate_taint
- [x] T004 [P] Create interface ZIF_SYUNAME_PARSER with methods parse_dml_statement, parse_call_statement, parse_assignment
- [x] T005 [P] Create interface ZIF_SYUNAME_REPORT with methods add_finding, generate_csv, get_statistics

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These test classes MUST be written and MUST FAIL before ANY implementation**
- [x] T006 Create test class LTC_SCANNER in ZCL_SYUNAME_SCANNER with failing tests for read_program and include detection
- [x] T007 Create test class LTC_TAINT in ZCL_SYUNAME_TAINT with failing tests for taint marking and propagation
- [x] T008 Create test class LTC_PARSER in ZCL_SYUNAME_PARSER with failing tests for DML/RFC/assignment parsing
- [x] T009 Create test class LTC_REPORT in ZCL_SYUNAME_REPORT with failing tests for CSV generation
- [x] T010 Create integration test program ZSYUNAME_TEST_INTEGRATION with test scenarios from quickstart

## Phase 3.3: Core Implementation (ONLY after tests are failing)
- [x] T011 Implement ZCL_SYUNAME_SCANNER class with READ REPORT and include resolution logic
- [x] T012 Implement ZCL_SYUNAME_TAINT class with tainted variable registry and scope management
- [x] T013 Implement ZCL_SYUNAME_PARSER class with SCAN ABAP-SOURCE tokenization and statement parsing
- [x] T014 Implement ZCL_SYUNAME_REPORT class with CSV formatting and application log writing
- [x] T015 Create factory class ZCL_SYUNAME_FACTORY with creation methods for all components

## Phase 3.4: Main Program
- [x] T016 Create REPORT ZSYUNAME_ANALYZER with selection screen parameters (p_prog, p_mode, p_output, p_debug)
- [x] T017 Implement main analysis logic orchestrating scanner, taint engine, parser, and reporter
- [x] T018 Add progress indication for large programs (>5000 lines)
- [x] T019 Implement debug mode with detailed tracing to application log

## Phase 3.5: Integration & Validation
- [x] T020 Test Scenario 1: Direct assignment (sy-uname → variable → INSERT)
- [x] T021 Test Scenario 2: Structure transfer (sy-uname → structure-field → INSERT FROM structure)
- [x] T022 Test Scenario 3: RFC call with tainted parameters
- [x] T023 Test performance with 10,000 line program (target: <60 seconds)
- [x] T024 Test edge cases: field-symbols, MOVE-CORRESPONDING, dynamic table names

## Phase 3.6: Polish & Documentation
- [x] T025 Add comprehensive ABAP documentation comments to all public methods
- [x] T026 Create transport request with all objects for deployment
- [x] T027 Validate CSV output format matches specification
- [x] T028 Run full test suite and ensure 100% pass rate

## Phase 3.7: Advanced Features & Optimization
- [x] T029 [P] Implement scope stack management for nested FORM/METHOD/FUNCTION boundaries
- [x] T030 [P] Add support for MOVE-CORRESPONDING taint propagation in ZCL_SYUNAME_TAINT
- [x] T031 [P] Implement field-symbol taint tracking with MEDIUM confidence level
- [x] T032 Optimize internal table access using SORTED/HASHED tables for performance
- [x] T033 Add caching for frequently accessed include files in ZCL_SYUNAME_SCANNER
- [x] T034 Implement circular include detection to prevent infinite loops
- [x] T035 Add memory management for programs >100,000 lines

## Phase 3.8: Enhanced Parsing Capabilities
- [x] T036 Parse and track taint through CONCATENATE statements
- [x] T037 Handle taint in LOOP AT statements with field assignment
- [x] T038 Track taint through CALL METHOD with IMPORTING/EXPORTING parameters
- [x] T039 Parse PERFORM statements and track parameter tainting
- [x] T040 Detect and handle dynamic SQL (EXEC SQL) statements

## Phase 3.9: Reporting Enhancements
- [x] T041 [P] Add JSON output format option to ZCL_SYUNAME_REPORT
- [x] T042 [P] Implement detailed statistics with confidence level breakdown
- [x] T043 Create summary report with top impacted tables
- [x] T044 Add HTML report generation for web viewing
- [x] T045 Implement batch mode for analyzing multiple programs

## Phase 3.10: Final Testing & Validation
- [x] T046 Run analysis on real SAP standard programs (with permission)
- [x] T047 Validate all quickstart.md test scenarios pass
- [x] T048 Performance benchmark: Process 100,000 lines within 10 minutes
- [x] T049 Memory test: Ensure <2GB usage for large programs
- [x] T050 Create demo video showing tool usage and results

## Dependencies
- Foundation objects (T001-T005) must complete first
- Tests (T006-T010) before implementation (T011-T015)
- T011-T014 before T015 (factory needs implementations)
- T015 before T016-T017 (main program needs factory)
- All implementation before integration tests (T020-T024)

## Parallel Execution Example
```
# Launch T002-T005 together (independent interfaces):
Task: "Create interface ZIF_SYUNAME_SCANNER"
Task: "Create interface ZIF_SYUNAME_TAINT"
Task: "Create interface ZIF_SYUNAME_PARSER"
Task: "Create interface ZIF_SYUNAME_REPORT"
```

## ABAP Code Generation Examples

### T001: Exception Class ZCX_SYUNAME_ERROR
```abap
CLASS zcx_syuname_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF program_not_found,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'PROGRAM_NAME',
      END OF program_not_found,

      BEGIN OF file_write_error,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'FILE_PATH',
      END OF file_write_error,

      BEGIN OF parse_error,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'STATEMENT',
      END OF parse_error.

    DATA: program_name TYPE programm,
          file_path    TYPE string,
          statement    TYPE string.

    METHODS: constructor
      IMPORTING
        textid       LIKE if_t100_message=>t100key OPTIONAL
        previous     LIKE previous OPTIONAL
        program_name TYPE programm OPTIONAL
        file_path    TYPE string OPTIONAL
        statement    TYPE string OPTIONAL.
ENDCLASS.

CLASS zcx_syuname_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    me->program_name = program_name.
    me->file_path = file_path.
    me->statement = statement.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### T011: Scanner Implementation (excerpt)
```abap
CLASS zcl_syuname_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_scanner.

  PRIVATE SECTION.
    DATA: mt_source_cache TYPE TABLE OF string,
          mt_includes     TYPE TABLE OF programm.

    METHODS: resolve_includes
      IMPORTING
        iv_program TYPE programm
      RAISING
        zcx_syuname_error.
ENDCLASS.

CLASS zcl_syuname_scanner IMPLEMENTATION.
  METHOD zif_syuname_scanner~read_program.
    DATA: lt_source TYPE TABLE OF string.

    " Read the ABAP source code
    READ REPORT iv_program_name INTO lt_source.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>program_not_found
          program_name = iv_program_name.
    ENDIF.

    " Convert to structured format
    LOOP AT lt_source INTO DATA(lv_line).
      APPEND VALUE #( line_number = sy-tabix
                      statement = lv_line ) TO rt_source.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_scanner~get_includes.
    CLEAR rt_includes.

    " Use SCAN ABAP-SOURCE to find INCLUDE statements
    DATA: lt_tokens TYPE stokesx_tab,
          lt_statements TYPE sstmnt_tab.

    SCAN ABAP-SOURCE mt_source_cache
         TOKENS INTO lt_tokens
         STATEMENTS INTO lt_statements
         WITH ANALYSIS.

    LOOP AT lt_statements INTO DATA(ls_statement)
      WHERE type = 'I'.  " Include statement

      READ TABLE lt_tokens INTO DATA(ls_token)
        INDEX ls_statement-from + 1.
      IF sy-subrc = 0.
        APPEND ls_token-str TO rt_includes.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

### T012: Taint Engine Implementation (excerpt)
```abap
CLASS zcl_syuname_taint DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_taint.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_tainted_internal,
             name       TYPE string,
             scope      TYPE string,
             scope_name TYPE string,
             line       TYPE i,
             path       TYPE string_table,
             confidence TYPE string,
           END OF ty_tainted_internal.

    DATA: mt_tainted_vars TYPE TABLE OF ty_tainted_internal,
          mt_scope_stack  TYPE TABLE OF string.
ENDCLASS.

CLASS zcl_syuname_taint IMPLEMENTATION.
  METHOD zif_syuname_taint~mark_tainted.
    " Add variable to tainted registry
    APPEND VALUE #( name = iv_variable
                    scope = iv_scope
                    scope_name = iv_scope
                    line = iv_line
                    path = it_path
                    confidence = 'HIGH' ) TO mt_tainted_vars.
  ENDMETHOD.

  METHOD zif_syuname_taint~is_tainted.
    " Check if variable is in tainted registry
    READ TABLE mt_tainted_vars TRANSPORTING NO FIELDS
      WITH KEY name = iv_variable
               scope = iv_scope.
    rv_tainted = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD zif_syuname_taint~propagate_taint.
    DATA: lt_new_path TYPE string_table.

    " Get existing path from source variable
    READ TABLE mt_tainted_vars INTO DATA(ls_source)
      WITH KEY name = iv_from_var
               scope = iv_scope.

    IF sy-subrc = 0.
      lt_new_path = ls_source-path.
      APPEND |{ iv_from_var } -> { iv_to_var }| TO lt_new_path.

      " Mark target variable as tainted
      me->zif_syuname_taint~mark_tainted(
        iv_variable = iv_to_var
        iv_scope = iv_scope
        iv_line = iv_line
        it_path = lt_new_path ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### T016: Main Program ZSYUNAME_ANALYZER
```abap
REPORT zsyuname_analyzer.

" Selection screen
PARAMETERS: p_prog   TYPE programm OBLIGATORY,
            p_mode   TYPE char1 DEFAULT 'L',
            p_output TYPE string DEFAULT '/tmp/syuname_results.csv',
            p_debug  TYPE abap_bool DEFAULT abap_false.

SELECT-OPTIONS: s_excl FOR sy-repid NO INTERVALS.

" Main processing
START-OF-SELECTION.
  DATA: lo_scanner  TYPE REF TO zif_syuname_scanner,
        lo_taint    TYPE REF TO zif_syuname_taint,
        lo_parser   TYPE REF TO zif_syuname_parser,
        lo_reporter TYPE REF TO zif_syuname_report,
        lv_findings TYPE i.

  TRY.
      " Create components via factory
      lo_scanner = zcl_syuname_factory=>create_scanner( ).
      lo_taint = zcl_syuname_factory=>create_taint_engine( ).
      lo_parser = zcl_syuname_factory=>create_parser( ).
      lo_reporter = zcl_syuname_factory=>create_reporter( ).

      " Log start
      MESSAGE i001(zsyuname) WITH p_prog.

      " Read program source
      DATA(lt_source) = lo_scanner->read_program( p_prog ).

      " Get includes if not LOCAL_ONLY
      IF p_mode = 'F'.
        DATA(lt_includes) = lo_scanner->get_includes( p_prog ).
        MESSAGE i002(zsyuname) WITH lines( lt_includes ).
      ENDIF.

      " Process source code
      LOOP AT lt_source INTO DATA(ls_line).
        " Check for sy-uname
        IF ls_line-statement CS 'sy-uname' OR
           ls_line-statement CS 'SY-UNAME'.

          " Mark initial taint
          lo_taint->mark_tainted(
            iv_variable = 'sy-uname'
            iv_scope = 'GLOBAL'
            iv_line = ls_line-line_number
            it_path = VALUE #( ( |sy-uname| ) ) ).
        ENDIF.

        " Parse for assignments, DML, calls
        " ... (parsing logic)
      ENDLOOP.

      " Generate report
      lo_reporter->generate_csv( p_output ).

      " Get statistics
      DATA(ls_stats) = lo_reporter->get_statistics( ).
      MESSAGE i003(zsyuname) WITH ls_stats-findings_total p_output.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
```

## Implementation Examples for Remaining Phases

### T029: Scope Stack Management
```abap
METHOD enter_scope.
  " Push new scope onto stack
  APPEND VALUE #( type = iv_scope_type
                  name = iv_scope_name
                  start_line = sy-index ) TO mt_scope_stack.

  " Clean tainted vars from parent scopes
  LOOP AT mt_tainted_vars INTO DATA(ls_var)
    WHERE scope <> iv_scope_name.
    ls_var-is_active = abap_false.
    MODIFY mt_tainted_vars FROM ls_var.
  ENDLOOP.
ENDMETHOD.
```

### T030: MOVE-CORRESPONDING Support
```abap
METHOD handle_move_corresponding.
  " Track all fields in source structure
  DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr.

  lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( source_struct ).

  LOOP AT lo_struct_descr->components INTO DATA(ls_comp).
    IF is_tainted( |{ source_struct }-{ ls_comp-name }| ).
      mark_tainted( |{ target_struct }-{ ls_comp-name }| ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.
```

### T031: Field-Symbol Tracking
```abap
METHOD track_field_symbol.
  " Reduce confidence for field-symbol assignments
  IF iv_statement CS 'FIELD-SYMBOL'.
    lv_confidence = 'MEDIUM'.

    " Extract field-symbol name
    DATA(lv_fs_name) = extract_field_symbol( iv_statement ).

    " Mark with reduced confidence
    mark_tainted( iv_variable = lv_fs_name
                  iv_confidence = 'MEDIUM'
                  iv_note = 'Field-symbol assignment' ).
  ENDIF.
ENDMETHOD.
```

### T036: CONCATENATE Parsing
```abap
METHOD parse_concatenate.
  " Check if any source is tainted
  DATA: lv_any_tainted TYPE abap_bool.

  LOOP AT it_source_vars INTO DATA(lv_var).
    IF is_tainted( lv_var ).
      lv_any_tainted = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  " If any source tainted, result is tainted
  IF lv_any_tainted = abap_true.
    propagate_taint( iv_to_var = iv_target_var
                     iv_confidence = 'MEDIUM' ).
  ENDIF.
ENDMETHOD.
```

### T041: JSON Output Format
```abap
METHOD generate_json.
  DATA: lo_json TYPE REF TO /ui2/cl_json.

  " Convert findings to JSON
  DATA(lv_json) = /ui2/cl_json=>serialize(
    data = mt_findings
    pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  " Write to file
  OPEN DATASET iv_output_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  TRANSFER lv_json TO iv_output_path.
  CLOSE DATASET iv_output_path.
ENDMETHOD.
```

## Performance Optimization Guidelines

### T032: Internal Table Optimization
- Use SORTED tables for mt_tainted_vars with key (name, scope)
- Use HASHED tables for mt_includes with key program_name
- Binary search for READ TABLE operations

### T033: Include Caching
- Implement LRU cache for top 100 includes
- Cache invalidation on transport activation
- Memory limit: 100MB for cache

### T035: Memory Management
- Implement sliding window for large programs
- Process in 10,000 line chunks
- Clear processed data after finding extraction

## Notes
- All ABAP objects use Z* namespace
- Test classes use LTC_ prefix (Local Test Class)
- Follow SAP naming conventions
- Commit transport after each phase
- Run ABAP Unit tests via SE80
- Performance target: 10,000+ lines/minute
- Memory limit: 2GB max usage

## Validation Checklist
- [x] All interfaces defined before implementation
- [x] All classes have test classes (TDD)
- [x] Exception class created first
- [x] Main program uses factory pattern
- [x] CSV output format specified
- [x] Application log integration included
- [x] Phase 3.7-3.10 tasks ready for implementation
- [x] Performance optimization implemented
- [x] Memory management tested
- [x] All test scenarios validated
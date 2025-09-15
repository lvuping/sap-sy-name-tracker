# ABAP API Contract: ZCL_SYUNAME Classes

## Public Interfaces

### ZIF_SYUNAME_SCANNER
```abap
INTERFACE zif_syuname_scanner PUBLIC.
  TYPES: BEGIN OF ty_source_line,
           line_number TYPE i,
           statement   TYPE string,
         END OF ty_source_line.

  TYPES: ty_source_lines TYPE TABLE OF ty_source_line.

  METHODS:
    read_program
      IMPORTING
        iv_program_name TYPE programm
      RETURNING
        VALUE(rt_source) TYPE ty_source_lines
      RAISING
        zcx_syuname_error,

    get_includes
      IMPORTING
        iv_program_name TYPE programm
      RETURNING
        VALUE(rt_includes) TYPE string_table
      RAISING
        zcx_syuname_error.
ENDINTERFACE.
```

### ZIF_SYUNAME_TAINT
```abap
INTERFACE zif_syuname_taint PUBLIC.
  TYPES: BEGIN OF ty_tainted_var,
           name       TYPE string,
           scope      TYPE string,
           scope_name TYPE string,
           line       TYPE i,
           path       TYPE string_table,
           confidence TYPE string,
         END OF ty_tainted_var.

  TYPES: ty_tainted_vars TYPE TABLE OF ty_tainted_var.

  METHODS:
    mark_tainted
      IMPORTING
        iv_variable TYPE string
        iv_scope    TYPE string
        iv_line     TYPE i
        it_path     TYPE string_table,

    is_tainted
      IMPORTING
        iv_variable TYPE string
        iv_scope    TYPE string
      RETURNING
        VALUE(rv_tainted) TYPE abap_bool,

    get_taint_path
      IMPORTING
        iv_variable TYPE string
        iv_scope    TYPE string
      RETURNING
        VALUE(rt_path) TYPE string_table,

    propagate_taint
      IMPORTING
        iv_from_var TYPE string
        iv_to_var   TYPE string
        iv_scope    TYPE string
        iv_line     TYPE i,

    enter_scope
      IMPORTING
        iv_scope_type TYPE string
        iv_scope_name TYPE string,

    exit_scope,

    get_all_tainted
      RETURNING
        VALUE(rt_vars) TYPE ty_tainted_vars.
ENDINTERFACE.
```

### ZIF_SYUNAME_PARSER
```abap
INTERFACE zif_syuname_parser PUBLIC.
  TYPES: BEGIN OF ty_dml_info,
           operation  TYPE string,    " INSERT/UPDATE/MODIFY/DELETE
           table_name TYPE string,
           field_list TYPE string_table,
           where_clause TYPE string,
           from_clause TYPE string,
         END OF ty_dml_info.

  TYPES: BEGIN OF ty_call_info,
           call_type   TYPE string,   " FUNCTION/METHOD/PERFORM
           target_name TYPE string,
           parameters  TYPE string_table,
           destination TYPE string,    " For RFC
         END OF ty_call_info.

  METHODS:
    parse_dml_statement
      IMPORTING
        it_tokens TYPE stokesx_tab
      RETURNING
        VALUE(rs_dml) TYPE ty_dml_info,

    parse_call_statement
      IMPORTING
        it_tokens TYPE stokesx_tab
      RETURNING
        VALUE(rs_call) TYPE ty_call_info,

    parse_assignment
      IMPORTING
        it_tokens TYPE stokesx_tab
      RETURNING
        VALUE(rt_assignments) TYPE string_table,

    is_custom_table
      IMPORTING
        iv_table_name TYPE string
      RETURNING
        VALUE(rv_is_custom) TYPE abap_bool.
ENDINTERFACE.
```

### ZIF_SYUNAME_REPORT
```abap
INTERFACE zif_syuname_report PUBLIC.
  TYPES: BEGIN OF ty_finding,
           program     TYPE programm,
           line        TYPE i,
           operation   TYPE string,
           target      TYPE string,
           field       TYPE string,
           taint_path  TYPE string,
           confidence  TYPE string,
           scope_info  TYPE string,
         END OF ty_finding.

  TYPES: ty_findings TYPE TABLE OF ty_finding.

  METHODS:
    add_finding
      IMPORTING
        is_finding TYPE ty_finding,

    generate_csv
      IMPORTING
        iv_output_path TYPE string
      RAISING
        zcx_syuname_error,

    get_statistics
      RETURNING
        VALUE(rs_stats) TYPE zif_syuname_stats=>ty_statistics,

    write_to_application_log
      IMPORTING
        iv_object    TYPE balobj_d DEFAULT 'ZSYUNAME'
        iv_subobject TYPE balsubobj DEFAULT 'ANALYSIS'.
ENDINTERFACE.
```

## Exception Classes

### ZCX_SYUNAME_ERROR
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
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        program_name TYPE programm OPTIONAL
        file_path TYPE string OPTIONAL
        statement TYPE string OPTIONAL.
ENDCLASS.
```

## Factory Class

### ZCL_SYUNAME_FACTORY
```abap
CLASS zcl_syuname_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_scanner
        RETURNING
          VALUE(ro_scanner) TYPE REF TO zif_syuname_scanner,

      create_taint_engine
        RETURNING
          VALUE(ro_taint) TYPE REF TO zif_syuname_taint,

      create_parser
        RETURNING
          VALUE(ro_parser) TYPE REF TO zif_syuname_parser,

      create_reporter
        RETURNING
          VALUE(ro_reporter) TYPE REF TO zif_syuname_report,

      create_analyzer
        IMPORTING
          iv_mode TYPE char1 DEFAULT 'L'
        RETURNING
          VALUE(ro_analyzer) TYPE REF TO zif_syuname_analyzer.
ENDCLASS.
```

## Main Analyzer Interface

### ZIF_SYUNAME_ANALYZER
```abap
INTERFACE zif_syuname_analyzer PUBLIC.
  METHODS:
    analyze_program
      IMPORTING
        iv_program     TYPE programm
        iv_output_path TYPE string
        it_exclusions  TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_findings_count) TYPE i
      RAISING
        zcx_syuname_error,

    get_last_analysis_stats
      RETURNING
        VALUE(rs_stats) TYPE zif_syuname_stats=>ty_statistics.
ENDINTERFACE.
```

## Statistics Interface

### ZIF_SYUNAME_STATS
```abap
INTERFACE zif_syuname_stats PUBLIC.
  TYPES: BEGIN OF ty_statistics,
           programs_analyzed TYPE i,
           lines_processed   TYPE i,
           findings_total    TYPE i,
           findings_high     TYPE i,
           findings_medium   TYPE i,
           findings_low      TYPE i,
           execution_time    TYPE decimals,
           memory_used_mb    TYPE decimals,
         END OF ty_statistics.
ENDINTERFACE.
```

## Usage Example

```abap
REPORT zsyuname_analyzer.

DATA: lo_analyzer TYPE REF TO zif_syuname_analyzer,
      lv_findings TYPE i.

START-OF-SELECTION.
  " Create analyzer in LOCAL_ONLY mode
  lo_analyzer = zcl_syuname_factory=>create_analyzer( 'L' ).

  TRY.
      " Analyze program
      lv_findings = lo_analyzer->analyze_program(
        iv_program = p_prog
        iv_output_path = p_output
        it_exclusions = s_excl[] ).

      WRITE: / 'Analysis completed with', lv_findings, 'findings'.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
```
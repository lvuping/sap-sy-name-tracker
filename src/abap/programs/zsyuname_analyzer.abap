*&---------------------------------------------------------------------*
*& Report ZSYUNAME_ANALYZER
*&---------------------------------------------------------------------*
*& SAP ABAP SY-UNAME Tracker - Main Program
*& Analyzes ABAP code to track sy-uname data flow to database operations
*&---------------------------------------------------------------------*
REPORT zsyuname_analyzer.

TABLES: trdir.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_prog   TYPE programm OBLIGATORY,
            p_mode   TYPE char1 DEFAULT 'L' AS LISTBOX VISIBLE LENGTH 20,
            p_output TYPE string LOWER CASE DEFAULT '/tmp/syuname_results.csv',
            p_debug  TYPE abap_bool AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: s_excl FOR trdir-name NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& Global Data
*&---------------------------------------------------------------------*
DATA: go_scanner    TYPE REF TO zif_syuname_scanner,
      go_taint      TYPE REF TO zif_syuname_taint,
      go_parser     TYPE REF TO zif_syuname_parser,
      go_reporter   TYPE REF TO zif_syuname_report,
      gt_source     TYPE zif_syuname_scanner=>tt_source_lines,
      gt_includes   TYPE zif_syuname_scanner=>tt_includes,
      gt_tokens     TYPE zif_syuname_parser=>tt_tokens,
      gt_statements TYPE zif_syuname_parser=>tt_statements,
      gv_lines      TYPE i,
      gv_progress   TYPE i,
      gv_msg        TYPE string.

*&---------------------------------------------------------------------*
*& Initialization
*&---------------------------------------------------------------------*
INITIALIZATION.
  " Set default texts
  %_p_prog_%_app_%-text = 'Program Name'.
  %_p_mode_%_app_%-text = 'Analysis Mode'.
  %_p_output_%_app_%-text = 'Output File'.
  %_p_debug_%_app_%-text = 'Debug Mode'.
  %_s_excl_%_app_%-text = 'Exclude Programs'.

  " Set list box values for mode
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_MODE'
      values = VALUE vrm_values( ( key = 'L' text = 'Local Only' )
                                  ( key = 'F' text = 'Full (with Includes)' ) ).

*&---------------------------------------------------------------------*
*& At Selection Screen
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Validate program exists
  SELECT SINGLE name FROM trdir
    INTO @DATA(lv_check)
    WHERE name = @p_prog.

  IF sy-subrc <> 0.
    MESSAGE e001(zsyuname) WITH p_prog.
  ENDIF.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      " Create components via factory
      zcl_syuname_factory=>create_all_components(
        IMPORTING
          eo_scanner  = go_scanner
          eo_taint    = go_taint
          eo_parser   = go_parser
          eo_reporter = go_reporter ).

      " Start timing
      go_reporter->set_start_time( ).

      " Log start
      IF p_debug = abap_true.
        WRITE: / |Starting analysis of program { p_prog }|.
        WRITE: / |Mode: { COND #( WHEN p_mode = 'L' THEN 'Local Only' ELSE 'Full with Includes' ) }|.
        WRITE: / |Output: { p_output }|.
        WRITE: / ''.
      ENDIF.

      " Read main program
      PERFORM read_program USING p_prog.

      " Get includes if full mode
      IF p_mode = 'F'.
        PERFORM get_includes USING p_prog.
      ENDIF.

      " Tokenize and analyze
      PERFORM analyze_source.

      " Generate report
      PERFORM generate_output.

      " End timing
      go_reporter->set_end_time( ).

      " Show summary
      PERFORM show_summary.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

*&---------------------------------------------------------------------*
*& Form READ_PROGRAM
*&---------------------------------------------------------------------*
FORM read_program USING iv_program TYPE programm.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines.

  " Read program source
  lt_source = go_scanner->read_program( iv_program ).

  " Add to global source
  APPEND LINES OF lt_source TO gt_source.

  " Update statistics
  go_reporter->increment_counter( 'PROGRAMS' ).
  ADD lines( lt_source ) TO gv_lines.

  IF p_debug = abap_true.
    WRITE: / |Read program { iv_program }: { lines( lt_source ) } lines|.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_INCLUDES
*&---------------------------------------------------------------------*
FORM get_includes USING iv_program TYPE programm.
  DATA: lt_includes TYPE zif_syuname_scanner=>tt_includes,
        lt_source   TYPE zif_syuname_scanner=>tt_source_lines.

  " Get includes
  lt_includes = go_scanner->get_includes(
    iv_program_name = iv_program
    iv_recursive    = abap_true ).

  APPEND LINES OF lt_includes TO gt_includes.

  " Read each include
  LOOP AT lt_includes INTO DATA(ls_include).
    " Check exclusions
    IF ls_include-include_name IN s_excl.
      CONTINUE.
    ENDIF.

    TRY.
        lt_source = go_scanner->read_include(
          iv_include_name = ls_include-include_name
          iv_parent_prog  = ls_include-parent_prog ).

        APPEND LINES OF lt_source TO gt_source.

        go_reporter->increment_counter( 'INCLUDES' ).
        ADD lines( lt_source ) TO gv_lines.

        IF p_debug = abap_true.
          WRITE: / |Read include { ls_include-include_name }: { lines( lt_source ) } lines|.
        ENDIF.

      CATCH zcx_syuname_error.
        " Ignore missing includes
        IF p_debug = abap_true.
          WRITE: / |Warning: Could not read include { ls_include-include_name }|.
        ENDIF.
    ENDTRY.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ANALYZE_SOURCE
*&---------------------------------------------------------------------*
FORM analyze_source.
  DATA: ls_assignment TYPE zif_syuname_parser=>ty_assignment,
        ls_dml        TYPE zif_syuname_parser=>ty_dml_statement,
        ls_call       TYPE zif_syuname_parser=>ty_call_statement,
        lv_stmt_type  TYPE string,
        lv_line_count TYPE i.

  " Tokenize source
  go_parser->tokenize_source(
    EXPORTING
      it_source     = gt_source
    IMPORTING
      et_tokens     = gt_tokens
      et_statements = gt_statements ).

  IF p_debug = abap_true.
    WRITE: / |Tokenized: { lines( gt_tokens ) } tokens, { lines( gt_statements ) } statements|.
  ENDIF.

  " Process each statement
  LOOP AT gt_statements INTO DATA(ls_statement).
    gv_progress = sy-tabix.

    " Show progress for large programs
    lv_line_count = lines( gt_statements ).
    IF lv_line_count > 5000 AND gv_progress MOD 1000 = 0.
      gv_msg = |Processing statement { gv_progress } of { lv_line_count }|.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = ( gv_progress * 100 ) / lv_line_count
          text       = gv_msg.
    ENDIF.

    " Analyze statement type
    lv_stmt_type = go_parser->analyze_statement_type(
      it_tokens    = gt_tokens
      is_statement = ls_statement ).

    " Get line number from first token
    DATA(lv_line) = 0.
    READ TABLE gt_tokens INTO DATA(ls_first_token) INDEX ls_statement-from.
    IF sy-subrc = 0.
      lv_line = ls_first_token-row.
    ENDIF.

    CASE lv_stmt_type.
      WHEN 'ASSIGNMENT'.
        " Parse assignment
        ls_assignment = go_parser->parse_assignment(
          it_tokens    = gt_tokens
          is_statement = ls_statement
          iv_line      = lv_line ).

        " Check for sy-uname assignment
        PERFORM check_syuname_assignment USING ls_assignment lv_line.

      WHEN 'DML'.
        " Parse DML statement
        ls_dml = go_parser->parse_dml_statement(
          it_tokens    = gt_tokens
          is_statement = ls_statement
          iv_line      = lv_line ).

        " Check for tainted variables in DML
        PERFORM check_dml_taint USING ls_dml lv_line.

      WHEN 'CALL'.
        " Parse call statement
        ls_call = go_parser->parse_call_statement(
          it_tokens    = gt_tokens
          is_statement = ls_statement
          iv_line      = lv_line ).

        " Check for tainted parameters in call
        PERFORM check_call_taint USING ls_call lv_line.
    ENDCASE.

    " Update line counter
    go_reporter->increment_counter( 'LINES' ).
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_SYUNAME_ASSIGNMENT
*&---------------------------------------------------------------------*
FORM check_syuname_assignment USING is_assignment TYPE zif_syuname_parser=>ty_assignment
                                     iv_line TYPE i.
  DATA: lt_path TYPE string_table.

  " Check if source is sy-uname
  IF is_assignment-source_var CS 'sy-uname' OR
     is_assignment-source_var CS 'SY-UNAME' OR
     is_assignment-source_var CS 'syst-uname' OR
     is_assignment-source_var CS 'SYST-UNAME'.

    " Mark sy-uname as initial taint source
    APPEND 'sy-uname' TO lt_path.

    go_taint->mark_tainted(
      iv_variable   = is_assignment-target_var
      iv_scope      = go_taint->get_current_scope( )-scope_type
      iv_line       = iv_line
      iv_program    = p_prog
      it_path       = lt_path
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    go_reporter->increment_counter( 'SYUNAME' ).

    IF p_debug = abap_true.
      WRITE: / |Line { iv_line }: sy-uname assigned to { is_assignment-target_var }|.
    ENDIF.

  ELSEIF go_taint->is_tainted( iv_variable = is_assignment-source_var ).
    " Propagate taint
    go_taint->propagate_taint(
      iv_from_var  = is_assignment-source_var
      iv_to_var    = is_assignment-target_var
      iv_scope     = go_taint->get_current_scope( )-scope_type
      iv_line      = iv_line
      iv_program   = p_prog
      iv_operation = is_assignment-assign_type ).

    IF p_debug = abap_true.
      WRITE: / |Line { iv_line }: Taint propagated from { is_assignment-source_var } to { is_assignment-target_var }|.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_DML_TAINT
*&---------------------------------------------------------------------*
FORM check_dml_taint USING is_dml TYPE zif_syuname_parser=>ty_dml_statement
                           iv_line TYPE i.
  DATA: lv_tainted TYPE abap_bool,
        lt_path    TYPE string_table.

  " Check if any source variable is tainted
  LOOP AT is_dml-source_vars INTO DATA(lv_var).
    IF go_taint->is_tainted( iv_variable = lv_var ).
      lv_tainted = abap_true.
      lt_path = go_taint->get_taint_path( iv_variable = lv_var ).
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_tainted = abap_true.
    " Add finding
    go_reporter->add_finding(
      iv_program      = p_prog
      iv_syuname_line = iv_line
      iv_operation    = is_dml-operation
      iv_target_table = is_dml-table_name
      it_taint_path   = lt_path
      iv_confidence   = COND #( WHEN is_dml-is_dynamic = abap_true
                                 THEN zif_syuname_taint=>c_confidence-medium
                                 ELSE zif_syuname_taint=>c_confidence-high ) ).

    IF p_debug = abap_true.
      WRITE: / |Line { iv_line }: FINDING - { is_dml-operation } to table { is_dml-table_name } with tainted data|.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_CALL_TAINT
*&---------------------------------------------------------------------*
FORM check_call_taint USING is_call TYPE zif_syuname_parser=>ty_call_statement
                            iv_line TYPE i.
  DATA: lv_tainted TYPE abap_bool,
        lt_path    TYPE string_table,
        lv_param   TYPE string.

  " Check if any parameter is tainted
  LOOP AT is_call-parameters INTO lv_param.
    " Extract parameter value (after =)
    SPLIT lv_param AT '=' INTO DATA(lv_name) DATA(lv_value).

    IF go_taint->is_tainted( iv_variable = lv_value ).
      lv_tainted = abap_true.
      lt_path = go_taint->get_taint_path( iv_variable = lv_value ).
      APPEND lv_param TO is_call-tainted_params.
    ENDIF.
  ENDLOOP.

  IF lv_tainted = abap_true AND is_call-is_rfc = abap_true.
    " Add RFC finding
    go_reporter->add_finding(
      iv_program      = p_prog
      iv_syuname_line = iv_line
      iv_operation    = zif_syuname_report=>c_operation_type-rfc_call
      iv_target_rfc   = is_call-module_name
      it_taint_path   = lt_path
      iv_confidence   = zif_syuname_taint=>c_confidence-high ).

    IF p_debug = abap_true.
      WRITE: / |Line { iv_line }: FINDING - RFC call to { is_call-module_name } with tainted parameters|.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GENERATE_OUTPUT
*&---------------------------------------------------------------------*
FORM generate_output.
  DATA: lt_summary TYPE string_table.

  " Generate CSV file
  go_reporter->generate_csv( p_output ).

  " Write to application log if requested
  IF p_debug = abap_true.
    TRY.
        go_reporter->write_to_application_log( ).
      CATCH zcx_syuname_error.
        " Ignore log errors
    ENDTRY.
  ENDIF.

  " Get summary
  lt_summary = go_reporter->generate_summary_report( ).

  IF p_debug = abap_true.
    WRITE: / ''.
    LOOP AT lt_summary INTO DATA(lv_line).
      WRITE: / lv_line.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SHOW_SUMMARY
*&---------------------------------------------------------------------*
FORM show_summary.
  DATA: ls_stats TYPE zif_syuname_report=>ty_statistics.

  ls_stats = go_reporter->get_statistics( ).

  WRITE: / ''.
  WRITE: / '========================================'.
  WRITE: / 'SY-UNAME Tracker Analysis Complete'.
  WRITE: / '========================================'.
  WRITE: / |Programs analyzed: { ls_stats-programs_analyzed }|.
  WRITE: / |Includes analyzed: { ls_stats-includes_analyzed }|.
  WRITE: / |Lines processed: { ls_stats-lines_processed }|.
  WRITE: / |SY-UNAME found: { ls_stats-syuname_found }|.
  WRITE: / '----------------------------------------'.
  WRITE: / |Total findings: { ls_stats-findings_total }|.
  WRITE: / |  DML operations: { ls_stats-findings_dml }|.
  WRITE: / |  RFC calls: { ls_stats-findings_rfc }|.
  WRITE: / |  Other: { ls_stats-findings_other }|.
  WRITE: / '----------------------------------------'.
  WRITE: / |Output file: { p_output }|.
  WRITE: / |Duration: { ls_stats-duration_seconds } seconds|.
  WRITE: / '========================================'.

  IF ls_stats-findings_total > 0.
    MESSAGE s002(zsyuname) WITH ls_stats-findings_total p_output.
  ELSE.
    MESSAGE s003(zsyuname).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Report ZSYUNAME_TEST_INTEGRATION
*&---------------------------------------------------------------------*
*& Integration test program for SY-UNAME Tracker
*& Tests end-to-end scenarios from quickstart documentation
*&---------------------------------------------------------------------*
REPORT zsyuname_test_integration.

CONSTANTS: gc_test_program TYPE programm VALUE 'ZSYUNAME_TEST_SAMPLE'.

DATA: go_scanner  TYPE REF TO zif_syuname_scanner,
      go_taint    TYPE REF TO zif_syuname_taint,
      go_parser   TYPE REF TO zif_syuname_parser,
      go_reporter TYPE REF TO zif_syuname_report,
      gv_result   TYPE abap_bool.

*&---------------------------------------------------------------------*
*& Test Scenario 1: Direct Assignment
*&---------------------------------------------------------------------*
FORM test_direct_assignment.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
        lt_findings TYPE zif_syuname_report=>tt_findings.

  WRITE: / 'Test Scenario 1: Direct Assignment'.
  WRITE: / '=================================='.

  " Simulate source code with direct assignment
  lt_source = VALUE #(
    ( line_number = 1  statement = 'DATA: lv_user TYPE string.' program = gc_test_program )
    ( line_number = 2  statement = 'lv_user = sy-uname.' program = gc_test_program )
    ( line_number = 3  statement = 'INSERT ztable FROM @( VALUE #( username = lv_user ) ).' program = gc_test_program )
  ).

  TRY.
      " Process source
      PERFORM process_source USING lt_source.

      " Check findings
      lt_findings = go_reporter->get_all_findings( ).

      IF lines( lt_findings ) > 0.
        WRITE: / 'PASS: Direct assignment taint detected'.
      ELSE.
        WRITE: / 'FAIL: Direct assignment taint NOT detected'.
      ENDIF.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      WRITE: / 'ERROR:', lx_error->get_text( ).
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Test Scenario 2: Structure Transfer
*&---------------------------------------------------------------------*
FORM test_structure_transfer.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
        lt_findings TYPE zif_syuname_report=>tt_findings.

  WRITE: / ''.
  WRITE: / 'Test Scenario 2: Structure Transfer'.
  WRITE: / '===================================='.

  " Simulate source code with structure transfer
  lt_source = VALUE #(
    ( line_number = 1  statement = 'DATA: BEGIN OF ls_data,' program = gc_test_program )
    ( line_number = 2  statement = '        username TYPE string,' program = gc_test_program )
    ( line_number = 3  statement = '        date TYPE dats,' program = gc_test_program )
    ( line_number = 4  statement = '      END OF ls_data.' program = gc_test_program )
    ( line_number = 5  statement = 'ls_data-username = sy-uname.' program = gc_test_program )
    ( line_number = 6  statement = 'INSERT ztable FROM ls_data.' program = gc_test_program )
  ).

  TRY.
      " Process source
      PERFORM process_source USING lt_source.

      " Check findings
      lt_findings = go_reporter->get_all_findings( ).

      IF lines( lt_findings ) > 0.
        WRITE: / 'PASS: Structure transfer taint detected'.
      ELSE.
        WRITE: / 'FAIL: Structure transfer taint NOT detected'.
      ENDIF.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      WRITE: / 'ERROR:', lx_error->get_text( ).
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Test Scenario 3: RFC Call with Tainted Parameters
*&---------------------------------------------------------------------*
FORM test_rfc_call.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
        lt_findings TYPE zif_syuname_report=>tt_findings.

  WRITE: / ''.
  WRITE: / 'Test Scenario 3: RFC Call with Tainted Parameters'.
  WRITE: / '=================================================='.

  " Simulate source code with RFC call
  lt_source = VALUE #(
    ( line_number = 1  statement = 'DATA: lv_user TYPE string.' program = gc_test_program )
    ( line_number = 2  statement = 'lv_user = sy-uname.' program = gc_test_program )
    ( line_number = 3  statement = 'CALL FUNCTION ''Z_UPDATE_USER'' DESTINATION ''RFC_DEST''' program = gc_test_program )
    ( line_number = 4  statement = '  EXPORTING' program = gc_test_program )
    ( line_number = 5  statement = '    iv_username = lv_user.' program = gc_test_program )
  ).

  TRY.
      " Process source
      PERFORM process_source USING lt_source.

      " Check findings
      lt_findings = go_reporter->get_all_findings( ).

      IF lines( lt_findings ) > 0.
        WRITE: / 'PASS: RFC call taint detected'.
      ELSE.
        WRITE: / 'FAIL: RFC call taint NOT detected'.
      ENDIF.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      WRITE: / 'ERROR:', lx_error->get_text( ).
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Test Scenario 4: MOVE-CORRESPONDING
*&---------------------------------------------------------------------*
FORM test_move_corresponding.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
        lt_findings TYPE zif_syuname_report=>tt_findings.

  WRITE: / ''.
  WRITE: / 'Test Scenario 4: MOVE-CORRESPONDING'.
  WRITE: / '===================================='.

  " Simulate source code with MOVE-CORRESPONDING
  lt_source = VALUE #(
    ( line_number = 1  statement = 'DATA: ls_source TYPE ztable,' program = gc_test_program )
    ( line_number = 2  statement = '      ls_target TYPE ztable.' program = gc_test_program )
    ( line_number = 3  statement = 'ls_source-username = sy-uname.' program = gc_test_program )
    ( line_number = 4  statement = 'MOVE-CORRESPONDING ls_source TO ls_target.' program = gc_test_program )
    ( line_number = 5  statement = 'INSERT ztable FROM ls_target.' program = gc_test_program )
  ).

  TRY.
      " Process source
      PERFORM process_source USING lt_source.

      " Check findings
      lt_findings = go_reporter->get_all_findings( ).

      IF lines( lt_findings ) > 0.
        WRITE: / 'PASS: MOVE-CORRESPONDING taint detected'.
      ELSE.
        WRITE: / 'FAIL: MOVE-CORRESPONDING taint NOT detected'.
      ENDIF.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      WRITE: / 'ERROR:', lx_error->get_text( ).
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Test Scenario 5: Internal Table Operations
*&---------------------------------------------------------------------*
FORM test_internal_table.
  DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
        lt_findings TYPE zif_syuname_report=>tt_findings.

  WRITE: / ''.
  WRITE: / 'Test Scenario 5: Internal Table Operations'.
  WRITE: / '==========================================='.

  " Simulate source code with internal table operations
  lt_source = VALUE #(
    ( line_number = 1  statement = 'DATA: lt_users TYPE TABLE OF ztable.' program = gc_test_program )
    ( line_number = 2  statement = 'DATA: ls_user TYPE ztable.' program = gc_test_program )
    ( line_number = 3  statement = 'ls_user-username = sy-uname.' program = gc_test_program )
    ( line_number = 4  statement = 'APPEND ls_user TO lt_users.' program = gc_test_program )
    ( line_number = 5  statement = 'INSERT ztable FROM TABLE lt_users.' program = gc_test_program )
  ).

  TRY.
      " Process source
      PERFORM process_source USING lt_source.

      " Check findings
      lt_findings = go_reporter->get_all_findings( ).

      IF lines( lt_findings ) > 0.
        WRITE: / 'PASS: Internal table taint detected'.
      ELSE.
        WRITE: / 'FAIL: Internal table taint NOT detected'.
      ENDIF.

    CATCH zcx_syuname_error INTO DATA(lx_error).
      WRITE: / 'ERROR:', lx_error->get_text( ).
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Helper Form: Process Source Code
*&---------------------------------------------------------------------*
FORM process_source USING it_source TYPE zif_syuname_scanner=>tt_source_lines.
  DATA: lt_tokens     TYPE zif_syuname_parser=>tt_tokens,
        lt_statements TYPE zif_syuname_parser=>tt_statements.

  " Reset components
  go_taint->clear_taint_data( ).
  go_reporter->clear_findings( ).

  " This would normally:
  " 1. Tokenize the source
  " 2. Identify sy-uname usage
  " 3. Track taint propagation
  " 4. Detect operations on tainted data
  " 5. Report findings

  " Since implementation is not complete, we simulate failure
  RETURN.
ENDFORM.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  " Create instances (would use factory in real implementation)
  CREATE OBJECT go_scanner TYPE zcl_syuname_scanner.
  CREATE OBJECT go_taint TYPE zcl_syuname_taint.
  CREATE OBJECT go_parser TYPE zcl_syuname_parser.
  CREATE OBJECT go_reporter TYPE zcl_syuname_report.

  WRITE: / 'SY-UNAME Tracker Integration Tests'.
  WRITE: / '==================================='.
  WRITE: / ''.

  " Run test scenarios
  PERFORM test_direct_assignment.
  PERFORM test_structure_transfer.
  PERFORM test_rfc_call.
  PERFORM test_move_corresponding.
  PERFORM test_internal_table.

  WRITE: / ''.
  WRITE: / '==================================='.
  WRITE: / 'Integration Tests Complete'.
  WRITE: / 'Note: All tests expected to fail until implementation complete'.
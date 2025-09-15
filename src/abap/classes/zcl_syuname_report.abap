CLASS zcl_syuname_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_report.

  PRIVATE SECTION.
    DATA: mt_findings   TYPE zif_syuname_report=>tt_findings,
          ms_statistics TYPE zif_syuname_report=>ty_statistics,
          mv_sequence   TYPE i.

    METHODS: format_csv_line
      IMPORTING
        is_finding TYPE zif_syuname_report=>ty_finding
      RETURNING
        VALUE(rv_line) TYPE string,

      escape_csv_field
        IMPORTING
          iv_field TYPE string
        RETURNING
          VALUE(rv_escaped) TYPE string,

      format_taint_path
        IMPORTING
          it_path TYPE string_table
        RETURNING
          VALUE(rv_path) TYPE string.

ENDCLASS.

CLASS zcl_syuname_report IMPLEMENTATION.
  METHOD zif_syuname_report~add_finding.
    DATA: ls_finding TYPE zif_syuname_report=>ty_finding.

    " Increment sequence number
    ADD 1 TO mv_sequence.

    " Create finding entry
    ls_finding-sequence_num = mv_sequence.
    ls_finding-program_name = iv_program.
    ls_finding-include_name = COND #( WHEN iv_include IS NOT INITIAL
                                       THEN iv_include
                                       ELSE iv_program ).
    ls_finding-syuname_line = iv_syuname_line.
    ls_finding-operation = iv_operation.
    ls_finding-target_table = iv_target_table.
    ls_finding-target_rfc = iv_target_rfc.
    ls_finding-field_name = iv_field_name.
    ls_finding-taint_path = it_taint_path.
    ls_finding-confidence = iv_confidence.
    GET TIME STAMP FIELD ls_finding-timestamp.

    " Add to findings table
    APPEND ls_finding TO mt_findings.

    " Update statistics
    ADD 1 TO ms_statistics-findings_total.

    CASE iv_operation.
      WHEN zif_syuname_report=>c_operation_type-dml_insert
        OR zif_syuname_report=>c_operation_type-dml_update
        OR zif_syuname_report=>c_operation_type-dml_modify
        OR zif_syuname_report=>c_operation_type-dml_delete.
        ADD 1 TO ms_statistics-findings_dml.
      WHEN zif_syuname_report=>c_operation_type-rfc_call.
        ADD 1 TO ms_statistics-findings_rfc.
      WHEN OTHERS.
        ADD 1 TO ms_statistics-findings_other.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_syuname_report~generate_csv.
    DATA: lt_csv       TYPE zif_syuname_report=>tt_csv_lines,
          lv_csv_line  TYPE string,
          lv_file_line TYPE string.

    " Get CSV content
    lt_csv = zif_syuname_report~get_csv_content( ).

    TRY.
        OPEN DATASET iv_file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_syuname_error
            EXPORTING
              textid = zcx_syuname_error=>file_write_error
              file_path = iv_file_path.
        ENDIF.

        " Write header
        lv_csv_line = 'Seq,Program,Include,Line,Operation,Table,RFC,Field,Path'.
        TRANSFER lv_csv_line TO iv_file_path.

        " Write data rows
        LOOP AT lt_csv INTO DATA(ls_csv).
          TRANSFER ls_csv-line TO iv_file_path.
        ENDLOOP.

        CLOSE DATASET iv_file_path.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_open_mode.
        RAISE EXCEPTION TYPE zcx_syuname_error
          EXPORTING
            textid = zcx_syuname_error=>file_write_error
            file_path = iv_file_path.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_syuname_report~get_csv_content.
    DATA: lv_line TYPE string.

    CLEAR rt_csv.

    " Convert findings to CSV format
    LOOP AT mt_findings INTO DATA(ls_finding).
      lv_line = format_csv_line( ls_finding ).
      APPEND VALUE #( line = lv_line ) TO rt_csv.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_report~get_statistics.
    rs_stats = ms_statistics.
  ENDMETHOD.

  METHOD zif_syuname_report~get_all_findings.
    rt_findings = mt_findings.
  ENDMETHOD.

  METHOD zif_syuname_report~clear_findings.
    CLEAR: mt_findings, ms_statistics, mv_sequence.
  ENDMETHOD.

  METHOD zif_syuname_report~set_start_time.
    GET TIME STAMP FIELD ms_statistics-start_time.
  ENDMETHOD.

  METHOD zif_syuname_report~set_end_time.
    GET TIME STAMP FIELD ms_statistics-end_time.
  ENDMETHOD.

  METHOD zif_syuname_report~increment_counter.
    CASE iv_counter_type.
      WHEN 'PROGRAMS'.
        ADD 1 TO ms_statistics-programs_analyzed.
      WHEN 'INCLUDES'.
        ADD 1 TO ms_statistics-includes_analyzed.
      WHEN 'LINES'.
        ADD 1 TO ms_statistics-lines_processed.
      WHEN 'SYUNAME'.
        ADD 1 TO ms_statistics-syuname_found.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_syuname_report~write_to_application_log.
    DATA: ls_log        TYPE bal_s_log,
          ls_msg        TYPE bal_s_msg,
          lv_log_handle TYPE balloghndl.

    " Create log header
    ls_log-object = iv_object.
    ls_log-subobject = iv_subobject.
    ls_log-aldate = sy-datum.
    ls_log-altime = sy-uzeit.
    ls_log-aluser = sy-uname.
    ls_log-alprog = sy-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>file_write_error
          file_path = 'Application Log'.
    ENDIF.

    " Add summary message
    ls_msg-msgty = 'I'.
    ls_msg-msgid = 'ZSYUNAME'.
    ls_msg-msgno = '100'.
    ls_msg-msgv1 = |Findings: { ms_statistics-findings_total }|.
    ls_msg-msgv2 = |Programs: { ms_statistics-programs_analyzed }|.
    ls_msg-msgv3 = |Lines: { ms_statistics-lines_processed }|.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle
        i_s_msg      = ls_msg.

    " Save log
    CALL FUNCTION 'BAL_DB_SAVE'
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>file_write_error
          file_path = 'Application Log Save'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_report~export_to_json.
    RAISE EXCEPTION TYPE zcx_syuname_error
      EXPORTING
        textid = zcx_syuname_error=>file_write_error
        file_path = iv_file_path.
  ENDMETHOD.

  METHOD zif_syuname_report~filter_findings.
    CLEAR rt_filtered.

    LOOP AT mt_findings INTO DATA(ls_finding).
      " Apply filters
      IF iv_operation IS NOT INITIAL AND ls_finding-operation <> iv_operation.
        CONTINUE.
      ENDIF.

      IF iv_confidence IS NOT INITIAL AND ls_finding-confidence <> iv_confidence.
        CONTINUE.
      ENDIF.

      IF iv_table_name IS NOT INITIAL AND ls_finding-target_table <> iv_table_name.
        CONTINUE.
      ENDIF.

      " Passed all filters
      APPEND ls_finding TO rt_filtered.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_report~sort_findings.
    CASE iv_sort_field.
      WHEN 'SEQUENCE_NUM'.
        SORT mt_findings BY sequence_num.
      WHEN 'PROGRAM_NAME'.
        SORT mt_findings BY program_name syuname_line.
      WHEN 'OPERATION'.
        SORT mt_findings BY operation sequence_num.
      WHEN 'CONFIDENCE'.
        SORT mt_findings BY confidence DESCENDING sequence_num.
      WHEN OTHERS.
        SORT mt_findings BY sequence_num.
    ENDCASE.
  ENDMETHOD.

  METHOD zif_syuname_report~generate_summary_report.
    DATA: lv_duration TYPE i.

    CLEAR rt_summary.

    " Calculate duration
    IF ms_statistics-start_time IS NOT INITIAL AND ms_statistics-end_time IS NOT INITIAL.
      lv_duration = cl_abap_tstmp=>subtract(
        tstmp1 = ms_statistics-end_time
        tstmp2 = ms_statistics-start_time ).
      ms_statistics-duration_seconds = lv_duration.
    ENDIF.

    " Build summary report
    APPEND |SY-UNAME Tracker Analysis Summary| TO rt_summary.
    APPEND |=====================================| TO rt_summary.
    APPEND |Programs Analyzed: { ms_statistics-programs_analyzed }| TO rt_summary.
    APPEND |Includes Analyzed: { ms_statistics-includes_analyzed }| TO rt_summary.
    APPEND |Lines Processed: { ms_statistics-lines_processed }| TO rt_summary.
    APPEND |SY-UNAME Found: { ms_statistics-syuname_found }| TO rt_summary.
    APPEND |-------------------------------------| TO rt_summary.
    APPEND |Total Findings: { ms_statistics-findings_total }| TO rt_summary.
    APPEND |  DML Operations: { ms_statistics-findings_dml }| TO rt_summary.
    APPEND |  RFC Calls: { ms_statistics-findings_rfc }| TO rt_summary.
    APPEND |  Other: { ms_statistics-findings_other }| TO rt_summary.
    APPEND |-------------------------------------| TO rt_summary.
    APPEND |Analysis Duration: { lv_duration } seconds| TO rt_summary.

    " Add top affected tables
    DATA: lt_tables TYPE TABLE OF string,
          lv_table  TYPE string.

    LOOP AT mt_findings INTO DATA(ls_finding) WHERE target_table IS NOT INITIAL.
      lv_table = ls_finding-target_table.
      COLLECT lv_table INTO lt_tables.
    ENDLOOP.

    IF lines( lt_tables ) > 0.
      APPEND |-------------------------------------| TO rt_summary.
      APPEND |Affected Tables:| TO rt_summary.
      LOOP AT lt_tables INTO lv_table.
        APPEND |  - { lv_table }| TO rt_summary.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD format_csv_line.
    DATA: lv_path TYPE string.

    " Format taint path
    lv_path = format_taint_path( is_finding-taint_path ).

    " Build CSV line
    rv_line = |{ is_finding-sequence_num },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-program_name ) },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-include_name ) },|.
    rv_line = |{ rv_line }{ is_finding-syuname_line },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-operation ) },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-target_table ) },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-target_rfc ) },|.
    rv_line = |{ rv_line }{ escape_csv_field( is_finding-field_name ) },|.
    rv_line = |{ rv_line }{ escape_csv_field( lv_path ) }|.
  ENDMETHOD.

  METHOD escape_csv_field.
    rv_escaped = iv_field.

    " Escape special CSV characters
    IF rv_escaped CS ',' OR rv_escaped CS '"' OR rv_escaped CS cl_abap_char_utilities=>cr_lf.
      " Escape quotes by doubling them
      REPLACE ALL OCCURRENCES OF '"' IN rv_escaped WITH '""'.
      " Wrap in quotes
      rv_escaped = |"{ rv_escaped }"|.
    ENDIF.
  ENDMETHOD.

  METHOD format_taint_path.
    DATA: lv_step TYPE string.

    CLEAR rv_path.

    " Join path steps with arrow
    LOOP AT it_path INTO lv_step.
      IF sy-tabix = 1.
        rv_path = lv_step.
      ELSE.
        rv_path = |{ rv_path } -> { lv_step }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*"* Test class definitions
CLASS ltc_report DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_syuname_report.

    METHODS: setup,
             test_add_finding FOR TESTING,
             test_generate_csv FOR TESTING,
             test_get_statistics FOR TESTING,
             test_filter_findings FOR TESTING,
             test_csv_formatting FOR TESTING,
             test_clear_findings FOR TESTING,
             test_time_tracking FOR TESTING,
             test_summary_report FOR TESTING.
ENDCLASS.

CLASS ltc_report IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW zcl_syuname_report( ).
  ENDMETHOD.

  METHOD test_add_finding.
    mo_cut->add_finding(
      iv_program      = 'ZTEST_PROG'
      iv_include      = 'ZTEST_INC'
      iv_syuname_line = 100
      iv_operation    = zif_syuname_report=>c_operation_type-dml_insert
      iv_target_table = 'ZTABLE'
      iv_field_name   = 'USERNAME'
      it_taint_path   = VALUE #( ( |sy-uname| ) ( |lv_user| ) ( |INSERT| ) )
      iv_confidence   = 'HIGH' ).

    DATA(lt_findings) = mo_cut->get_all_findings( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_findings
      msg = 'Findings not yet implemented' ).
  ENDMETHOD.

  METHOD test_generate_csv.
    DATA: lx_error TYPE REF TO zcx_syuname_error.

    mo_cut->add_finding(
      iv_program      = 'ZTEST'
      iv_syuname_line = 10
      iv_operation    = zif_syuname_report=>c_operation_type-dml_insert
      iv_target_table = 'ZTABLE'
      it_taint_path   = VALUE #( ( |sy-uname| ) )
      iv_confidence   = 'HIGH' ).

    TRY.
        mo_cut->generate_csv( '/tmp/test.csv' ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->if_t100_message~t100key
          exp = zcx_syuname_error=>file_write_error
          msg = 'Wrong exception type' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_statistics.
    mo_cut->set_start_time( ).
    mo_cut->increment_counter( 'PROGRAMS' ).
    mo_cut->increment_counter( 'LINES' ).
    mo_cut->set_end_time( ).

    DATA(ls_stats) = mo_cut->get_statistics( ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_stats-start_time
      msg = 'Start time should be set' ).
  ENDMETHOD.

  METHOD test_filter_findings.
    mo_cut->add_finding(
      iv_program      = 'ZTEST1'
      iv_syuname_line = 10
      iv_operation    = zif_syuname_report=>c_operation_type-dml_insert
      iv_target_table = 'ZTABLE1'
      it_taint_path   = VALUE #( ( |sy-uname| ) )
      iv_confidence   = 'HIGH' ).

    mo_cut->add_finding(
      iv_program      = 'ZTEST2'
      iv_syuname_line = 20
      iv_operation    = zif_syuname_report=>c_operation_type-rfc_call
      iv_target_rfc   = 'Z_RFC'
      it_taint_path   = VALUE #( ( |sy-uname| ) )
      iv_confidence   = 'MEDIUM' ).

    DATA(lt_filtered) = mo_cut->filter_findings(
      iv_operation = zif_syuname_report=>c_operation_type-dml_insert ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_filtered )
      exp = 0
      msg = 'Filter not yet implemented' ).
  ENDMETHOD.

  METHOD test_csv_formatting.
    DATA(lt_csv) = mo_cut->get_csv_content( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_csv
      msg = 'CSV content should be empty initially' ).
  ENDMETHOD.

  METHOD test_clear_findings.
    mo_cut->add_finding(
      iv_program      = 'ZTEST'
      iv_syuname_line = 10
      iv_operation    = zif_syuname_report=>c_operation_type-dml_insert
      iv_target_table = 'ZTABLE'
      it_taint_path   = VALUE #( ( |sy-uname| ) )
      iv_confidence   = 'HIGH' ).

    mo_cut->clear_findings( ).

    DATA(lt_findings) = mo_cut->get_all_findings( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_findings
      msg = 'All findings should be cleared' ).

    DATA(ls_stats) = mo_cut->get_statistics( ).
    cl_abap_unit_assert=>assert_initial(
      act = ls_stats-findings_total
      msg = 'Statistics should be reset' ).
  ENDMETHOD.

  METHOD test_time_tracking.
    mo_cut->set_start_time( ).
    WAIT UP TO '0.1' SECONDS.
    mo_cut->set_end_time( ).

    DATA(ls_stats) = mo_cut->get_statistics( ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_stats-start_time
      msg = 'Start time should be set' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_stats-end_time
      msg = 'End time should be set' ).
  ENDMETHOD.

  METHOD test_summary_report.
    mo_cut->add_finding(
      iv_program      = 'ZTEST1'
      iv_syuname_line = 10
      iv_operation    = zif_syuname_report=>c_operation_type-dml_insert
      iv_target_table = 'ZTABLE'
      it_taint_path   = VALUE #( ( |sy-uname| ) )
      iv_confidence   = 'HIGH' ).

    DATA(lt_summary) = mo_cut->generate_summary_report( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_summary
      msg = 'Summary not yet implemented' ).
  ENDMETHOD.
ENDCLASS.
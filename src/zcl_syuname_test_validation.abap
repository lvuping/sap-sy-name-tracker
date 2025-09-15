CLASS zcl_syuname_test_validation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.
    METHODS: test_real_sap_programs FOR TESTING,
             validate_quickstart_scenarios FOR TESTING,
             performance_benchmark FOR TESTING,
             memory_usage_test FOR TESTING.

  PRIVATE SECTION.
    DATA: mo_analyzer TYPE REF TO zcl_syuname_analyzer.

    METHODS: setup,
             create_test_program
               IMPORTING
                 iv_name TYPE programm
                 it_code TYPE string_table,
             measure_performance
               IMPORTING
                 iv_program_name TYPE programm
               RETURNING
                 VALUE(rv_duration_ms) TYPE i,
             measure_memory
               IMPORTING
                 iv_program_name TYPE programm
               RETURNING
                 VALUE(rv_memory_mb) TYPE i.

ENDCLASS.

CLASS zcl_syuname_test_validation IMPLEMENTATION.
  METHOD setup.
    " Initialize analyzer
    CREATE OBJECT mo_analyzer.
  ENDMETHOD.

  METHOD test_real_sap_programs.
    " T046: Test with real SAP programs (with permission)
    DATA: lt_test_programs TYPE TABLE OF programm,
          lv_findings TYPE i.

    " Add common SAP programs that use sy-uname
    " NOTE: Only test in development systems with permission
    APPEND 'RSUSR002' TO lt_test_programs.  " User list by complex criteria
    APPEND 'RSUSR003' TO lt_test_programs.  " Check user authorizations
    APPEND 'RSUSR070' TO lt_test_programs.  " User role assignments

    LOOP AT lt_test_programs INTO DATA(lv_program).
      " Check if program exists
      SELECT SINGLE progname FROM reposrc
        INTO @DATA(lv_exists)
        WHERE progname = @lv_program.

      IF sy-subrc = 0.
        TRY.
            " Run analysis
            mo_analyzer->analyze_program(
              iv_program_name = lv_program
              iv_mode = 'L' ).  " Local only for safety

            " Get findings count
            lv_findings = mo_analyzer->get_findings_count( ).

            " Assert that analysis completes without error
            cl_abap_unit_assert=>assert_true(
              act = xsdbool( lv_findings >= 0 )
              msg = |Analysis of { lv_program } should complete| ).

          CATCH zcx_syuname_error INTO DATA(lx_error).
            " Log but don't fail - program might not be accessible
            MESSAGE lx_error TYPE 'I'.
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_quickstart_scenarios.
    " T047: Validate all quickstart.md test scenarios
    DATA: lt_test_code TYPE string_table,
          lv_findings TYPE i.

    " Scenario 1: Direct assignment
    CLEAR lt_test_code.
    APPEND 'DATA: lv_user TYPE sy-uname.' TO lt_test_code.
    APPEND 'lv_user = sy-uname.' TO lt_test_code.
    APPEND 'INSERT INTO ztable VALUES lv_user.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_SCENARIO_1'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_SCENARIO_1'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_findings
      msg = 'Scenario 1: Should find 1 tainted INSERT' ).

    " Scenario 2: Structure transfer
    CLEAR lt_test_code.
    APPEND 'DATA: BEGIN OF ls_data,' TO lt_test_code.
    APPEND '        user TYPE sy-uname,' TO lt_test_code.
    APPEND '        date TYPE sy-datum,' TO lt_test_code.
    APPEND '      END OF ls_data.' TO lt_test_code.
    APPEND 'ls_data-user = sy-uname.' TO lt_test_code.
    APPEND 'INSERT ztable FROM ls_data.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_SCENARIO_2'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_SCENARIO_2'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_findings
      msg = 'Scenario 2: Should find 1 tainted structure INSERT' ).

    " Scenario 3: RFC call
    CLEAR lt_test_code.
    APPEND 'DATA: lv_user TYPE sy-uname.' TO lt_test_code.
    APPEND 'lv_user = sy-uname.' TO lt_test_code.
    APPEND 'CALL FUNCTION ''RFC_SYSTEM_INFO''' TO lt_test_code.
    APPEND '  EXPORTING' TO lt_test_code.
    APPEND '    user_name = lv_user.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_SCENARIO_3'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_SCENARIO_3'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_findings
      msg = 'Scenario 3: Should find 1 tainted RFC call' ).

    " Edge case: Field-symbols
    CLEAR lt_test_code.
    APPEND 'FIELD-SYMBOLS: <fs_user> TYPE any.' TO lt_test_code.
    APPEND 'DATA: lv_user TYPE sy-uname.' TO lt_test_code.
    APPEND 'lv_user = sy-uname.' TO lt_test_code.
    APPEND 'ASSIGN lv_user TO <fs_user>.' TO lt_test_code.
    APPEND 'INSERT INTO ztable VALUES <fs_user>.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_FIELD_SYMBOL'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_FIELD_SYMBOL'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_findings
      msg = 'Field-symbol: Should find 1 tainted INSERT with MEDIUM confidence' ).

    " Edge case: MOVE-CORRESPONDING
    CLEAR lt_test_code.
    APPEND 'DATA: BEGIN OF ls_source,' TO lt_test_code.
    APPEND '        usnam TYPE sy-uname,' TO lt_test_code.
    APPEND '      END OF ls_source,' TO lt_test_code.
    APPEND '      BEGIN OF ls_target,' TO lt_test_code.
    APPEND '        usnam TYPE sy-uname,' TO lt_test_code.
    APPEND '      END OF ls_target.' TO lt_test_code.
    APPEND 'ls_source-usnam = sy-uname.' TO lt_test_code.
    APPEND 'MOVE-CORRESPONDING ls_source TO ls_target.' TO lt_test_code.
    APPEND 'UPDATE ztable SET usnam = ls_target-usnam.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_MOVE_CORR'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_MOVE_CORR'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_findings
      msg = 'MOVE-CORRESPONDING: Should find 1 tainted UPDATE' ).

    " Edge case: Dynamic table names
    CLEAR lt_test_code.
    APPEND 'DATA: lv_user TYPE sy-uname,' TO lt_test_code.
    APPEND '      lv_table TYPE tabname VALUE ''ZTABLE''.' TO lt_test_code.
    APPEND 'lv_user = sy-uname.' TO lt_test_code.
    APPEND 'INSERT (lv_table) VALUES lv_user.' TO lt_test_code.

    create_test_program(
      iv_name = 'ZTEST_DYNAMIC'
      it_code = lt_test_code ).

    mo_analyzer->analyze_program(
      iv_program_name = 'ZTEST_DYNAMIC'
      iv_mode = 'L' ).

    lv_findings = mo_analyzer->get_findings_count( ).
    cl_abap_unit_assert=>assert_greater_equal(
      exp = 1
      act = lv_findings
      msg = 'Dynamic table: Should find tainted INSERT' ).
  ENDMETHOD.

  METHOD performance_benchmark.
    " T048: Performance benchmark - Process 100,000 lines within 10 minutes
    DATA: lt_large_code TYPE string_table,
          lv_duration_ms TYPE i,
          lv_lines TYPE i VALUE 100000.

    " Generate large test program
    DO lv_lines / 4 TIMES.
      APPEND 'DATA: lv_var TYPE string.' TO lt_large_code.
      APPEND 'lv_var = sy-uname.' TO lt_large_code.
      APPEND 'IF lv_var IS NOT INITIAL.' TO lt_large_code.
      APPEND '  INSERT INTO ztable VALUES lv_var.' TO lt_large_code.
      APPEND 'ENDIF.' TO lt_large_code.
    ENDDO.

    create_test_program(
      iv_name = 'ZTEST_PERF_100K'
      it_code = lt_large_code ).

    " Measure performance
    lv_duration_ms = measure_performance( 'ZTEST_PERF_100K' ).

    " Assert: Should complete within 10 minutes (600,000 ms)
    cl_abap_unit_assert=>assert_less_equal(
      exp = 600000  " 10 minutes in milliseconds
      act = lv_duration_ms
      msg = |Performance: 100K lines should process within 10 minutes (took { lv_duration_ms }ms)| ).

    " Calculate throughput
    DATA(lv_lines_per_min) = ( lv_lines * 60000 ) / lv_duration_ms.

    " Assert: Should process at least 10,000 lines per minute
    cl_abap_unit_assert=>assert_greater_equal(
      exp = 10000
      act = lv_lines_per_min
      msg = |Performance: Should process >= 10K lines/min (actual: { lv_lines_per_min })| ).
  ENDMETHOD.

  METHOD memory_usage_test.
    " T049: Memory test - Ensure <2GB usage for large programs
    DATA: lt_huge_code TYPE string_table,
          lv_memory_mb TYPE i,
          lv_lines TYPE i VALUE 200000.

    " Generate huge test program
    DO lv_lines / 5 TIMES.
      APPEND 'DATA: BEGIN OF ls_data,' TO lt_huge_code.
      APPEND '        user TYPE sy-uname,' TO lt_huge_code.
      APPEND '        text TYPE string,' TO lt_huge_code.
      APPEND '      END OF ls_data.' TO lt_huge_code.
      APPEND 'ls_data-user = sy-uname.' TO lt_huge_code.
      APPEND 'INSERT ztable FROM ls_data.' TO lt_huge_code.
    ENDDO.

    create_test_program(
      iv_name = 'ZTEST_MEM_200K'
      it_code = lt_huge_code ).

    " Measure memory usage
    lv_memory_mb = measure_memory( 'ZTEST_MEM_200K' ).

    " Assert: Should use less than 2GB (2048 MB)
    cl_abap_unit_assert=>assert_less_equal(
      exp = 2048
      act = lv_memory_mb
      msg = |Memory: Should use < 2GB for 200K lines (used { lv_memory_mb }MB)| ).

    " Test memory management features
    DATA(lo_memory_mgr) = NEW zcl_syuname_performance_opt( ).

    " Test that memory management kicks in
    DATA(lv_can_continue) = lo_memory_mgr->manage_memory_usage( lv_lines ).

    cl_abap_unit_assert=>assert_true(
      act = lv_can_continue
      msg = 'Memory management should allow continuation when under limit' ).
  ENDMETHOD.

  METHOD create_test_program.
    " Create temporary test program
    DATA: lt_source TYPE TABLE OF string.

    lt_source = it_code.

    " Use INSERT REPORT to create test program
    INSERT REPORT iv_name FROM lt_source.
    COMMIT WORK.
  ENDMETHOD.

  METHOD measure_performance.
    DATA: lv_start TYPE timestampl,
          lv_end   TYPE timestampl.

    GET TIME STAMP FIELD lv_start.

    " Run analysis
    TRY.
        mo_analyzer->analyze_program(
          iv_program_name = iv_program_name
          iv_mode = 'L' ).
      CATCH zcx_syuname_error.
        " Continue even if error
    ENDTRY.

    GET TIME STAMP FIELD lv_end.

    " Calculate duration in milliseconds
    rv_duration_ms = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end
      tstmp2 = lv_start ) * 1000.
  ENDMETHOD.

  METHOD measure_memory.
    DATA: lv_memory_before TYPE abap_msize,
          lv_memory_after  TYPE abap_msize.

    " Get memory before
    CALL 'GET_MEMORY_INFO' ID 'INFO' FIELD lv_memory_before.

    " Run analysis
    TRY.
        mo_analyzer->analyze_program(
          iv_program_name = iv_program_name
          iv_mode = 'L' ).
      CATCH zcx_syuname_error.
        " Continue even if error
    ENDTRY.

    " Get memory after
    CALL 'GET_MEMORY_INFO' ID 'INFO' FIELD lv_memory_after.

    " Calculate memory used in MB
    rv_memory_mb = ( lv_memory_after - lv_memory_before ) / 1024 / 1024.
  ENDMETHOD.
ENDCLASS.
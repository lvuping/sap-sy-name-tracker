CLASS zcl_syuname_scanner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_scanner.

  PRIVATE SECTION.
    DATA: mt_source_cache TYPE zif_syuname_scanner=>tt_source_lines,
          mt_includes     TYPE zif_syuname_scanner=>tt_includes.

    METHODS: resolve_includes
      IMPORTING
        iv_program    TYPE programm
        iv_recursive  TYPE abap_bool DEFAULT abap_true
        iv_level      TYPE i DEFAULT 0
      RAISING
        zcx_syuname_error.

ENDCLASS.

CLASS zcl_syuname_scanner IMPLEMENTATION.
  METHOD zif_syuname_scanner~read_program.
    DATA: lt_source TYPE TABLE OF string,
          lv_line   TYPE string,
          lv_index  TYPE i.

    CLEAR rt_source.

    " Read the ABAP source code
    READ REPORT iv_program_name INTO lt_source.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>program_not_found
          program_name = iv_program_name.
    ENDIF.

    " Convert to structured format
    LOOP AT lt_source INTO lv_line.
      lv_index = sy-tabix.
      APPEND VALUE #( line_number = lv_index
                      statement = lv_line
                      program = iv_program_name ) TO rt_source.

      " Also add to cache
      APPEND VALUE #( line_number = lv_index
                      statement = lv_line
                      program = iv_program_name ) TO mt_source_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_scanner~get_includes.
    DATA: lt_source TYPE TABLE OF string,
          lv_line   TYPE string,
          lv_include TYPE programm.

    CLEAR rt_includes.

    " Read the program source
    READ REPORT iv_program_name INTO lt_source.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>program_not_found
          program_name = iv_program_name.
    ENDIF.

    " Search for INCLUDE statements
    LOOP AT lt_source INTO lv_line.
      IF lv_line CP 'INCLUDE *' OR lv_line CP '*INCLUDE *'.
        " Extract include name
        FIND REGEX 'INCLUDE\s+(\S+)' IN lv_line
          SUBMATCHES lv_include.

        IF sy-subrc = 0.
          " Remove trailing period if present
          REPLACE ALL OCCURRENCES OF '.' IN lv_include WITH ''.

          APPEND VALUE #( include_name = lv_include
                          parent_prog = iv_program_name
                          level = 0 ) TO rt_includes.

          " Store in cache
          APPEND VALUE #( include_name = lv_include
                          parent_prog = iv_program_name
                          level = 0 ) TO mt_includes.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Resolve nested includes if recursive
    IF iv_recursive = abap_true AND lines( rt_includes ) > 0.
      PERFORM resolve_includes
        USING iv_program_name iv_recursive 1
        RAISING zcx_syuname_error.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_scanner~read_include.
    DATA: lt_source TYPE TABLE OF string,
          lv_line   TYPE string,
          lv_index  TYPE i.

    CLEAR rt_source.

    " Read the include source code
    READ REPORT iv_include_name INTO lt_source.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>include_not_found
          include_name = iv_include_name
          parent_program = iv_parent_prog.
    ENDIF.

    " Convert to structured format
    LOOP AT lt_source INTO lv_line.
      lv_index = sy-tabix.
      APPEND VALUE #( line_number = lv_index
                      statement = lv_line
                      program = iv_include_name ) TO rt_source.

      " Also add to cache with include name
      APPEND VALUE #( line_number = lv_index
                      statement = lv_line
                      program = iv_include_name ) TO mt_source_cache.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_scanner~get_source_cache.
    rt_cache = mt_source_cache.
  ENDMETHOD.

  METHOD zif_syuname_scanner~clear_cache.
    CLEAR: mt_source_cache, mt_includes.
  ENDMETHOD.

  METHOD resolve_includes.
    DATA: lt_nested_includes TYPE zif_syuname_scanner=>tt_includes,
          ls_include         TYPE zif_syuname_scanner=>ty_include_info.

    " Prevent infinite recursion
    IF iv_level > 10.
      RETURN.
    ENDIF.

    LOOP AT mt_includes INTO ls_include
      WHERE parent_prog = iv_program
        AND level = iv_level - 1.

      TRY.
          " Get includes from this include
          lt_nested_includes = zif_syuname_scanner~get_includes(
            iv_program_name = ls_include-include_name
            iv_recursive    = abap_false ).

          " Add nested includes with incremented level
          LOOP AT lt_nested_includes INTO DATA(ls_nested).
            ls_nested-level = iv_level.
            ls_nested-parent_prog = ls_include-include_name.
            APPEND ls_nested TO mt_includes.
          ENDLOOP.

        CATCH zcx_syuname_error.
          " Ignore missing includes
      ENDTRY.
    ENDLOOP.

    " Continue recursion if needed
    IF iv_recursive = abap_true AND lines( lt_nested_includes ) > 0.
      resolve_includes(
        iv_program   = iv_program
        iv_recursive = iv_recursive
        iv_level     = iv_level + 1 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*"* Test class definitions
CLASS ltc_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_syuname_scanner.

    METHODS: setup,
             test_read_program FOR TESTING,
             test_get_includes FOR TESTING,
             test_read_include FOR TESTING,
             test_cache_operations FOR TESTING,
             test_recursive_includes FOR TESTING.
ENDCLASS.

CLASS ltc_scanner IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW zcl_syuname_scanner( ).
  ENDMETHOD.

  METHOD test_read_program.
    DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
          lx_error  TYPE REF TO zcx_syuname_error.

    TRY.
        lt_source = mo_cut->read_program( 'ZSYUNAME_TEST_PROG' ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->if_t100_message~t100key
          exp = zcx_syuname_error=>program_not_found
          msg = 'Wrong exception type' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_includes.
    DATA: lt_includes TYPE zif_syuname_scanner=>tt_includes,
          lx_error    TYPE REF TO zcx_syuname_error.

    TRY.
        lt_includes = mo_cut->get_includes( 'ZSYUNAME_MAIN' ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->if_t100_message~t100key
          exp = zcx_syuname_error=>program_not_found
          msg = 'Wrong exception type' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_read_include.
    DATA: lt_source TYPE zif_syuname_scanner=>tt_source_lines,
          lx_error  TYPE REF TO zcx_syuname_error.

    TRY.
        lt_source = mo_cut->read_include(
          iv_include_name = 'ZSYUNAME_INC'
          iv_parent_prog  = 'ZSYUNAME_MAIN' ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->if_t100_message~t100key
          exp = zcx_syuname_error=>include_not_found
          msg = 'Wrong exception type' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_cache_operations.
    DATA: lt_cache TYPE zif_syuname_scanner=>tt_source_lines.

    lt_cache = mo_cut->get_source_cache( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_cache
      msg = 'Cache should be initially empty' ).

    mo_cut->clear_cache( ).
    lt_cache = mo_cut->get_source_cache( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_cache
      msg = 'Cache should be empty after clear' ).
  ENDMETHOD.

  METHOD test_recursive_includes.
    DATA: lt_includes TYPE zif_syuname_scanner=>tt_includes,
          lx_error    TYPE REF TO zcx_syuname_error.

    TRY.
        lt_includes = mo_cut->get_includes(
          iv_program_name = 'ZSYUNAME_RECURSIVE'
          iv_recursive    = abap_true ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Exception should be raised' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
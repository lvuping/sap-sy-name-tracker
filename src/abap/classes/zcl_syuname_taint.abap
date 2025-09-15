CLASS zcl_syuname_taint DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_taint.

  PRIVATE SECTION.
    DATA: mt_tainted_vars TYPE zif_syuname_taint=>tt_tainted_vars,
          mt_scope_stack  TYPE TABLE OF zif_syuname_taint=>ty_scope_info,
          ms_current_scope TYPE zif_syuname_taint=>ty_scope_info.

    METHODS: get_full_variable_name
      IMPORTING
        iv_variable TYPE string
        iv_scope    TYPE string
      RETURNING
        VALUE(rv_full_name) TYPE string.

ENDCLASS.

CLASS zcl_syuname_taint IMPLEMENTATION.
  METHOD zif_syuname_taint~mark_tainted.
    DATA: ls_tainted TYPE zif_syuname_taint=>ty_tainted_var.

    " Create tainted variable entry
    ls_tainted-name = iv_variable.
    ls_tainted-scope = iv_scope.
    ls_tainted-scope_name = COND #( WHEN iv_scope_name IS INITIAL
                                     THEN iv_scope
                                     ELSE iv_scope_name ).
    ls_tainted-line = iv_line.
    ls_tainted-program = iv_program.
    ls_tainted-path = it_path.
    ls_tainted-confidence = iv_confidence.

    " Add to tainted variables table
    APPEND ls_tainted TO mt_tainted_vars.
  ENDMETHOD.

  METHOD zif_syuname_taint~is_tainted.
    DATA: lv_full_name TYPE string.

    lv_full_name = get_full_variable_name( iv_variable = iv_variable
                                            iv_scope = iv_scope ).

    " Check if variable is tainted
    READ TABLE mt_tainted_vars TRANSPORTING NO FIELDS
      WITH KEY name = iv_variable
               scope = iv_scope.

    rv_tainted = xsdbool( sy-subrc = 0 ).

    " Also check without scope qualification
    IF rv_tainted = abap_false.
      READ TABLE mt_tainted_vars TRANSPORTING NO FIELDS
        WITH KEY name = iv_variable.
      rv_tainted = xsdbool( sy-subrc = 0 ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_taint~get_taint_path.
    DATA: ls_tainted TYPE zif_syuname_taint=>ty_tainted_var.

    CLEAR rt_path.

    " Find the tainted variable
    READ TABLE mt_tainted_vars INTO ls_tainted
      WITH KEY name = iv_variable
               scope = iv_scope.

    IF sy-subrc = 0.
      rt_path = ls_tainted-path.
    ELSE.
      " Try without scope
      READ TABLE mt_tainted_vars INTO ls_tainted
        WITH KEY name = iv_variable.
      IF sy-subrc = 0.
        rt_path = ls_tainted-path.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_taint~propagate_taint.
    DATA: lt_source_path TYPE string_table,
          lt_new_path    TYPE string_table,
          lv_path_step   TYPE string.

    " Get the source variable's taint path
    lt_source_path = zif_syuname_taint~get_taint_path(
      iv_variable = iv_from_var
      iv_scope = iv_scope ).

    IF lines( lt_source_path ) > 0.
      " Build new path
      lt_new_path = lt_source_path.

      " Add the propagation step
      IF iv_operation IS NOT INITIAL.
        lv_path_step = |{ iv_from_var } ->{ iv_operation }-> { iv_to_var }|.
      ELSE.
        lv_path_step = |{ iv_from_var } -> { iv_to_var }|.
      ENDIF.
      APPEND lv_path_step TO lt_new_path.

      " Mark target variable as tainted
      zif_syuname_taint~mark_tainted(
        iv_variable   = iv_to_var
        iv_scope      = iv_scope
        iv_scope_name = iv_scope_name
        iv_line       = iv_line
        iv_program    = iv_program
        it_path       = lt_new_path
        iv_confidence = zif_syuname_taint=>c_confidence-high ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_taint~enter_scope.
    DATA: ls_scope TYPE zif_syuname_taint=>ty_scope_info.

    " Create new scope entry
    ls_scope-scope_type = iv_scope_type.
    ls_scope-scope_name = iv_scope_name.
    ls_scope-start_line = iv_start_line.
    ls_scope-parent_prog = iv_program.

    " Push to scope stack
    APPEND ls_scope TO mt_scope_stack.

    " Update current scope
    ms_current_scope = ls_scope.
  ENDMETHOD.

  METHOD zif_syuname_taint~exit_scope.
    DATA: lv_lines TYPE i.

    lv_lines = lines( mt_scope_stack ).

    IF lv_lines > 0.
      " Pop from scope stack
      DELETE mt_scope_stack INDEX lv_lines.

      " Update current scope to previous
      lv_lines = lines( mt_scope_stack ).
      IF lv_lines > 0.
        READ TABLE mt_scope_stack INTO ms_current_scope INDEX lv_lines.
      ELSE.
        " Back to global scope
        CLEAR ms_current_scope.
        ms_current_scope-scope_type = zif_syuname_taint=>c_scope_type-global.
        ms_current_scope-scope_name = 'GLOBAL'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_taint~get_current_scope.
    rs_scope = ms_current_scope.
  ENDMETHOD.

  METHOD zif_syuname_taint~get_all_tainted.
    rt_tainted = mt_tainted_vars.
  ENDMETHOD.

  METHOD zif_syuname_taint~clear_taint_data.
    CLEAR: mt_tainted_vars, mt_scope_stack, ms_current_scope.
  ENDMETHOD.

  METHOD zif_syuname_taint~set_field_tainted.
    DATA: lv_field_name TYPE string.

    " Create composite field name
    lv_field_name = |{ iv_structure }-{ iv_field }|.

    " Mark the field as tainted
    zif_syuname_taint~mark_tainted(
      iv_variable   = lv_field_name
      iv_scope      = iv_scope
      iv_line       = iv_line
      iv_program    = iv_program
      it_path       = it_path
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    " Also mark the structure itself as potentially tainted
    zif_syuname_taint~mark_tainted(
      iv_variable   = iv_structure
      iv_scope      = iv_scope
      iv_line       = iv_line
      iv_program    = iv_program
      it_path       = it_path
      iv_confidence = zif_syuname_taint=>c_confidence-medium ).
  ENDMETHOD.

  METHOD zif_syuname_taint~is_field_tainted.
    DATA: lv_field_name TYPE string.

    " Create composite field name
    lv_field_name = |{ iv_structure }-{ iv_field }|.

    " Check if field is tainted
    rv_tainted = zif_syuname_taint~is_tainted(
      iv_variable = lv_field_name
      iv_scope = iv_scope ).

    " If not found, check if entire structure is tainted
    IF rv_tainted = abap_false.
      rv_tainted = zif_syuname_taint~is_tainted(
        iv_variable = iv_structure
        iv_scope = iv_scope ).
    ENDIF.
  ENDMETHOD.

  METHOD get_full_variable_name.
    IF iv_scope = zif_syuname_taint=>c_scope_type-global.
      rv_full_name = iv_variable.
    ELSE.
      rv_full_name = |{ iv_scope }:{ iv_variable }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*"* Test class definitions
CLASS ltc_taint DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_syuname_taint.

    METHODS: setup,
             test_mark_tainted FOR TESTING,
             test_is_tainted FOR TESTING,
             test_propagate_taint FOR TESTING,
             test_scope_management FOR TESTING,
             test_field_tainting FOR TESTING,
             test_taint_path FOR TESTING,
             test_clear_data FOR TESTING.
ENDCLASS.

CLASS ltc_taint IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW zcl_syuname_taint( ).
  ENDMETHOD.

  METHOD test_mark_tainted.
    mo_cut->mark_tainted(
      iv_variable   = 'LV_USER'
      iv_scope      = zif_syuname_taint=>c_scope_type-global
      iv_line       = 10
      iv_program    = 'ZTEST'
      it_path       = VALUE #( ( |sy-uname| ) )
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    DATA(lv_is_tainted) = mo_cut->is_tainted(
      iv_variable = 'LV_USER'
      iv_scope    = zif_syuname_taint=>c_scope_type-global ).

    cl_abap_unit_assert=>assert_false(
      act = lv_is_tainted
      msg = 'Variable should not be tainted in stub implementation' ).
  ENDMETHOD.

  METHOD test_is_tainted.
    DATA(lv_result) = mo_cut->is_tainted(
      iv_variable = 'LV_NOT_TAINTED'
      iv_scope    = zif_syuname_taint=>c_scope_type-global ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Non-tainted variable should return false' ).
  ENDMETHOD.

  METHOD test_propagate_taint.
    mo_cut->mark_tainted(
      iv_variable   = 'LV_SOURCE'
      iv_scope      = zif_syuname_taint=>c_scope_type-global
      iv_line       = 5
      iv_program    = 'ZTEST'
      it_path       = VALUE #( ( |sy-uname| ) )
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    mo_cut->propagate_taint(
      iv_from_var   = 'LV_SOURCE'
      iv_to_var     = 'LV_TARGET'
      iv_scope      = zif_syuname_taint=>c_scope_type-global
      iv_line       = 10
      iv_program    = 'ZTEST'
      iv_operation  = 'MOVE' ).

    DATA(lv_is_tainted) = mo_cut->is_tainted(
      iv_variable = 'LV_TARGET'
      iv_scope    = zif_syuname_taint=>c_scope_type-global ).

    cl_abap_unit_assert=>assert_false(
      act = lv_is_tainted
      msg = 'Propagation not yet implemented' ).
  ENDMETHOD.

  METHOD test_scope_management.
    mo_cut->enter_scope(
      iv_scope_type = zif_syuname_taint=>c_scope_type-form
      iv_scope_name = 'TEST_FORM'
      iv_start_line = 100
      iv_program    = 'ZTEST' ).

    DATA(ls_scope) = mo_cut->get_current_scope( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_scope-scope_type
      exp = ''
      msg = 'Scope not yet implemented' ).

    mo_cut->exit_scope( ).
  ENDMETHOD.

  METHOD test_field_tainting.
    mo_cut->set_field_tainted(
      iv_structure = 'LS_DATA'
      iv_field     = 'USERNAME'
      iv_scope     = zif_syuname_taint=>c_scope_type-global
      iv_line      = 20
      iv_program   = 'ZTEST'
      it_path      = VALUE #( ( |sy-uname| ) ( |LS_DATA-USERNAME| ) ) ).

    DATA(lv_is_tainted) = mo_cut->is_field_tainted(
      iv_structure = 'LS_DATA'
      iv_field     = 'USERNAME'
      iv_scope     = zif_syuname_taint=>c_scope_type-global ).

    cl_abap_unit_assert=>assert_false(
      act = lv_is_tainted
      msg = 'Field tainting not yet implemented' ).
  ENDMETHOD.

  METHOD test_taint_path.
    mo_cut->mark_tainted(
      iv_variable   = 'LV_VAR1'
      iv_scope      = zif_syuname_taint=>c_scope_type-global
      iv_line       = 5
      iv_program    = 'ZTEST'
      it_path       = VALUE #( ( |sy-uname| ) ( |LV_VAR1| ) )
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    DATA(lt_path) = mo_cut->get_taint_path(
      iv_variable = 'LV_VAR1'
      iv_scope    = zif_syuname_taint=>c_scope_type-global ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_path
      msg = 'Path retrieval not yet implemented' ).
  ENDMETHOD.

  METHOD test_clear_data.
    mo_cut->mark_tainted(
      iv_variable   = 'LV_TEST'
      iv_scope      = zif_syuname_taint=>c_scope_type-global
      iv_line       = 1
      iv_program    = 'ZTEST'
      it_path       = VALUE #( ( |sy-uname| ) )
      iv_confidence = zif_syuname_taint=>c_confidence-high ).

    mo_cut->clear_taint_data( ).

    DATA(lt_all) = mo_cut->get_all_tainted( ).
    cl_abap_unit_assert=>assert_initial(
      act = lt_all
      msg = 'All data should be cleared' ).
  ENDMETHOD.
ENDCLASS.
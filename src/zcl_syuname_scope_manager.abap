CLASS zcl_syuname_scope_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_scope,
             type       TYPE string,  " FORM, METHOD, FUNCTION, MAIN
             name       TYPE string,  " Name of routine/method
             start_line TYPE i,
             end_line   TYPE i,
             parent     TYPE string,  " Parent scope name
           END OF ty_scope.

    TYPES: tt_scope_stack TYPE TABLE OF ty_scope.

    METHODS: enter_scope
      IMPORTING
        iv_scope_type TYPE string
        iv_scope_name TYPE string
        iv_start_line TYPE i,

      exit_scope
        IMPORTING
          iv_end_line TYPE i,

      get_current_scope
        RETURNING
          VALUE(rv_scope) TYPE string,

      get_scope_path
        RETURNING
          VALUE(rt_path) TYPE string_table,

      is_in_scope
        IMPORTING
          iv_variable   TYPE string
          iv_scope_name TYPE string
        RETURNING
          VALUE(rv_in_scope) TYPE abap_bool,

      clear_all.

  PRIVATE SECTION.
    DATA: mt_scope_stack TYPE tt_scope_stack,
          mv_current_depth TYPE i.

    METHODS: get_full_scope_name
      RETURNING
        VALUE(rv_name) TYPE string.

ENDCLASS.

CLASS zcl_syuname_scope_manager IMPLEMENTATION.
  METHOD enter_scope.
    DATA: ls_scope TYPE ty_scope.

    ls_scope-type = iv_scope_type.
    ls_scope-name = iv_scope_name.
    ls_scope-start_line = iv_start_line.

    " Set parent scope
    IF lines( mt_scope_stack ) > 0.
      READ TABLE mt_scope_stack INTO DATA(ls_parent) INDEX lines( mt_scope_stack ).
      ls_scope-parent = ls_parent-name.
    ELSE.
      ls_scope-parent = 'GLOBAL'.
    ENDIF.

    " Push to stack
    APPEND ls_scope TO mt_scope_stack.
    ADD 1 TO mv_current_depth.
  ENDMETHOD.

  METHOD exit_scope.
    CHECK lines( mt_scope_stack ) > 0.

    " Update end line of current scope
    DATA(lv_index) = lines( mt_scope_stack ).
    READ TABLE mt_scope_stack ASSIGNING FIELD-SYMBOL(<ls_scope>) INDEX lv_index.
    IF sy-subrc = 0.
      <ls_scope>-end_line = iv_end_line.
    ENDIF.

    " Pop from stack
    DELETE mt_scope_stack INDEX lv_index.
    SUBTRACT 1 FROM mv_current_depth.
  ENDMETHOD.

  METHOD get_current_scope.
    IF lines( mt_scope_stack ) = 0.
      rv_scope = 'GLOBAL'.
    ELSE.
      rv_scope = get_full_scope_name( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_scope_path.
    CLEAR rt_path.

    " Build path from root to current
    LOOP AT mt_scope_stack INTO DATA(ls_scope).
      APPEND ls_scope-name TO rt_path.
    ENDLOOP.

    IF lines( rt_path ) = 0.
      APPEND 'GLOBAL' TO rt_path.
    ENDIF.
  ENDMETHOD.

  METHOD is_in_scope.
    " Check if variable is accessible in given scope
    DATA: lv_current_scope TYPE string.

    lv_current_scope = get_current_scope( ).

    " Global variables are accessible everywhere
    IF iv_scope_name = 'GLOBAL'.
      rv_in_scope = abap_true.
      RETURN.
    ENDIF.

    " Check if variable scope matches current or parent scopes
    LOOP AT mt_scope_stack INTO DATA(ls_scope).
      IF ls_scope-name = iv_scope_name.
        rv_in_scope = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_in_scope = abap_false.
  ENDMETHOD.

  METHOD clear_all.
    CLEAR: mt_scope_stack, mv_current_depth.
  ENDMETHOD.

  METHOD get_full_scope_name.
    DATA: lt_names TYPE string_table.

    LOOP AT mt_scope_stack INTO DATA(ls_scope).
      APPEND ls_scope-name TO lt_names.
    ENDLOOP.

    CONCATENATE LINES OF lt_names INTO rv_name SEPARATED BY '/'.
  ENDMETHOD.
ENDCLASS.
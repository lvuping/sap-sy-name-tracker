CLASS zcl_syuname_fieldsymbol_track DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_field_symbol,
             name       TYPE string,
             assigned_to TYPE string,
             line_number TYPE i,
             confidence  TYPE string,
             is_tainted  TYPE abap_bool,
             taint_source TYPE string,
           END OF ty_field_symbol.

    TYPES: tt_field_symbols TYPE TABLE OF ty_field_symbol.

    METHODS: track_field_symbol_assignment
      IMPORTING
        iv_statement    TYPE string
        iv_line_number  TYPE i
        io_taint_engine TYPE REF TO zif_syuname_taint
      RETURNING
        VALUE(rv_handled) TYPE abap_bool,

      analyze_assign_statement
        IMPORTING
          iv_statement TYPE string
        EXPORTING
          ev_field_symbol TYPE string
          ev_assigned_var TYPE string
          ev_is_assign    TYPE abap_bool,

      handle_field_symbol_in_statement
        IMPORTING
          iv_statement    TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint,

      get_field_symbol_status
        IMPORTING
          iv_fs_name TYPE string
        RETURNING
          VALUE(rs_status) TYPE ty_field_symbol,

      is_field_symbol_tainted
        IMPORTING
          iv_fs_name TYPE string
        RETURNING
          VALUE(rv_tainted) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_field_symbols TYPE tt_field_symbols.

    METHODS: extract_field_symbol_name
      IMPORTING
        iv_statement TYPE string
      RETURNING
        VALUE(rv_name) TYPE string,

      update_field_symbol_registry
        IMPORTING
          is_field_symbol TYPE ty_field_symbol,

      propagate_through_field_symbol
        IMPORTING
          iv_fs_name      TYPE string
          iv_target_var   TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint.

ENDCLASS.

CLASS zcl_syuname_fieldsymbol_track IMPLEMENTATION.
  METHOD track_field_symbol_assignment.
    DATA: lv_field_symbol TYPE string,
          lv_assigned_var TYPE string,
          lv_is_assign    TYPE abap_bool.

    rv_handled = abap_false.

    " Analyze the statement
    analyze_assign_statement(
      EXPORTING
        iv_statement    = iv_statement
      IMPORTING
        ev_field_symbol = lv_field_symbol
        ev_assigned_var = lv_assigned_var
        ev_is_assign    = lv_is_assign ).

    IF lv_is_assign = abap_true.
      DATA: ls_field_symbol TYPE ty_field_symbol.

      ls_field_symbol-name = lv_field_symbol.
      ls_field_symbol-assigned_to = lv_assigned_var.
      ls_field_symbol-line_number = iv_line_number.
      ls_field_symbol-confidence = 'MEDIUM'.  " Field-symbols always MEDIUM confidence

      " Check if assigned variable is tainted
      IF io_taint_engine->is_tainted(
           iv_variable = lv_assigned_var
           iv_scope    = 'CURRENT' ).

        ls_field_symbol-is_tainted = abap_true.
        ls_field_symbol-taint_source = lv_assigned_var.

        " Mark field-symbol as tainted with MEDIUM confidence
        DATA: lt_path TYPE string_table.
        APPEND |Field-Symbol Assignment: { lv_assigned_var }| TO lt_path.
        APPEND |  -> { lv_field_symbol } (MEDIUM confidence)| TO lt_path.

        io_taint_engine->mark_tainted(
          iv_variable = lv_field_symbol
          iv_scope    = 'CURRENT'
          iv_line     = iv_line_number
          it_path     = lt_path ).

        " Set confidence level
        io_taint_engine->set_confidence(
          iv_variable   = lv_field_symbol
          iv_confidence = 'MEDIUM' ).
      ENDIF.

      " Update registry
      update_field_symbol_registry( ls_field_symbol ).
      rv_handled = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD analyze_assign_statement.
    DATA: lv_statement TYPE string.

    CLEAR: ev_field_symbol, ev_assigned_var, ev_is_assign.
    lv_statement = to_upper( iv_statement ).

    " Check for ASSIGN pattern
    IF lv_statement CS 'ASSIGN'.
      ev_is_assign = abap_true.

      " Pattern 1: ASSIGN variable TO <fs>
      FIND REGEX 'ASSIGN\s+(\S+)\s+TO\s+<(\S+)>'
        IN lv_statement
        SUBMATCHES ev_assigned_var ev_field_symbol.

      IF sy-subrc <> 0.
        " Pattern 2: ASSIGN (variable) TO <fs>
        FIND REGEX 'ASSIGN\s+\((\S+)\)\s+TO\s+<(\S+)>'
          IN lv_statement
          SUBMATCHES ev_assigned_var ev_field_symbol.
      ENDIF.

      IF sy-subrc <> 0.
        " Pattern 3: ASSIGN COMPONENT OF structure TO <fs>
        FIND REGEX 'ASSIGN\s+COMPONENT\s+\S+\s+OF\s+(\S+)\s+TO\s+<(\S+)>'
          IN lv_statement
          SUBMATCHES ev_assigned_var ev_field_symbol.
      ENDIF.

      " Clean up names
      REPLACE ALL OCCURRENCES OF '.' IN ev_field_symbol WITH ''.
      REPLACE ALL OCCURRENCES OF '>' IN ev_field_symbol WITH ''.
      REPLACE ALL OCCURRENCES OF '<' IN ev_field_symbol WITH ''.
    ENDIF.
  ENDMETHOD.

  METHOD handle_field_symbol_in_statement.
    DATA: lv_statement TYPE string.

    lv_statement = to_upper( iv_statement ).

    " Check if statement contains field-symbols
    IF lv_statement CS '<' AND lv_statement CS '>'.
      " Extract field-symbol name
      DATA(lv_fs_name) = extract_field_symbol_name( lv_statement ).

      IF lv_fs_name IS NOT INITIAL.
        " Check if this field-symbol is tainted
        IF is_field_symbol_tainted( lv_fs_name ).
          " Look for assignments FROM field-symbol
          IF lv_statement CS '=' AND lv_statement CS lv_fs_name.
            " Pattern: variable = <fs>
            DATA: lv_target_var TYPE string.
            FIND REGEX '(\S+)\s*=\s*<' && lv_fs_name && '>'
              IN lv_statement
              SUBMATCHES lv_target_var.

            IF sy-subrc = 0.
              propagate_through_field_symbol(
                iv_fs_name      = lv_fs_name
                iv_target_var   = lv_target_var
                iv_line_number  = iv_line_number
                io_taint_engine = io_taint_engine ).
            ENDIF.
          ENDIF.

          " Look for field-symbol in DML operations
          IF lv_statement CS 'INSERT' OR
             lv_statement CS 'UPDATE' OR
             lv_statement CS 'MODIFY'.
            " Add finding with MEDIUM confidence
            DATA: lt_path TYPE string_table.
            APPEND |DML with Field-Symbol <{ lv_fs_name }> (MEDIUM confidence)| TO lt_path.

            io_taint_engine->add_finding(
              iv_type       = 'DML_FIELD_SYMBOL'
              iv_confidence = 'MEDIUM'
              iv_line       = iv_line_number
              it_path       = lt_path ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_field_symbol_status.
    CLEAR rs_status.

    READ TABLE mt_field_symbols INTO rs_status
      WITH KEY name = iv_fs_name.
  ENDMETHOD.

  METHOD is_field_symbol_tainted.
    rv_tainted = abap_false.

    READ TABLE mt_field_symbols TRANSPORTING NO FIELDS
      WITH KEY name = iv_fs_name
               is_tainted = abap_true.

    IF sy-subrc = 0.
      rv_tainted = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD extract_field_symbol_name.
    CLEAR rv_name.

    " Extract field-symbol name from statement
    FIND REGEX '<(\S+)>'
      IN iv_statement
      SUBMATCHES rv_name.

    " Remove brackets if still present
    REPLACE ALL OCCURRENCES OF '<' IN rv_name WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN rv_name WITH ''.
  ENDMETHOD.

  METHOD update_field_symbol_registry.
    " Check if field-symbol already exists
    READ TABLE mt_field_symbols TRANSPORTING NO FIELDS
      WITH KEY name = is_field_symbol-name.

    IF sy-subrc = 0.
      " Update existing entry
      MODIFY mt_field_symbols FROM is_field_symbol
        TRANSPORTING assigned_to line_number confidence is_tainted taint_source
        WHERE name = is_field_symbol-name.
    ELSE.
      " Add new entry
      APPEND is_field_symbol TO mt_field_symbols.
    ENDIF.
  ENDMETHOD.

  METHOD propagate_through_field_symbol.
    DATA: lt_path TYPE string_table.

    " Build taint path
    APPEND |Field-Symbol Propagation: <{ iv_fs_name }>| TO lt_path.
    APPEND |  -> { iv_target_var } (MEDIUM confidence)| TO lt_path.

    " Propagate taint with MEDIUM confidence
    io_taint_engine->propagate_taint(
      iv_from_var = |<{ iv_fs_name }>|
      iv_to_var   = iv_target_var
      iv_scope    = 'CURRENT'
      iv_line     = iv_line_number ).

    " Set confidence level
    io_taint_engine->set_confidence(
      iv_variable   = iv_target_var
      iv_confidence = 'MEDIUM' ).
  ENDMETHOD.
ENDCLASS.
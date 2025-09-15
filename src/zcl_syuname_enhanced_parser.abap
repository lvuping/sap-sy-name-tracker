CLASS zcl_syuname_enhanced_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_concatenate_parts,
             variable_name TYPE string,
             is_tainted    TYPE abap_bool,
             position      TYPE i,
           END OF ty_concatenate_parts.

    TYPES: tt_concatenate_parts TYPE TABLE OF ty_concatenate_parts.

    TYPES: BEGIN OF ty_loop_variable,
             loop_var    TYPE string,
             table_name  TYPE string,
             is_tainted  TYPE abap_bool,
             field_assignments TYPE string_table,
           END OF ty_loop_variable.

    TYPES: tt_loop_variables TYPE TABLE OF ty_loop_variable.

    METHODS: parse_concatenate_statement
      IMPORTING
        iv_statement    TYPE string
        iv_line_number  TYPE i
        io_taint_engine TYPE REF TO zif_syuname_taint
      RETURNING
        VALUE(rv_handled) TYPE abap_bool,

      handle_loop_at_statement
        IMPORTING
          iv_statement    TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint
        RETURNING
          VALUE(rv_handled) TYPE abap_bool,

      track_call_method_taint
        IMPORTING
          iv_statement    TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint
        RETURNING
          VALUE(rv_handled) TYPE abap_bool,

      parse_perform_statement
        IMPORTING
          iv_statement    TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint
        RETURNING
          VALUE(rv_handled) TYPE abap_bool,

      detect_dynamic_sql
        IMPORTING
          iv_statement    TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint
        RETURNING
          VALUE(rv_is_dynamic) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_loop_context TYPE tt_loop_variables.

    METHODS: extract_concatenate_variables
      IMPORTING
        iv_statement TYPE string
      RETURNING
        VALUE(rt_variables) TYPE tt_concatenate_parts,

      extract_loop_context
        IMPORTING
          iv_statement TYPE string
        EXPORTING
          ev_loop_var   TYPE string
          ev_table_name TYPE string,

      extract_method_parameters
        IMPORTING
          iv_statement TYPE string
        EXPORTING
          et_importing TYPE string_table
          et_exporting TYPE string_table
          et_changing  TYPE string_table,

      extract_perform_parameters
        IMPORTING
          iv_statement TYPE string
        EXPORTING
          ev_form_name TYPE string
          et_using     TYPE string_table
          et_changing  TYPE string_table,

      propagate_concatenate_taint
        IMPORTING
          it_parts        TYPE tt_concatenate_parts
          iv_target       TYPE string
          iv_line_number  TYPE i
          io_taint_engine TYPE REF TO zif_syuname_taint.

ENDCLASS.

CLASS zcl_syuname_enhanced_parser IMPLEMENTATION.
  METHOD parse_concatenate_statement.
    DATA: lv_statement TYPE string,
          lv_target    TYPE string.

    rv_handled = abap_false.
    lv_statement = to_upper( iv_statement ).

    IF lv_statement CS 'CONCATENATE'.
      " Extract target variable
      FIND REGEX 'CONCATENATE\s+.+\s+INTO\s+(\S+)'
        IN lv_statement
        SUBMATCHES lv_target.

      IF sy-subrc = 0.
        " Extract all source variables
        DATA(lt_parts) = extract_concatenate_variables( lv_statement ).

        " Check if any part is tainted
        DATA(lv_any_tainted) = abap_false.
        LOOP AT lt_parts INTO DATA(ls_part).
          IF io_taint_engine->is_tainted(
               iv_variable = ls_part-variable_name
               iv_scope    = 'CURRENT' ).
            ls_part-is_tainted = abap_true.
            lv_any_tainted = abap_true.
            MODIFY lt_parts FROM ls_part.
          ENDIF.
        ENDLOOP.

        " If any source is tainted, target becomes tainted
        IF lv_any_tainted = abap_true.
          propagate_concatenate_taint(
            it_parts        = lt_parts
            iv_target       = lv_target
            iv_line_number  = iv_line_number
            io_taint_engine = io_taint_engine ).

          rv_handled = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_loop_at_statement.
    DATA: lv_statement TYPE string,
          lv_loop_var  TYPE string,
          lv_table_name TYPE string.

    rv_handled = abap_false.
    lv_statement = to_upper( iv_statement ).

    IF lv_statement CS 'LOOP AT'.
      extract_loop_context(
        EXPORTING
          iv_statement  = lv_statement
        IMPORTING
          ev_loop_var   = lv_loop_var
          ev_table_name = lv_table_name ).

      IF lv_loop_var IS NOT INITIAL.
        DATA: ls_loop_context TYPE ty_loop_variable.

        ls_loop_context-loop_var = lv_loop_var.
        ls_loop_context-table_name = lv_table_name.

        " Check if table contains tainted data
        IF io_taint_engine->is_tainted(
             iv_variable = lv_table_name
             iv_scope    = 'CURRENT' ).

          ls_loop_context-is_tainted = abap_true.

          " Mark loop variable as tainted
          DATA: lt_path TYPE string_table.
          APPEND |LOOP AT { lv_table_name } (tainted)| TO lt_path.
          APPEND |  -> { lv_loop_var }| TO lt_path.

          io_taint_engine->mark_tainted(
            iv_variable = lv_loop_var
            iv_scope    = 'CURRENT'
            iv_line     = iv_line_number
            it_path     = lt_path ).
        ENDIF.

        " Add to loop context for tracking
        APPEND ls_loop_context TO mt_loop_context.
        rv_handled = abap_true.
      ENDIF.
    ELSEIF lv_statement CS 'ENDLOOP'.
      " Remove last loop context
      IF lines( mt_loop_context ) > 0.
        DELETE mt_loop_context INDEX lines( mt_loop_context ).
      ENDIF.
      rv_handled = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD track_call_method_taint.
    DATA: lv_statement TYPE string,
          lt_importing TYPE string_table,
          lt_exporting TYPE string_table,
          lt_changing  TYPE string_table.

    rv_handled = abap_false.
    lv_statement = to_upper( iv_statement ).

    IF lv_statement CS 'CALL METHOD' OR lv_statement CS '->'.
      extract_method_parameters(
        EXPORTING
          iv_statement = lv_statement
        IMPORTING
          et_importing = lt_importing
          et_exporting = lt_exporting
          et_changing  = lt_changing ).

      " Check EXPORTING parameters for taint
      LOOP AT lt_exporting INTO DATA(lv_export_param).
        IF io_taint_engine->is_tainted(
             iv_variable = lv_export_param
             iv_scope    = 'CURRENT' ).

          " Find corresponding IMPORTING parameter
          READ TABLE lt_importing INTO DATA(lv_import_param) INDEX sy-tabix.
          IF sy-subrc = 0.
            " Propagate taint
            DATA: lt_path TYPE string_table.
            APPEND |CALL METHOD: { lv_export_param } (tainted)| TO lt_path.
            APPEND |  -> { lv_import_param }| TO lt_path.

            io_taint_engine->propagate_taint(
              iv_from_var = lv_export_param
              iv_to_var   = lv_import_param
              iv_scope    = 'CURRENT'
              iv_line     = iv_line_number ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      " CHANGING parameters are both input and output
      LOOP AT lt_changing INTO DATA(lv_changing_param).
        IF io_taint_engine->is_tainted(
             iv_variable = lv_changing_param
             iv_scope    = 'CURRENT' ).

          " Add note about CHANGING parameter
          io_taint_engine->add_note(
            iv_variable = lv_changing_param
            iv_note     = 'Tainted via CHANGING parameter' ).
        ENDIF.
      ENDLOOP.

      rv_handled = xsdbool( lines( lt_importing ) > 0 OR
                            lines( lt_exporting ) > 0 OR
                            lines( lt_changing ) > 0 ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_perform_statement.
    DATA: lv_statement TYPE string,
          lv_form_name TYPE string,
          lt_using     TYPE string_table,
          lt_changing  TYPE string_table.

    rv_handled = abap_false.
    lv_statement = to_upper( iv_statement ).

    IF lv_statement CS 'PERFORM'.
      extract_perform_parameters(
        EXPORTING
          iv_statement = lv_statement
        IMPORTING
          ev_form_name = lv_form_name
          et_using     = lt_using
          et_changing  = lt_changing ).

      IF lv_form_name IS NOT INITIAL.
        " Check USING parameters for taint
        LOOP AT lt_using INTO DATA(lv_using_param).
          IF io_taint_engine->is_tainted(
               iv_variable = lv_using_param
               iv_scope    = 'CURRENT' ).

            " Mark form routine as receiving tainted data
            DATA: lt_path TYPE string_table.
            APPEND |PERFORM { lv_form_name } USING { lv_using_param } (tainted)| TO lt_path.

            io_taint_engine->add_finding(
              iv_type       = 'PERFORM_TAINTED'
              iv_confidence = 'HIGH'
              iv_line       = iv_line_number
              it_path       = lt_path ).
          ENDIF.
        ENDLOOP.

        " CHANGING parameters
        LOOP AT lt_changing INTO DATA(lv_changing_param).
          IF io_taint_engine->is_tainted(
               iv_variable = lv_changing_param
               iv_scope    = 'CURRENT' ).

            io_taint_engine->add_note(
              iv_variable = lv_changing_param
              iv_note     = |Tainted in PERFORM { lv_form_name }| ).
          ENDIF.
        ENDLOOP.

        rv_handled = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD detect_dynamic_sql.
    DATA: lv_statement TYPE string.

    rv_is_dynamic = abap_false.
    lv_statement = to_upper( iv_statement ).

    " Check for dynamic SQL patterns
    IF lv_statement CS 'EXEC SQL' OR
       lv_statement CS 'ADBC' OR
       lv_statement CS 'CL_SQL_STATEMENT' OR
       lv_statement CS 'EXECUTE IMMEDIATE'.

      rv_is_dynamic = abap_true.

      " Check if statement contains tainted variables
      DATA: lv_pattern TYPE string,
            lv_variable TYPE string.

      " Look for variable references in SQL
      FIND ALL OCCURRENCES OF REGEX '\:(\w+)' IN lv_statement
        RESULTS DATA(lt_matches).

      LOOP AT lt_matches INTO DATA(ls_match).
        " Extract variable name
        lv_variable = lv_statement+ls_match-offset(ls_match-length).
        REPLACE ':' IN lv_variable WITH ''.

        IF io_taint_engine->is_tainted(
             iv_variable = lv_variable
             iv_scope    = 'CURRENT' ).

          " Critical finding - SQL injection risk
          DATA: lt_path TYPE string_table.
          APPEND |DYNAMIC SQL with tainted variable: { lv_variable }| TO lt_path.
          APPEND |  ** CRITICAL: Potential SQL injection risk **| TO lt_path.

          io_taint_engine->add_finding(
            iv_type       = 'DYNAMIC_SQL_TAINTED'
            iv_confidence = 'CRITICAL'
            iv_line       = iv_line_number
            it_path       = lt_path ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD extract_concatenate_variables.
    DATA: lv_parts_section TYPE string,
          lt_parts TYPE string_table,
          lv_position TYPE i.

    " Extract the part between CONCATENATE and INTO
    FIND REGEX 'CONCATENATE\s+(.+)\s+INTO'
      IN iv_statement
      SUBMATCHES lv_parts_section.

    IF sy-subrc = 0.
      " Split by spaces
      SPLIT lv_parts_section AT space INTO TABLE lt_parts.

      " Process each part
      LOOP AT lt_parts INTO DATA(lv_part).
        CHECK lv_part IS NOT INITIAL AND
              lv_part <> 'SEPARATED' AND
              lv_part <> 'BY'.

        ADD 1 TO lv_position.
        APPEND VALUE #(
          variable_name = lv_part
          is_tainted    = abap_false
          position      = lv_position
        ) TO rt_variables.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD extract_loop_context.
    CLEAR: ev_loop_var, ev_table_name.

    " Pattern: LOOP AT table INTO variable
    FIND REGEX 'LOOP\s+AT\s+(\S+)\s+INTO\s+(\S+)'
      IN iv_statement
      SUBMATCHES ev_table_name ev_loop_var.

    IF sy-subrc <> 0.
      " Pattern: LOOP AT table ASSIGNING <fs>
      FIND REGEX 'LOOP\s+AT\s+(\S+)\s+ASSIGNING\s+<(\S+)>'
        IN iv_statement
        SUBMATCHES ev_table_name ev_loop_var.

      IF sy-subrc = 0.
        ev_loop_var = |<{ ev_loop_var }>|.
      ENDIF.
    ENDIF.

    " Clean up names
    REPLACE ALL OCCURRENCES OF '.' IN ev_loop_var WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN ev_table_name WITH ''.
  ENDMETHOD.

  METHOD extract_method_parameters.
    CLEAR: et_importing, et_exporting, et_changing.

    " Extract IMPORTING parameters
    FIND REGEX 'IMPORTING\s+([^A-Z]+)'
      IN iv_statement
      SUBMATCHES DATA(lv_importing).
    IF sy-subrc = 0.
      SPLIT lv_importing AT space INTO TABLE et_importing.
    ENDIF.

    " Extract EXPORTING parameters
    FIND REGEX 'EXPORTING\s+([^A-Z]+)'
      IN iv_statement
      SUBMATCHES DATA(lv_exporting).
    IF sy-subrc = 0.
      SPLIT lv_exporting AT space INTO TABLE et_exporting.
    ENDIF.

    " Extract CHANGING parameters
    FIND REGEX 'CHANGING\s+([^A-Z]+)'
      IN iv_statement
      SUBMATCHES DATA(lv_changing).
    IF sy-subrc = 0.
      SPLIT lv_changing AT space INTO TABLE et_changing.
    ENDIF.
  ENDMETHOD.

  METHOD extract_perform_parameters.
    CLEAR: ev_form_name, et_using, et_changing.

    " Extract form name
    FIND REGEX 'PERFORM\s+(\S+)'
      IN iv_statement
      SUBMATCHES ev_form_name.

    " Extract USING parameters
    FIND REGEX 'USING\s+([^\.]+)'
      IN iv_statement
      SUBMATCHES DATA(lv_using).
    IF sy-subrc = 0.
      SPLIT lv_using AT space INTO TABLE et_using.
    ENDIF.

    " Extract CHANGING parameters
    FIND REGEX 'CHANGING\s+([^\.]+)'
      IN iv_statement
      SUBMATCHES DATA(lv_changing).
    IF sy-subrc = 0.
      SPLIT lv_changing AT space INTO TABLE et_changing.
    ENDIF.
  ENDMETHOD.

  METHOD propagate_concatenate_taint.
    DATA: lt_path TYPE string_table,
          lt_tainted_parts TYPE string_table.

    " Build list of tainted parts
    LOOP AT it_parts INTO DATA(ls_part) WHERE is_tainted = abap_true.
      APPEND ls_part-variable_name TO lt_tainted_parts.
    ENDLOOP.

    " Build taint path
    APPEND |CONCATENATE with tainted parts:| TO lt_path.
    LOOP AT lt_tainted_parts INTO DATA(lv_part).
      APPEND |  - { lv_part }| TO lt_path.
    ENDLOOP.
    APPEND |  -> { iv_target }| TO lt_path.

    " Propagate taint with MEDIUM confidence (concatenation)
    io_taint_engine->mark_tainted(
      iv_variable = iv_target
      iv_scope    = 'CURRENT'
      iv_line     = iv_line_number
      it_path     = lt_path ).

    io_taint_engine->set_confidence(
      iv_variable   = iv_target
      iv_confidence = 'MEDIUM' ).
  ENDMETHOD.
ENDCLASS.
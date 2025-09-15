CLASS zcl_syuname_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_syuname_parser.

  PRIVATE SECTION.
    DATA: mt_tokens     TYPE zif_syuname_parser=>tt_tokens,
          mt_statements TYPE zif_syuname_parser=>tt_statements.

    METHODS: is_dml_statement
      IMPORTING
        it_tokens TYPE zif_syuname_parser=>tt_tokens
        is_statement TYPE zif_syuname_parser=>ty_statement
      RETURNING
        VALUE(rv_is_dml) TYPE abap_bool,

      is_call_statement
        IMPORTING
          it_tokens TYPE zif_syuname_parser=>tt_tokens
          is_statement TYPE zif_syuname_parser=>ty_statement
        RETURNING
          VALUE(rv_is_call) TYPE abap_bool,

      is_assignment_statement
        IMPORTING
          it_tokens TYPE zif_syuname_parser=>tt_tokens
          is_statement TYPE zif_syuname_parser=>ty_statement
        RETURNING
          VALUE(rv_is_assignment) TYPE abap_bool.

ENDCLASS.

CLASS zcl_syuname_parser IMPLEMENTATION.
  METHOD zif_syuname_parser~parse_dml_statement.
    DATA: lv_idx TYPE i.

    CLEAR rs_dml.
    rs_dml-line_number = iv_line.

    " Check first token for DML operation
    READ TABLE it_tokens INTO DATA(ls_token) INDEX is_statement-from.
    IF sy-subrc = 0.
      CASE ls_token-str.
        WHEN 'INSERT'.
          rs_dml-operation = zif_syuname_parser=>c_operation-insert.
        WHEN 'UPDATE'.
          rs_dml-operation = zif_syuname_parser=>c_operation-update.
        WHEN 'DELETE'.
          rs_dml-operation = zif_syuname_parser=>c_operation-delete.
        WHEN 'MODIFY'.
          rs_dml-operation = zif_syuname_parser=>c_operation-modify.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDIF.

    " Extract table name
    rs_dml-table_name = extract_table_name(
      it_tokens = it_tokens
      is_statement = is_statement
      iv_operation = rs_dml-operation ).

    " Check for dynamic table
    LOOP AT it_tokens INTO ls_token FROM is_statement-from TO is_statement-to.
      IF ls_token-str = '(' OR ls_token-str = 'TABLE'.
        rs_dml-is_dynamic = abap_true.
      ENDIF.
      " Collect source variables
      IF ls_token-type = 'I'. " Identifier
        APPEND ls_token-str TO rs_dml-source_vars.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_parser~parse_call_statement.
    DATA: lv_idx TYPE i.

    CLEAR rs_call.
    rs_call-line_number = iv_line.

    " Identify call type
    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      CASE ls_token-str.
        WHEN 'CALL'.
          " Check next token
          lv_idx = sy-tabix + 1.
          READ TABLE it_tokens INTO DATA(ls_next) INDEX lv_idx.
          IF sy-subrc = 0.
            CASE ls_next-str.
              WHEN 'FUNCTION'.
                rs_call-call_type = zif_syuname_parser=>c_call_type-function.
              WHEN 'METHOD'.
                rs_call-call_type = zif_syuname_parser=>c_call_type-method.
            ENDCASE.
          ENDIF.
        WHEN 'PERFORM'.
          rs_call-call_type = zif_syuname_parser=>c_call_type-form.
        WHEN 'SUBMIT'.
          rs_call-call_type = zif_syuname_parser=>c_call_type-submit.
      ENDCASE.

      " Extract module name (literal or variable after call type)
      IF ls_token-type = 'L'. " Literal
        rs_call-module_name = ls_token-str.
        REPLACE ALL OCCURRENCES OF '''' IN rs_call-module_name WITH ''.
      ENDIF.

      " Check for RFC
      IF ls_token-str = 'DESTINATION'.
        rs_call-is_rfc = abap_true.
        " Get destination
        lv_idx = sy-tabix + 1.
        READ TABLE it_tokens INTO ls_next INDEX lv_idx.
        IF sy-subrc = 0.
          rs_call-destination = ls_next-str.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Extract parameters
    rs_call-parameters = extract_parameters(
      it_tokens = it_tokens
      is_statement = is_statement
      iv_call_type = rs_call-call_type ).
  ENDMETHOD.

  METHOD zif_syuname_parser~parse_assignment.
    DATA: lv_idx TYPE i,
          lv_found_eq TYPE abap_bool.

    CLEAR rs_assign.
    rs_assign-line_number = iv_line.

    " Find assignment operator
    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      CASE ls_token-str.
        WHEN '='.
          lv_found_eq = abap_true.
          rs_assign-assign_type = 'MOVE'.
        WHEN 'MOVE'.
          rs_assign-assign_type = 'MOVE'.
        WHEN 'MOVE-CORRESPONDING'.
          rs_assign-assign_type = 'MOVE-CORRESPONDING'.
        WHEN 'ASSIGN'.
          rs_assign-assign_type = 'FIELD-SYMBOL'.
      ENDCASE.

      " Before '=' is target, after is source
      IF lv_found_eq = abap_false AND ls_token-type = 'I'.
        IF rs_assign-target_var IS INITIAL.
          rs_assign-target_var = ls_token-str.
        ENDIF.
      ELSEIF lv_found_eq = abap_true AND ls_token-type = 'I'.
        IF rs_assign-source_var IS INITIAL.
          rs_assign-source_var = ls_token-str.
        ENDIF.
      ENDIF.

      " Check for structure component
      IF ls_token-str CS '-'.
        rs_assign-is_structure = abap_true.
        SPLIT ls_token-str AT '-' INTO DATA(lv_struct) rs_assign-field_name.
      ENDIF.
    ENDLOOP.

    " Handle MOVE statement
    IF rs_assign-assign_type = 'MOVE' AND lv_found_eq = abap_false.
      " MOVE source TO target
      LOOP AT it_tokens INTO ls_token FROM is_statement-from TO is_statement-to.
        IF ls_token-str = 'TO'.
          lv_found_eq = abap_true.
        ELSEIF ls_token-type = 'I'.
          IF lv_found_eq = abap_false.
            rs_assign-source_var = ls_token-str.
          ELSE.
            rs_assign-target_var = ls_token-str.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_parser~tokenize_source.
    DATA: lt_source_lines TYPE TABLE OF string,
          lt_tokens_temp  TYPE TABLE OF stokesx,
          lt_stmts_temp   TYPE TABLE OF sstmnt.

    CLEAR: et_tokens, et_statements.

    " Convert source lines to string table
    LOOP AT it_source INTO DATA(ls_source).
      APPEND ls_source-statement TO lt_source_lines.
    ENDLOOP.

    " Use SCAN ABAP-SOURCE to tokenize
    SCAN ABAP-SOURCE lt_source_lines
         TOKENS INTO lt_tokens_temp
         STATEMENTS INTO lt_stmts_temp.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_syuname_error
        EXPORTING
          textid = zcx_syuname_error=>parse_error
          statement = 'Failed to tokenize source code'.
    ENDIF.

    " Convert to our format
    LOOP AT lt_tokens_temp INTO DATA(ls_token_temp).
      APPEND VALUE #( str = ls_token_temp-str
                      type = ls_token_temp-type
                      row = ls_token_temp-row
                      col = ls_token_temp-col ) TO et_tokens.
    ENDLOOP.

    LOOP AT lt_stmts_temp INTO DATA(ls_stmt_temp).
      APPEND VALUE #( from = ls_stmt_temp-from
                      to = ls_stmt_temp-to
                      type = CONV #( ls_stmt_temp-type ) ) TO et_statements.
    ENDLOOP.

    " Store for later use
    mt_tokens = et_tokens.
    mt_statements = et_statements.
  ENDMETHOD.

  METHOD zif_syuname_parser~analyze_statement_type.
    DATA: ls_first_token TYPE zif_syuname_parser=>ty_token.

    rv_type = 'UNKNOWN'.

    " Get first token of statement
    READ TABLE it_tokens INTO ls_first_token INDEX is_statement-from.
    IF sy-subrc = 0.
      CASE ls_first_token-str.
        WHEN 'INSERT' OR 'UPDATE' OR 'DELETE' OR 'MODIFY'.
          rv_type = 'DML'.
        WHEN 'CALL' OR 'PERFORM' OR 'SUBMIT'.
          rv_type = 'CALL'.
        WHEN 'MOVE' OR 'ASSIGN'.
          rv_type = 'ASSIGNMENT'.
        WHEN OTHERS.
          " Check for assignment with '='
          LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
            IF ls_token-str = '='.
              rv_type = 'ASSIGNMENT'.
              EXIT.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD zif_syuname_parser~extract_table_name.
    DATA: lv_next_is_table TYPE abap_bool.

    CLEAR rv_table.

    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      " After operation keyword, next identifier is usually table
      IF ls_token-str = iv_operation.
        lv_next_is_table = abap_true.
      ELSEIF lv_next_is_table = abap_true AND ls_token-type = 'I'.
        " Skip keywords
        IF ls_token-str <> 'FROM' AND ls_token-str <> 'INTO' AND ls_token-str <> 'TABLE'.
          rv_table = ls_token-str.
          EXIT.
        ENDIF.
      ELSEIF ls_token-str = '('.
        " Dynamic table - get variable name
        READ TABLE it_tokens INTO DATA(ls_next) INDEX sy-tabix + 1.
        IF sy-subrc = 0 AND ls_next-type = 'I'.
          rv_table = |({ ls_next-str })|. " Indicate dynamic
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_parser~extract_parameters.
    DATA: lv_in_exporting TYPE abap_bool,
          lv_param_name   TYPE string.

    CLEAR rt_params.

    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      CASE ls_token-str.
        WHEN 'EXPORTING' OR 'CHANGING' OR 'TABLES'.
          lv_in_exporting = abap_true.
        WHEN 'IMPORTING' OR 'RECEIVING' OR 'EXCEPTIONS'.
          lv_in_exporting = abap_false.
      ENDCASE.

      " Collect parameter names when in EXPORTING/CHANGING section
      IF lv_in_exporting = abap_true AND ls_token-type = 'I'.
        " Look for = sign to identify parameter assignment
        READ TABLE it_tokens INTO DATA(ls_next) INDEX sy-tabix + 1.
        IF sy-subrc = 0 AND ls_next-str = '='.
          lv_param_name = ls_token-str.
          " Get parameter value
          READ TABLE it_tokens INTO DATA(ls_value) INDEX sy-tabix + 2.
          IF sy-subrc = 0.
            APPEND |{ lv_param_name }={ ls_value-str }| TO rt_params.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_parser~is_rfc_call.
    rv_is_rfc = abap_false.

    " Check for DESTINATION keyword
    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      IF ls_token-str = 'DESTINATION'.
        rv_is_rfc = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_syuname_parser~get_destination.
    DATA: lv_found_dest TYPE abap_bool.

    CLEAR rv_destination.

    LOOP AT it_tokens INTO DATA(ls_token) FROM is_statement-from TO is_statement-to.
      IF ls_token-str = 'DESTINATION'.
        lv_found_dest = abap_true.
      ELSEIF lv_found_dest = abap_true.
        rv_destination = ls_token-str.
        " Remove quotes if literal
        REPLACE ALL OCCURRENCES OF '''' IN rv_destination WITH ''.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_dml_statement.
    rv_is_dml = abap_false.
  ENDMETHOD.

  METHOD is_call_statement.
    rv_is_call = abap_false.
  ENDMETHOD.

  METHOD is_assignment_statement.
    rv_is_assignment = abap_false.
  ENDMETHOD.
ENDCLASS.

*"* Test class definitions
CLASS ltc_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zif_syuname_parser.

    METHODS: setup,
             test_parse_dml_insert FOR TESTING,
             test_parse_dml_update FOR TESTING,
             test_parse_rfc_call FOR TESTING,
             test_parse_assignment FOR TESTING,
             test_tokenize_source FOR TESTING,
             test_analyze_statement FOR TESTING,
             test_extract_table_name FOR TESTING,
             test_dynamic_sql FOR TESTING.
ENDCLASS.

CLASS ltc_parser IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW zcl_syuname_parser( ).
  ENDMETHOD.

  METHOD test_parse_dml_insert.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          ls_dml       TYPE zif_syuname_parser=>ty_dml_statement,
          lx_error     TYPE REF TO zcx_syuname_error.

    lt_tokens = VALUE #(
      ( str = 'INSERT' type = 'K' row = 1 col = 1 )
      ( str = 'ZTABLE' type = 'I' row = 1 col = 8 )
      ( str = 'FROM'   type = 'K' row = 1 col = 15 )
      ( str = 'LS_DATA' type = 'I' row = 1 col = 20 ) ).

    ls_statement = VALUE #( from = 1 to = 4 type = 'S' ).

    TRY.
        ls_dml = mo_cut->parse_dml_statement(
          it_tokens    = lt_tokens
          is_statement = ls_statement
          iv_line      = 1 ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Parse error expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_dml_update.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          ls_dml       TYPE zif_syuname_parser=>ty_dml_statement,
          lx_error     TYPE REF TO zcx_syuname_error.

    lt_tokens = VALUE #(
      ( str = 'UPDATE' type = 'K' row = 1 col = 1 )
      ( str = 'ZTABLE' type = 'I' row = 1 col = 8 )
      ( str = 'SET'    type = 'K' row = 1 col = 15 )
      ( str = 'USER'   type = 'I' row = 1 col = 19 )
      ( str = '='      type = 'O' row = 1 col = 24 )
      ( str = 'LV_USER' type = 'I' row = 1 col = 26 ) ).

    ls_statement = VALUE #( from = 1 to = 6 type = 'S' ).

    TRY.
        ls_dml = mo_cut->parse_dml_statement(
          it_tokens    = lt_tokens
          is_statement = ls_statement
          iv_line      = 1 ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Parse error expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_rfc_call.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          ls_call      TYPE zif_syuname_parser=>ty_call_statement,
          lx_error     TYPE REF TO zcx_syuname_error.

    lt_tokens = VALUE #(
      ( str = 'CALL'       type = 'K' row = 1 col = 1 )
      ( str = 'FUNCTION'   type = 'K' row = 1 col = 6 )
      ( str = '''Z_RFC'''  type = 'L' row = 1 col = 15 )
      ( str = 'DESTINATION' type = 'K' row = 1 col = 23 )
      ( str = 'LV_DEST'    type = 'I' row = 1 col = 35 ) ).

    ls_statement = VALUE #( from = 1 to = 5 type = 'S' ).

    TRY.
        ls_call = mo_cut->parse_call_statement(
          it_tokens    = lt_tokens
          is_statement = ls_statement
          iv_line      = 1 ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Parse error expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_parse_assignment.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          ls_assign    TYPE zif_syuname_parser=>ty_assignment,
          lx_error     TYPE REF TO zcx_syuname_error.

    lt_tokens = VALUE #(
      ( str = 'LV_USER' type = 'I' row = 1 col = 1 )
      ( str = '='      type = 'O' row = 1 col = 9 )
      ( str = 'SY-UNAME' type = 'I' row = 1 col = 11 ) ).

    ls_statement = VALUE #( from = 1 to = 3 type = 'S' ).

    TRY.
        ls_assign = mo_cut->parse_assignment(
          it_tokens    = lt_tokens
          is_statement = ls_statement
          iv_line      = 1 ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Parse error expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_tokenize_source.
    DATA: lt_source     TYPE zif_syuname_scanner=>tt_source_lines,
          lt_tokens     TYPE zif_syuname_parser=>tt_tokens,
          lt_statements TYPE zif_syuname_parser=>tt_statements,
          lx_error      TYPE REF TO zcx_syuname_error.

    lt_source = VALUE #(
      ( line_number = 1 statement = 'DATA: lv_user TYPE string.' program = 'ZTEST' )
      ( line_number = 2 statement = 'lv_user = sy-uname.' program = 'ZTEST' ) ).

    TRY.
        mo_cut->tokenize_source(
          EXPORTING
            it_source     = lt_source
          IMPORTING
            et_tokens     = lt_tokens
            et_statements = lt_statements ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Tokenization error expected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_analyze_statement.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          lv_type      TYPE string.

    lt_tokens = VALUE #(
      ( str = 'INSERT' type = 'K' row = 1 col = 1 )
      ( str = 'ZTABLE' type = 'I' row = 1 col = 8 ) ).

    ls_statement = VALUE #( from = 1 to = 2 type = 'S' ).

    lv_type = mo_cut->analyze_statement_type(
      it_tokens    = lt_tokens
      is_statement = ls_statement ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_type
      exp = 'UNKNOWN'
      msg = 'Should return UNKNOWN for unimplemented analysis' ).
  ENDMETHOD.

  METHOD test_extract_table_name.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          lv_table     TYPE string.

    lt_tokens = VALUE #(
      ( str = 'INSERT' type = 'K' row = 1 col = 1 )
      ( str = 'ZTABLE' type = 'I' row = 1 col = 8 )
      ( str = 'FROM'   type = 'K' row = 1 col = 15 ) ).

    ls_statement = VALUE #( from = 1 to = 3 type = 'S' ).

    lv_table = mo_cut->extract_table_name(
      it_tokens    = lt_tokens
      is_statement = ls_statement
      iv_operation = zif_syuname_parser=>c_operation-insert ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_table
      msg = 'Should return empty for unimplemented extraction' ).
  ENDMETHOD.

  METHOD test_dynamic_sql.
    DATA: lt_tokens    TYPE zif_syuname_parser=>tt_tokens,
          ls_statement TYPE zif_syuname_parser=>ty_statement,
          ls_dml       TYPE zif_syuname_parser=>ty_dml_statement,
          lx_error     TYPE REF TO zcx_syuname_error.

    lt_tokens = VALUE #(
      ( str = 'INSERT'   type = 'K' row = 1 col = 1 )
      ( str = '('        type = 'O' row = 1 col = 8 )
      ( str = 'LV_TABLE' type = 'I' row = 1 col = 9 )
      ( str = ')'        type = 'O' row = 1 col = 18 )
      ( str = 'FROM'     type = 'K' row = 1 col = 20 ) ).

    ls_statement = VALUE #( from = 1 to = 5 type = 'S' ).

    TRY.
        ls_dml = mo_cut->parse_dml_statement(
          it_tokens    = lt_tokens
          is_statement = ls_statement
          iv_line      = 1 ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_syuname_error INTO lx_error.
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Parse error expected for dynamic SQL' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
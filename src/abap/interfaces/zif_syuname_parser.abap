INTERFACE zif_syuname_parser
  PUBLIC.

  TYPES: BEGIN OF ty_dml_statement,
           operation    TYPE string,  " INSERT, UPDATE, DELETE, MODIFY
           table_name   TYPE string,
           line_number  TYPE i,
           source_vars  TYPE string_table,
           target_fields TYPE string_table,
           is_dynamic   TYPE abap_bool,
         END OF ty_dml_statement,
         tt_dml_statements TYPE TABLE OF ty_dml_statement.

  TYPES: BEGIN OF ty_call_statement,
           call_type    TYPE string,  " FUNCTION, METHOD, FORM, SUBMIT
           module_name  TYPE string,
           line_number  TYPE i,
           parameters   TYPE string_table,
           tainted_params TYPE string_table,
           is_rfc       TYPE abap_bool,
           destination  TYPE string,
         END OF ty_call_statement,
         tt_call_statements TYPE TABLE OF ty_call_statement.

  TYPES: BEGIN OF ty_assignment,
           target_var   TYPE string,
           source_var   TYPE string,
           line_number  TYPE i,
           assign_type  TYPE string,  " MOVE, =, MOVE-CORRESPONDING, FIELD-SYMBOL
           is_structure TYPE abap_bool,
           field_name   TYPE string,
         END OF ty_assignment,
         tt_assignments TYPE TABLE OF ty_assignment.

  TYPES: BEGIN OF ty_token,
           str  TYPE string,
           type TYPE c,
           row  TYPE i,
           col  TYPE i,
         END OF ty_token,
         tt_tokens TYPE TABLE OF ty_token.

  TYPES: BEGIN OF ty_statement,
           from TYPE i,
           to   TYPE i,
           type TYPE string,
         END OF ty_statement,
         tt_statements TYPE TABLE OF ty_statement.

  CONSTANTS: BEGIN OF c_operation,
               insert TYPE string VALUE 'INSERT',
               update TYPE string VALUE 'UPDATE',
               delete TYPE string VALUE 'DELETE',
               modify TYPE string VALUE 'MODIFY',
             END OF c_operation.

  CONSTANTS: BEGIN OF c_call_type,
               function TYPE string VALUE 'FUNCTION',
               method   TYPE string VALUE 'METHOD',
               form     TYPE string VALUE 'FORM',
               submit   TYPE string VALUE 'SUBMIT',
             END OF c_call_type.

  METHODS: parse_dml_statement
    IMPORTING
      it_tokens     TYPE tt_tokens
      is_statement  TYPE ty_statement
      iv_line       TYPE i
    RETURNING
      VALUE(rs_dml) TYPE ty_dml_statement
    RAISING
      zcx_syuname_error,

    parse_call_statement
    IMPORTING
      it_tokens     TYPE tt_tokens
      is_statement  TYPE ty_statement
      iv_line       TYPE i
    RETURNING
      VALUE(rs_call) TYPE ty_call_statement
    RAISING
      zcx_syuname_error,

    parse_assignment
    IMPORTING
      it_tokens        TYPE tt_tokens
      is_statement     TYPE ty_statement
      iv_line          TYPE i
    RETURNING
      VALUE(rs_assign) TYPE ty_assignment
    RAISING
      zcx_syuname_error,

    tokenize_source
    IMPORTING
      it_source      TYPE zif_syuname_scanner=>tt_source_lines
    EXPORTING
      et_tokens      TYPE tt_tokens
      et_statements  TYPE tt_statements
    RAISING
      zcx_syuname_error,

    analyze_statement_type
    IMPORTING
      it_tokens       TYPE tt_tokens
      is_statement    TYPE ty_statement
    RETURNING
      VALUE(rv_type)  TYPE string,

    extract_table_name
    IMPORTING
      it_tokens         TYPE tt_tokens
      is_statement      TYPE ty_statement
      iv_operation      TYPE string
    RETURNING
      VALUE(rv_table)   TYPE string,

    extract_parameters
    IMPORTING
      it_tokens         TYPE tt_tokens
      is_statement      TYPE ty_statement
      iv_call_type      TYPE string
    RETURNING
      VALUE(rt_params)  TYPE string_table,

    is_rfc_call
    IMPORTING
      it_tokens       TYPE tt_tokens
      is_statement    TYPE ty_statement
    RETURNING
      VALUE(rv_is_rfc) TYPE abap_bool,

    get_destination
    IMPORTING
      it_tokens           TYPE tt_tokens
      is_statement        TYPE ty_statement
    RETURNING
      VALUE(rv_destination) TYPE string.

ENDINTERFACE.
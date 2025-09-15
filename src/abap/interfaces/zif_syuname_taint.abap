INTERFACE zif_syuname_taint
  PUBLIC.

  TYPES: BEGIN OF ty_tainted_var,
           name        TYPE string,
           scope       TYPE string,
           scope_name  TYPE string,
           line        TYPE i,
           program     TYPE programm,
           confidence  TYPE string,
           path        TYPE string_table,
         END OF ty_tainted_var,
         tt_tainted_vars TYPE TABLE OF ty_tainted_var.

  TYPES: BEGIN OF ty_scope_info,
           scope_type  TYPE string,  " GLOBAL, FORM, METHOD, FUNCTION
           scope_name  TYPE string,  " Name of FORM/METHOD/FUNCTION
           parent_prog TYPE programm,
           start_line  TYPE i,
           end_line    TYPE i,
         END OF ty_scope_info,
         tt_scopes TYPE TABLE OF ty_scope_info.

  CONSTANTS: BEGIN OF c_confidence,
               high   TYPE string VALUE 'HIGH',
               medium TYPE string VALUE 'MEDIUM',
               low    TYPE string VALUE 'LOW',
             END OF c_confidence.

  CONSTANTS: BEGIN OF c_scope_type,
               global   TYPE string VALUE 'GLOBAL',
               form     TYPE string VALUE 'FORM',
               method   TYPE string VALUE 'METHOD',
               function TYPE string VALUE 'FUNCTION',
               local    TYPE string VALUE 'LOCAL',
             END OF c_scope_type.

  METHODS: mark_tainted
    IMPORTING
      iv_variable   TYPE string
      iv_scope      TYPE string DEFAULT 'GLOBAL'
      iv_scope_name TYPE string OPTIONAL
      iv_line       TYPE i
      iv_program    TYPE programm
      it_path       TYPE string_table
      iv_confidence TYPE string DEFAULT c_confidence-high,

    is_tainted
    IMPORTING
      iv_variable TYPE string
      iv_scope    TYPE string DEFAULT 'GLOBAL'
      iv_line     TYPE i OPTIONAL
    RETURNING
      VALUE(rv_tainted) TYPE abap_bool,

    get_taint_path
    IMPORTING
      iv_variable TYPE string
      iv_scope    TYPE string DEFAULT 'GLOBAL'
    RETURNING
      VALUE(rt_path) TYPE string_table,

    propagate_taint
    IMPORTING
      iv_from_var   TYPE string
      iv_to_var     TYPE string
      iv_scope      TYPE string DEFAULT 'GLOBAL'
      iv_scope_name TYPE string OPTIONAL
      iv_line       TYPE i
      iv_program    TYPE programm
      iv_operation  TYPE string OPTIONAL,

    enter_scope
    IMPORTING
      iv_scope_type TYPE string
      iv_scope_name TYPE string
      iv_start_line TYPE i
      iv_program    TYPE programm,

    exit_scope,

    get_current_scope
    RETURNING
      VALUE(rs_scope) TYPE ty_scope_info,

    get_all_tainted
    RETURNING
      VALUE(rt_tainted) TYPE tt_tainted_vars,

    clear_taint_data,

    set_field_tainted
    IMPORTING
      iv_structure  TYPE string
      iv_field      TYPE string
      iv_scope      TYPE string DEFAULT 'GLOBAL'
      iv_line       TYPE i
      iv_program    TYPE programm
      it_path       TYPE string_table,

    is_field_tainted
    IMPORTING
      iv_structure TYPE string
      iv_field     TYPE string
      iv_scope     TYPE string DEFAULT 'GLOBAL'
    RETURNING
      VALUE(rv_tainted) TYPE abap_bool.

ENDINTERFACE.
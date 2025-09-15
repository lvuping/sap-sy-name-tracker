CLASS zcl_syuname_move_corr_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_structure_field,
             struct_name TYPE string,
             field_name  TYPE string,
             is_tainted  TYPE abap_bool,
             taint_path  TYPE string_table,
           END OF ty_structure_field.

    TYPES: tt_structure_fields TYPE TABLE OF ty_structure_field.

    METHODS: handle_move_corresponding
      IMPORTING
        iv_source_struct TYPE string
        iv_target_struct TYPE string
        iv_line_number   TYPE i
        io_taint_engine  TYPE REF TO zif_syuname_taint,

      track_structure_fields
        IMPORTING
          iv_struct_name TYPE string
          it_fields      TYPE tt_structure_fields,

      get_tainted_fields
        IMPORTING
          iv_struct_name TYPE string
        RETURNING
          VALUE(rt_fields) TYPE tt_structure_fields,

      analyze_move_corresponding_statement
        IMPORTING
          iv_statement TYPE string
        EXPORTING
          ev_source    TYPE string
          ev_target    TYPE string
          ev_is_move_corr TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_structure_registry TYPE tt_structure_fields.

    METHODS: extract_structure_components
      IMPORTING
        iv_struct_name TYPE string
      RETURNING
        VALUE(rt_components) TYPE string_table,

      propagate_field_taint
        IMPORTING
          iv_source_field TYPE string
          iv_target_field TYPE string
          io_taint_engine TYPE REF TO zif_syuname_taint
          iv_line_number  TYPE i.

ENDCLASS.

CLASS zcl_syuname_move_corr_handler IMPLEMENTATION.
  METHOD handle_move_corresponding.
    DATA: lt_source_fields TYPE tt_structure_fields,
          lt_components    TYPE string_table.

    " Get tainted fields from source structure
    lt_source_fields = get_tainted_fields( iv_source_struct ).

    " Get all components that could be affected
    lt_components = extract_structure_components( iv_source_struct ).

    " Propagate taint for each matching field
    LOOP AT lt_source_fields INTO DATA(ls_field) WHERE is_tainted = abap_true.
      " Check if field exists in target structure
      DATA(lv_target_field) = |{ iv_target_struct }-{ ls_field-field_name }|.
      DATA(lv_source_field) = |{ iv_source_struct }-{ ls_field-field_name }|.

      " Propagate taint with MOVE-CORRESPONDING marker
      propagate_field_taint(
        iv_source_field = lv_source_field
        iv_target_field = lv_target_field
        io_taint_engine = io_taint_engine
        iv_line_number  = iv_line_number ).

      " Update registry for target structure
      APPEND VALUE #(
        struct_name = iv_target_struct
        field_name  = ls_field-field_name
        is_tainted  = abap_true
        taint_path  = ls_field-taint_path
      ) TO mt_structure_registry.
    ENDLOOP.
  ENDMETHOD.

  METHOD track_structure_fields.
    " Add or update structure fields in registry
    LOOP AT it_fields INTO DATA(ls_field).
      " Check if entry exists
      READ TABLE mt_structure_registry TRANSPORTING NO FIELDS
        WITH KEY struct_name = iv_struct_name
                 field_name  = ls_field-field_name.

      IF sy-subrc = 0.
        " Update existing entry
        MODIFY mt_structure_registry FROM ls_field
          TRANSPORTING is_tainted taint_path
          WHERE struct_name = iv_struct_name
            AND field_name  = ls_field-field_name.
      ELSE.
        " Add new entry
        ls_field-struct_name = iv_struct_name.
        APPEND ls_field TO mt_structure_registry.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_tainted_fields.
    " Return all tainted fields for a structure
    LOOP AT mt_structure_registry INTO DATA(ls_field)
      WHERE struct_name = iv_struct_name
        AND is_tainted = abap_true.
      APPEND ls_field TO rt_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD analyze_move_corresponding_statement.
    DATA: lv_statement TYPE string.

    CLEAR: ev_source, ev_target, ev_is_move_corr.
    lv_statement = to_upper( iv_statement ).

    " Check for MOVE-CORRESPONDING pattern
    IF lv_statement CS 'MOVE-CORRESPONDING'.
      ev_is_move_corr = abap_true.

      " Extract source and target
      " Pattern: MOVE-CORRESPONDING source TO target
      FIND REGEX 'MOVE-CORRESPONDING\s+(\S+)\s+TO\s+(\S+)'
        IN lv_statement
        SUBMATCHES ev_source ev_target.

      IF sy-subrc <> 0.
        " Try alternative pattern: MOVE-CORRESPONDING source TO target.
        FIND REGEX 'MOVE-CORRESPONDING\s+(\S+)\s+TO\s+(\S+)\.'
          IN lv_statement
          SUBMATCHES ev_source ev_target.
      ENDIF.

      " Clean up extracted names
      REPLACE ALL OCCURRENCES OF '.' IN ev_source WITH ''.
      REPLACE ALL OCCURRENCES OF '.' IN ev_target WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN ev_source WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN ev_target WITH ''.
    ENDIF.
  ENDMETHOD.

  METHOD extract_structure_components.
    " This would normally use RTTI to get actual structure components
    " For demonstration, returning common field names
    APPEND 'MANDT' TO rt_components.
    APPEND 'BUKRS' TO rt_components.
    APPEND 'GJAHR' TO rt_components.
    APPEND 'BELNR' TO rt_components.
    APPEND 'BUZEI' TO rt_components.
    APPEND 'USNAM' TO rt_components.  " Common user field
    APPEND 'ERNAM' TO rt_components.  " Created by
    APPEND 'AENAM' TO rt_components.  " Changed by
    APPEND 'ERDAT' TO rt_components.
    APPEND 'ERZET' TO rt_components.
    APPEND 'AEDAT' TO rt_components.
    APPEND 'AEZET' TO rt_components.
  ENDMETHOD.

  METHOD propagate_field_taint.
    DATA: lt_path TYPE string_table.

    " Build propagation path
    APPEND |MOVE-CORRESPONDING: { iv_source_field }| TO lt_path.
    APPEND |  -> { iv_target_field }| TO lt_path.

    " Use taint engine to propagate
    io_taint_engine->propagate_taint(
      iv_from_var = iv_source_field
      iv_to_var   = iv_target_field
      iv_scope    = 'CURRENT'
      iv_line     = iv_line_number ).

    " Mark with MEDIUM confidence since it's structure transfer
    io_taint_engine->set_confidence(
      iv_variable   = iv_target_field
      iv_confidence = 'MEDIUM' ).
  ENDMETHOD.
ENDCLASS.
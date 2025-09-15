CLASS zcl_syuname_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: create_scanner
      RETURNING
        VALUE(ro_scanner) TYPE REF TO zif_syuname_scanner,

      create_taint_engine
        RETURNING
          VALUE(ro_taint) TYPE REF TO zif_syuname_taint,

      create_parser
        RETURNING
          VALUE(ro_parser) TYPE REF TO zif_syuname_parser,

      create_reporter
        RETURNING
          VALUE(ro_reporter) TYPE REF TO zif_syuname_report,

      create_all_components
        EXPORTING
          eo_scanner  TYPE REF TO zif_syuname_scanner
          eo_taint    TYPE REF TO zif_syuname_taint
          eo_parser   TYPE REF TO zif_syuname_parser
          eo_reporter TYPE REF TO zif_syuname_report.

ENDCLASS.

CLASS zcl_syuname_factory IMPLEMENTATION.
  METHOD create_scanner.
    CREATE OBJECT ro_scanner TYPE zcl_syuname_scanner.
  ENDMETHOD.

  METHOD create_taint_engine.
    CREATE OBJECT ro_taint TYPE zcl_syuname_taint.
  ENDMETHOD.

  METHOD create_parser.
    CREATE OBJECT ro_parser TYPE zcl_syuname_parser.
  ENDMETHOD.

  METHOD create_reporter.
    CREATE OBJECT ro_reporter TYPE zcl_syuname_report.
  ENDMETHOD.

  METHOD create_all_components.
    eo_scanner = create_scanner( ).
    eo_taint = create_taint_engine( ).
    eo_parser = create_parser( ).
    eo_reporter = create_reporter( ).
  ENDMETHOD.
ENDCLASS.
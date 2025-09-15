INTERFACE zif_syuname_scanner
  PUBLIC.

  TYPES: BEGIN OF ty_source_line,
           line_number TYPE i,
           statement   TYPE string,
           program     TYPE programm,
         END OF ty_source_line,
         tt_source_lines TYPE TABLE OF ty_source_line.

  TYPES: BEGIN OF ty_include_info,
           include_name TYPE programm,
           parent_prog  TYPE programm,
           level        TYPE i,
         END OF ty_include_info,
         tt_includes TYPE TABLE OF ty_include_info.

  METHODS: read_program
    IMPORTING
      iv_program_name TYPE programm
    RETURNING
      VALUE(rt_source) TYPE tt_source_lines
    RAISING
      zcx_syuname_error,

    get_includes
    IMPORTING
      iv_program_name TYPE programm
      iv_recursive    TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_includes) TYPE tt_includes
    RAISING
      zcx_syuname_error,

    read_include
    IMPORTING
      iv_include_name TYPE programm
      iv_parent_prog  TYPE programm
    RETURNING
      VALUE(rt_source) TYPE tt_source_lines
    RAISING
      zcx_syuname_error,

    get_source_cache
    RETURNING
      VALUE(rt_cache) TYPE tt_source_lines,

    clear_cache.
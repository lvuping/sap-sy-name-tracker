INTERFACE zif_syuname_report
  PUBLIC.

  TYPES: BEGIN OF ty_finding,
           sequence_num  TYPE i,
           program_name  TYPE programm,
           include_name  TYPE programm,
           syuname_line  TYPE i,
           operation     TYPE string,
           target_table  TYPE string,
           target_rfc    TYPE string,
           field_name    TYPE string,
           taint_path    TYPE string_table,
           confidence    TYPE string,
           timestamp     TYPE timestampl,
         END OF ty_finding,
         tt_findings TYPE TABLE OF ty_finding.

  TYPES: BEGIN OF ty_statistics,
           programs_analyzed  TYPE i,
           includes_analyzed  TYPE i,
           lines_processed    TYPE i,
           syuname_found      TYPE i,
           findings_total     TYPE i,
           findings_dml       TYPE i,
           findings_rfc       TYPE i,
           findings_other     TYPE i,
           start_time         TYPE timestampl,
           end_time           TYPE timestampl,
           duration_seconds   TYPE i,
         END OF ty_statistics.

  TYPES: BEGIN OF ty_csv_line,
           line TYPE string,
         END OF ty_csv_line,
         tt_csv_lines TYPE TABLE OF ty_csv_line.

  CONSTANTS: BEGIN OF c_operation_type,
               dml_insert  TYPE string VALUE 'INSERT',
               dml_update  TYPE string VALUE 'UPDATE',
               dml_modify  TYPE string VALUE 'MODIFY',
               dml_delete  TYPE string VALUE 'DELETE',
               rfc_call    TYPE string VALUE 'CALL_FUNCTION',
               method_call TYPE string VALUE 'CALL_METHOD',
               form_call   TYPE string VALUE 'PERFORM',
               submit      TYPE string VALUE 'SUBMIT',
             END OF c_operation_type.

  METHODS: add_finding
    IMPORTING
      iv_program      TYPE programm
      iv_include      TYPE programm OPTIONAL
      iv_syuname_line TYPE i
      iv_operation    TYPE string
      iv_target_table TYPE string OPTIONAL
      iv_target_rfc   TYPE string OPTIONAL
      iv_field_name   TYPE string OPTIONAL
      it_taint_path   TYPE string_table
      iv_confidence   TYPE string DEFAULT 'HIGH',

    generate_csv
    IMPORTING
      iv_file_path TYPE string
    RAISING
      zcx_syuname_error,

    get_csv_content
    RETURNING
      VALUE(rt_csv) TYPE tt_csv_lines,

    get_statistics
    RETURNING
      VALUE(rs_stats) TYPE ty_statistics,

    get_all_findings
    RETURNING
      VALUE(rt_findings) TYPE tt_findings,

    clear_findings,

    set_start_time,

    set_end_time,

    increment_counter
    IMPORTING
      iv_counter_type TYPE string,

    write_to_application_log
    IMPORTING
      iv_object    TYPE balobj_d DEFAULT 'ZSYUNAME'
      iv_subobject TYPE balsubobj DEFAULT 'ANALYSIS'
    RAISING
      zcx_syuname_error,

    export_to_json
    IMPORTING
      iv_file_path TYPE string
    RAISING
      zcx_syuname_error,

    filter_findings
    IMPORTING
      iv_operation   TYPE string OPTIONAL
      iv_confidence  TYPE string OPTIONAL
      iv_table_name  TYPE string OPTIONAL
    RETURNING
      VALUE(rt_filtered) TYPE tt_findings,

    sort_findings
    IMPORTING
      iv_sort_field TYPE string DEFAULT 'SEQUENCE_NUM',

    generate_summary_report
    RETURNING
      VALUE(rt_summary) TYPE string_table.

ENDINTERFACE.
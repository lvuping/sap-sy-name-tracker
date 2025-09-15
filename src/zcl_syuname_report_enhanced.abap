CLASS zcl_syuname_report_enhanced DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_finding_json,
             sequence_number TYPE i,
             program_name    TYPE string,
             line_number     TYPE i,
             operation_type  TYPE string,
             target_object   TYPE string,
             field_name      TYPE string,
             taint_path      TYPE string_table,
             confidence      TYPE string,
             timestamp       TYPE timestampl,
           END OF ty_finding_json.

    TYPES: tt_findings_json TYPE TABLE OF ty_finding_json.

    TYPES: BEGIN OF ty_statistics,
             total_findings     TYPE i,
             high_confidence    TYPE i,
             medium_confidence  TYPE i,
             low_confidence     TYPE i,
             critical_findings  TYPE i,
             tables_affected    TYPE i,
             rfc_calls_affected TYPE i,
             programs_analyzed  TYPE i,
             lines_processed    TYPE i,
             execution_time_sec TYPE i,
           END OF ty_statistics.

    TYPES: BEGIN OF ty_table_summary,
             table_name   TYPE tabname,
             impact_count TYPE i,
             operations   TYPE string_table,
             risk_level   TYPE string,
           END OF ty_table_summary.

    TYPES: tt_table_summary TYPE TABLE OF ty_table_summary.

    METHODS: generate_json_report
      IMPORTING
        it_findings    TYPE tt_findings_json
        iv_output_path TYPE string
      RAISING
        zcx_syuname_error,

      generate_detailed_statistics
        IMPORTING
          it_findings TYPE tt_findings_json
        RETURNING
          VALUE(rs_statistics) TYPE ty_statistics,

      create_summary_report
        IMPORTING
          it_findings TYPE tt_findings_json
        RETURNING
          VALUE(rt_summary) TYPE tt_table_summary,

      generate_html_report
        IMPORTING
          it_findings    TYPE tt_findings_json
          is_statistics  TYPE ty_statistics
          iv_output_path TYPE string
        RAISING
          zcx_syuname_error,

      setup_batch_mode
        IMPORTING
          it_programs TYPE TABLE OF programm
        RETURNING
          VALUE(rv_batch_id) TYPE sysuuid_x16.

  PRIVATE SECTION.
    DATA: mv_batch_id TYPE sysuuid_x16,
          mt_batch_programs TYPE TABLE OF programm,
          mv_batch_index TYPE i.

    METHODS: build_html_header
      RETURNING
        VALUE(rv_html) TYPE string,

      build_html_findings_table
        IMPORTING
          it_findings TYPE tt_findings_json
        RETURNING
          VALUE(rv_html) TYPE string,

      build_html_statistics
        IMPORTING
          is_statistics TYPE ty_statistics
        RETURNING
          VALUE(rv_html) TYPE string,

      build_html_footer
        RETURNING
          VALUE(rv_html) TYPE string,

      escape_html
        IMPORTING
          iv_text TYPE string
        RETURNING
          VALUE(rv_escaped) TYPE string,

      get_risk_color
        IMPORTING
          iv_confidence TYPE string
        RETURNING
          VALUE(rv_color) TYPE string.

ENDCLASS.

CLASS zcl_syuname_report_enhanced IMPLEMENTATION.
  METHOD generate_json_report.
    DATA: lv_json TYPE string.

    TRY.
        " Convert findings to JSON using SAP's JSON serialization
        " For ABAP 7.40+, we use a simple JSON builder
        lv_json = '{' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '  "report_metadata": {' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '    "timestamp": "' && sy-datum && sy-uzeit && '",' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '    "analyzer_version": "1.0.0",' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '    "sap_system": "' && sy-sysid && '"' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '  },' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '  "findings": [' && cl_abap_char_utilities=>newline.

        " Add each finding
        LOOP AT it_findings INTO DATA(ls_finding).
          IF sy-tabix > 1.
            lv_json = lv_json && ',' && cl_abap_char_utilities=>newline.
          ENDIF.

          lv_json = lv_json && '    {' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "sequence": ' && ls_finding-sequence_number && ',' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "program": "' && ls_finding-program_name && '",' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "line": ' && ls_finding-line_number && ',' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "operation": "' && ls_finding-operation_type && '",' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "target": "' && ls_finding-target_object && '",' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "field": "' && ls_finding-field_name && '",' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "confidence": "' && ls_finding-confidence && '",' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '      "taint_path": [' && cl_abap_char_utilities=>newline.

          " Add taint path
          LOOP AT ls_finding-taint_path INTO DATA(lv_path_step).
            IF sy-tabix > 1.
              lv_json = lv_json && ',' && cl_abap_char_utilities=>newline.
            ENDIF.
            lv_json = lv_json && '        "' && lv_path_step && '"'.
          ENDLOOP.

          lv_json = lv_json && cl_abap_char_utilities=>newline && '      ]' && cl_abap_char_utilities=>newline.
          lv_json = lv_json && '    }'.
        ENDLOOP.

        lv_json = lv_json && cl_abap_char_utilities=>newline && '  ]' && cl_abap_char_utilities=>newline.
        lv_json = lv_json && '}' && cl_abap_char_utilities=>newline.

        " Write to file
        OPEN DATASET iv_output_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_syuname_error
            EXPORTING
              textid = zcx_syuname_error=>file_write_error
              file_path = iv_output_path.
        ENDIF.

        TRANSFER lv_json TO iv_output_path.
        CLOSE DATASET iv_output_path.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_syuname_error
          EXPORTING
            textid = zcx_syuname_error=>file_write_error
            file_path = iv_output_path
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD generate_detailed_statistics.
    CLEAR rs_statistics.

    " Count total findings
    rs_statistics-total_findings = lines( it_findings ).

    " Count by confidence level
    LOOP AT it_findings INTO DATA(ls_finding).
      CASE ls_finding-confidence.
        WHEN 'HIGH'.
          ADD 1 TO rs_statistics-high_confidence.
        WHEN 'MEDIUM'.
          ADD 1 TO rs_statistics-medium_confidence.
        WHEN 'LOW'.
          ADD 1 TO rs_statistics-low_confidence.
        WHEN 'CRITICAL'.
          ADD 1 TO rs_statistics-critical_findings.
      ENDCASE.

      " Count affected objects
      IF ls_finding-operation_type CS 'INSERT' OR
         ls_finding-operation_type CS 'UPDATE' OR
         ls_finding-operation_type CS 'MODIFY' OR
         ls_finding-operation_type CS 'DELETE'.
        ADD 1 TO rs_statistics-tables_affected.
      ELSEIF ls_finding-operation_type CS 'CALL_FUNCTION' OR
             ls_finding-operation_type CS 'RFC'.
        ADD 1 TO rs_statistics-rfc_calls_affected.
      ENDIF.
    ENDLOOP.

    " Get unique programs
    SORT it_findings BY program_name.
    DELETE ADJACENT DUPLICATES FROM it_findings COMPARING program_name.
    rs_statistics-programs_analyzed = lines( it_findings ).

    " Calculate execution time (example)
    GET TIME STAMP FIELD DATA(lv_end_time).
    " rs_statistics-execution_time_sec = calculated_value.
  ENDMETHOD.

  METHOD create_summary_report.
    DATA: lt_temp_summary TYPE tt_table_summary,
          ls_summary TYPE ty_table_summary.

    " Group findings by target table
    LOOP AT it_findings INTO DATA(ls_finding).
      " Skip non-table operations
      IF ls_finding-target_object IS INITIAL.
        CONTINUE.
      ENDIF.

      " Check if table already in summary
      READ TABLE lt_temp_summary INTO ls_summary
        WITH KEY table_name = ls_finding-target_object.

      IF sy-subrc = 0.
        " Update existing entry
        ADD 1 TO ls_summary-impact_count.
        APPEND ls_finding-operation_type TO ls_summary-operations.
        MODIFY lt_temp_summary FROM ls_summary INDEX sy-tabix.
      ELSE.
        " Create new entry
        CLEAR ls_summary.
        ls_summary-table_name = ls_finding-target_object.
        ls_summary-impact_count = 1.
        APPEND ls_finding-operation_type TO ls_summary-operations.

        " Determine risk level
        IF ls_finding-confidence = 'CRITICAL'.
          ls_summary-risk_level = 'CRITICAL'.
        ELSEIF ls_finding-confidence = 'HIGH'.
          ls_summary-risk_level = 'HIGH'.
        ELSE.
          ls_summary-risk_level = 'MEDIUM'.
        ENDIF.

        APPEND ls_summary TO lt_temp_summary.
      ENDIF.
    ENDLOOP.

    " Sort by impact count (descending)
    SORT lt_temp_summary BY impact_count DESCENDING.

    rt_summary = lt_temp_summary.
  ENDMETHOD.

  METHOD generate_html_report.
    DATA: lv_html TYPE string.

    TRY.
        " Build HTML report
        lv_html = build_html_header( ).
        lv_html = lv_html && build_html_statistics( is_statistics ).
        lv_html = lv_html && build_html_findings_table( it_findings ).
        lv_html = lv_html && build_html_footer( ).

        " Write to file
        OPEN DATASET iv_output_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_syuname_error
            EXPORTING
              textid = zcx_syuname_error=>file_write_error
              file_path = iv_output_path.
        ENDIF.

        TRANSFER lv_html TO iv_output_path.
        CLOSE DATASET iv_output_path.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_syuname_error
          EXPORTING
            textid = zcx_syuname_error=>file_write_error
            file_path = iv_output_path
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD setup_batch_mode.
    " Generate batch ID
    CALL FUNCTION 'SYSTEM_UUID_CREATE'
      IMPORTING
        uuid = mv_batch_id.

    rv_batch_id = mv_batch_id.

    " Store programs for batch processing
    mt_batch_programs = it_programs.
    mv_batch_index = 0.

    " Log batch start
    MESSAGE i100(zsyuname) WITH 'Batch mode started' lines( it_programs ) 'programs'.
  ENDMETHOD.

  METHOD build_html_header.
    rv_html = '<!DOCTYPE html>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '<html lang="en">' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '<head>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <meta charset="UTF-8">' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <title>SAP SY-UNAME Taint Analysis Report</title>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <style>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    body { font-family: Arial, sans-serif; margin: 20px; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    h1 { color: #1e4788; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    .statistics { background: #f0f0f0; padding: 15px; margin: 20px 0; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    table { border-collapse: collapse; width: 100%; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    th { background: #1e4788; color: white; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    .high { color: #d32f2f; font-weight: bold; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    .medium { color: #f57c00; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    .low { color: #388e3c; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    .critical { color: #b71c1c; font-weight: bold; background: #ffebee; }' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  </style>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '</head>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '<body>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <h1>SAP SY-UNAME Taint Analysis Report</h1>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <p>Generated: ' && sy-datum && ' ' && sy-uzeit && '</p>' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD build_html_findings_table.
    rv_html = '  <h2>Findings Detail</h2>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <table>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    <thead>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <tr>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>#</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Program</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Line</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Operation</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Target</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Confidence</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '        <th>Taint Path</th>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      </tr>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    </thead>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    <tbody>' && cl_abap_char_utilities=>newline.

    LOOP AT it_findings INTO DATA(ls_finding).
      DATA(lv_class) = get_risk_color( ls_finding-confidence ).

      rv_html = rv_html && '      <tr class="' && lv_class && '">' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && ls_finding-sequence_number && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && escape_html( ls_finding-program_name ) && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && ls_finding-line_number && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && escape_html( ls_finding-operation_type ) && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && escape_html( ls_finding-target_object ) && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td>' && ls_finding-confidence && '</td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '        <td><small>' && cl_abap_char_utilities=>newline.

      LOOP AT ls_finding-taint_path INTO DATA(lv_path).
        rv_html = rv_html && escape_html( lv_path ) && '<br>' && cl_abap_char_utilities=>newline.
      ENDLOOP.

      rv_html = rv_html && '        </small></td>' && cl_abap_char_utilities=>newline.
      rv_html = rv_html && '      </tr>' && cl_abap_char_utilities=>newline.
    ENDLOOP.

    rv_html = rv_html && '    </tbody>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  </table>' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD build_html_statistics.
    rv_html = '  <div class="statistics">' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    <h2>Statistics Summary</h2>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    <ul>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Total Findings: <strong>' && is_statistics-total_findings && '</strong></li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Critical: <span class="critical">' && is_statistics-critical_findings && '</span></li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>High Confidence: <span class="high">' && is_statistics-high_confidence && '</span></li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Medium Confidence: <span class="medium">' && is_statistics-medium_confidence && '</span></li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Low Confidence: <span class="low">' && is_statistics-low_confidence && '</span></li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Tables Affected: ' && is_statistics-tables_affected && '</li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>RFC Calls: ' && is_statistics-rfc_calls_affected && '</li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '      <li>Programs Analyzed: ' && is_statistics-programs_analyzed && '</li>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '    </ul>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  </div>' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD build_html_footer.
    rv_html = '  <hr>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '  <p><small>Generated by SAP SY-UNAME Analyzer v1.0.0</small></p>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '</body>' && cl_abap_char_utilities=>newline.
    rv_html = rv_html && '</html>' && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD escape_html.
    rv_escaped = iv_text.
    REPLACE ALL OCCURRENCES OF '&' IN rv_escaped WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_escaped WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_escaped WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_escaped WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF '''' IN rv_escaped WITH '&#39;'.
  ENDMETHOD.

  METHOD get_risk_color.
    CASE iv_confidence.
      WHEN 'CRITICAL'.
        rv_color = 'critical'.
      WHEN 'HIGH'.
        rv_color = 'high'.
      WHEN 'MEDIUM'.
        rv_color = 'medium'.
      WHEN 'LOW'.
        rv_color = 'low'.
      WHEN OTHERS.
        rv_color = ''.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
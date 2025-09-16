*&---------------------------------------------------------------------*
*& Report ZSYUNAME_ANALYZER
*&---------------------------------------------------------------------*
*& SAP ABAP SY-UNAME Tracker - Monolithic Version
*& Tracks sy-uname (user ID) data flow through ABAP code
*&---------------------------------------------------------------------*
REPORT zsyuname_anlz.

TYPES: BEGIN OF ty_taint_entry,
         variable TYPE string,
         line     TYPE i,
         context  TYPE string,
       END OF ty_taint_entry.

TYPES: BEGIN OF ty_result,
         seq_num      TYPE i,
         program      TYPE string,
         uname_line   TYPE i,
         operation    TYPE string,
         target       TYPE string,
         field        TYPE string,
         taint_path   TYPE string,
       END OF ty_result.

TYPES: tt_results TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

DATA: gt_source     TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      gt_tokens     TYPE STANDARD TABLE OF stokesx WITH DEFAULT KEY,
      gt_statements TYPE STANDARD TABLE OF sstmnt WITH DEFAULT KEY,
      gt_tainted    TYPE STANDARD TABLE OF ty_taint_entry WITH DEFAULT KEY,
      gt_results    TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY,
      gv_seq_num    TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_prog   TYPE progname OBLIGATORY,
            p_output TYPE string LOWER CASE DEFAULT '/tmp/syuname_results.csv'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_scanner DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_scanner DEFINITION.
  PUBLIC SECTION.
    METHODS: read_program
      IMPORTING iv_program TYPE progname
      EXPORTING et_source  TYPE STANDARD TABLE.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_scanner IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_scanner IMPLEMENTATION.
  METHOD read_program.
    DATA: lt_include TYPE STANDARD TABLE OF progname WITH DEFAULT KEY,
          lv_include TYPE progname.

    CLEAR et_source.

    READ REPORT iv_program INTO et_source.
    IF sy-subrc <> 0.
      MESSAGE |Program { iv_program } not found| TYPE 'E'.
      RETURN.
    ENDIF.

    " Handle includes
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = iv_program
      TABLES
        includetab = lt_include
      EXCEPTIONS
        OTHERS     = 1.

    LOOP AT lt_include INTO lv_include.
      DATA: lt_include_source TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      READ REPORT lv_include INTO lt_include_source.
      IF sy-subrc = 0.
        APPEND LINES OF lt_include_source TO et_source.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_taint_tracker DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_taint_tracker DEFINITION.
  PUBLIC SECTION.
    METHODS: initialize,
             add_tainted
               IMPORTING iv_variable TYPE string
                         iv_line     TYPE i
                         iv_context  TYPE string,
             is_tainted
               IMPORTING iv_variable   TYPE string
               RETURNING VALUE(rv_tainted) TYPE abap_bool,
             get_taint_path
               IMPORTING iv_variable TYPE string
               RETURNING VALUE(rv_path) TYPE string.

  PRIVATE SECTION.
    DATA: mt_tainted TYPE STANDARD TABLE OF ty_taint_entry WITH DEFAULT KEY.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_taint_tracker IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_taint_tracker IMPLEMENTATION.
  METHOD initialize.
    CLEAR mt_tainted.
    " sy-uname is always tainted
    DATA: ls_entry TYPE ty_taint_entry.
    ls_entry-variable = 'SY-UNAME'.
    ls_entry-line = 0.
    ls_entry-context = 'SYSTEM'.
    APPEND ls_entry TO mt_tainted.
  ENDMETHOD.

  METHOD add_tainted.
    DATA: ls_entry TYPE ty_taint_entry.
    ls_entry-variable = iv_variable.
    ls_entry-line = iv_line.
    ls_entry-context = iv_context.
    APPEND ls_entry TO mt_tainted.
  ENDMETHOD.

  METHOD is_tainted.
    rv_tainted = abap_false.
    READ TABLE mt_tainted TRANSPORTING NO FIELDS
      WITH KEY variable = iv_variable.
    IF sy-subrc = 0.
      rv_tainted = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_taint_path.
    DATA: ls_entry TYPE ty_taint_entry.
    CLEAR rv_path.
    LOOP AT mt_tainted INTO ls_entry WHERE variable = iv_variable.
      IF rv_path IS NOT INITIAL.
        rv_path = |{ rv_path } -> |.
      ENDIF.
      rv_path = |{ rv_path }{ ls_entry-variable }:{ ls_entry-line }|.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_parser DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_parser DEFINITION.
  PUBLIC SECTION.
    METHODS: parse_statements
               IMPORTING it_source  TYPE STANDARD TABLE
                         io_taint   TYPE REF TO lcl_taint_tracker
               EXPORTING et_results TYPE STANDARD TABLE.

  PRIVATE SECTION.
    METHODS: analyze_statement
               IMPORTING it_tokens  TYPE STANDARD TABLE
                         iv_line    TYPE i
                         io_taint   TYPE REF TO lcl_taint_tracker
               EXPORTING et_results TYPE tt_results,
             extract_token_value
               IMPORTING it_tokens TYPE STANDARD TABLE
                         iv_index  TYPE i
               RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_parser IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_parser IMPLEMENTATION.
  METHOD parse_statements.
    DATA: lt_tokens     TYPE STANDARD TABLE OF stokesx WITH DEFAULT KEY,
          lt_statements TYPE STANDARD TABLE OF sstmnt WITH DEFAULT KEY,
          ls_statement  TYPE sstmnt,
          lv_line       TYPE i.

    CLEAR et_results.

    " Tokenize source code with analysis for offset/length
    SCAN ABAP-SOURCE it_source
         TOKENS INTO lt_tokens
         STATEMENTS INTO lt_statements
         WITH ANALYSIS.

    " Process each statement
    LOOP AT lt_statements INTO ls_statement.
      lv_line = ls_statement-trow.

      " Extract tokens for this statement
      DATA: lt_stmt_tokens TYPE STANDARD TABLE OF stokesx WITH DEFAULT KEY,
            ls_token       TYPE stokesx,
            lv_from        TYPE i,
            lv_to          TYPE i.

      lv_from = ls_statement-from.
      lv_to = ls_statement-to.

      LOOP AT lt_tokens INTO ls_token FROM lv_from TO lv_to.
        APPEND ls_token TO lt_stmt_tokens.
      ENDLOOP.

      DATA: lt_stmt_results TYPE tt_results.

      analyze_statement( EXPORTING it_tokens = lt_stmt_tokens
                                  iv_line = lv_line
                                  io_taint = io_taint
                        IMPORTING et_results = lt_stmt_results ).

      APPEND LINES OF lt_stmt_results TO et_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD analyze_statement.
    DATA: lv_keyword TYPE string,
          ls_result  TYPE ty_result,
          lv_idx     TYPE i.

    CLEAR et_results.

    " Get first token (keyword)
    lv_keyword = extract_token_value( it_tokens = it_tokens iv_index = 1 ).

    CASE lv_keyword.
      WHEN 'DATA' OR 'MOVE' OR '='.
        " Assignment tracking
        DATA: lv_target TYPE string,
              lv_source TYPE string.

        IF lv_keyword = 'DATA'.
          " DATA: var = sy-uname.
          lv_target = extract_token_value( it_tokens = it_tokens iv_index = 2 ).
          lv_source = extract_token_value( it_tokens = it_tokens iv_index = 4 ).
        ELSEIF lv_keyword = 'MOVE'.
          " MOVE sy-uname TO var.
          lv_source = extract_token_value( it_tokens = it_tokens iv_index = 2 ).
          lv_target = extract_token_value( it_tokens = it_tokens iv_index = 4 ).
        ELSE.
          " var = sy-uname.
          lv_target = extract_token_value( it_tokens = it_tokens iv_index = 1 ).
          lv_source = extract_token_value( it_tokens = it_tokens iv_index = 3 ).
        ENDIF.

        IF lv_source = 'SY-UNAME' OR io_taint->is_tainted( lv_source ) = abap_true.
          io_taint->add_tainted( iv_variable = lv_target
                                iv_line = iv_line
                                iv_context = 'ASSIGNMENT' ).
        ENDIF.

      WHEN 'INSERT' OR 'UPDATE' OR 'MODIFY' OR 'DELETE'.
        " Database operations
        DATA: lv_table TYPE string,
              lv_var   TYPE string.

        lv_table = extract_token_value( it_tokens = it_tokens iv_index = 2 ).

        " Check for tainted variables in the statement
        DATA: ls_token_check TYPE stokesx.
        LOOP AT it_tokens INTO ls_token_check WHERE str CS 'SY-UNAME'.
          gv_seq_num = gv_seq_num + 1.
          ls_result-seq_num = gv_seq_num.
          ls_result-program = p_prog.
          ls_result-uname_line = iv_line.
          ls_result-operation = lv_keyword.
          ls_result-target = lv_table.
          ls_result-field = 'DIRECT'.
          ls_result-taint_path = |SY-UNAME:{ iv_line }|.
          APPEND ls_result TO et_results.
          EXIT.
        ENDLOOP.

        " Check for tainted variables
        lv_idx = 3.
        WHILE lv_idx <= lines( it_tokens ).
          lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
          IF io_taint->is_tainted( lv_var ) = abap_true.
            gv_seq_num = gv_seq_num + 1.
            ls_result-seq_num = gv_seq_num.
            ls_result-program = p_prog.
            ls_result-uname_line = iv_line.
            ls_result-operation = lv_keyword.
            ls_result-target = lv_table.
            ls_result-field = lv_var.
            ls_result-taint_path = io_taint->get_taint_path( lv_var ).
            APPEND ls_result TO et_results.
            EXIT.
          ENDIF.
          lv_idx = lv_idx + 1.
        ENDWHILE.

      WHEN 'CALL'.
        " Function calls
        DATA: lv_next TYPE string,
              lv_func TYPE string.

        lv_next = extract_token_value( it_tokens = it_tokens iv_index = 2 ).
        IF lv_next = 'FUNCTION'.
          lv_func = extract_token_value( it_tokens = it_tokens iv_index = 3 ).

          " Check for tainted parameters
          lv_idx = 4.
          WHILE lv_idx <= lines( it_tokens ).
            lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
            IF lv_var = 'EXPORTING' OR lv_var = 'CHANGING'.
              lv_idx = lv_idx + 1.
              WHILE lv_idx <= lines( it_tokens ).
                lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
                IF lv_var = 'IMPORTING' OR lv_var = 'TABLES' OR lv_var = 'EXCEPTIONS'.
                  EXIT.
                ENDIF.
                lv_idx = lv_idx + 2. " Skip parameter name
                IF lv_idx <= lines( it_tokens ).
                  lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
                  IF io_taint->is_tainted( lv_var ) = abap_true.
                    gv_seq_num = gv_seq_num + 1.
                    ls_result-seq_num = gv_seq_num.
                    ls_result-program = p_prog.
                    ls_result-uname_line = iv_line.
                    ls_result-operation = 'CALL_FUNCTION'.
                    ls_result-target = lv_func.
                    ls_result-field = lv_var.
                    ls_result-taint_path = io_taint->get_taint_path( lv_var ).
                    APPEND ls_result TO et_results.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDWHILE.
            ENDIF.
            lv_idx = lv_idx + 1.
          ENDWHILE.
        ENDIF.

      WHEN 'SELECT'.
        " SELECT statements with tainted WHERE conditions
        DATA: lv_where_found TYPE abap_bool,
              ls_where_token TYPE stokesx.
        lv_where_found = abap_false.

        LOOP AT it_tokens INTO ls_where_token WHERE str = 'WHERE'.
          lv_where_found = abap_true.
          EXIT.
        ENDLOOP.

        IF lv_where_found = abap_true.
          " Check for tainted variables in WHERE clause
          lv_idx = 1.
          WHILE lv_idx <= lines( it_tokens ).
            lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
            IF lv_var = 'WHERE'.
              lv_idx = lv_idx + 1.
              WHILE lv_idx <= lines( it_tokens ).
                lv_var = extract_token_value( it_tokens = it_tokens iv_index = lv_idx ).
                IF io_taint->is_tainted( lv_var ) = abap_true.
                  gv_seq_num = gv_seq_num + 1.
                  ls_result-seq_num = gv_seq_num.
                  ls_result-program = p_prog.
                  ls_result-uname_line = iv_line.
                  ls_result-operation = 'SELECT_WHERE'.
                  ls_result-target = extract_token_value( it_tokens = it_tokens iv_index = 4 ).
                  ls_result-field = lv_var.
                  ls_result-taint_path = io_taint->get_taint_path( lv_var ).
                  APPEND ls_result TO et_results.
                  EXIT.
                ENDIF.
                lv_idx = lv_idx + 1.
              ENDWHILE.
              EXIT.
            ENDIF.
            lv_idx = lv_idx + 1.
          ENDWHILE.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD extract_token_value.
    DATA: ls_token TYPE stokesx.
    CLEAR rv_value.

    READ TABLE it_tokens INTO ls_token INDEX iv_index.
    IF sy-subrc = 0.
      rv_value = ls_token-str.
      " Clean up the value
      REPLACE ALL OCCURRENCES OF '''' IN rv_value WITH ''.
      REPLACE ALL OCCURRENCES OF '`' IN rv_value WITH ''.
      CONDENSE rv_value.
      TRANSLATE rv_value TO UPPER CASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_reporter DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_reporter DEFINITION.
  PUBLIC SECTION.
    METHODS: write_csv
               IMPORTING it_results TYPE STANDARD TABLE
                         iv_file    TYPE string.

  PRIVATE SECTION.
    METHODS: format_csv_line
               IMPORTING is_result TYPE ty_result
               RETURNING VALUE(rv_line) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_reporter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_reporter IMPLEMENTATION.
  METHOD write_csv.
    DATA: lv_line   TYPE string,
          ls_result TYPE ty_result.

    OPEN DATASET iv_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      MESSAGE |Cannot open file { iv_file }| TYPE 'E'.
      RETURN.
    ENDIF.

    " Write header
    lv_line = 'Seq,Program,Line,Operation,Target,Field,TaintPath'.
    TRANSFER lv_line TO iv_file.

    " Write data
    LOOP AT it_results INTO ls_result.
      lv_line = format_csv_line( ls_result ).
      TRANSFER lv_line TO iv_file.
    ENDLOOP.

    CLOSE DATASET iv_file.

    MESSAGE |Results written to { iv_file }| TYPE 'S'.
  ENDMETHOD.

  METHOD format_csv_line.
    rv_line = |{ is_result-seq_num },{ is_result-program },{ is_result-uname_line },|
           && |{ is_result-operation },{ is_result-target },{ is_result-field },|
           && |{ is_result-taint_path }|.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROCESSING
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_scanner TYPE REF TO lcl_scanner,
        lo_taint   TYPE REF TO lcl_taint_tracker,
        lo_parser  TYPE REF TO lcl_parser,
        lo_reporter TYPE REF TO lcl_reporter.

  " Create instances
  CREATE OBJECT lo_scanner.
  CREATE OBJECT lo_taint.
  CREATE OBJECT lo_parser.
  CREATE OBJECT lo_reporter.

  " Initialize taint tracker
  lo_taint->initialize( ).

  " Read program source
  lo_scanner->read_program( EXPORTING iv_program = p_prog
                            IMPORTING et_source = gt_source ).

  IF gt_source IS INITIAL.
    MESSAGE 'No source code found' TYPE 'E'.
    RETURN.
  ENDIF.

  " Parse and analyze
  lo_parser->parse_statements( EXPORTING it_source = gt_source
                                         io_taint = lo_taint
                               IMPORTING et_results = gt_results ).

  " Write results
  IF gt_results IS NOT INITIAL.
    lo_reporter->write_csv( EXPORTING it_results = gt_results
                                      iv_file = p_output ).

    WRITE: / |Analysis complete. Found { lines( gt_results ) } tainted operations|.
  ELSE.
    WRITE: / 'No sy-uname usage found in the program'.
  ENDIF.

END-OF-SELECTION.
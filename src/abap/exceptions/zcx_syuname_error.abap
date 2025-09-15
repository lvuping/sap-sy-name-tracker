CLASS zcx_syuname_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF program_not_found,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'PROGRAM_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF program_not_found,

      BEGIN OF file_write_error,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'FILE_PATH',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF file_write_error,

      BEGIN OF parse_error,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'STATEMENT',
        attr2 TYPE scx_attrname VALUE 'LINE_NUMBER',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parse_error,

      BEGIN OF include_not_found,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'INCLUDE_NAME',
        attr2 TYPE scx_attrname VALUE 'PARENT_PROGRAM',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF include_not_found,

      BEGIN OF invalid_parameter,
        msgid TYPE symsgid VALUE 'ZSYUNAME',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'PARAMETER_NAME',
        attr2 TYPE scx_attrname VALUE 'PARAMETER_VALUE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_parameter.

    DATA: program_name    TYPE programm,
          file_path       TYPE string,
          statement       TYPE string,
          line_number     TYPE i,
          include_name    TYPE programm,
          parent_program  TYPE programm,
          parameter_name  TYPE string,
          parameter_value TYPE string.

    METHODS: constructor
      IMPORTING
        textid          LIKE if_t100_message=>t100key OPTIONAL
        previous        LIKE previous OPTIONAL
        program_name    TYPE programm OPTIONAL
        file_path       TYPE string OPTIONAL
        statement       TYPE string OPTIONAL
        line_number     TYPE i OPTIONAL
        include_name    TYPE programm OPTIONAL
        parent_program  TYPE programm OPTIONAL
        parameter_name  TYPE string OPTIONAL
        parameter_value TYPE string OPTIONAL.

ENDCLASS.

CLASS zcx_syuname_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).

    me->program_name = program_name.
    me->file_path = file_path.
    me->statement = statement.
    me->line_number = line_number.
    me->include_name = include_name.
    me->parent_program = parent_program.
    me->parameter_name = parameter_name.
    me->parameter_value = parameter_value.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
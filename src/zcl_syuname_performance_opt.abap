CLASS zcl_syuname_performance_opt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_tainted_sorted,
             name       TYPE string,
             scope      TYPE string,
             line       TYPE i,
             confidence TYPE string,
           END OF ty_tainted_sorted.

    TYPES: tt_tainted_sorted TYPE SORTED TABLE OF ty_tainted_sorted
                              WITH UNIQUE KEY name scope.

    TYPES: BEGIN OF ty_include_cache,
             program_name TYPE programm,
             timestamp    TYPE timestampl,
             source_code  TYPE string_table,
             hit_count    TYPE i,
           END OF ty_include_cache.

    TYPES: tt_include_cache TYPE HASHED TABLE OF ty_include_cache
                            WITH UNIQUE KEY program_name.

    TYPES: BEGIN OF ty_circular_check,
             parent_prog TYPE programm,
             include_prog TYPE programm,
           END OF ty_circular_check.

    TYPES: tt_circular_check TYPE TABLE OF ty_circular_check.

    CONSTANTS: c_cache_size_limit TYPE i VALUE 100,
               c_cache_ttl_seconds TYPE i VALUE 900,  " 15 minutes
               c_memory_limit_mb TYPE i VALUE 2048,   " 2GB limit
               c_chunk_size TYPE i VALUE 10000.       " Process in chunks

    METHODS: optimize_tainted_table
      IMPORTING
        it_tainted_regular TYPE ANY TABLE
      RETURNING
        VALUE(rt_tainted_sorted) TYPE tt_tainted_sorted,

      create_include_cache
        RETURNING
          VALUE(ro_cache) TYPE REF TO zcl_syuname_performance_opt,

      get_cached_include
        IMPORTING
          iv_program_name TYPE programm
        RETURNING
          VALUE(rt_source) TYPE string_table,

      add_to_cache
        IMPORTING
          iv_program_name TYPE programm
          it_source_code  TYPE string_table,

      check_circular_include
        IMPORTING
          iv_parent_prog  TYPE programm
          iv_include_prog TYPE programm
        RETURNING
          VALUE(rv_is_circular) TYPE abap_bool,

      manage_memory_usage
        IMPORTING
          iv_current_lines TYPE i
        RETURNING
          VALUE(rv_continue) TYPE abap_bool,

      get_memory_usage_mb
        RETURNING
          VALUE(rv_usage_mb) TYPE i,

      clear_old_cache_entries.

  PRIVATE SECTION.
    DATA: mt_include_cache TYPE tt_include_cache,
          mt_circular_stack TYPE tt_circular_check,
          mv_total_memory_mb TYPE i.

    METHODS: calculate_cache_score
      IMPORTING
        is_entry TYPE ty_include_cache
      RETURNING
        VALUE(rv_score) TYPE i,

      evict_lru_cache_entry.

ENDCLASS.

CLASS zcl_syuname_performance_opt IMPLEMENTATION.
  METHOD optimize_tainted_table.
    " Convert regular table to sorted table for binary search
    LOOP AT it_tainted_regular INTO DATA(ls_entry).
      INSERT VALUE #(
        name       = ls_entry-('NAME')
        scope      = ls_entry-('SCOPE')
        line       = ls_entry-('LINE')
        confidence = ls_entry-('CONFIDENCE')
      ) INTO TABLE rt_tainted_sorted.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_include_cache.
    ro_cache = NEW zcl_syuname_performance_opt( ).
    CLEAR ro_cache->mt_include_cache.
  ENDMETHOD.

  METHOD get_cached_include.
    DATA: ls_cache_entry TYPE ty_include_cache.

    " Try to get from cache (O(1) with hashed table)
    READ TABLE mt_include_cache INTO ls_cache_entry
      WITH TABLE KEY program_name = iv_program_name.

    IF sy-subrc = 0.
      " Check if cache is still valid (TTL)
      GET TIME STAMP FIELD DATA(lv_current_time).
      DATA(lv_age_seconds) = cl_abap_tstmp=>subtract(
        tstmp1 = lv_current_time
        tstmp2 = ls_cache_entry-timestamp ).

      IF lv_age_seconds < c_cache_ttl_seconds.
        " Cache hit - increment counter
        ls_cache_entry-hit_count = ls_cache_entry-hit_count + 1.
        MODIFY TABLE mt_include_cache FROM ls_cache_entry.
        rt_source = ls_cache_entry-source_code.
      ELSE.
        " Cache expired - remove entry
        DELETE TABLE mt_include_cache WITH TABLE KEY program_name = iv_program_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD add_to_cache.
    DATA: ls_cache_entry TYPE ty_include_cache.

    " Check cache size limit
    IF lines( mt_include_cache ) >= c_cache_size_limit.
      evict_lru_cache_entry( ).
    ENDIF.

    " Add new entry to cache
    GET TIME STAMP FIELD ls_cache_entry-timestamp.
    ls_cache_entry-program_name = iv_program_name.
    ls_cache_entry-source_code = it_source_code.
    ls_cache_entry-hit_count = 1.

    INSERT ls_cache_entry INTO TABLE mt_include_cache.
  ENDMETHOD.

  METHOD check_circular_include.
    rv_is_circular = abap_false.

    " Check if this include path already exists
    READ TABLE mt_circular_stack TRANSPORTING NO FIELDS
      WITH KEY parent_prog = iv_parent_prog
               include_prog = iv_include_prog.

    IF sy-subrc = 0.
      rv_is_circular = abap_true.
      RETURN.
    ENDIF.

    " Check reverse path (circular dependency)
    READ TABLE mt_circular_stack TRANSPORTING NO FIELDS
      WITH KEY parent_prog = iv_include_prog
               include_prog = iv_parent_prog.

    IF sy-subrc = 0.
      rv_is_circular = abap_true.
      RETURN.
    ENDIF.

    " Add to stack for tracking
    APPEND VALUE #(
      parent_prog = iv_parent_prog
      include_prog = iv_include_prog
    ) TO mt_circular_stack.
  ENDMETHOD.

  METHOD manage_memory_usage.
    rv_continue = abap_true.

    " Get current memory usage
    DATA(lv_usage_mb) = get_memory_usage_mb( ).

    IF lv_usage_mb > c_memory_limit_mb.
      rv_continue = abap_false.

      " Try to free memory
      clear_old_cache_entries( ).

      " Check again after cleanup
      lv_usage_mb = get_memory_usage_mb( ).
      IF lv_usage_mb < c_memory_limit_mb * 90 / 100.  " Below 90% of limit
        rv_continue = abap_true.
      ENDIF.
    ENDIF.

    " Update tracked usage
    mv_total_memory_mb = lv_usage_mb.
  ENDMETHOD.

  METHOD get_memory_usage_mb.
    DATA: lv_memory_info TYPE abap_msize.

    " Get memory usage from system
    CALL 'GET_MEMORY_INFO' ID 'INFO' FIELD lv_memory_info.

    " Convert to MB
    rv_usage_mb = lv_memory_info / 1024 / 1024.
  ENDMETHOD.

  METHOD clear_old_cache_entries.
    DATA: lt_to_delete TYPE TABLE OF programm.

    GET TIME STAMP FIELD DATA(lv_current_time).

    " Find expired entries
    LOOP AT mt_include_cache INTO DATA(ls_entry).
      DATA(lv_age_seconds) = cl_abap_tstmp=>subtract(
        tstmp1 = lv_current_time
        tstmp2 = ls_entry-timestamp ).

      IF lv_age_seconds > c_cache_ttl_seconds.
        APPEND ls_entry-program_name TO lt_to_delete.
      ENDIF.
    ENDLOOP.

    " Delete expired entries
    LOOP AT lt_to_delete INTO DATA(lv_prog).
      DELETE TABLE mt_include_cache WITH TABLE KEY program_name = lv_prog.
    ENDLOOP.

    " Clear circular check stack if too large
    IF lines( mt_circular_stack ) > 1000.
      " Keep only last 500 entries
      DATA(lv_start) = lines( mt_circular_stack ) - 500.
      DELETE mt_circular_stack FROM 1 TO lv_start.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_cache_score.
    " LRU scoring: higher hit count and newer timestamp = higher score
    GET TIME STAMP FIELD DATA(lv_current_time).

    DATA(lv_age_seconds) = cl_abap_tstmp=>subtract(
      tstmp1 = lv_current_time
      tstmp2 = is_entry-timestamp ).

    " Score formula: hit_count * 1000 - age_seconds
    rv_score = is_entry-hit_count * 1000 - lv_age_seconds.
  ENDMETHOD.

  METHOD evict_lru_cache_entry.
    DATA: lv_min_score TYPE i VALUE 999999999,
          lv_evict_prog TYPE programm.

    " Find entry with lowest score
    LOOP AT mt_include_cache INTO DATA(ls_entry).
      DATA(lv_score) = calculate_cache_score( ls_entry ).
      IF lv_score < lv_min_score.
        lv_min_score = lv_score.
        lv_evict_prog = ls_entry-program_name.
      ENDIF.
    ENDLOOP.

    " Evict the entry
    IF lv_evict_prog IS NOT INITIAL.
      DELETE TABLE mt_include_cache WITH TABLE KEY program_name = lv_evict_prog.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
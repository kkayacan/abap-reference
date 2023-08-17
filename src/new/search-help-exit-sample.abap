FUNCTION z_rt_wwgnb_shexit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  BREAK-POINT ID zdevrt00002.

  DATA: BEGIN OF lt_rec OCCURS 0,
          class TYPE m_wwgna-class,
          kschl TYPE m_wwgna-kschl,
          kschg TYPE m_wwgna-kschg,
          spras TYPE m_wwgna-spras,
          klpos TYPE m_wwgna-klpos,
          klart TYPE m_wwgna-klart,
          clint TYPE m_wwgna-clint,
          wwskz TYPE m_wwgna-wwskz,
        END OF lt_rec.

  IF callcontrol-step = 'DISP'.
*    LOOP AT record_tab.
*      lt_rec = record_tab-string.
*      APPEND lt_rec.
*    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM record_tab COMPARING ALL FIELDS.
    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        parameter   = 'CLASS'
        fieldname   = 'CLASS'
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        results_tab = lt_rec
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.

    CALL FUNCTION 'F4UT_PARAMETER_VALUE_GET'
      EXPORTING
        parameter   = 'CLINT'
        fieldname   = 'CLINT'
      TABLES
        shlp_tab    = shlp_tab
        record_tab  = record_tab
        results_tab = lt_rec
      CHANGING
        shlp        = shlp
        callcontrol = callcontrol.



    SELECT wwgna~clint,
           kssk~objek FROM zrt_v_m_wwgna AS wwgna
      INNER JOIN kssk ON kssk~clint EQ wwgna~clint
      INTO TABLE @DATA(lt_wwgna).

    LOOP AT lt_rec.
      READ TABLE  lt_wwgna  WITH KEY objek = lt_rec-clint TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 4.
        DELETE record_tab WHERE string+3(10) = lt_rec-clint.
      ENDIF.
    ENDLOOP.



*    BREAK-POINT.
  ENDIF.


ENDFUNCTION.
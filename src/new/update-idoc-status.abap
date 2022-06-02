FORM update_idoc_status .
    TYPES: BEGIN OF t_idoc,
             docnum TYPE edidc-docnum,
           END OF t_idoc.
  
    DATA: lt_kasa_icmal  TYPE TABLE OF zfit_possumup,
          lt_idoc        TYPE TABLE OF t_idoc,
          lt_idoc_status TYPE STANDARD TABLE OF bdidocstat,
          ls_kasa_icmal  LIKE LINE OF lt_kasa_icmal,
          ls_idoc        TYPE t_idoc,
          ls_idoc_status LIKE LINE OF lt_idoc_status,
          lv_status      TYPE edidc-status,
          lv_is_updated  TYPE xfeld.
  
    LOOP AT gt_rows INTO gs_row.
      READ TABLE gt_data INTO gs_data INDEX gs_row-index.
      IF sy-subrc = 0.
        SELECT docnum FROM  zfit_possumup APPENDING CORRESPONDING FIELDS OF TABLE lt_kasa_icmal "#EC CI_SEL_NESTED
               WHERE  budat = gs_data-budat
               AND    prctr = gs_data-prctr
               AND    paytp = gs_data-paytp.
      ENDIF.
    ENDLOOP.
  
    LOOP AT lt_kasa_icmal INTO ls_kasa_icmal.
      ls_idoc-docnum = ls_kasa_icmal-docnum.
      COLLECT ls_idoc INTO lt_idoc.
    ENDLOOP.
  
    DELETE lt_idoc WHERE docnum = space.
  
    LOOP AT lt_idoc INTO ls_idoc.
      SELECT SINGLE status FROM  edidc INTO lv_status  "#EC CI_SEL_NESTED
             WHERE  docnum  = ls_idoc-docnum.
      IF sy-subrc = 0 AND lv_status = '51'.
        ls_idoc_status-docnum = ls_idoc-docnum.
        ls_idoc_status-status = '53'.
        APPEND ls_idoc_status TO lt_idoc_status.
        CALL FUNCTION 'IDOC_STATUS_WRITE_TO_DATABASE'
          EXPORTING
            idoc_number               = ls_idoc-docnum
          TABLES
            idoc_status               = lt_idoc_status
          EXCEPTIONS
            idoc_foreign_lock         = 1
            idoc_not_found            = 2
            idoc_status_records_empty = 3
            idoc_status_invalid       = 4
            db_error                  = 5
            OTHERS                    = 6.
        IF sy-subrc = 0.
          lv_is_updated = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  
    IF lv_is_updated = 'X'.
      COMMIT WORK.
    ENDIF.
  ENDFORM.
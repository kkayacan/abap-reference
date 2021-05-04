FORM on_before_save.
    DATA: ls_total TYPE zcod_segment_01,
          lt_total LIKE TABLE OF ls_total,
          lv_total TYPE c LENGTH 30.
    LOOP AT total.
      ls_total = <vim_total_struc>.
      CLEAR ls_total-segment.
      COLLECT ls_total INTO lt_total.
    ENDLOOP.
    LOOP AT lt_total INTO ls_total WHERE hr <> 100
                                   OR    hq <> 100
                                   OR    oh <> 100.
      IF ls_total-hr <> 100.
        WRITE ls_total-hr TO lv_total DECIMALS 2.
      ELSEIF ls_total-hq <> 100.
        WRITE ls_total-hq TO lv_total DECIMALS 2.
      ELSEIF ls_total-oh <> 100.
        WRITE ls_total-oh TO lv_total DECIMALS 2.
      ENDIF.
      CONDENSE lv_total.
      MESSAGE s045(zco) WITH ls_total-bukrs ls_total-gjahr ls_total-poper lv_total DISPLAY LIKE 'E'.
  * Comp: & period: & & total must be 100. Currently &
      vim_abort_saving = abap_true.
      EXIT.
    ENDLOOP.
    IF vim_abort_saving = abap_true.
      sy-subrc = 4.
    ELSE.
      sy-subrc = 0.
    ENDIF.
  ENDFORM.
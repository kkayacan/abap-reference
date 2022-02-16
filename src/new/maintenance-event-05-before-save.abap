FORM before_save.

    TYPES: BEGIN OF lst_channel,
             key TYPE zoaa_rsicalcg-channel,
           END OF lst_channel.
    DATA: BEGIN OF ls_total.
            INCLUDE STRUCTURE zoaa_rsicalcg.
            INCLUDE STRUCTURE vimtbflags.
    DATA: END OF ls_total.
    DATA: lt_calc    TYPE TABLE OF zoaa_rsicalcg,
          ls_calc    TYPE zoaa_rsicalcg,
          ls_oos     TYPE zoaa_rsicalcg,
          ls_lim     TYPE zoaa_rsicalcg,
          ls_ons     TYPE zoaa_rsicalcg,
          lt_channel TYPE TABLE OF lst_channel,
          ls_channel TYPE lst_channel.
    FIELD-SYMBOLS: <ls_xfrom> TYPE x, "Hexadecimal value of from value
                   <ls_xto>   TYPE x. "Hexadecimal value of to value
  
    LOOP AT total.
      IF <action> <> geloescht        AND
         <action> <> update_geloescht AND
         <action> <> neuer_geloescht.
        ASSIGN total TO <ls_xfrom> CASTING.
        ASSIGN ls_calc TO <ls_xto> CASTING.
        <ls_xto> = <ls_xfrom>.
        APPEND ls_calc TO lt_calc.
        ls_channel-key = ls_calc-channel.
        COLLECT ls_channel INTO lt_channel.
      ENDIF.
    ENDLOOP.
  
    LOOP AT lt_channel INTO ls_channel.
      CLEAR: ls_ons, ls_lim, ls_oos.
      READ TABLE lt_calc INTO ls_oos WITH KEY channel           = ls_channel-key
                                              rough_stock_value = 'out_of_stock'.
      READ TABLE lt_calc INTO ls_lim WITH KEY channel           = ls_channel-key
                                              rough_stock_value = 'limited'.
      READ TABLE lt_calc INTO ls_ons WITH KEY channel           = ls_channel-key
                                              rough_stock_value = 'on_stock'.
      IF NOT ( ls_oos-quantity_high > 0 AND ls_lim-quantity_high > 0 AND ls_ons-quantity_high > 0 ).
        MESSAGE ID 'ZOAA' TYPE 'S' NUMBER '030' WITH ls_channel-key DISPLAY LIKE 'E'.
        vim_abort_saving = 'X'.
        sy-subrc = 4.
        RETURN.
      ENDIF.
      IF NOT ( ls_ons-quantity_high > ls_ons-quantity_low AND
               ls_lim-quantity_high > ls_lim-quantity_low AND
               ls_oos-quantity_high > ls_oos-quantity_low ).
        MESSAGE ID 'ZOAA' TYPE 'S' NUMBER '030' WITH ls_channel-key DISPLAY LIKE 'E'.
        vim_abort_saving = 'X'.
        sy-subrc = 4.
        RETURN.
      ENDIF.
      IF NOT ( ( ls_ons-quantity_low - ls_lim-quantity_high = 1 ) AND
               ( ls_lim-quantity_low - ls_oos-quantity_high = 1 ) ).
        MESSAGE ID 'ZOAA' TYPE 'S' NUMBER '030' WITH ls_channel-key DISPLAY LIKE 'E'.
        vim_abort_saving = 'X'.
        sy-subrc = 4.
        RETURN.
      ENDIF.
    ENDLOOP.
  
    sy-subrc = 0.
  
  ENDFORM.
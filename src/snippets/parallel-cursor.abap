  SORT lt_cards BY card_type memb_id card_valid_from DESCENDING.

  DATA(lv_tabix) = 1.

  LOOP AT lt_cards ASSIGNING FIELD-SYMBOL(<ls_active>).
    IF <ls_active>-active_object_id IS INITIAL.
      <ls_active>-active_object_id = <ls_active>-object_id.
    ENDIF.
    DATA(lv_found) = abap_false.
    LOOP AT lt_cards ASSIGNING FIELD-SYMBOL(<ls_all>) FROM lv_tabix.
      IF <ls_all>-card_type < <ls_active>-card_type OR
         <ls_all>-memb_id   < <ls_active>-memb_id.
        CONTINUE.
      ELSEIF <ls_all>-card_type = <ls_active>-card_type AND
             <ls_all>-memb_id   = <ls_active>-memb_id.
        IF lv_found = abap_false.
          lv_tabix = sy-tabix.
          lv_found = abap_true.
        ENDIF.
        IF <ls_all>-object_id       <> <ls_active>-object_id AND
           <ls_all>-card_valid_from <= <ls_active>-card_valid_from.
          <ls_all>-active_object_id = <ls_active>-object_id.
        ENDIF.
      ELSEIF <ls_all>-card_type > <ls_active>-card_type OR
             <ls_all>-memb_id   > <ls_active>-memb_id.
        IF lv_found = abap_false.
          lv_tabix = sy-tabix.
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
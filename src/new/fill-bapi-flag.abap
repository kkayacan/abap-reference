FORM fill_bapi_flag USING it_bapi  TYPE STANDARD TABLE
                 CHANGING ct_bapix TYPE STANDARD TABLE.

  DATA: lo_sdescr TYPE REF TO cl_abap_structdescr,
        lo_edescr TYPE REF TO cl_abap_elemdescr.

  APPEND INITIAL LINE TO ct_bapix ASSIGNING FIELD-SYMBOL(<ls_bapix>).
  lo_sdescr ?= cl_abap_structdescr=>describe_by_data( <ls_bapix> ).
  DATA(lt_comp) = lo_sdescr->get_components( ).

  CLEAR ct_bapix[].

  LOOP AT it_bapi ASSIGNING FIELD-SYMBOL(<ls_bapi>).
    APPEND INITIAL LINE TO ct_bapix ASSIGNING <ls_bapix>.
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      lo_edescr ?= <ls_comp>-type.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_bapix> TO FIELD-SYMBOL(<lv_bapix>).
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <ls_bapi> TO FIELD-SYMBOL(<lv_bapi>).
      IF sy-subrc = 0.
        IF lo_edescr->output_length = '1'.
          IF <lv_bapi> IS NOT INITIAL.
            <lv_bapix> = abap_true.
          ENDIF.
        ELSE.
          <lv_bapix> = <lv_bapi>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
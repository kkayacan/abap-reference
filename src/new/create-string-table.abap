FORM create_string_table USING itab    TYPE ANY TABLE
                      CHANGING er_itab TYPE REF TO data.

  DATA lr_line TYPE REF TO data.

  CREATE DATA lr_line LIKE LINE OF itab.
  ASSIGN lr_line->* TO FIELD-SYMBOL(<stru>).
  DATA(lo_stru) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <stru> ) ).
  DATA(lt_comp) = lo_stru->get_components( ).

  LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
    <ls_comp>-type = cl_abap_elemdescr=>get_string( ).
  ENDLOOP.

  DATA(lo_tabledescr) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_comp ) ).
  CREATE DATA er_itab TYPE HANDLE lo_tabledescr.

ENDFORM.
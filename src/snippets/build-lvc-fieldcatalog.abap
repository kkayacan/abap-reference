FORM build_fcat USING it_list TYPE ANY TABLE
             CHANGING ct_fcat TYPE lvc_t_fcat.
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lo_salv)
    CHANGING
      t_table      = it_list ).

  ct_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = lo_salv->get_columns( )
        r_aggregations = lo_salv->get_aggregations( ) ).

  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    <ls_fcat>-no_sign = abap_false.
  ENDLOOP.
ENDFORM.
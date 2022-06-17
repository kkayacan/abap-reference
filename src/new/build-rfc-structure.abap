METHOD get_name.

    TYPES: BEGIN OF lst_plant,
             plant TYPE werks_d,
           END OF lst_plant,
           ltt_plant TYPE STANDARD TABLE OF lst_plant WITH DEFAULT KEY.

    DATA: lt_fields     TYPE STANDARD TABLE OF rfc_fields,
          lt_comp       TYPE cl_abap_structdescr=>component_table,
          lr_plant_data TYPE REF TO data.

    FIELD-SYMBOLS <lt_plant_data> TYPE INDEX TABLE.

    CALL FUNCTION 'RFC_GET_STRUCTURE_DEFINITION' DESTINATION lcl_plant=>c_destination
      EXPORTING
        tabname          = 'T001W'
      TABLES
        fields           = lt_fields
      EXCEPTIONS
        table_not_active = 1
        OTHERS           = 2.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
      APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
      <ls_comp>-name = <ls_field>-fieldname.
      CASE <ls_field>-exid.
        WHEN 'I'. <ls_comp>-type = cl_abap_elemdescr=>get_i( ).
        WHEN 'D'. <ls_comp>-type = cl_abap_elemdescr=>get_d( ).
        WHEN 'T'. <ls_comp>-type = cl_abap_elemdescr=>get_t( ).
        WHEN 'C'. <ls_comp>-type = cl_abap_elemdescr=>get_c( CONV i( <ls_field>-intlength ) ).
        WHEN 'N'. <ls_comp>-type = cl_abap_elemdescr=>get_n( CONV i( <ls_field>-intlength ) ).
        WHEN 'P'. <ls_comp>-type = cl_abap_elemdescr=>get_p( p_length = CONV i( <ls_field>-intlength ) p_decimals = CONV i( <ls_field>-decimals ) ).
      ENDCASE.
    ENDLOOP.

    DATA(lo_structdescr) = cl_abap_structdescr=>create( lt_comp ).
    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( lo_structdescr ).
    CREATE DATA lr_plant_data TYPE HANDLE lo_tabledescr.
    ASSIGN lr_plant_data->* TO <lt_plant_data>.

    CALL FUNCTION 'ISIDE_CE_GET_PLANT_TXT' DESTINATION lcl_plant=>c_destination
      EXPORTING
        it_plant      = VALUE ltt_plant( ( plant = iv_werks ) )
      IMPORTING
        et_plant_data = <lt_plant_data>.

    READ TABLE <lt_plant_data> ASSIGNING FIELD-SYMBOL(<ls_plant>) INDEX 1.
    IF <ls_plant> IS ASSIGNED.
      ASSIGN COMPONENT 'NAME1' OF STRUCTURE <ls_plant> to FIELD-SYMBOL(<lv_name1>).
      IF <lv_name1> IS ASSIGNED.
        rv_name1 = <lv_name1>.
      ENDIF.
    ENDIF.

  ENDMETHOD.
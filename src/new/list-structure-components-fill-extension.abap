METHOD fill_container.
* IS_VALUE TYPE ANY
* ES_CONTAINER TYPE CSEQUENCE

  DATA lv_offset TYPE i.

  LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_value ) )->get_components( ) INTO DATA(ls_comp).
    DATA(lref_type) = CAST cl_abap_elemdescr( ls_comp-type ).
    ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_value TO FIELD-SYMBOL(<lv_field>).
    IF sy-subrc = 0 AND <lv_field> IS NOT INITIAL.
      es_container+lv_offset(lref_type->output_length) = <lv_field>.
    ENDIF.
    ADD lref_type->output_length TO lv_offset.
  ENDLOOP.

ENDMETHOD.
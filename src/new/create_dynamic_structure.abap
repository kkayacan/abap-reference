METHOD build_extension_structures.

  DATA: lt_dfies TYPE TABLE OF dfies,
        lt_comp  TYPE cl_abap_structdescr=>component_table.

  CALL FUNCTION 'DDIF_FIELDINFO_GET' DESTINATION iv_destination
    EXPORTING
      tabname        = iv_structure
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
  <ls_comp>-name = 'PREQ_ITEM'.
  <ls_comp>-type = cl_abap_elemdescr=>get_n( 5 ).

  LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_dfies>-fieldname.
    CASE <ls_dfies>-inttype.
      WHEN 'I'. <ls_comp>-type = cl_abap_elemdescr=>get_i( ).
      WHEN 'D'. <ls_comp>-type = cl_abap_elemdescr=>get_d( ).
      WHEN 'T'. <ls_comp>-type = cl_abap_elemdescr=>get_t( ).
      WHEN 'C'. <ls_comp>-type = cl_abap_elemdescr=>get_c( CONV i( <ls_dfies>-leng ) ).
      WHEN 'N'. <ls_comp>-type = cl_abap_elemdescr=>get_n( CONV i( <ls_dfies>-leng ) ).
      WHEN 'P'. <ls_comp>-type = cl_abap_elemdescr=>get_p( p_length = CONV i( <ls_dfies>-leng ) p_decimals = CONV i( <ls_dfies>-decimals ) ).
    ENDCASE.
  ENDLOOP.

  DATA(lo_structdescr) = cl_abap_structdescr=>create( lt_comp ).
  CREATE DATA rs_extstruc-bapi_te_mereqitem TYPE HANDLE lo_structdescr.

  CLEAR lt_comp.

  APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
  <ls_comp>-name = 'PREQ_ITEM'.
  <ls_comp>-type = cl_abap_elemdescr=>get_n( 5 ).

  LOOP AT lt_dfies ASSIGNING <ls_dfies>.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_dfies>-fieldname.
    <ls_comp>-type = cl_abap_elemdescr=>get_c( 1 ).
  ENDLOOP.

  lo_structdescr = cl_abap_structdescr=>create( lt_comp ).
  CREATE DATA rs_extstruc-bapi_te_mereqitemx TYPE HANDLE lo_structdescr.

ENDMETHOD.
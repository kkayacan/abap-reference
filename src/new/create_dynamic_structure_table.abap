METHOD create_result_table.

  DATA: lt_dfies       TYPE TABLE OF dfies,
        lt_comp        TYPE cl_abap_structdescr=>component_table,
        lo_structdescr TYPE REF TO cl_abap_structdescr.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = iv_tabname
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
    APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
    <ls_comp>-name = <ls_dfies>-fieldname.
    CASE <ls_dfies>-inttype.
      WHEN 'I'. <ls_comp>-type = cl_abap_elemdescr=>get_i( ).
      WHEN 'b'. <ls_comp>-type = cl_abap_elemdescr=>get_int1( ).
      WHEN 's'. <ls_comp>-type = cl_abap_elemdescr=>get_int2( ).
      WHEN 'D'. <ls_comp>-type = cl_abap_elemdescr=>get_d( ).
      WHEN 'T'. <ls_comp>-type = cl_abap_elemdescr=>get_t( ).
      WHEN 'C'. <ls_comp>-type = cl_abap_elemdescr=>get_c( CONV i( <ls_dfies>-leng ) ).
      WHEN 'N'. <ls_comp>-type = cl_abap_elemdescr=>get_n( CONV i( <ls_dfies>-leng ) ).
      WHEN 'P'. <ls_comp>-type = cl_abap_elemdescr=>get_p( p_length = CONV i( <ls_dfies>-leng ) p_decimals = CONV i( <ls_dfies>-decimals ) ).
    ENDCASE.
  ENDLOOP.

  lo_structdescr = cl_abap_structdescr=>create( lt_comp ).
  DATA(lo_tabledescr) = cl_abap_tabledescr=>create( lo_structdescr ).
  CREATE DATA er_tab TYPE HANDLE lo_tabledescr.

ENDMETHOD.

************************************************************************

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

************************************************************************

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

*********************************************************************************

DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
lr_line TYPE REF TO data.
FIELD-SYMBOLS: <lt_list> TYPE STANDARD TABLE,
         <ls_list> TYPE any.

APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
<ls_comp>-name = 'LIFNR'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'LIFNR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'NAME1'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'NAME1_GP' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'BPKIND'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_BPKIND' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'TEXT40'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_TEXT40' ).
IF p_odeme = abap_true.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'XREF3'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'XREF3' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'BELNR'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BELNR_D' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'BLDAT'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BLDAT' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'BUDAT'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BUDAT' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'VADE_TARIHI'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DZFBDT' ).
ENDIF.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WAERS'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WAERS' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
IF p_stcoz = abap_true.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR_M'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR_M'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR_G'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR_G'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR_A'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR_A'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
ENDIF.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR_BF'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR_BF'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).

DATA(lv_date_h) = CONV d( |{ p_gjahr }1201| ).
DATA(lv_date_l) = CONV d( |{ p_gjahr }0101| ).
DO.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'W' && lv_date_l(6).
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'D' && lv_date_l(6).
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
CALL FUNCTION 'OIL_GET_NEXT_MONTH'
EXPORTING
i_date = lv_date_l
IMPORTING
e_date = lv_date_l.
IF lv_date_l > lv_date_h.
EXIT.
ENDIF.
ENDDO.
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'WRBTR_AF'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
<ls_comp>-name = 'DMBTR_AF'.
<ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
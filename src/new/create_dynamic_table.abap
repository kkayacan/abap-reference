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
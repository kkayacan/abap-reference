DATA: lt_mwdat             TYPE STANDARD TABLE OF rtax1u15.

CLEAR lt_mwdat.
CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
  EXPORTING
    i_bukrs = ls_poheader-comp_code
    i_mwskz = <ls_item_bapi>-tax_code
    i_waers = lv_waers
    i_wrbtr = CONV bseg-wrbtr( <ls_item_scrn>-brtwr )
  TABLES
    t_mwdat = lt_mwdat
  EXCEPTIONS
    OTHERS  = 19.
IF lt_mwdat IS NOT INITIAL.
  <ls_item_bapi>-net_price = lt_mwdat[ 1 ]-kawrt.
ENDIF.
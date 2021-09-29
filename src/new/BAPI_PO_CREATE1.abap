FUNCTION z_create_po.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_PO_HEAD) TYPE  ZST_POCREATE_HEAD
*"     VALUE(IT_PO_ITEM) TYPE  ZTT_POCREATE_ITEM
*"     VALUE(IT_ATTACHMENT) TYPE  ZTT_ATTACHMENT OPTIONAL
*"  EXPORTING
*"     VALUE(EV_EBELN) TYPE  EBELN
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA: ls_poheader          TYPE bapimepoheader,
        ls_poheaderx         TYPE bapimepoheaderx,
        lt_poitem            TYPE STANDARD TABLE OF bapimepoitem,
        lt_poitemx           TYPE STANDARD TABLE OF bapimepoitemx,
        lv_po_item           TYPE bapimepoitem-po_item,
        lv_assigned_store_id TYPE werks_d,
        lt_mwdat             TYPE STANDARD TABLE OF rtax1u15.

  TRY.
      cl_wsrs_plant=>get_and_check_plant(
        CHANGING
          cv_plant = lv_assigned_store_id ).
    CATCH cx_wsrs_plant INTO DATA(lx_wsrs_plant).
      et_return = VALUE #( ( type = 'E' id = 'ZFIORI_PO' number = '001' ) ).
      RETURN.
  ENDTRY.

  SELECT SINGLE t001k~bukrs AS comp_code, t001w~ekorg AS purch_org
    FROM t001k
    JOIN t001w ON t001w~bwkey = t001k~bwkey
    WHERE t001w~werks = @lv_assigned_store_id
    INTO CORRESPONDING FIELDS OF @ls_poheader.

  SELECT low FROM tvarvc
    WHERE name = 'FAPP4_EKGRP'
    INTO TABLE @DATA(lt_ekgrp).
  LOOP AT lt_ekgrp ASSIGNING FIELD-SYMBOL(<ls_ekgrp>).
    IF <ls_ekgrp>-low(4) = ls_poheader-purch_org.
      ls_poheader-pur_group = <ls_ekgrp>-low+5(3).
    ENDIF.
  ENDLOOP.

  SELECT SINGLE partner1 FROM but050
    WHERE partner2   = @lv_assigned_store_id
      AND date_to   >= @is_po_head-aedat
      AND date_from <= @is_po_head-aedat
      AND reltyp     = 'Z00001'
    INTO @ls_poheader-vendor.

  ls_poheader-doc_type   = 'Z004'.
  ls_poheader-creat_date = is_po_head-aedat.
  ls_poheader-doc_date   = is_po_head-aedat.
  ls_poheader-ref_1      = is_po_head-ihrez.

  ls_poheaderx-comp_code  = abap_true.
  ls_poheaderx-doc_type   = abap_true.
  ls_poheaderx-creat_date = abap_true.
  ls_poheaderx-vendor     = abap_true.
  ls_poheaderx-purch_org  = abap_true.
  ls_poheaderx-pur_group  = abap_true.
  ls_poheaderx-doc_date   = abap_true.
  ls_poheaderx-ref_1      = abap_true.

  SELECT SINGLE waers FROM t001
    WHERE bukrs = @ls_poheader-comp_code
    INTO @DATA(lv_waers).

  LOOP AT it_po_item ASSIGNING FIELD-SYMBOL(<ls_item_scrn>).
    ADD 10 TO lv_po_item.
    APPEND INITIAL LINE TO lt_poitem ASSIGNING FIELD-SYMBOL(<ls_item_bapi>).
    <ls_item_bapi>-po_item  = lv_po_item.
    <ls_item_bapi>-material = <ls_item_scrn>-matnr.
    <ls_item_bapi>-plant    = lv_assigned_store_id.
    <ls_item_bapi>-stge_loc = '0001'.
    <ls_item_bapi>-quantity = <ls_item_scrn>-menge.
    <ls_item_bapi>-po_unit  = <ls_item_scrn>-meins.
    SELECT SINGLE mwskz FROM ztax
      WHERE percent = @<ls_item_scrn>-tax_percent
        AND begda  <= @is_po_head-aedat
        AND endda  >= @is_po_head-aedat
      INTO @<ls_item_bapi>-tax_code.

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
    <ls_item_bapi>-price_unit = <ls_item_scrn>-menge.
    <ls_item_bapi>-po_price   = '2'.

    <ls_item_bapi>-period_ind_expiration_date = 'D'.

    APPEND INITIAL LINE TO lt_poitemx ASSIGNING FIELD-SYMBOL(<ls_itemx>).
    <ls_itemx>-po_item    = <ls_item_bapi>-po_item.
    <ls_itemx>-po_itemx   = abap_true.
    <ls_itemx>-material   = abap_true.
    <ls_itemx>-plant      = abap_true.
    <ls_itemx>-stge_loc   = abap_true.
    <ls_itemx>-quantity   = abap_true.
    <ls_itemx>-po_unit    = abap_true.
    <ls_itemx>-tax_code   = abap_true.
    <ls_itemx>-net_price  = abap_true.
    <ls_itemx>-price_unit = abap_true.
    <ls_itemx>-po_price   = abap_true.
    <ls_itemx>-period_ind_expiration_date = ''.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = ls_poheader
      poheaderx        = ls_poheaderx
    IMPORTING
      exppurchaseorder = ev_ebeln
    TABLES
      return           = et_return
      poitem           = lt_poitem
      poitemx          = lt_poitemx.

  IF NOT line_exists( et_return[ type = 'E' ] ).
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

ENDFUNCTION.
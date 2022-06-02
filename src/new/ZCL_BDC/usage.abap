DATA(lo_po) = NEW zcl_bdc( ).

lo_po->bdc_dynpro( program = 'SAPLMEGUI' dynpro  = '0014' ).
lo_po->bdc_field( fnam = 'BDC_OKCODE' fval = '=MECHOB' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               0010SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               0030SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               2000SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1105SUB1' ).
lo_po->bdc_field( fnam = 'MEPO_TOPLINE-BSART' fval = 'NB' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             2000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4001SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             2000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB3' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4002SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1301SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               6000SUB1' ).
lo_po->bdc_field( fnam = 'DYN_6000-LIST' fval = '   1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1303SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1102TABSTRIPCONTROL1SUB' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1319SUB1' ).
lo_po->bdc_field( fnam = 'BDC_CURSOR' fval = 'MEPO1319-MATKL' ).
*    lo_po->bdc_field( fnam = 'MEPO1319-SPINF' fval = 'X' ).
lo_po->bdc_dynpro( program = 'SAPLMEGUI' dynpro  = '0002' ).
lo_po->bdc_field( fnam = 'BDC_OKCODE' fval = '=MEOK' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               0003SUB0' ).
lo_po->bdc_field( fnam = 'BDC_CURSOR' fval = 'MEPO_SELECT-EBELN' ).
lo_po->bdc_field( fnam = 'MEPO_SELECT-EBELN' fval = ls_lips-vgbel ).
lo_po->bdc_field( fnam = 'MEPO_SELECT-BSTYP_F' fval = 'X' ).
lo_po->bdc_dynpro( program = 'SAPLMEGUI' dynpro  = '0014' ).
lo_po->bdc_field( fnam = 'BDC_OKCODE' fval = '=MENACH' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               0010SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               0030SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               2000SUB0' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1105SUB1' ).
lo_po->bdc_field( fnam = 'MEPO_TOPLINE-BSART' fval = 'NB' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             2000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4001SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             2000SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1100SUB3' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             4002SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1200SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1301SUB1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               6000SUB1' ).
lo_po->bdc_field( fnam = 'DYN_6000-LIST' fval = '   1' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1303SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1102TABSTRIPCONTROL1SUB' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEGUI                               1319SUB1' ).
*    lo_po->bdc_field( fnam = 'MEPO1319-SPINF' fval = 'X' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1000SUB2' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1000SUB3' ).
lo_po->bdc_field( fnam = 'BDC_SUBSCR' fval = 'SAPLMEVIEWS                             1000SUB4' ).
lo_po->bdc_dynpro( program = 'SAPDV70A' dynpro  = '0100' ).
lo_po->bdc_field( fnam = 'BDC_CURSOR' fval = 'DV70A-STATUSICON(02)' ).
lo_po->bdc_field( fnam = 'BDC_OKCODE' fval = '=V70R' ).
lo_po->bdc_field( fnam = 'DV70A-SELKZ(02)' fval = 'X' ).
lo_po->bdc_dynpro( program = 'SAPDV70A' dynpro  = '0100' ).
lo_po->bdc_field( fnam = 'BDC_CURSOR' fval = 'DNAST-KSCHL(02)' ).
lo_po->bdc_field( fnam = 'BDC_OKCODE' fval = '=V70S' ).

DATA(lv_mode)   = CONV ctu_mode('N').
DATA(lv_update) = CONV ctu_update('S').

DATA(lt_msg) = lo_po->bdc_transaction(
    p_tcode  = 'ME22N'
    p_ctu    = abap_true
    p_mode   = lv_mode
    p_update = lv_update ).
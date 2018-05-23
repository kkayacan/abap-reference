AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kschl-low.
  PERFORM get_kschl USING 'S_KSCHL-LOW'.

FORM get_kschl USING p_fname.

  DATA: BEGIN  OF ls_help,
          kschl LIKE zor_rt_r003-kschl,
          vtext LIKE t685t-vtext,
        END  OF ls_help,
        lt_help LIKE  TABLE  OF ls_help.

  SELECT DISTINCT z~kschl, t685t~vtext
    FROM zor_rt_r003 AS z
    LEFT OUTER JOIN t685t ON t685t~spras = @sy-langu
                         AND t685t~kvewe = 'A'
                         AND t685t~kappl = 'M'
                         AND t685t~kschl = z~kschl
    INTO TABLE @lt_help
    WHERE z~kschl <> @space
    ORDER BY z~kschl.

  PERFORM call_value_request TABLES  lt_help
              USING  'KSCHL' 'VTEXT'  sy-dynnr  p_fname.

ENDFORM.

FORM call_value_request  TABLES pt_value_tab
                           USING    pv_retfield
                                    pv_pvalkey
                                    pv_dynpnr
                                    pv_dynprofield.

  DATA:
    lv_dynpnr      TYPE sydynnr,
    lv_dynprofield TYPE dynfnam.

  lv_dynpnr      = pv_dynpnr.
  lv_dynprofield = pv_dynprofield.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = pv_retfield
      pvalkey         = pv_pvalkey
      dynpprog        = sy-repid
      dynpnr          = lv_dynpnr
      dynprofield     = lv_dynprofield
      value_org       = 'S'
    TABLES
      value_tab       = pt_value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
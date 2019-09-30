FORM calculate_tax_amount  USING    ip_bukrs
                                    ip_belnr
                                    ip_gjahr
                                    ip_buzei
                                    ip_dmbtr
                                    ip_waers
                           CHANGING cp_mwskz
                                    cp_percent
                                    cp_amount.

  DATA lt_mwdat TYPE TABLE OF rtax1u15.

  CLEAR: cp_mwskz, cp_percent, cp_amount.

  SELECT SINGLE mwskz FROM rseg INTO @cp_mwskz       "#EC CI_SEL_NESTED
    WHERE belnr = @ip_belnr
      AND gjahr = @ip_gjahr
      AND buzei = @ip_buzei.

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs                 = ip_bukrs
      i_mwskz                 = cp_mwskz
      i_waers                 = ip_waers
      i_wrbtr                 = ip_dmbtr
    TABLES
      t_mwdat                 = lt_mwdat
    EXCEPTIONS
      bukrs_not_found         = 1
      country_not_found       = 2
      mwskz_not_defined       = 3
      mwskz_not_valid         = 4
      account_not_found       = 5
      different_discount_base = 6
      different_tax_base      = 7
      txjcd_not_valid         = 8
      not_found               = 9
      ktosl_not_found         = 10
      kalsm_not_found         = 11
      parameter_error         = 12
      knumh_not_found         = 13
      kschl_not_found         = 14
      unknown_error           = 15
      OTHERS                  = 16.

  READ TABLE lt_mwdat ASSIGNING FIELD-SYMBOL(<ls_mwdat>) INDEX 1.
  IF sy-subrc = 0.
    cp_amount = <ls_mwdat>-wmwst.
    WRITE <ls_mwdat>-msatz TO cp_percent DECIMALS 0.
    CONDENSE cp_percent.
  ENDIF.

ENDFORM.

FORM calc_tax_from_net USING ip_bukrs
                             ip_mwskz
                             ip_waers
                             ip_wrbtr
                    CHANGING cp_percent
                             cp_amount.

  DATA lt_mwdat TYPE TABLE OF rtax1u15.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs           = ip_bukrs
      i_mwskz           = ip_mwskz
      i_waers           = ip_waers
      i_wrbtr           = ip_wrbtr
    TABLES
      t_mwdat           = lt_mwdat
    EXCEPTIONS
      bukrs_not_found   = 1
      country_not_found = 2
      mwskz_not_defined = 3
      mwskz_not_valid   = 4
      ktosl_not_found   = 5
      kalsm_not_found   = 6
      parameter_error   = 7
      knumh_not_found   = 8
      kschl_not_found   = 9
      unknown_error     = 10
      account_not_found = 11
      txjcd_not_valid   = 12
      OTHERS            = 13.

  READ TABLE lt_mwdat ASSIGNING FIELD-SYMBOL(<ls_mwdat>) INDEX 1.
  IF sy-subrc = 0.
    WRITE <ls_mwdat>-msatz TO cp_percent DECIMALS 0.
    CONDENSE cp_percent.
    cp_amount = <ls_mwdat>-wmwst.
  ENDIF.

ENDFORM.
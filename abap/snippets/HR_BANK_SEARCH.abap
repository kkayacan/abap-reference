PROCESS ON VALUE-REQUEST.
  FIELD P0009-BANKL MODULE LIST_BANKL.

MODULE LIST_BANKL.
  PERFORM BANK_SEARCH USING 'P0009-BANKS'  "country
                            'P0009-BANKL'  "bank key
                            'P0009-BANKN'. "bank account
ENDMODULE.

FORM bank_search USING p_dynpfield_country LIKE feld-name
                       p_dynpfield_bankkey LIKE feld-name
                       p_dynpfield_bankacc LIKE feld-name.

  CALL FUNCTION 'HR_BANK_SEARCH'
    EXPORTING
      p_dynpfield_country  = p_dynpfield_country
      p_dynpfield_bankkey  = p_dynpfield_bankkey
      p_dynpfield_bankacc  = p_dynpfield_bankacc
      p_dynpfield_bankname = 'BNKA_BF-BANKA'
    EXCEPTIONS
      fieldname_not_valid  = 1
      country_not_found    = 2
      OTHERS               = 3.

  IF sy-subrc EQ 2.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
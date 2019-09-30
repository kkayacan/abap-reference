CALL FUNCTION 'FI_TAX_GET_TAX_ACCOUNTS'
  EXPORTING
    i_mwskz         = ls_kdv-mwskz
    i_bukrs         = p_bukrs
    i_ktosl         = 'MWS'
  IMPORTING
    e_t030k         = ls_t030k
  EXCEPTIONS
    bukrs_not_found = 1
    entry_not_found = 2
    parameter_error = 3
    OTHERS          = 4.

ls_actx-gl_account = ls_t030k-konts.
ls_actx-acct_key   = ls_t030k-ktosl.
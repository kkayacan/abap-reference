CALL FUNCTION 'SUSR_DDL_USERS_BY_AUTHVALUES'
EXPORTING
  id_suso  = 'ZMRM_WFSTR'
  id_fld1  = 'WERKS'
  id_val1  = CONV xuval( lv_werks )
IMPORTING
  et_users = lt_users.
READ TABLE lt_users INTO ls_user INDEX 1.
ls_badi_approver-businessuser = ls_user-bname.
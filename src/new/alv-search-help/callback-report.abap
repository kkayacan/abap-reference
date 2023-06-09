REPORT zqm_r_subroutine.

FORM posnr_help_cb TABLES   record_tab STRUCTURE seahlpres
                 CHANGING shlp TYPE shlp_descr_t
                          callcontrol LIKE ddshf4ctrl.
  DATA lv_vbeln TYPE vbrk-vbeln.
  GET PARAMETER ID 'VF' FIELD lv_vbeln.
  IF lv_vbeln IS NOT INITIAL.
    APPEND VALUE #( shlpname  = 'F4_POSNR_VBRP'
                    shlpfield = 'VBELN'
                    sign      = 'I'
                    option    = 'EQ'
                    low       = lv_vbeln ) TO shlp-selopt.
  ENDIF.
ENDFORM.
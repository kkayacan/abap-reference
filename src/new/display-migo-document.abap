    CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        i_action            = 'A04'
        i_refdoc            = 'R02'
        i_notree            = 'X'
        i_no_auth_check     = ' '
        i_deadend           = 'X'
        i_skip_first_screen = 'X'
        i_okcode            = 'OK_GO'
        i_mblnr             = <ls_list>-mblnr
        i_mjahr             = <ls_list>-mjahr.
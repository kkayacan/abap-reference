DATA lt_texttab       TYPE STANDARD TABLE OF tline-tdline.

  CALL FUNCTION 'ISU_POPUP_TEXT_EDIT'
    EXPORTING
      x_title       = TEXT-133
    CHANGING
      xy_texttab    = lt_texttab
    EXCEPTIONS
      general_fault = 1
      OTHERS        = 2.


              CALL FUNCTION 'RECA_GUI_TEXTEDIT_POPUP'
                EXPORTING
                  if_readonly = abap_true
                  id_tdobject = 'EBANH'
                  id_tdname   = CONV thead-tdname( ls_main_alv-banfn )
                  id_tdid     = 'B01'
                  id_tdspras  = 'T'
                  id_activity = '03'
                  id_title    = TEXT-087
                EXCEPTIONS
                  error       = 1
                  OTHERS      = 2.
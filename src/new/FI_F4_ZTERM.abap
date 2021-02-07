              IF mode = display.
                lv_xshow = abap_true.
              ENDIF.
              CALL FUNCTION 'FI_F4_ZTERM'
                EXPORTING
                  i_koart       = 'K'
*                 i_zterm       = mepo1226-zterm
                  i_xshow       = lv_xshow
                IMPORTING
                  e_zterm       = <ls_payment>-zterm
                EXCEPTIONS
                  nothing_found = 01.
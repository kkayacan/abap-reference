  METHOD on_addrdet.

    DATA lt_dia TYPE STANDARD TABLE OF addr1_dia.
    FIELD-SYMBOLS <ls_dia> LIKE LINE OF lt_dia.

    APPEND INITIAL LINE TO lt_dia ASSIGNING <ls_dia>.
    <ls_dia>-addrnumber = /acrn/_ithl_st_ITHLDOSY_hs-addrnumber.
    <ls_dia>-maint_mode = 'DISPLAY'.

    CALL FUNCTION 'ADDR_DIALOG'
      TABLES
        number_handle_tab = lt_dia
      EXCEPTIONS
        address_not_exist = 1
        group_not_valid   = 2
        parameter_error   = 3
        internal_error    = 4
        OTHERS            = 5.

  ENDMETHOD.
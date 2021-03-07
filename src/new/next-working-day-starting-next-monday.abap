METHOD calc_payment_date.

    DATA: lv_wotnr         TYPE p,
          lv_holiday_found TYPE scal-indicator,
          lt_thol          TYPE TABLE OF thol.
  
    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = ip_approve_date
      IMPORTING
        wotnr = lv_wotnr.
  
    DATA(lv_payment_date) = CONV d( ip_approve_date + ( 8 - lv_wotnr ) ).
  
    DO.
      CALL FUNCTION 'DAY_IN_WEEK'
        EXPORTING
          datum = lv_payment_date
        IMPORTING
          wotnr = lv_wotnr.
      IF lv_wotnr = 6 OR lv_wotnr = 7.
        ADD 1 TO lv_payment_date.
      ELSE.
        CLEAR: lv_holiday_found, lt_thol.
        CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
          EXPORTING
            date                         = lv_payment_date
            holiday_calendar_id          = 'TR'
            with_holiday_attributes      = 'X'
          IMPORTING
            holiday_found                = lv_holiday_found
          TABLES
            holiday_attributes           = lt_thol
          EXCEPTIONS
            calendar_buffer_not_loadable = 1
            date_after_range             = 2
            date_before_range            = 3
            date_invalid                 = 4
            holiday_calendar_id_missing  = 5
            holiday_calendar_not_found   = 6
            OTHERS                       = 7.
        CASE lv_holiday_found.
          WHEN 'X'.
            READ TABLE lt_thol ASSIGNING FIELD-SYMBOL(<ls_thol>) INDEX 1.
            CASE sy-subrc.
              WHEN 0.
                CASE <ls_thol>-klass.
                  WHEN '2'. "yarım gün
                    EXIT.
                  WHEN OTHERS.
                    ADD 1 TO lv_payment_date.
                ENDCASE.
              WHEN OTHERS.
                ADD 1 TO lv_payment_date.
            ENDCASE.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDIF.
    ENDDO.
  
    rv_payment_date = lv_payment_date.
  
  ENDMETHOD.
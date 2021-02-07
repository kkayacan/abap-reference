      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZFUELTYPES'
        EXCEPTIONS
          OTHERS    = 99.
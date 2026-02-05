          DATA gt_callstack TYPE sys_callst.
          CALL FUNCTION 'SYSTEM_CALLSTACK'
            IMPORTING
              et_callstack = gt_callstack.

          IF line_exists( gt_callstack[ eventname = 'ON_USER_COMMAND' ] ).
            LEAVE TO SCREEN 0.
          ELSE.
            LEAVE TO SCREEN 0100.
          ENDIF.
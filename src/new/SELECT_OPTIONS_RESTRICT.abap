REPORT TESTREP.

* Include type pool SSCR
TYPE-POOLS SSCR.

* Define the object to be passed to the RESTRICTION parameter
DATA RESTRICT TYPE SSCR_RESTRICT.

* Auxiliary objects for filling RESTRICT
DATA OPT_LIST TYPE SSCR_OPT_LIST.
DATA ASS      TYPE SSCR_ASS.

* Define the selection screen objects
* First block: 3 SELECT-OPTIONS
SELECTION-SCREEN BEGIN OF BLOCK BLOCK_0 WITH FRAME TITLE TEXT-BL0.
  SELECT-OPTIONS SEL_0_0 FOR SY-TVAR0.
  SELECT-OPTIONS SEL_0_1 FOR SY-TVAR1.
  SELECT-OPTIONS SEL_0_2 FOR SY-TVAR2.
  SELECT-OPTIONS SEL_0_3 FOR SY-TVAR3.
SELECTION-SCREEN END   OF BLOCK BLOCK_0.

* Second block: 2 SELECT-OPTIONS
SELECTION-SCREEN BEGIN OF BLOCK BLOCK_1 WITH FRAME TITLE TEXT-BL1.
  SELECT-OPTIONS SEL_1_0 FOR SY-SUBRC.
  SELECT-OPTIONS SEL_1_1 FOR SY-REPID.
SELECTION-SCREEN END   OF BLOCK BLOCK_1.

INITIALIZATION.

* Define the option list

* ALL: All options allowed
  MOVE 'ALL'        TO OPT_LIST-NAME.
  MOVE 'X' TO: OPT_LIST-OPTIONS-BT,
               OPT_LIST-OPTIONS-CP,
               OPT_LIST-OPTIONS-EQ,
               OPT_LIST-OPTIONS-GE,
               OPT_LIST-OPTIONS-GT,
               OPT_LIST-OPTIONS-LE,
               OPT_LIST-OPTIONS-LT,
               OPT_LIST-OPTIONS-NB,
               OPT_LIST-OPTIONS-NE,
               OPT_LIST-OPTIONS-NP.
  APPEND OPT_LIST TO RESTRICT-OPT_LIST_TAB.

* NOPATTERN: CP and NP not allowed
  CLEAR OPT_LIST.
  MOVE 'NOPATTERN'  TO OPT_LIST-NAME.
  MOVE 'X' TO: OPT_LIST-OPTIONS-BT,
               OPT_LIST-OPTIONS-EQ,
               OPT_LIST-OPTIONS-GE,
               OPT_LIST-OPTIONS-GT,
               OPT_LIST-OPTIONS-LE,
               OPT_LIST-OPTIONS-LT,
               OPT_LIST-OPTIONS-NB,
               OPT_LIST-OPTIONS-NE.
  APPEND OPT_LIST TO RESTRICT-OPT_LIST_TAB.

* NOINTERVLS: BT and NB not allowed
  CLEAR OPT_LIST.
  MOVE 'NOINTERVLS' TO OPT_LIST-NAME.
  MOVE 'X' TO: OPT_LIST-OPTIONS-CP,
               OPT_LIST-OPTIONS-EQ,
               OPT_LIST-OPTIONS-GE,
               OPT_LIST-OPTIONS-GT,
               OPT_LIST-OPTIONS-LE,
               OPT_LIST-OPTIONS-LT,
               OPT_LIST-OPTIONS-NE,
               OPT_LIST-OPTIONS-NP.
  APPEND OPT_LIST TO RESTRICT-OPT_LIST_TAB.

* EQ_AND_CP: only EQ and CP allowed
  CLEAR OPT_LIST.
  MOVE 'EQ_AND_CP'  TO OPT_LIST-NAME.
  MOVE 'X' TO: OPT_LIST-OPTIONS-CP,
               OPT_LIST-OPTIONS-EQ.
  APPEND OPT_LIST TO RESTRICT-OPT_LIST_TAB.

* JUST_EQ: Only EQ allowed
  CLEAR OPT_LIST.
  MOVE 'JUST_EQ' TO OPT_LIST-NAME.
  MOVE 'X' TO OPT_LIST-OPTIONS-EQ.
  APPEND OPT_LIST TO RESTRICT-OPT_LIST_TAB.

* Assign selection screen objects to option list and sign

* KIND = 'A': applies to all SELECT-OPTIONS
  MOVE: 'A'          TO ASS-KIND,
        '*'          TO ASS-SG_MAIN,
        'NOPATTERN'  TO ASS-OP_MAIN,
        'NOINTERVLS' TO ASS-OP_ADDY.
  APPEND ASS TO RESTRICT-ASS_TAB.

* KIND = 'B': applies to all SELECT-OPTIONS in block BLOCK_0,
*             that is, SEL_0_0, SEL_0_1, SEL_0_2
  CLEAR ASS.
  MOVE: 'B'          TO ASS-KIND,
        'BLOCK_0'    TO ASS-NAME,
        'I'          TO ASS-SG_MAIN,
        '*'          TO ASS-SG_ADDY,
        'NOINTERVLS' TO ASS-OP_MAIN.
  APPEND ASS TO RESTRICT-ASS_TAB.

* KIND = 'S': applies to SELECT-OPTION SEL-0-2
  CLEAR ASS.
  MOVE: 'S'          TO ASS-KIND,
        'SEL_0_2'    TO ASS-NAME,
        'I'          TO ASS-SG_MAIN,
        '*'          TO ASS-SG_ADDY,
        'EQ_AND_CP'  TO ASS-OP_MAIN,
        'ALL'        TO ASS-OP_ADDY.
  APPEND ASS TO RESTRICT-ASS_TAB.

* KIND = 'S': Applies to SELECT-OPTION SEL_0_3
  CLEAR ASS.
  MOVE: 'S'        TO ASS-KIND,
        'SEL_0_3'  TO ASS-NAME,
        'I'        TO ASS-SG_MAIN,
        'N'        TO ASS-SG_ADDY,
        'JUST_EQ'  TO ASS-OP_MAIN.
  APPEND ASS TO RESTRICT-ASS_TAB.

* Call function module
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
       EXPORTING
             RESTRICTION                = RESTRICT
*           DB                          = ' '
       EXCEPTIONS
             TOO_LATE                   = 1
             REPEATED                   = 2
             NOT_DURING_SUBMIT          = 3
            DB_CALL_AFTER_REPORT_CALL  = 4
            SELOPT_WITHOUT_OPTIONS     = 5
             SELOPT_WITHOUT_SIGNS       = 6
             INVALID_SIGN               = 7
            REPORT_CALL_AFTER_DB_ERROR = 8
              EMPTY_OPTION_LIST          = 9
             INVALID_KIND               = 10
             REPEATED_KIND_A            = 11
             OTHERS                     = 12.

* Exception handling
  IF SY-SUBRC NE 0.
     ...
  ENDIF.
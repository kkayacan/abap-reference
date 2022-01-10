*&--------------------------------------------------------------------&*
*& Report:       Z_USEREXIT(V10)                                &*
*&--------------------------------------------------------------------&*
*& Selection Texts:
*& P_ALV   ALV format
*& P_AUTH   Include authority-check search
*& P_BADI   Display BADIs
*& P_CUSB   Customer BADIs only
*& P_BTE   Display business trans events
*& P_DEVC   Show development class exits
*& P_EXIT   Display user exits
*& P_FUNC   Show function modules
*& P_LIMIT   Limit no. of submits to search
*& P_LST   Standard list format
*& P_PNAME   Program name
*& P_PROG   Display program exits
*& P_SUBM   Show submits
*& P_TCODE   Transaction code
*& P_TEXT   Search for text
*& P_WFLOW   Display workflow links
*&--------------------------------------------------------------------&*
*& Text symbols:
*& M01   Enter TCode or program
*& M02   Enter at least one scope criteria
*& S01   Selection data (TCode takes precedence over program name)
*& S02   Scope criteria
*& S03   Display criteria
*&--------------------------------------------------------------------&*
REPORT  Z_USEREXIT
  NO STANDARD PAGE HEADING
  LINE-SIZE 201.
 
TABLES: sxs_attr,
        tobjt,
        tstct,               "TCode texts
        trdirt,              "Program texts
        sxc_exit.            "BADI exits
 
TYPE-POOLS: slis.
 
DATA: tabix         LIKE sy-tabix,
      w_linnum      TYPE i,
      w_off         TYPE i,
      w_index       LIKE sy-tabix,
      w_include     LIKE trdir-NAME,
      w_prog        LIKE trdir-NAME,
      w_incl        LIKE trdir-NAME,
      w_area        LIKE rs38l-AREA,
      w_level,
      w_str(50)     TYPE c,
      w_cnt(2)      TYPE c,
      w_funcname    LIKE tfdir-funcname,
      w_fsel        LIKE sy-ucomm,    " Determination of screen field
      w_gridtxt(70) TYPE c.           "ALV grid title
 
CONSTANTS: c_fmod(40) TYPE c VALUE 'Function modules searched: ',
           c_subm(40) TYPE c VALUE 'Submit programs searched: ',
           c_devc(60) TYPE c VALUE 'User-exits from development classes in function modules',
           c_col1(12) TYPE c VALUE 'Enhanmt Type',
           c_col2(40) TYPE c VALUE 'Enhancement',
           c_col3(30) TYPE c VALUE 'Program/Include',
           c_col4(20) TYPE c VALUE 'Enhancement Name',
           c_col5(40) TYPE c VALUE 'Enhancement Description',
           c_col6(8)  TYPE c VALUE 'Project',
           c_col7(1)  TYPE c VALUE 'S',
           c_col8(12) TYPE c VALUE 'ChangeName',
           c_col9(10) TYPE c VALUE 'ChangeDate',
           c_x        TYPE c VALUE 'X'.
 
* Work Areas: ABAP Workbench
DATA: BEGIN OF wa_d010inc.
DATA: master TYPE d010inc-master.
DATA: END OF wa_d010inc.
 
DATA: BEGIN OF wa_tfdir.
DATA: funcname TYPE tfdir-funcname,
      pname    TYPE tfdir-pname,
      İNCLUDE  TYPE tfdir-İNCLUDE.
DATA: END OF wa_tfdir.
 
DATA: BEGIN OF wa_tadir.
DATA: devclass TYPE tadir-devclass.
DATA: END OF wa_tadir.
 
DATA: BEGIN OF wa_tstc.
DATA: pgmna TYPE tstc-pgmna.
DATA: END OF wa_tstc.
 
DATA: BEGIN OF wa_tstcp.
DATA: param TYPE tstcp-param.
DATA: END OF wa_tstcp.
 
DATA: BEGIN OF wa_enlfdir.
DATA: AREA TYPE enlfdir-AREA.
DATA: END OF wa_enlfdir.
 
* Work Areas: BADIs
DATA: BEGIN OF wa_sxs_attr.
DATA: exit_name TYPE sxs_attr-exit_name.
DATA: END OF wa_sxs_attr.
 
DATA: BEGIN OF wa_sxs_attrt.
DATA: text TYPE sxs_attrt-text.
DATA: END OF wa_sxs_attrt.
 
* Work Areas: Enhancements
DATA: BEGIN OF wa_modsap.
DATA: member TYPE modsap-member.
DATA: END OF wa_modsap.
 
DATA: BEGIN OF wa_modsapa.
DATA: NAME TYPE modsapa-NAME.
DATA: END OF wa_modsapa.
 
DATA: BEGIN OF wa_modsapt.
DATA: modtext TYPE modsapt-modtext.
DATA: END OF wa_modsapt.
 
* Work Areas: Business Transaction Events
DATA: BEGIN OF wa_tbe01t.
DATA: text1 TYPE tbe01t-text1.
DATA: END OF wa_tbe01t.
 
DATA: BEGIN OF wa_tps01t.
DATA: text1 TYPE tps01t-text1.
DATA: END OF wa_tps01t.
 
* user-exits
TYPES:  BEGIN OF ty_mod,
          member  LIKE modact-member,
          NAME    LIKE modact-NAME,
          status  LIKE modattr-status,
          anam    LIKE modattr-anam,
          adat    LIKE modattr-adat,
        END OF ty_mod.
DATA:   w_mod  TYPE ty_mod.
 
TYPES: BEGIN OF t_userexit,
      TYPE(12) TYPE c,
      pname    LIKE trdir-NAME,
      txt(300),
      LEVEL    TYPE c,
      modname(30) TYPE c,
      modtext(60) TYPE c,
      modattr     TYPE ty_mod,
      colour(4)   TYPE c,
END OF t_userexit.
DATA: i_userexit TYPE STANDARD TABLE OF t_userexit WITH HEADER LINE.
 
* Function module developmnet classes
TYPES: BEGIN OF t_devclass,
      clas   LIKE trdir-clas,
END OF t_devclass.
DATA: i_devclass TYPE STANDARD TABLE OF t_devclass WITH HEADER LINE.
 
* Submit programs
TYPES: BEGIN OF t_submit,
      pname     LIKE trdir-NAME,
      LEVEL,
      done,
END OF t_submit.
DATA: i_submit TYPE STANDARD TABLE OF t_submit WITH HEADER LINE.
 
* Source code
TYPES: BEGIN OF t_sourcetab,
        LİNE(200),
      END OF t_sourcetab.
DATA: sourcetab TYPE STANDARD TABLE OF t_sourcetab WITH HEADER LINE.
DATA c_overflow(30000) TYPE c.
 
* Description of an ABAP/4 source analysis token
DATA: i_stoken TYPE STANDARD TABLE OF stokex WITH HEADER LINE.
DATA wa_stoken LIKE i_stoken.
 
* Description of an ABAP/4 source analysis statement
DATA: i_sstmnt TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE. "#EC NEEDED
 
* keywords for searching ABAP code
TYPES: BEGIN OF t_keywords,
      WORD(30),
END OF t_keywords.
DATA: keywords TYPE STANDARD TABLE OF t_keywords WITH HEADER LINE.
 
* function modules within program
TYPES: BEGIN OF t_fmodule,
      NAME   LIKE rs38l-NAME,
      pname  LIKE trdir-NAME,
      pname2 LIKE trdir-NAME,
      LEVEL,
      bapi,
      done,
END OF t_fmodule.
DATA: i_fmodule TYPE STANDARD TABLE OF t_fmodule WITH HEADER LINE.
 
* ALV definitions
DATA i_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA i_layout   TYPE slis_layout_alv.
DATA i_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE.
 
*&--------------------------------------------------------------------&*
*& Selection Options                                                  &*
*&--------------------------------------------------------------------&*
SELECTION-SCREEN BEGIN OF BLOCK selscr1 WITH FRAME TITLE text-s01.
PARAMETER: p_pname LIKE trdir-NAME,
           p_tcode LIKE syst-tcode,
           p_limit(4) TYPE n DEFAULT 500.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK selscr1.
 
SELECTION-SCREEN BEGIN OF BLOCK selscr2 WITH FRAME TITLE text-s02.
PARAMETER: p_badi  AS CHECKBOX DEFAULT c_x,
           p_cusb  AS CHECKBOX DEFAULT c_x,
           p_bte   AS CHECKBOX DEFAULT c_x,
           p_exit  AS CHECKBOX DEFAULT c_x,
           p_prog  AS CHECKBOX DEFAULT c_x,
           p_wflow AS CHECKBOX,
           p_auth  AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETER: p_text(40) TYPE c.
SELECTION-SCREEN END OF BLOCK selscr2.
 
SELECTION-SCREEN BEGIN OF BLOCK selscr3 WITH FRAME TITLE text-s03.
PARAMETER: p_alv RADIOBUTTON GROUP rad1 DEFAULT 'X',
           p_lst RADIOBUTTON GROUP rad1.
SELECTION-SCREEN SKIP.
PARAMETER: p_devc  LIKE rihea-dy_ofn DEFAULT ' ' MODIF ID a01,
           p_func  LIKE rihea-dy_ofn DEFAULT ' ' MODIF ID a01,
           p_subm  LIKE rihea-dy_ofn DEFAULT ' ' MODIF ID a01.
SELECTION-SCREEN END OF BLOCK selscr3.
 
*&--------------------------------------------------------------------&*
*& START-OF-SELECTION                                                 &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.
 
  IF p_pname IS INITIAL AND p_tcode IS INITIAL.
    MESSAGE i000(g01) WITH text-m01.
    STOP.
  ENDIF.
 
  IF p_badi  IS INITIAL AND
     p_exit  IS INITIAL AND
     p_bte   IS INITIAL AND
     p_wflow IS INITIAL AND
     p_auth  IS INITIAL AND
     p_prog  IS INITIAL.
    MESSAGE i000(g01) WITH text-m02.
    STOP.
  ENDIF.
 
* ensure P_LIMIT is not zero.
  IF p_limit = 0.
    p_limit = 1.
  ENDIF.
 
  PERFORM data_select.
  PERFORM get_submit_data.
  PERFORM get_fm_data.
  PERFORM get_additional_data.
  PERFORM data_display.
 
*&--------------------------------------------------------------------&*
*& Form DATA_SELECT                                                   &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM data_select.
 
* data selection message to sap gui
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    DESTINATION 'SAPGUI'
    KEEPING LOGICAL UNIT OF WORK
    EXPORTING
      text                  = 'Get programs/includes'       "#EC NOTEXT
    EXCEPTIONS
      system_failure
      communication_failure
    .                                                       "#EC *
 
* get TCode name for ALV grid title
  CLEAR w_gridtxt.
  IF NOT p_tcode IS INITIAL.
    SELECT SINGLE * FROM tstct WHERE tcode = p_tcode
                                 AND sprsl = sy-langu.
    CONCATENATE 'TCode:' p_tcode tstct-ttext INTO w_gridtxt
                                SEPARATED BY SPACE.
  ENDIF.
* get program name for ALV grid title
  IF NOT p_pname IS INITIAL.
    SELECT SINGLE * FROM trdirt WHERE NAME = p_pname
                                 AND sprsl = sy-langu.
    CONCATENATE 'Program:' p_pname tstct-ttext INTO w_gridtxt
                                SEPARATED BY SPACE.
  ENDIF.
 
* determine search words
  keywords-WORD = 'CALL'.
  APPEND keywords.
  keywords-WORD = 'FORM'.
  APPEND keywords.
  keywords-WORD = 'PERFORM'.
  APPEND keywords.
  keywords-WORD = 'SUBMIT'.
  APPEND keywords.
  keywords-WORD = 'INCLUDE'.
  APPEND keywords.
  keywords-WORD = 'AUTHORITY-CHECK'.
  APPEND keywords.
 
  IF NOT p_tcode IS INITIAL.
* get program name from TCode
    SELECT SINGLE pgmna FROM tstc INTO wa_tstc-pgmna
                 WHERE tcode EQ p_tcode.
    IF NOT wa_tstc-pgmna IS INITIAL.
      p_pname = wa_tstc-pgmna.
* TCode does not include program name, but does have reference TCode
    ELSE.
      SELECT SINGLE param FROM tstcp INTO wa_tstcp-param
                   WHERE tcode EQ p_tcode.
      IF sy-subrc = 0.
        CHECK wa_tstcp-param(1)   = '/'.
        CHECK wa_tstcp-param+1(1) = '*'.
        IF wa_tstcp-param CA ' '.
        ENDIF.
        w_off = sy-fdpos + 1.
        SUBTRACT 2 FROM sy-fdpos.
        IF sy-fdpos GT 0.
          p_tcode = wa_tstcp-param+2(sy-fdpos).
        ENDIF.
        SELECT SINGLE pgmna FROM tstc INTO wa_tstc-pgmna
               WHERE tcode EQ p_tcode.
        p_pname = wa_tstc-pgmna.
        IF sy-subrc <> 0.
          MESSAGE s110(/saptrx/asc) WITH 'No program found for: ' p_tcode. "#EC NOTEXT
          STOP.
        ENDIF.
      ELSE.
        MESSAGE s110(/saptrx/asc) WITH 'No program found for: ' p_tcode. "#EC NOTEXT
        STOP.
      ENDIF.
 
    ENDIF.
  ENDIF.
 
* Call customer-function aus Program coding
  READ REPORT p_pname INTO sourcetab.
  IF sy-subrc > 0.
    MESSAGE e017(ENHANCEMENT) WITH p_pname RAISING no_program. "#EC *
  ENDIF.
 
  SCAN ABAP-SOURCE sourcetab TOKENS     INTO i_stoken
                             STATEMENTS INTO i_sstmnt
                             KEYWORDS   FROM keywords
                             OVERFLOW INTO c_overflow
                             WITH INCLUDES WITH ANALYSIS.   "#EC
  IF sy-subrc > 0. "keine/syntakt. falsche Ablauflog./Fehler im Skanner
    MESSAGE e130(ENHANCEMENT) RAISING syntax_error.         "#EC
  ENDIF.
 
* check I_STOKEN for entries
  CLEAR w_linnum.
  DESCRIBE TABLE i_stoken LINES w_linnum.
  IF w_linnum GT 0.
    w_level = '0'.
    w_prog = ''.
    w_incl = ''.
    PERFORM data_search TABLES i_stoken USING w_level w_prog w_incl.
  ENDIF.
 
ENDFORM.                        "DATA_SELECT
 
*&--------------------------------------------------------------------&*
*& Form GET_FM_DATA                                                   &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM get_fm_data.
 
* data selection message to sap gui
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    DESTINATION 'SAPGUI'
    KEEPING LOGICAL UNIT OF WORK
    EXPORTING
      text                  = 'Get function module data'    "#EC NOTEXT
    EXCEPTIONS
      system_failure
      communication_failure
    .                                                       "#EC *
 
* Function module data
  SORT i_fmodule BY NAME.
  DELETE ADJACENT DUPLICATES FROM i_fmodule COMPARING NAME.
 
  LOOP AT i_fmodule WHERE done  NE c_x.
 
    CLEAR:   i_stoken, i_sstmnt, sourcetab, wa_tfdir, w_include .
    REFRESH: i_stoken, i_sstmnt, sourcetab.
 
    CLEAR wa_tfdir.
    SELECT SINGLE funcname pname İNCLUDE FROM tfdir INTO wa_tfdir
                            WHERE funcname = i_fmodule-NAME.
    CHECK sy-subrc = 0.
 
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        PROGRAM = wa_tfdir-pname
      IMPORTING
        group   = w_area.
 
    CONCATENATE 'L' w_area 'U' wa_tfdir-İNCLUDE INTO w_include.
    i_fmodule-pname  = w_include.
    i_fmodule-pname2 = wa_tfdir-pname.
    MODIFY i_fmodule.
 
    READ REPORT i_fmodule-pname INTO sourcetab.
    IF sy-subrc = 0.
 
      SCAN ABAP-SOURCE sourcetab TOKENS     INTO i_stoken
                                 STATEMENTS INTO i_sstmnt
                                 KEYWORDS   FROM keywords
                                 WITH INCLUDES
                                 WITH ANALYSIS.
      IF sy-subrc > 0.
        MESSAGE e130(ENHANCEMENT) RAISING syntax_error.
      ENDIF.
 
* check i_stoken for entries
      CLEAR w_linnum.
      DESCRIBE TABLE i_stoken LINES w_linnum.
      IF w_linnum GT 0.
        w_level = '1'.
        w_prog  = i_fmodule-pname2.
        w_incl =  i_fmodule-pname.
        PERFORM data_search TABLES i_stoken USING w_level w_prog w_incl.
      ENDIF.
    ENDIF.
 
  ENDLOOP.
 
* store development classes
  IF p_devc = c_x.
    LOOP AT i_fmodule.
      CLEAR: wa_tadir, wa_enlfdir.
 
      SELECT SINGLE AREA FROM enlfdir INTO wa_enlfdir-AREA
                            WHERE funcname = i_fmodule-NAME.
      CHECK NOT wa_enlfdir-AREA IS INITIAL.
 
      SELECT SINGLE devclass INTO wa_tadir-devclass
                      FROM tadir WHERE pgmid    = 'R3TR'
                                   AND object   = 'FUGR'
                                   AND obj_name = wa_enlfdir-AREA.
      CHECK NOT wa_tadir-devclass IS INITIAL.
      MOVE wa_tadir-devclass TO i_devclass-clas.
      APPEND i_devclass.
      i_fmodule-done = c_x.
      MODIFY i_fmodule.
    ENDLOOP.
 
    SORT i_devclass.
    DELETE ADJACENT DUPLICATES FROM i_devclass.
  ENDIF.
 
ENDFORM.                        "GET_FM_DATA
 
*&--------------------------------------------------------------------&*
*& Form GET_SUBMIT_DATA                                               &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM get_submit_data.
 
* data selection message to sap gui
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    DESTINATION 'SAPGUI'
    KEEPING LOGICAL UNIT OF WORK
    EXPORTING
      text                  = 'Get submit data'             "#EC NOTEXT
    EXCEPTIONS
      system_failure
      communication_failure
    .                                                       "#EC *
 
  SORT i_submit.
  DELETE ADJACENT DUPLICATES FROM i_submit COMPARING pname.
  w_level = '0'.
 
  LOOP AT i_submit WHERE done NE c_x.
 
    CLEAR:   i_stoken, i_sstmnt, sourcetab.
    REFRESH: i_stoken, i_sstmnt, sourcetab.
 
    READ REPORT i_submit-pname INTO sourcetab.
    IF sy-subrc = 0.
 
      SCAN ABAP-SOURCE sourcetab TOKENS     INTO i_stoken
                                 STATEMENTS INTO i_sstmnt
                                 KEYWORDS   FROM keywords
                                 WITH INCLUDES
                                 WITH ANALYSIS.
      IF sy-subrc > 0.
*        message e130(enhancement) raising syntax_error.
        CONTINUE.
      ENDIF.
 
* check i_stoken for entries
      CLEAR w_linnum.
      DESCRIBE TABLE i_stoken LINES w_linnum.
      IF w_linnum GT 0.
        w_prog  = i_submit-pname.
        w_incl = ''.
        PERFORM data_search TABLES i_stoken USING w_level w_prog w_incl.
      ENDIF.
    ENDIF.
 
* restrict number of submit program selected for processing
    DESCRIBE TABLE i_submit LINES w_linnum.
    IF w_linnum GE p_limit.
      w_level = '1'.
    ENDIF.
    i_submit-done = c_x.
    MODIFY i_submit.
  ENDLOOP.
 
ENDFORM.                       "GET_SUBMIT_DATA
 
*&--------------------------------------------------------------------&*
*& Form DATA_SEARCH                                                   &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM data_search TABLES p_stoken STRUCTURE stoken
                        USING p_level l_prog l_incl.
 
  LOOP AT p_stoken.
 
    CLEAR i_userexit.
 
* Workflow
    IF p_wflow = c_x.
      IF p_level EQ '1'.    " do not perform for function modules (2nd pass)
        IF  p_stoken-str+1(16) CS 'SWE_EVENT_CREATE'.
          REPLACE ALL OCCURRENCES OF '''' IN p_stoken-str WITH ''.
          i_userexit-TYPE = 'WorkFlow'.
          i_userexit-txt  = p_stoken-str.
          CONCATENATE l_prog '/' l_incl INTO i_userexit-pname.
          APPEND i_userexit.
        ENDIF.
      ENDIF.
    ENDIF.
 
    tabix = sy-tabix + 1.
    i_userexit-LEVEL = p_level.
    IF i_userexit-LEVEL = '0'.
      IF l_incl IS INITIAL.
        i_userexit-pname = p_pname.
      ELSE.
        CONCATENATE  p_pname '-' l_incl INTO i_userexit-pname.
      ENDIF.
    ELSE.
      IF l_incl IS INITIAL.
        i_userexit-pname = l_prog.
      ELSE.
        CONCATENATE  l_prog '-' l_incl INTO i_userexit-pname.
      ENDIF.
    ENDIF.
 
* AUTHORITY-CHECKS
    IF p_auth = c_x.
      IF p_stoken-str EQ 'AUTHORITY-CHECK'.
        CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
        w_index = sy-tabix + 2.
        READ TABLE p_stoken INDEX w_index INTO wa_stoken.
        CHECK NOT wa_stoken-str CS 'STRUCTURE'.
        CHECK NOT wa_stoken-str CS 'SYMBOL'.
        READ TABLE i_submit WITH KEY pname = wa_stoken-str.
        IF sy-subrc <> 0.
          i_userexit-pname = i_submit-pname.
          i_userexit-TYPE = 'AuthCheck'.
          i_userexit-txt  = wa_stoken-str.
          REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH SPACE.
          CLEAR tobjt.
          SELECT SINGLE * FROM tobjt WHERE object = i_userexit-txt
                                       AND langu  = sy-langu.
          i_userexit-modname = 'AUTHORITY-CHECK'.
          i_userexit-modtext = tobjt-ttext.
          APPEND i_userexit.
        ENDIF.
      ENDIF.
    ENDIF.
 
* Text searches
    IF NOT p_text IS INITIAL.
      IF p_stoken-str CS p_text.
        i_userexit-pname = i_submit-pname.
        i_userexit-TYPE = 'TextSearch'.
        i_userexit-txt  = wa_stoken-str.
        i_userexit-modname = 'Text Search'.
        i_userexit-modtext = p_stoken-str.
        APPEND i_userexit.
      ENDIF.
    ENDIF.
 
* Include (SE38)
    IF p_stoken-str EQ 'INCLUDE'.
      CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
      w_index = sy-tabix + 1.
      READ TABLE p_stoken INDEX w_index INTO wa_stoken.
      CHECK NOT wa_stoken-str CS 'STRUCTURE'.
      CHECK NOT wa_stoken-str CS 'SYMBOL'.
      READ TABLE i_submit WITH KEY pname = wa_stoken-str.
      IF sy-subrc <> 0.
        i_submit-pname = wa_stoken-str.
        i_submit-LEVEL = p_level.
        APPEND i_submit.
      ENDIF.
    ENDIF.
 
* Enhancements (SMOD)
    IF p_exit = c_x.
      IF p_stoken-str EQ 'CUSTOMER-FUNCTION'.
        CLEAR w_funcname.
        READ TABLE p_stoken INDEX tabix.
        TRANSLATE p_stoken-str USING ''' '.
        CONDENSE p_stoken-str.
        IF l_prog IS INITIAL.
          CONCATENATE 'EXIT' p_pname p_stoken-str INTO w_funcname
                       SEPARATED BY '_'.
        ELSE.
          CONCATENATE 'EXIT' l_prog p_stoken-str INTO w_funcname
                 SEPARATED BY '_'.
        ENDIF.
        SELECT SINGLE member FROM modsap INTO wa_modsap-member
              WHERE member = w_funcname.
        IF sy-subrc = 0.   " check for valid enhancement
          i_userexit-TYPE = 'Enhancement'.
          i_userexit-txt  = w_funcname.
          APPEND i_userexit.
        ELSE.
          CLEAR wa_d010inc.
          SELECT SINGLE master INTO wa_d010inc-master
                FROM d010inc
                   WHERE İNCLUDE = l_prog.
          CONCATENATE 'EXIT' wa_d010inc-master p_stoken-str INTO w_funcname
                 SEPARATED BY '_'.
          i_userexit-TYPE = 'Enhancement'.
          i_userexit-txt  = w_funcname.
        ENDIF.
      ENDIF.
    ENDIF.
 
* BADIs (SE18)
    IF p_badi = c_x.
      IF p_stoken-str CS 'cl_exithandler='.
        w_index = sy-tabix + 4.
        READ TABLE p_stoken INDEX w_index INTO wa_stoken.
        i_userexit-txt = wa_stoken-str.
        REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH SPACE.
        i_userexit-TYPE = 'BADI'.
        CLEAR sxs_attr.   " ensure a real BADI
        IF p_cusb = c_x.   "customer BADIs only
          SELECT SINGLE * FROM sxs_attr WHERE exit_name = i_userexit-txt
                                          AND internal <> c_x.
        ELSE.
          SELECT SINGLE * FROM sxs_attr WHERE exit_name = i_userexit-txt.
        ENDIF.
        IF sy-subrc = 0.
          APPEND i_userexit.
        ENDIF.
      ENDIF.
    ENDIF.
 
* Business transaction events (FIBF)
    IF p_bte = c_x.
      IF p_stoken-str CS 'OPEN_FI_PERFORM'.
        i_userexit-TYPE = 'BusTrEvent'.
        i_userexit-txt = p_stoken-str.
        REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH SPACE.
        i_userexit-modname =  i_userexit-txt+16(8).
        CASE i_userexit-txt+25(1).
          WHEN 'E'.
            CLEAR wa_tbe01t.
            SELECT SINGLE text1 INTO wa_tbe01t-text1 FROM tbe01t
                             WHERE event = i_userexit-txt+16(8)
                               AND spras = sy-langu.
            IF wa_tbe01t-text1 IS INITIAL.
              i_userexit-modtext = ''.                      "#EC NOTEXT
            ELSE.
              i_userexit-modtext = wa_tbe01t-text1.
            ENDIF.
            i_userexit-modname+8 = '/P&S'.                  "#EC NOTEXT
          WHEN 'P'.
            CLEAR wa_tps01t.
            SELECT SINGLE text1 INTO wa_tps01t-text1 FROM tps01t
                             WHERE procs = i_userexit-txt+16(8)
                               AND spras = sy-langu.
            i_userexit-modtext = wa_tps01t-text1.
            i_userexit-modname+8 = '/Process'.
        ENDCASE.
 
        APPEND i_userexit.
      ENDIF.
    ENDIF.
 
* Program exits (SE38)
    IF p_prog = c_x.
      IF p_stoken-str CS 'USEREXIT_'.
        CHECK NOT p_stoken-str CS '-'.   " ensure not USEREXIT_XX-XXX
        CHECK NOT p_stoken-str CS '('.   " ensure not SUBMIT_XX(X)
        i_userexit-TYPE = 'Program Exit'.
        i_userexit-txt = p_stoken-str.
        REPLACE ALL OCCURRENCES OF '''' IN i_userexit-txt WITH SPACE.
        APPEND i_userexit.
      ENDIF.
    ENDIF.
 
* Submit programs (SE38)
    IF p_stoken-str CS 'SUBMIT'.
      CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
      CHECK NOT p_stoken-str CS '_'.   " ensure not SUBMIT_XXX
      w_index = sy-tabix + 1.
      READ TABLE p_stoken INDEX w_index INTO wa_stoken.
      CHECK NOT wa_stoken-str CS '_'.   " ensure not SUBMIT_XXX
      REPLACE ALL OCCURRENCES OF '''' IN wa_stoken-str WITH SPACE.
      READ TABLE i_submit WITH KEY pname = wa_stoken-str.
      IF sy-subrc <> 0.
        i_submit-pname = wa_stoken-str.
        i_submit-LEVEL = p_level.
        APPEND i_submit.
      ENDIF.
    ENDIF.
 
* Perform routines (which reference external programs)
    IF p_stoken-str CS 'PERFORM'.
      CHECK p_level EQ '0'.    " do not perform for function modules (2nd pass)
      w_index = sy-tabix + 1.
      READ TABLE p_stoken INDEX w_index INTO wa_stoken.
      IF NOT wa_stoken-ovfl IS INITIAL.
        w_off = wa_stoken-off1 + 10.
        w_str = c_overflow+w_off(30).
        FIND ')' IN w_str MATCH OFFSET w_off.
        IF sy-subrc = 0.
          w_off = w_off + 1.
          wa_stoken-str = w_str(w_off).
        ENDIF.
      ENDIF.
 
      CHECK wa_stoken-str CS '('.
      w_off = 0.
      WHILE sy-subrc  = 0.
        IF wa_stoken-str+w_off(1) EQ '('.
          REPLACE SECTION OFFSET w_off LENGTH 1 OF wa_stoken-str WITH ''.
          REPLACE ALL OCCURRENCES OF ')' IN wa_stoken-str WITH SPACE.
          READ TABLE i_submit WITH KEY pname = wa_stoken-str.
          IF sy-subrc <> 0.
            i_submit-pname = wa_stoken-str.
            APPEND i_submit.
          ENDIF.
          EXIT.
        ELSE.
          REPLACE SECTION OFFSET w_off LENGTH 1 OF wa_stoken-str WITH ''.
          SHIFT wa_stoken-str LEFT DELETING LEADING SPACE.
        ENDIF.
      ENDWHILE.
    ENDIF.
 
* Function modules (SE37)
    IF p_stoken-str CS 'FUNCTION'.
 
      CLEAR i_fmodule.
      IF p_level EQ '0'.    " do not perform for function modules (2nd pass)
        w_index = sy-tabix + 1.
        READ TABLE p_stoken INDEX w_index INTO wa_stoken.
 
        IF wa_stoken-str CS 'BAPI'.
          i_fmodule-bapi = c_x.
        ENDIF.
 
        REPLACE FIRST OCCURRENCE OF '''' IN wa_stoken-str WITH SPACE.
        REPLACE FIRST OCCURRENCE OF '''' IN wa_stoken-str WITH SPACE.
        IF sy-subrc = 4.   " didn't find 2nd quote (ie name truncated)
          CLEAR wa_tfdir.
          CONCATENATE wa_stoken-str '%' INTO wa_stoken-str.
          SELECT SINGLE funcname INTO wa_tfdir-funcname FROM tfdir
                       WHERE funcname LIKE wa_stoken-str.
          IF sy-subrc = 0.
            i_fmodule-NAME = wa_tfdir-funcname.
          ELSE.
            CONTINUE.
          ENDIF.
        ELSE.
          i_fmodule-NAME = wa_stoken-str.
        ENDIF.
        i_fmodule-LEVEL = p_level.
        APPEND i_fmodule.
      ENDIF.
    ENDIF.
 
  ENDLOOP.
 
ENDFORM.                        "DATA_SEARCH
 
*&--------------------------------------------------------------------&*
*& Form GET_ADDITIONAL_DATA                                           &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM get_additional_data.
 
* data selection message to sap gui
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    DESTINATION 'SAPGUI'
    KEEPING LOGICAL UNIT OF WORK
    EXPORTING
      text                  = 'Get additional data'         "#EC NOTEXT
    EXCEPTIONS
      system_failure
      communication_failure
    .                                                       "#EC *
 
  LOOP AT i_userexit.
 
* Workflow
    IF i_userexit-TYPE EQ 'WorkFlow'.
      CONTINUE.
    ENDIF.
 
* Enhancement data
    IF  i_userexit-TYPE CS 'Enh'.
      CLEAR: wa_modsapa.
      SELECT SINGLE NAME INTO wa_modsapa-NAME FROM modsap
                        WHERE member = i_userexit-txt.
      CHECK sy-subrc = 0.
      i_userexit-modname = wa_modsapa-NAME.
 
      CLEAR wa_modsapt.
      SELECT SINGLE modtext INTO wa_modsapt-modtext FROM modsapt
                        WHERE NAME = wa_modsapa-NAME
                                     AND sprsl = sy-langu.
      i_userexit-modtext = wa_modsapt-modtext.
 
* Get the CMOD project name
      CLEAR w_mod.
      SELECT SINGLE modact~member modact~NAME modattr~status
                    modattr~anam  modattr~adat
        INTO w_mod
        FROM modact
        INNER JOIN modattr
          ON modattr~NAME = modact~NAME
        WHERE modact~member = wa_modsapa-NAME
          AND modact~typ    = SPACE.
      IF sy-subrc = 0.
        i_userexit-modattr  = w_mod.
      ENDIF.
    ENDIF.
 
* BADI data
    IF  i_userexit-TYPE EQ 'BADI'.
      CLEAR wa_sxs_attr.
      SELECT SINGLE exit_name INTO wa_sxs_attr-exit_name FROM sxs_attr
                                    WHERE exit_name = i_userexit-txt.
      IF sy-subrc = 0.
        i_userexit-modname = i_userexit-txt.
      ELSE.
        i_userexit-modname = 'Dynamic call'.                "#EC NOTEXT
      ENDIF.
      CLEAR wa_sxs_attrt.
      SELECT SINGLE text INTO wa_sxs_attrt-text FROM sxs_attrt
                                     WHERE exit_name = wa_sxs_attr-exit_name
                                       AND sprsl = sy-langu.
      i_userexit-modtext = wa_sxs_attrt-text.
    ENDIF.
 
* BADI Implementation
    IF  i_userexit-TYPE EQ 'BADI'.
      CLEAR sxc_exit.
      SELECT COUNT( * ) FROM sxc_exit WHERE exit_name = i_userexit-txt.
      w_cnt = sy-dbcnt.
* determine id BADI is for interal or external use
      CLEAR sxs_attr.
      SELECT SINGLE * FROM sxs_attr WHERE exit_name = i_userexit-txt.
      IF sxs_attr-internal = 'X'.
        wa_sxs_attrt-text = 'SAP '.
      ELSE.
        wa_sxs_attrt-text = 'CUST'.
      ENDIF.
*        concatenate wa_sxs_attrt-text w_cnt into i_userexit-modattr-name
*        separated by space.
      WRITE wa_sxs_attrt-text TO i_userexit-modattr-NAME.
      WRITE w_cnt             TO i_userexit-modattr-NAME+5.
    ENDIF.
 
    MODIFY i_userexit.
  ENDLOOP.
 
* get enhancements via program package
  CLEAR wa_tadir.
  SELECT SINGLE devclass INTO wa_tadir-devclass FROM tadir
                             WHERE pgmid    = 'R3TR'
                               AND object   = 'PROG'
                               AND obj_name = p_pname.
  IF sy-subrc = 0.
    CLEAR: wa_modsapa, wa_modsapt.
    SELECT NAME FROM modsapa INTO wa_modsapa-NAME
                          WHERE devclass = wa_tadir-devclass.
      SELECT SINGLE modtext FROM modsapt INTO wa_modsapt-modtext
                          WHERE NAME = wa_modsapa-NAME
                            AND sprsl = sy-langu.
 
      CLEAR i_userexit.
      READ TABLE i_userexit WITH KEY modname = wa_modsapa-NAME.
      IF sy-subrc <> 0.
        i_userexit-modtext = wa_modsapt-modtext.
        i_userexit-TYPE = 'Enhancement'.                    "#EC NOTEXT
        i_userexit-modname  = wa_modsapa-NAME.
        i_userexit-txt = 'Determined from program DevClass'. "#EC NOTEXT
        i_userexit-pname = 'Unknown'.                       "#EC NOTEXT
        APPEND i_userexit.
      ENDIF.
    ENDSELECT.
  ENDIF.
 
* set row colour.
  LOOP AT i_userexit.
    CASE i_userexit-TYPE.
      WHEN 'BADI'.
        i_userexit-colour = 'C601'.
      WHEN 'Enhancement'.
        i_userexit-colour = 'C501'.
      WHEN 'Program Exit'.
        i_userexit-colour = 'C401'.
      WHEN 'WorkFlow'.
        i_userexit-colour = 'C301'.
      WHEN 'BusTrEvent'.
        i_userexit-colour = 'C201'.
    ENDCASE.
    MODIFY i_userexit.
  ENDLOOP.
 
ENDFORM.                        "GET_ADDITIONAL_DATA
 
*&--------------------------------------------------------------------&*
*& Form DATA_DISPLAY                                                  &*
*&--------------------------------------------------------------------&*
*&                                                                    &*
*&--------------------------------------------------------------------&*
FORM data_display.
 
* data selection message to sap gui
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    DESTINATION 'SAPGUI'
    KEEPING LOGICAL UNIT OF WORK
    EXPORTING
      text                  = 'Prepare screen for display'  "#EC NOTEXT
    EXCEPTIONS
      system_failure
      communication_failure
    .                                                       "#EC *
 
  SORT i_userexit BY TYPE txt modname.
  DELETE ADJACENT DUPLICATES FROM i_userexit COMPARING txt pname modname.
 
* ensure records selected.
  DESCRIBE TABLE i_userexit LINES w_linnum.
  IF w_linnum = 0.
    MESSAGE s003(g00).   "No data records were selected
    EXIT.
  ENDIF.
 
  IF p_alv = ' '.
 
* format headings
    WRITE: 'Enhancements from main program: ', p_pname.
    WRITE: 'Enhancements from TCode: ', p_tcode.
    WRITE: 201''.
    ULINE.
    FORMAT COLOR COL_HEADING.
    WRITE: /    sy-vline,
           (12) c_col1,                    "Enhanmt Type
                sy-vline,
           (40) c_col2,                    "Enhancement
                sy-vline,
           (30) c_col3,                    "Program/Include
                sy-vline,
           (20) c_col4,                    "Enhancement name
                sy-vline,
           (40) c_col5,                    "Enhancement description
                sy-vline,
           (8)  c_col6,                    "Project
                sy-vline,
           (1)  c_col7,                    "S
                sy-vline,
           (12) c_col8,                    "ChangeName
                sy-vline,
           (10)  c_col9,                    "ChangeDate
                sy-vline.
    FORMAT RESET.
    ULINE.
 
* format lines
    LOOP AT i_userexit.
* set line colour
      CASE i_userexit-TYPE.
        WHEN 'Enhancement'.
          FORMAT COLOR 3 INTENSIFIED OFF.
        WHEN 'BADI'.
          FORMAT COLOR 4 INTENSIFIED OFF.
        WHEN 'BusTrEvent'.
          FORMAT COLOR 5 INTENSIFIED OFF.
        WHEN 'Program Exit'.
          FORMAT COLOR 6 INTENSIFIED OFF.
        WHEN OTHERS.
          FORMAT RESET.
      ENDCASE.
      WRITE: / sy-vline,
               i_userexit-TYPE,
               sy-vline,
               i_userexit-txt(40),
               sy-vline,
               i_userexit-pname(30),
               sy-vline,
               i_userexit-modname(20),
               sy-vline,
               i_userexit-modtext(40),
               sy-vline.
 
      WRITE:  i_userexit-modattr-NAME,
              sy-vline,
              i_userexit-modattr-status,
              sy-vline,
              i_userexit-modattr-anam,
              sy-vline,
              i_userexit-modattr-adat NO-ZERO,
              sy-vline.
      HIDE: i_userexit-modname, i_userexit-TYPE, i_userexit-modattr-NAME.
 
    ENDLOOP.
    FORMAT RESET.
    ULINE.
 
* user-exits from development class of function modules
    IF p_devc = c_x.
      WRITE: /.
      WRITE: / c_devc.
      WRITE: 201''.
      ULINE (90).
      WRITE: 201''.
 
      LOOP AT i_devclass.
        CLEAR wa_modsapa.
        SELECT NAME FROM modsapa INTO wa_modsapa
                     WHERE devclass = i_devclass-clas.
          SELECT SINGLE NAME modtext INTO CORRESPONDING FIELDS OF wa_modsapt
                                     FROM modsapt
                                       WHERE NAME  = wa_modsapa-NAME
                                         AND sprsl = sy-langu.
          FORMAT COLOR 3 INTENSIFIED OFF.
          WRITE: / sy-vline,
                   (12) 'Enhancement',
                   sy-vline,
                  wa_modsapa-NAME,
                  sy-vline,
                  wa_modsapt-modtext,
                  sy-vline.
        ENDSELECT.
      ENDLOOP.
      WRITE: 201''.
      ULINE (90).
      FORMAT RESET.
    ENDIF.
 
* display fuction modules used in program
    WRITE /.
    DESCRIBE TABLE i_fmodule LINES w_linnum.
    WRITE: / c_fmod , AT 35 w_linnum.                       "#EC NOTEXT
    WRITE: 201''.
 
    IF p_func = c_x.
      ULINE (38).
      WRITE: 201''.
      LOOP AT i_fmodule.
        WRITE: sy-vline,
               i_fmodule-NAME,
               sy-vline,
               i_fmodule-bapi,
               sy-vline.
        WRITE: 201''.
      ENDLOOP.
      WRITE: 201''.
      ULINE (38).
    ENDIF.
 
* display submit programs used in program
    WRITE /.
    DESCRIBE TABLE i_submit LINES w_linnum.
    WRITE: / c_subm , AT 35 w_linnum.                       "#EC NOTEXT
    WRITE: 201''.
    IF p_subm = c_x.
      ULINE (44).
      WRITE: 201''.
      LOOP AT i_submit.
        WRITE: sy-vline,
               i_submit-pname,
               sy-vline.
        WRITE: 201''.
      ENDLOOP.
      WRITE: 201''.
      ULINE (44).
    ENDIF.
 
* issue message with number of user-exits displayed
    DESCRIBE TABLE i_userexit LINES w_linnum.
    MESSAGE s697(56) WITH w_linnum.
 
  ELSE.    " Show in alv format
 
* issue message with number of user-exits displayed
    DESCRIBE TABLE i_userexit LINES w_linnum.
    MESSAGE s697(56) WITH w_linnum.
 
* Create field catalog
    PERFORM create_field_catalog USING 'TYPE'           'T_USEREXIT' ' ' 'Type'.
    PERFORM create_field_catalog USING 'PNAME'          'T_USEREXIT' ' ' 'Program name'.
    PERFORM create_field_catalog USING 'TXT'            'T_USEREXIT' ' ' 'Enhancement'.
    PERFORM create_field_catalog USING 'LEVEL'          'T_USEREXIT' c_x 'Level'.
    PERFORM create_field_catalog USING 'MODNAME'        'T_USEREXIT' ' ' 'Enhancement name'.
    PERFORM create_field_catalog USING 'MODTEXT'        'T_USEREXIT' ' ' 'Enhancement text'.
    PERFORM create_field_catalog USING 'MODATTR-MEMBER' 'T_USEREXIT' c_x 'Member'.
    PERFORM create_field_catalog USING 'MODATTR-NAME'   'T_USEREXIT' ' ' 'Project'.
    PERFORM create_field_catalog USING 'MODATTR-STATUS' 'T_USEREXIT' ' ' 'Status'.
    PERFORM create_field_catalog USING 'MODATTR-ANAM'   'T_USEREXIT' ' ' 'Changed by'.
    PERFORM create_field_catalog USING 'MODATTR-ADAT'   'T_USEREXIT' ' ' 'Change date'.
 
* Layout
    CLEAR i_layout.
    i_layout-colwidth_optimize = c_x.
    i_layout-info_fieldname    = 'COLOUR'.
 
* Sort
    CLEAR i_sort.
    i_sort-fieldname = 'TYPE'.
    i_sort-tabname   = 'T_USEREXIT'.
    i_sort-up = c_x.
    APPEND i_sort.
 
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-cprog
        i_callback_user_command = 'USER_COMMAND'
        is_layout               = i_layout
        it_fieldcat             = i_fieldcat[]
        it_sort                 = i_sort[]
        i_default               = c_x
        i_save                  = 'A'
        i_grid_title            = w_gridtxt
      TABLES
        t_outtab                = i_userexit.
 
  ENDIF.
 
* issue message with number of user-exits displayed
  DESCRIBE TABLE i_userexit LINES w_linnum.
  MESSAGE s697(56) WITH w_linnum.
 
ENDFORM.                        "DATA_DISPLAY
 
*&---------------------------------------------------------------------&*
*& Form  CREATE_FIELD_CATALOG                                          &*
*&---------------------------------------------------------------------&*
FORM create_field_catalog USING    p_fieldname
                                   p_tabname
                                   p_hide
                                   p_text.
 
  i_fieldcat-fieldname        = p_fieldname.
  i_fieldcat-tabname          = p_tabname.
  i_fieldcat-no_out           = p_hide.
  i_fieldcat-seltext_l        = p_text.
 
  APPEND i_fieldcat.
 
ENDFORM.                    " CREATE_FIELD_CATALOG
 
*&---------------------------------------------------------------------&*
*& Form  CREATE_FIELD_CATALOG                                          &*
*&---------------------------------------------------------------------&*
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  READ TABLE i_userexit INDEX rs_selfield-tabindex.
  CHECK sy-subrc = 0.
  CASE r_ucomm.
    WHEN '&IC1'.
      CASE rs_selfield-sel_tab_field.
        WHEN 'T_USEREXIT-MODNAME'.
          READ TABLE i_userexit INDEX rs_selfield-tabindex.
          CASE i_userexit-TYPE.
            WHEN 'Enhancement'.
              SET PARAMETER ID 'MON' FIELD i_userexit-modname.
              CALL TRANSACTION 'SMOD'.
            WHEN 'BADI'.
              SET PARAMETER ID 'EXN' FIELD i_userexit-modname.
              CALL TRANSACTION 'SE18' AND SKIP FIRST SCREEN.
            WHEN 'BusTrEvent'.
              SUBMIT rfopfi00 WITH event = i_userexit-modname(8) AND RETURN.
            WHEN OTHERS.
              MESSAGE s030(cj). "Navigation not possible
          ENDCASE.
        WHEN 'T_USEREXIT-MODATTR-NAME'.
          IF NOT i_userexit-modattr-NAME IS INITIAL.
            SET PARAMETER ID 'MON_KUN' FIELD i_userexit-modattr-NAME.
            CALL TRANSACTION 'CMOD'.
          ELSE.
            MESSAGE s030(cj)."Navigation not possible
          ENDIF.
        WHEN OTHERS.
          MESSAGE s030(cj)."Navigation not possible
      ENDCASE.
  ENDCASE.
 
ENDFORM.                    "user_command
 
*&--------------------------------------------------------------------&*
*& AT LINE-SELECTION                                                  ௥*
*&--------------------------------------------------------------------&*
AT LINE-SELECTION.
 
  GET CURSOR FIELD w_fsel.
 
  CASE w_fsel.
 
    WHEN 'I_USEREXIT-MODNAME'.
      CASE i_userexit-TYPE.
        WHEN 'Enhancement'.
          SET PARAMETER ID 'MON' FIELD i_userexit-modname.
          CALL TRANSACTION 'SMOD'.
        WHEN 'BADI'.
          SET PARAMETER ID 'EXN' FIELD i_userexit-modname.
          CALL TRANSACTION 'SE18' AND SKIP FIRST SCREEN.
        WHEN 'BusTrEvent'.
          SUBMIT rfopfi00 WITH event = i_userexit-modname(8) AND RETURN.
        WHEN OTHERS.
          MESSAGE s030(cj)."Navigation not possible
      ENDCASE.
 
    WHEN 'I_USEREXIT-MODATTR-NAME'.
      IF NOT i_userexit-modattr-NAME IS INITIAL.
        SET PARAMETER ID 'MON_KUN' FIELD i_userexit-modattr-NAME.
        CALL TRANSACTION 'CMOD'.
      ELSE.
        MESSAGE s030(cj)."Navigation not possible
      ENDIF.
 
    WHEN OTHERS.
      MESSAGE s030(cj)."Navigation not possible
 
  ENDCASE.
 
*&--------------------------------------------------------------------&*
*& AT SELECTION-SCREEN                                                &*
*&--------------------------------------------------------------------&*
AT SELECTION-SCREEN ON RADIOBUTTON GROUP rad1.
 
* grey-out checkboxes if ALV selected
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_alv = c_x.
      IF screen-group1 = 'A01'.
        screen-İNPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'A01'.
        screen-İNPUT = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
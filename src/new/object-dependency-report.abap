*&---------------------------------------------------------------------*
*& Report ZNM_OBJ_DEP_TRANS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZNM_OBJ_DEP_TRANS.

*----------------------------------------------------------------------*
* Created by NM for Object Dependencies Checks and Transport
*----------------------------------------------------------------------*
* Text Symbols
* B01  Object selection
* B02  Aditional options
* C01  Remote
* C02  Obj. Desc.
* C03  Deepness
* C04  Scope
* CM1  Cross-system objects version check
* M01  Objects not found
* M02  Please fill all required fields
* M03  Critical error
* M04  No objects selected
* M05  Transport not allowed for multiple targets
* M06  Error creating transport request
* M07  Objects added to request (
* M08  Please select a remote system
* M09  RFCs destinations missing
* M10  Request canceled, object with $TEMP detected
* M11  Navigation not suported
* M12  Transport canceled
* M13  Object dependencies check not support
* M14  No dependecies found
* M15  Error treating transport request
* M16  Please fill deepness for standard objects
* O01  Equal
* O02  Different
* O03  New
* O04  No version
* P01  Adding object
* P02  Checking Dependecies
* P03  Checking Remote
* P04  Display objects
* P05  Processing options
* PB1  % Complete
*
* Selection Texts
* P_DEEP  Dependencies Deepness
* P_ERFC  Exclude RFCs if exist
* P_ICD  Include Tables CDs
* P_ILO  Include Tables Locks
* P_ITM  Include Tables Maintenances
* P_OBJECT  Object Type
* P_OBJ_N  Object Name
* P_PGMID  Program ID
* P_RFC  Remote Versions Checks
* P_RFC_D  System Name
* P_ST  Include Standard Objects
* P_TR  Transport Request
* R_OBJ  Check Workbench Object
* R_TR  Check Transport Request
*
* Standard Status GUI function codes: &ALL, &SAL, &OUP, &ODN, &ILT, %PC, &OL0, &OAD and &AVE
* &ALL ICON_SELECT_ALL
* &SAL ICON_DESELECT_ALL
* &OUP ICON_SORT_UP
* &ODN ICON_SORT_DOWN
* &ILT ICON_FILTER
* %PC ICON_EXPORT
* &OL0 ICON_ALV_VARIANTS
* &OAD ICON_ALV_VARIANT_CHOOSE
* &AVE ICON_ALV_VARIANT_SAVE
* TR ICON_IMPORT_TRANSPORT_REQUEST
* Status GUI function code: TR Create Transport 
*
*---------------------------------------------- LOCAL CLASS DEFINITION *
CLASS obj_dep_trans DEFINITION FINAL.
   PUBLIC SECTION.
     TYPE-POOLS: abap, icon.
*--------------------------------------------------- Public Structures *
     TYPES:
       BEGIN OF ty_objects,
         status   TYPE icon_d,       "Check status
         pgmid    TYPE pgmid,        "Program ID in Requests and Tasks
         object   TYPE trobjtype,    "Object Type
         obj_name TYPE sobj_name,    "Object Name in Object Directory
         obj_desc TYPE ddtext,       "Object Explanatory short text
         deep     TYPE i,            "Object deepness position
         scope    TYPE icon_d,       "Object Scope
         devclass TYPE developclass, "Development Package
         target   TYPE tr_target,    "Transport Target of Request
         remote(10),                 "Remote check status
       END OF ty_objects.
     DATA:
       t_objects   TYPE TABLE OF ty_objects, "Objects to transport
       t_objs_desc TYPE TABLE OF ko100,      "Objects prograns IDs
       t_e071      TYPE TABLE OF e071,       "Object Entries of Requests/Tasks
       t_e071k     TYPE TABLE OF e071k.
*------------------------------------------------------ Public Methods *
     METHODS:
*---------- Public Methods Constructor Definition ----------*
       constructor,
*---------- Set global values ----------*
       set_values IMPORTING i_robj   TYPE abap_bool   "Add from Object
                            i_pgmid  TYPE pgmid       "Program ID in Requests and Tasks
                            i_object TYPE trobjtype   "Object Type
                            i_obj_n  TYPE sobj_name   "Object Name in Object Directory
                            i_rtr    TYPE abap_bool   "Add from TR
                            i_tr     TYPE trkorr      "Transport request
                            i_deep   TYPE i           "Dependencies deepness
                            i_rfc    TYPE abap_bool   "Remote versions checks
                            i_rfc_d  TYPE tmssysnam   "System Name
                            i_st     TYPE abap_bool   "Include standard objects
                            i_itm    TYPE abap_bool   "Include Tables Maintenance
                            i_ilo    TYPE abap_bool   "Include Lock objects
                            i_icd    TYPE abap_bool   "Include Change documents
                            i_erfc   TYPE abap_bool,  "Exclude RFCs if exist
*---------- Public Methods PGMID Definition ----------*
       pgmid_f4,
*---------- Public Methods Object F4 Definition ----------*
       object_f4 CHANGING c_pgmid  TYPE pgmid      "Program ID in Requests and Tasks
                          c_object TYPE trobjtype, "Object Type
*---------- Public Methods Object Name F4 Definition ----------*
       object_name_f4 IMPORTING i_object TYPE trobjtype  "Object Type
                       CHANGING c_obj_n  TYPE sobj_name, "Object Name in Object Directory
*---------- Public Methods Transport Request F4 Definition ----------*
       tr_f4 CHANGING c_tr TYPE trkorr,  "Request/Task
*---------- Public Methods RFC F4 Definition ----------*
       rfc_f4 CHANGING c_rfc_d TYPE tmssysnam, "TMS: System Name
*---------- Public Methods Screen PAI Definition ----------*
       screen_pai,
*---------- Public Methods Run Checks Definition ----------*
       run_checks,
*---------- Public Methods Display Objects Definition ----------*
       display_objects.
*----------------------------------------------------- Public Handlers *
     METHODS:
*---------- Public Handler On User Command Definition ----------*
       on_user_command FOR EVENT added_function OF cl_salv_events  IMPORTING e_salv_function,
*---------- Public Handler On Double Click Definition ----------*
       on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column. "#EC NEEDED
   PRIVATE SECTION.
*--------------------------------------------------- Private Constants *
     DATA:
       c_r3tr TYPE pgmid        VALUE 'R3TR',  "Main object
       c_tobj TYPE trobjtype    VALUE 'TOBJ',  "Table content in transport
       c_chdo TYPE trobjtype    VALUE 'CHDO',  "Change documents
       c_fugr TYPE trobjtype    VALUE 'FUGR',  "Function group
       c_tabl TYPE trobjtype    VALUE 'TABL',  "Table
       c_devc TYPE trobjtype    VALUE 'DEVC',  "Development class
       c_temp TYPE developclass VALUE '$TMP'.  "Local development class
*--------------------------------------------------- Private Variables *
     DATA:
       v_robj    TYPE abap_bool, "Add from Object
       v_pgmid   TYPE pgmid,     "Program ID in Requests and Tasks
       v_object  TYPE trobjtype, "Object Type
       v_obj_n   TYPE sobj_name, "Object Name in Object Directory
       v_rtr     TYPE abap_bool, "Add from TR
       v_tr      TYPE trkorr,    "Transport request
       v_deep    TYPE i,         "Dependencies deepness
       v_rfc     TYPE abap_bool, "Remote versions checks
       v_rfc_d   TYPE tmssysnam, "System Name
       v_st      TYPE abap_bool, "Include standard objects
       v_itm     TYPE abap_bool, "Include Tables Maintenance
       v_ilo     TYPE abap_bool, "Include Lock objects
       v_icd     TYPE abap_bool, "Include Change documents
       v_erfc    TYPE abap_bool, "Exclude RFCs if exist
       v_percent TYPE i.         "Progress bar percentage
*--------------------------------------------------- Private Class ALV *
     DATA o_objects TYPE REF TO cl_salv_table. "Objects ALV
*----------------------------------------------------- Private Methods *
     METHODS:
*---------- Private Methods Progress Bar Definition ----------*
       progress_bar IMPORTING value(i_value) TYPE itex132 "#EC CI_VALPAR
                                    i_tabix  TYPE i,
*---------- Private Methods Execute Add Objects Definition ----------*
       execute_add_objects,
*---------- Private Methods Execute Add From Transport Definition ----------*
       execute_add_from_transport,
*---------- Private Methods Check Add Object Definition *
       check_add_object IMPORTING value(i_pgmid)  TYPE pgmid "Program ID in Requests and Tasks
                                  value(i_object) TYPE any   "Object Type
                                  value(i_obj_n)  TYPE any   "Object Name in Object Directory
                                  is_env_tab      TYPE senvi "Info system
                                  i_deep          TYPE i,    "Deepness
       add_objects_of_devclass IMPORTING i_obj_n TYPE any "Object Name in Object Directory
                                         i_deep  TYPE i,  "Deepness
*---------- Private Methods Add Object Definition ----------*
       add_object IMPORTING i_deep    TYPE i            "Deepness
                   CHANGING cs_object TYPE ty_objects,  "Objects table
*---------- Private Methods Objects Dependencies Check Definition ----------*
       objects_dependencies_check,
*---------- Private Methods Remote Objects Check Definition ----------*
       remote_objects_check,
*---------- Private Methods Exclude RFCs Definition ----------*
       exclude_rfcs IMPORTING i_obj_name TYPE sobj_name  "Object Name in Object Directory
                     CHANGING c_no_rfc   TYPE abap_bool, "Found RFC flag
*---------- Private Methods Include Maintenances Definition ----------*
       include_maintenances,
*---------- Private Methods Include Locks Definition ----------*
       include_locks,
*---------- Private Methods CDs Definition ----------*
       include_cds,
*---------- Private Methods Add TOBJ Content Definition ----------*
       add_tobj_content IMPORTING i_obj_name TYPE sobj_name, "Object Name in Object Directory
*---------- Private Methods Object Header Definition ----------*
       add_object_header IMPORTING i_pgmid   TYPE pgmid    "Program ID in Requests and Tasks
                                   i_object  TYPE any      "Object Type
                                   i_obj_n   TYPE any      "Object Name in Object Directory
                                   i_objfunc TYPE objfunc, "Object function
*---------- Private Methods Object Keys Definition ----------*
       add_object_keys IMPORTING i_pgmid  TYPE pgmid "Program ID in Requests and Tasks
                                 i_object TYPE any   "Object Type
                                 i_obj_n  TYPE any   "Object Name in Object Directory
                                 i_tabkey TYPE any.  "Table key
ENDCLASS.                    "obj_dep_trans DEFINITION
*--------------------------------------------------------- GLOBAL DATA *
DATA go_odt TYPE REF TO obj_dep_trans.                      "#EC NEEDED
*---------------------------------------------------- SELECTION SCREEN *
*---------------------------------------------------- Object selection *
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECTION-SCREEN SKIP 1.
PARAMETERS r_obj RADIOBUTTON GROUP rbt USER-COMMAND rbt DEFAULT 'X'.  "Add from Objects or Development Package
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETERS:
   p_pgmid  TYPE pgmid DEFAULT 'R3TR', "Program ID in Requests and Tasks
   p_object TYPE trobjtype,            "Object Type
   p_obj_n  TYPE sobj_name.            "Object Name in Object Directory
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS r_tr RADIOBUTTON GROUP rbt.  "Add from TR
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 4.
PARAMETERS p_tr TYPE trkorr.  "Transport request
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_deep TYPE i DEFAULT 0. "Dependencies deepness
SELECTION-SCREEN END OF BLOCK b01.
*-------------------------------------------------- Aditional options *
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_rfc AS CHECKBOX. "Remote versions checks
SELECTION-SCREEN COMMENT (37) com_rfc.
PARAMETERS p_rfc_d TYPE tmssysnam.  "System Name
SELECTION-SCREEN END OF LINE.
PARAMETERS p_st AS CHECKBOX.  "Include standard objects
SELECTION-SCREEN SKIP 1.
PARAMETERS:
   p_itm  AS CHECKBOX, "Include Tables Maintenance
   p_ilo  AS CHECKBOX, "Include Lock objects
   p_icd  AS CHECKBOX, "Include Change documents
   p_erfc AS CHECKBOX. "Exclude RFCs if exist
SELECTION-SCREEN END OF BLOCK b02.
*------------------------------------------ SELECTION SCREEN PAI HELPS *
*------------------------------------------------------- Program ID F4 *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pgmid.
   go_odt->pgmid_f4( ).
*------------------------------------------------------ Object Type F4 *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_object.
   go_odt->object_f4( CHANGING c_pgmid = p_pgmid c_object = p_object ).
*------------------------------------------------------ Object Name F4 *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_obj_n.
   go_odt->object_name_f4( EXPORTING i_object = p_object CHANGING c_obj_n = p_obj_n ).
*------------------------------------------------ Transport request F4 *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tr.
   go_odt->tr_f4( CHANGING c_tr = p_tr ).
*-------------------------------------------------- Systems and RFC F4 *
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rfc_d.
   go_odt->rfc_f4( CHANGING c_rfc_d = p_rfc_d ).
*------------------------------------------------ SELECTION SCREEN PAI *
AT SELECTION-SCREEN.
   go_odt->set_values( i_robj   = r_obj      "Add from Object
                       i_pgmid  = p_pgmid    "Program ID in Requests and Tasks
                       i_object = p_object   "Object Type
                       i_obj_n  = p_obj_n    "Object Name in Object Directory
                       i_rtr    = r_tr       "Add from TR
                       i_tr     = p_tr       "Transport request
                       i_deep   = p_deep     "Dependencies deepness
                       i_rfc    = p_rfc      "Remote versions checks
                       i_rfc_d  = p_rfc_d    "System Name
                       i_st     = p_st       "Include standard objects
                       i_itm    = p_itm      "Include Tables Maintenance
                       i_ilo    = p_ilo      "Include Lock objects
                       i_icd    = p_icd      "Include Change documents
                       i_erfc   = p_erfc ).  "Exclude RFCs if exist
   go_odt->screen_pai( ).  "Screen Process after input
*------------------------------------------------------- REPORT EVENTS *
*----------------------------------------------- Initialization events *
INITIALIZATION.
   com_rfc = 'Cross-system objects version check'(cm1).
   CREATE OBJECT go_odt. "Create main class
*---------------------------------------------------- Executing events *
START-OF-SELECTION.
   go_odt->run_checks( ).      "Execution
   go_odt->display_objects( ). "Result display
*------------------------------------------ LOCAL CLASS IMPLEMENTATION *
CLASS obj_dep_trans IMPLEMENTATION.
*---------------------------- Public Method Constructor Implementation *
   METHOD constructor.
     CALL FUNCTION 'TR_OBJECT_TABLE' "Fill Program IDs
       TABLES
         wt_object_text = t_objs_desc.
   ENDMETHOD.                    "constructor
*-------------------------- Public Method to Set Values Implementation *
   METHOD set_values.
     v_robj   = i_robj.   "Add from Object
     v_pgmid  = i_pgmid.  "Program ID in Requests and Tasks
     v_object = i_object. "Object Type
     v_obj_n  = i_obj_n.  "Object Name in Object Directory
     v_rtr    = i_rtr.    "Add from TR
     v_tr     = i_tr.     "Transport request
     v_deep   = i_deep.   "Dependencies deepness
     v_rfc    = i_rfc.    "Remote versions checks
     v_rfc_d  = i_rfc_d.  "System Name
     v_st     = i_st.     "Include standard objects
     v_itm    = i_itm.    "Include Tables Maintenance
     v_ilo    = i_ilo.    "Include Lock objects
     v_icd    = i_icd.    "Include Change documents
     v_erfc   = i_erfc.   "Exclude RFCs if exist
   ENDMETHOD.                    "set_values
*------------------------------- Public Method PGMID F4 Implementation *
   METHOD pgmid_f4.
     DATA lt_pgmids TYPE TABLE OF ko101.  "Program IDs with Description
*---------- Read PGMID ----------*
     CALL FUNCTION 'TR_PGMID_TABLE'
       TABLES
         wt_pgmid_text = lt_pgmids.
*---------- Set PGMID F4 ----------*
     CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST' "#EC FB_RC "#EC CI_SUBRC
       EXPORTING
         retfield        = 'PGMID'
         dynpprog        = sy-cprog
         value_org       = 'S'
         dynpnr          = '1000'
         dynprofield     = 'TRE071X-PGMID'
       TABLES
         value_tab       = lt_pgmids
       EXCEPTIONS
         parameter_error = 1
         no_values_found = 2
         OTHERS          = 3.
   ENDMETHOD.                    "pgmid_f4
*------------------------------ Public Method Object F4 Implementation *
   METHOD object_f4.
     CONSTANTS:
       lc_object TYPE fieldname VALUE 'OBJECT', "Object field
       lc_pgmid  TYPE fieldname VALUE 'PGMID'.  "Program ID in Requests and Tasks field
     DATA:
       lt_shlp          TYPE shlp_descr,           "Description of Search Help
       lt_return_values TYPE TABLE OF ddshretval,  "Interface Structure Search Help
       ls_return_values LIKE LINE OF lt_return_values,
       lv_rc            TYPE sysubrc.              "Return Value of ABAP Statements
     FIELD-SYMBOLS <interface> TYPE ddshiface. "Interface description of a F4 help method
*---------- Get search help ----------*
     CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
       EXPORTING
         shlpname = 'SCTSOBJECT'
       IMPORTING
         shlp     = lt_shlp.
*---------- Fill search help ----------*
     LOOP AT lt_shlp-interface ASSIGNING <interface>.
       IF <interface>-shlpfield = lc_object.
         <interface>-valfield = abap_true.
         <interface>-value    = c_object.
       ENDIF.
       IF <interface>-shlpfield = lc_pgmid.
         <interface>-valfield = abap_true.
         <interface>-value    = c_pgmid.
       ENDIF.
     ENDLOOP.
*---------- Call search help ----------*
     CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
       EXPORTING
         shlp          = lt_shlp
       IMPORTING
         rc            = lv_rc
       TABLES
         return_values = lt_return_values.
*---------- Set search help return ----------*
     IF lv_rc IS INITIAL.
       READ TABLE lt_return_values INTO ls_return_values WITH KEY fieldname = lc_object.
       IF sy-subrc IS INITIAL.
         c_object = ls_return_values-fieldval.
       ENDIF.
       READ TABLE lt_return_values INTO ls_return_values WITH KEY fieldname = lc_pgmid.
       IF sy-subrc IS INITIAL.
         c_pgmid = ls_return_values-fieldval.
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "object_f4
*------------------------- Public Method Object Name F4 Implementation *
   METHOD object_name_f4.
     DATA lv_object_type TYPE seu_obj.  "Object type
*---------- Get objects repository information ----------*
     lv_object_type = i_object.
     CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'  "#EC FB_RC "#EC CI_SUBRC
       EXPORTING
         object_type          = lv_object_type
         object_name          = c_obj_n
       IMPORTING
         object_name_selected = c_obj_n
       EXCEPTIONS
         cancel               = 1
         wrong_type           = 2
         OTHERS               = 3.
   ENDMETHOD.                    "object_name_f4
*------------------- Public Method Transport Request F4 Implementation *
   METHOD tr_f4.
     CALL FUNCTION 'TR_F4_REQUESTS'
       IMPORTING
         ev_selected_request = c_tr.
   ENDMETHOD.                    "tr_f4
*--------------------------------- Public Method RFC F4 Implementation *
   METHOD rfc_f4.
     CALL FUNCTION 'TMS_UI_F4_SYSTEMS'
       CHANGING
         cv_system = c_rfc_d.
   ENDMETHOD.                    "rfc_f4
*----------------------------- Public Method Screen Pai Implementation *
   METHOD screen_pai.
     IF sy-ucomm = 'ONLI'.
*---------- Check required data ----------*
       IF ( v_robj IS NOT INITIAL AND ( v_pgmid IS INITIAL OR v_object IS INITIAL OR v_obj_n IS INITIAL ) ) OR
          ( v_rtr IS NOT INITIAL AND v_tr IS INITIAL ) OR
          ( v_rfc IS NOT INITIAL AND v_rfc_d IS INITIAL ).
         MESSAGE e398(00) WITH 'Please fill all required fields'(m02) space space space DISPLAY LIKE 'W'.
       ENDIF.
       IF v_rfc IS NOT INITIAL AND v_rfc_d = sy-sysid.
         MESSAGE e398(00) WITH 'Please select a remote system'(m08) space space space DISPLAY LIKE 'W'.
       ENDIF.
*---------- Add first object ----------*
       progress_bar( i_value = 'Adding object'(p01) i_tabix = '10' ).
       CASE abap_true.
         WHEN v_robj.  "Add object or dev class objects
           execute_add_objects( ).
         WHEN v_rtr. "Add TR objects
           execute_add_from_transport( ).
       ENDCASE.
       IF t_objects IS INITIAL.
         MESSAGE e398(00) WITH 'Objects not found'(m01) space space space DISPLAY LIKE 'W'.
       ENDIF.
       IF v_st IS NOT INITIAL AND ( v_deep IS INITIAL OR v_deep = 0 ). "Performance Test
         MESSAGE e398(00) WITH 'Please fill deepness for standard objects'(m16) space space space DISPLAY LIKE 'I'.
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "screen_pai
*----------------------------- Public Method Run Checks Implementation *
   METHOD run_checks.
     IF t_objects IS NOT INITIAL.
*---------- Dependecies check ----------*
       progress_bar( i_value = 'Checking Dependecies'(p02) i_tabix = '20' ).
       objects_dependencies_check( ).
       progress_bar( i_value = 'Processing options'(p05) i_tabix = '50' ).
*---------- Include Tables Maintenance ----------*
       IF v_itm IS NOT INITIAL.
         include_maintenances( ).
         objects_dependencies_check( ).
       ENDIF.
*---------- Include Lock objects ----------*
       IF v_ilo IS NOT INITIAL. include_locks( ). ENDIF.
*---------- Include Change documents ----------*
       IF v_icd IS NOT INITIAL. include_cds( ). ENDIF.
*---------- Remote check ----------*
       IF v_rfc IS NOT INITIAL.
         progress_bar( i_value = 'Checking Remote'(p03) i_tabix = '80' ).
         remote_objects_check( ).
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "run_checks
*------------------------ Public Method Display Objects Implementation *
   METHOD display_objects.
     DATA:
       lr_events        TYPE REF TO cl_salv_events_table,      "ALV Events
       lr_display       TYPE REF TO cl_salv_display_settings,  "ALV Output Appearance
       lr_columns       TYPE REF TO cl_salv_columns_table,     "ALV Columns
       lr_column        TYPE REF TO cl_salv_column_table,
       lr_selections    TYPE REF TO cl_salv_selections,        "ALV Selections
       lr_layout        TYPE REF TO cl_salv_layout.            "ALV Layout
     DATA:
       lt_column_ref TYPE salv_t_column_ref, "Columns of ALV List
       ls_column_ref TYPE salv_s_column_ref,
       ls_key        TYPE salv_s_layout_key.
     DATA:
       lv_title   TYPE lvc_title,  "ALV title
       lv_lines   TYPE i,          "Number of objects
       lv_lines_c TYPE string.
     IF t_objects IS NOT INITIAL.
       progress_bar( i_value = 'Display objects'(p04) i_tabix = '90' ).
       IF o_objects IS NOT BOUND.
         TRY.
             IF lines( t_objects ) = 1.
               MESSAGE s398(00) WITH 'No dependecies found'(m14) space space space DISPLAY LIKE 'W'.
             ELSE.
               SORT t_objects BY deep pgmid object obj_name.
             ENDIF.
*---------- Create ALV ----------*
             cl_salv_table=>factory( IMPORTING r_salv_table = o_objects
                                      CHANGING t_table      = t_objects ).
*---------- Set ALV Functions ----------*
             o_objects->set_screen_status(
               pfstatus      = 'STATUS'
               report        = sy-cprog
               set_functions = o_objects->c_functions_all ).
*---------- Set Layout ----------*
             lr_layout = o_objects->get_layout( ).
             ls_key-report = sy-repid.
             lr_layout->set_key( ls_key ).
             lr_layout->set_save_restriction( ).
*---------- Set ALV selections ----------*
             lr_selections = o_objects->get_selections( ).
             lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
*---------- Set ALV Display and Title ----------*
             lr_display = o_objects->get_display_settings( ).
             lr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).
             lv_lines = lines( t_objects ).
             lv_lines_c = lv_lines.
             CONDENSE lv_lines_c NO-GAPS.
             CONCATENATE '(' lv_lines_c ')' INTO lv_lines_c.
             CONCATENATE sy-title lv_lines_c INTO lv_title SEPARATED BY space.
             lr_display->set_list_header( lv_title ).
*---------- Set ALV Columns ----------*
             lr_columns = o_objects->get_columns( ).
             lr_columns->set_key_fixation( ).
             lr_columns->set_optimize( ).
             lt_column_ref = lr_columns->get( ).
             LOOP AT lt_column_ref INTO ls_column_ref. "Default format for all columns
               lr_column ?= lr_columns->get_column( ls_column_ref-columnname ).
               lr_column->set_f4( if_salv_c_bool_sap=>false ).
               lr_column->set_alignment( if_salv_c_alignment=>centered ).
               IF ls_column_ref-columnname = 'STATUS' OR
                  ls_column_ref-columnname = 'PGMID' OR
                  ls_column_ref-columnname = 'OBJECT' OR
                  ls_column_ref-columnname = 'OBJ_NAME'.
                 lr_column->set_key( if_salv_c_bool_sap=>true ).
               ENDIF.
               IF ls_column_ref-columnname = 'OBJ_NAME' OR
                  ls_column_ref-columnname = 'DEVCLASS'.
                 lr_column->set_alignment( if_salv_c_alignment=>left ).
               ENDIF.
               IF ls_column_ref-columnname = 'OBJ_DESC'.
                 lr_column->set_alignment( if_salv_c_alignment=>left ).
*                 lr_column->set_short_text( 'Obj. Desc.'(c02) ).
                 lr_column->set_medium_text( 'Obj. Desc.'(c02) ).
                 lr_column->set_long_text( 'Obj. Desc.'(c02) ).
               ENDIF.
               IF ls_column_ref-columnname = 'DEEP'.
*                 lr_column->set_short_text( 'Deepness'(c03) ).
                 lr_column->set_medium_text( 'Deepness'(c03) ).
                 lr_column->set_long_text( 'Deepness'(c03) ).
               ENDIF.
               IF ls_column_ref-columnname = 'SCOPE'.
*                 lr_column->set_short_text( 'Scope'(c04) ).
                 lr_column->set_medium_text( 'Scope'(c04) ).
                 lr_column->set_long_text( 'Scope'(c04) ).
               ENDIF.
               IF ls_column_ref-columnname = 'REMOTE'.
                 IF v_rfc IS INITIAL.
                   lr_column->set_visible( if_salv_c_bool_sap=>false ).
                 ELSE.
*                   lr_column->set_short_text( 'Remote'(c01) ).
                   lr_column->set_medium_text( 'Remote'(c01) ).
                   lr_column->set_long_text( 'Remote'(c01) ).
                 ENDIF.
               ENDIF.
             ENDLOOP.
*---------- Register ALV Events ----------*
             lr_events = o_objects->get_event( ).
             SET HANDLER on_user_command FOR lr_events.
             SET HANDLER on_double_click FOR lr_events.
*---------- Display Objects ALV ----------*
             o_objects->display( ).
           CATCH cx_root.                                 "#EC CATCH_ALL
             MESSAGE s398(00) WITH 'Critical error'(m03) space space space DISPLAY LIKE 'E'.
         ENDTRY.
       ELSE. "Refresh ALV
         o_objects->refresh( ).
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "display_objects
*--------------- Public Handler On User Command Handler Implementation *
   METHOD on_user_command.
     CHECK e_salv_function = 'TR'.  "Create transport request
     DATA lr_selections TYPE REF TO cl_salv_selections. "ALV Selections
     DATA:
       lt_rows       TYPE salv_t_row,          "ALV Rows
       ls_row        TYPE i,
       lt_e071_temp  TYPE TABLE OF e071,       "Change & Transport System: Object Entries of Requests/Tasks
       ls_e071       LIKE LINE OF t_e071,
       lt_e071k_temp TYPE TABLE OF e071k,
       lt_objects    TYPE TABLE OF ty_objects, "Objects to transport
       ls_object     LIKE LINE OF t_objects,
       lt_targets    TYPE TABLE OF tr_target,  "Transport Target of Request
       ls_target     LIKE LINE OF lt_targets.
     DATA:
       lv_order TYPE trkorr, "Request/Task
       lv_task  TYPE trkorr.
*---------- Get selected lines ----------*
     lr_selections = o_objects->get_selections( ).
     lt_rows = lr_selections->get_selected_rows( ).
*---------- Get selected objects to transport ----------*
     LOOP AT lt_rows INTO ls_row.
       READ TABLE t_objects INTO ls_object INDEX ls_row.
       IF sy-subrc IS INITIAL AND ls_object-scope = icon_transport AND ( ls_object-status = icon_led_yellow OR ls_object-status = icon_led_green ).
         IF ls_object-devclass = c_temp.
           MESSAGE i398(00) WITH 'Request canceled, object with $TEMP detected'(m10) space space space DISPLAY LIKE 'E'.
           RETURN.
         ENDIF.
         APPEND ls_object TO lt_objects.
         MOVE-CORRESPONDING ls_object TO ls_e071.
         APPEND ls_e071 TO t_e071.
         IF ls_object-object = c_tobj. "Add TABU object directly to the transport
           add_tobj_content( i_obj_name = ls_object-obj_name ).  "Object Name in Object Directory
         ENDIF.
       ENDIF.
     ENDLOOP.
*---------- Get possible target ----------*
     LOOP AT lt_objects INTO ls_object.
       ls_target = ls_object-target.
       APPEND ls_target TO lt_targets.
     ENDLOOP.
     SORT lt_targets.
     DELETE ADJACENT DUPLICATES FROM lt_targets.
*---------- Create transport request and task ----------*
     IF lt_objects IS NOT INITIAL. "Objects selected to transport
       IF lines( lt_targets ) = 1. "Only one valid target
         CALL FUNCTION 'TRINT_ORDER_CHOICE'  "Create transport request
           EXPORTING
             iv_tarsystem           = ls_target
           IMPORTING
             we_order               = lv_order
             we_task                = lv_task
           TABLES
             wt_e071                = lt_e071_temp
             wt_e071k               = lt_e071k_temp
           EXCEPTIONS
             no_correction_selected = 1
             display_mode           = 2
             object_append_error    = 3
             recursive_call         = 4
             wrong_order_type       = 5
             OTHERS                 = 6.
         IF sy-subrc IS INITIAL AND lv_task IS NOT INITIAL.
           ls_e071-pgmid    = c_r3tr.  "Add objects development class to transport
           ls_e071-object   = c_devc.
           ls_e071-obj_name = ls_object-devclass.
           APPEND ls_e071 TO t_e071.
           CALL FUNCTION 'TRINT_APPEND_COMM' "Add object to transport request
             EXPORTING
               wi_exclusive       = abap_false
               wi_sel_e071        = abap_true
               wi_sel_e071k       = abap_true
               wi_trkorr          = lv_task
             TABLES
               wt_e071            = t_e071
               wt_e071k           = t_e071k
             EXCEPTIONS
               e071k_append_error = 1
               e071_append_error  = 2
               trkorr_empty       = 3
               OTHERS             = 4.
           IF sy-subrc IS INITIAL. "Added with sucess
*---------- Sort and compress request --------*
             CALL FUNCTION 'TR_SORT_AND_COMPRESS_COMM' "#EC FB_RC   "#EC CI_SUBRC
               EXPORTING
                 iv_trkorr                      = lv_task
               EXCEPTIONS
                 trkorr_not_found               = 1
                 order_released                 = 2
                 error_while_modifying_obj_list = 3
                 tr_enqueue_failed              = 4
                 no_authorization               = 5
                 OTHERS                         = 6.
             MESSAGE i001(00) WITH 'Objects added to request ('(m07) lv_order ')' space.
           ELSE.
             MESSAGE s398(00) WITH 'Error creating transport request'(m06) space space space DISPLAY LIKE 'E'.
           ENDIF.
         ELSE.
           MESSAGE s398(00) WITH 'Transport canceled'(m12) space space space DISPLAY LIKE 'W'.
         ENDIF.
       ELSE.
         MESSAGE i398(00) WITH 'Transport not allowed for multiple targets'(m05) space space space.
       ENDIF.
     ELSE.
       MESSAGE i398(00) WITH 'No objects selected'(m04) space space space.
     ENDIF.
   ENDMETHOD.                    "on_user_command
*----------------------- Public Handler On Double Click Implementation *
   METHOD on_double_click.
     DATA ls_object LIKE LINE OF t_objects.  "Objects to transport
     READ TABLE t_objects INTO ls_object INDEX row.  "Get selected Row
     IF sy-subrc IS INITIAL.
       IF column = 'DEVCLASS' OR column = 'TARGET'.  "Call Development Package
         IF ls_object-devclass IS NOT INITIAL.
           SET PARAMETER ID 'PACKNAME' FIELD ls_object-devclass.
           CALL TRANSACTION 'SE21' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
         ENDIF.
       ELSE.
*---------- Display objects ----------*
         CASE ls_object-object.
           WHEN c_tobj.  "Display Tables Maintenance
             SET PARAMETER ID 'DVI' FIELD ls_object-obj_name.
             CALL TRANSACTION 'SE54'.                     "#EC CI_CALLTA
           WHEN c_chdo.  "Display change documents
             CALL TRANSACTION 'SCDO'.                     "#EC CI_CALLTA
           WHEN OTHERS.  "Display all
             CALL FUNCTION 'RS_TOOL_ACCESS'
               EXPORTING
                 operation           = 'SHOW'
                 object_name         = ls_object-obj_name
                 object_type         = ls_object-object
               EXCEPTIONS
                 not_executed        = 1
                 invalid_object_type = 2
                 OTHERS              = 3.
             IF sy-subrc IS NOT INITIAL.
               MESSAGE s398(00) WITH 'Navigation not suported'(m11) space space space DISPLAY LIKE 'W'.
             ENDIF.
         ENDCASE.
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "on_double_click
*-------------------------- Private Method Progress Bar Implementation *
   METHOD progress_bar.
     DATA:
       lv_text(40),
       lv_percentage TYPE p,
       lv_percent_char(3).
     lv_percentage = ( i_tabix / 100 ) * 100.
     lv_percent_char = lv_percentage.
     SHIFT lv_percent_char LEFT DELETING LEADING ' '.
     CONCATENATE i_value '...' INTO i_value.
     CONCATENATE i_value lv_percent_char '% Complete'(pb1) INTO lv_text SEPARATED BY space.
     IF lv_percentage GT v_percent OR i_tabix = 1.
       CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
           percentage = lv_percentage
           text       = lv_text.
       v_percent = lv_percentage.
     ENDIF.
   ENDMETHOD.                    "progress_bar
*------------------- Private Method Execute Add Objects Implementation *
   METHOD execute_add_objects.
     DATA ls_env_dummy  TYPE senvi.
     CASE v_object.
*---------- Development class ----------*
       WHEN c_devc.
         add_objects_of_devclass( i_obj_n = v_obj_n  "Object Name in Object Directory
                                  i_deep  = '0' ).   "Deepness
*---------- All others object ----------*
       WHEN OTHERS.
         check_add_object( i_pgmid    = v_pgmid      "Program ID in Requests and Tasks
                           i_object   = v_object     "Object Type
                           i_obj_n    = v_obj_n      "Object Name in Object Directory
                           is_env_tab = ls_env_dummy "Info system
                           i_deep     = '0' ).       "Deepness
     ENDCASE.
   ENDMETHOD.                    "execute_add_objects
*------------ Private Method Execute Add from Transport Implementation *
   METHOD execute_add_from_transport.
     DATA:
       lt_request_headers TYPE trwbo_request_headers,  "Context of a request
       ls_request_headers TYPE trwbo_request_header,
       lt_objects         TYPE tr_objects,             "Objects
       ls_object          TYPE e071,                   "Object Entries of Requests/Tasks
       ls_env_dummy       TYPE senvi.                  "System Info Environement
*---------- Read Requests and Tasks ----------*
     CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
       EXPORTING
         iv_trkorr          = v_tr
       IMPORTING
         et_request_headers = lt_request_headers
       EXCEPTIONS
         invalid_input      = 1
         OTHERS             = 2.
     IF sy-subrc IS NOT INITIAL.
       MESSAGE e398(00) WITH 'Error treating transport request'(m15) space space space DISPLAY LIKE 'W'.
     ENDIF.
*---------- Read objects inside main request ----------*
     READ TABLE lt_request_headers INTO ls_request_headers WITH KEY trfunction = 'K'.
     IF sy-subrc IS NOT INITIAL.
       MESSAGE e398(00) WITH 'Error treating transport request'(m15) space space space DISPLAY LIKE 'W'.
     ENDIF.
     CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
       EXPORTING
         is_request_header      = ls_request_headers
         iv_condense_objectlist = 'X'
       IMPORTING
         et_objects             = lt_objects
       EXCEPTIONS
         invalid_input          = 1
         OTHERS                 = 2.
     IF sy-subrc IS NOT INITIAL.
       MESSAGE e398(00) WITH 'Error treating transport request'(m15) space space space DISPLAY LIKE 'W'.
     ENDIF.
     CALL FUNCTION 'TR_SORT_OBJECT_AND_KEY_LIST'
       CHANGING
         ct_objects = lt_objects.
     LOOP AT lt_objects INTO ls_object.  "Add found objects to processing
       check_add_object( i_pgmid    = ls_object-pgmid    "Program ID in Requests and Tasks
                         i_object   = ls_object-object   "Object Type
                         i_obj_n    = ls_object-obj_name "Object Name in Object Directory
                         is_env_tab = ls_env_dummy       "Info system
                         i_deep     = '0' ).             "Deepness
     ENDLOOP.
   ENDMETHOD.                    "execute_add_from_transport
*---------------------- Private Method Check Add Object Implementation *
   METHOD check_add_object.
     DATA lo_wb_object TYPE REF TO cl_wb_object. "Repository Object
     DATA:
       ls_tadir          TYPE tadir,             "Directory of Repository Objects
       ls_wb_object_type TYPE wbobjtype,         "Global WB Type
       ls_object         LIKE LINE OF t_objects. "Objects to transport line
     DATA:
       lv_tr_object   TYPE trobjtype,  "Object Type
       lv_tr_obj_name TYPE trobj_name, "Object Name in Object List
       lv_trans_pgmid TYPE pgmid.      "Program ID in Requests and Tasks
*---------- Object convertions ----------*
     IF i_pgmid <> c_r3tr.
       SELECT pgmid UP TO 1 ROWS FROM tadir              "#EC CI_GENBUFF
         INTO i_pgmid
        WHERE object   = i_object
          AND obj_name = i_obj_n.
       ENDSELECT.
*---------- Is not a TADIR object and Conversion required ----------*
       IF sy-subrc IS NOT INITIAL.
         lv_tr_object   = i_object.
         lv_tr_obj_name = i_obj_n.
         cl_wb_object=>create_from_transport_key( EXPORTING p_object                = lv_tr_object
                                                            p_obj_name              = lv_tr_obj_name
                                                  RECEIVING p_wb_object             = lo_wb_object
                                                 EXCEPTIONS objecttype_not_existing = 1
                                                            empty_object_key        = 2
                                                            key_not_available       = 3
                                                            OTHERS                  = 4 ).
         IF sy-subrc IS INITIAL.
           lo_wb_object->get_global_wb_key( IMPORTING p_object_type     = ls_wb_object_type
                                           EXCEPTIONS key_not_available = 1
                                                      OTHERS            = 2 ).
           IF sy-subrc IS INITIAL.
             lo_wb_object->get_transport_key( IMPORTING p_pgmid           = lv_trans_pgmid "#EC CI_SUBRC
                                             EXCEPTIONS key_not_available = 1
                                                        OTHERS            = 2 ).
*---------- Check Program ID ----------*
             CASE lv_trans_pgmid.
               WHEN c_r3tr.  "Main objects
                 i_pgmid = lv_trans_pgmid.
               WHEN 'LIMU'.  "Sub object
                 CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
                   EXPORTING
                     p_limu_objtype = lv_tr_object
                     p_limu_objname = lv_tr_obj_name
                   IMPORTING
                     p_r3tr_objtype = lv_tr_object
                     p_r3tr_objname = lv_tr_obj_name
                   EXCEPTIONS
                     no_mapping     = 1
                     OTHERS         = 2.
                 IF sy-subrc IS INITIAL.
                   ls_object-pgmid    = c_r3tr.
                   ls_object-object   = lv_tr_object.
                   ls_object-obj_name = lv_tr_obj_name.
                   add_object( EXPORTING i_deep    = i_deep        "Deepness
                                CHANGING cs_object = ls_object ).  "Objects table
                   RETURN.
                 ENDIF.
               WHEN OTHERS.  "Include objects
                 i_pgmid = c_r3tr.
                 CALL FUNCTION 'GET_TADIR_TYPE_FROM_WB_TYPE'
                   EXPORTING
                     wb_objtype        = ls_wb_object_type-subtype_wb
                   IMPORTING
                     transport_objtype = lv_tr_object
                   EXCEPTIONS
                     no_mapping_found  = 1
                     no_unique_mapping = 2
                     OTHERS            = 3.
                 IF sy-subrc IS INITIAL.
                   i_object = lv_tr_object.
                   IF is_env_tab-encl_obj IS NOT INITIAL.
                     i_obj_n = is_env_tab-encl_obj.
                   ENDIF.
                 ENDIF.
             ENDCASE.
           ENDIF.
         ENDIF.
       ENDIF.
     ENDIF.
*---------- Check in TADIR ----------*
     SELECT SINGLE * FROM tadir
       INTO ls_tadir
      WHERE pgmid    = i_pgmid
        AND object   = i_object
        AND obj_name = i_obj_n.
*---------- Add object ----------*
     IF ls_tadir IS NOT INITIAL.
       MOVE-CORRESPONDING ls_tadir TO ls_object.
*---------- Set SAP Generated object status ----------*
       IF ls_tadir-genflag IS NOT INITIAL.
         ls_object-status = icon_generate.
       ENDIF.
*---------- Add object to be checked ----------*
       add_object( EXPORTING i_deep    = i_deep        "Deepness
                    CHANGING cs_object = ls_object ).  "Objects table
*---------- Error Object not valid ----------*
     ELSE.
       IF lines( t_objects ) > 0. "Skip first object
         ls_object-pgmid    = i_pgmid.
         ls_object-object   = i_object.
         ls_object-obj_name = i_obj_n.
         ls_object-status   = icon_led_red.
         add_object( EXPORTING i_deep    = i_deep        "Deepness
                      CHANGING cs_object = ls_object ).  "Objects table
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "check_add_object
*--- Private Method Add Object from Development Package Implementation *
   METHOD add_objects_of_devclass.
     DATA:
       ls_env_dummy  TYPE senvi,
       lt_objectlist TYPE TABLE OF rseui_set,  "Transfer table (object list) - info system
       ls_objectlist LIKE LINE OF lt_objectlist.
     DATA:
       lv_devclass TYPE devclass,  "Package
       lv_deep     TYPE i.         "Deepness
*---------- Get Development Package Objects ----------*
     lv_devclass = i_obj_n.
     CALL FUNCTION 'RS_GET_OBJECTS_OF_DEVCLASS' "#EC FB_RC  "#EC CI_SUBRC
       EXPORTING
         devclass            = lv_devclass
       TABLES
         objectlist          = lt_objectlist
       EXCEPTIONS
         no_objects_selected = 1
         OTHERS              = 2.
*---------- Add found objects  ----------*
     LOOP AT lt_objectlist INTO ls_objectlist.
       lv_deep = i_deep.
       IF ls_objectlist-obj_name <> lv_devclass.
         ADD 1 TO lv_deep.
       ENDIF.
       check_add_object( i_pgmid    = c_r3tr                 "Program ID in Requests and Tasks
                         i_object   = ls_objectlist-obj_type "Object Type
                         i_obj_n    = ls_objectlist-obj_name "Object Name in Object Directory
                         is_env_tab = ls_env_dummy           "Info system
                         i_deep     = lv_deep ).             "Deepness
     ENDLOOP.
   ENDMETHOD.                    "add_objects_of_devclass
*---------------------------- Private Method Add Object Implementation *
   METHOD add_object.
     DATA:
        ls_objs_desc LIKE LINE OF t_objs_desc, "Objects prograns ID line"Info Environment
        lt_devclass  TYPE scts_devclass,       "Development Packages
        ls_devclass  TYPE trdevclass.
     DATA:
       lv_object    TYPE trobjtype,  "Object Type
       lv_objname   TYPE sobj_name,  "Object Name in Object Directory
       lv_namespace TYPE namespace.  "Object Namespace
*---------- Check if already added ----------*
     READ TABLE t_objects TRANSPORTING NO FIELDS WITH KEY pgmid    = cs_object-pgmid
                                                          object   = cs_object-object
                                                          obj_name = cs_object-obj_name.
     IF sy-subrc IS NOT INITIAL. "New object
*---------- Check if is customer objects ----------*
       lv_object  = cs_object-object.
       lv_objname = cs_object-obj_name.
       CALL FUNCTION 'TRINT_GET_NAMESPACE'      "#EC FB_RC "#EC CI_SUBRC
         EXPORTING
           iv_pgmid            = cs_object-pgmid
           iv_object           = lv_object
           iv_obj_name         = lv_objname
         IMPORTING
           ev_namespace        = lv_namespace
         EXCEPTIONS
           invalid_prefix      = 1
           invalid_object_type = 2
           OTHERS              = 3.
       IF lv_namespace = '/0CUST/' OR v_st IS NOT INITIAL.  "Is customer object
*---------- Read object description ----------*
         READ TABLE t_objs_desc INTO ls_objs_desc WITH KEY object = cs_object-object.
         IF sy-subrc IS INITIAL.
           cs_object-obj_desc = ls_objs_desc-text.  "Object type description
         ENDIF.
*---------- Read development class tecnical information ----------*
         IF cs_object-devclass IS INITIAL.
           SELECT SINGLE devclass FROM tadir
             INTO cs_object-devclass
            WHERE pgmid    = cs_object-pgmid
              AND object   = cs_object-object
              AND obj_name = cs_object-obj_name.
         ENDIF.
         IF cs_object-devclass IS NOT INITIAL AND cs_object-devclass <> c_temp.
           ls_devclass-devclass = cs_object-devclass.
           APPEND ls_devclass TO lt_devclass.
           CALL FUNCTION 'TR_READ_DEVCLASSES'
             EXPORTING
               it_devclass = lt_devclass
             IMPORTING
               et_devclass = lt_devclass.
           READ TABLE lt_devclass INTO ls_devclass INDEX 1.
           IF sy-subrc IS INITIAL.
             cs_object-target = ls_devclass-target.  "Development package target
           ENDIF.
         ENDIF.
*---------- Add object deepness ----------*
         IF cs_object-deep IS INITIAL.
           cs_object-deep = i_deep.
         ENDIF.
*---------- Add object scope ----------*
         IF lv_namespace = '/0CUST/' AND cs_object-target IS NOT INITIAL.
           cs_object-scope = icon_transport.
         ENDIF.
*---------- Add object to transport ----------*
         APPEND cs_object TO t_objects.
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "add_object
*------------ Private Method Objects Dependencies Check Implementation *
   METHOD objects_dependencies_check.
     DATA:
       lt_env_tab  TYPE TABLE OF senvi,  "Object to check dependencies
       ls_env_tab  TYPE senvi.           "Info Environment
     DATA:
       lv_obj_type TYPE seu_obj,   "Object type
       lv_no_rfc   TYPE abap_bool, "RFC Flag
       lv_deep     TYPE i,         "Actual Deepness
       lv_tcode    TYPE tcode.     "Transaction Code
     FIELD-SYMBOLS <ls_object> LIKE LINE OF t_objects.  "Objects to transport
     LOOP AT t_objects ASSIGNING <ls_object> WHERE status IS INITIAL.
*---------- Check if Transaction exist ----------*
       IF <ls_object>-object = 'PROG'.
         CLEAR lv_tcode.
         SELECT tcode FROM tstc UP TO 1 ROWS "#EC CI_SEL_NESTED "#EC CI_GENBUFF
           INTO lv_tcode
          WHERE pgmna = <ls_object>-obj_name.
         ENDSELECT.
         IF sy-subrc IS INITIAL.
           check_add_object( i_pgmid    = c_r3tr         "Program ID in Requests and Tasks
                             i_object   = 'TRAN'         "Object Type
                             i_obj_n    = lv_tcode       "Object Name in Object Directory
                             is_env_tab = ls_env_tab     "Info system
                             i_deep     = lv_deep + 1 ). "Deepness
         ENDIF.
       ENDIF.
*---------- Check deepness ----------*
       lv_deep = <ls_object>-deep + 1. "Set Deepness
       IF v_deep IS NOT INITIAL AND lv_deep > v_deep.
         <ls_object>-status = icon_led_yellow.  "Status checked
         CONTINUE.
       ENDIF.
*---------- Exclude RFCs if exist ----------*
       IF v_erfc IS NOT INITIAL AND <ls_object>-object = c_fugr.
         CLEAR lv_no_rfc.
         exclude_rfcs( EXPORTING i_obj_name = <ls_object>-obj_name "Object Name in Object Directory
                        CHANGING c_no_rfc   = lv_no_rfc ).         "Found RFC Flag
         IF lv_no_rfc IS INITIAL.
           <ls_object>-status = icon_led_red.
           CONTINUE.
         ENDIF.
       ENDIF.
*---------- Get object dependecies ----------*
       REFRESH lt_env_tab.
       lv_obj_type = <ls_object>-object.
       CALL FUNCTION 'REPOSITORY_ENVIRONMENT_RFC'
         EXPORTING
           obj_type        = lv_obj_type
           object_name     = <ls_object>-obj_name
         TABLES
           environment_tab = lt_env_tab.
       IF lines( lt_env_tab ) IS INITIAL AND lines( t_objects ) = 1.
         MESSAGE s398(00) WITH 'Object dependencies check not support'(m13) space space space DISPLAY LIKE 'E'.
         <ls_object>-status = icon_led_red.
       ELSE.
         DELETE lt_env_tab WHERE type   = lv_obj_type
                             AND object = <ls_object>-obj_name.
*---------- Add founded dependecies ----------*
         LOOP AT lt_env_tab INTO ls_env_tab.              "#EC CI_NESTED
           CHECK ls_env_tab-type <> 'MESS'.
           CASE ls_env_tab-type.
             WHEN c_devc.  "Add from Development class
               add_objects_of_devclass( i_obj_n = ls_env_tab-object  "Object Name in Object Directory
                                        i_deep  = lv_deep ).         "Deepness
             WHEN OTHERS.  "Add all others object
               check_add_object( i_pgmid    = space              "Program ID in Requests and Tasks
                                 i_object   = ls_env_tab-type    "Object Type
                                 i_obj_n    = ls_env_tab-object  "Object Name in Object Directory
                                 is_env_tab = ls_env_tab         "Info system
                                 i_deep     = lv_deep ).         "Deepness
           ENDCASE.
         ENDLOOP.
         <ls_object>-status = icon_led_green.  "Status checked
       ENDIF.
     ENDLOOP.
   ENDMETHOD.                    "objects_dependencies_check
*------------------ Private Method Remote Objects Check Implementation *
   METHOD remote_objects_check.
     DATA:
       ls_e071 TYPE e071,          "Change & Transport System: Object Entries of Requests/Tasks
       lt_vrso TYPE TABLE OF vrso, "Version control: Object list (subset of VRSD)
       ls_vrso LIKE LINE OF lt_vrso.
     DATA:
       lv_diagnosis(20),                 "Version check result
       lv_local_rfc_dest  TYPE rfcdest,  "Logical Destinations
       lv_remote_rfc_dest TYPE rfcdest.
     FIELD-SYMBOLS <ls_object> LIKE LINE OF t_objects. "Objects to transport
*---------- Get local rfc destination ----------*
     SELECT desadm FROM tmscsys UP TO 1 ROWS             "#EC CI_NOFIRST
       INTO lv_local_rfc_dest
      WHERE sysnam = sy-sysid.
     ENDSELECT.
*---------- Get remote rfc destination ----------*
     SELECT desadm FROM tmscsys UP TO 1 ROWS             "#EC CI_NOFIRST
       INTO lv_remote_rfc_dest
      WHERE sysnam = v_rfc_d.
     ENDSELECT.
*---------- Check objects versions ----------*
     IF lv_local_rfc_dest IS NOT INITIAL AND lv_remote_rfc_dest IS NOT INITIAL.
       LOOP AT t_objects ASSIGNING <ls_object>.
*---------- Get Sub Objects ----------*
         ls_e071-object   = <ls_object>-object.
         ls_e071-obj_name = <ls_object>-obj_name.
         REFRESH lt_vrso.
         CALL FUNCTION 'TRINT_RESOLVE_OBJ'
           EXPORTING
             is_e071             = ls_e071
           TABLES
             et_vrso             = lt_vrso
           EXCEPTIONS
             not_versionable     = 1
             communication_error = 2
             OTHERS              = 3.
         IF sy-subrc IS INITIAL.
*---------- Remote check all objects and subobjects ----------*
           LOOP AT lt_vrso INTO ls_vrso WHERE objtype <> 'DOCU'. "#EC CI_NESTED
             CLEAR lv_diagnosis.
             CALL FUNCTION 'TRINT_COMP_VERSION'
               EXPORTING
                 is_vrso          = ls_vrso
                 dest1            = lv_local_rfc_dest
                 dest2            = lv_remote_rfc_dest
               IMPORTING
                 ev_diagnosis     = lv_diagnosis
               EXCEPTIONS
                 rfc_error_loc    = 1
                 rfc_error_rem    = 2
                 intern_error_loc = 3
                 intern_error_rem = 4
                 OTHERS           = 5.
             IF sy-subrc IS INITIAL.
               CASE lv_diagnosis.
                 WHEN 0.
                   <ls_object>-remote = 'Equal'(o01).
                 WHEN 1.
                   <ls_object>-remote = 'Different'(o02).
                   EXIT.
                 WHEN 3.
                   <ls_object>-remote = 'New'(o03).
                   EXIT.
               ENDCASE.
             ENDIF.
           ENDLOOP.
         ELSE.
           <ls_object>-remote = 'No version'(o04).
         ENDIF.
       ENDLOOP.
     ELSE.
       MESSAGE s398(00) WITH 'RFCs destinations missing'(m09) space space space DISPLAY LIKE 'W'.
     ENDIF.
   ENDMETHOD.                    "remote_objects_check
*-------------------------- Private Method Exclude RFCs Implementation *
   METHOD exclude_rfcs.
     DATA lt_fbinfo_remote TYPE TABLE OF fbinfor.  "Function Module Information
     DATA lv_complete_area TYPE rs38l_area.        "Function group, to which the function module belongs
*---------- Check if all are RFCs ----------*
     lv_complete_area = i_obj_name.
     CALL FUNCTION 'FUNCTION_SELECT_TFDIR'      "#EC FB_RC "#EC CI_SUBRC
       EXPORTING
         im_complete_area        = lv_complete_area
       IMPORTING
         ex_fbinfo_remote        = lt_fbinfo_remote
       EXCEPTIONS
         include_not_found_trdir = 1
         report_source_not_found = 2
         permission_failure      = 3
         OTHERS                  = 4.
     LOOP AT lt_fbinfo_remote TRANSPORTING NO FIELDS WHERE remote <> 'R'.
       c_no_rfc = abap_true. "One function found that are not RFCs
       EXIT.
     ENDLOOP.
   ENDMETHOD.                    "exclude_rfcs
*------------------ Private Method Include Maintenances Implementation *
   METHOD include_maintenances.
     DATA:
       ls_tvdir      TYPE tvdir,             "View Directory
       ls_object     LIKE LINE OF t_objects, "Objects to transport line
       ls_object_add LIKE LINE OF t_objects.
     DATA:
       lv_obj_type,                  "Object type
       lv_tobj_name  TYPE sobj_name, "Object Name in Object Directory
       lv_objectname TYPE ob_object. "Object Name
     LOOP AT t_objects INTO ls_object WHERE object = c_tabl AND status = icon_led_green.
       CLEAR ls_tvdir.
       SELECT SINGLE * FROM tvdir                     "#EC CI_SEL_NESTED
         INTO ls_tvdir
        WHERE tabname = ls_object-obj_name.
       IF sy-subrc IS INITIAL.
*---------- Add Function Group if exist ----------*
         ls_object_add-pgmid    = c_r3tr.
         ls_object_add-object   = c_fugr.
         ls_object_add-obj_name = ls_tvdir-area.
         add_object( EXPORTING i_deep    = ls_object-deep + 1  "Deepness
                      CHANGING cs_object = ls_object_add ).    "Objects table
         IF ls_tvdir-bastab IS INITIAL. lv_obj_type = 'V'. ELSE. lv_obj_type = 'S'. ENDIF.
*---------- Add Definition of a Maintenance and Transport Object ----------*
         CLEAR lv_tobj_name.
         lv_objectname = ls_object-obj_name.
         CALL FUNCTION 'CTO_OBJECT_GET_TADIR_KEY'
           EXPORTING
             iv_objectname = lv_objectname
             iv_objecttype = lv_obj_type
           IMPORTING
             ev_obj_name   = lv_tobj_name.
         ls_object_add-pgmid    = c_r3tr.
         ls_object_add-object   = c_tobj.
         ls_object_add-obj_name = lv_tobj_name.
         ls_object_add-status   = icon_led_green.
         add_object( EXPORTING i_deep    = ls_object-deep + 1  "Deepness
                      CHANGING cs_object = ls_object_add ).    "Objects table
       ENDIF.
     ENDLOOP.
   ENDMETHOD.                    "include_maintenances
*------------------------- Private Method Include Locks Implementation *
   METHOD include_locks.
     DATA:
       ls_object     LIKE LINE OF t_objects,  "Objects to transport line
       ls_object_add LIKE LINE OF t_objects.
     DATA lv_viewname TYPE viewname. "Maintenance view name
*---------- Add lock objects if exist ----------*
     LOOP AT t_objects INTO ls_object WHERE object = c_tabl AND status = icon_led_green.
       CLEAR lv_viewname.
       SELECT viewname FROM dd25l UP TO 1 ROWS "#EC CI_SEL_NESTED "#EC CI_NOFIRST
         INTO lv_viewname
        WHERE aggtype = 'E'
          AND roottab = ls_object-obj_name.
       ENDSELECT.
       IF sy-subrc IS INITIAL.
         ls_object_add-pgmid    = c_r3tr.
         ls_object_add-object   = 'ENQU'.
         ls_object_add-obj_name = lv_viewname.
         ls_object_add-status   = icon_led_green.
         add_object( EXPORTING i_deep    = ls_object-deep + 1  "Deepness
                      CHANGING cs_object = ls_object_add ).    "Objects table
       ENDIF.
     ENDLOOP.
   ENDMETHOD.                    "include_locks
*--------------------------- Private Method Include CDs Implementation *
   METHOD include_cds.
     DATA:
       ls_object     LIKE LINE OF t_objects,  "Objects to transport line
       ls_object_add LIKE LINE OF t_objects.
     DATA lv_object TYPE cdobjectcl. "Object class
*---------- Add change document object if exist ----------*
     LOOP AT t_objects INTO ls_object WHERE object = c_tabl AND status = icon_led_green.
       CLEAR lv_object.
       SELECT object FROM tcdob UP TO 1 ROWS "#EC CI_SEL_NESTED "#EC CI_GENBUFF
         INTO lv_object
        WHERE tabname = ls_object-obj_name.
       ENDSELECT.
       IF sy-subrc IS INITIAL.
         ls_object_add-pgmid    = c_r3tr.
         ls_object_add-object   = c_chdo.
         ls_object_add-obj_name = lv_object.
         ls_object_add-status   = icon_led_green.
         add_object( EXPORTING i_deep = ls_object-deep + 1   "Deepness
                      CHANGING cs_object = ls_object_add ).  "Objects table
       ENDIF.
     ENDLOOP.
   ENDMETHOD.                    "include_cds
*---------------------- Private Method Add TOBJ Content Implementation *
   METHOD add_tobj_content.
     CONSTANTS:
       lc_tabu  TYPE trobjtype VALUE 'TABU',
       lc_tvdir TYPE sobj_name VALUE 'TVDIR',
       lc_tddat TYPE sobj_name VALUE 'TDDAT',
       lc_tvimf TYPE sobj_name VALUE 'TVIMF'.
     DATA:
       lt_tvimf TYPE TABLE OF tvimf,   "User routines called from view maintenance
       ls_tvimf LIKE LINE OF lt_tvimf.
     DATA lv_tabkey TYPE tabkey.  "Table Key
*---------- Add table content ----------*
     add_object_header( i_pgmid   = c_r3tr   "Program ID in Requests and Tasks
                        i_object  = lc_tabu  "Object Type
                        i_obj_n   = lc_tvdir "Object Name in Object Directory
                        i_objfunc = 'K' ).   "Object function
     add_object_keys( i_pgmid  = c_r3tr        "Program ID in Requests and Tasks
                      i_object = lc_tabu       "Object Type
                      i_obj_n  = lc_tvdir      "Object Name in Object Directory
                      i_tabkey = i_obj_name ). "Table Key
     CLEAR lt_tvimf. "Read User routines called from view maintenance
     SELECT * FROM tvimf                                 "#EC CI_GENBUFF
       INTO TABLE lt_tvimf
      WHERE tabname = i_obj_name.
     LOOP AT lt_tvimf INTO ls_tvimf.
       AT FIRST.
         add_object_header( i_pgmid  = c_r3tr    "Program ID in Requests and Tasks
                            i_object  = lc_tabu  "Object Type
                            i_obj_n   = lc_tvimf "Object Name in Object Directory
                            i_objfunc = 'K' ).   "Object function
       ENDAT.
       lv_tabkey    = i_obj_name.
       lv_tabkey+30 = ls_tvimf-event.
       add_object_keys( i_pgmid  = c_r3tr        "Program ID in Requests and Tasks
                        i_object = lc_tabu       "Object Type
                        i_obj_n  = lc_tvimf      "Object Name in Object Directory
                        i_tabkey = lv_tabkey ).  "Table Key
       CLEAR lv_tabkey.
     ENDLOOP.
     add_object_header( i_pgmid   = c_r3tr   "Program ID in Requests and Tasks
                        i_object  = lc_tabu  "Object Type
                        i_obj_n   = lc_tddat "Object Name in Object Directory
                        i_objfunc = 'K' ).   "Object function
     add_object_keys( i_pgmid  = c_r3tr        "Program ID in Requests and Tasks
                      i_object = lc_tabu       "Object Type
                      i_obj_n  = lc_tddat      "Object Name in Object Directory
                      i_tabkey = i_obj_name ). "Table Key
   ENDMETHOD.                    "add_tobj_content
*--------------------- Private Method Add Object Header Implementation *
   METHOD add_object_header.
     DATA ls_e071 LIKE LINE OF t_e071.
     READ TABLE t_e071 TRANSPORTING NO FIELDS WITH KEY pgmid    = i_pgmid
                                                       object   = i_object
                                                       obj_name = i_obj_n
                                                       objfunc  = i_objfunc.
     IF sy-subrc IS NOT INITIAL.
       ls_e071-pgmid    = i_pgmid.
       ls_e071-object   = i_object.
       ls_e071-obj_name = i_obj_n.
       ls_e071-objfunc  = i_objfunc.
       APPEND ls_e071 TO t_e071.
     ENDIF.
   ENDMETHOD.                    "add_object_header
*----------------------- Private Method Add Object Keys Implementation *
   METHOD add_object_keys.
     DATA ls_e071k LIKE LINE OF t_e071k.
     ls_e071k-pgmid      = i_pgmid.
     ls_e071k-object     = i_object.
     ls_e071k-objname    = i_obj_n.
     ls_e071k-mastertype = i_object.
     ls_e071k-mastername = i_obj_n.
     ls_e071k-tabkey     = i_tabkey.
     APPEND ls_e071k TO t_e071k.
   ENDMETHOD.                    "add_object_keys
ENDCLASS.                    "obj_dep_trans IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Include           ZTEKB_F4_001
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS cl_f4_utilities DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

TYPES :
  BEGIN OF ty_domain_fix_val,
    key  LIKE ddfixvalue-low,
    text LIKE ddfixvalue-ddtext,
  END OF ty_domain_fix_val,
  ty_t_domain_fix_val TYPE STANDARD TABLE OF ty_domain_fix_val,

  BEGIN OF ty_f4_ret,
    fieldvar(132),
  END OF ty_f4_ret,
  ty_t_f4_ret     TYPE STANDARD TABLE OF ty_f4_ret,

  ty_t_ddshretval TYPE STANDARD TABLE OF ddshretval.

*----------------------------------------------------------------------*
*       CLASS cl_f4_utilities DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_f4_utilities DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS :
      f4_help_with_internal_table
        IMPORTING
          ip_dynpprog       TYPE c DEFAULT sy-repid "prog name
          ip_dynpnr         TYPE c DEFAULT '1000'   "screen number
          ip_dynpfield      TYPE c OPTIONAL "input field name
          ip_dynpfield_text TYPE c OPTIONAL "input -text- field name

          ip_retfieldname   TYPE c "return (key) fieldname
          " in F4 help popup table
          ip_textfieldname  TYPE c OPTIONAL "text fieldname
          " in F4 help popup table

          ip_multi_sel      TYPE c OPTIONAL
          " checkbox field in internal table ..
          " for multiple line choice

        EXPORTING
          ep_key_val        TYPE any "selected key value
          ep_text_val       TYPE any  "selected text value

          ep_t_multi_ret    TYPE ty_t_f4_ret

        CHANGING
          cp_data_table     TYPE STANDARD TABLE, "value hits table

      f4_help_for_table_field
        IMPORTING
          ip_tabname   TYPE c  OPTIONAL
          ip_fieldname TYPE c OPTIONAL
          ip_sh_name   TYPE c OPTIONAL
        EXPORTING
          ep_key_val   TYPE any ,"selected key value


      get_sh_records
        IMPORTING
          ip_tabname      TYPE c OPTIONAL
          ip_fieldname    TYPE c OPTIONAL
          ip_sh_name      TYPE c OPTIONAL
        EXPORTING
          ep_t_f4_records TYPE ty_t_ddshretval
          ep_s_f4_desc    TYPE shlp_descr,


      f4_help_with_retval
        IMPORTING
          ip_t_f4_records   TYPE ty_t_ddshretval
          ip_s_f4_desc      TYPE shlp_descr

          ip_dynpprog       TYPE c DEFAULT sy-repid "prog name
          ip_dynpnr         TYPE c DEFAULT '1000'   "screen number
          ip_dynpfield      TYPE c OPTIONAL "input field name
          ip_dynpfield_text TYPE c OPTIONAL "input -text- field name

          ip_retfieldname   TYPE c OPTIONAL "return (key) fieldname
          " in F4 help popup table
          ip_textfieldname  TYPE c OPTIONAL "text fieldname
          " in F4 help popup table

          ip_multi_sel      TYPE c OPTIONAL
          " checkbox field in internal table ..
          " for multiple line choice

        EXPORTING
          ep_key_val        TYPE any "selected key value
          ep_text_val       TYPE any  "selected text value

          ep_t_multi_ret    TYPE ty_t_f4_ret,



      get_parameter_dynp_value
        IMPORTING
          ip_dynpprog  TYPE c DEFAULT sy-repid "prog name
          ip_dynpnr    TYPE c DEFAULT '1000'   "screen number
          ip_dynpfield TYPE c OPTIONAL "input field name
        EXPORTING
          ep_value     TYPE any,

      read_domain_values
        IMPORTING
          ip_table     TYPE c
          ip_field     TYPE c
        EXPORTING
          ep_t_val_tab TYPE ty_t_domain_fix_val,

      run_f4_with_params
        IMPORTING
          ip_t_field_ranges TYPE ddshselops OPTIONAL
          ip_f4help_name    TYPE c
          ip_ret_fieldname  TYPE c

        EXPORTING
          ep_value          TYPE any.


ENDCLASS.                    "cl_f4_utilities DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_f4_utilities IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_f4_utilities IMPLEMENTATION.

  METHOD run_f4_with_params.

    DATA: lv_shelp_name    TYPE shlpname,
          ls_shelp         TYPE shlp_descr,
          ls_selopt        TYPE ddshselopt,
          ls_rc            LIKE sy-subrc,
          lt_return_values TYPE TABLE OF ddshretval,
          ls_return_values TYPE ddshretval,
          ls_interface     LIKE LINE OF ls_shelp-interface,
          lw_field_ranges  LIKE LINE OF ip_t_field_ranges.

    lv_shelp_name = ip_f4help_name.

* get description for search help
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = lv_shelp_name
      IMPORTING
        shlp     = ls_shelp.

* set parameter field value :

    LOOP AT ip_t_field_ranges INTO lw_field_ranges.

      ls_selopt-shlpname = lv_shelp_name.
      ls_selopt-shlpfield = lw_field_ranges-shlpfield.
      ls_selopt-sign      = lw_field_ranges-sign.
      ls_selopt-option    = lw_field_ranges-option.
      ls_selopt-low       = lw_field_ranges-low.
      COLLECT ls_selopt INTO ls_shelp-selopt.

    ENDLOOP.

    ls_interface-valfield = 'X'.
    ls_interface-valtabname = 'X'.

    MODIFY ls_shelp-interface FROM ls_interface
           TRANSPORTING valtabname valfield
           WHERE shlpfield = ip_ret_fieldname.

* call F4 dialog
    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shelp
      IMPORTING
        rc            = ls_rc
      TABLES
        return_values = lt_return_values.

    READ TABLE  lt_return_values INTO ls_return_values INDEX 1.
    CHECK sy-subrc EQ 0.
    ep_value = ls_return_values-fieldval.

  ENDMETHOD.                    "run_f4_with_params


  METHOD read_domain_values.

    DATA :
      tabname         TYPE  ddobjname,
      fieldname       TYPE  dfies-fieldname,
      lfieldname      TYPE  dfies-lfieldname,
      lv_dfies        TYPE dfies,

      lt_fixed_values TYPE ddfixvalues,
      lw_fixed_values LIKE LINE OF lt_fixed_values,

      lw_t_ep         LIKE LINE OF ep_t_val_tab.


    CLEAR ep_t_val_tab.

    tabname = ip_table.
    fieldname = ip_field.
    lfieldname   = ip_field.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname      = tabname
        fieldname    = fieldname
        langu        = sy-langu
        lfieldname   = lfieldname
      IMPORTING
        dfies_wa     = lv_dfies
      TABLES
        fixed_values = lt_fixed_values.

    LOOP AT lt_fixed_values INTO lw_fixed_values.

      lw_t_ep-key =  lw_fixed_values-low.
      lw_t_ep-text =  lw_fixed_values-ddtext.
      APPEND lw_t_ep TO ep_t_val_tab.

    ENDLOOP.

  ENDMETHOD.                    "read_domain_values

  METHOD f4_help_with_internal_table.

    DATA : lv_dynpprog          TYPE sy-repid,
           lv_dynpnr            TYPE sy-dynnr,
           lv_dynprofield       TYPE help_info-dynprofld,
           lv_retfieldname      TYPE  dfies-fieldname,

           lv_textfieldname(30),


           lt_return_tab        TYPE STANDARD TABLE OF ddshretval,
           lw_return_tab        LIKE LINE OF lt_return_tab,

           t_dynfields          TYPE TABLE OF dynpread,
           w_dynfields          LIKE LINE OF t_dynfields.


    FIELD-SYMBOLS: <w_table>     TYPE any,
                   <f_textfield> TYPE any,
                   <f_keyfield>  TYPE any,
                   <f_scr_field> TYPE any,
                   <lv_cb_field> TYPE any.


    DATA l_ref_field_copy TYPE REF TO data.

*--1-- Translations :

    lv_dynpprog     = ip_dynpprog.
    lv_dynpnr       = ip_dynpnr.
    lv_dynprofield  = ip_dynpfield.
    lv_retfieldname = ip_retfieldname.

    TRANSLATE :
    lv_dynpprog TO UPPER CASE,
    lv_dynpnr TO UPPER CASE,
    lv_dynprofield TO UPPER CASE,
    lv_retfieldname TO UPPER CASE.

*--2-- Display f4 help popup screen :

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*       DDIC_STRUCTURE  = ' '
        retfield        = lv_retfieldname
*       PVALKEY         = ' '
        dynpprog        = lv_dynpprog
        dynpnr          = lv_dynpnr
        dynprofield     = lv_dynprofield
*       STEPL           = 0
*       WINDOW_TITLE    =
*       value           = <f_field>
        value_org       = 'S'
        multiple_choice = ip_multi_sel
*       DISPLAY         = ' '
*       CALLBACK_PROGRAM       = ' '
*       CALLBACK_FORM   = ' '
      TABLES
        value_tab       = cp_data_table
*       FIELD_TAB       =
        return_tab      = lt_return_tab
*       DYNPFLD_MAPPING =
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


*--3-- Find selected line and text if requested :
*      ( ip_dynpfield_text was supplied than it will be set auto..)


    IF ep_key_val IS REQUESTED
    OR ep_text_val IS REQUESTED
    OR ip_dynpfield_text IS SUPPLIED.

      " -- Get selected value :
      CLEAR lw_return_tab.
      READ TABLE lt_return_tab INTO lw_return_tab INDEX 1.
      ep_key_val = lw_return_tab-fieldval.

      " -- Set screen field value :

      ASSIGN (ip_dynpfield) TO <f_scr_field>.
      IF sy-subrc EQ 0.
        <f_scr_field> = lw_return_tab-fieldval.
      ENDIF.

      lv_textfieldname  = ip_textfieldname.
      TRANSLATE  lv_textfieldname TO UPPER CASE.

      " -- Search this values in displayed value table :

      LOOP AT cp_data_table ASSIGNING <w_table>.

        ASSIGN COMPONENT lv_retfieldname OF STRUCTURE <w_table>
        TO <f_keyfield>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <f_keyfield>
          IMPORTING
            output = <f_keyfield>.

        IF <f_keyfield> EQ lw_return_tab-fieldval.

          ASSIGN COMPONENT lv_textfieldname OF STRUCTURE <w_table>
          TO <f_textfield>.
          IF sy-subrc EQ 0.
            ep_text_val = <f_textfield>.
            EXIT.
          ENDIF.

        ENDIF.

      ENDLOOP.

      " Set text feield value if supplied :
      IF ip_dynpfield_text IS SUPPLIED.

        IF NOT ep_text_val IS INITIAL.

          w_dynfields-fieldname = ip_dynpfield_text.
          w_dynfields-fieldvalue = ep_text_val.
          APPEND w_dynfields TO t_dynfields.

          CALL FUNCTION 'DYNP_VALUES_UPDATE'
            EXPORTING
              dyname     = lv_dynpprog
              dynumb     = lv_dynpnr
            TABLES
              dynpfields = t_dynfields
            EXCEPTIONS
              OTHERS     = 8.

        ENDIF.

      ENDIF.

    ELSEIF ip_multi_sel EQ 'X'.
      " set selected lines in table

      LOOP AT lt_return_tab INTO lw_return_tab.

        APPEND lw_return_tab-fieldval TO ep_t_multi_ret.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "cl_f4_utilities

  METHOD get_sh_records.

    " Returns search help records belong to a Table/Field
    " or Search help
    " ( RFC search help )

    DATA :
      lv_tabname    TYPE dfies-tabname,
      lv_fieldname  TYPE dfies-fieldname,
      lv_searchhelp TYPE shlpname.

    REFRESH ep_t_f4_records.
    CLEAR ep_s_f4_desc.

    lv_tabname    = ip_tabname.
    lv_fieldname  = ip_fieldname.
    lv_searchhelp = ip_sh_name.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname             = lv_tabname
        fieldname           = lv_fieldname
        searchhelp          = lv_searchhelp
        suppress_recordlist = 'X'
      TABLES
        return_tab          = ep_t_f4_records
      EXCEPTIONS
        field_not_found     = 1
        no_help_for_field   = 2
        inconsistent_help   = 3
        no_values_found     = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF NOT lv_searchhelp IS INITIAL.

      CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
        EXPORTING
          shlpname = lv_searchhelp
*         SHLPTYPE = 'SH'
        IMPORTING
          shlp     = ep_s_f4_desc.

    ELSE.

      CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
        EXPORTING
          tabname           = lv_tabname
          fieldname         = lv_fieldname
*         SELECTION_SCREEN  = ' '
        IMPORTING
          shlp              = ep_s_f4_desc
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.
    ENDIF.


  ENDMETHOD.

  METHOD f4_help_with_retval.

    DATA :
      lt_fcat   TYPE lvc_t_fcat,
      ls_fcat   TYPE lvc_s_fcat,
      lv_colpos TYPE i VALUE 0.

    DATA ls_field_desc LIKE LINE OF ip_s_f4_desc-fielddescr.

    DATA lr_table TYPE REF TO data.
    DATA lr_wa TYPE REF TO data.
    FIELD-SYMBOLS <lt_hits> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_hits> TYPE any.
    FIELD-SYMBOLS <lv_field> TYPE any.

    DATA ls_interface LIKE LINE OF ip_s_f4_desc-interface.
    DATA ls_f4_records LIKE LINE OF ip_t_f4_records.
    DATA ls_f4_records_prev LIKE LINE OF ip_t_f4_records.

    " Field Cat for the dynamic f4hep internal table :

    LOOP AT ip_s_f4_desc-fielddescr INTO ls_field_desc.

      ADD 1 TO lv_colpos.
      CLEAR ls_fcat.
      ls_fcat-col_pos    = lv_colpos.
      ls_fcat-fieldname  = ls_field_desc-fieldname.

      IF NOT ls_field_desc-tabname IS INITIAL.
        ls_fcat-ref_table  = ls_field_desc-tabname.
      ELSE.

        CLEAR ls_interface.
        READ TABLE ip_s_f4_desc-interface INTO ls_interface
        WITH KEY shlpfield = ls_field_desc-fieldname.
        IF sy-subrc EQ 0.
          ls_fcat-ref_table  = ls_interface-valtabname.
        ELSE.

          SELECT SINGLE
            tabname
            fieldname
          FROM dd03l
          INTO (ls_fcat-ref_table,
                ls_fcat-ref_field)
          WHERE rollname  EQ ls_field_desc-rollname
            AND fieldname EQ ls_field_desc-rollname.

          ls_fcat-fieldname = ls_fcat-ref_field.

          IF sy-subrc NE 0.

            ls_fcat-ref_table = 'DD03P'.

            ls_fcat-fieldname =
            ls_fcat-ref_field = 'DDTEXT'.

          ENDIF.

        ENDIF.

      ENDIF.

      ls_fcat-ref_field  = ls_field_desc-fieldname.

      ls_fcat-rollname    = ls_field_desc-rollname.
      ls_fcat-domname     = ls_field_desc-domname.

      ls_fcat-datatype    = ls_field_desc-datatype.
      ls_fcat-inttype     = ls_field_desc-inttype.
      ls_fcat-intlen      = ls_field_desc-intlen.

      ls_fcat-coltext     = ls_field_desc-fieldtext.
      ls_fcat-seltext     = ls_field_desc-fieldtext.
      ls_fcat-reptext     = ls_field_desc-reptext.
      ls_fcat-scrtext_s   = ls_field_desc-scrtext_s.
      ls_fcat-scrtext_m   = ls_field_desc-scrtext_m.
      ls_fcat-scrtext_l   = ls_field_desc-scrtext_l.
      ls_fcat-key         = ls_field_desc-keyflag.

      APPEND ls_fcat TO lt_fcat.

    ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_fcat
      IMPORTING
        ep_table        = lr_table.

    ASSIGN lr_table->* TO <lt_hits>.

    CREATE DATA lr_wa LIKE LINE OF <lt_hits>.

    ASSIGN lr_wa->* TO <ls_hits>.

    " Fill the dynamic table :

    LOOP AT ip_t_f4_records INTO ls_f4_records.

      IF ls_f4_records-recordpos NE
         ls_f4_records_prev-recordpos.

        IF NOT <ls_hits> IS INITIAL.
          APPEND <ls_hits> TO <lt_hits>.
          CLEAR <ls_hits>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT ls_f4_records-fieldname
      OF STRUCTURE <ls_hits>
      TO <lv_field>.

      <lv_field> = ls_f4_records-fieldval.

      ls_f4_records_prev = ls_f4_records.

    ENDLOOP.

    APPEND <ls_hits> TO <lt_hits>.

    " Disp. table with f4 help :

    READ TABLE ip_s_f4_desc-interface
    INTO ls_interface
    WITH KEY f4field = 'X'.

    CALL METHOD f4_help_with_internal_table
      EXPORTING
        ip_dynpprog       = ip_dynpprog
        ip_dynpnr         = ip_dynpnr
        ip_dynpfield      = ip_dynpfield
        ip_dynpfield_text = ip_dynpfield_text
        ip_retfieldname   = ls_interface-shlpfield
        ip_textfieldname  = ip_textfieldname
        ip_multi_sel      = ip_multi_sel
      IMPORTING
        ep_key_val        = ep_key_val
        ep_text_val       = ep_text_val
        ep_t_multi_ret    = ep_t_multi_ret
      CHANGING
        cp_data_table     = <lt_hits>.

  ENDMETHOD.

  METHOD f4_help_for_table_field.

    DATA :
      lv_tabname    TYPE dfies-tabname,
      lv_fieldname  TYPE dfies-fieldname,
      lv_searchhelp TYPE shlpname,
      lt_return_tab TYPE STANDARD TABLE OF ddshretval,
      ls_return_tab LIKE LINE OF lt_return_tab.

    lv_tabname    = ip_tabname.
    lv_fieldname  = ip_fieldname.
    lv_searchhelp = ip_sh_name.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = lv_tabname
        fieldname         = lv_fieldname
        searchhelp        = lv_searchhelp
*       SHLPPARAM         = ' '
*       DYNPPROG          = ' '
*       DYNPNR            = ' '
*       DYNPROFIELD       = ' '
*       STEPL             = 0
*       VALUE             = ' '
*       MULTIPLE_CHOICE   = ' '
*       DISPLAY           = ' '
*       SUPPRESS_RECORDLIST       = ' '
*       CALLBACK_PROGRAM  = ' '
*       CALLBACK_FORM     = ' '
*       SELECTION_SCREEN  = ' '
*     IMPORTING
*       USER_RESET        = USER_RESET
      TABLES
        return_tab        = lt_return_tab
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.

    CHECK sy-subrc EQ 0.
    ep_key_val = ls_return_tab-fieldval.

  ENDMETHOD.                    "f4_help_for_table_field

  METHOD get_parameter_dynp_value.
*    IMPORTING
*      ip_dynpprog TYPE c DEFAULT sy-repid "prog name
*      ip_dynpnr TYPE c DEFAULT '1000'   "screen number
*      ip_dynpfield TYPE c OPTIONAL "input field name
*    EXPORTING
*      ep_value TYPE any.

    DATA :
      dyname       TYPE  d020s-prog,
      dynumb       TYPE  d020s-dnum,
      t_dynpfields TYPE STANDARD TABLE OF dynpread,
      w_dynpfields LIKE LINE OF t_dynpfields,
      dynprofield  TYPE  help_info-dynprofld.


    dyname = ip_dynpprog.
    dynumb = ip_dynpnr.

    w_dynpfields-fieldname = ip_dynpfield.
    APPEND w_dynpfields TO t_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = dyname
        dynumb               = dynumb
      TABLES
        dynpfields           = t_dynpfields[]
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR w_dynpfields.
    READ TABLE t_dynpfields INTO w_dynpfields INDEX 1.

    ep_value = w_dynpfields-fieldvalue.


  ENDMETHOD.                    "get_parameter_dynp_value



ENDCLASS.                    "cl_f4_utilities IMPLEMENTATION
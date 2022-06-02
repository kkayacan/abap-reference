class ZCL_FW01_BDC definition
  public
  create public .

public section.
*"* public components of class ZCL_FW01_BDC
*"* do not include other source files here!!!

    METHODS:

    open_group
      IMPORTING
        p_group    TYPE apqi-groupid
        p_user     TYPE apqi-userid
        p_keep     TYPE apqi-qerase
        p_holddate TYPE apqi-startdate
        p_ctu      TYPE apqi-putactive,

    close_group
      IMPORTING
        p_ctu TYPE apqi-putactive,

    bdc_transaction
      IMPORTING
        p_tcode  TYPE simple
        p_ctu    TYPE simple
        p_mode   TYPE simple
        p_update TYPE simple
      RETURNING
        value(et_msg) TYPE tab_bdcmsgcoll,

    bdc_dynpro
      IMPORTING
        program TYPE simple
        dynpro  TYPE simple,

    bdc_field
      IMPORTING
        fnam TYPE simple
        fval TYPE simple,

    bdc_nodata
      IMPORTING
        p_nodata TYPE simple.

protected section.
*"* protected components of class ZCL_FW01_BDC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_FW01_BDC
*"* do not include other source files here!!!

    DATA: bdcdata TYPE TABLE OF bdcdata,
          messtab TYPE TABLE OF bdcmsgcoll,
          nodata_character TYPE c LENGTH 1 VALUE '/'.

ENDCLASS.



CLASS ZCL_FW01_BDC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->BDC_DYNPRO
* +-------------------------------------------------------------------------------------------------+
* | [--->] PROGRAM                        TYPE        SIMPLE
* | [--->] DYNPRO                         TYPE        SIMPLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bdc_dynpro.
    DATA ls_bdcdata LIKE LINE OF me->bdcdata.
    ls_bdcdata-program  = program.
    ls_bdcdata-dynpro   = dynpro.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO me->bdcdata.
  ENDMETHOD.                    "BDC_DYNPRO


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->BDC_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] FNAM                           TYPE        SIMPLE
* | [--->] FVAL                           TYPE        SIMPLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bdc_field.
    DATA ls_bdcdata LIKE LINE OF me->bdcdata.
    IF fval <> me->nodata_character.
      ls_bdcdata-fnam = fnam.
      ls_bdcdata-fval = fval.
      APPEND ls_bdcdata TO me->bdcdata.
    ENDIF.
  ENDMETHOD.                    "BDC_FIELD


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->BDC_NODATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_NODATA                       TYPE        SIMPLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bdc_nodata.
    me->nodata_character = p_nodata.
  ENDMETHOD.                    "BDC_NODATA


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->BDC_TRANSACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_TCODE                        TYPE        SIMPLE
* | [--->] P_CTU                          TYPE        SIMPLE
* | [--->] P_MODE                         TYPE        SIMPLE
* | [--->] P_UPDATE                       TYPE        SIMPLE
* | [<-()] ET_MSG                         TYPE        TAB_BDCMSGCOLL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bdc_transaction.

    DATA: l_subrc LIKE sy-subrc.

    DATA:   gs_opt     TYPE ctu_params.
*    gs_opt-nobinpt = 'X'.
*    gs_opt-dismode = 'N'.
*    gs_opt-updmode = 'A'.
*    gs_opt-RACOMMIT = 'X'.

    IF p_ctu <> 'X'.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode     = p_tcode
        TABLES
          dynprotab = me->bdcdata
        EXCEPTIONS
          OTHERS    = 1.
    ELSE.
      CALL TRANSACTION p_tcode USING me->bdcdata
                       MODE   p_mode
                       UPDATE p_update
*                       OPTIONS FROM gs_opt
                       MESSAGES INTO me->messtab.
    ENDIF.
    l_subrc = sy-subrc.
    CLEAR me->bdcdata.
    sy-subrc = l_subrc.

    et_msg = me->messtab.

  ENDMETHOD.                    "BDC_TRANSACTION


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->CLOSE_GROUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_CTU                          TYPE        APQI-PUTACTIVE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_group.

    IF p_ctu <> 'X'.
* close batchinput group
      CALL FUNCTION 'BDC_CLOSE_GROUP'.
    ENDIF.

  ENDMETHOD.                    "close_group


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FW01_BDC->OPEN_GROUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_GROUP                        TYPE        APQI-GROUPID
* | [--->] P_USER                         TYPE        APQI-USERID
* | [--->] P_KEEP                         TYPE        APQI-QERASE
* | [--->] P_HOLDDATE                     TYPE        APQI-STARTDATE
* | [--->] P_CTU                          TYPE        APQI-PUTACTIVE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD open_group.

    IF p_ctu <> 'X'.
      CALL FUNCTION 'BDC_OPEN_GROUP'
        EXPORTING
          client   = sy-mandt
          group    = p_group
          user     = p_user
          keep     = p_keep
          holddate = p_holddate.
    ENDIF.

  ENDMETHOD.                    "open_group
ENDCLASS.
FUNCTION ZFM_FORM_PDF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_FORM_NAME) TYPE  TDSFNAME
*"  EXPORTING
*"     REFERENCE(EX_SIZE) TYPE  SOOD-OBJLEN
*"     REFERENCE(EX_CONTENT) TYPE  SOLI_TAB
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Data declarations
*----------------------------------------------------------------------*
  DATA : LV_NAME TYPE RS38L_FNAM,                     " Variable to get the name of the function module
         LV_SIZE TYPE I,                              " Integer variable to get the size of the form
         WA_OUT_INFO TYPE SSFCRESCL,                  " Work area to get the output information of the smart form
         WA_OUTPUT_OPT TYPE SSFCTRLOP,                " Work area to set the output control option of smart form
         WA_OUT TYPE SSFCOMPOP,                       " Work area to set the output options
         IT_LINES TYPE STANDARD TABLE OF TLINE,       " Internal table to get the binary content of the smart form
         IT_OTF TYPE STANDARD TABLE OF ITCOO.         " Internal table to hold the otf format of smart form


 

*----------------------------------------------------------------------*
* Calling the function module to get the function module generated for the smart form.
*----------------------------------------------------------------------*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = IM_FORM_NAME
    IMPORTING
      FM_NAME            = LV_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.



*----------------------------------------------------------------------*
* Calling the function module of the smart form and getting the content of the form.
*----------------------------------------------------------------------*

  WA_OUTPUT_OPT-GETOTF = 'X'.
  WA_OUTPUT_OPT-DEVICE = 'PRINTER'.
  WA_OUTPUT_OPT-PREVIEW = ''.
  WA_OUTPUT_OPT-NO_DIALOG = 'X'.
  WA_OUT-TDDEST = 'LOCL'.

* Note: Function module is to be replaced with the function module of the calling smart form
* Data to be passed to the function module is to be added as the importing parameter

  CALL FUNCTION LV_NAME
    EXPORTING
      CONTROL_PARAMETERS = WA_OUTPUT_OPT
      OUTPUT_OPTIONS     = WA_OUT
      USER_SETTINGS      = ' '
    IMPORTING
      JOB_OUTPUT_INFO    = WA_OUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.


*----------------------------------------------------------------------*
* Calling the function module to convert the otf data of smart form into binary contents
*----------------------------------------------------------------------*

  IT_OTF = WA_OUT_INFO-OTFDATA.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
      MAX_LINEWIDTH         = 132
    IMPORTING
      BIN_FILESIZE          = LV_SIZE
    TABLES
      OTF                   = IT_OTF
      LINES                 = IT_LINES
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      ERR_BAD_OTF           = 4
      OTHERS                = 5.


*----------------------------------------------------------------------*
* Calling the function module to convert the binary data of smart form into format attached to the mail
*----------------------------------------------------------------------*

  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    EXPORTING
      LINE_WIDTH_DST              = 255
    TABLES
      CONTENT_IN                  = IT_LINES
      CONTENT_OUT                 = EX_CONTENT
    EXCEPTIONS
      ERR_LINE_WIDTH_SRC_TOO_LONG = 1
      ERR_LINE_WIDTH_DST_TOO_LONG = 2
      ERR_CONV_FAILED             = 3
      OTHERS                      = 4.


 


*----------------------------------------------------------------------*
* Exporting the contents back to the calling program
*----------------------------------------------------------------------*

  EX_SIZE = LV_SIZE.

ENDFUNCTION.
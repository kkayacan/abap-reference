class ZCL_STRING definition
  public
  create public .

public section.

  class-methods WRAP
    importing
      !TEXTLINE type C
      !DELIMITER type C default SPACE
      !OUTPUTLEN type I default 50
    exporting
      !OUT_LINE1 type C
      !OUT_LINE2 type C
      !OUT_LINE3 type C
      !OUT_LINE4 type C
      !OUT_LINES type TABLE
    exceptions
      OUTPUTLEN_TOO_LARGE .
  class-methods TO_BAPIRET2
    importing
      !IV_TEXT type STRING
      !IV_TYPE type MSGTY default 'E'
    returning
      value(RS_BAPIRET2) type BAPIRET2 .
protected section.
private section.

  class-methods SPLIT_COMPLEX
    importing
      !ID_TEXT type C
      !ID_LEN type I
      !ID_DEL type C
    changing
      !CT_LINES type TABLE optional .
ENDCLASS.



CLASS ZCL_STRING IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_STRING=>SPLIT_COMPLEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] ID_TEXT                        TYPE        C
* | [--->] ID_LEN                         TYPE        I
* | [--->] ID_DEL                         TYPE        C
* | [<-->] CT_LINES                       TYPE        TABLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SPLIT_COMPLEX.

  data: ld_len         type i,
        ld_pos         type i,
        ld_strlen      type i,
        lr_string      type ref to data.
*        ld_string(256) type c.

  field-symbols: <ld_char> type c,
                 <ld_string> type c.                        "H1407684

  CREATE DATA lr_string LIKE id_text.
*  assign id_text to <ld_string>.                            "H1407684
  ASSIGN lr_string->* to <ld_string>.
  <ld_string> = id_text.

  refresh ct_lines.
* get starting position
  CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
           exporting im_string   = <ld_string>              "H1407684
                     im_pos_tech = id_len
           importing ex_pos_tech = ld_pos ).

      DATA lr_line TYPE REF TO data.
      CREATE DATA lr_line LIKE LINE OF ct_lines.
      ASSIGN lr_line->* to FIELD-SYMBOL(<lv_line>).

* check each single character - processing rigth to left
  do.
*   check for exit
    ld_strlen = strlen( <ld_string> ).                      "H1407684
    ld_len = ld_pos.
    ld_pos = ld_pos - 1.
    if ld_pos lt 0 or ld_strlen eq 0.
      exit.
    endif.
*   get actual character
    CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
             exporting im_string   = <ld_string>            "H1407684
                       im_pos_tech = ld_pos
             importing ex_pos_tech = ld_pos ).
    ld_len = ld_len - ld_pos.
    assign <ld_string>+ld_pos(ld_len) to <ld_char> casting. "H1407684
*   check actual character
    if ( <ld_char> eq id_del and ld_pos ne 0 ) or
       ( ld_strlen le id_len ).
*     delimiter found or string is short enough
      if ld_pos gt 0.                                       "H1365149
        concatenate <ld_string>(ld_pos) <ld_char>           "H1407684
                       into <lv_line> RESPECTING BLANKS.     "H1355908
      else.                                                 "H1365149
        <lv_line> = <ld_char>.                               "H1365149
      endif.                                                "H1365149
      append <lv_line> to ct_lines.
      shift <ld_string> by ld_pos places.                   "H1407684
      shift <ld_string> by ld_len places.                   "H1407684
      CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
               exporting im_string   = <ld_string>          "H1407684
                         im_pos_tech = id_len
               importing ex_pos_tech = ld_pos ).
    elseif ld_pos = 0.
*     no delimiter found - do a break at ID_LEN
      CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
               exporting im_string   = <ld_string>          "H1407684
                         im_pos_tech = id_len
               importing ex_pos_tech = ld_pos ).
      <lv_line> = <ld_string>(ld_pos).                       "H1407684
      append <lv_line> TO ct_lines.
      shift <ld_string> by ld_pos places.                   "H1407684
      CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
               exporting im_string   = <ld_string>          "H1407684
                         im_pos_tech = id_len
               importing ex_pos_tech = ld_pos ).
    endif.
  enddo.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_STRING=>TO_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* | [--->] IV_TYPE                        TYPE        MSGTY (default ='E')
* | [<-()] RS_BAPIRET2                    TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
method TO_BAPIRET2.

  DATA lv_textline TYPE c LENGTH 203.

  lv_textline = iv_text.

  zcl_string=>wrap(
    EXPORTING
      textline            = lv_textline
    IMPORTING
      out_line1           = rs_bapiret2-message_v1
      out_line2           = rs_bapiret2-message_v2
      out_line3           = rs_bapiret2-message_v3
      out_line4           = rs_bapiret2-message_v4
    EXCEPTIONS
      outputlen_too_large = 1
      others              = 2 ).

  rs_bapiret2-type   = iv_type.
  rs_bapiret2-id     = '00'.
  rs_bapiret2-number = '398'.

  MESSAGE ID rs_bapiret2-id TYPE rs_bapiret2-type NUMBER rs_bapiret2-number
  WITH rs_bapiret2-message_v1 rs_bapiret2-message_v2
       rs_bapiret2-message_v3 rs_bapiret2-message_v4
  INTO rs_bapiret2-message.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_STRING=>WRAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] TEXTLINE                       TYPE        C
* | [--->] DELIMITER                      TYPE        C (default =SPACE)
* | [--->] OUTPUTLEN                      TYPE        I (default =50)
* | [<---] OUT_LINE1                      TYPE        C
* | [<---] OUT_LINE2                      TYPE        C
* | [<---] OUT_LINE3                      TYPE        C
* | [<---] OUT_LINE4                      TYPE        C
* | [<---] OUT_LINES                      TYPE        TABLE
* | [EXC!] OUTPUTLEN_TOO_LARGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method WRAP.

* complete re-design with note 1307869
  CONSTANTS: MAX_OUTPUTLEN TYPE I VALUE 256.

  data: ld_line(max_outputlen) type c,
        ld_str_len type i,
        ld_fieldlen type i,
        begin of ls_line,
          line(256) type c,
        end of ls_line,
        lt_lines like TABLE OF ls_line,
        lv_outputlen TYPE i.

  lv_outputlen = outputlen.

* initial checks
  if lv_outputlen gt max_outputlen.
    raise outputlen_too_large.
  endif.
  describe field textline length ld_fieldlen in character mode.
  if ld_fieldlen lt lv_outputlen.
    lv_outputlen = ld_fieldlen.
  endif.

* check for split-option
  ld_str_len = strlen( textline ).
  if delimiter eq space or
     ld_str_len gt lv_outputlen.
*   complex split: last occurrence of delimiter before split-position
    zcl_string=>split_complex(
      EXPORTING
        id_text  = textline
        id_len   = lv_outputlen
        id_del   = delimiter
      CHANGING
        ct_lines = lt_lines ).
  else.
*   do a simple split with the delimiter
    split textline at delimiter into table lt_lines
                      in character mode.
  endif.

* fill the return parameters
  loop at lt_lines INTO ls_line.
    case sy-tabix.
      when 1. out_line1 = ls_line-line.
      when 2. out_line2 = ls_line-line.
      when 3. out_line3 = ls_line-line.
      when 4. out_line4 = ls_line-line.
    endcase.
    ld_line = ls_line-line.
    append ld_line to out_lines.
  endloop.

endmethod.
ENDCLASS.
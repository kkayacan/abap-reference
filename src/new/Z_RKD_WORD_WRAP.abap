FUNCTION Z_RKD_WORD_WRAP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TEXTLINE) TYPE  C
*"     VALUE(DELIMITER) TYPE  C DEFAULT SPACE
*"     VALUE(OUTPUTLEN) TYPE  I DEFAULT 35
*"  EXPORTING
*"     VALUE(OUT_LINE1) TYPE  C
*"     VALUE(OUT_LINE2) TYPE  C
*"     VALUE(OUT_LINE3) TYPE  C
*"     VALUE(OUT_LINE4) TYPE  C
*"  TABLES
*"      OUT_LINES OPTIONAL
*"  EXCEPTIONS
*"      OUTPUTLEN_TOO_LARGE
*"----------------------------------------------------------------------

* complete re-design with note 1307869
  CONSTANTS: MAX_OUTPUTLEN TYPE I VALUE 256.

  data: ld_line(max_outputlen) type c,
        ld_str_len type i,
        ld_fieldlen type i,
        begin of lt_lines occurs 0,
          line(256) type c,
        end of lt_lines.

* initial checks
  if outputlen gt max_outputlen.
    raise outputlen_too_large.
  endif.
  describe field textline length ld_fieldlen in character mode.
  if ld_fieldlen lt outputlen.
    outputlen = ld_fieldlen.
  endif.

* check for split-option
  ld_str_len = strlen( textline ).
  if delimiter eq space or
     ld_str_len gt outputlen.
*   complex split: last occurrence of delimiter before split-position
    perform split_complex tables lt_lines
                           using textline
                                 outputlen
                                 delimiter.
  else.
*   do a simple split with the delimiter
    split textline at delimiter into table lt_lines
                      in character mode.
  endif.

* fill the return parameters
  loop at lt_lines.
    case sy-tabix.
      when 1. out_line1 = lt_lines-line.
      when 2. out_line2 = lt_lines-line.
      when 3. out_line3 = lt_lines-line.
      when 4. out_line4 = lt_lines-line.
    endcase.
    ld_line = lt_lines-line.
    append ld_line to out_lines.
  endloop.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  SPLIT_COMPLEX      "new with note 1307869
*&---------------------------------------------------------------------*
*       split the ID_TEXT at last delimiter ID_DEL with a maximum
*       length ID_LEN
*&---------------------------------------------------------------------*
form split_complex tables ct_lines
                    using id_text type c
                          id_len  type i
                          id_del  type c.

  data: ld_len         type i,
        ld_pos         type i,
        ld_strlen      type i.
*        ld_string(256) type c.

  field-symbols: <ld_char> type c,
                 <ld_string> type c.                        "H1407684

  assign id_text to <ld_string>.                            "H1407684
  refresh ct_lines.
* get starting position
  CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
           exporting im_string   = <ld_string>              "H1407684
                     im_pos_tech = id_len
           importing ex_pos_tech = ld_pos ).
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
                       into ct_lines RESPECTING BLANKS.     "H1355908
      else.                                                 "H1365149
        ct_lines = <ld_char>.                               "H1365149
      endif.                                                "H1365149
      append ct_lines.
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
      ct_lines = <ld_string>(ld_pos).                       "H1407684
      append ct_lines.
      shift <ld_string> by ld_pos places.                   "H1407684
      CL_SCP_LINEBREAK_UTIL=>STRING_SPLIT_AT_POSITION(
               exporting im_string   = <ld_string>          "H1407684
                         im_pos_tech = id_len
               importing ex_pos_tech = ld_pos ).
    endif.
  enddo.

endform.
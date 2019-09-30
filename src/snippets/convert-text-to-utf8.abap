DATA : lo_conv      TYPE REF TO cl_abap_conv_out_ce,
       lv_xstring   TYPE xstring.

* Encoding format
lo_conv = cl_abap_conv_out_ce=>create( encoding  = 'UTF-8' ).

TRY. "For BOM if needed
        lo_conv->write( EXPORTING data   = cl_abap_char_utilities=>byte_order_mark_utf8 ).
    CATCH cx_sy_codepage_converter_init .
    CATCH cx_sy_conversion_codepage .
    CATCH cx_parameter_invalid_type .
    CATCH cx_parameter_invalid_range .
ENDTRY.

*here data is a string table but you can use structure with another method
LOOP AT data_table  INTO data_line.
    TRY.
          lo_conv->write( EXPORTING data   = data_line ).
        CATCH cx_sy_codepage_converter_init .
        CATCH cx_sy_conversion_codepage .
        CATCH cx_parameter_invalid_type .
        CATCH cx_parameter_invalid_range .
    ENDTRY.
*   next line!
    TRY.
          lo_conv->write( EXPORTING data   = cl_abap_char_utilities=>cr_lf ).
        CATCH cx_sy_codepage_converter_init .
        CATCH cx_sy_conversion_codepage .
        CATCH cx_parameter_invalid_type .
        CATCH cx_parameter_invalid_range .
    ENDTRY.
ENDLOOP.

*get full content in one xstring 
lv_xstring = lo_conv->get_buffer( ).

* et voila!! 
OPEN DATASET lv_filename FOR OUTPUT IN BINARY MODE.
IF sy-subrc EQ 0.
  TRANSFER lv_xline TO lv_filename.
  CLOSE DATASET lv_filename.
ENDIF.

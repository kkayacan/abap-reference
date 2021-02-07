REPORT yxlsx_read.

DATA: excel           TYPE REF TO zcl_excel,
      reader          TYPE REF TO zif_excel_reader.

DATA: worksheet      TYPE REF TO zcl_excel_worksheet,
      highest_column TYPE zexcel_cell_column,
      highest_row    TYPE int4,
      column         TYPE zexcel_cell_column VALUE 1,
      col_str        TYPE zexcel_cell_column_alpha,
      row            TYPE int4               VALUE 1,
      value          TYPE zexcel_cell_value.

CREATE OBJECT reader TYPE zcl_excel_reader_2007.
excel = reader->load_file( 'C:\Users\kerem.kayacan\Temp\yakit_sarfiyat.xlsx' ).

BREAK-POINT.

worksheet = excel->get_active_worksheet( ).
highest_column = worksheet->get_highest_column( ).
highest_row    = worksheet->get_highest_row( ).

WRITE: 'Highest column: ', highest_column, 'Highest row: ', highest_row.
WRITE: /.

WHILE row <= highest_row.
  WHILE column <= highest_column.
    col_str = zcl_excel_common=>convert_column2alpha( column ).
    worksheet->get_cell(
      EXPORTING
        ip_column = col_str
        ip_row    = row
      IMPORTING
        ep_value = value
    ).
    WRITE: value.
    column = column + 1.
  ENDWHILE.
  WRITE: /.
  column = 1.
  row = row + 1.
ENDWHILE.

BREAK-POINT.
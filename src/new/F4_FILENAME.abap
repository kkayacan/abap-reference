PARAMETERS: p_fname TYPE ibipparms-path OBLIGATORY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'F4_FILENAME'
   IMPORTING
     file_name = p_fname.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          INPUT                = 1
          KZMEINH              = 'X'
          MATNR                = WA_MARC-MATNR
          MEINH                = ARTICLES-VRKME
          MEINS                = WA_MARC-MEINS
        IMPORTING
          OUTPUT               = L_OUTPUT
        EXCEPTIONS
          CONVERSION_NOT_FOUND = 1
          INPUT_INVALID        = 2
          MATERIAL_NOT_FOUND   = 3
          MEINH_NOT_FOUND      = 4
          MEINS_MISSING        = 5
          NO_MEINH             = 6
          OUTPUT_INVALID       = 7
          OVERFLOW             = 8
          OTHERS               = 9.
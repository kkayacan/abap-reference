REPORT YDTP_MASS_DOWNLOAD.

*  ======================================================================================================================
*    Mass download version 1.5.2.
*  ----------------------------------------------------------------------------------------------------------------------
*    PROGRAM DESCRIPTION & USE
*    Allows a user to download programs, Functions, DD definitions, etc to the presentation server.  This version searches
*    recursively for nested includes and function modules, and allows you to download the resulting code as standard text
*    or HTML web pages within a suitable directory structure.
*
*    You can either search by object name, using wildcards if you wish, or a combination of Author and object name.  If
*    you want all objects returned for a particular author then select the author name and choose the most suitable
*    radiobutton.  All objects will be returned if the fields to the right hand side of the radiobutton are left completely
*    blank.
*
*    Compatible with R/3 Enterprise and Netweaver, for older versions of SAP you will need Direct Download version 5.xx.
*    This version removes the programming limitations imposed by developing across SAP releases 3 to 4.6.
*
*    In order to be able to download files to the SAP server you must first set up a logical filepath within transaction
*    'FILE', or use an existing one.  You must also create a external operating system command in SM69 called ZDTX_MKDIR. This
*    will then be used to create any directories needed on the SAP server

*    This program is intended to allow a person to keep a visual representation of a program for backup purposes only as
*    has not been designed to allow programs to be uploaded to SAP systems.
*  ----------------------------------------------------------------------------------------------------------------------
*
*   Author          : Copyright (C) 1998 E.G.Mellodew
*   program contact : www.dalestech.com

*   This program is free software; you can redistribute it and/or
*   modify it under the terms of the GNU General Public License
*   as published by the Free Software Foundation; either version 2
*   of the License, or (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program; if not, write to the Free Software
*   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*
*  ----------------------------------------------------------------------------------------------------------------------

*  ----------------------------------------------------------------------------------------------------------------------
*    SAP Tables
*  ----------------------------------------------------------------------------------------------------------------------
  tables: trdir, seoclass, tfdir, enlfdir, dd02l, tadiv, dd40l, transfdesc.
  type-pools: abap, seor.

*  ----------------------------------------------------------------------------------------------------------------------
*    Types
*  ----------------------------------------------------------------------------------------------------------------------
*   text element structure
  types: tTextTable like textpool.
*   GUI titles
  types: tGUITitle like d347t.

*   Message classes
  types: begin of tMessage,
           arbgb like t100-arbgb,
           stext like t100a-stext,
           msgnr like t100-msgnr,
           text  like t100-text,
         end of tMessage.

*   Screen flow.
  types: begin of tScreenFlow,
           screen like d020s-dnum,
           code like d022s-line,
         end of tScreenFlow.

*   Holds all domain texts
  types: begin of tDomainStructure,
           domname type domname,
           domvalue_l type domvalue_l,
           domvalue_h type domvalue_l,
           ddtext type val_text,
         end of tDomainStructure.

*   Holds a table\structure definition
  types: begin of tDictTableStructure,
           fieldname like dd03l-fieldname,
           position  like dd03l-position,
           keyflag   like dd03l-keyflag,
           rollname  like dd03l-rollname,
           domname   like dd03l-domname,
           datatype  like dd03l-datatype,
           leng      like dd03l-leng,
           lowercase type lowercase,
           ddtext    like dd04t-ddtext,
           iDomains type tDomainStructure occurs 0,
         end of tdictTableStructure.

*  -- Holds a table type
  types: begin of tTableType,
           typename   type ttypename,  " Name of table type
           rowtype    type ttrowtype,  " Name of row type for table types
           ttypkind   type ttypkind,   " Category of table type (range or general table type)
           range_ctyp type range_ctyp, " Elem. type of LOW and HIGH components of a Ranges type
           reftype    type ddreftype,  " Type of Object Referenced
           occurs     type ddoccurs,   " Initial Line Number for Table Types
           ddtext     type ddtext,     " Description
         end of tTableType.

*   Holds a tables attributes + its definition
  types: begin of tDictTable,
           tablename    like dd03l-tabname,
           tableTitle   like dd02t-ddtext,
           iStructure type tDictTableStructure occurs 0,
         end of tDictTable.

  types: begin of tDictFilename,
           tablename    like dd03l-tabname,
           filename type string,
         end of tDictFilename.

  types: begin of tTransformation,
           xsltName like trdir-name,
           xsltDesc like tftit-stext,
           subc     like trdir-subc,
         end of tTransformation.

*   Include program names
  types: begin of tInclude,
           includeName like trdir-name,
           includeTitle like tftit-stext,
         end of tInclude.

*   Exception class texts
  types: begin of tConcept,
           constName type string,
           concept type sotr_conc,
         end of tConcept.

*   Method
  types: begin of tMethod,
           cmpName(61),
           descript like vseomethod-descript,
           exposure like vseomethod-exposure,
           methodKey type string,
         end of tMethod.

*   Interfaces
  types: begin of tInterface,
           interfaceName like vseoclass-clsname,
         end of tInterface.

*   Class
  types: begin of tClass,
           scanned(1),
           clsname like vseoclass-clsname,
           descript like vseoclass-descript,
           msg_id like vseoclass-msg_id,
           exposure like vseoclass-exposure,
           state like vseoclass-state,
           clsfinal like vseoclass-clsfinal,
           r3release like vseoclass-r3release,
           iMethods type tMethod occurs 0,
           iDictStruct type tDictTable occurs 0,
           iTextElements type tTextTable occurs 0,
           iMessages type tMessage occurs 0,
           iInterfaces type tInterface occurs 0,
           iConcepts type tConcept occurs 0,
           iTableTypes type tTableType occurs 0,
           iTransformations type tTransformation occurs 0,
           textElementKey type string,
           publicClassKey type string,
           privateClassKey type string,
           protectedClassKey type string,
           typesClassKey type string,
           exceptionClass type abap_bool,
         end of tClass.

*   function modules
  types: begin of tFunction,
           functionName like tfdir-funcName,
           functionGroup like enlfdir-area,
           includeNumber like tfdir-include,
           functionMainInclude like tfdir-funcName,
           functionTitle like tftit-stext,
           topIncludeName like tfdir-funcName,
           progname like tfdir-pname,
           programLinkName like tfdir-pname,
           messageClass like t100-arbgb,
           iTextElements type tTextTable occurs 0,
           iSelectiontexts type tTextTable occurs 0,
           iMessages type tMessage occurs 0,
           iIncludes type tInclude occurs 0,
           iDictStruct type tDictTable occurs 0,
           iGUITitle type tGUITitle occurs 0,
           iScreenFlow type tScreenFlow occurs 0,
           iTableTypes type tTableType occurs 0,
           iTransformations type tTransformation occurs 0,
         end of tFunction.

  types: begin of tProgram,
           progname like trdir-name,
           programTitle like tftit-stext,
           subc like trdir-subc,
           messageClass like t100-arbgb,
           iMessages type tMessage occurs 0,
           iTextElements type tTextTable occurs 0,
           iSelectiontexts type tTextTable occurs 0,
           iGUITitle type tGUITitle occurs 0,
           iScreenFlow type tScreenFlow occurs 0,
           iIncludes type tInclude occurs 0,
           iDictStruct type tDictTable occurs 0,
           iTableTypes type tTableType occurs 0,
           iTransformations type tTransformation occurs 0,
         end of tProgram.

*  ----------------------------------------------------------------------------------------------------------------------
*    Internal tables
*  ----------------------------------------------------------------------------------------------------------------------
*    Dictionary object
  data: iDictionary type standard table of tDictTable with header line.
*    Dictionary objects which have previously been downloaded
  data: iDictFilename type standard table of tDictFilename with header line.
*    Table Types
  data: iTableTypes type standard table of tTableType with header line.
*    Table Type objects which have previously been downloaded
  data: iTableTypeFilename type standard table of tDictFilename with header line.
*   Function modules.
  data: iFunctions type standard table of tFunction with header line.
*   Function modules used within programs.
  data: iProgFunctions type standard table of tFunction with header line.
*   Tree display structure.
  data: iTreeDisplay type standard table of snodetext with header line.
*   Message class data
  data: iMessages type standard table of tMessage with header line.
*   Holds a single message class an all of its messages
  data: iSingleMessageClass type standard table of tMessage with header line.
*   Holds program related data
  data: iPrograms type standard table of tProgram with header line.
*   Classes
  data: iClasses type standard table of tClass with header line.
*   Table of paths created on the SAP server
  data: iServerPaths type standard table of string with header line.
*   Table of XSL Transformations
  data: iTransformations type standard table of tTransformation with header line.

*  ----------------------------------------------------------------------------------------------------------------------
*    Table prototypes
*  ----------------------------------------------------------------------------------------------------------------------
  data: dumiDictStructure type standard table of tDictTableStructure.
  data: dumiTextTab type standard table of tTextTable.
  data: dumiIncludes type standard table of tInclude.
  data: dumiHtml type standard table of string.
  data: dumiHeader type standard table of string .
  data: dumiScreen type standard table of tScreenFlow .
  data: dumIGUITitle type standard table of tGUITitle.
  data: dumiMethods type standard table of tMethod.
  data: dumiConcepts type standard table of tConcept.
  data: dumiInterfaces type standard table of tInterface.

*  ----------------------------------------------------------------------------------------------------------------------
*     Global objects
*  ----------------------------------------------------------------------------------------------------------------------
  data: objFile type ref to cl_gui_frontend_services.
  data: objRuntimeError type ref to cx_root.

*  ----------------------------------------------------------------------------------------------------------------------
*    Constants
*  ----------------------------------------------------------------------------------------------------------------------
  constants: VERSIONNO type string value '1.5.2'.
  constants: TABLES type string value 'TABLES'.
  constants: TABLE type string value 'TABLE'.
  constants: LIKE type string value 'LIKE'.
  constants: TYPE type string value 'TYPE'.
  constants: TYPEREFTO type string value 'TYPE REF TO'.
  constants: STRUCTURE type string value 'STRUCTURE'.
  constants: LOWSTRUCTURE type string value 'structure'.
  constants: OCCURS type string value 'OCCURS'.
  constants: FUNCTION type string value 'FUNCTION'.
  constants: CALLFUNCTION type string value ' CALL FUNCTION'.
  constants: MESSAGE type string  value 'MESSAGE'.
  constants: INCLUDE type string value 'INCLUDE'.
  constants: TRANSFORMATION type string value 'TRANSFORMATION'.
  constants: LOWINCLUDE type string value 'include'.
  constants: DESTINATION type string value 'DESTINATION'.
  constants: IS_TABLE type string value 'T'.
  constants: IS_TRANSFORMATION type string value 'X'.
  constants: IS_PROGRAM type string value 'P'.
  constants: IS_SCREEN type string value 'S'.
  constants: IS_GUITITLE type string value 'G'.
  constants: IS_DOCUMENTATION type string value 'D'.
  constants: IS_MESSAGECLASS type string value 'MC'.
  constants: IS_FUNCTION type string value 'F'.
  constants: IS_CLASS type string value 'C'.
  constants: IS_METHOD type string value 'M'.
  constants: ASTERIX type string value '*'.
  constants: COMMA type string value ','.
  constants: PERIOD type string value '.'.
  constants: DASH type string value '-'.
  constants: TRUE type abap_bool value 'X'.
  constants: FALSE type abap_bool value ''.
  constants: LT type string value '&lt;'.
  constants: GT type string value '&gt;'.
  constants: UNIX type string value 'UNIX'.
  constants: NON_UNIX type string value 'not UNIX'.
  constants: HTMLEXTENSION type string value 'html'.
  constants: TEXTEXTENSION type string value 'txt'.
  constants: SS_CODE type c value 'C'.
  constants: SS_TABLE type c value 'T'.

*  ----------------------------------------------------------------------------------------------------------------------
*    Global variables
*  ----------------------------------------------------------------------------------------------------------------------
  data: statusBarMessage(100).
  data: forcedExit type abap_bool value FALSE.
  data: startTime like sy-uzeit.
  data: runTime like sy-uzeit.
  data: downloadFileExtension type string.
  data: downloadFolder type string.
  data: serverSlashSeparator type string.
  data: frontendSlashSeparator type string.
  data: slashSeparatorToUse type string.
  data: serverFilesystem type filesys_d.
  data: serverFolder type string.
  data: frontendOpSystem type string.
  data: serverOpSystem type string.
  data: customerNameSpace type string.
  ranges: soProgramName for trdir-name.
  ranges: soAuthor for usr02-bname.
  ranges: soTableNames for dd02l-tabname.
  ranges: soTableTypeNames for dd40l-typename.
  ranges: soFunctionName  for tfdir-funcName.
  ranges: soClassName for vseoclass-clsname.
  ranges: soFunctionGroup for enlfdir-area.
  ranges: soXsltName for tadir-obj_name.
  field-symbols: <waDictStruct> type tDictTable.

*  ----------------------------------------------------------------------------------------------------------------------
*    Selection screen declaration
*  ----------------------------------------------------------------------------------------------------------------------
*   Author
  selection-screen: begin of block b1 with frame title tBlock1.
    selection-screen begin of line.
      selection-screen comment 5(23) tAuth.
      parameters: pAuth like usr02-bname memory id MAUTH.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 5(36) tPmod.
      parameters: pMod as checkbox.
    selection-screen end of line.
  selection-screen: end of block b1.

  selection-screen begin of block b2 with frame title tBlock2.
*   Tables
    selection-screen begin of line.
      parameters: rTable as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(15) tRtable.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tPtable.
      select-options: soTable for dd02l-tabname.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(79) tTnote.
    selection-screen end of line.

*   Table Types
    selection-screen begin of line.
      parameters: rTabType as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(15) trtabtyp.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tptabtyp.
      select-options: sotabtyp for dd40l-typename.
    selection-screen end of line.

*   Message classes
    selection-screen begin of line.
      parameters: rMess as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(18) tPMes.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(18) tMname.
      parameters: pMname like t100-arbgb memory id MMNAME.
    selection-screen end of line.

*   Function modules
    selection-screen begin of line.
      parameters: rFunc as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(30) tRfunc.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tPfname.
      select-options: soFname for tfdir-funcName.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tFgroup.
      select-options: soFgroup for enlfdir-area.
    selection-screen end of line.

*   XSLT
    selection-screen begin of line.
      parameters: rxslt as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(30) trxslt.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tpxslt.
      select-options: soxslt for transfdesc-xsltdesc.
    selection-screen end of line.

*   Classes
    selection-screen begin of line.
      parameters: rClass as checkbox default 'X'. "radiobutton group r1.
      selection-screen comment 5(30) tRClass.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tPcName.
      select-options: soClass for seoclass-clsname.
    selection-screen end of line.

*   Programs / includes
    selection-screen begin of line.
      parameters: rProg as checkbox default 'X'. "radiobutton group r1 default 'X'.
      selection-screen comment 5(18) tProg.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 10(15) tRpname.
      select-options: soProg for trdir-name.
    selection-screen end of line.

    selection-screen skip.
*   Language
    selection-screen begin of line.
      selection-screen comment 1(27) tMLang.
      parameters: pMLang like t100-sprsl default sy-langu.
    selection-screen end of line.

*   Package
    selection-screen begin of line.
      selection-screen comment 1(24) tPack.
      select-options: soPack for tadiv-devclass DEFAULT 'Z*'.
    selection-screen end of line.

*   Customer objects
    selection-screen begin of line.
      selection-screen comment 1(27) tCust.
      parameters: pCust as checkbox default 'X'.
    selection-screen end of line.

*   Alt customer name range
    selection-screen begin of line.
      selection-screen comment 1(27) tNRange.
      parameters: pCName type namespace memory id MNAMESPACE.
    selection-screen end of line.

*   Tadir content
    selection-screen begin of line.
      selection-screen comment 1(27) dlTadir.
      parameters: pTadir as checkbox default 'X'.
    selection-screen end of line.

*   Transaction code list
    selection-screen begin of line.
      selection-screen comment 1(27) dlTcode.
      parameters: pTcode as checkbox default 'X'.
    selection-screen end of line.

  selection-screen: end of block b2.

*   Additional things to download.
  selection-screen: begin of block b3 with frame title tBlock3.
    selection-screen begin of line.
      selection-screen comment 1(33) tPtext.
      parameters: pText as checkbox default 'X' memory id MTEXT.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tMess.
      parameters: pMess as checkbox default 'X' memory id MMESS.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tTTyp.
      parameters: pTTyp as checkbox default 'X' memory id MTTYP.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tXslt.
      parameters: pTrans as checkbox default 'X' memory id MXSLT.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tPinc.
      parameters: pInc as checkbox default 'X' memory id MINC.
      selection-screen comment 40(20) tReci.
      parameters: pReci as checkbox default 'X' memory id MRECI.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tPfunc.
      parameters: pFunc as checkbox default 'X' memory id MFUNC.
      selection-screen comment 40(20) tRecf.
      parameters: pRecf as checkbox default 'X' memory id MRECF.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tRecC.
      parameters: pRecC as checkbox default 'X' memory id MRECC.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tFDoc.
      parameters: pFDoc as checkbox default 'X' memory id MFDOC.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tCDoc.
      parameters: pCDoc as checkbox default 'X' memory id MCDOC.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tPscr.
      parameters: pScr as checkbox default 'X' memory id MSCR.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tPdict.
      parameters: pDict as checkbox default 'X' memory id MDICT.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(33) tSortT.
      parameters: pSortT as checkbox default ' ' memory id MSORTT.
    selection-screen end of line.
  selection-screen: end of block b3.

*   File details
  selection-screen: begin of block b4 with frame title tBlock4.
    selection-screen begin of line.
      selection-screen comment 1(20) tPhtml.
      parameters: pHtml radiobutton group g1.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 5(29) tBack.
      parameters: pBack as checkbox default 'X'.
    selection-screen end of line.

    selection-screen begin of line.
      selection-screen comment 1(20) tPtxt.
      parameters: pTxt radiobutton group g1 default 'X'.
    selection-screen end of line.

    selection-screen skip.

*   Download to SAP server
    selection-screen begin of line.
      selection-screen comment 1(25) tServ.
      parameters: pServ radiobutton group g2.
    selection-screen end of line.
    selection-screen begin of line.
      selection-screen comment 8(20) tSPath.
      parameters: pLogical like filename-fileintern memory id MLOGICAL.
    selection-screen end of line.
    selection-screen comment /28(60) tSDPath.

*   Download to PC
    selection-screen begin of line.
      selection-screen comment 1(25) tPc.
      parameters: pPc radiobutton group g2 default 'X'.
    selection-screen end of line.
    selection-screen begin of line.
      selection-screen comment 8(20) tPpath.
      parameters: pFolder like rlgrap-filename memory id MFOLDER.
    selection-screen end of line.
  selection-screen: end of block b4.

*   Display options
  selection-screen: begin of block b5 with frame title tBlock5.
*   Display final report
*    selection-screen begin of line.
*      selection-screen comment 1(33) tRep.
*      parameters: pRep as checkbox default ''.
*    selection-screen end of line.
*   Display progress messages
    selection-screen begin of line.
      selection-screen comment 1(33) tProMess.
      parameters: pProMess as checkbox default 'X'.
    selection-screen end of line.
  selection-screen: end of block b5.
  data pRep type c length 1.
*  ----------------------------------------------------------------------------------------------------------------------
*   Display a directory picker window
*  ----------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for pFolder.

  data: objFile type ref to cl_gui_frontend_services.
  data: pickedFolder type string.
  data: initialFolder type string.

    if sy-batch is initial.
      create object objFile.

      if not pFolder is initial.
        initialFolder = pFolder.
      else.
        objFile->get_temp_directory( changing temp_dir = initialFolder
                                     exceptions cntl_error = 1
                                               error_no_gui = 2
                                               not_supported_by_gui = 3 ).
      endif.

      objFile->directory_browse( exporting initial_folder = initialFolder
                                 changing selected_folder = pickedFolder
                                 exceptions cntl_error = 1
                                            error_no_gui = 2
                                            not_supported_by_gui = 3 ).

      if sy-subrc = 0.
        pFolder = pickedFolder.
      else.
        write: / 'An error has occured picking a folder'.
      endif.
    endif.

*  ----------------------------------------------------------------------------------------------------------------------
  at selection-screen.
*  ----------------------------------------------------------------------------------------------------------------------
    case 'X'.
      when pPc.
        if pFolder is initial.
*         User must enter a path to save to
          message e000(oo) with 'You must enter a file path'.
        endif.

      when pServ.
        if pLogical is initial.
*         User must enter a logical path to save to
          message e000(oo) with 'You must enter a logical file name'.
        endif.
    endcase.

*  ----------------------------------------------------------------------------------------------------------------------
  at selection-screen on pLogical.
*  ----------------------------------------------------------------------------------------------------------------------
    if not pServ is initial.
      call function 'FILE_GET_NAME' exporting logical_filename = pLogical
                                    importing file_name = serverFolder
                                    exceptions file_not_found = 1
                                               others = 2.
      if sy-subrc = 0.
        if serverFolder is initial.
          message e000(oo) with 'No file path returned from logical filename'.
        else.
*         Path to display on the selection screen
          tSDPath = serverFolder.
*         Remove the trailing slash off the path as the subroutine buildFilename will add an extra one
          shift serverFolder right deleting trailing serverSlashSeparator.
          shift serverFolder left deleting leading space.
        endif.
      else.
        message e000(oo) with 'Logical filename does not exist'.
      endif.
    endif.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soProg-low.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'PROG'
                                                        object_name  = soProg-low
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soProg-low
                                              exceptions cancel = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soProg-high.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'PROG'
                                                        object_name  = soProg-high
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soProg-high
                                              exceptions cancel = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soxslt-low.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4'
      exporting
        object_type           = 'XSLT'
        object_name           = soxslt-low
        suppress_selection    = 'X'
        use_alv_grid          = ''
        without_personal_list = ''
      importing
        object_name_selected  = soxslt-low
      exceptions
        cancel                = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soxslt-high.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4'
      exporting
        object_type           = 'XSLT'
        object_name           = soxslt-high
        suppress_selection    = 'X'
        use_alv_grid          = ''
        without_personal_list = ''
      importing
        object_name_selected  = soxslt-high
      exceptions
        cancel                = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soClass-low.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'F4_DD_ALLTYPES' exporting object = soClass-low
                                             suppress_selection = 'X'
                                             display_only = ''
                                             only_types_for_clifs = 'X'
                                   importing result = soClass-low.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soClass-high.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'F4_DD_ALLTYPES' exporting object = soClass-high
                                             suppress_selection = 'X'
                                             display_only = ''
                                             only_types_for_clifs = 'X'
                                   importing result = soClass-high.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soFName-low.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'FUNC'
                                                        object_name  = soFname-low
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soFName-low
                                              exceptions cancel = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soFName-high.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'FUNC'
                                                        object_name  = soFname-high
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soFName-high
                                              exceptions cancel = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soFGroup-low.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'FUGR'
                                                        object_name  = soFGroup-low
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soFGroup-low
                                              exceptions cancel = 1.

*   ---------------------------------------------------------------------------------------------------------------------
  at selection-screen on value-request for soFGroup-high.
*   ---------------------------------------------------------------------------------------------------------------------
    call function 'REPOSITORY_INFO_SYSTEM_F4' exporting object_type  = 'FUGR'
                                                        object_name  = soFGroup-high
                                                        suppress_selection   = 'X'
                                                        use_alv_grid = ''
                                                        without_personal_list = ''
                                              importing object_name_selected = soFGroup-high
                                              exceptions cancel = 1.

*  ----------------------------------------------------------------------------------------------------------------------
*   initialisation
*  ----------------------------------------------------------------------------------------------------------------------
  initialization.
*   Parameter screen texts.
    tBlock1 = 'Author (Optional)'.
    tBlock2 = 'Objects to download'.
    tBlock3 = 'Additional downloads for programs, function modules and classes'.
    tBlock4 = 'Download parameters'.
    tBlock5 = 'Display options'.
    tAuth   = 'Author name'.
    tPmod   = 'Include programs modified by author'.
    tCust   = 'Only customer objects'.
    tNRange = 'Alt customer name range'.
    tRtable = 'Tables / Structures'.
    trtabtyp = 'Table types'.
    tPtable = 'Table name'.
    tTnote  = 'Note: tables are stored under the username of the last person who modified them'.
    tRfunc  = 'Function modules'.
    tPfname = 'Function name'.
    tFgroup = 'Function group'.
    tRClass  = 'Classes'.
    tPcname = 'Class name'.
    tMess   = 'Message class'.
    tMName  = 'Class name'.
    tMLang  = 'Language'.
    tProg   = 'Programs'.
    tRpname = 'Program name'.
    tPack   = 'Package'.
    tPtxt   = 'Text document'.
    tPhtml  = 'HTML document'.
    tBack   = 'Include background colour'.
    tPtext  = 'Text elements'.
    tPinc   = 'Include programs'.
    tRecI   = 'Recursive search'.
    tPpath  = 'File path'.
    tSPath  = 'Logical file name'.
    tPmes   = 'Message classes'.
    tPfunc  = 'Function modules'.
    tFDoc    = 'Function module documentation'.
    tCDoc    = 'Class documentation'.
    tRecf   = 'Recursive search'.
    tRecC   = 'Class recursive search'.
    tPscr   = 'Screens'.
    tPdict  = 'Dictionary structures'.
    tSortT  = 'Sort table fields alphabetically'.
    tServ   = 'Download to server'.
    tPc     = 'Download to PC'.
*    tRep    = 'Display download report'.
    tProMess  = 'Display progress messages'.
    tRxslt     = 'Transformations'.
    tPxslt    = 'XSLT Name'.
    tTtyp = 'Table Types'.
    tXslt = 'Transformation'.
    dlTadir = 'Download TADIR content'.
    dlTcode = 'Download t-code list'.

*   Determine the frontend operating system type.
    if sy-batch is initial.
      perform determineFrontendOPSystem using frontendSlashSeparator frontendOpSystem.
    endif.
    perform determineServerOpsystem using serverSlashSeparator serverFileSystem serverOpsystem.

*   Determine if the external command exists.  If it doesn't then disable the server input field
    perform findExternalCommand using serverFileSystem.

    if pFolder is initial.
      CONCATENATE 'C:\temp\' sy-sysid '_' sy-datum into pFolder.
    endif.

*  ----------------------------------------------------------------------------------------------------------------------
  start-of-selection.
*  ----------------------------------------------------------------------------------------------------------------------
    perform checkComboBoxes.
    perform fillSelectionRanges.
    startTime = sy-uzeit.

*   Don't display status messages if we are running in the background
    if not sy-batch is initial.
      pProMess = ''.
    endif.

*   Fool the HTML routines to stop them hyperlinking anything with a space in them
    if pCName is initial.
      customerNameSpace  = '^'.
    else.
      customerNameSpace = pCName.
    endif.

*   Set the file extension and output type of the file
    if pTxt is initial.
      downloadFileExtension = HTMLEXTENSION.
    else.
      downloadFileExtension = TEXTEXTENSION.
    endif.

*   Determine which operating slash and download directory to use
    case 'X'.
      when pPc.
        slashSeparatorToUse = frontendSlashSeparator.
        downloadFolder = pFolder.
      when pServ.
        slashSeparatorToUse = serverSlashSeparator.
        downloadFolder = serverFolder.
    endcase.

    loop at soPack.
      if soPack-low ca '*'.
        soPack-option = 'CP'.
        modify soPack.
      endif.
    endloop.

*   Main program flow.
      if pTadir = 'X'.
        perform downloadTadir using downloadFolder
                                    pServ
                                    slashSeparatorToUse
                                    pProMess
                                    serverFileSystem
                                    pCust
                                    soAuthor[]
                                    soPack[]
                                    customerNameSpace.
      endif.

      if pTcode = 'X'.
        perform downloadTstct using downloadFolder
                                    pServ
                                    slashSeparatorToUse
                                    pProMess
                                    serverFileSystem
                                    pCust
                                    soAuthor[]
                                    soPack[]
                                    customerNameSpace.
        perform downloadTstcp using downloadFolder
                                    pServ
                                    slashSeparatorToUse
                                    pProMess
                                    serverFileSystem
                                    pCust
                                    soAuthor[]
                                    soPack[]
                                    customerNameSpace.
      endif.
*    case 'X'.
*     Select tables
      if rTable = 'X'.
        perform retrieveTables using iDictionary[]
                                     soTableNames[]
                                     soAuthor[]
                                     soPack[].
          if not ( iDictionary[] is initial ).
            if pPc = 'X'.
              concatenate pFolder slashSeparatorToUse 'DDStructures' into downloadFolder.
            endif.
            perform downloadDDStructures using iDictionary[]
                                               iDictFilename[]
                                               downloadFolder
                                               HTMLEXtension
                                               space
                                               pSortT
                                               slashSeparatorToUse
                                               pServ
                                               pProMess
                                               serverFileSystem
                                               pBack.
          endif.
      endif.
      if rTabType = 'X'.
        perform retrieveTableTypes using iTableTypes[]
                                         soTableTypeNames[]
                                         soAuthor[]
                                         soPack[].
        if not ( iTableTypes[] is initial ).
          if pPc = 'X'.
            CONCATENATE pFolder slashSeparatorToUse 'DDTableTypes' into downloadFolder.
          endif.
          perform downloadDDTableTypes using iTabletypes[]
                                             iTableTypeFilename[]
                                             downloadFolder
                                             htmlExtension
                                             space
                                             pSortt
                                             slashSeparatorToUse
                                             pServ
                                             pProMess
                                             serverFileSystem
                                             pBack.
        endif.
      endif.
*     Select message classes tables
      if rMess = 'X'.
        perform retrieveMessageClass using iMessages[]
                                           soAuthor[]      "Author
                                           pMname          "Message class name
                                           pMLang          "Message class language
                                           pMod            "Modified by author
                                           soPack[].       "Package
          if not ( iMessages[] is initial ).
            if pPc = 'X'.
              CONCATENATE pFolder slashSeparatorToUse 'MessageClass' into downloadFolder.
            endif.
            sort iMessages ascending by arbgb msgnr.
            loop at iMessages.
              append iMessages to iSingleMessageClass.
              at end of arbgb.
                perform downloadMessageClass using iSingleMessageClass[]
                                                   iMessages-arbgb
                                                   downloadFolder
                                                   downloadFileExtension
                                                   pHtml
                                                   space
                                                   customerNameSpace
                                                   pInc
                                                   pDict
                                                   pMess
                                                   slashSeparatorToUse
                                                   pServ
                                                   pProMess
                                                   serverFileSystem
                                                   pBack.
                clear iSingleMessageClass[].
              endat.
            endloop.
         endif.
      endif.
*     Select function modules
      if rFunc = 'X'.
        perform retrieveFunctions using soFunctionName[]   "Function name
                                        soFunctionGroup[]  "Function group
                                        iFunctions[]       "Found functions
                                        soAuthor[]         "Author
                                        pText              "Get text elements
                                        pScr               "Get screens
                                        pCust              "Customer data only
                                        customerNameSpace  "Customer name range
                                        soPack[].             "Package

*         Find Dict structures, messages, functions, includes etc.
          perform scanForAdditionalFuncStuff using iFunctions[]
                                                   pRecI                   "Search for includes recursively
                                                   pRecF                   "Search for functions recursively
                                                   pInc                    "Search for includes
                                                   pFunc                   "Search for functions
                                                   pDict                   "search for dictionary objects
                                                   pMess                   "Search for messages
                                                   pTrans                  "Search for transformations
                                                   pCust                   "Customer data only
                                                   customerNameSpace.      "Customer name range

          if not ( iFunctions[] is initial ).
            if pPc = 'X'.
              CONCATENATE pFolder slashSeparatorToUse 'Functions' into downloadFolder.
            endif.
             perform downloadFunctions using iFunctions[]
                                             iDictFilename[]
                                             iTableTypeFilename[]
                                             downloadFolder
                                             downloadFileExtension
                                             space
                                             pFDoc
                                             pHtml
                                             customerNameSpace
                                             pInc
                                             pDict
                                             TEXTEXTENSION
                                             HTMLEXTENSION
                                             pSortT
                                             slashSeparatorToUse
                                             pServ
                                             pProMess
                                             serverFileSystem
                                             pBack.
          endif.
      endif.
*     Select Classes
      if rClass = 'X'.
        perform retrieveClasses using iClasses[]
                                      iFunctions[]
                                      soClassName[]       "Class name
                                      soAuthor[]          "Author
                                      customerNameSpace   "Customer name range
                                      pMod                "Also modified by author
                                      pCust               "Customer object only
                                      pMess               "Find messages
                                      pText               "Text Elements
                                      pDict               "Dictionary structures
                                      pFunc               "Get functions
                                      pInc                "Get includes
                                      pTrans
                                      pRecF               "Search recursively for functions
                                      pRecI               "Search recursively for includes
                                      pRecC               "Search recursively for classes
                                      pMLang              "Language
                                      soPack[].           "Package


*         Find Dict structures, messages, functions, includes etc.
          perform scanForAdditionalFuncStuff using iFunctions[]
                                                   pRecI                   "Search for includes recursively
                                                   pRecF                   "Search for functions recursively
                                                   pInc                    "Search for includes
                                                   pFunc                   "Search for functions
                                                   pDict                   "search for dictionary objects
                                                   pMess                   "Search for messages
                                                   pTrans                  "Search for transformations
                                                   pCust                   "Customer data only
                                                   customerNameSpace.      "Customer name range


          if not ( iClasses[] is initial ).
            if pPc = 'X'.
              CONCATENATE pFolder slashSeparatorToUse 'Classes' into downloadFolder.
            endif.
            perform downloadClasses using iClasses[]
                                          iFunctions[]
                                          iDictFilename[]
                                          iTableTypeFilename[]
                                          downloadFolder
                                          downloadFileExtension
                                          HTMLEXTENSION
                                          TEXTEXTENSION
                                          pHtml
                                          customerNameSpace
                                          pInc
                                          pDict
                                          pCDoc
                                          pSortT
                                          slashSeparatorToUse
                                          pServ
                                          pProMess
                                          serverFileSystem
                                          pBack.
          endif.
      endif.
*     Select programs
      if rProg = 'X'.
        downloadFolder = pFolder.
        perform downloadTrdirt using downloadFolder
                                    pServ
                                    slashSeparatorToUse
                                    pProMess
                                    serverFileSystem
                                    pCust
                                    soAuthor[]
                                    soPack[]
                                    customerNameSpace.
        perform retrievePrograms using iPrograms[]
                                       iProgFunctions[]
                                       soProgramName[]    "Program name
                                       soAuthor[]         "Author
                                       customerNamespace  "Customer name range
                                       pMod               "Also modified by author
                                       pCust              "Customer object only
                                       pMess              "Find messages
                                       pText              "Text Elements
                                       pDict              "Dictionay structures
                                       pFunc              "Get functions
                                       pInc               "Get includes
                                       pScr               "Get screens
                                       pTrans             "Get Transformations
                                       pRecF              "Search recursively for functions
                                       pRecI              "Search recursively for includes
                                       soPack[].             "Package

          if not ( iPrograms[] is initial ).
            if pPc = 'X'.
              CONCATENATE pFolder slashSeparatorToUse 'Programs' into downloadFolder.
            endif.
            perform downloadPrograms using iPrograms[]
                                           iProgFunctions[]
                                           iDictFilename[]
                                           iTableTypeFilename[]
                                           downloadFolder
                                           downloadFileExtension
                                           HTMLEXTENSION
                                           TEXTEXTENSION
                                           pHtml
                                           customerNameSpace
                                           pInc
                                           pDict
                                           '' "Documentation
                                           pSortT
                                           slashSeparatorToUse
                                           pServ
                                           pProMess
                                           serverFileSystem
                                           pBack.
            endif.
      endif.
      if rXslt = 'X'.
        perform retrieveXslt using iTransformations[]
                                   soXsltName[]           "XSL Transformation name
                                   soAuthor[]             "Author
                                   customerNamespace      "Customer name range
                                   pMod                   "Also modified by author
                                   pCust                  "Customer object only
                                   soPack[].              "Package

          if not ( iTransformations[] is initial ).
            if pPc = 'X'.
              CONCATENATE pFolder slashSeparatorToUse 'XSLT' into downloadFolder.
            endif.
            perform downloadXSLT using iTransformations[]
                                       downloadFolder
                                       downloadFileExtension
                                       htmlExtension
                                       textExtension
                                       pHtml
                                       customerNamespace
                                       slashSeparatorToUse
                                       pServ
                                       space
                                       pProMess
                                       serverFileSystem
                                       pBack.
          endif.
      endif.
*    endcase.

*  ----------------------------------------------------------------------------------------------------------------------
  end-of-selection.
*  ----------------------------------------------------------------------------------------------------------------------
    if forcedExit = 0.
*     Decide what to download
*  *    case 'X'.
*  *     Download tables
*        if rTable = 'X'.
*          if not ( iDictionary[] is initial ).
*            perform downloadDDStructures using iDictionary[]
*                                               iDictFilename[]
*                                               downloadFolder
*                                               HTMLEXtension
*                                               space
*                                               pSortT
*                                               slashSeparatorToUse
*                                               pServ
*                                               pProMess
*                                               serverFileSystem
*                                               pBack.
*          endif.
*        endif.
*      if rTabType = 'X'.
*        if not ( iTableTypes[] is initial ).
*          perform downloadDDTableTypes using iTabletypes[]
*                                             iTableTypeFilename[]
*                                             downloadFolder
*                                             htmlExtension
*                                             space
*                                             pSortt
*                                             slashSeparatorToUse
*                                             pServ
*                                             pProMess
*                                             serverFileSystem
*                                             pBack.
*        endif.
*      endif.
*  *     Download message class
*        if rMess = 'X'.
*          if not ( iMessages[] is initial ).
*            sort iMessages ascending by arbgb msgnr.
*            loop at iMessages.
*              append iMessages to iSingleMessageClass.
*              at end of arbgb.
*                perform downloadMessageClass using iSingleMessageClass[]
*                                                   iMessages-arbgb
*                                                   downloadFolder
*                                                   downloadFileExtension
*                                                   pHtml
*                                                   space
*                                                   customerNameSpace
*                                                   pInc
*                                                   pDict
*                                                   pMess
*                                                   slashSeparatorToUse
*                                                   pServ
*                                                   pProMess
*                                                   serverFileSystem
*                                                   pBack.
*                clear iSingleMessageClass[].
*              endat.
*            endloop.
*         endif.
*       endif.
*  *     Download functions
*        if rFunc = 'X'.
*          if not ( iFunctions[] is initial ).
*             perform downloadFunctions using iFunctions[]
*                                             iDictFilename[]
*                                             iTableTypeFilename[]
*                                             downloadFolder
*                                             downloadFileExtension
*                                             space
*                                             pFDoc
*                                             pHtml
*                                             customerNameSpace
*                                             pInc
*                                             pDict
*                                             TEXTEXTENSION
*                                             HTMLEXTENSION
*                                             pSortT
*                                             slashSeparatorToUse
*                                             pServ
*                                             pProMess
*                                             serverFileSystem
*                                             pBack.
*          endif.
*        endif.
*  *     Download Classes
*        if rClass = 'X'.
*          if not ( iClasses[] is initial ).
*            perform downloadClasses using iClasses[]
*                                          iFunctions[]
*                                          iDictFilename[]
*                                          iTableTypeFilename[]
*                                          downloadFolder
*                                          downloadFileExtension
*                                          HTMLEXTENSION
*                                          TEXTEXTENSION
*                                          pHtml
*                                          customerNameSpace
*                                          pInc
*                                          pDict
*                                          pCDoc
*                                          pSortT
*                                          slashSeparatorToUse
*                                          pServ
*                                          pProMess
*                                          serverFileSystem
*                                          pBack.
*          endif.
*        endif.
*  *     Download programs
*        if rProg = 'X'.
*          if not ( iPrograms[] is initial ).
*            perform downloadPrograms using iPrograms[]
*                                           iProgFunctions[]
*                                           iDictFilename[]
*                                           iTableTypeFilename[]
*                                           downloadFolder
*                                           downloadFileExtension
*                                           HTMLEXTENSION
*                                           TEXTEXTENSION
*                                           pHtml
*                                           customerNameSpace
*                                           pInc
*                                           pDict
*                                           '' "Documentation
*                                           pSortT
*                                           slashSeparatorToUse
*                                           pServ
*                                           pProMess
*                                           serverFileSystem
*                                           pBack.
*            endif.
*          endif.
*        if rXSLT = 'X'.
*          if not ( iTransformations[] is initial ).
*            perform downloadXSLT using iTransformations[]
*                                       downloadFolder
*                                       downloadFileExtension
*                                       htmlExtension
*                                       textExtension
*                                       pHtml
*                                       customerNamespace
*                                       slashSeparatorToUse
*                                       pServ
*                                       space
*                                       pProMess
*                                       serverFileSystem
*                                       pBack.
*          endif.
*        endif.
*  *    endcase.

*     Free all the memory IDs we may have built up in the program
*     Free up any memory used for caching HTML versions of objects
      perform freeMemory using iPrograms[]
                               iFunctions[]
                               iProgFunctions[]
                               iDictionary[]
                               iTableTypes[]
                               iTransformations[].

      if not pRep is initial.
        get time.
        runTime = sy-uzeit - startTime.

        case 'X'.
          when rTable.
            perform fillTreeNodeTables using iDictionary[]
                                             iTreeDisplay[]
                                             runTime.
          when rTabType.
            perform fillTreeNodeTableTypes using iTableTypes[]
                                                 iTreeDisplay[]
                                                 runtime.

          when rMess.
            perform fillTreeNodeMessages using iMessages[]
                                               iTreeDisplay[]
                                               runTime.


          when rFunc.
            perform fillTreeNodeFunctions using iFunctions[]
                                                iTreeDisplay[]
                                                runTime.

          when rClass.
            perform fillTreeNodeClasses using iClasses[]
                                              iFunctions[]
                                              iTreeDisplay[]
                                              runTime.

          when rProg.
            perform fillTreeNodePrograms using iPrograms[]
                                               iProgFunctions[]
                                               iTreeDisplay[]
                                               runTime.

          when rXSLT.
            perform fillTreeNodeXSLT using iTransformations[]
                                           iTreeDisplay[]
                                           runtime.
        endcase.

        if not ( iTreeDisplay[] is initial ).
          perform displayTree using iTreeDisplay[].
        else.
          statusBarMessage = 'No items found matching selection criteria'.
          perform displayStatus using statusBarMessage 2.
        endif.
      endif.
    endif.

*   Clear out all the internal tables
    clear iPrograms[].
    clear iFunctions[].
    clear iClasses[].
    clear iProgFunctions[].
    clear iMessages[].
    clear iDictionary[].
    clear iDictFilename[].
    clear iTableTypeFilename[].
    clear iTransformations[].
    clear iTableTypes[].

*  --- Memory IDs
*   User name
    set parameter id 'MAUTH' field pAuth.
*   Message class
    set parameter id 'MMNAME' field pMname.
*   Customer namespace
    set parameter id 'MNAMESPACE' field pCName.
*   Folder
    set parameter id 'MFOLDER' field pFolder.
*   Logical filepath
    set parameter id 'MLOGICAL' field pLogical.
*   Text element checkbox
    set parameter id 'MTEXT' field pText.
*   Messages checkbox
    set parameter id 'MMESS' field pMess.
*   Includes checkbox
    set parameter id 'MINC' field pInc.
*   Recursive includes checkbox.
    set parameter id 'MRECI' field pReci.
*   Functions checkbox
    set parameter id 'MFUNC' field pFunc.
*   Recursive functions checkbox
    set parameter id 'MRECF' field pRecf.
*   Recursive classes checkbox
    set parameter id 'MRECF' field pRecC.
*   Function module documentation checkbox
    set parameter id 'MFDOC' field pFDoc.
*   Class documentation checkbox
    set parameter id 'MCDOC' field pCDoc.
*   Screens checkbox
    set parameter id 'MSCR' field pScr.
*   Dictionary checkbox
    set parameter id 'MDICT' field pDict.
*   Sort table ascending checkBox
    set parameter id 'MSORTT' field pSortT.
*   Table Types checkbox
    set parameter id 'MTTYP' field pTTyp.
*   XSLT checkbox
    set parameter id 'MXSLT' field pTrans.

    message 'DONE' type 'S'.

*  **********************************************************************************************************************
*  **************************************************SUBROUTINES*********************************************************
*  **********************************************************************************************************************

*  ----------------------------------------------------------------------------------------------------------------------
*    free memory...
*  ----------------------------------------------------------------------------------------------------------------------
  form freeMemory using iLocPrograms like iPrograms[]
                        iLocFunctions like iFunctions[]
                        iLocProgfunctions like iProgFunctions[]
                        iLocDictionary like iDictionary[]
                        iLocTableTypes like iTableTypes[]
                        iLocTransformation like iTransformations[] .

  field-symbols: <wafunction> like line of iLocfunctions.
  field-symbols: <waProgram> like line of iLocPrograms.
  field-symbols: <waDictStruct> type tDictTable.
  field-symbols: <waTableTypeStruct> like line of iLocTableTypes.
  field-symbols: <waTransformation> type tTransformation.

    loop at iLocFunctions assigning <waFunction>.
      loop at <waFunction>-iDictStruct assigning <waDictStruct>.
        free memory id <waDictStruct>-tablename.
      endloop.
    endloop.

    loop at iLocProgFunctions assigning <waFunction>.
      loop at <waFunction>-iDictStruct assigning <waDictStruct>.
        free memory id <waDictStruct>-tablename.
      endloop.
    endloop.

    loop at iLocPrograms assigning <waProgram>.
      loop at <waProgram>-iDictStruct assigning <waDictStruct>.
        free memory id <waDictStruct>-tablename.
      endloop.
    endloop.

    loop at iLocDictionary assigning <waDictStruct>.
      free memory id <waDictStruct>-tablename.
    endloop.

    loop at iLocTableTypes assigning <waTableTypeStruct>.
      free memory id <waTableTypeStruct>-typename.
    endloop.

    loop at iLocTransformation assigning <waTransformation>.
      free memory id <waTransformation>-xsltName.
    endloop.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*    checkComboBoxes...  Check input parameters
*  ----------------------------------------------------------------------------------------------------------------------
  form checkComboBoxes.

    if pAuth is initial.
      if soPack[] is initial.
        case 'X'.
          when rTable.
            if soTable[] is initial.
              statusBarMessage = 'You must enter either a table name or author.'.
            endif.
          when rFunc.
            if ( soFName[] is initial ) and ( soFGroup[] is initial ).
              if soFName[] is initial.
                statusBarMessage = 'You must enter either a function name or author.'.
              else.
                if soFGroup[] is initial.
                  statusBarMessage = 'You must enter either a function group, or an author name.'.
                endif.
              endif.
            endif.
          when rProg.
            if soProg[] is initial.
                statusBarMessage = 'You must enter either a program name or author name.'.
            endif.
        endcase.
      endif.
    else.
*     Check the user name of the person objects are to be downloaded for
      if pAuth = 'SAP*' or pauth = 'SAP'.
        statusBarMessage = 'Sorry cannot download all objects for SAP standard user'.
      endif.
    endif.

    if not statusBarMessage is initial.
      perform displayStatus using statusBarMessage 3.
      forcedExit = 1.
      stop.
    endif.
  endform.                                                                                "checkComboBoxes

*  ----------------------------------------------------------------------------------------------------------------------
*   fillSelectionRanges...      for selection routines
*  ----------------------------------------------------------------------------------------------------------------------
  form fillSelectionRanges.

  data: strLength type i.

    strLength = strlen( pcName ).

    if not pAuth is initial.
      soAuthor-sign = 'I'.
      soAuthor-option = 'EQ'.
      soAuthor-low = pAuth.
      append soAuthor.
    endif.

*   Tables
    if not soTable is initial.
      soTableNames[] = soTable[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
         loop at soTableNames.
          if soTableNames-low+0(strLength) <> pcName.
            concatenate pcName soTableNames-low into soTableNames-low.
          endif.

          if soTableNames-high+0(strLength) <> pcName.
            concatenate pcName soTableNames-high into soTableNames-high.
          endif.

          modify soTableNames.
        endloop.
      endif.
    endif.

*   Function names
    if not soFName is initial.
      soFunctionName[] = soFname[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
         loop at soFunctionName.
          if soFunctionName-low+0(strLength) <> pcName.
            concatenate pcName soFunctionName-low into soFunctionName-low.
          endif.

          if soFunctionName-high+0(strLength) <> pcName.
            concatenate pcName soFunctionName-high into soFunctionName-high.
          endif.

          modify soFunctionName.
        endloop.
      endif.
    endif.

*   Table Types
    if not soTabTyp is initial.
      soTableTypeNames[] = soTabTyp[].

*     Add in the customer namespace if we need to
      if not pCname is initial.
        loop at soTableTypeNames.
          if soTableTypeNames-low+0(strlength) <> pcname.
            concatenate pCname soTableTypeNames-low into soTableTypeNames-low.
          endif.

          if soTableNames-high+0(strlength) <> pcname.
            concatenate pcname soTableTypeNames-high into soTableTypeNames-high.
          endif.

          modify soTableTypeNames.
        endloop.
      endif.
    endif.

*   Function group
    if not soFGroup is initial.
      soFunctionGroup[] = soFGroup[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
         loop at soFunctionName.
          if soFunctionGroup-low+0(strLength) <> pcName.
            concatenate pcName soFunctionGroup-low into soFunctionGroup-low.
          endif.

          if soFunctionGroup-high+0(strLength) <> pcName.
            concatenate pcName soFunctionGroup-high into soFunctionGroup-high.
          endif.

          modify soFunctionGroup.
        endloop.
      endif.
    endif.

*   Class names
    if not soClass is initial.
      soClassName[] = soClass[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
         loop at soClassName.
          if soClassName-low+0(strLength) <> pcName.
            concatenate pcName soClassName-low into soClassName-low.
          endif.

          if soClassName-high+0(strLength) <> pcName.
            concatenate pcName soClassName-high into soClassName-high.
          endif.

          modify soClassName.
        endloop.
      endif.
    endif.

*   Program names
    if not soProg is initial.
      soProgramName[] = soProg[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
         loop at soProgramName.
          if soProgramName-low+0(strLength) <> pcName.
            concatenate pcName soProgramName-low into soProgramName-low.
          endif.

          if soProgramName-high+0(strLength) <> pcName.
            concatenate pcName soProgramName-high into soProgramName-high.
          endif.

          modify soProgramName.
        endloop.
      endif.
    endif.

*   XSLT names
    if not soXslt is initial.
      soXsltName[] = soXslt[].
*     Add in the customer namespace if we need to
      if not pcName is initial.
        loop at soXsltName.
          if soXsltName-low+0(strlength) <> pcName.
            concatenate pcName soXsltName-low into soXsltname-low.
          endif.

          if soXsltName-high+0(strlength) <> pcName.
            concatenate pcName soXsltName-high into soXsltName-high.
          endif.

          modify soXsltName.
        endloop.
      endif.
    endif.
  endform.                                                                                          " fillSelectionRanges

*  ----------------------------------------------------------------------------------------------------------------------
*    retrieveTables...             Search for tables in dictionary
*  ----------------------------------------------------------------------------------------------------------------------
  form retrieveTables using iLocDictStructure like iDictionary[]
                            soTable like soTable[]
                            soAuthor like soAuthor[]
                            value(soLocPackage) like soPack[].

  data: iDictStructure type standard table of tDictTable.
  data: waDictStructure type tDictTable.

    select a~tabname as tableName
           into corresponding fields of table iDictStructure
           from dd02l as a
           inner join tadir as b
             on a~tabname = b~obj_name
           where a~tabname in soTable
             and a~tabclass <> 'CLUSTER'
             and a~tabclass <> 'POOL'
             and a~tabclass <> 'VIEW'
             and a~as4user in soAuthor
             and a~as4local = 'A'
             and b~pgmid = 'R3TR'
             and b~object = 'TABL'
             and b~devclass in soLocPackage[].

    loop at iDictStructure into waDictStructure.
      perform findTableDescription using waDictStructure-tablename
                                         waDictStructure-tableTitle.

      perform findTableDefinition using waDictStructure-tableName
                                        waDictStructure-iStructure[].

      append waDictStructure to iLocDictStructure.
      clear waDictStructure.
    endloop.
  endform.                                                                                                "retrieveTables

*  ----------------------------------------------------------------------------------------------------------------------
*    retrieveTableTypes
*  ----------------------------------------------------------------------------------------------------------------------
  form retrieveTableTypes  using iLocTableTypes      like iTableTypes[]
                                 soTableTypeNames    like soTable[]
                                 soAuthor            like soAuthor[]
                                 value(soLocPackage) like soPack[].

    select *
           into corresponding fields of table iLocTableTypes
           from dd40l as a
           inner join dd40t as t
              on a~typename = t~typename
           inner join tadir as b
             on a~typename = b~obj_name
           where a~typename in sotabletypenames
             and t~ddlanguage eq sy-langu
             and a~as4user in soauthor
             and a~as4local = 'A'
             and b~pgmid = 'R3TR'
             and b~object = 'TTYP'
             and b~devclass in solocpackage[].

  endform.                                                                                          " RETRIEVETABLETYPES

*  -------------------------------------------------------------------------------------------------------
*    retrieveXSLT...
*  -------------------------------------------------------------------------------------------------------
  form retrieveXSLT using ilocTransformations like iTransformations[]
                          rangexslt   like soxsltname[]
                          rangeauthor like soAuthor[]
                          value(custnamerange)
                          value(alsomodifiedbyauthor)
                          value(customerprogsonly)
                          value(solocpackage) like sopack[].

  data: warangexslt like line of rangexslt.

    if rangexslt[] is initial.
*     We are finding all programs by an author
      perform findAllXsltForAuthor using iLocTransformations[]
                                         rangexslt[]
                                         rangeauthor[]
                                         custnamerange
                                         alsomodifiedbyauthor
                                         customerprogsonly
                                         solocpackage[].
    else.
      read table rangexslt index 1 into warangexslt.
      if warangexslt-low cs asterix.
        perform findXsltByWildcard using ilocTransformations[]
                                         rangexslt[]
                                         rangeauthor[]
                                         custnamerange
                                         customerprogsonly
                                         solocpackage[].
      else.
        perform checkXsltDoesExist using ilocTransformations[]
                                         rangexslt[].
      endif.
    endif.
  endform.                                                                               "retrieveXSLT

*  ----------------------------------------------------------------------------------------------------------------------
*    findTableDescription...  Search for table description in dictionary
*  ----------------------------------------------------------------------------------------------------------------------
  form findTableDescription using value(tableName)
                                        tableDescription.

      select single ddtext
                    from dd02t
                    into tableDescription
                    where tabname = tableName
                     and ddlanguage = pMLang.
  endform.                                                                                          "findTableDescription

*  ----------------------------------------------------------------------------------------------------------------------
*    findTableDefinition... Find the structure of a table from the SAP database.
*  ----------------------------------------------------------------------------------------------------------------------
  form findTableDefinition using value(tablename)
                                 iDictStruct like dumIDictStructure[].

  data gotstate like dcobjif-gotstate.
  data: definition type standard table of DD03P with header line.
  data: iDomainDataA type standard table of dd07v with header line.
  data: iDomainDataN type standard table of dd07v with header line.
  data: waDictStruct type tDictTableStructure.
  data: waDomainStruct type tDomainStructure.
  data: wadd02v_n type dd02v.

    clear iDictStruct[].

    call function 'DD_INT_TABL_GET'
      exporting
        tabname              = tablename
        langu                = pmlang
      importing
        gotstate             = gotstate
        dd02v_n              = wadd02v_n
      tables
        dd03p_n              = definition
      exceptions
        internal_error       = 1.

    if sy-subrc = 0 and not wadd02v_n is initial.
      call function 'DD_TABL_EXPAND'
        exporting
          dd02v_wa                = wadd02v_n
          mode                    = 46
          prid                    = 0
        tables
          dd03p_tab               = definition
        exceptions
          illegal_parameter       = 1.
     endif.

    if sy-subrc = 0 and gotstate = 'A'.
      loop at definition.
        move-corresponding definition to waDictStruct.
        perform removeLeadingZeros changing waDictStruct-position.
        perform removeLeadingZeros changing waDictStruct-leng.

*       Add any domain data
        call function 'DD_DOMA_GET'
          exporting
            domain_name         = definition-domname
            get_state           = 'M  '
            langu               = pmlang
*            prid                = 0
            withtext            = 'X'
           tables
             dd07v_tab_a         = iDomainDataA
             dd07v_tab_n         = iDomainDataN
          exceptions
            illegal_value       = 1
            op_failure          = 2.

        loop at iDomainDataA.
          move-corresponding iDomainDataA to waDomainStruct.
          append waDomainStruct to waDictStruct-iDomains.
        endloop.

        clear iDomainDataA[].
        append waDictStruct to iDictStruct.
      endloop.
    endif.
  endform.                                                                                           "findTableDefinition

*  ----------------------------------------------------------------------------------------------------------------------
*    retrieveMessageClass...   Retrieve a message class from the SAP database
*  ----------------------------------------------------------------------------------------------------------------------
  form retrieveMessageClass using iLocMessages like iMessages[]
                                  rangeAuthor like soAuthor[]
                                  value(messageClassName)
                                  value(messageClassLang)
                                  value(modifiedBy)
                                  value(soLocPackage) like soPack[].

  data: waMessage type tMessage.
  data: iMClasses type standard table of arbgb.


    if not messageClassName is initial.
*     Check to see if the message class exists in the package
      if not soLocPackage[] is initial.
          select obj_name as arbgb
                 into table iMClasses
                 from tadir
                 where pgmid = 'R3TR'
                   and object = 'MSAG'
                   and devclass in soLocPackage.
      endif.

      replace '*' with '%' into messageClassName.
      if iMCLasses[] is initial.
        select t100~arbgb
               t100~text
               t100~msgnr
               t100a~stext
               appending corresponding fields of table iLocMessages
               from t100
               inner join t100a
                 on t100a~arbgb = t100~arbgb
               where t100a~masterLang = messageClassLang
                 and t100~sprsl = messageClassLang
                 and t100~arbgb like messageClassName
                 and t100a~respUser in rangeAuthor[].
      else.
        select t100~arbgb
               t100~text
               t100~msgnr
               t100a~stext
               appending corresponding fields of table iLocMessages
               from t100
               inner join t100a
                 on t100a~arbgb = t100~arbgb
               for all entries in iMClasses
                 where t100~sprsl = messageClassLang
                 and ( t100~arbgb like messageClassName and t100~arbgb = iMClasses-table_line )
                 and t100a~masterLang = messageClassLang
                 and t100a~respUser in rangeAuthor[].
      endif.
    else.
      if modifiedBy is initial.
*         Select by author
          select t100~arbgb                             "#EC CI_BUFFJOIN
                 t100~msgnr
                 t100~text
                 t100a~stext
                 appending corresponding fields of table iLocMessages
                 from t100
                 inner join t100a
                   on t100a~arbgb = t100~arbgb
                 inner join tadir
                   on t100~arbgb = tadir~obj_name
                 where t100a~masterLang = messageClassLang
                   and t100a~respUser in rangeAuthor[]
                   and tadir~pgmid = 'R3TR'
                   and tadir~object = 'MSAG'
                   and tadir~devclass in soLocPackage[].

      else.
*       Select also by the last person who modified the message class
        select t100~arbgb                             "#EC CI_BUFFJOIN
               t100~msgnr
               t100~text
               t100a~stext
               appending corresponding fields of table iLocMessages
               from t100
               inner join t100a
                 on t100a~arbgb = t100~arbgb
               inner join tadir
                 on t100~arbgb = tadir~obj_name
               where t100a~masterLang = messageClassLang
                 and t100a~respUser in rangeAuthor[]
                 and t100a~lastUser in rangeAuthor[]
                 and tadir~pgmid = 'R3TR'
                 and tadir~object = 'MSAG'
                 and tadir~devclass in soLocPackage[].
      endif.
    endif.
  endform.                                                                                          "retrieveMessageClass

*  ----------------------------------------------------------------------------------------------------------------------
*    retrieveFunctions...   Retrieve function modules from SAP DB.  May be called in one of two ways
*  ----------------------------------------------------------------------------------------------------------------------
  form retrieveFunctions using soFName like soFunctionName[]
                               soFGroup like soFunctionGroup[]
                               iLocFunctionNames like iFunctions[]
                               value(solocAuthor) like soAuthor[]
                               value(getTextElements)
                               value(getScreens)
                               value(customerOnly)
                               value(customerNameRange)
                               value(soLocPackage) like soPack[].

  data: waFunctionName type tFunction.
  data: noGroupsFound type abap_bool value TRUE.
  data: previousFG type v_fdir-area.

*   select by function name and/or function group.
    select a~funcName as functionName
           a~area as functionGroup
           into corresponding fields of table iLocfunctionNames
           from v_fdir as a
           inner join tlibv as b
             on a~area = b~area
           inner join tadir as c
             on a~area = c~obj_name
           where a~funcName in soFName[]
             and a~area in soFGroup[]
             and a~generated = ''
             and b~uname in soLocAuthor[]
             and pgmid = 'R3TR'
             and object = 'FUGR'
             and devclass in solocPackage[]
             order by a~area.

    loop at iLocFunctionNames into waFunctionName.
      perform retrieveFunctionDetail using waFunctionName-functionName
                                           waFunctionName-progname
                                           waFunctionName-includeNumber
                                           waFunctionName-functionTitle.

      perform findMainFunctionInclude using waFunctionName-progname
                                            wafunctionName-functionGroup
                                            waFunctionName-includeNumber
                                            waFunctionName-functionMainInclude.

      perform findFunctionTopInclude using waFunctionName-progname
                                           wafunctionName-functionGroup
                                           waFunctionName-topIncludeName.

*     Find all user defined includes within the function group
      perform scanForFunctionIncludes using waFunctionName-progname
                                            customerOnly
                                            customerNameRange
                                            waFunctionName-iIncludes[].
*     Find main message class
      perform findMainMessageClass using waFunctionName-progname
                                         waFunctionName-messageClass.

*     Find any screens declared within the main include
      if not getScreens is initial.
        if previousFG is initial or previousFG <> waFunctionName-functionGroup.
          perform findFunctionScreenFlow using waFunctionName.

*         Search for any GUI texts
          perform retrieveGUITitles using waFunctionName-iGUITitle[]
                                          waFunctionName-progname.
        endif.
      endif.

      if not getTextElements is initial.
*       Find the program texts from out of the database.
        perform retrieveProgramTexts using waFunctionName-iSelectionTexts[]
                                           waFunctionName-iTextElements[]
                                           waFunctionName-progname.
      endif.

      previousFG = waFunctionName-functionGroup.
      modify iLocFunctionNames from waFunctionName.
    endloop.
  endform.                                                                                             "retrieveFunctions

*  ----------------------------------------------------------------------------------------------------------------------
*    retrieveFunctionDetail...   Retrieve function module details from SAP DB.
*  ----------------------------------------------------------------------------------------------------------------------
  form retrieveFunctionDetail using value(functionName)
                                          progname
                                          includeName
                                          titleText.

    select single pname
                  include
                  from tfdir
                  into (progname, includeName)
                  where funcName = functionName.

    if sy-subrc = 0.
      select single stext
                    from tftit
                    into titleText
                    where spras = pMLang
                      and funcName = functionName.
    endif.
  endform.                                                                                        "retrieveFunctionDetail                                                                                  "findFunctionTopInclude

*  ----------------------------------------------------------------------------------------------------------------------
*   scanForAdditionalFuncStuff... Search for additional things relating to functions
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForAdditionalFuncStuff using iLocFunctions like iFunctions[]
                                        value(recursiveIncludes)
                                        value(recursiveFunctions)
                                        value(searchForIncludes)
                                        value(searchForFunctions)
                                        value(searchForDictionary)
                                        value(searchForMessages)
                                        value(searchForTransformations)
                                        value(customerOnly)
                                        value(customerNameRange).

  data: waFunction type tFunction.
  data: waInclude type tInclude.

    loop at iLocFunctions into waFunction.
      if not searchForIncludes is initial.
*       Search in the main program
        perform scanForIncludePrograms using waFunction-progname
                                             recursiveIncludes
                                             customerOnly
                                             customerNameRange
                                             waFunction-iIncludes[].

*       Search in the main include
        perform scanForIncludePrograms using waFunction-functionMainInclude
                                             recursiveIncludes
                                             customerOnly
                                             customerNameRange
                                             waFunction-iIncludes[].

*       Search in the top include
        perform scanForIncludePrograms using waFunction-topIncludeName
                                             recursiveIncludes
                                             customerOnly
                                             customerNameRange
                                             waFunction-iIncludes[].
      endif.

      if not searchForFunctions is initial.
        perform scanForFunctions using waFunction-functionMainInclude
                                       waFunction-programLinkName
                                       recursiveIncludes
                                       recursiveFunctions
                                       customerOnly
                                       customerNameRange
                                       iLocFunctions[].
      endif.

      modify iLocFunctions from waFunction.
    endloop.


    loop at iLocFunctions into waFunction.
*     Now we have everthing perhaps we had better find all the dictionary structures
      if not searchForDictionary is initial.
        perform scanForTables using waFunction-progname
                                    customerOnly
                                    customerNameRange
                                    waFunction-iDictStruct[].

        perform scanForLikeOrType using waFunction-progname
                                        customerOnly
                                        customerNameRange
                                        waFunction-iDictStruct[]
                                        waFunction-iTableTypes[].

        perform scanForTables using waFunction-functionMainInclude
                                    customerOnly
                                    customerNameRange
                                    waFunction-iDictStruct[].

        perform scanForLikeOrType using waFunction-functionMainInclude
                                        customerOnly
                                        customerNameRange
                                        waFunction-iDictStruct[]
                                        waFunction-iTableTypes[].

        loop at waFunction-iIncludes into waInclude.
          perform scanForTables using waInclude-includeName
                                      customerOnly
                                      customerNameRange
                                      waFunction-iDictStruct[].

          perform scanForLikeOrType using waInclude-includeName
                                          customerOnly
                                          customerNameRange
                                          waFunction-iDictStruct[]
                                          waFunction-iTableTypes[].
        endloop.

        modify iLocFunctions from waFunction.
      endif.

*     Now search for all messages
      if not searchForMessages is initial.
        perform scanForMessages using waFunction-progName
                                      waFunction-messageClass
                                      waFunction-iMessages[].
        modify iLocFunctions from waFunction.
      endif.

*     Search for TRansformations.
      if not searchForTransformations is initial.
         perform scanForTransformations using waFunction-progName
                                              customerOnly
                                              customerNameRange
                                              waFunction-iTransformations[].
      endif.
    endloop.
  endform.                                                                                    "scanForAdditionalFuncStuff

*  ----------------------------------------------------------------------------------------------------------------------
*   scanForClasses... Search each class or method for other classes
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForClasses using value(className)
                            value(classLinkName)
                            value(customerOnly)
                            value(customerNameRange)
                                  iLocClasses like iClasses[]
                            value(soLocPackage) like soPack[].

  data iLines type standard table of string with header line.
  data: head type string.
  data: tail type string.
  data: lineLength type i value 0.
  data: waLine type string.
  data: waClass type tClass.
  data: waSearchClass type tClass.
  data: castClassName type program.
  data: exceptionCustomerNameRange type string.

*   Build the name of the possible cusotmer exception classes
    concatenate customerNameRange 'CX_' into  exceptionCustomerNameRange.

*   Read the program code from the textpool.
    castClassName = className.
    read report castClassName into iLines.

    loop at iLines into waLine.
*     Find custom tables.
      lineLength = strLen( waLine ).
      if lineLength > 0.
        if waLine(1) = ASTERIX.
          continue.
        endif.

        translate waLine to upper case.

        find TYPEREFTO in waLine ignoring case.
        if sy-subrc = 0.
*         Have found a reference to another class
          split waLine at TYPE into head tail.
          shift tail left deleting leading space.
          split tail at 'REF' into head tail.
          shift tail left deleting leading space.
          split tail at 'TO' into head tail.
          shift tail left deleting leading space.
          if tail cs PERIOD.
            split tail at PERIOD into head tail.
          else.
            if tail cs COMMA.
              split tail at COMMA into head tail.
            endif.
          endif.
        else.
*         Try and find classes which are only referenced through static mehods
          find '=>' in waLine match offset sy-fdpos.
          if sy-subrc = 0.
            head = waline+0(sy-fdpos).
            shift head left deleting leading space.
            condense head.
            find 'call method' in head ignoring case.
            if sy-subrc = 0.
              shift head left deleting leading space.
              split head at space into head tail.
              split tail at space into head tail.
*             Should have the class name here
              head = tail.
            else.
*             Still have a class name even though it does not have the words call method in front
              if waLine cs '='.
                split waLine at '=' into tail head.
                shift head left deleting leading space.
                split head at '=' into head tail.
              endif.
              sy-subrc = 0.
            endif.
          endif.
        endif.

        if sy-subrc = 0.
          try.
            if head+0(1) = 'Y' or head+0(1) = 'Z' or head cs customerNameRange.
*             We have found a class best append it to our class table if we do not already have it.
              read table iLocClasses into waSearchClass with key clsName = head.
              if sy-subrc <> 0.
                if head+0(3) = 'CX_'
                   or head+0(4) = 'ZCX_'
                   or head+0(4) = 'YCX_'
                   or head cs exceptionCustomerNameRange.

                  waClass-exceptionClass = TRUE.
                endif.

                waClass-clsname = head.

*               Check the package
                if not soLocPackage[] is initial.
                  select single obj_name
                         from tadir
                         into waClass-clsName
                         where pgmid = 'R3TR'
                           and object = 'CLAS'
                           and obj_name = waClass-clsName
                           and devclass in soLocPackage[].
                  if sy-subrc = 0.
                    append waClass to iLocClasses.
                  endif.
                else.
                  append waClass to iLocClasses.
                endif.
              endif.
            endif.
            catch cx_sy_range_out_of_bounds.
          endtry.
        endif.
      endif.
    endloop.
  endform.                                                                                                "scanForClasses

*  ----------------------------------------------------------------------------------------------------------------------
*   scanForIncludePrograms... Search each program for include programs
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForIncludePrograms using value(programName)
                                    value(recursiveIncludes)
                                    value(customerOnly)
                                    value(customerNameRange)
                                          iLocIncludes like dumiIncludes[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iKeywords type standard table of text20 with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: waTokens type stokes.
  data: waInclude type tInclude.
  data: waIncludeExists type tInclude.
  data: maxLines type i.
  data: nextLine type i.
  data: castProgramName type program.

*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iIncludeLines.

    append INCLUDE to iKeywords.
    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements keywords from iKeywords.

    clear iIncludeLines[].

    maxLines = lines( iTokens ).
    loop at iTokens where str = INCLUDE and type = 'I'.
       nextLine = sy-tabix + 1.
       if nextLine <= maxLines.
         read table iTokens index nextLine into waTokens.

*        Are we only to find customer includes?
         if not customerOnly is initial.
           try.
             if waTokens-str+0(1) = 'Y' or waTokens-str+0(1) = 'Z' or waTokens-str cs customerNameRange
                or waTokens-str+0(2) = 'LZ' or waTokens-str+0(2) = 'LY'
                or waTokens-str+0(2) = 'MZ' or waTokens-str+0(2) = 'MY'.

             else.
               continue.
             endif.
             catch cx_sy_range_out_of_bounds into objRuntimeError.
           endtry.
         endif.

         waInclude-includeName = waTokens-str.

*        Best find the program title text as well.
         perform findProgramOrIncludeTitle using waInclude-includeName
                                                 waInclude-includeTitle.

*        Don't append the include if we already have it listed
         read table iLocIncludes into waIncludeExists with key includeName = waInclude-includeName.
         if sy-subrc <> 0.
           append waInclude to iLocIncludes.

           if not recursiveIncludes is initial.
*            Do a recursive search for other includes
             perform scanForIncludePrograms using waInclude-includeName
                                                  recursiveIncludes
                                                  customerOnly
                                                  customerNameRange
                                                  iLocIncludes[].
           endif.
         endif.
       endif.
     endloop.
  endform.                                                                                        "scanForIncludePrograms

*  ----------------------------------------------------------------------------------------------------------------------
*   scanForIncludePrograms... Search each program for include programs
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForTransformations using value(programName)
                                    value(customerOnly)
                                    value(customerNameRange)
                                          iLocTransformation like iTransformations[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iKeywords type standard table of text20 with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: waTokens type stokes.
  data: waTransformation type tTransformation.
  data: waTransformationExists type tTransformation.
  data: maxLines type i.
  data: nextLine type i.
  data: castProgramName type program.


*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iIncludeLines.

    append 'CALL' to iKeywords.
    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements keywords from iKeywords.

    clear iIncludeLines[].

    maxLines = lines( iTokens ).
    loop at iTokens where str = TRANSFORMATION and type = 'I'.
       nextLine = sy-tabix + 1.
       if nextLine <= maxLines.
         read table iTokens index nextLine into waTokens.

*        Are we only to find customer transformations?
         if not customerOnly is initial.
           try.
             if waTokens-str+0(1) = 'Y' or waTokens-str+0(1) = 'Z' or waTokens-str cs customerNameRange.
             else.
               continue.
             endif.
             catch cx_sy_range_out_of_bounds into objRuntimeError.
           endtry.
         endif.

         waTransformation-xsltName = waTokens-str.

*        Don't append the include if we already have it listed
         read table iLocTransformation into waTRansformationExists with key xsltName = waTransformation-xsltName.
         if sy-subrc <> 0.
           append waTransformation to iLocTransformation.

           perform scanForTransIncludes using waTransformation-xsltName
                                              customerOnly
                                              customerNameRange
                                              iLocTransformation[].

         endif.
       endif.
     endloop.
  endform.                                                                                        "scanForIncludePrograms

*  ----------------------------------------------------------------------------------------------------------------------
*   scanforTransIncludes...
*  ----------------------------------------------------------------------------------------------------------------------
  form scanforTransIncludes using value(xsltName)
                                  value(customerOnly)
                                  value(customerNameRange)
                                        iLocTransformations like iTransformations[].

  data: iXsltSource type o2pageline_table.
  data: waXsltAttributes type o2xsltattr.
  data: waXSLTSource like line of iXSLTSource.
  data: dummy type string.
  data: waTransformationExists type tTransformation.
  data: waTransformation type tTransformation.

     cl_o2_api_xsltdesc=>load( exporting p_xslt_desc = xsltname
                              importing p_source = iXsltSource
                                        p_attributes = waXsltAttributes
                              exceptions not_existing = 1
                                         permission_failure = 2
                                         error_occured = 3
                                         version_not_found = 4 ).

     loop at iXsltSource into waXSLTSource.
       if waXsltSource-line cs '<xsl:include'.
         split waXsltSource-line at '"' into dummy waXsltSource-line.
         split waXsltSource-line at '"' into waXSLTSource dummy.

*        Are we only to find customer transformations?
         if not customerOnly is initial.
           try.
             if waXsltSource-line+0(1) = 'Y' or waXsltSource-line+0(1) = 'Z' or waXsltSource-line cs customerNameRange.
             else.
               continue.
             endif.
             catch cx_sy_range_out_of_bounds into objRuntimeError.
           endtry.
         endif.

*        Don't append the include if we already have it listed
         read table iLocTransformations into waTransformationExists with key xsltName = waXsltSource-line.
         if sy-subrc <> 0.
           waTransformation-xsltName = waXSLTSource-line.
           append waTransformation to iLocTransformations.

*          Ok we have an include.  Are there any sub includes
           perform scanforTransIncludes using waTransformation-xsltName
                                              customerOnly
                                              customerNameRange
                                              iLocTransformations[].
         endif.
       endif.
     endloop.
  endform.


*  ----------------------------------------------------------------------------------------------------------------------
*   scanForFunctions... Search each program for function modules
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForFunctions using value(programName)
                              value(programLinkName)
                              value(recursiveIncludes)
                              value(recursiveFunctions)
                              value(customerOnly)
                              value(customerNameRange)
                                    iLocFunctions like iFunctions[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: waTokens type stokes.
  data: waFunction type tFunction.
  data: waFunctionComparison type tFunction.
  data: maxLines type i.
  data: nextLine type i.
  data: castProgramName type program.
  data: skipThisloop type abap_bool.

*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iIncludeLines.
    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements.
    clear iIncludeLines[].

    maxLines = lines( iTokens ).
    loop at iTokens where str = FUNCTION and type = 'I'.

       nextLine = sy-tabix + 1.
       if nextLine <= maxLines.
         read table iTokens index nextLine into waTokens.

*        Are we only to find customer functions
         skipThisLoop = FALSE.
         if not customerOnly is initial.
           try.
             if waTokens-str+1(1) = 'Y' or waTokens-str+1(1) = 'Z' or waTokens-str cs customerNameRange.
             else.
               skipThisLoop = TRUE.
             endif.
           catch cx_sy_range_out_of_bounds into objRuntimeError.
           cleanup.
             skipThisLoop = TRUE.
           endtry.
         endif.

         if skipThisLoop = FALSE.
           waFunction-functionName = waTokens-str.
           replace all occurrences of '''' in waFunction-functionName with ' '.
           condense waFunction-functionName.

*          Don't add a function if we alread have it listed.
           read table iLocFunctions with key functionName = waFunction-functionName into waFunctionComparison.
           if sy-subrc <> 0.
*            Add in the link name if the function is linked to a program
             waFunction-programLinkName = programLinkName.

*            Don't download functions which are called through an RFC destination
             nextline = sy-tabix + 2.
             read table iTokens index nextLine into waTokens.
             if waTokens-str <> DESTINATION.

*              Find the function group
               select single area from v_fdir into wafunction-functionGroup where funcName = waFunction-functionName.

               if sy-subrc = 0.
*                Best find the function number as well.
                 perform retrieveFunctionDetail using waFunction-functionName
                                                      waFunction-progname
                                                      waFunction-includeNumber
                                                      waFunction-functionTitle.

                 perform findMainFunctionInclude using waFunction-progname
                                                       waFunction-functionGroup
                                                       waFunction-includeNumber
                                                       waFunction-functionMainInclude.

                 perform findFunctionTopInclude using waFunction-progname
                                                      wafunction-functionGroup
                                                      waFunction-topIncludeName.

*                Find main message class
                 perform findMainMessageClass using waFunction-progname
                                                    waFunction-messageClass.

                 append waFunction to iLocFunctions.

*                Now lets search a little bit deeper and do a recursive search for other includes
                 if not recursiveIncludes is initial.
                   perform scanForIncludePrograms using waFunction-functionMainInclude
                                                        recursiveIncludes
                                                        customerOnly
                                                        customerNameRange
                                                        waFunction-iIncludes[].
                 endif.

*                Now lets search a little bit deeper and do a recursive search for other functions
                 if not recursiveFunctions is initial.
                   perform scanForFunctions using waFunction-functionMainInclude
                                                  space
                                                  recursiveIncludes
                                                  recursiveFunctions
                                                  customerOnly
                                                  customerNameRange
                                                  iLocFunctions[].
                 endif.
                 clear waFunction.
               endif.
             endif.
           endif.

           clear waFunction.
         endif.
       endif.
     endloop.
  endform.                                                                                              "scanForFunctions

*  ----------------------------------------------------------------------------------------------------------------------
*    scanForFunctionIncludes... Find all user defined includes within the function group
*  ----------------------------------------------------------------------------------------------------------------------
  form scanForFunctionIncludes using poolName
                                     value(customerOnly)
                                     value(customerNameRange)
                                     iLocIncludes like dumiIncludes[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iKeywords type standard table of text20 with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: waTokens type stokes.
  data: waInclude type tInclude.
  data: waIncludeExists type tInclude.
  data: maxLines type i.
  data: nextLine type i.
  data: castProgramName type program.

*   Read the program code from the textpool.
    castProgramName = poolName.
    read report castProgramName into iIncludeLines.

    append INCLUDE to iKeywords.
    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements keywords from iKeywords.

    clear iIncludeLines[].

    maxLines = lines( iTokens ).
    loop at iTokens where str = INCLUDE and type = 'I'.
       nextLine = sy-tabix + 1.
       if nextLine <= maxLines.
         read table iTokens index nextLine into waTokens.

         if waTokens-str cp '*F++'.
*          Are we only to find customer includes?
           if not customerOnly is initial.
             try.
               if waTokens-str+0(2) = 'LY' or waTokens-str+0(2) = 'LZ' or waTokens-str cs customerNameRange.
               else.
                 continue.
               endif.
               catch cx_sy_range_out_of_bounds into objRuntimeError.
             endtry.
           endif.

           waInclude-includeName = waTokens-str.

*          Best find the program title text as well.
           perform findProgramOrIncludeTitle using waInclude-includeName
                                                   waInclude-includeTitle.

*          Don't append the include if we already have it listed
           read table iLocIncludes into waIncludeExists with key includeName = waInclude-includeName.
           if sy-subrc <> 0.
             append waInclude to iLocIncludes.
           endif.
         endif.
       endif.
     endloop.
  endform.                                                                                       "scanForFunctionIncludes

*  ----------------------------------------------------------------------------------------------------------------------
*    findProgramOrIncludeTitle...   Finds the title text of a program.
*  ----------------------------------------------------------------------------------------------------------------------
  form findProgramOrIncludeTitle using value(programName)
                                             titleText.

    select single text
                  from trdirt
                  into titleText
                  where name = programName
                    and sprsl = pMLang.
  endform.                                                                                     "findProgramOrIncludeTitle

*  ----------------------------------------------------------------------------------------------------------------------
*   retrievePrograms...    find programs and sub objects from SAP DB
*  ----------------------------------------------------------------------------------------------------------------------
  form retrievePrograms using iLocProgram like iPrograms[]
                              iLocFunctions like iFunctions[]
                              rangeProgram like soProgramName[]
                              rangeAuthor like soAuthor[]
                              value(custNameRange)
                              value(alsoModifiedByauthor)
                              value(customerProgsOnly)
                              value(getMessages)
                              value(getTextElements)
                              value(getCustDictStructures)
                              value(getFunctions)
                              value(getIncludes)
                              value(getScreens)
                              value(getTransformations)
                              value(recursiveFuncSearch)
                              value(recursiveIncludeSearch)
                              value(soLocPackage) like soPack[].

  data: waRangeProgram like line of rangeProgram.

    if rangeProgram[] is initial.
*     We are finding all programs by an author
      perform findAllProgramsForAuthor using iLocProgram[]
                                             rangeProgram[]
                                             rangeAuthor[]
                                             custNameRange
                                             alsoModifiedByAuthor
                                             customerProgsOnly
                                             soLocPackage[].
    else.
      read table rangeProgram index 1 into waRangeProgram.
      if waRangeProgram-low cs ASTERIX.
        perform findProgramsByWildcard using iLocProgram[]
                                             rangeProgram[]
                                             rangeAuthor[]
                                             custNameRange
                                             customerProgsOnly
                                             soLocPackage[].
      else.
        perform checkProgramDoesExist using iLocProgram[]
                                            rangeProgram[].
      endif.
    endif.

*   Find extra items
    perform scanForAdditionalProgStuff using iLocProgram[]
                                             iLocFunctions[]
                                             getTextElements
                                             getMessages
                                             getScreens
                                             getCustDictStructures
                                             getFunctions
                                             getIncludes
                                             getTransformations
                                             customerProgsOnly
                                             custNameRange
                                             recursiveIncludeSearch
                                             recursiveFuncSearch.
  endform.                                                                               "retrievePrograms

*  -------------------------------------------------------------------------------------------------------
*    scanForAdditionalProgStuff...
*  -------------------------------------------------------------------------------------------------------
  form scanForAdditionalProgStuff using iLocProgram like iPrograms[]
                                        iLocFunctions like iFunctions[]
                                        value(getTextElements)
                                        value(getMessages)
                                        value(getScreens)
                                        value(getCustDictStructures)
                                        value(getFunctions)
                                        value(getIncludes)
                                        value(getTransformations)
                                        value(customerOnly)
                                        value(customerNameRange)
                                        value(recursiveIncludeSearch)
                                        value(recursiveFuncSearch).

  data: waProgram type tProgram.
  data: waInclude type tInclude.
  data: myTabix type syTabix.

*   Best to find all the includes used in a program first
    if not getIncludes is initial.
      loop at iLocProgram into waProgram.
        myTabix = sy-tabix.
        perform scanForIncludePrograms using waProgram-progName
                                             recursiveIncludeSearch
                                             customerOnly
                                             customerNameRange
                                             waProgram-iIncludes[].

        modify iLocProgram from waProgram index myTabix.
      endloop.
    endif.

*   Once we have a list of all the includes we need to loop round them an select all the other objects
    loop at iLocProgram into waProgram.
      myTabix = sy-tabix.
      perform findProgramDetails using waProgram-progName
                                       waProgram-subc
                                       waProgram-programTitle
                                       waProgram
                                       getTextElements
                                       getMessages
                                       getScreens
                                       getTransformations
                                       getCustDictStructures
                                       customerOnly
                                       customerNameRange.

*     Find any screens
      if not getScreens is initial.
        perform findProgramScreenFlow using waProgram.
      endif.

      loop at waProgram-iIncludes into waInclude.
        perform findProgramDetails using waInclude-includeName
                                         'I'
                                         waInclude-includeTitle
                                         waProgram
                                         getTextElements
                                         getMessages
                                         getScreens
                                         getTransformations
                                         getCustDictStructures
                                         customerOnly
                                         customerNameRange.
      endloop.

      modify iLocProgram from waProgram index myTabix.
    endloop.

*   Now we have all the program includes and details we need to find extra functions
    if not getFunctions is initial.
      loop at iLocProgram into waProgram.
*       Find any functions defined in the code
        perform scanForFunctions using waProgram-progname
                                       waProgram-progname
                                       space
                                       space
                                       customerOnly
                                       customerNameRange
                                       iLocFunctions[].
      endloop.

*     We have a list of all the functions so lets go and find details and other function calls
      perform scanForAdditionalFuncStuff using iLocFunctions[]
                                               recursiveIncludeSearch
                                               recursiveFuncSearch
                                               getIncludes
                                               getFunctions
                                               getCustDictStructures
                                               getMessages
                                               getTransformations
                                               customerOnly
                                               customerNameRange.
    endif.
  endform.                                                                     "scanForAdditionalProgStuff

*  -------------------------------------------------------------------------------------------------------
*    findProgramDetails...
*  -------------------------------------------------------------------------------------------------------
  form findProgramDetails using value(programName)
                                value(programType)
                                      programTitle
                                      waProgram type tProgram
                                value(getTextElements)
                                value(getMessages)
                                value(getScreens)
                                value(getTransformations)
                                value(getCustDictStructures)
                                value(customerOnly)
                                value(customerNameRange).

    perform findProgramOrIncludeTitle using programName
                                            programTitle.

    if not getTextElements is initial.
*     Find the program texts from out of the database.
      perform retrieveProgramTexts using waProgram-iSelectionTexts[]
                                         waProgram-iTextElements[]
                                         programName.
    endif.

*   Search for any GUI texts
    if not getScreens is initial and ( programType = 'M' or programType = '1' ).
      perform retrieveGUITitles using waProgram-iGUITitle[]
                                      programName.
    endif.

*   Find individual messages
    if not getMessages is initial.
      if programType = 'M' or programType = '1'.
        perform findMainMessageClass using programName
                                           waProgram-messageClass.
      endif.

      perform scanForMessages using programName
                                    waProgram-messageClass
                                    waProgram-iMessages[].
    endif.

*   Find any XSLT Transformations
    if not getTransformations is initial.
      perform scanForTransformations using programName
                                           customerOnly
                                           customerNameRange
                                           waProgram-iTransformations[].
    endif.

    if not getCustDictStructures is initial.
      perform scanForTables using programName
                                  customerOnly
                                  customerNameRange
                                  waProgram-iDictStruct[].

      perform scanForLikeOrType using programName
                                      customerOnly
                                      customerNameRange
                                      waProgram-iDictStruct[]
                                      waProgram-iTableTypes[].
    endif.
  endform.                                                                             "findProgramDetails

*  -------------------------------------------------------------------------------------------------------
*    findAllProgramsForAuthor...
*  -------------------------------------------------------------------------------------------------------
  form findAllProgramsForAuthor using iLocProgram like iPrograms[]
                                      rangeProgram like soProgramName[]
                                      rangeAuthor like soAuthor[]
                                      value(custNameRange)
                                      value(alsoModifiedByauthor)
                                      value(customerProgsOnly)
                                      value(soLocPackage) like soPack[].

  data: altCustomerNameRange type string.
  field-symbols: <waProgram> type tProgram.
  data: genFlag type genFlag.
  data: objName type tadir-obj_name.
  data: progName type reposrc-progname.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

*   select by name and author
    if not alsoModifiedByAuthor is initial.
*     Programs modified by author
*     Program to search for is an executable program
      if customerProgsOnly is initial.
*       Select all programs
        select a~progname
               a~subc
               appending corresponding fields of table ilocProgram
               from reposrc as a
               inner join tadir as b
                 on a~progname = b~obj_name
               where a~progname in rangeProgram
                 and a~cnam in rangeAuthor
                 and ( a~subc = '1' or a~subc = 'M' or a~subc = 'S' )
                 and b~pgmid = 'R3TR'
                 and b~object = 'PROG'
                 and b~devclass in soLocPackage.
        select obj_name into objName
          from tadir
          where obj_name in rangeProgram
            and pgmid = 'R3TR'
            and object = 'FUGR'
            and devclass in soLocPackage.
          concatenate 'SAPL' objName into progName.
          select progname subc
            appending corresponding fields of table iLocProgram
            from reposrc
            where progname = progName
              and subc = 'F'
              and cnam in rangeAuthor.
        endselect.
      else.
*       Select only customer specific programs
        select progname
               subc
               appending corresponding fields of table iLocProgram
               from reposrc as a
               inner join tadir as b
                 on a~progname = b~obj_name
               where a~progname  in rangeProgram
                 and ( a~progname like altCustomerNameRange
                       or a~progname like 'Z%'
                       or a~progname like 'Y%'
                       or a~progname like 'SAPMZ%'
                       or a~progname like 'SAPMY%')
                 and a~cnam in rangeAuthor
                 and ( a~subc = '1' or a~subc = 'M' or a~subc = 'S' )
                 and b~pgmid = 'R3TR'
                 and b~object = 'PROG'
                 and b~devclass in soLocPackage.
        select obj_name into objName
          from tadir
          where obj_name in rangeProgram
            and ( obj_name like altCustomerNameRange
                  or obj_name like 'Z%'
                  or obj_name like 'Y%' )
            and pgmid = 'R3TR'
            and object = 'FUGR'
            and devclass in soLocPackage.
          concatenate 'SAPL' objName into progName.
          select progname subc
            appending corresponding fields of table iLocProgram
            from reposrc
            where progname = progName
              and subc = 'F'
              and cnam in rangeAuthor.
        endselect.
      endif.
    else.

*     Programs created by author
      if customerProgsOnly is initial.
*       Select all programs
        select progname
               subc
               appending corresponding fields of table iLocProgram
               from reposrc as a
               inner join tadir as b
                 on a~progname = b~obj_name
               where a~progname in rangeProgram
                 and ( a~subc = '1' or a~subc = 'M' or a~subc = 'S' )
                 and ( a~cnam in rangeAuthor or a~unam in rangeAuthor )
                 and b~pgmid = 'R3TR'
                 and b~object = 'PROG'
                 and b~devclass in soLocPackage.
        select obj_name into objName
          from tadir
          where obj_name in rangeProgram
            and pgmid = 'R3TR'
            and object = 'FUGR'
            and devclass in soLocPackage.
          concatenate 'SAPL' objName into progName.
          select progname subc
            appending corresponding fields of table iLocProgram
            from reposrc
            where progname = progName
              and subc = 'F'
              and ( cnam in rangeAuthor or unam in rangeAuthor ).
        endselect.
      else.
*       Select only customer specific programs
        select a~progname
               a~subc
               appending corresponding fields of table iLocProgram
               from reposrc as a
               inner join tadir as b
                 on a~progname = b~obj_name
               where a~progname in rangeProgram
                 and ( a~progname like altCustomerNameRange
                       or a~progname like 'Z%'
                       or a~progname like 'Y%'
                       or a~progname like 'SAPMZ%'
                       or a~progname like 'SAPMY%')
                 and ( a~subc = '1' or a~subc = 'M' or a~subc = 'S' )
                 and ( a~cnam in rangeAuthor or a~unam in rangeAuthor )
                 and b~pgmid = 'R3TR'
                 and b~object = 'PROG'
                 and b~devclass in soLocPackage.
        select obj_name into objName
          from tadir
          where obj_name in rangeProgram
            and ( obj_name like altCustomerNameRange
                  or obj_name like 'Z%'
                  or obj_name like 'Y%' )
            and pgmid = 'R3TR'
            and object = 'FUGR'
            and devclass in soLocPackage.
          concatenate 'SAPL' objName into progName.
          select progname subc
            appending corresponding fields of table iLocProgram
            from reposrc
            where progname = progName
              and subc = 'F'
              and ( cnam in rangeAuthor or unam in rangeAuthor ).
        endselect.
      endif.
    endif.
  endform.                                                                       "findAllProgramsForAuthor

*  -------------------------------------------------------------------------------------------------------
*    findAllXSLTForAuthor...
*  -------------------------------------------------------------------------------------------------------
  form findAllXSLTForAuthor using ilocTransformations  like iTransformations[]
                                  rangexslt like soxsltname[]
                                  rangeauthor like soauthor[]
                                  value(custnamerange)
                                  value(alsomodifiedbyauthor)
                                  value(customerprogsonly)
                                  value(solocpackage) like sopack[].

  data: altCustomerNameRange type string.
  data: waTransformation like line of iTransformations.
  field-symbols: <waxslt> type tTransformation.
  data: genFlag type genflag.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altcustomernamerange.

*   select by name and author
    if not alsomodifiedbyauthor is initial.
*     Programs modified by author
*     Program to search for is an executable program
      if customerprogsonly is initial.
*       Select all XSLT programs
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    author    in rangeauthor
               and    obj_name  in rangexslt
               and    devclass  in solocpackage.

          append waTransformation to iLocTransformations.
        endselect.

      else.
*       Select only customer specific programs
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    author    in rangeauthor
               and    obj_name  in rangexslt
               and    devclass  in solocpackage
               and ( obj_name like altcustomernamerange
                     or obj_name like 'Z%'
                     or obj_name like 'Y%').

          append waTransformation to iLocTransformations.
        endselect.
      endif.
    else.
*     Programs created by author
      if customerprogsonly is initial.
*       Select all programs
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    author    in rangeauthor
               and    obj_name  in rangexslt
               and    devclass  in solocpackage.

          append waTransformation to iLocTransformations.
        endselect.
      else.
*       Select only customer specific programs
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    author    in rangeauthor
               and    obj_name  in rangexslt
               and    devclass  in solocpackage
               and ( obj_name like altcustomernamerange
                     or obj_name like 'Z%'
                     or obj_name like 'Y%').

          append waTransformation to iLocTransformations.
        endselect.
      endif.
    endif.
  endform.                                                                       "findAllXSLTForAuthor

*  -------------------------------------------------------------------------------------------------------
*    findXsltByWildcard...
*  -------------------------------------------------------------------------------------------------------
  form findXsltByWildcard using iLocTransformations like iTransformations[]
                                    value(rangexslt) like soxsltname[]
                                    value(rangeauthor) like soauthor[]
                                    value(custnamerange)
                                    value(customerprogsonly)
                                    value(solocpackage) like sopack[].

  data: altcustomernamerange type string.
  data: waTransformation like line of ilocTransformations.
  field-symbols: <waTransformation> type tTransformation.
  data: genflag type genflag.

    if customerprogsonly is initial.
*     build up the customer name range used for select statements
      if custnamerange <> '^'.
        concatenate custnamerange '%' into altcustomernamerange.

        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    obj_name  in rangexslt
               and    obj_name  like altcustomernamerange
               and    author    in rangeauthor.

          append waTransformation to iLocTransformations.
        endselect.
      else.
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    obj_name  in rangexslt
               and    author    in rangeauthor.

          append waTransformation to iLocTransformations.
        endselect.
      endif.
    else.
*     Only customer xslt
      if custnamerange <> '^'.
        concatenate custnamerange '%' into altcustomernamerange.

        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    obj_name  in rangexslt
               and    obj_name  like altcustomernamerange
               and    ( obj_name like 'Z%' or
                        obj_name like 'Y%' ).

          append waTransformation to iLocTransformations.
        endselect.
      else.
        select obj_name
               into   waTransformation-xsltname
               from   tadir
               where  pgmid     = 'R3TR'
               and    object    = 'XSLT'
               and    obj_name  in rangexslt
               and    ( obj_name like 'Z%' or
                        obj_name like 'Y%' ).

          append waTransformation to iLocTransformations.
        endselect.
      endif.
    endif.
  endform.                                                                         "findxsltByWildcard

*  -------------------------------------------------------------------------------------------------------
*    checkXsltDoesExist...
*  -------------------------------------------------------------------------------------------------------
  form checkXsltDoesExist using ilocTransformations like iTransformations[]
                                rangexslt like soxsltname[].

  data: waTransformation type tTransformation.

*  -- Make sure XSLT exists
    select single obj_name
           into   waTransformation-xsltname
           from   tadir
           where  pgmid     = 'R3TR'
           and    object    = 'XSLT'
           and    obj_name  in rangexslt.

    if sy-subrc = 0.
      append waTransformation to iLocTransformations.
    endif.
  endform.                                                                          "checkxsltDoesExist

*  -------------------------------------------------------------------------------------------------------
*    checkProgramDoesExist...
*  -------------------------------------------------------------------------------------------------------
  form checkProgramDoesExist using iLocProgram like iPrograms[]
                                   rangeProgram like soProgramName[].

*    Check to see if the program is an executable program
     select progname
            subc
            into corresponding fields of table iLocProgram
            from reposrc
            where progname in rangeProgram
              and ( subc = '1' or
                    subc = 'I' or
                    subc = 'M' or
                    subc = 'S' ).
  endform.                                                                          "checkProgramDoesExist

*  -------------------------------------------------------------------------------------------------------
*    findProgramsByWildcard.. Search in the system for programs
*  -------------------------------------------------------------------------------------------------------
  form checkAndAddTableType using waDictionary-tableName
                                  iLocTableTypes like iTableTypes[].

    select *
           into corresponding fields of table iLocTableTypes
           from dd40l as a
           inner join dd40t as t
              on a~typename = t~typename
           where a~typename = waDictionary-tableName
             and t~ddlanguage eq sy-langu
             and a~as4local = 'A'.
  endform.

*  -------------------------------------------------------------------------------------------------------
*    findProgramsByWildcard.. Search in the system for programs
*  -------------------------------------------------------------------------------------------------------
  form findProgramsByWildcard using iLocProgram like iPrograms[]
                                    value(rangeProgram) like soProgramName[]
                                    value(rangeAuthor) like soAuthor[]
                                    value(custNameRange)
                                    value(customerProgsOnly)
                                    value(soLocPackage) like soPack[].

  data: altCustomerNameRange type string.
  field-symbols: <waProgram> type tProgram.
  data: genFlag type genFlag.

    if customerProgsOnly is initial.
*     build up the customer name range used for select statements
      if custNameRange <> '^'.
        concatenate custNameRange '%' into altCustomerNameRange.

        select progname
               subc
               from reposrc
               appending corresponding fields of table iLocProgram
               where progname  in rangeProgram
                 and progname like altCustomerNameRange
                 and ( subc = '1' or subc = 'M' or subc = 'S' )
                 and ( cnam in rangeAuthor or unam in rangeAuthor ).
      else.
        select progname
               subc
               from reposrc
               appending corresponding fields of table iLocProgram
               where progname  in rangeProgram
                 and ( subc = '1' or subc = 'M' or subc = 'S' )
                 and ( cnam in rangeAuthor or unam in rangeAuthor ).
      endif.
    else.
*     Only customer programs
      if custNameRange <> '^'.
        concatenate custNameRange '%' into altCustomerNameRange.

        select progname
               subc
               from reposrc
               appending corresponding fields of table iLocProgram
               where progname  in rangeProgram
                 and ( progname like altCustomerNameRange
                       or progname like 'Z%'
                       or progname like 'Y%'
                       or progname like 'SAPMZ%'
                       or progname like 'SAPMY%')
                 and ( subc = '1' or subc = 'M' or subc = 'S' )
                 and ( cnam in rangeAuthor or unam in rangeAuthor ).
      else.
        select progname
               subc
               from reposrc
               appending corresponding fields of table iLocProgram
               where progname  in rangeProgram
               and (    progname like 'Z%'
                     or progname like 'Y%'
                     or progname like 'SAPMZ%'
                     or progname like 'SAPMY%')
               and ( subc = '1' or subc = 'M' or subc = 'S' )
               and ( cnam in rangeAuthor or unam in rangeAuthor ).
      endif.
    endif.
  endform.                                                                         "findProgramsByWildcard

*  -------------------------------------------------------------------------------------------------------
*    retrieveProgramTexts... Find the text elements and selection texts for a program
*  -------------------------------------------------------------------------------------------------------
  form retrieveProgramTexts using iLocSelectionTexts like dumiTextTab[]
                                  iLocTextElements like dumiTextTab[]
                                  value(programName).

  data: iTextTable type standard table of tTextTable with header line.
  data: waTexts type tTextTable.
  data: castProgramName(50).

    move programName to castProgramName.

    read textpool castProgramName into iTextTable language pMLang.
    delete iTextTable where key = 'R'.

*   Selection texts.
    loop at iTextTable where id = 'S'.
      move iTextTable-key to waTexts-key.
      move iTextTable-entry to waTexts-entry.
      append waTexts to iLocSelectiontexts.
      clear waTexts.
    endloop.

*   Text elements.
    delete iTextTable where key = 'S'.
    loop at iTextTable where id = 'I'.
      move iTextTable-key to waTexts-key.
      move iTextTable-entry to waTexts-entry.
      append waTexts to iLocTextElements.
    endloop.
  endform.                                                                           "retrieveProgramTexts

*  -------------------------------------------------------------------------------------------------------
*    retrieveGUITitles...  Search for any GUI texts
*  -------------------------------------------------------------------------------------------------------
  form retrieveGUITitles using iLocGUITitle like dumIGUITitle[]
                               value(programName).

    select obj_code
           text
           from d347t
           appending corresponding fields of table iLocGUItitle
           where progname = programName.
  endform.                                                                              "retrieveGUITitles

*  -------------------------------------------------------------------------------------------------------
*     findMainMessageClass... find the message class stated at the top of  program.
*  -------------------------------------------------------------------------------------------------------
  form findMainMessageClass using value(programName)
                                        messageClass.

    select single msgid
                  from trdire into messageClass
                  where report = programName.
  endform.                                                                           "findMainMessageClass

*  -------------------------------------------------------------------------------------------------------
*   retrieveClasses...    find classes and sub objects from SAP DB
*  -------------------------------------------------------------------------------------------------------
  form retrieveClasses using iLocClasses like iClasses[]
                             iLocFunctions like iFunctions[]
                             rangeClass like soClassName[]
                             rangeAuthor like soAuthor[]
                             value(custNameRange)
                             value(alsoModifiedByauthor)
                             value(customerProgsOnly)
                             value(getMessages)
                             value(getTextElements)
                             value(getCustDictStructures)
                             value(getFunctions)
                             value(getIncludes)
                             value(getTransformations)
                             value(recursiveFuncSearch)
                             value(recursiveIncludeSearch)
                             value(recursiveClassSearch)
                             value(language)
                             value(soLocPackage) like soPack[].

  data: waRangeClass like line of rangeClass.
  data: waClass like line of iLocCLasses[].

    if rangeClass[] is initial.
*     We are finding all programs by an author
      perform findAllClassesForAuthor using iLocClasses[]
                                             rangeClass[]
                                             rangeAuthor[]
                                             custNameRange
                                             alsoModifiedByAuthor
                                             customerProgsOnly
                                             language.
    else.
      read table rangeClass index 1 into waRangeClass.
      if waRangeClass-low cs ASTERIX.
        perform findClassesByWildcard using iLocClasses[]
                                            rangeClass[]
                                            rangeAuthor[]
                                            custNameRange
                                            customerProgsOnly
                                            language.
      else.
        perform checkClassDoesExist using iLocClasses[]
                                          rangeClass[].
      endif.
    endif.

*   Check the package
    if not soLocPackage[] is initial.
      loop at iLocClasses into waClass.
        select single obj_name
               from tadir
               into waClass-clsName
               where pgmid = 'R3TR'
                 and object = 'CLAS'
                 and obj_name = waClass-clsName
                 and devclass in soLocPackage[].
        if sy-subrc <> 0.
          delete iLocClasses.
        endif.
      endloop.
    endif.

*   Find extra items
    if not iLocClasses[] is initial.
      perform scanForAdditionalClassStuff using iLocClasses[]
                                                iLocFunctions[]
                                                getTextElements
                                                getMessages
                                                getCustDictStructures
                                                getFunctions
                                                getIncludes
                                                getTransformations
                                                customerProgsOnly
                                                custNameRange
                                                recursiveIncludeSearch
                                                recursiveFuncSearch
                                                recursiveClassSearch
                                                soLocPackage[].
    endif.
  endform.                                                                                "retrieveClasses

*  -------------------------------------------------------------------------------------------------------
*    findAllClassesForAuthor...
*  -------------------------------------------------------------------------------------------------------
  form findAllClassesForAuthor using iLocClass like iClasses[]
                                     rangeClass like soClassName[]
                                     rangeAuthor like soAuthor[]
                                     value(custNameRange)
                                     value(alsoModifiedByauthor)
                                     value(customerClassesOnly)
                                     value(language).

  data: altCustomerNameRange type string.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

*   select by name and author
    if not alsoModifiedByAuthor is initial.
*     Classes modified by author
      if customerClassesOnly is initial.
*       Select all classes
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and langu = language
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).

        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                 and langu = language
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      else.
*       Select only customer specific classes
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and ( clsname like altCustomerNameRange
                       or clsname like 'Z%'
                       or clsname like 'Y%')
                 and langu = language
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).

        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and ( clsname like altCustomerNameRange
                         or clsname like 'Z%'
                         or clsname like 'Y%')
                   and langu = language
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      endif.
    else.
*     Programs created by author
      if customerClassesOnly is initial.
*       Select all classes
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and langu = language
                 and author in rangeAuthor
                 and version = '1'
                 and ( state = '0' or state = '1' ).

        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and langu = language
                   and author in rangeAuthor
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      else.
*       Select only customer specific classes
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and ( clsname like altCustomerNameRange
                       or clsname like 'Z%'
                       or clsname like 'Y%')
                 and langu = language
                 and author in rangeAuthor
                 and version = '1'
                 and ( state = '0' or state = '1' ).

        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and ( clsname like altCustomerNameRange
                         or clsname like 'Z%'
                         or clsname like 'Y%')
                   and langu = language
                   and author in rangeAuthor
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      endif.
    endif.
  endform.                                                                        "findAllClassesForAuthor

*  -------------------------------------------------------------------------------------------------------
*    findClassesByWildcard...  Find classes using a wildcard search
*  -------------------------------------------------------------------------------------------------------
  form findClassesByWildcard using iLocClass like iClasses[]
                                   rangeClass like soClassName[]
                                   value(rangeAuthor) like soAuthor[]
                                   value(custNameRange)
                                   value(customerClassesOnly)
                                   value(language).

  data: altCustomerNameRange type string.

    if customerClassesOnly is initial.
*     Searching for customer and SAP classes
      if custNameRange <> '^'.
*       build up the customer name range used for select statements
        concatenate custNameRange '%' into altCustomerNameRange.

        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and clsname like custNameRange
                 and langu = language
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).
        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and clsname like custNameRange
                   and langu = language
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      else.
*       Searching using normal name ranges
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and langu = language
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).
        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and langu = language
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      endif.
    else.
*     searching for only customer classes
      if custNameRange <> '^'.
*       build up the customer name range used for select statements
        concatenate custNameRange '%' into altCustomerNameRange.

        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and clsname like custNameRange
                 and langu = language
                 and ( clsname like 'ZC%' or clsname like 'YC%' )
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).
        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and langu = language
                   and ( clsname like 'ZC%' or clsname like 'YC%' )
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      else.
*       Searching using normal name ranges
        select clsname descript msg_id
               from vseoclass
               appending corresponding fields of table ilocClass
               where clsname in rangeClass
                 and ( clsname like 'ZC%' or clsname like 'YC%' )
                 and ( author in rangeAuthor or changedby in rangeAuthor )
                 and version = '1'
                 and ( state = '0' or state = '1' ).
        if sy-subrc <> 0.
          select clsname descript msg_id
                 from vseoclass
                 appending corresponding fields of table ilocClass
                 where clsname in rangeClass
                   and ( clsname like 'ZC%' or clsname like 'YC%' )
                   and ( author in rangeAuthor or changedby in rangeAuthor )
                   and version = '0'
                   and ( state = '0' or state = '1' ).
        endif.
      endif.
    endif.
  endform.                                                                          "findClassesByWildcard

*  -------------------------------------------------------------------------------------------------------
*    checkClassDoesExist...
*  -------------------------------------------------------------------------------------------------------
  form checkClassDoesExist using iLocClass like iClasses[]
                                 rangeClass like soClassName[].

  data: waClass type tClass.

    select single clsname descript msg_id
           from vseoclass
           into corresponding fields of waClass
           where clsname in rangeClass
             and version = '1'
             and ( state = '0' or state = '1' ).

    if sy-subrc <> 0.
      select single clsname descript msg_id
           from vseoclass
           into corresponding fields of waClass
           where clsname in rangeClass
             and version = '0'
             and ( state = '0' or state = '1' ).
    endif.

     if not waClass-clsname is initial.
       append waClass to iLocClass.
     endif.
  endform.                                                                            "checkClassDoesExist

*  -------------------------------------------------------------------------------------------------------
*    scanForAdditionalClassStuff...
*  -------------------------------------------------------------------------------------------------------
  form scanForAdditionalClassStuff using iLocClasses like iClasses[]
                                         iLocFunctions like iFunctions[]
                                         value(getTextElements)
                                         value(getMessages)
                                         value(getCustDictStructures)
                                         value(getFunctions)
                                         value(getIncludes)
                                         value(getTransformations)
                                         value(customerOnly)
                                         value(customerNameRange)
                                         value(recursiveIncludeSearch)
                                         value(recursiveFuncSearch)
                                         value(recursiveClassSearch)
                                         value(soLocPackage) like soPack[].

  data: waClass type tClass.
  data: waMethod type tMethod.
  data: myTabix type syTabix.
  data: scanningForClasses type abap_bool value FALSE.
  data: classNewLines type i value 0.
  data: classCurrentLines type i value 0.

    loop at iLocClasses into waClass where scanned is initial.
*    Once we have a list of all the classes we need to loop round them an select all the other objects
      myTabix = sy-tabix.
      perform findClassDetails using waClass-clsName
                                     waClass
                                     iLocFunctions[]
                                     getTextElements
                                     getMessages
                                     getFunctions
                                     getCustDictStructures
                                     getTransformations
                                     customerOnly
                                     customerNameRange.

*     Set the scanned class so we do not check them again when running recursively.
      waClass-scanned = 'X'.
      modify iLocClasses from waClass index myTabix.
    endloop.

*   Now we have all the classes and details we need to find extra classes
    if not recursiveClassSearch is initial.
      classCurrentLines = lines( iLocClasses ).
      loop at iLocClasses into waClass.
*       Don't try and find any other details for an exception class
        if ( waClass-clsName ns 'ZCX_' or waClass-clsName ns 'CX_'  ).
*         Find any classes defined in the main class definition
          perform scanForClasses using waClass-privateClassKey
                                       waClass-clsname
                                       customerOnly
                                       customerNameRange
                                       iLocClasses[]
                                       soLocPackage[].

          perform scanForClasses using waClass-publicClassKey
                                       waClass-clsname
                                       customerOnly
                                       customerNameRange
                                       iLocClasses[]
                                       soLocPackage[].

          perform scanForClasses using waClass-protectedClassKey
                                       waClass-clsname
                                       customerOnly
                                       customerNameRange
                                       iLocClasses[]
                                       soLocPackage[].

          loop at waClass-iMethods into waMethod.
*           Find any classes defined in any of the methods
            perform scanForClasses using waMethod-methodKey
                                         waClass-clsname
                                         customerOnly
                                         customerNameRange
                                         iLocClasses[]
                                         soLocPackage[].
          endloop.
        endif.
      endloop.

*     We have a list of all the classes so lets go and find their details
      classNewLines = lines( iLocClasses ).
      if classNewLines > classCurrentLines.
        perform scanForAdditionalClassStuff using iLocClasses[]
                                                  iLocFunctions[]
                                                  getTextElements
                                                  getMessages
                                                  getCustDictStructures
                                                  getFunctions
                                                  getIncludes
                                                  getTransformations
                                                  customerOnly
                                                  customerNameRange
                                                  recursiveIncludeSearch
                                                  recursiveFuncSearch
                                                  recursiveClassSearch
                                                  soLocPackage[].
      endif.
    endif.
  endform.                                                                   "scanForAdditionalClassStuff

*  -------------------------------------------------------------------------------------------------------
*    findClassDetails...
*  -------------------------------------------------------------------------------------------------------
  form findClassDetails using value(className)
                                    waClass type tClass
                                    iLocFunctions like iFunctions[]
                                    value(getTextElements)
                                    value(getMessages)
                                    value(getFunctions)
                                    value(getCustDictStructures)
                                    value(getTransformations)
                                    value(customerOnly)
                                    value(customerNameRange).

  data: iEmptySelectionTexts type standard table of tTextTable.
  data: myTabix type syTabix.
  data: waMethod type tMethod.
  data: rnBlankAuthor like soAuthor[].
  data: rnBlankPackage like soPack[].
  data: waInterface type tInterface.

*   Build up the keys we will use for finding data
    perform buildClassKeys using waClass.

    if waClass-descript is initial.
      perform findClassDescription using className
                                         waClass-descript.
    endif.

*   Find the class attributes.
    select single exposure msg_id state clsfinal r3release
                  from vseoclass
                  into (waClass-exposure, waClass-msg_id, waClass-state,
                        waClass-clsfinal, waClass-r3release)
                  where clsName = waClass-clsName.

*   Don't try and find any other details for an exception class
    if ( waClass-clsName cs 'ZCX_' or waClass-clsName cs 'CX_'  ).
*     Exception texts
      perform findExceptionTexts using waClass-publicClassKey
                                       waClass-iConcepts[].
      waClass-scanned = 'X'.
    else.
      if not getTextElements is initial.
*       Find the class texts from out of the database.
        perform retrieveProgramTexts using iEmptySelectionTexts[]
                                           waClass-iTextElements[]
                                           waClass-textElementKey.
      endif.

*     Find any declared dictionary structures
      if not getCustDictStructures is initial.
        perform scanForTables using waClass-privateClassKey
                                    customerOnly
                                    customerNameRange
                                    waClass-iDictStruct[].

        perform scanForTables using waClass-publicClassKey
                                    customerOnly
                                    customerNameRange
                                    waClass-iDictStruct[].

        perform scanForTables using waClass-protectedClassKey
                                    customerOnly
                                    customerNameRange
                                    waClass-iDictStruct[].

        perform scanForTables using waClass-typesClassKey
                                    customerOnly
                                    customerNameRange
                                    waClass-iDictStruct[].

        perform scanForLikeOrType using waClass-privateClassKey
                                        customerOnly
                                        customerNameRange
                                        waClass-iDictStruct[]
                                        waClass-iTableTypes[].

        perform scanForLikeOrType using waClass-publicClassKey
                                        customerOnly
                                        customerNameRange
                                        waClass-iDictStruct[]
                                        waClass-iTableTypes[].

        perform scanForLikeOrType using waClass-protectedClassKey
                                        customerOnly
                                        customerNameRange
                                        waClass-iDictStruct[]
                                        waClass-iTableTypes[].

        perform scanForLikeOrType using waClass-typesClassKey
                                        customerOnly
                                        customerNameRange
                                        waClass-iDictStruct[]
                                        waClass-iTableTypes[].
      endif.

*     Find all the interfaces used in this class
      perform findClassInterfaces using className
                                        waClass-iInterfaces[].

*     Find all the methods defined by the interfaces
      perform findInterfaceMethods using className
                                         waClass-iInterfaces[]
                                         waClass-iMethods[].

*     Methods
*     Find all the methods for this class
      perform findClassMethods using className
                                     waClass-iMethods[].

      loop at waClass-iMethods[] into waMethod.
        myTabix = sy-tabix.
*       Find individual messages
        if not getMessages is initial.
          perform scanForMessages using waMethod-methodKey
                                        waClass-msg_id
                                        waClass-iMessages[].
        endif.

        if not getCustDictStructures is initial.
*         Find any declared dictionary structures
          perform scanForTables using waMethod-methodKey
                                      customerOnly
                                      customerNameRange
                                      waClass-iDictStruct[].

          perform scanForLikeOrType using waMethod-methodKey
                                          customerOnly
                                          customerNameRange
                                          waClass-iDictStruct[]
                                          waClass-iTableTypes[].
        endif.

        if not getfunctions is initial.
          perform scanForFunctions using waMethod-methodKey
                                         waClass-clsName
                                         space
                                         space
                                         customerOnly
                                         customerNameRange
                                         iLocFunctions[].
        endif.

*       Find any XSLT Transformations
        if not getTransformations is initial.
          perform scanForTransformations using waMethod-methodKey
                                               customerOnly
                                               customerNameRange
                                               waClass-iTransformations[].
        endif.

        modify waClass-iMethods from waMethod index myTabix.
      endloop.

*     If the class has specified a message class but were unable to find any specific messages
*     then retrieve the whole message class.
      if ( not waClass-msg_id is initial and waClass-iMessages[] is initial ).
        perform retrieveMessageClass using waClass-iMessages[]
                                           rnBlankAuthor[]
                                           waClass-msg_id
                                           pMLang
                                           ''
                                           rnBlankPackage[].
      endif.
    endif.
  endform.                                                                               "findClassDetails

*  ----------------------------------------------------------------------------------------------------------------------
*   Find all interface methods used by the class
*  ----------------------------------------------------------------------------------------------------------------------
  form findInterfaceMethods using value(className)
                                        iLocInterfaces like dumiInterfaces[]
                                        iLocMethods like dumiMethods[].

  types: begin of tIntMethod,
           interfaceName like vseomethod-clsname,
           cmpName like vseomethod-cmpname,
           exposure like vseomethod-exposure,
         end of tIntMethod.
  data: waMethod type tMethod.
  data: waInterface like line of iLocInterfaces.
  data: iIntMethod type standard table of tIntMethod with header line.

    loop at iLocInterfaces into waInterface.
       select a~clsname as interfaceName
              a~cmpName
              b~exposure
              appending table iIntMethod
              from seocompo as a
              inner join seocompodf as b
                on a~clsname = b~clsname
                and a~cmpname = b~cmpname
              where a~clsname = waInterface-interfaceName.
    endloop.

    loop at iIntMethod.
      concatenate iIntMethod-interfaceName '~' iIntMethod-cmpname into waMethod-cmpname.
      waMethod-exposure = iIntMethod-exposure.

      select single descript
             from seocompotx
             into waMethod-descript
             where clsname = iIntMethod-interfaceName
               and cmpname = iIntMethod-cmpname
               and langu = pMLang.

      perform findMethodKey using className
                                  waMethod-cmpName
                                  waMethod-methodKey.

      if not waMethod-methodKey is initial.
        append waMethod to iLocMethods.
      endif.

      clear waMethod.
    endloop.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*     Check to see if their are any interfaces being used by this class
*  ----------------------------------------------------------------------------------------------------------------------
  form findClassInterfaces using value(className)
                                       iLocInterfaces like dumiInterfaces[].

  data: isInheritance type abap_bool value abap_true.
  data: iInterfaces type standard table of vseoimplem with header line.
  data: waLocInterface like line of iLocInterfaces.

    select * from vseoimplem
             into table iInterfaces
             where clsname = className.

    while isInheritance = ABAP_TRUE.
      perform findClassParent using className
                                    className.

      if className is initial.
        isInheritance = ABAP_FALSE.
      else.
        select * from vseoimplem
                 appending table iInterfaces
                 where clsname = className.
      endif.
    endwhile.

    if not iInterfaces[] is initial.
      sort iInterfaces ascending by refclsname.
      delete adjacent duplicates from iInterfaces comparing refclsname.

      loop at iInterfaces.
        waLocInterface-interfaceName = iInterfaces-refclsname.
        append wALocInterface to iLocInterfaces.
      endloop.
    endif.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*    Find the parent of the current class
*  ----------------------------------------------------------------------------------------------------------------------
  form findClassParent using value(className)
                                   parentClassName.

  data: waInheritance type seor_inheritance_r.
  data: clskey type seoclskey.

    clskey-clsname = className.

      call function 'SEO_INHERITANC_READ'
       exporting
         clskey                   = clskey
         version                  = seoc_version_active
       importing
         inheritance              = waInheritance
*         redefinitions            = redefinitions
       exceptions
         class_not_existing       = 1.

     parentClassName = waInheritance-refclsname.
  endform.

*  -------------------------------------------------------------------------------------------------------
*    buildClassKeys...   Finds the title text of a class.
*  -------------------------------------------------------------------------------------------------------
  form buildClassKeys using waClass type tClass.

  data: classNameLength type i.
  data: loops type i.

    classNameLength = strlen( waClass-clsName ).

    cl_oo_classname_service=>get_pubsec_name( exporting clsname = waClass-clsName
                                              receiving result = waClass-publicClassKey ).

    cl_oo_classname_service=>get_prisec_name( exporting clsname = waClass-clsName
                                              receiving result = waClass-privateClassKey ).

    cl_oo_classname_service=>get_prosec_name( exporting clsname = waClass-clsName
                                              receiving result = waClass-protectedClassKey ).


*   Text element key - length of text element key has to be 32 characters.
    loops = 30 - classNameLength.
    waClass-textElementKey = waClass-clsName.
    do loops times.
      concatenate waClass-textElementKey '=' into waClass-textElementKey.
    enddo.
*   Save this for later.
    concatenate waClass-textElementKey 'CP' into waClass-textElementKey.

*   Types Class key - length of class name has to be 32 characters.
    loops = 30 - classNameLength.
    waClass-typesClassKey = waClass-clsName.
    do loops times.
      concatenate waClass-typesClassKey '=' into waClass-typesClassKey.
    enddo.
*   Save this for later
    concatenate waClass-typesClassKey 'CT' into waClass-typesClassKey.
  endform.                                                                                 "buildClassKeys

*  -------------------------------------------------------------------------------------------------------
*    findClassDescription...   Finds the title text of a class.
*  -------------------------------------------------------------------------------------------------------
  form findClassDescription using value(className)
                                        titleText.

    select single descript
                  from vseoclass
                  into titleText
                  where clsname = className
                    and langu = pMLang.
    if sy-subrc <> 0.
      select single descript
                    from vseoclass
                    into titleText
                    where clsname = className.
    endif.
  endform.                                                                           "findClassDescription

*  -------------------------------------------------------------------------------------------------------
*    findExceptionTexts...   Fiond the texts of an exception class.
*  -------------------------------------------------------------------------------------------------------
  form findExceptionTexts using publicClassKey
                                iConcepts like dumiConcepts[].

  data: castClassName type program.
  data: iTempLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iKeywords type standard table of text20 with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: waTokens type stokes.
  data: waCurrentToken type stokes.
  data: waConcept like line of iConcepts.
  data: tokenLength type i.
  data: myRow type i.

    castClassName = publicClassKey.
    read report castClassName into iTempLines.

    append 'CONSTANTS' to iKeywords.
    scan abap-source iTempLines tokens into iTokens statements into iStatements keywords from iKeywords.

    delete iTokens where str = 'CONSTANTS'.
    delete iTokens where str = 'VALUE'.
    delete iTokens where str = 'TYPE'.

    loop at iTokens into waTokens where str = 'SOTR_CONC'.
*     The loop before holds the constant name
      myRow = sy-tabix - 1.
      read table iTokens index myRow into waCurrentToken.
      waConcept-constName = waCurrentToken-str.

*     The loop after holds the constant name
      myRow = myRow + 2.
      read table iTokens index myRow into waCurrentToken.
      tokenLength = strLen( waCurrentToken-str ).
      if tokenLength = 34.
*       Most likely an exception text.
        replace all occurrences of '''' in waCurrentToken-str with ' ' .
        waConcept-concept = waCurrentToken-str.
        append waConcept to iConcepts.
      endif.
    endloop.
  endform.

*  -------------------------------------------------------------------------------------------------------
*    findClassMethods...   Finds the methods of a class.
*  -------------------------------------------------------------------------------------------------------
  form findClassMethods using value(className)
                              iLocMethods like dumiMethods[].

  data: iMethods type standard table of tMethod with header line.
  data: iRedefinedMethods type standard table of seoredef with header line.
  data: originalClassName type seoclsname.
  data: waMethod like line of iMethods.

    select cmpName descript exposure
           from vseomethod
           into corresponding fields of table iMethods
             where clsname = className
               and version = '1'
               and langu = pMLang
               and ( state = '0' or state = '1' ).

    if sy-subrc <> 0.
      select cmpName descript exposure
             from vseomethod
             into corresponding fields of table iMethods
             where clsname = className
               and version = '0'
               and langu = pMLang
               and ( state = '0' or state = '1' ).
    endif.

     select *
            from seoredef
            into table iRedefinedMethods
            where clsname = className
              and version = '1'.

*    For Each method we must find the original class the method was created in
     loop at iRedefinedMethods.
       perform findRedefinitionClass using iRedefinedMethods-refclsname
                                           iRedefinedMethods-mtdname
                                           originalClassName.

       waMethod-cmpname = iRedefinedMethods-mtdname.

       select single descript exposure
           from vseomethod
           into corresponding fields of  waMethod
             where clsname = originalClassName
               and cmpname = iRedefinedMethods-mtdname
               and version = '1'
               and langu = pMLang
               and ( state = '0' or state = '1' ).

       concatenate `Redefined: ` waMethod-descript into waMethod-descript.
       append waMethod to iMethods.
     endloop.

*   Find the method key so that we can acces the source code later
    loop at iMethods.
      perform findMethodKey using className
                                  iMethods-cmpName
                                  iMethods-methodKey.
      append iMethods to iLocMethods.
    endloop.
  endform.                                                                               "findClassMethods

*  -------------------------------------------------------------------------------------------------------
*   findRedefinitionClass... find the original class the method was redefined from
*  -------------------------------------------------------------------------------------------------------
  form findRedefinitionClass using value(redefinedClassName) type seoclsname
                                   value(methodName) type seocpdname
                                         originalClassName type seoclsname.

  data: waRedef type seoredef.

    select single *
           from seoredef
           into waRedef
           where refclsName = redefinedClassName
             and mtdname = methodName.

    if sy-subrc = 0.
*     There is a higher class still.
      originalClassName = waRedef-refclsName.
      perform findRedefinitionClassRecur using waRedef-refclsname
                                               waRedef-mtdname
                                               originalClassName.
    else.
*     We are at the higher class.
      originalClassName = waRedef-refclsName.
    endif.
  endform.

*  -------------------------------------------------------------------------------------------------------
*   findRedefinitionClassRecur... Recursively find the original class the method was redefined from
*  -------------------------------------------------------------------------------------------------------
  form findRedefinitionClassRecur using value(redefinedClassName) type seoclsname
                                   value(methodName) type seocpdname
                                         originalClassName type seoclsname.

  data: waRedef type seoredef.

    select single *
           from seoredef
           into waRedef
           where clsName = redefinedClassName
             and mtdname = methodName.

    if sy-subrc = 0.
*     There is a higher class still.
      originalClassName = waRedef-refclsName.
      perform findRedefinitionClassRecur using waRedef-refclsname
                                               waRedef-mtdname
                                               originalClassName.
    endif.
  endform.

*  -------------------------------------------------------------------------------------------------------
*   findMethodKey... find the unique key which identifes this method
*  -------------------------------------------------------------------------------------------------------
  form findMethodKey using value(className)
                           value(methodName)
                                 methodKey.

  data: methodID type seocpdkey.
  data: locMethodKey type program.

    methodID-clsname = className.
    methodID-cpdName = methodName.

    cl_oo_classname_service=>get_method_include( exporting mtdkey = methodID
                                                 receiving result = locMethodKey
                                                 exceptions class_not_existing = 1
                                                            method_not_existing = 2 ).

    methodKey = locMethodKey.
  endform.                                                                                  "findMethodKey

*  -------------------------------------------------------------------------------------------------------
*   scanForMessages... Search each program for messages
*  -------------------------------------------------------------------------------------------------------
  form scanForMessages using value(programName)
                             value(mainMessageClass)
                                   iLocMessages like iMessages[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: iKeywords type standard table of text20 with header line.
  data: waMessage type tMessage.
  data: waMessageComparison type tMessage.
  data: watokens type stokes.
  data: nextLine type i.
  data: stringLength type i value 0.
  data: workingOnMessage type abap_bool value FALSE.
  data: castProgramName type program.

*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iIncludeLines.

    append MESSAGE to iKeywords.
    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements keywords from iKeywords.

    clear iIncludeLines[].

    loop at iTokens.
      if iTokens-str = MESSAGE.
        workingOnMessage = TRUE.
        continue.
      endif.

      if workingOnMessage = TRUE.
        stringLength = strlen( iTokens-str ).

*       Message declaration 1
        if stringLength = 4 and iTokens-str+0(1) ca sy-abcde.
          waMessage-msgnr = iTokens-str+1(3).
          waMessage-arbgb = mainMessageClass.
        else.
          if iTokens-str cs '''' or iTokens-str cs '`'.
*           Message declaration 2
            translate iTokens-str using ''' '.
            translate iTokens-str using '` '.
            condense iTokens-str.
            shift iTokens-str left deleting leading space.
            waMessage-text = iTokens-str.
            waMessage-arbgb = 'Hard coded'.
          else.
            if iTokens-str = 'ID'.
*             Message declaration 3
              nextLine = sy-tabix + 1.
              read table iTokens index nextLine into waTokens.
              translate waTokens-str using ''' '.
              condense iTokens-str.
              shift waTokens-str left deleting leading space.
              if not waTokens-str = 'SY-MSGID'.
                waMessage-arbgb = waTokens-str.

                nextLine = nextLine + 4.
                read table iTokens index nextLine into waTokens.
                translate waTokens-str using ''' '.
                condense waTokens-str.
                shift waTokens-str left deleting leading space.
                waMessage-msgnr = waTokens-str.
              else.
                workingOnMessage = FALSE.
              endif.
            else.
              if stringLength >= 5 and iTokens-str+4(1) = '('.
*                Message declaration 4
                 waMessage-msgnr = iTokens-str+1(3).
                 shift iTokens-str left up to '('.
                 replace '(' into iTokens-str with space.
                 replace ')' into iTokens-str with space.
                 condense iTokens-str.
                 waMessage-arbgb = iTokens-str.
              endif.
            endif.
          endif.
        endif.

*        find the message text
         if not waMessage-arbgb is initial and not waMessage-msgnr is initial and waMessage-text is initial.
           select single text
                         from t100
                         into waMessage-text
                         where sprsl = pMLang
                           and arbgb = waMessage-arbgb
                           and msgnr = waMessage-msgnr.
         endif.

*        Append the message
         if not waMessage is initial.
           if not waMessage-text is initial.
*            Don't append the message if we already have it listed
             read table iLocMessages with key arbgb = waMessage-arbgb
                                              msgnr = waMessage-msgnr
                                              into waMessageComparison.
             if sy-subrc <> 0.
               append waMessage to iLocMessages.
             endif.
           endif.
           clear waMessage.
           workingOnMessage = FALSE.
         endif.
       endif.
     endloop.
  endform.                                                                                 "scanForMessages

*  -------------------------------------------------------------------------------------------------------
*   scanForTables... Search each program for dictionary tables
*  -------------------------------------------------------------------------------------------------------
  form scanForTables using value(programName)
                           value(customerOnly)
                           value(customerNameRange)
                                 iLocDictionary like iDictionary[].

  data: iIncludeLines type standard table of string with header line.
  data: iTokens type standard table of stokes with header line.
  data: iStatements type standard table of sstmnt with header line.
  data: iKeywords type standard table of text20 with header line.
  data: waDictionary type tDictTable.
  data: waDictionaryComparison type tDictTable.
  data: castProgramName type program.

*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iIncludeLines.

    append TABLES to iKeywords.

    scan abap-source iIncludeLines tokens into iTokens with includes statements into iStatements keywords from iKeywords.
    clear iIncludeLines[].

    sort iTokens ascending by str.
    delete iTokens where str = TABLES.

    loop at iTokens.
      if not CustomerOnly is initial.
        try.
          case iTokens-str+0(1).
            when 'Y' or 'Z' or customerNameRange.
            when others.
              continue.
          endcase.
*          if ( iTokens-str+0(1) <> 'Y' or iTokens-str+0(1) <> 'Z' or iTokens-str ns customerNameRange ).
*            continue.
*          endif.
        catch cx_sy_range_out_of_bounds into objRuntimeError.
        endtry.
      endif.

      waDictionary-tablename = iTokens-str.
*     Don't append the object if we already have it listed
      read table iLocDictionary into waDictionaryComparison with key tablename = waDictionary-tableName.
      if sy-subrc <> 0.
        perform findTableDescription using waDictionary-tablename
                                           waDictionary-tableTitle.

        perform findTableDefinition using waDictionary-tableName
                                          waDictionary-iStructure[].

        append waDictionary to iLocDictionary.
      endif.
    endloop.
  endform.                                                                                 "scanForTables

*  -------------------------------------------------------------------------------------------------------
*    findProgramScreenFlow...
*  -------------------------------------------------------------------------------------------------------
  form findProgramScreenFlow using waProgram type tProgram.

  data: iFlow type standard table of tScreenFlow with header line.

    call function 'DYNPRO_PROCESSINGLOGIC'
         exporting
              rep_name  = waProgram-progname
         tables
              scr_logic = iFlow.

    sort iFlow ascending by screen.
    delete adjacent duplicates from iFlow comparing screen.
    if waProgram-subc <> 'M'.
      delete iFlow where screen >= '1000' and screen <= '1099'.
    endif.

    loop at iFlow.
      append iFlow to waProgram-iScreenFlow.
    endloop.
  endform.                                                                          "findProgramScreenFlow

*  ----------------------------------------------------------------------------------------------------------------------
*    findMainFunctionInclude...  Find the main include that contains the source code
*  ----------------------------------------------------------------------------------------------------------------------
  form findMainFunctionInclude using value(programName)
                                     value(functionGroup)
                                     value(functionIncludeNo)
                                           functionIncludeName.

  data: namespace TYPE string,
        iResults type match_result_tab,
        waResult type match_result,
        startingPosition type i.

      find all occurrences of '/' in functionGroup results iResults.
      if sy-subrc = 0.
        read table iResults index sy-tfill into waResult.
        startingPosition = waResult-offset + 1.
        namespace = functiongroup+0(startingPosition).
        functionGroup = functionGroup+startingPosition.
      endif.

      concatenate namespace 'L' functionGroup 'U' functionIncludeNo into functionIncludeName.
  endform.                                                                                       "findMainFunctionInclude

*  ----------------------------------------------------------------------------------------------------------------------
*    findFunctionTopInclude...  Find the top include for the function group
*  ----------------------------------------------------------------------------------------------------------------------
  form findFunctionTopInclude using value(programName)
                                    value(functionGroup)
                                          topIncludeName.

  data: namespace TYPE string,
        iResults type match_result_tab,
        waResult type match_result,
        startingPosition type i.

      find all occurrences of '/' in functionGroup results iResults.
      if sy-subrc = 0.
        read table iResults index sy-tfill into waResult.
        startingPosition = waResult-offset + 1.
        namespace = functiongroup+0(startingPosition).
        functionGroup = functionGroup+startingPosition.
      endif.

      concatenate namespace 'L' functionGroup 'TOP' into topIncludeName.
  endform.                                                                                        "findFunctionTopInclude

*  -------------------------------------------------------------------------------------------------------
*    findFunctionScreenFlow...
*  -------------------------------------------------------------------------------------------------------
  form findFunctionScreenFlow using waFunction type tFunction.

  data: iFlow type standard table of tScreenFlow with header line.

    call function 'DYNPRO_PROCESSINGLOGIC'
         exporting
              REP_NAME  = waFunction-progName
         tables
              SCR_LOGIC = iFlow.

    sort iFlow ascending by screen.
    delete adjacent duplicates from iFlow comparing screen.

    loop at iFlow.
      append iFlow to waFunction-iScreenFlow.
    endloop.
  endform.                                                                          "findFunctionScreenFlow

*  -------------------------------------------------------------------------------------------------------
*   scanForLikeOrType... Look for any dictionary objects referenced by a like or type statement
*  -------------------------------------------------------------------------------------------------------
  form scanForLikeOrType using value(programName)
                               value(customerOnly)
                               value(customerNameRange)
                               iLocDictionary like iDictionary[]
                               iLocTableTypes like iTableTypes[].

  data iLines type standard table of string with header line.
  data: head type string.
  data: tail type string.
  data: junk type string.
  data: lineType type string.
  data: lineLength type i value 0.
  data: endOfLine type abap_bool value TRUE.
  data: waDictionary type tDictTable.
  data: waDictionaryComparison type tDictTable.
  data: waLine type string.
  data: castProgramName type program.


*   Read the program code from the textpool.
    castProgramName = programName.
    read report castProgramName into iLines.

    loop at iLines into waLine.
*     Find custom tables.
      lineLength = strLen( waLine ).
      if lineLength > 0.
        if waLine(1) = ASTERIX.
          continue.
        endif.

        translate waLine to upper case.

*       Determine the lineType.
        if endOfLine = TRUE.
          shift waLine up to LIKE.
          if sy-subrc = 0.
            lineType = LIKE.
          else.
            shift waLine up to TYPE.
            if sy-subrc = 0.
              find 'BEGIN OF' in waLine.
              if sy-subrc <> 0.
                find 'END OF' in waLine.
                if sy-subrc <> 0.
                  find 'VALUE' in waLine.
                  if sy-subrc <> 0.
                    lineType = TYPE.
                  endif.
                endif.
              endif.
            else.
              shift waLine up to INCLUDE.
              if sy-subrc = 0.
                split waLine at space into junk iLines.
              endif.

              shift waLine up to STRUCTURE.
              if sy-subrc = 0.
                lineType = STRUCTURE.
              else.
                continue.
              endif.
            endif.
          endif.
        else.
          lineType = COMMA.
        endif.

        case linetype.
          when LIKE or TYPE or STRUCTURE.
*           Work on the appropriate lineType
            shift waLine up to space.
            shift waLine left deleting leading space.
            if waLine cs TABLE.
              split waLine at TABLE into head tail.
              split tail at 'OF' into head tail.
              waLine = tail.
              shift waLine left deleting leading space.
              replace all occurrences of 'WITH HEADER LINE' in waLine with ''.
            endif.

*           Are we only to download SAP dictionary structures.
            if not CustomerOnly is initial.
              try.
                if waLine+0(1) = 'Y' or waLine+0(1) = 'Z' or waLine cs customerNameRange.
                else.
                  lineType = ''.
                  continue.
                endif.
                catch cx_sy_range_out_of_bounds into objRuntimeError.
              endtry.
            endif.

            if waLine cs COMMA.
              split waLine at COMMA into head tail.
              if waLine cs DASH.
                split head at DASH into head tail.
              endif.
              if waLine cs OCCURS.
                split waLine at SPACE into head tail.
              endif.
            else.
              if waLine cs PERIOD.
                split waLine at PERIOD into head tail.
                if waLine cs DASH.
                  split head at DASH into head tail.
                endif.
                if waLine cs OCCURS.
                  split waLine at SPACE into head tail.
                endif.
              else.
                split waLine at SPACE into head tail.
                if waLine cs DASH.
                  split head at DASH into head tail.
                endif.
              endif.
            endif.

            if not head is initial.
              waDictionary-tableName = head.
*             Don't append the object if we already have it listed
              read table iLocDictionary into waDictionaryComparison
                                        with key tablename = waDictionary-tableName.
              if sy-subrc <> 0.
                perform findTableDescription using waDictionary-tablename
                                                   waDictionary-tableTitle.

                perform findTableDefinition using waDictionary-tableName
                                                  waDictionary-iStructure[].

*               Only append if the item is a table and not a structure or data element
                if waDictionary-iStructure[] is initial.
*                 Not a table, but is it a table type?
                  perform checkandAddTableType using waDictionary-tableName
                                                     iLocTableTypes[].
                else.
*                 It is a table
                  append waDictionary to iLocDictionary.
                endif.
              endif.
              clear waDictionary.
            endif.

            lineType = ''.
        endcase.
      endif.
    endloop.
  endform.                                                                              "scanForLikeOrType

*  -------------------------------------------------------------------------------------------------------
*    displayStatus...
*  -------------------------------------------------------------------------------------------------------
  form displayStatus using value(message)
                           value(delay).

    call function 'SAPGUI_PROGRESS_INDICATOR'
         exporting
              percentage = 0
              text       = message
         exceptions
              others     = 1.

    if delay > 0.
      wait up to delay seconds.
    endif.
  endform.                                                                                  "displayStatus

*  -------------------------------------------------------------------------------------------------------
*    removeLeadingZeros...
*  -------------------------------------------------------------------------------------------------------
  form removeLeadingZeros changing myValue.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
         exporting
              INPUT   = myValue
        importing
             OUTPUT  = myValue
         exceptions
              others  = 1.
  endform.                                                                             "removeLeadingZeros

*  -------------------------------------------------------------------------------------------------------
*   determineFrontendOPSystem.... Determine the frontend operating system type.
*  -------------------------------------------------------------------------------------------------------
  form determineFrontendOPSystem using separator
                                       operatingSystem.

  data: platformID type i value 0.

    create object objFile.

    call method objFile->get_platform receiving platform = platformID
                                      exceptions cntl_error = 1
                                      error_no_gui = 2
                                      not_supported_by_gui = 3.
    case platformID.
      when objFile->PLATFORM_WINDOWS95
           or objFile->PLATFORM_WINDOWS98
           or objFile->PLATFORM_NT351
           or objFile->PLATFORM_NT40
           or objFile->PLATFORM_NT50
           or objFile->PLATFORM_MAC
           or objFile->PLATFORM_OS2
           or 14.      "XP
        separator = '\'.
        operatingSystem = NON_UNIX.
      when others.
        separator = '/'.
        operatingSystem = UNIX.
    endcase.
  endform.                                                                      "determineFrontendOpSystem

*  -------------------------------------------------------------------------------------------------------
*   determineServerOPSystem.... Determine the server operating system type.
*  -------------------------------------------------------------------------------------------------------
  form determineServerOPSystem using separator
                                     serverFileSystem
                                     serverOpSystem.

*   Find the file system
    select single fileSys
                  from opSystem
                  into serverFileSystem
                  where opSys = sy-opsys.

    find 'WINDOWS' in serverFilesystem ignoring case.
    if sy-subrc = 0.
      separator = '\'.
      serverOpSystem = NON_UNIX.
      serverFileSystem = 'Windows NT'.
    else.
      find 'DOS' in serverFilesystem ignoring case.
      if sy-subrc = 0.
        separator = '\'.
        serverOpSystem = NON_UNIX.
      else.
        separator = '/'.
        serverOpSystem = UNIX.
      endif.
    endif.
  endform.                                                                        "determineServerOpSystem

*  -------------------------------------------------------------------------------------------------------
*   findExternalCommand.... Determine if the external command exists.  If it doesn't then disable the
*                           server input field
*  -------------------------------------------------------------------------------------------------------
  form findExternalCommand using value(locServerFileSystem).

  data: castServerOpSys type syopsys.

    castServerOpSys = locServerFileSystem.

    call function 'SXPG_COMMAND_CHECK'
      exporting
        commandname                      = 'ZDTX_MKDIR'
        operatingsystem                  = castServerOpSys
      exceptions
        command_not_found                = 1
        others                           = 0.

    if sy-subrc <> 0.
      loop at screen.
        if screen-name = 'PLOGICAL'.
          screen-input = 0.
          modify screen.
        endif.

        if screen-name = 'PSERV'.
          screen-input = 0.
          modify screen.
        endif.

        if screen-name = 'PPC'.
          screen-input = 0.
          modify screen.
        endif.
      endloop.

*      message s000(oo) with 'Download to server disabled,' 'external command ZDTX_MKDIR not defined.'.
    endif.
  endform.

*  *******************************************************************************************************
*  ****************************************DOWNLOAD ROUTINES**********************************************
*  *******************************************************************************************************
*  -------------------------------------------------------------------------------------------------------
*   downloadTadir... download content of table TADIR
*  -------------------------------------------------------------------------------------------------------
  form downloadTadir using value(pathname)
                           value(saveToServer)
                           value(slashSeparator)
                           value(displayProgressMessage)
                           value(locServerFileSystem)
                           value(customerProgsOnly)
                           rangeAuthor like soAuthor[]
                           value(soLocPackage) like soPack[]
                           value(custNameRange).

    data: iContentTable type table of tadir.
    data: tableFilename type string.
    data: tableFilenameWithPath type string.
    data: newSubDirectory type string.
    data: completeSavePath type string.
    data: altCustomerNameRange type string.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

      if customerProgsOnly is initial.
*       Select all objects
        select *
               appending corresponding fields of table iContentTable
               from tadir
               where author in rangeAuthor
                 and devclass in soLocPackage.

      else.
*       Select only customer specific objects
        select *
               appending corresponding fields of table iContentTable
               from tadir
               where (    obj_name like altCustomerNameRange
                       or obj_name like 'Z%'
                       or obj_name like 'Y%'
                       or obj_name like 'SALZ%'
                       or obj_name like 'SAPLY%'
                       or obj_name like 'SAPMZ%'
                       or obj_name like 'SAPMY%')
                 and author in rangeAuthor
                 and devclass in soLocPackage.
      endif.
  "cont.
    perform buildFilename using pathName
                                space
                                'TADIR'
                                space
                                space
                                TEXTEXTENSION
                                IS_METHOD
                                saveToServer
                                slashSeparator
                                tableFilenameWithPath
                                tablefilename
                                newSubDirectory
                                completeSavePath.

      if saveToServer is initial.
        perform saveFileToPcWH using iContentTable[]
                                   tableFilenameWithPath
                                   tableFilename
                                   'X'
                                   space
                                   displayProgressMessage
                                   'TADIR'.
      else.
        perform saveFileToServer using iContentTable[]
                                       tableFilenameWithPath
                                       tableFilename
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

  endform.
  form downloadTstct using value(pathname)
                           value(saveToServer)
                           value(slashSeparator)
                           value(displayProgressMessage)
                           value(locServerFileSystem)
                           value(customerProgsOnly)
                           rangeAuthor like soAuthor[]
                           value(soLocPackage) like soPack[]
                           value(custNameRange).

    data: iContentTable type table of tstct.
    data: tableFilename type string.
    data: tableFilenameWithPath type string.
    data: newSubDirectory type string.
    data: completeSavePath type string.
    data: altCustomerNameRange type string.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

      if customerProgsOnly is initial.
*       Select all objects
        select *
               appending corresponding fields of table iContentTable
               from tstct
               where sprsl = sy-langu.

      else.
*       Select only customer specific objects
        select *
               appending corresponding fields of table iContentTable
               from tstct
               where sprsl = sy-langu
               and   (    tcode like altCustomerNameRange
                       or tcode like 'Z%'
                       or tcode like 'Y%').
      endif.
  "cont.
    perform buildFilename using pathName
                                space
                                'TSTCT'
                                space
                                space
                                TEXTEXTENSION
                                IS_METHOD
                                saveToServer
                                slashSeparator
                                tableFilenameWithPath
                                tablefilename
                                newSubDirectory
                                completeSavePath.

      if saveToServer is initial.
        perform saveFileToPcWH using iContentTable[]
                                   tableFilenameWithPath
                                   tableFilename
                                   'X'
                                   space
                                   displayProgressMessage
                                   'TSTCT'.
      else.
        perform saveFileToServer using iContentTable[]
                                       tableFilenameWithPath
                                       tableFilename
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

  endform.
  form downloadTstcp using value(pathname)
                           value(saveToServer)
                           value(slashSeparator)
                           value(displayProgressMessage)
                           value(locServerFileSystem)
                           value(customerProgsOnly)
                           rangeAuthor like soAuthor[]
                           value(soLocPackage) like soPack[]
                           value(custNameRange).

    data: iContentTable type table of tstcp.
    data: tableFilename type string.
    data: tableFilenameWithPath type string.
    data: newSubDirectory type string.
    data: completeSavePath type string.
    data: altCustomerNameRange type string.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

      if customerProgsOnly is initial.
*       Select all objects
        select *
               appending corresponding fields of table iContentTable
               from tstcp.

      else.
*       Select only customer specific objects
        select *
               appending corresponding fields of table iContentTable
               from tstcp
               where (    tcode like altCustomerNameRange
                       or tcode like 'Z%'
                       or tcode like 'Y%').
      endif.
  "cont.
    perform buildFilename using pathName
                                space
                                'TSTCP'
                                space
                                space
                                TEXTEXTENSION
                                IS_METHOD
                                saveToServer
                                slashSeparator
                                tableFilenameWithPath
                                tablefilename
                                newSubDirectory
                                completeSavePath.

      if saveToServer is initial.
        perform saveFileToPcWH using iContentTable[]
                                   tableFilenameWithPath
                                   tableFilename
                                   'X'
                                   space
                                   displayProgressMessage
                                   'TSTCP'.
      else.
        perform saveFileToServer using iContentTable[]
                                       tableFilenameWithPath
                                       tableFilename
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

  endform.
*  -------------------------------------------------------------------------------------------------------
*   downloadTrdirt... download content of table TRDIRT
*  -------------------------------------------------------------------------------------------------------
  form downloadTrdirt using value(pathname)
                           value(saveToServer)
                           value(slashSeparator)
                           value(displayProgressMessage)
                           value(locServerFileSystem)
                           value(customerProgsOnly)
                           rangeAuthor like soAuthor[]
                           value(soLocPackage) like soPack[]
                           value(custNameRange).

    data: iContentTable type table of trdirt.
    data: tableFilename type string.
    data: tableFilenameWithPath type string.
    data: newSubDirectory type string.
    data: completeSavePath type string.
    data: altCustomerNameRange type string.

*   build up the customer name range used for select statements
    concatenate custNameRange '%' into altCustomerNameRange.

        select *
               appending corresponding fields of table iContentTable
               from trdirt
               where (    name like altCustomerNameRange
                       or name like 'Z%'
                       or name like 'Y%'
                       or name like 'SAPMZ%'
                       or name like 'SAPMY%')
               and     sprsl = pMLang.
  "cont.
    perform buildFilename using pathName
                                'programs'
                                'TRDIRT'
                                space
                                space
                                TEXTEXTENSION
                                IS_PROGRAM
                                saveToServer
                                slashSeparator
                                tableFilenameWithPath
                                tablefilename
                                newSubDirectory
                                completeSavePath.

      if saveToServer is initial.
        perform saveFileToPcWH using iContentTable[]
                                   tableFilenameWithPath
                                   tableFilename
                                   'X'
                                   space
                                   displayProgressMessage
                                   'TRDIRT'.
      else.
        perform saveFileToServer using iContentTable[]
                                       tableFilenameWithPath
                                       tableFilename
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

  endform.
*  -------------------------------------------------------------------------------------------------------
*   downloadDDStructures... download database objects to file
*  -------------------------------------------------------------------------------------------------------
  form downloadDDStructures using iLocDictionary like iDictionary[]
                                  iLocDictionaryFilename like iDictFilename[]
                                  value(pathname)
                                  value(HtmlFileExtension)
                                  value(subDir)
                                  value(sortTablesAsc)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.


  field-symbols: <waDictionary> type tDictTable.
  data: tableFilename type string.
  data: tableFilenameWithPath type string.
  data: iHtmlTable type standard table of string with header line.
  data: newSubDirectory type string.
  data: completeSavePath type string.
  data: waDictFilename like line of iLocDictionaryFilename.

    loop at iLocDictionary assigning <waDictionary>.
      perform buildFilename using pathName
                                  subDir
                                  <waDictionary>-tablename
                                  space
                                  space
                                  HtmlFileExtension
                                  IS_TABLE
                                  saveToServer
                                  slashSeparator
                                  tableFilenameWithPath
                                  tablefilename
                                  newSubDirectory
                                  completeSavePath.

      read table iLocDictionaryFilename into waDictFilename with key tableName = <waDictionary>-tablename
                                                                     filename = completeSavePath.
      if sy-subrc <> 0.
*       Try and import a converted table to memory as it will be much quicker than converting it again
        import iHtmlTable from memory id <waDictionary>-tablename.
        if sy-subrc <> 0.
          if displayProgressMessage = ABAP_TRUE.
            concatenate 'Converting table' <waDictionary>-tablename 'to html' into statusBarMessage separated by space.
            perform displayStatus using statusBarMessage 0.
          endif.

          perform convertDDtoHTML using <waDictionary>-iStructure[]
                                        iHtmlTable[]
                                        <waDictionary>-tableName
                                        <waDictionary>-tableTitle
                                        sortTablesAsc
                                        addBackground.

          export iHtmlTable to memory id <waDictionary>-tablename.
        endif.

        if saveToServer is initial.
          perform saveFileToPc using iHtmlTable[]
                                     tableFilenameWithPath
                                     tableFilename
                                     space
                                     space
                                     displayProgressMessage.
        else.
          perform saveFileToServer using iHtmlTable[]
                                         tableFilenameWithPath
                                         tableFilename
                                         completeSavePath
                                         displayProgressMessage
                                         locServerFileSystem.
        endif.

        waDictFilename-tablename = <waDictionary>-tablename.
        waDictFilename-filename = completeSavePath.
        append waDictFilename to iLocDictionaryFilename.
        clear waDictFilename.
      endif.

      clear iHtmlTable[].
    endloop.
  endform.                                                                           "downloadDDStructures

*  ----------------------------------------------------------------------------------------------------------------------
*    downloadDDTableTypes
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadDDTableTypes using iLocTableTypes like iTableTypes[]
                                  iLocTableTypeFilename like iDictFilename[]
                                  value(pathname)
                                  value(htmlFileExtension)
                                  value(subdir)
                                  value(sortTablesAsc)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.

  field-symbols: <watabletypes> like line of iLocTableTypes.
  field-symbols <waTtFilename> like line of iLocTableTypeFilename[].
  data: tableFilename type string.
  data: tableFilenameWithPath type string.
  data: iHtmlTable type standard table of string with header line.
  data: newSubDirectory type string.
  data: completeSavePath type string.

    loop at iLocTableTypes assigning <waTableTypes>.
      perform buildFilename using pathname
                                  subdir
                                  <waTableTypes>-typename
                                  space
                                  space
                                  htmlFileExtension
                                  IS_TABLE
                                  saveToServer
                                  slashSeparator
                                  tableFilenameWithPath
                                  tableFilename
                                  newSubDirectory
                                  completeSavePath.

      read table iLocTableTypeFilename into <waTTFilename> with key tableName = <waTableTypes>-typename
                                                                     filename = completeSavePath.
      if sy-subrc <> 0.
*       Try and import a converted table to memory as it will be much quicker than converting it again
        import iHtmlTable from memory id <waTableTypes>-typename.
        if sy-subrc <> 0.
          if displayProgressMessage = ABAP_TRUE.
            concatenate 'Converting table type' <waTableTypes>-typename 'to html' into statusBarMessage separated by space.
            perform displayStatus using statusBarMessage 0.
          endif.

*      --     Need a specific routine for table types.  All data is in the table
          perform convertTableTypeToHtml using iLocTableTypes[]
                                               iHtmlTable[]
                                               <waTableTypes>-typename
                                               <waTableTypes>-ddtext
                                               sortTablesAsc
                                               addBackground.

          if saveToServer is initial.
            perform saveFileToPC using iHtmlTable[]
                                       tableFilenameWithPath
                                       tableFilename
                                       space
                                       space
                                       displayProgressMessage.
          else.
            perform saveFileToServer using iHtmlTable[]
                                           tableFilenameWithPath
                                           tableFilename
                                           completeSavePath
                                           displayProgressMessage
                                           locServerFileSystem.
          endif.
        endif.
      endif.
      clear iHtmlTable[].
    endloop.
  endform.

*  -------------------------------------------------------------------------------------------------------
*   downloadMessageClass...
*  -------------------------------------------------------------------------------------------------------
  form downloadMessageClass using iLocMessages like iMessages[]
                                  value(messageClassName)
                                  value(userFilePath)
                                  value(fileExtension)
                                  value(HTMLfileFlag)
                                        subDir
                                  value(customerNameRange)
                                  value(getIncludes)
                                  value(getDictStructures)
                                  value(userHasSelectedMessageClasses)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.

  data: htmlPageName type string.
  data: newFilenameOnly type string.
  data: newFilenameWithPath type string.
  data: iHtmlTable type standard table of string with header line.
  data: newSubDirectory type string.
  data: completeSavePath type string.

    perform appendMessagesToFile using iLocMessages[]
                                       iHtmlTable[]
                                       userHasSelectedMessageClasses.


    concatenate `message class ` messageClassName into htmlPageName.

    if htmlFileFlag is initial.
      append '' to iHtmlTable.
      append  '----------------------------------------------------------------------------------' to iHtmlTable.

      perform buildFooterMessage using iHtmlTable.
      append iHtmlTable.
    else.
      perform convertCodeToHtml using iHtmlTable[]
                                      htmlPageName
                                      space
                                      IS_MESSAGECLASS
                                      ''
                                      FALSE
                                      fileExtension
                                      customerNameRange
                                      getIncludes
                                      getDictStructures
                                      addBackground.
    endif.

    perform buildFilename using userFilePath
                                subDir
                                messageClassName
                                space
                                space
                                fileExtension
                                IS_MESSAGECLASS
                                saveToServer
                                slashSeparator
                                newFilenameWithPath
                                newFilenameOnly
                                newSubDirectory
                                completeSavePath.

      if saveToServer is initial.
        perform saveFileToPc using iHtmlTable[]
                                   newFilenameWithPath
                                   newFilenameOnly
                                   space
                                   space
                                   displayProgressMessage.
      else.
*       Save the file to the SAP server
        perform saveFileToServer using iHtmlTable[]
                                       newFilenameWithPath
                                       newFilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.
  endform.                                                                          "downloadMessageClass

*  -------------------------------------------------------------------------------------------------------
*    appendMessagesToFile
*  -------------------------------------------------------------------------------------------------------
  form appendMessagesToFile using iLocMessages like iMessages[]
                                  iLocHtml like dumiHtml[]
                                  value(userHasSelectedMessageClasses).

  data: previousMessageID like iMessages-arbgb.
  field-symbols: <waMessage> type tMessage.
  data: waHtml type string.

    sort iLocMessages ascending by arbgb msgnr.

    if not iLocMessages[] is initial.
      if userHasSelectedMessageClasses is initial.
*       Only add these extra lines if we are actually appending them to the end of some program code
        append waHtml to iLocHtml.
        append waHtml to iLocHtml.

        append '*Messages' to iLocHtml.
        append '*----------------------------------------------------------' to iLocHtml.
      endif.

      loop at iLocMessages assigning <waMessage>.
        if ( <waMessage>-arbgb <> previousMessageID ).

          if userHasSelectedMessageClasses is initial.
*           Only add this extra lines if we are actually appending them to the end of some program code
            append '*' to iLocHtml.
            concatenate `* Message class: ` <waMessage>-arbgb into waHtml.
            append waHtml to iLocHtml.
          endif.

          previousMessageID = <waMessage>-arbgb.
          clear waHtml.
        endif.

        if userHasSelectedMessageClasses is initial.
*         Only add this extra lines if we are actually appending them to the end of some program code
          concatenate '*' <waMessage>-msgnr `   ` <waMessage>-text into waHtml.
        else.
          concatenate <waMessage>-msgnr `   ` <waMessage>-text into waHtml.
        endif.

        append waHtml to iLocHtml.
      endloop.
    endif.
  endform.                                                                           "appendMessagesToFile

*  -------------------------------------------------------------------------------------------------------
*    downloadFunctions...       Download function modules to file.
*  -------------------------------------------------------------------------------------------------------
  form downloadFunctions using iLocFunctions like iFunctions[]
                               iLocDictionaryFilename like iDictFilename[]
                               iLocTableTypeFilename like iTableTypeFilename[]
                               value(userFilePath)
                               value(fileExtension)
                               value(subDir)
                               value(downloadDocumentation)
                               value(convertToHtml)
                               value(customerNameRange)
                               value(getIncludes)
                               value(getDictStruct)
                               value(textFileExtension)
                               value(htmlFileExtension)
                               value(sortTablesAsc)
                               value(slashSeparator)
                               value(saveToServer)
                               value(displayProgressMessage)
                               value(locServerFileSystem)
                               value(addBackground) type abap_bool.

  data: mainSubdir type string.
  data: incSubdir type string.
  field-symbols: <waFunction> type tFunction.
  field-symbols: <waInclude> type tInclude.
  data: iEmptyTextelements type standard table of tTextTable.
  data: iEmptySelectionTexts type standard table of tTextTable.
  data: iEmptyMessages type standard table of tMessage.
  data: iEmptyGuiTitles type standard table of tGuiTitle.
  data: functionDocumentationExists type abap_bool value FALSE.


    loop at iLocFunctions assigning <waFunction>.
      if subDir is initial.
        incSubDir = <waFunction>-functionName.
        mainSubDir = ''.
      else.
        concatenate Subdir <waFunction>-functionName into incSubDir separated by slashSeparator.
        mainSubDir = SubDir.
      endif.

      if not downloadDocumentation is initial.
        perform downloadFunctionDocs using <waFunction>-functionName
                                           <waFunction>-functionTitle
                                           userFilePath
                                           fileExtension
                                           convertToHtml
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           mainSubDir
                                           functionDocumentationExists
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download main source code
      perform readFunctionAndDownload using <waFunction>-iTextelements[]
                                            <waFunction>-iSelectionTexts[]
                                            <wafunction>-iMessages[]
                                            <wafunction>-functionName
                                            <wafunction>-functionMainInclude
                                            <waFunction>-functionTitle
                                            userFilePath
                                            fileExtension
                                            mainSubDir
                                            convertToHtml
                                            functionDocumentationExists
                                            customerNameRange
                                            getIncludes
                                            getDictStruct
                                            slashSeparator
                                            saveToServer
                                            displayProgressMessage
                                            locServerFileSystem
                                            addBackground.

*     Download top include
      perform readIncludeAndDownload using iEmptyTextelements[]
                                           iEmptySelectionTexts[]
                                           iEmptyMessages[]
                                           iEmptyGuiTitles[]
                                           <waFunction>-topIncludeName
                                           <waFunction>-functionName
                                           <waFunction>-functionTitle
                                           IS_FUNCTION
                                           userFilePath
                                           fileExtension
                                           mainSubDir
                                           convertToHtml
                                           customerNameRange
                                           getIncludes
                                           getDictStruct
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.

*     Download screens.
      if not <waFunction>-iScreenFlow[] is initial.
        perform downloadScreens using <wafunction>-iScreenFlow[]
                                      <wafunction>-progname
                                      userFilePath
                                      textFileExtension
                                      mainSubDir
                                      slashSeparator
                                      saveToServer
                                      displayProgressMessage
                                      locServerFileSystem.
      endif.

*     Download GUI titles
      if not <waFunction>-iGUITitle[] is initial.
        perform downloadGUITitles using <wafunction>-iGUITitle
                                        userFilePath
                                        textFileExtension
                                        mainsubDir
                                        slashSeparator
                                        saveToServer
                                        displayProgressMessage
                                        locServerFileSystem.
      endif.

*     Download all other includes
      loop at <wafunction>-iIncludes assigning <waInclude>.
        perform readIncludeAndDownload using iEmptyTextelements[]
                                             iEmptySelectionTexts[]
                                             iEmptyMessages[]
                                             iEmptyGuiTitles[]
                                             <waInclude>-IncludeName
                                             space
                                             <waInclude>-IncludeTitle
                                             IS_PROGRAM
                                             userFilePath
                                             fileExtension
                                             incSubDir
                                             convertToHtml
                                             customerNameRange
                                             getIncludes
                                             getDictStruct
                                             slashSeparator
                                             saveToServer
                                             displayProgressMessage
                                             locServerFileSystem
                                             addBackground.

      endloop.

*     Download all dictionary structures
      if not <wafunction>-iDictStruct[] is initial.
        perform downloadDDStructures using <wafunction>-iDictStruct[]
                                           iLocDictionaryFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           mainSubDir
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download all Table Types
      if not <waFunction>-iTableTypes[] is initial.
        perform downloadDDTableTypes using <waFunction>-iTableTypes[]
                                           iLocTableTypeFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           mainSubDir
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download Transformations
      if not <waFunction>-iTransformations[] is initial.
        perform downloadXSLT using <waFunction>-iTransformations[]
                                   userFilePath
                                   fileExtension
                                   htmlFileExtension
                                   textFileExtension
                                   convertToHtml
                                   customerNamespace
                                   slashSeparatorToUse
                                   saveToServer
                                   subdir
                                   displayProgressMessage
                                   serverFileSystem
                                   addBackground.
      endif.
    endloop.
  endform.                                                                              "downloadFunctions

*  -------------------------------------------------------------------------------------------------------
*     readIcludeAndDownload...
*  -------------------------------------------------------------------------------------------------------
  form readIncludeAndDownload using iLocTextElements like dumiTextTab[]
                                    iLocSelectionTexts like dumiTextTab[]
                                    iLocMessages like iMessages[]
                                    iLocGUITitles like dumIGUITitle[]
                                    value(programName)
                                    value(functionName)
                                    value(programDescription)
                                    value(overideProgType)
                                    value(userFilePath)
                                    value(fileExtension)
                                    value(additionalSubDir)
                                    value(convertToHtml)
                                    value(customerNameRange)
                                    value(getIncludes)
                                    value(getDictStructures)
                                    value(slashSeparator)
                                    value(saveToServer)
                                    value(displayProgressMessage)
                                    value(locServerFileSystem)
                                    value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: localFilenameWithPath type string.
  data: localFilenameOnly type string.
  data: newSubdirectory type string.
  data: objectName type string.
  data: completeSavePath type string.

    read report programName into iLines.

*   Download GUI titles for main program
    if not iLocGUITitles[] is initial.
      perform appendGUITitles using iLocGUITitles[]
                                    iLines[].
    endif.

*   Download text elements for main program
    if not iLocTextElements[] is initial.
      perform appendTextElements using iLocTextElements[]
                                       iLines[].
    endif.

*   Download selection texts for main program
    if not iLocSelectiontexts[] is initial.
      perform appendSelectionTexts using iLocSelectiontexts[]
                                         iLines[].
    endif.

*   Download messages classes for main program.
    if not iLocMessages[] is initial.
      perform appendMessagesToFile using iLocMessages[]
                                         iLines[]
                                         space.
    endif.

    if convertToHtml is initial.
      append '' to iLines.
      append '----------------------------------------------------------------------------------' to iLines.
      perform buildFooterMessage using iLines.
      append iLines.
    else.
      perform convertCodeToHtml using iLines[]
                                      programName
                                      programDescription
                                      overideProgType
                                      space
                                      space
                                      fileExtension
                                      customerNameRange
                                      getIncludes
                                      getDictStructures
                                      addBackground.
    endif.

    if functionName is initial.
      objectName = programName.
    else.
      objectName = functionName.
    endif.

    perform buildFilename using userFilePath
                                additionalSubDir
                                objectName
                                space
                                programName
                                fileExtension
                                overideProgType
                                saveToServer
                                slashSeparator
                                localFilenameWithPath
                                localfilenameOnly
                                newSubdirectory
                                completeSavePath.

    if saveToServer is initial.
      perform saveFileToPc using iLines[]
                                 localFilenameWithPath
                                 localFilenameOnly
                                 space
                                 space
                                 displayProgressMessage.
    else.
      perform saveFileToServer using iLines[]
                                     localFilenameWithPath
                                     localFilenameOnly
                                     completeSavePath
                                     displayProgressMessage
                                     locServerFileSystem.
    endif.
  endform.                                                                         "readIncludeAndDownload

*  -------------------------------------------------------------------------------------------------------
*     readClassAndDownload...
*  -------------------------------------------------------------------------------------------------------
  form readClassAndDownload using waLocClass type tClass
                                  value(className)
                                  value(functionName)
                                  value(overideProgType)
                                  value(userFilePath)
                                  value(fileExtension)
                                  value(additionalSubDir)
                                  value(convertToHtml)
                                  value(customerNameRange)
                                  value(getIncludes)
                                  value(getDictStructures)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.

  data: iTempLines type standard table of string with header line.
  data: iLines type standard table of string with header line.
  data: localFilenameWithPath type string.
  data: localFilenameOnly type string.
  data: newSubdirectory type string.
  data: objectName type string.
  data: castClassName type program.
  data: completeSavePath type string.

*   Build up attribute comments
    append '**************************************************************************' to iLines.
    append '*   Class attributes.                                                    *' to iLines.
    append '**************************************************************************' to iLines.
    case waLocClass-exposure.
      when 0.
        append `Instantiation: Private` to iLines.
      when 1.
        append `Instantiation: Protected` to iLines.
      when 2.
        append `Instantiation: Public` to iLines.
    endcase.
    concatenate `Message class: ` waLocClass-msg_id into iLines.
    append iLines.
    case waLocClass-state.
      when 0.
        append `State: Only Modelled` to iLines.
      when 1.
        append `State: Implemented` to iLines.
    endcase.
    concatenate `Final Indicator: ` waLocClass-clsfinal into iLines.
    append iLines.
    concatenate `R/3 Release: ` waLocClass-r3Release into iLines.
    append iLines.
    clear iLines.
    append iLines.

    castClassName = waLocClass-publicClassKey.
    read report castClassName into iTempLines.
    if sy-subrc = 0.
      perform reFormatClassCode using iTempLines[].

      append '**************************************************************************' to iLines.
      append '*   Public section of class.                                             *' to iLines.
      append '**************************************************************************' to iLines.
      loop at iTempLines.
        append iTempLines to iLines.
      endloop.
    endif.

    castClassName = waLocClass-privateClassKey.
    read report castClassName into iTempLines.
    if sy-subrc = 0.
      perform reFormatClassCode using iTempLines[].

      append iLines.
      append '**************************************************************************' to iLines.
      append '*   Private section of class.                                            *' to iLines.
      append '**************************************************************************' to iLines.
      loop at iTempLines.
        append iTempLines to iLines.
      endloop.
    endif.

    castClassName = waLocClass-ProtectedClassKey.
    read report castClassName into iTempLines.
    if sy-subrc = 0.
      perform reFormatClassCode using iTempLines[].

      append iLines.
      append '**************************************************************************' to iLines.
      append '*   Protected section of class.                                          *' to iLines.
      append '**************************************************************************' to iLines.
      loop at iTempLines.
        append iTempLines to iLines.
      endloop.
    endif.

    castClassName = waLocClass-typesClassKey.
    read report castClassName into iTempLines.
    if sy-subrc = 0.
      append iLines.
      append '**************************************************************************' to iLines.
      append '*   Types section of class.                                              *' to iLines.
      append '**************************************************************************' to iLines.
      loop at iTempLines.
        append iTempLines to iLines.
      endloop.
    endif.

*   Download text elements for this class
    if not waLocClass-iTextElements[] is initial.
      perform appendTextElements using waLocClass-iTextElements[]
                                       iLines[].
    endif.

*   Download messages classes for this class.
    if not waLocClass-iMessages[] is initial.
      perform appendMessagesToFile using waLocClass-iMessages[]
                                         iLines[]
                                         space.
    endif.

*   Download exception texts for this class
    if not waLocClass-iConcepts[] is initial.
      perform appendExceptionTexts using waLocClass-iConcepts[]
                                         iLines[].
    endif.


    if convertToHtml is initial.
      append '' to iLines.
      append '----------------------------------------------------------------------------------' to iLines.
      perform buildFooterMessage using iLines.
      append iLines.
    else.
      perform convertClassToHtml using iLines[]
                                      className
                                      waLocClass-descript
                                      overideProgType
                                      fileExtension
                                      customerNameRange
                                      getDictStructures
                                      addBackground.
    endif.

    if functionName is initial.
      objectName = className.
    else.
      objectName = functionName.
    endif.

    perform buildFilename using userFilePath
                                additionalSubDir
                                objectName
                                space
                                className
                                fileExtension
                                overideProgType
                                saveToServer
                                slashSeparator
                                localFilenameWithPath
                                localfilenameOnly
                                newSubdirectory
                                completeSavePath.

    if saveToServer is initial.
      perform saveFileToPc using iLines[]
                                 localFilenameWithPath
                                 localFilenameOnly
                                 space
                                 space
                                 displayProgressMessage.
    else.
      perform saveFileToServer using iLines[]
                                     localFilenameWithPath
                                     localFilenameOnly
                                     completeSavePath
                                     displayProgressMessage
                                     locServerFileSystem.
    endif.
  endform.                                                                           "readClassAndDownload

*  -------------------------------------------------------------------------------------------------------
*     readMethodAndDownload...
*  -------------------------------------------------------------------------------------------------------
  form readMethodAndDownload using waLocMethod type tMethod
                                  value(methodName)
                                  value(methodKey)
                                  value(functionName)
                                  value(overideProgType)
                                  value(userFilePath)
                                  value(fileExtension)
                                  value(additionalSubDir)
                                  value(convertToHtml)
                                  value(customerNameRange)
                                  value(getIncludes)
                                  value(getDictStructures)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: iTempLines type standard table of string with header line.
  data: localFilenameWithPath type string.
  data: localFilenameOnly type string.
  data: newSubdirectory type string.
  data: objectName type string.
  data: castMethodKey type program.
  data: completeSavePath type string.

*   Add the method scope to the downloaded file
    append '**************************************************************************' to iLines.
    append '*   Method attributes.                                                   *' to iLines.
    append '**************************************************************************' to iLines.
    case waLocMethod-exposure.
      when 0.
        append `Instantiation: Private` to iLines.
      when 1.
        append `Instantiation: Protected` to iLines.
      when 2.
        append `Instantiation: Public` to iLines.
    endcase.
    append '**************************************************************************' to iLines.
    append '' to iLines.

    castMethodKey = waLocMethod-methodKey.
    read report castMethodKey into iTempLines.
    loop at iTempLines.
      append iTempLines to iLines.
    endloop.

    if convertToHtml is initial.
      append '' to iLines.
      append '----------------------------------------------------------------------------------' to iLines.
      perform buildFooterMessage using iLines.
      append iLines.
    else.
      perform convertCodeToHtml using iLines[]
                                      methodName
                                      waLocMethod-descript
                                      overideProgType
                                      space
                                      space
                                      fileExtension
                                      customerNameRange
                                      getIncludes
                                      getDictStructures
                                      addBackground.
    endif.

    if functionName is initial.
      objectName = methodName.
    else.
      objectName = functionName.
    endif.

    case waLocMethod-exposure.
      when 0.
*       Private
        concatenate additionalSubDir slashSeparator 'private_methods' into additionalSubDir.
      when 1.
*       Protected
        concatenate additionalSubDir slashSeparator 'protected_methods' into additionalSubDir.
      when 2.
*       Public
        concatenate additionalSubDir slashSeparator 'public_methods' into additionalSubDir.
    endcase.

    perform buildFilename using userFilePath
                                additionalSubDir
                                objectName
                                space
                                methodName
                                fileExtension
                                overideProgType
                                saveToServer
                                slashSeparator
                                localFilenameWithPath
                                localfilenameOnly
                                newSubdirectory
                                completeSavePath.

    if saveToServer is initial.
      perform saveFileToPc using iLines[]
                                 localFilenameWithPath
                                 localFilenameOnly
                                 space
                                 space
                                 displayProgressMessage.
    else.
      perform saveFileToServer using iLines[]
                                     localFilenameWithPath
                                     localFilenameOnly
                                     completeSavePath
                                     displayProgressMessage
                                     locServerFileSystem.
    endif.
  endform.                                                                          "readMethodAndDownload

*  -------------------------------------------------------------------------------------------------------
*     readXsltAndDownload...
*  -------------------------------------------------------------------------------------------------------
  form readXsltAndDownload using value(xsltName)
                                       xsltDescription
                                 value(userFilepath)
                                 value(fileExtension)
                                 value(convertToHtml)
                                 value(customerNameRange)
                                 value(slashSeparator)
                                 value(saveToServer)
                                 value(subdir)
                                 value(displayProgressMessage)
                                 value(locServerFileSystem)
                                 value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: iLocXsltSource type o2pageline_table.
  data: waXsltAttributes type o2xsltattr.
  data: localFilenameWithPath type string.
  data: localFilenameOnly type string.
  data: newSubDirectory type string.
  data: objectName type string.
  data: completeSavePath type string.

    cl_o2_api_xsltdesc=>load( exporting p_xslt_desc = xsltname
                              importing p_source = iLocXsltSource
                                        p_attributes = waXsltAttributes
                              exceptions not_existing = 1
                                         permission_failure = 2
                                         error_occured = 3
                                         version_not_found = 4 ).

     if sy-subrc = 0.
       xsltDescription = waXsltAttributes-descript.

       append lines of iLocXsltSource to iLines.

      if convertToHtml is initial.
        append '' to iLines.
        append '----------------------------------------------------------------------------------' to ilines.
        perform buildFooterMessage using iLines.
        append iLines.
      else.
        perform convertCodeToHtml using iLines[]
                                        xsltName
                                        waXsltAttributes-descript
                                        space
                                        space
                                        space
                                        fileExtension
                                        customerNameRange
                                        space
                                        space
                                        addBackground.
      endif.

      perform buildfilename using userFilePath
                                  subdir
                                  xsltName
                                  space
                                  xsltName
                                  fileExtension
                                  IS_TRANSFORMATION
                                  saveToServer
                                  slashSeparator
                                  localFilenameWithPath
                                  localFilenameOnly
                                  newSubdirectory
                                  completeSavePath.

      if saveToServer is initial.
        perform saveFileTopc using iLines[]
                                   localFilenameWithPath
                                   localFilenameOnly
                                   space
                                   space
                                   displayProgressMessage.
      else.
        perform saveFileToServer using iLines[]
                                       localFilenameWithPath
                                       localFilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFilesystem.
      endif.
    endif.
  endform.                                                                         "readXSLTAndDownload

*  -------------------------------------------------------------------------------------------------------
*     readFunctionAndDownload...
*  -------------------------------------------------------------------------------------------------------
  form readFunctionAndDownload using iLocTextElements like dumiTextTab[]
                                     iLocSelectionTexts like dumiTextTab[]
                                     iLocMessages like iMessages[]
                                     value(functionName)
                                     value(functionInternalName)
                                     value(shortText)
                                     value(userFilePath)
                                     value(fileExtension)
                                     value(subDir)
                                     value(convertToHtml)
                                     value(functionDocumentationExists)
                                     value(customerNameRange)
                                     value(getIncludes)
                                     value(getDictStructures)
                                     value(slashSeparator)
                                     value(saveToServer)
                                     value(displayProgressMessage)
                                     value(locServerFileSystem)
                                     value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: localFilenameWithPath type string.
  data: localFilenameOnly type string.
  data: newSubDirectory type string.
  data: completeSavePath type string.

    read report functionInternalName into iLines.

*   If we found any text elements for this function then we ought to append them to the main include.
    if not iLocTextElements[] is initial.
      perform appendTextElements using iLocTextElements[]
                                       iLines[].
    endif.

*   If we found any message classes for this function then we ought to append them to the main include.
    if not iLocMessages[] is initial.
      perform appendMessagesToFile using iLocMessages[]
                                         iLines[]
                                         space.
    endif.

    if convertToHtml is initial.
      append '' to iLines.
      append '----------------------------------------------------------------------------------' to iLines.
      perform buildFooterMessage using iLines.
      append iLines.
    else.
      perform convertFunctionToHtml using iLines[]
                                          functionName
                                          shortText
                                          IS_FUNCTION
                                          functionDocumentationExists
                                          TRUE
                                          fileExtension
                                          customerNameRange
                                          getIncludes
                                          getDictStructures
                                          addBackground.
    endif.

    perform buildFilename using userFilePath
                                subDir
                                functionName
                                space
                                space
                                fileExtension
                                IS_FUNCTION
                                saveToServer
                                slashSeparator
                                localFilenameWithPath
                                localfilenameOnly
                                newSubDirectory
                                completeSavePath.

    if saveToServer is initial.
      perform saveFileToPc using iLines[]
                                 localFilenameWithPath
                                 localFilenameOnly
                                 space
                                 space
                                 displayProgressMessage.
    else.
      perform saveFileToServer using iLines[]
                                     localFilenameWithPath
                                     localFilenameOnly
                                     completeSavePath
                                     displayProgressMessage
                                     locServerFileSystem.
    endif.
  endform.                                                                        "readFunctionAndDownload

*  -------------------------------------------------------------------------------------------------------
*    buildFilename...
*  -------------------------------------------------------------------------------------------------------
  form buildFilename using value(userPath)
                           value(additionalSubDirectory)
                           value(objectName)
                           value(mainFunctionNo)
                           value(includeName)
                           value(fileExtension)
                           value(downloadType)
                           value(downloadToServer)
                           value(slashSeparator)
                                 newFilenameWithPath
                                 newFilenameOnly
                                 newSubDirectory
                                 completePath.

*   If we are running on a non UNIX environment we will need to remove forward slashes from the additional path.
    if downloadToServer is initial.
      if frontendOpSystem = NON_UNIX.
        if not additionalSubdirectory is initial.
          translate additionalSubdirectory using '/_'.
          if additionalSubdirectory+0(1) = '_'.
            shift additionalSubdirectory left by 1 places.
          endif.
        endif.
      endif.
    else.
      if serverOpSystem = NON_UNIX.
        if not additionalSubdirectory is initial.
          translate additionalSubdirectory using '/_'.
          if additionalSubdirectory+0(1) = '_'.
            shift additionalSubdirectory left by 1 places.
          endif.
        endif.
      endif.
    endif.

    case downloadType.
*     Programs
      when IS_PROGRAM.
        if additionalSubDirectory is initial.
          concatenate userPath slashSeparator objectName PERIOD fileExtension into newFilenameWithPath.
          concatenate userPath slashSeparator into completePath.
        else.
          concatenate userPath slashSeparator additionalSubdirectory
                               slashSeparator objectName PERIOD fileExtension into newFilenameWithPath.
          concatenate userPath slashSeparator additionalSubdirectory into completePath.
        endif.

*     Function Modules
      when IS_FUNCTION.
        if additionalSubdirectory is initial.
          find 'top' in includeName ignoring case.
          if sy-subrc = 0.
            concatenate userPath slashSeparator objectName
                                 slashSeparator 'Global-' objectName
                                 PERIOD fileExtension
                                 into newFilenameWithPath.
          else.
            if includeName cs mainFunctionNo and not mainFunctionNo is initial.
              concatenate userPath slashSeparator objectName
                                   slashSeparator objectName
                                   PERIOD fileExtension
                                   into newFilenameWithPath.
            else.
              concatenate userPath slashSeparator objectName
                                   slashSeparator objectName
                                   PERIOD fileExtension
                                   into newFilenameWithPath.
            endif.
          endif.
          newSubDirectory = objectName.
          concatenate userPath
                      slashSeparator
                      newSubDirectory
                      slashSeparator into completePath.
        else.
          find 'top' in includeName ignoring case.
          if sy-subrc = 0.
            concatenate userPath slashSeparator additionalSubdirectory
                                 slashSeparator objectName
                                 slashSeparator 'Global-' objectName
                                 PERIOD fileExtension
                                 into newFilenameWithPath.
          else.
            if includeName cs mainFunctionNo and not mainFunctionNo is initial.
              concatenate userPath slashSeparator additionalSubdirectory
                                   slashSeparator objectName
                                   slashSeparator objectName
                                   PERIOD fileExtension
                                   into newFilenameWithPath.
            else.
              concatenate userPath slashSeparator additionalSubdirectory
                                   slashSeparator objectName
                                   slashSeparator objectName
                                   PERIOD fileExtension
                                   into newFilenameWithPath.
            endif.
          endif.
          concatenate additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator additionalSubdirectory slashSeparator objectName into completePath.
        endif.

*     Table definition
      when IS_TABLE.
        if additionalSubdirectory is initial.
          concatenate userPath slashSeparator 'Dictionary_Objects'   " slashSeparator objectName
                               slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into NewSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userpath slashSeparator additionalSubDirectory
                               slashSeparator 'Dictionary_Objects'   " slashSeparator objectName
                               slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into completePath.
        endif.

*     Table definition
      when IS_TRANSFORMATION.
        if additionalSubdirectory is initial.
          concatenate userPath slashSeparator 'Transformations'   " slashSeparator objectName
                               slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into NewSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userpath slashSeparator additionalSubDirectory
                               slashSeparator 'Transformations'   " slashSeparator objectName
                               slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into completePath.
        endif.

*     Program & Function documentation
      when IS_DOCUMENTATION.
        if additionalSubDirectory is initial.
          concatenate userPath slashSeparator objectName
                               slashSeparator 'Docs-'
                               objectName PERIOD
                               fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userPath slashSeparator additionalSubDirectory
                               slashSeparator objectName
                               slashSeparator 'Docs-'
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userpath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userpath slashSeparator additionalSubDirectory slashSeparator objectName into completePath.
      endif.

*     Screens
      when IS_SCREEN.
        if additionalSubDirectory is initial.
          concatenate userpath slashSeparator 'Screens'
                               slashSeparator 'screen_'
                               objectName PERIOD
                               fileExtension into newFilenameWithPath.

          concatenate userPath slashSeparator 'screens' into newSubDirectory.
          concatenate userPath slashSeparator 'screens' into completePath.

        else.
          concatenate userpath slashSeparator additionalSubdirectory
                               slashSeparator 'Screens'
                               slashSeparator 'screen_'
                               objectName PERIOD
                               fileExtension into newFilenameWithPath.

          concatenate userPath slashSeparator additionalSubDirectory slashSeparator 'screens' into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory slashSeparator 'screens' into completePath.
        endif.

*     GUI title
      when IS_GUITITLE.
        if additionalSubDirectory is initial.
          concatenate userpath slashSeparator 'Screens'
                               slashSeparator 'gui_title_'
                               objectName PERIOD
                               fileExtension into newFilenameWithPath.

          concatenate userPath slashSeparator 'screens' into newSubDirectory.
          concatenate userPath slashSeparator 'screens' into completePath.
        else.
          concatenate userpath slashSeparator additionalSubdirectory
                               slashSeparator 'Screens'
                               slashSeparator 'gui_title_'
                               objectName PERIOD
                               fileExtension into newFilenameWithPath.

          concatenate userPath slashSeparator additionalSubDirectory slashSeparator 'Screens' into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory slashSeparator 'Screens' into completePath.
        endif.

*     Message Class
      when IS_MESSAGECLASS.
        if additionalSubDirectory is initial.
          concatenate userPath slashSeparator objectName
                               slashSeparator 'Message class-'
                               objectName PERIOD
                               fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userPath slashSeparator additionalSubDirectory
                               slashSeparator objectName
                               slashSeparator 'Message class-'
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userpath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userpath slashSeparator additionalSubDirectory slashSeparator objectName into completePath.
      endif.

*     Class definition
      when IS_CLASS.
        if additionalSubdirectory is initial.
          concatenate userPath slashSeparator objectName
                               slashSeparator 'Class-'
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userpath slashSeparator additionalSubDirectory
                               slashSeparator objectName
                               slashSeparator 'Class-'
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into completePath.
        endif.

*     Class definition
      when IS_METHOD.
        if additionalSubdirectory is initial.
          concatenate userPath slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

          concatenate userPath slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator objectName into completePath.
        else.
          concatenate userpath slashSeparator additionalSubDirectory
                               slashSeparator
                               objectName PERIOD fileExtension
                               into newFilenameWithPath.

*          concatenate userPath slashSeparator additionalSubDirectory slashSeparator objectName into newSubDirectory.
          concatenate userPath slashSeparator additionalSubDirectory into completePath.
        endif.
    endcase.

    translate completePath to lower case.
    concatenate objectName PERIOD fileExtension into newFilenameOnly.
    translate newFilenameOnly to lower case.
    translate newFilenameWithPath to lower case.
    translate newSubDirectory to lower case.

*   If we are running on a non UNIX environment we will need to remove incorrect characters from the filename.
    if downloadToServer is initial.
      if frontendOpSystem = NON_UNIX.
        translate newFilenameOnly using '/_'.
        translate newFilenameWithPath using '/_'.
        translate newFilenameOnly using '< '.
        translate newFilenameWithPath using '< '.
        translate newFilenameOnly using '> '.
        translate newFilenameWithPath using '> '.
        translate newFilenameOnly using '? '.
        translate newFilenameWithPath using '? '.
        translate newFilenameOnly using '| '.
        translate newFilenameWithPath using '| '.
        condense newFilenameOnly no-gaps.
        condense newFilenameWithPath no-gaps.
      endif.
    else.
      if serverOpSystem = NON_UNIX.
        translate newFilenameOnly using '/_'.
        translate newFilenameWithPath using '/_'.
        translate newFilenameOnly using '< '.
        translate newFilenameWithPath using '< '.
        translate newFilenameOnly using '> '.
        translate newFilenameWithPath using '> '.
        translate newFilenameOnly using '? '.
        translate newFilenameWithPath using '? '.
        translate newFilenameOnly using '| '.
        translate newFilenameWithPath using '| '.
        condense newFilenameOnly no-gaps.
        condense newFilenameWithPath no-gaps.
      endif.
    endif.
  endform.                                                                                 "buildFilename

*  -------------------------------------------------------------------------------------------------------
*    saveFileToPc...    write an internal table to a file on the local PC
*  -------------------------------------------------------------------------------------------------------
  form saveFileToPc using iDownload type standard table
                          value(filenameWithPath)
                          value(filename)
                          value(writeFieldSeparator)
                          value(truncateTrailingBlanks)
                          value(displayProgressMessage).

  data: statusMessage type string.
  data: objFile type ref to cl_gui_frontend_services.
  data: strSubrc type string.

    if not displayProgressMessage is initial.
      concatenate `Downloading: ` filename into statusMessage.
      perform displayStatus using statusMessage 0.
    endif.

    create object objFile.
    objFile->gui_download( exporting filename = filenameWithPath
                                      filetype = 'ASC'
                                      write_field_separator = writeFieldSeparator
                                      trunc_trailing_blanks = truncateTrailingBlanks
                             changing data_tab = iDownload[]
                           exceptions file_write_error        = 1
                                      no_batch                = 2
                                      gui_refuse_filetransfer = 3
                                      invalid_type            = 4
                                      no_authority            = 5
                                      unknown_error           = 6
                                      header_not_allowed      = 7
                                      separator_not_allowed   = 8
                                      filesize_not_allowed    = 9
                                      header_too_long         = 10
                                      dp_error_create         = 11
                                      dp_error_send           = 12
                                      dp_error_write          = 13
                                      unknown_dp_error        = 14
                                      access_denied           = 15
                                      dp_out_of_memory        = 16
                                      disk_full               = 17
                                      dp_timeout              = 18
                                      file_not_found          = 19
                                      dataprovider_exception  = 20
                                      control_flush_error     = 21
                                      not_supported_by_gui    = 22
                                      error_no_gui            = 23 ).

     if sy-subrc <> 0.
       strSubrc = sy-subrc.
       concatenate `File save error: ` filename ` sy-subrc: ` strSubrc into statusMessage.
       perform displayStatus using statusMessage 3.
     endif.
  endform.                                                                                                  "saveFileToPc

  form saveFileToPcWH using iDownload type standard table
                          value(filenameWithPath)
                          value(filename)
                          value(writeFieldSeparator)
                          value(truncateTrailingBlanks)
                          value(displayProgressMessage)
                          value(tabName).

  types: begin of ty_head, "Structure for header
               name type dd03p-fieldname,
               END OF ty_head.
  data: lt_head type table of ty_head,
        ls_head type ty_head,
        lt_dd03p type table of dd03p,
        ls_dd03p like line of lt_dd03p.

  data: statusMessage type string.
*  data: objFile type ref to cl_gui_frontend_services.
  data: strSubrc type string.

    if not displayProgressMessage is initial.
      concatenate `Downloading: ` filename into statusMessage.
      perform displayStatus using statusMessage 0.
    endif.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name                = tabName
      TABLES
        DD03P_TAB           = lt_dd03p
      EXCEPTIONS
        ILLEGAL_INPUT       = 1
        OTHERS              = 2.

    loop at lt_dd03p into ls_dd03p.
      ls_head-name = ls_dd03p-fieldname.
      append ls_head to lt_head.
    endloop.

*    create object objFile.
*    objFile->gui_download( exporting filename = filenameWithPath
*                                      filetype = 'ASC'
*                                      write_field_separator = writeFieldSeparator
*                                      trunc_trailing_blanks = truncateTrailingBlanks
*                                      fieldnames            = lt_head
*                             changing data_tab = iDownload[]
*                           exceptions file_write_error        = 1
*                                      no_batch                = 2
*                                      gui_refuse_filetransfer = 3
*                                      invalid_type            = 4
*                                      no_authority            = 5
*                                      unknown_error           = 6
*                                      header_not_allowed      = 7
*                                      separator_not_allowed   = 8
*                                      filesize_not_allowed    = 9
*                                      header_too_long         = 10
*                                      dp_error_create         = 11
*                                      dp_error_send           = 12
*                                      dp_error_write          = 13
*                                      unknown_dp_error        = 14
*                                      access_denied           = 15
*                                      dp_out_of_memory        = 16
*                                      disk_full               = 17
*                                      dp_timeout              = 18
*                                      file_not_found          = 19
*                                      dataprovider_exception  = 20
*                                      control_flush_error     = 21
*                                      not_supported_by_gui    = 22
*                                      error_no_gui            = 23 ).

      call function 'GUI_DOWNLOAD'
        exporting
          filename                        = filenameWithPath
          filetype                        = 'ASC'
          write_field_separator           = writeFieldSeparator
          trunc_trailing_blanks           = truncateTrailingBlanks
        tables
          data_tab                        = iDownload[]
          fieldnames                      = lt_head
        exceptions
          FILE_WRITE_ERROR                = 1
          NO_BATCH                        = 2
          GUI_REFUSE_FILETRANSFER         = 3
          INVALID_TYPE                    = 4
          NO_AUTHORITY                    = 5
          UNKNOWN_ERROR                   = 6
          HEADER_NOT_ALLOWED              = 7
          SEPARATOR_NOT_ALLOWED           = 8
          FILESIZE_NOT_ALLOWED            = 9
          HEADER_TOO_LONG                 = 10
          DP_ERROR_CREATE                 = 11
          DP_ERROR_SEND                   = 12
          DP_ERROR_WRITE                  = 13
          UNKNOWN_DP_ERROR                = 14
          ACCESS_DENIED                   = 15
          DP_OUT_OF_MEMORY                = 16
          DISK_FULL                       = 17
          DP_TIMEOUT                      = 18
          FILE_NOT_FOUND                  = 19
          DATAPROVIDER_EXCEPTION          = 20
          CONTROL_FLUSH_ERROR             = 21
          OTHERS                          = 22.
     if sy-subrc <> 0.
       strSubrc = sy-subrc.
       concatenate `File save error: ` filename ` sy-subrc: ` strSubrc into statusMessage.
       perform displayStatus using statusMessage 3.
     endif.
  endform.
*  ----------------------------------------------------------------------------------------------------------------------
*    saveFileToServer...    write an internal table to a file on the SAP server
*  ----------------------------------------------------------------------------------------------------------------------
  form saveFileToServer using iDownload type standard table
                              value(filenameWithPath)
                              value(filename)
                              value(path)
                              value(displayProgressMessage)
                               value(locServerFileSystem).

  data: waDownload type string.
  data: statusMessage type string.

    if not displayProgressMessage is initial.
      concatenate `Downloading: ` filename into statusMessage.
      perform displayStatus using statusMessage 0.
    endif.

    read table iServerPaths with key table_line = path.
    if sy-subrc <> 0.
      perform createServerDirectory using path locServerFileSystem.
      append path to iServerPaths.
    endif.

    open dataset filenameWithPath for output in text mode encoding default.
    if sy-subrc = 0.
      loop at iDownload into waDownload.
        transfer waDownload to filenameWithPath.
        if sy-subrc <> 0.
          message e000(oo) with 'Error transferring data to file'.
        endif.
      endloop.

      close dataset filenameWithPath.
      if sy-subrc <> 0.
        message e000(oo) with 'Error closing file'.
      endif.
    else.
*     Unable to create a file
      message e000(oo) with 'Error creating file on SAP server' 'check permissions'.
    endif.
  endform.                                                                                              "saveFileToServer

*  ----------------------------------------------------------------------------------------------------------------------
*   createServerDirectory...
*  ----------------------------------------------------------------------------------------------------------------------
  form createServerDirectory using value(path)
                                   value(locServerFileSystem).

  data: castServerOpSys type syopsys.

    castServerOpSys = locServerFileSystem.

*    Parameters for remove command.
  data: param1 type sxpgcolist-parameters.
*    Return status
  data: funcStatus type extcmdexex-status.
*    Command line listing returned by the function
  data: iServerOutput type standard table of btcxpm.
  data: waServeroutput type btcxpm.
*    Targetsystem type conversion variable.
  data: target type rfcdisplay-rfchost.
*   Operating system
  data: operatingSystem type sxpgcolist-opsystem.
*    Head for split command.
  data: head type string..
  data: tail type string.

  param1 = path.
  target = sy-host.
  operatingSystem = locServerFileSystem.

  call function 'SXPG_COMMAND_EXECUTE'
     exporting
       commandname                         = 'ZDTX_MKDIR'
       additional_parameters               = param1
       operatingsystem                     = castServerOpSys
       targetsystem                        = target
       stdout                              = 'X'
       stderr                              = 'X'
       terminationwait                     = 'X'
     importing
       status                              = funcStatus
     tables
       exec_protocol                       = iServerOutput[]
     exceptions
      no_permission                       = 1
      command_not_found                   = 2
      parameters_too_long                 = 3
      security_risk                       = 4
      wrong_check_call_interface          = 5
      program_start_error                 = 6
      program_termination_error           = 7
      x_error                             = 8
      parameter_expected                  = 9
      too_many_parameters                 = 10
      illegal_command                     = 11
      wrong_asynchronous_parameters       = 12
      cant_enq_tbtco_entry                = 13
      jobcount_generation_error           = 14
      others                              = 15.

    if sy-subrc = 0.
*     Although the function succeded did the external command actually work
      if funcStatus = 'E'.
*       External command returned with an error
        if sy-opsys cs 'Windows NT'.
          read table iServeroutput index 1 into waServeroutput.
          if waServeroutput-message ns 'already exists'.
*           An error occurred creating the directory on the server
            message e000(oo) with 'An error occurred creating a directory'.
          endif.
        else.
          read table iServerOutput index 2 into waServeroutput.
          split waServeroutput-message at space into head tail.
          shift tail left deleting leading space.
          if tail <> 'Do not specify an existing file.'.
*           An error occurred creating the directory on the server
            message e000(oo) with 'An error occurred creating a directory'.
          endif.
        endif.
      endif.
    else.
      case sy-subrc.
        when 1.
*         No permissions to run the command
          message e000(oo) with 'No permissions to run external command ZDTX_MKDIR'.
        when 2.
*         External command not found
          message E000(oo) with 'External comand ZDTX_MKDIR not found'.

        when others.
*         Unable to create the directory
          message e000(oo) with 'An error occurred creating a directory'
                                ', subrc:'
                                sy-subrc.
      endcase.
    endif.
  endform.                                                                                         "createServerDirectory

*  ----------------------------------------------------------------------------------------------------------------------
*   appendTextElements...
*  ----------------------------------------------------------------------------------------------------------------------
  form appendTextElements using iLocTextElements like dumiTextTab[]
                                iLocLines like dumiHtml[].

  field-symbols: <waTextElement> type tTextTable.
  data: waLine type string.

    if lines( iLocTextElements ) > 0.
      append '' to iLocLines.

      append '*Text elements' to iLocLines.
      append '*----------------------------------------------------------' to  iLocLines.
      loop at iLocTextElements assigning <waTextElement>.
        concatenate '*  ' <waTextElement>-key <waTextElement>-entry into waLine separated by space.
        append waLine to iLocLines.
      endloop.
    endif.
  endform.                                                                                            "appendTextElements

*  ----------------------------------------------------------------------------------------------------------------------
*   appendGUITitles...
*  ----------------------------------------------------------------------------------------------------------------------
  form appendGUITitles using iLocGUItitles like dumiGUITitle[]
                             iLocLines like dumiHtml[].

  field-symbols: <waGUITitle> type tGUITitle.
  data: waLine type string.

    if lines( iLocGUItitles ) > 0.
      append '' to iLocLines.

      append '*GUI Texts' to iLocLines.
      append '*----------------------------------------------------------' to  iLocLines.
      loop at iLocGUItitles assigning <waGUItitle>.
        concatenate '*  ' <waGUItitle>-obj_code '-->' <waGUItitle>-text into waLine separated by space.
        append waLine to iLocLines.
      endloop.
    endif.
  endform.                                                                                               "appendGUITitles

*  ----------------------------------------------------------------------------------------------------------------------
*   appendSelectionTexts...
*  ----------------------------------------------------------------------------------------------------------------------
  form appendSelectionTexts using iLocSelectionTexts like dumiTextTab[]
                                  iLocLines like dumiHtml[].

  field-symbols: <waSelectionText> type tTextTable.
  data: waLine type string.

    if lines( iLocSelectionTexts ) > 0.
      append '' to iLocLines.
      append '' to iLocLines.

      append '*Selection texts' to iLocLines.
      append '*----------------------------------------------------------' to  iLocLines.
      loop at iLocSelectiontexts assigning <waSelectionText>.
        concatenate '*  ' <waSelectionText>-key <waSelectionText>-entry into waLine separated by space.
        append waLine to iLocLines.
      endloop.
    endif.
  endform.                                                                                          "appendSelectionTexts

*  ----------------------------------------------------------------------------------------------------------------------
*   appendExceptionTexts...
*  ----------------------------------------------------------------------------------------------------------------------
  form appendExceptionTexts using iConcepts like dumiConcepts[]
                                  iLocLines like dumiHtml[].

  field-symbols: <waConcept> type tConcept.
  data: waLine type string.
  data: conceptText type sotr_txt.

    if lines( iConcepts ) > 0.
      append '' to iLocLines.

      append '*Exception texts' to iLocLines.
      append '*----------------------------------------------------------' to  iLocLines.
      loop at iConcepts assigning <waConcept>.
*       Find the text for this concept
        call function 'SOTR_GET_TEXT_KEY' exporting concept = <waConcept>-concept
                                                    langu = pMLang
                                                    search_in_second_langu = 'X'
*                                                    second_langu = 'DE'
                                          importing e_text = conceptText
                                          exceptions no_entry_found = 1
                                                     parameter_error = 2
                                                     others  = 3.

        if sy-subrc = 0.
          concatenate '*  ' <waConcept>-constName '-' conceptText  into waLine separated by space.
          append waLine to iLocLines.
        endif.
      endloop.
    endif.
  endform.                                                                                          "appendExceptionTexts

*  ----------------------------------------------------------------------------------------------------------------------
*   downloadFunctionDocs...
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadFunctionDocs using value(functionName)
                                  value(functionDescription)
                                  value(userFilePath)
                                  value(fileExtension)
                                  value(convertToHtml)
                                  value(slashSeparator)
                                  value(saveToServer)
                                  value(displayProgressMessage)
                                        subDir
                                        documentationDownloaded
                                  value(locServerFileSystem)
                                  value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: iDocumentation type standard table of funct with header line.
  data: iExceptions type standard table of rsexc with header line.
  data: iExport type standard table of rsexp with header line.
  data: iParameter type standard table of rsimp with header line.
  data: iTables type standard table of rstbl with header line.
  data: iScriptLines type standard table of tline with header line.
  data: htmlPageName type string.
  data: newFilenameWithPath type string.
  data: newFilenameOnly type string.
  data: object like dokhl-object.
  data: stringLength type i value 0.
  data: newSubDirectory type string.
  data: waLine(255).
  data: completeSavePath type string.

    documentationDownloaded = FALSE.
    object = functionName.

    call function 'FUNCTION_IMPORT_DOKU'
         exporting
              funcName           = functionName
         tables
              DOKUMENTATION      = iDocumentation
              EXCEPTION_LIST     = iExceptions
              EXPORT_PARAMETER   = iExport
              IMPORT_PARAMETER   = iParameter
              TABLES_PARAMETER   = iTables
         exceptions
              ERROR_MESSAGE      = 1
              FUNCTION_NOT_FOUND = 2
              INVALID_NAME       = 3
              others             = 4.

    call function 'DOCU_GET'
         exporting
              ID                     = 'FU'
              LANGU                  = pMLang
              OBJECT                 = object
              TYP                    = 'T'
              VERSION_ACTIVE_OR_LAST = 'L'
         tables
              line                   = iScriptLines
         exceptions
              NO_DOCU_ON_SCREEN      = 1
              NO_DOCU_SELF_DEF       = 2
              NO_DOCU_TEMP           = 3
              RET_CODE               = 4
              others                 = 5.

    if sy-subrc = 0 and not ( iScriptLines[] is initial ).
      append 'SHORT TEXT' to iLines.
      concatenate space functionDescription into functionDescription separated by space.
      append functionDescription to iLines.
      append space to iLines.
      loop at iScriptLines.
        move iScriptLines-tdline to iLines.
        concatenate space iLines into iLines separated by space.
        while iLines CP '&*' or iLines CP '*&'.
          replace '&' into iLines with space.
          shift iLines left deleting leading space.
        endwhile.
        append iLines.
      endloop.

      clear iLines.
      if not ( iDocumentation[] is initial ).
        append iLines.
        append 'PARAMETER DOCUMENTATION' to iLines.
        append '-----------------------' to iLines.
        append iLines.

        describe field iDocumentation-parameter length stringLength in character mode.
        stringLength = stringLength + 3.
        loop at iDocumentation.
          move iDocumentation-parameter to waLine.
          move iDocumentation-stext to waLine+stringLength.
          append waLine to iLines.
        endloop.
      endif.

      concatenate `Documentation - ` functionName into htmlPageName.

      if convertToHtml is initial.
        append iLines.
        append  '----------------------------------------------------------------------------------' to iLines.
        append iLines.
        perform buildFooterMessage using iLines.
        append iLines.
      else.
        perform convertCodeToHtml using iLines[]
                                        htmlPageName
                                        space
                                        IS_DOCUMENTATION
                                        TRUE
                                        space
                                        space
                                        space
                                        space
                                        space
                                        addBackground.
      endif.

      perform buildFilename using userFilePath
                                  subDir
                                  functionName
                                  space
                                  space
                                  fileExtension
                                  IS_DOCUMENTATION
                                  saveToServer
                                  slashSeparator
                                  newFilenameWithPath
                                  newFilenameOnly
                                  newSubDirectory
                                  completeSavePath.

      if saveToServer is initial.
        perform saveFileToPC using iLines[]
                                   newFilenameWithPath
                                   newfilenameOnly
                                   space
                                   space
                                   displayProgressMessage.
      else.
        perform saveFileToServer using iLines[]
                                       newFilenameWithPath
                                       newfilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

      documentationDownloaded = TRUE.
    endif.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*   downloadClassDocs...
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadClassDocs using value(className) type seoclsname
                               value(userFilePath)
                               value(fileExtension)
                               value(convertToHtml)
                               value(slashSeparator)
                               value(saveToServer)
                               value(displayProgressMessage)
                                     subDir
                                     documentationDownloaded
                               value(locServerFileSystem)
                               value(addBackground) type abap_bool.

  data: iLines type standard table of string with header line.
  data: iDocumentation type standard table of funct with header line.
  data: iExceptions type standard table of rsexc with header line.
  data: iExport type standard table of rsexp with header line.
  data: iParameter type standard table of rsimp with header line.
  data: iTables type standard table of rstbl with header line.
  data: iScriptLines type standard table of tline with header line.
  data: htmlPageName type string.
  data: newFilenameWithPath type string.
  data: newFilenameOnly type string.
  data: object like dokhl-object.
  data: stringLength type i value 0.
  data: newSubDirectory type string.
  data: waLine(255).
  data: completeSavePath type string.

    documentationDownloaded = FALSE.
    object = className.

    call function 'DOC_OBJECT_GET'
      exporting
        class                  = 'CL'
        name                   = object
        language               = pMLang
*       short_text             = ' '
*       appendix               = ' '
*     importing
*       header                 = header
     tables
       itf_lines              = iScriptLines[]
     exceptions
       object_not_found       = 1.

    if sy-subrc = 0 and not ( iScriptLines[] is initial ).
      loop at iScriptLines.
        move iScriptLines-tdline to iLines.
        concatenate space iLines into iLines separated by space.
        while iLines CP '&*' or iLines CP '*&'.
          replace '&' into iLines with space.
          shift iLines left deleting leading space.
        endwhile.
        append iLines.
      endloop.

      concatenate `Documentation - ` className into htmlPageName.

      if convertToHtml is initial.
        append iLines.
        append  '----------------------------------------------------------------------------------' to iLines.
        append iLines.
        perform buildFooterMessage using iLines.
        append iLines.
      else.
        perform convertCodeToHtml using iLines[]
                                        htmlPageName
                                        space
                                        IS_DOCUMENTATION
                                        TRUE
                                        space
                                        space
                                        space
                                        space
                                        space
                                        addBackground.
      endif.

      perform buildFilename using userFilePath
                                  subDir
                                  className
                                  space
                                  space
                                  fileExtension
                                  IS_DOCUMENTATION
                                  saveToServer
                                  slashSeparator
                                  newFilenameWithPath
                                  newFilenameOnly
                                  newSubDirectory
                                  completeSavePath.

      if saveToServer is initial.
        perform saveFileToPC using iLines[]
                                   newFilenameWithPath
                                   newfilenameOnly
                                   space
                                   space
                                   displayProgressMessage.
      else.
        perform saveFileToServer using iLines[]
                                       newFilenameWithPath
                                       newfilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

      documentationDownloaded = TRUE.
    endif.
  endform.
*  ----------------------------------------------------------------------------------------------------------------------
*    downloadScreens...
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadScreens using iLocScreenFlow like dumiScreen[]
                             value(programName)
                             value(userFilePath)
                             value(textFileExtension)
                             value(subdir)
                             value(slashSeparator)
                             value(saveToServer)
                             value(displayProgressMessage)
                             value(locServerFileSystem).


  tables: d020t.
  data: header like d020s.
  data: iFields type standard table of d021s with header line.
  data: iFlowLogic type standard table of d022s with header line.
  field-symbols <waScreen> type tScreenFlow.
  data: waCharHeader type scr_chhead.
  data: iScreenChar type standard table of scr_chfld with header line.
  data: iFieldsChar type standard table of scr_chfld with header line.
  data: stars type string value '****************************************************************'.
  data: comment1 type string value '*   This file was generated by Direct Download Enterprise.     *'.
  data: comment2 type string value '*   Please do not change it manually.                          *'.
  data: dynproText type string value '%_DYNPRO'.
  data: headerText type string value '%_HEADER'.
  data: paramsText type string value '%_PARAMS'.
  data: descriptionText type string value '%_DESCRIPTION'.
  data: fieldsText type string value '%_FIELDS'.
  data: flowlogicText type string value '%_FLOWLOGIC'.
  data: programLength type string.
  data: newSubDirectory type string.
  data: newFilenameWithPath type string.
  data: newFilenameOnly type string.
  data: completeSavePath type string.

    loop at iLocScreenFlow assigning <waScreen>.
      call function 'RS_IMPORT_DYNPRO'
           exporting
                dylang = pMLang
                dyname = programName
                dynumb = <waScreen>-screen
           importing
                header = header
           tables
                ftab   = iFields
                pltab  = iFlowLogic.

      call function 'RS_SCRP_HEADER_RAW_TO_CHAR'
           exporting
                header_int  = header
           importing
                header_char = waCharHeader
           exceptions
                others      = 1.

*     Add in the top comments for the file
      append stars to iScreenChar .
      append comment1 to iScreenChar.
      append comment2 to iScreenChar.
      append stars to iScreenChar.

*     Screen identification
      append dynproText to iScreenChar.
      append waCharHeader-prog to iScreenChar.
      append waCharHeader-dnum to iScreenChar.
      append sy-saprl to iScreenChar.
      describe field d020t-prog length programLength in character mode.
      concatenate `                ` programLength into iScreenChar.
      append iScreenChar.

*     Header
      append headerText to iScreenChar.
      append waCharHeader to iScreenChar.

*     Description text
      append descriptionText to iScreenChar.
      select single dtxt from d020T into iScreenChar
                         where prog = programName
                               and dynr = <waScreen>-screen
                               and lang = pMLang.
      append iScreenChar.

*     Fieldlist text
      append fieldsText to iScreenChar.

      call function 'RS_SCRP_FIELDS_RAW_TO_CHAR'
           tables
                fields_int  = iFields[]
                fields_char = iFieldsChar[]
           exceptions
                others      = 1.

      loop at iFieldsChar.
        move-corresponding iFieldsChar to iScreenChar.
        append iScreenChar.
      endloop.

*     Flowlogic text
      append flowlogicText to iScreenChar.
*     Flow logic.
      loop at iFlowLogic.
        append iFlowLogic to iScreenChar.
      endloop.

      perform buildFilename using userFilePath
                                  subDir
                                  waCharHeader-dnum
                                  space
                                  space
                                  textFileExtension
                                  IS_SCREEN
                                  saveToServer
                                  slashSeparator
                                  newFilenameWithPath
                                  newfilenameOnly
                                  newSubDirectory
                                  completeSavePath.

      if saveToServer is initial.
*       Save the screen to the local computer
        perform saveFileToPc using iScreenChar[]
                                   newFilenameWithPath
                                   newFilenameOnly
                                   'X'
                                   'X'
                                   displayProgressMessage.
      else.
*       Save the screen to the SAP server
        perform saveFileToServer using iScreenChar[]
                                       newFilenameWithPath
                                       newFilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

      clear header. clear waCharHeader.
      clear iScreenChar[].
      clear iFieldsChar[].
      clear iFields[].
      clear iFlowLogic[].
    endloop.
  endform.                                                                                               "downloadScreens

*  ----------------------------------------------------------------------------------------------------------------------
*    downloadGUITitles..
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadGUITitles using iLocGUITitles like dumIGUITitle[]
                               value(userFilePath)
                               value(textFileExtension)
                               value(subDir)
                               value(slashSeparator)
                               value(saveToServer)
                               value(displayProgressMessage)
                               value(locServerFileSystem).

  data: iLines type standard table of string with header line.
  field-symbols: <waGUITitle> type tGUITitle.
  data: newSubDirectory type string.
  data: newFilenameWithPath type string.
  data: newFilenameOnly type string.
  data: completeSavePath type string.

    loop at iLocGUITitles assigning <waGUITitle>.
      append <waGUITitle>-text to iLines.

      perform buildFilename using userFilePath
                                  subDir
                                  <waGUITitle>-obj_code
                                  space
                                  space
                                  textFileExtension
                                  IS_GUITITLE
                                  saveToServer
                                  slashSeparator
                                  newFilenameWithPath
                                  newfilenameOnly
                                  newSubDirectory
                                  completeSavePath.

      if saveToServer is initial.
        perform saveFileToPc using iLines[]
                                   newFilenameWithPath
                                   newFilenameOnly
                                   space
                                   space
                                   displayProgressMessage.
      else.
        perform saveFileToServer using iLines[]
                                       newFilenameWithPath
                                       newFilenameOnly
                                       completeSavePath
                                       displayProgressMessage
                                       locServerFileSystem.
      endif.

      clear iLines[].
    endloop.
  endform.                                                                                             "downloadGUITitles

*  ----------------------------------------------------------------------------------------------------------------------
*    downloadPrograms..
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadPrograms using iLocProgram like iPrograms[]
                              iLocFunctions like iFunctions[]
                              iLocDictionaryFilename like iDictFilename[]
                              iLocTableTypeFilename like iTableTypeFilename[]
                              value(userFilePath)
                              value(fileExtension)
                              value(htmlFileExtension)
                              value(textFileExtension)
                              value(convertToHtml)
                              value(customerNameRange)
                              value(getIncludes)
                              value(getDictStruct)
                              value(downloadDocumentation)
                              value(sortTablesAsc)
                              value(slashSeparator)
                              value(saveToServer)
                              value(displayProgressMessage)
                              value(locServerFileSystem)
                              value(addBackground) type abap_bool.


  data: iProgFunctions type standard table of tFunction with header line.
  field-symbols: <waProgram> type tProgram.
  field-symbols: <waInclude> type tInclude.
  data: iEmptyTextelements type standard table of tTextTable.
  data: iEmptySelectionTexts type standard table of tTextTable.
  data: iEmptyMessages type standard table of tMessage.
  data: iEmptyGuiTitles type standard table of tGUITitle.
  data: locConvertToHtml(1).
  data: locFileExtension type string.

    sort iLocProgram ascending by progname.

    loop at iLocProgram assigning <waProgram>.
*     if the program to download is this program then always download as text otherwise you will get a rubbish file
      if <waprogram>-progname = sy-cprog.
        locConvertToHtml = ''.
        locFileExtension = TEXTEXTENSION.
      else.
        locConvertToHtml = convertToHtml.
        locFileExtension = fileExtension.
      endif.

*     Download the main program
      perform readIncludeAndDownload using <waProgram>-iTextelements[]
                                           <waProgram>-iSelectionTexts[]
                                           <waProgram>-iMessages[]
                                           <waProgram>-iGUITitle[]
                                           <waprogram>-progname
                                           space
                                           <waprogram>-programTitle
                                           IS_PROGRAM
                                           userFilePath
                                           locFileExtension
                                           <waprogram>-progname
                                           locConvertToHtml
                                           customerNameRange
                                           getIncludes
                                           getDictStruct
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.

*     Download screens.
      if not <waProgram>-iScreenFlow[] is initial.
        perform downloadScreens using <waProgram>-iScreenFlow[]
                                      <waProgram>-progname
                                      userFilePath
                                      textFileExtension
                                      <waprogram>-progname
                                      slashSeparator
                                      saveToServer
                                      displayProgressMessage
                                      locServerFileSystem.
      endif.

*     Download GUI titles
      if not <waProgram>-iGUITitle[] is initial.
        perform downloadGUITitles using <waProgram>-iGUITitle
                                        userFilePath
                                        textFileExtension
                                        <waProgram>-progName
                                        slashSeparator
                                        saveToServer
                                        displayProgressMessage
                                        locServerFileSystem.
      endif.

*     Download all other includes
      loop at <waProgram>-iIncludes assigning <waInclude>.
        perform readIncludeAndDownload using iEmptyTextelements[]
                                             iEmptySelectionTexts[]
                                             iEmptyMessages[]
                                             iEmptyGuiTitles[]
                                             <waInclude>-IncludeName
                                             space
                                             <waInclude>-IncludeTitle
                                             IS_PROGRAM
                                             userFilePath
                                             fileExtension
                                             <waProgram>-progName
                                             convertToHtml
                                             customerNameRange
                                             getIncludes
                                             getDictStruct
                                             slashSeparator
                                             saveToServer
                                             displayProgressMessage
                                             locServerFileSystem
                                             addBackground.

      endloop.

*     Download all dictionary structures
      if not <waProgram>-iDictStruct[] is initial.
        perform downloadDDStructures using <waProgram>-iDictStruct[]
                                           iLocDictionaryFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           <waProgram>-progName
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download all Table Types
      if not <waProgram>-iTableTypes[] is initial.
        perform downloadDDTableTypes using <waProgram>-iTableTypes[]
                                           iLocTableTypeFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           <waProgram>-progName
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download Transformations
      if not <waProgram>-iTransformations[] is initial.
        perform downloadXSLT using <waProgram>-iTransformations[]
                                   userFilePath
                                   fileExtension
                                   htmlFileExtension
                                   textFileExtension
                                   convertToHtml
                                   customerNamespace
                                   slashSeparatorToUse
                                   saveToServer
                                   <waProgram>-progName
                                   displayProgressMessage
                                   serverFileSystem
                                   addBackground.
      endif.

*     Download any functions used by these programs
      loop at iLocFunctions into iProgFunctions where programLinkName = <waProgram>-progName.
        append iProgFunctions.
      endloop.

      if not iProgFunctions[] is initial.
        perform downloadFunctions using iProgFunctions[]
                                        iLocDictionaryFilename[]
                                        iTableTypeFilename[]
                                        userFilePath
                                        fileExtension
                                        <waProgram>-progName
                                        downloadDocumentation
                                        convertToHtml
                                        customerNameRange
                                        getIncludes
                                        getDictStruct
                                        textFileExtension
                                        htmlFileExtension
                                        sortTablesAsc
                                        slashSeparator
                                        saveToServer
                                        displayProgressMessage
                                        locServerFileSystem
                                        addBackground.
         clear iProgFunctions[].
       endif.
    endloop.
  endform.                                                                                              "downloadPrograms

*  ----------------------------------------------------------------------------------------------------------------------
*    downloadClasses..
*  ----------------------------------------------------------------------------------------------------------------------
  form downloadClasses using iLocClasses like iClasses[]
                             iLocFunctions like iFunctions[]
                             iLocDictionaryFilename like iDictFilename[]
                             iLocTableTypeFilename like iTableTypeFilename[]
                             value(userFilePath)
                             value(fileExtension)
                             value(htmlFileExtension)
                             value(textFileExtension)
                             value(convertToHtml)
                             value(customerNameRange)
                             value(getIncludes)
                             value(getDictStruct)
                             value(downloadDocumentation)
                             value(sortTablesAsc)
                             value(slashSeparator)
                             value(saveToServer)
                             value(displayProgressMessage)
                             value(locServerFileSystem)
                             value(addBackground) type abap_bool.


  data: iClassFunctions type standard table of tFunction with header line.
  field-symbols: <waClass> type tClass.
  field-symbols: <waMethod> type tMethod.
  data: additionalSubDirectory type string.
  data: classDocumentationExists type abap_bool value FALSE.

    sort iLocClasses ascending by clsname.

    loop at iLocClasses assigning <waClass>.
*     Download the class
      perform readClassAndDownload using <waClass>
                                          <waClass>-clsname
                                          space
                                          IS_CLASS
                                          userFilePath
                                          fileExtension
                                          space
                                          convertToHtml
                                          customerNameRange
                                          getIncludes
                                          getDictStruct
                                          slashSeparator
                                          saveToServer
                                          displayProgressMessage
                                          locServerFileSystem
                                          addBackground.


*     Download all of the methods
      loop at <waClass>-iMethods assigning <waMethod>.
        additionalSubDirectory = <waClass>-clsName.
      perform readMethodAndDownload using <waMethod>
                                          <waMethod>-cmpName
                                          <waMethod>-methodKey
                                          space
                                          IS_METHOD
                                          userFilePath
                                          fileExtension
                                          additionalSubDirectory
                                          convertToHtml
                                          customerNameRange
                                          getIncludes
                                          getDictStruct
                                          slashSeparator
                                          saveToServer
                                          displayProgressMessage
                                          locServerFileSystem
                                          addBackground.

      endloop.

*     Download all dictionary structures
      if not <waClass>-iDictStruct[] is initial.
        perform downloadDDStructures using <waClass>-iDictStruct[]
                                           iLocDictionaryFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           <waClass>-clsName
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download all Table Types
      if not <waClass>-iTableTypes[] is initial.
        perform downloadDDTableTypes using <waClass>-iTableTypes[]
                                           iLocTableTypeFilename[]
                                           userFilePath
                                           htmlFileExtension
                                           <waClass>-clsName
                                           sortTablesAsc
                                           slashSeparator
                                           saveToServer
                                           displayProgressMessage
                                           locServerFileSystem
                                           addBackground.
      endif.

*     Download Transformations
      if not <waClass>-iTransformations[] is initial.
        perform downloadXSLT using <waClass>-iTransformations[]
                                   userFilePath
                                   fileExtension
                                   htmlFileExtension
                                   textFileExtension
                                   convertToHtml
                                   customerNamespace
                                   slashSeparatorToUse
                                   saveToServer
                                   <waClass>-clsName
                                   displayProgressMessage
                                   serverFileSystem
                                   addBackground.
      endif.

*     Download any functions used by these programs
      loop at iLocFunctions into iClassFunctions where programLinkName = <waClass>-clsName.
        append iClassFunctions.
      endloop.

      if not iClassFunctions[] is initial.
        perform downloadFunctions using iClassFunctions[]
                                        iLocDictionaryFilename[]
                                        iLocTableTypeFilename[]
                                        userFilePath
                                        fileExtension
                                        <waClass>-clsName
                                        downloadDocumentation
                                        convertToHtml
                                        customerNameRange
                                        getIncludes
                                        getDictStruct
                                        textFileExtension
                                        htmlFileExtension
                                        sortTablesAsc
                                        slashSeparator
                                        saveToServer
                                        displayProgressMessage
                                        locServerFileSystem
                                        addBackground.
         clear iClassFunctions[].
       endif.

     if downloadDocumentation = TRUE.
       perform downloadClassDocs using <waClass>-clsName
                                        userFilePath
                                        fileExtension
                                        convertToHtml
                                        slashSeparator
                                        saveToServer
                                        displayProgressMessage
                                       '' "subdirectory
                                        classDocumentationExists
                                        locServerFileSystem
                                        addBackground.
      endif.
    endloop.
  endform.                                                                                               "downloadClasses

*  -------------------------------------------------------------------------------------------------------
*     downloadXslt...
*  -------------------------------------------------------------------------------------------------------
  form downloadXslt using iLocTransformation like iTransformations[]
                              value(userFilePath)
                              value(fileExtension)
                              value(htmlFileExtension)
                              value(textFileExtension)
                              value(convertToHtml)
                              value(customerNameRange)
                              value(slashSeparator)
                              value(saveToServer)
                              value(subdir)
                              value(displayProgressMessage)
                              value(locServerFileSystem)
                              value(addBackground) type abap_bool.

  field-symbols: <waTransformation> type tTransformation.

    sort iLocTransformation ascending.

    loop at iLocTransformation assigning <waTransformation>.
*     Download the main program
      perform readXsltAndDownload using <waTransformation>-xsltname
                                        <waTransformation>-xsltDesc
                                        userFilePath
                                        fileExtension
                                        convertToHtml
                                        customerNameRange
                                        slashSeparator
                                        saveToServer
                                        subdir
                                        displayProgressMessage
                                        locServerFileSystem
                                        addBackground.
    endloop.
  endform.                                                                                              "downloadXSLT

*  ----------------------------------------------------------------------------------------------------------------------
*    reFormatClassCode...   Expand a classes public, private and protected section from the 72 characters that the class
*                           builder sets it to back to the wide editor mode
*  ----------------------------------------------------------------------------------------------------------------------
  form reFormatClassCode using iTempLines like dumiHtml[].

  field-symbols: <waLine> type string.
  data: newLine type string.
  data: iNewTable type standard table of string.
  data: foundOne type abap_bool value FALSE.

    loop at iTempLines assigning <waLine>.
      if not <waLine> is initial.
        if foundOne = FALSE.
          find 'data' in <waLine> respecting case.
          if sy-subrc = 0.
            foundOne = TRUE.
          endif.

          find 'constants' in <waLine> respecting case.
          if sy-subrc = 0.
            foundOne = TRUE.
          endif.

          if foundOne = TRUE.
            newLine = <waLine>.

            if ( newLine cs '.' or newLine cs '*' ).
              replace '!' in <waLine> with ''.
              append newLine to iNewTable.
              clear newLine.
              foundOne = FALSE.
            endif.
          else.
            replace '!' in <waLine> with ''.
            append <waLine> to iNewTable.
          endif.
        else.
          concatenate newLine <waLine> into newLine separated by space.
          if ( newLine cs '.' or newLine cs '*' ).
            append newLine to iNewTable.
            clear newLine.
            foundOne = FALSE.
          endif.
        endif.
      else.
        replace '!' in <waLine> with ''.
        append <waLine> to iNewTable[].
      endif.
    endloop.

    iTempLines[] = iNewTable[].
  endform.                                                                             "reFormatClassCode

*  **********************************************************************************************************************
*  *********************************************HTML ROUTINES************************************************************
*  **********************************************************************************************************************

*  ----------------------------------------------------------------------------------------------------------------------
*    convertDDToHTML...   Convert text description to HTML
*  ----------------------------------------------------------------------------------------------------------------------
  form convertDDToHTML using iLocDictStructure like dumiDictStructure[]
                             iLocHtml like dumiHtml[]
                             value(tableName)
                             value(tableTitle)
                             value(sortTablesAsc)
                             value(addBackground) type abap_bool..

  data: iColumnCaptions type standard table of string with header line.
  data: waDictionary type tDictTableStructure.
  data: waHtml type string.
  data: title type string.
  field-symbols: <iLocDictStructure> type tDictTableStructure.
*   Holds one cell from the internal table
  field-symbols: <fsField>.
*   The value of one cell form the internal table
  data: wTextCell type string.
  data: rowCounter(3).
*   The base type of the field we are reading
  data: wFieldBaseType.

    perform buildDDColumnHeaders using iColumnCaptions[].

*   Add a html header to the table
    concatenate 'Dictionary object-' tablename into title separated by space.
    perform addHTMLHeader using iLocHtml[]
                                title
                                addBackground
                                SS_TABLE.

    append `<body>` to iLocHtml.
    append `  <table class="outerTable">` to iLocHtml.
    append `    <tr>` to iLocHtml.
    concatenate `      <td><h2>Table: ` tableName '</h2>' into waHtml.
    append waHtml to iLocHtml.
    concatenate `  <h3>Description: ` tableTitle '</h3></td>' into waHtml.
    append waHtml to iLocHtml.
    append `    </tr>` to iLocHtml.

    append `    <tr>` to iLocHtml.
    append `      <td><!--This is where our main table begins  -->` to iLocHtml.
    append `<table class="innerTable">` to iLocHtml.

*   Do we need to sort the fields into alphabetical order
    if not sortTablesAsc is initial.
      sort iLocDictStructure ascending by fieldname.
    endif.

*   This is where the header fields are defined
    append `<tr>` to iLocHtml.
    loop at iColumnCaptions.
      concatenate `  <th>` iColumnCaptions `</th>` into waHtml.
      append waHtml to iLocHtml.
    endloop.
    append `</tr>` to iLocHtml.

*   Add the table cells here
    loop at iLocDictStructure assigning <iLocDictStructure>.
      append `<tr class="cell">` to iLocHtml.
      rowcounter = rowcounter + 1.
      concatenate `  <td>` rowcounter `</td>` into waHtml.
      append waHtml to iLocHtml.

      do.
*       Assign each field in the table to the field symbol
        assign component sy-index of structure <iLocDictStructure> to <fsField>.
        if sy-subrc = 0.
          describe field <fsField> type wFieldBaseType.
          if wFieldBaseType <> 'h'.
            move <fsField> to wTextCell.
            waHtml = `  <td>`.

*           Add the caption name
            if wTextCell is initial.
              concatenate waHtml '&nbsp;' '</td>' into waHtml.
            else.
              concatenate waHtml wTextCell '</td>' into waHtml.
            endif.

            append waHtml to iLocHtml.
            clear waHtml.
          endif.
        else.
          exit.
        endif.
      enddo.

      append `</tr>` to iLocHtml.
    endloop.

    append `      </table>` to iLocHtml.
    append `     </td>` to iLocHtml.
    append `   </tr>` to iLocHtml.
    append '<br/>' to iLocHtml.

*   Add in any domain entries
    perform addDomainToHTML using iLocDictStructure[]
                                  iLocHtml[].

*   Add a html footer to the table
    perform addHtmlFooter using iLocHtml[].
  endform.                                                                                               "convertDDToHTML

*  ----------------------------------------------------------------------------------------------------------------------
*    addDomaintoHTML...   Add domain vlaues into the HTML
*  ----------------------------------------------------------------------------------------------------------------------
  form addDomaintoHTML using iLocDictStructure like dumiDictStructure[]
                             iLocHtml like dumiHtml[].

  data: iColumnCaptions type standard table of string with header line.
  data: waDictionary type tDictTableStructure.
  data: waHtml type string.
  data: title type string.
  field-symbols: <iLocDictStructure> type tDictTableStructure.
  data: iDomStructure type standard table of tDomainStructure with header line.
*   Holds one cell from the internal table
  field-symbols: <fsField>.
*   The value of one cell form the internal table
  data: wTextCell type string.
  data: rowCounter(3).
*   The base type of the field we are reading
  data: wFieldBaseType.
  data: addedHeader type flag value ''.

    perform buildDomColumnHeaders using iColumnCaptions[].

*   Add the table cells here
    loop at iLocDictStructure assigning <iLocDictStructure>.
      loop at <iLocDictStructure>-iDomains into iDomStructure.
*       OK, lets add the header since we know we have some domain texrt to add.
        if addedHeader = ''.
          append `  <table class="outerTable">` to iLocHtml.
          append `    <tr>` to iLocHtml.
          concatenate `      <td><h2>Fixed Domain Values ` '</h2>' into waHtml.
          append waHtml to iLocHtml.
          append `    </tr>` to iLocHtml.

          append `    <tr>` to iLocHtml.
          append `      <td><!--This is where our main table begins  -->` to iLocHtml.
          append `<table class="innerTable">` to iLocHtml.

*         This is where the header fields are defined
          append `<tr>` to iLocHtml.
          loop at iColumnCaptions.
            concatenate `  <th>` iColumnCaptions `</th>` into waHtml.
            append waHtml to iLocHtml.
          endloop.
          append `</tr>` to iLocHtml.
          addedHeader = 'X'.
        endif.

        append `<tr class="cell">` to iLocHtml.
        do.
*         Assign each field in the table to the field symbol
          assign component sy-index of structure iDomStructure to <fsField>.
          if sy-subrc = 0.
            describe field <fsField> type wFieldBaseType.
            if wFieldBaseType <> 'h'.
              move <fsField> to wTextCell.
              waHtml = `  <td>`.

*             Add the caption name
              if wTextCell is initial.
                concatenate waHtml '&nbsp;' '</td>' into waHtml.
              else.
                concatenate waHtml wTextCell '</td>' into waHtml.
              endif.

              append waHtml to iLocHtml.
              clear waHtml.
            endif.
          else.
            exit.
          endif.
        enddo.

        append `</tr>` to iLocHtml.
      endloop.
    endloop.

    if addedHeader = 'X'.
      append `      </table>` to iLocHtml.
      append `     </td>` to iLocHtml.
      append `   </tr>` to iLocHtml.
    endif.
  endform.
*  ----------------------------------------------------------------------------------------------------------------------
*    convertTableTypeToHtml
*  ----------------------------------------------------------------------------------------------------------------------
  form convertTableTypeToHtml using iLocTableTypes like iTableTypes[]
                                    iLocHtml like dumIhtml[]
                                    value(tableName)
                                    value(tableTitle)
                                    value(sortTablesAsc)
                                    value(addBackground) type abap_bool.

    data: iColumnCaptions type standard table of string with header line.
    data: waDictionary type tDictTableStructure.
    data: waHtml type string.
    data: title type string.
    field-symbols: <iLocTableType> like line of iLocTableTypes.
*   Holds one cell from the internal table
    field-symbols: <fsField>.
*   The value of one cell form the internal table
    data: wTextCell type string.
    data: rowCounter(3).

    perform buildColumnHeadersTableTypes using iColumnCaptions[].

*   Add a html header to the table
    concatenate 'Dictionary object (Table Type)-' tablename into title separated by space.
    perform addHtmlHeader using iLocHtml[]
                                title
                                addBackground
                                SS_TABLE.

    append '<body>' to ilocHtml.
    append '  <table class="outerTable">' to iLocHtml.
    append '    <tr>' to iLocHtml.
    concatenate '      <td><h2>Table: ' tablename '</h2>' into waHtml.
    append wahtml to iLocHtml.
    concatenate '  <h3>Description: ' tableTitle '</h3></td>' into waHtml.
    append wahtml to iLocHtml.
    append '    </tr>' to iLocHtml.

    append '    <tr>' to iLocHtml.
    append '      <td><!--This is where our main table begins  -->' to iLocHtml.
    append '<table class="innerTable">' to iLocHtml.

*   This is where the header fields are defined
    append '<tr>' to iLocHtml.
    loop at iColumnCaptions.
      concatenate '  <th>' iColumnCaptions '</th>' into waHtml.
      append waHtml to iLocHtml.
    endloop.
    append '</tr>' to iLocHtml.

*   Add the table cells here
    loop at iLocTableTypes assigning <iLocTableType>.
      append '<tr class="cell">' to iLocHtml.
      rowCounter = rowCounter + 1.
      concatenate '  <td>' rowCounter '</td>' into waHtml.
      append waHtml to iLocHtml.

      do.
*       Assign each field in the table to the field symbol
        assign component sy-index of structure <iLocTableType> to <fsField>.
        if sy-subrc = 0.
          move <fsField> to wTextCell.
          waHtml = '  <td>'.

*         Add the caption name
          if wTextCell is initial.
            concatenate waHtml '&nbsp;' '</td>' into waHtml.
          else.
            concatenate waHtml wTextCell '</td>' into waHtml.
          endif.

          append waHtml to iLocHtml.
          clear waHtml.
        else.
          exit.
        endif.
      enddo.

      append '</tr>' to iLocHtml.
    endloop.

    append '      </table>' to iLocHtml.
    append '     </td>' to iLocHtml.
    append '   </tr>' to iLocHtml.

*   Add a html footer to the table
    perform addHtmlFooter using iLocHtml[].
  endform.                                                                                  "convertTableTypeToHTML
                                                                       "convertITABtoHtml

*  ----------------------------------------------------------------------------------------------------------------------
*    convertCodeToHtml... Builds an HTML table based upon a text table.
*  ----------------------------------------------------------------------------------------------------------------------
  form convertCodeToHtml using iContents like dumIHtml[]
                               value(programName)
                               value(ShortDescription)
                               value(sourceCodeType)
                               value(functionDocumentationExists)
                               value(isMainFunctionInclude)
                               value(htmlExtension)
                               value(customerNameRange)
                               value(getIncludes)
                               value(getDictStructures)
                               value(addBackground) type abap_bool.

  data: htmlTable type standard table of string with header line.
  data: head(255).
  data: tail(255).
  data: myTabix type syTabix.
  data: nextLine type syTabix.
  data: hyperlinkName type string.
  data: copyOfCurrentLine type string.
  data: currentLineLength type i value 0.
  data: copyLineLength type i value 0.
  data: ignoreFutureLines type abap_bool value FALSE.
  data: foundAsterix type abap_bool value FALSE.
  data: lowercaseLink type string.
  data: waNextLine type string.
  data: waContent type string.
  data: inCommentMode type abap_bool value 'X'.

*   Add a html header to the table
    perform addHTMLHeader using htmlTable[]
                                programName
                                addBackground
                                SS_CODE.

    append '<body>' to htmlTable.
*   Prgroamname and description
    append '<table class="outerTable">' to htmlTable.
    append `  <tr class="normalBoldLarge">` to htmlTable.

    concatenate `     <td><h2>Code listing for: ` programName `</h2>` into htmlTable.
    append htmlTable.

    concatenate `<h3> Description: ` ShortDescription `</h3></td>` into htmlTable.
    append htmlTable.
    append `   </tr>` to htmlTable.

*   Code
    append `  <tr>` to htmlTable.
    append `     <td>` to htmlTable.

*   Table containing code
    append `     <table class="innerTable">` to htmlTable.
    append `       <tr>` to htmlTable.
    append `          <td>` to htmlTable.


    loop at iContents into waContent.
      myTabix = sy-tabix.

      if not ( waContent is initial ).
        while ( waContent cs '<' or waContent cs '>' ).
          replace '<' in waContent with LT.
          replace '>' in waContent with GT.
        endwhile.

        if waContent+0(1) <> ASTERIX.
          if myTabix = 1.
            append `   <div class="code">` to htmlTable.
            inCommentMode = FALSE.
          else.
            if inCommentMode = TRUE.
              append `   </div>` to htmlTable.
              inCommentMode = FALSE.
              append `   <div class="code">` to htmlTable.
            endif.
          endif.

          currentLineLength = strlen( waContent ).
          copyOfCurrentLine = waContent.

*         Don't hyperlink anything for files of type documentation
          if sourceCodeType <> IS_DOCUMENTATION.
*           Check for any functions to highlight
            if ( waContent cs CALLFUNCTION ) and ( waContent <> 'DESTINATION' ).
              nextLine = myTabix + 1.
              read table iContents into waNextLine index nextLine.
              translate waNextLine to upper case.
              if waNextLine ns 'DESTINATION'.
                shift copyOfCurrentLine left deleting leading space.

                copyLineLength = strlen( copyofCurrentLine ).

                split copyOfCurrentLine at space into head tail.
                split tail at space into head tail.
                split tail at space into head tail.
*               Function name is now in head
                translate head using ''' '.
                shift head left deleting leading space.

                try.
                  if head+0(1) = 'Y' or head+0(1) = 'Z' or head+0(1) = 'y' or head+0(1) = 'z' or head cs customerNameRange.
*                   Definately a customer function module
                    hyperLinkName = head.

                    if sourceCodeType = IS_FUNCTION.
                      copyOfCurrentLine = 'call function <a href ="../'.
                    else.
                      copyOfCurrentLine = 'call function <a href ="'.
                    endif.

                    lowercaseLink = hyperlinkName.
                    translate lowercaseLink to lower case.
*                   If we are running on a non UNIX environment we will need to remove forward slashes
                    if frontendOpSystem = NON_UNIX.
                      translate lowercaseLink using '/_'.
                    endif.

                    concatenate copyOfCurrentLine
                                lowercaseLink     "hyperlinkName
                                '/'
                                lowercaseLink     "hyperlinkName
                                Period htmlExtension '">'
                                ''''
                                hyperlinkName
                                ''''
                                '</a>'
                                tail into copyOfCurrentLine.

*                   Pad the string back out with spaces
                    while copyLineLength < currentLineLength.
                      shift copyOfCurrentLine right by 1 places.
                      copyLineLength = copyLineLength + 1.
                    endwhile.

                    waContent = copyOfCurrentLine.
                  endif.
                  catch cx_sy_range_out_of_bounds into objRuntimeError.
                endtry.
              endif.
            endif.
          endif.

*         Check for any customer includes to hyperlink
          if waContent cs INCLUDE or waContent cs LOWINCLUDE.
            shift copyOfCurrentLine left deleting leading space.
            copyLineLength = strlen( copyOfCurrentLine ).

            split copyOfCurrentLine at space into head tail.
            shift tail left deleting leading space.

            try.
              if ( tail+0(1) = 'Y' or tail+0(1) = 'Z' or tail+0(1) = 'y' or tail+0(1) = 'z' or tail cs customerNameRange or tail+0(2) = 'mz' or tail+0(2) = 'MZ' )
                  and not getIncludes is initial and  tail ns STRUCTURE and tail ns LOWSTRUCTURE.

*               Hyperlink for program includes
                clear waContent.
                shift tail left deleting leading space.
                split tail at PERIOD into hyperlinkName tail.
                copyOfCurrentLine = 'include <a href ="'.

                lowercaseLink = hyperlinkName.
                translate lowercaseLink to lower case.

*               If we are running on a non UNIX environment we will need to remove forward slashes
                if frontendOpSystem = NON_UNIX.
                  translate lowercaseLink using '/_'.
                endif.

                concatenate copyOfCurrentLine
                            lowercaseLink       "hyperlinkName
                            PERIOD htmlExtension '">'
                            hyperlinkName
                            '</a>'
                            PERIOD tail into copyOfCurrentLine.

*               Pad the string back out with spaces
                while copyLineLength < currentLineLength.
                  shift copyOfCurrentLine right by 1 places.
                  copyLineLength = copyLineLength + 1.
                endwhile.
                waContent = copyOfCurrentLine.
              else.
                if not getDictStructures is initial.
*                Hyperlink for structure include e.g. "include structure zfred."
                 copyLineLength = strlen( copyofCurrentLine ).
                 split copyOfCurrentLine at space into head tail.
                 shift tail left deleting leading space.
                 split tail at space into head tail.

                 try.
                   if tail+0(1) = 'Y' or tail+0(1) = 'Z' or tail+0(1) = 'y' or tail+0(1) = 'z' or tail cs customerNameRange.
                     clear waContent.
                     shift tail left deleting leading space.
                     split tail at PERIOD into hyperlinkName tail.
                     copyOfCurrentLine = 'include structure <a href ='.

                     lowercaseLink = hyperlinkName.
                     translate lowercaseLink to lower case.
*                    If we are running on a non UNIX environment we will need to remove forward slashes
                     if frontendOpSystem = NON_UNIX.
                       translate lowercaseLink using '/_'.
                     endif.

                     concatenate copyOfCurrentLine
                                 '"'
                                 lowercaseLink    "hyperlinkName
                                 '/'
                                 'dictionary-'
                                 lowercaseLink    "hyperlinkName
                                 PERIOD htmlExtension
                                 '">'
                                 hyperlinkName
                                 '</a>'
                                 PERIOD tail into copyOfCurrentLine.

*                    Pad the string back out with spaces
                     while copyLineLength < currentLineLength.
                       shift copyOfCurrentLine right by 1 places.
                       copyLineLength = copyLineLength + 1.
                     endwhile.
                     waContent = copyOfCurrentLine.
                   endif.
                   catch cx_sy_range_out_of_bounds into objRuntimeError.
                 endtry.
               endif.
             endif.
              catch cx_sy_range_out_of_bounds into objRuntimeError.
           endtry.
          endif.
       else.
         if  waContent+0(1) = ASTERIX.
           if myTabix = 1.
             append `   <div class="codeComment">` to htmlTable.
             inCommentMode = TRUE.
           else.
             if inCommentMode = FALSE.
               append `   </div>` to htmlTable.
               append `   <div class="codeComment">` to htmlTable.
               inCommentMode = TRUE.
             endif.
           endif.
         endif.
       endif.


       if inCommentMode = TRUE.
         while waContent cs ' '.
           replace space with '&nbsp;' into waContent.
           if sy-subrc <> 0.
             exit.
           endif.
         endwhile.
       endif.
       htmlTable =  waContent.

       try.
         if htmlTable+0(1) = ` `.
           while htmlTable cs ` `.
             replace ` ` with '&nbsp;' into htmlTable.
             if sy-subrc <> 0.
               exit.
             endif.
           endwhile.
         endif.
       catch cx_sy_range_out_of_bounds into objRuntimeError.
       endtry.
      else.
        htmlTable = ''.
      endif.
      concatenate htmlTable `<br />` into htmlTable.
      append htmlTable.
    endloop.

    append `            </div>` to htmlTable.
    append `          </td>`  to htmlTable.
    append `        </tr>` to htmlTable.
    append `      </table>` to htmlTable.
    append `      </td>` to htmlTable.
    append `      </tr>` to htmlTable.

*   Add a html footer to the table
    perform addHtmlFooter using htmlTable[].

    iContents[] = htmlTable[].
  endform.                                                                                             "convertCodeToHtml

*  ----------------------------------------------------------------------------------------------------------------------
*    convertClassToHtml... Builds an HTML table based upon a text table.
*  ----------------------------------------------------------------------------------------------------------------------
  form convertClassToHtml using iContents like dumIHtml[]
                                value(className)
                                value(ShortDescription)
                                value(sourceCodeType)
                                value(htmlExtension)
                                value(customerNameRange)
                                value(getDictStructures)
                                value(addBackground) type abap_bool.

  data: htmlTable type standard table of string with header line.
  data: myTabix type syTabix.
  data: waContent type string.
  data: head type string.
  data: tail type string.
  data: hyperlinkName type string.
  data: lowercaseLink type string.
  data: copyOfCurrentLine type string.
  data: currentLineLength type i value 0.
  data: copyLineLength type i value 0.
  data: inCommentMode type abap_bool value 'X'.
  data: methodDirectory type string.

*   Add a html header to the table
    perform addHTMLHeader using htmlTable[]
                                className
                                addBackground
                                SS_CODE.

    append '<body>' to htmlTable.
*   Class name and description
    append '<table class="outerTable">' to htmlTable.
    append `  <tr class="normalBoldLarge">` to htmlTable.

    concatenate `     <td><h2>Code listing for class: ` className `</h2>` into htmlTable.
    append htmlTable.

    concatenate `<h3> Description: ` ShortDescription `</h3></td>` into htmlTable.
    append htmlTable.
    append `   </tr>` to htmlTable.

*   Code
    append `  <tr>` to htmlTable.
    append `     <td>` to htmlTable.

*   Table containing code
    append `     <table class="innerTable">` to htmlTable.
    append `       <tr>` to htmlTable.
    append `          <td>` to htmlTable.


    loop at iContents into waContent.
      myTabix = sy-tabix.

*     Comments
      if not ( waContent is initial ).
         if waContent+0(1) = ASTERIX.
           htmltable = wacontent.
           if myTabix = 1.
             append `   <div class="codeComment">` to htmlTable.
             inCommentMode = TRUE.
           else.
             if inCommentMode = FALSE.
               append `   </div>` to htmlTable.
               append `   <div class="codeComment">` to htmlTable.
               inCommentMode = TRUE.
             endif.
           endif.
         else.
           if myTabix = 1.
             append `   <div class="code">` to htmlTable.
             inCommentMode = FALSE.
           else.
             if inCommentMode = TRUE.
               append `   </div>` to htmlTable.
               inCommentMode = FALSE.
               append `   <div class="code">` to htmlTable.
             endif.
           endif.

*          Smaller than, greater than signs
           if not ( waContent is initial ).
             while ( waContent cs '<' or waContent cs '>' ).
               replace '<' in waContent with LT.
               replace '>' in waContent with GT.
             endwhile.

*  --        Hyperlink methods in the class
*  --        Setup sudirectory where method will be saved
             if wacontent CS 'public section.'.
               methoddirectory = 'public_methods'.
             elseif wacontent CS 'private section.'.
               methoddirectory = 'private_methods'.
             elseif wacontent CS 'protected section.'.
              methoddirectory = 'protected_methods'.
             endif.

*  --        When it is a method, make a link
             find regex '([:space:]*methods[:space:]*)(.*)' in wacontent
                                                            ignoring case
                                                            submatches head hyperlinkname.

             if sy-subrc = 0.
               shift hyperlinkname left deleting leading space.
               concatenate methoddirectory
                           '/'
                           hyperlinkname
                           '.html'
                           into lowercaselink.

               translate lowercaselink to lower case.
               concatenate head
                           ' <a href="'
                           lowercaselink
                           '">'
                           hyperlinkname
                           '</a>'
                           into wacontent.
               shift wacontent right by 2 places.
             endif.

*            Dictionary structures
             if not getDictStructures is initial.
               find 'class' in waContent ignoring case.
               if sy-subrc <> 0.
*                Hyperlink for dictionary/structure include
                 copyLineLength = strlen( waContent ).
                 copyOfCurrentLine = waContent.
                 split copyOfCurrentLine at space into head tail.
                 shift tail left deleting leading space.
                 split tail at space into head tail.

                 try.
                   if tail+0(1) = 'Y' or tail+0(1) = 'Z' or tail+0(1) = 'y' or tail+0(1) = 'z' or tail cs customerNameRange.
                     clear waContent.
                     shift tail left deleting leading space.
                     split tail at PERIOD into hyperlinkName tail.
                     copyOfCurrentLine = 'include structure <a href ='.

                     lowercaseLink = hyperlinkName.
                     translate lowercaseLink to lower case.
*                    If we are running on a non UNIX environment we will need to remove forward slashes
                     if frontendOpSystem = NON_UNIX.
                       translate lowercaseLink using '/_'.
                     endif.

                     concatenate copyOfCurrentLine
                                 '"'
                                 lowercaseLink    "hyperlinkName
                                 '/'
                                 'dictionary-'
                                 lowercaseLink    "hyperlinkName
                                 PERIOD htmlExtension
                                 '">'
                                 hyperlinkName
                                 '</a>'
                                 PERIOD tail into copyOfCurrentLine.

*                    Pad the string back out with spaces
                     while copyLineLength < currentLineLength.
                       shift copyOfCurrentLine right by 1 places.
                       copyLineLength = copyLineLength + 1.
                     endwhile.
                     waContent = copyOfCurrentLine.
                   endif.
                   catch cx_sy_range_out_of_bounds into objRuntimeError.
                 endtry.
               endif.
             endif.

             htmlTable = waContent.

              try.
                if htmlTable+0(1) = ` `.
                  while htmlTable cs ` `.
                    replace ` ` with '&nbsp;' into htmlTable.
                    if sy-subrc <> 0.
                      exit.
                    endif.
                  endwhile.
                endif.
              catch cx_sy_range_out_of_bounds into objRuntimeError.
              endtry.
           else.
             htmlTable = ''.
           endif.
         endif.
      else.
        htmlTable = ''.
      endif.

      concatenate htmlTable '<br />' into htmlTable.
      append htmlTable.
    endloop.

    append `            </div>` to htmlTable.
    append `          </td>`  to htmlTable.
    append `        </tr>` to htmlTable.
    append `      </table>` to htmlTable.
    append `      </td>` to htmlTable.
    append `      </tr>` to htmlTable.

*   Add a html footer to the table
    perform addHtmlFooter using htmlTable[].

    iContents[] = htmlTable[].
  endform.                                                                                            "convertClassToHtml

*  ----------------------------------------------------------------------------------------------------------------------
*    convertFunctionToHtml... Builds an HTML table based upon a text table.
*  ----------------------------------------------------------------------------------------------------------------------
  form convertFunctionToHtml using iContents like dumIHtml[]
                                   value(functionName)
                                   value(ShortDescription)
                                   value(sourceCodeType)
                                   value(functionDocumentationExists)
                                   value(isMainFunctionInclude)
                                   value(htmlExtension)
                                   value(customerNameRange)
                                   value(getIncludes)
                                   value(getDictStructures)
                                   value(addBackground) type abap_bool.

  data: htmlTable type standard table of string with header line.
  data: head(255).
  data: tail(255).
  data: myTabix type syTabix.
  data: nextLine type syTabix.
  data: hyperlinkName type string.
  data: copyOfCurrentLine type string.
  data: currentLineLength type i value 0.
  data: copyLineLength type i value 0.
  data: ignoreFutureLines type abap_bool value FALSE.
  data: foundAsterix type abap_bool value FALSE.
  data: lowercaseLink type string.
  data: waNextLine type string.
  data: waContent type string.
  data: inCommentMode type abap_bool value 'X'.

*   Add a html header to the table
    perform addHTMLHeader using htmlTable[]
                                functionName
                                addBackground
                                SS_CODE.

    append '<body>' to htmlTable.
*   Class name and description
    append '<table class="outerTable">' to htmlTable.
    append `  <tr class="normalBoldLarge">` to htmlTable.

    concatenate `     <td><h2>Code listing for function ` functionName `</h2>` into htmlTable.
    append htmlTable.

    concatenate `<h3> Description: ` ShortDescription `</h3></td>` into htmlTable.
    append htmlTable.
    append `   </tr>` to htmlTable.

*   Code
    append `  <tr>` to htmlTable.
    append `     <td>` to htmlTable.

*   Table containing code
    append `     <table class="innerTable">` to htmlTable.
    append `       <tr>` to htmlTable.
    append `          <td>` to htmlTable.

    loop at iContents into waContent.
      myTabix = sy-tabix.

*     Extra code for adding global and doc hyperlinks to functions
      if sourceCodeType = IS_FUNCTION and isMainFunctionInclude = TRUE.
        if not ( waContent is initial ).
          if sy-tabix > 1.
            if waContent+0(1) = ASTERIX and ignoreFutureLines = FALSE.
              foundAsterix = TRUE.
            else.
              if foundAsterix = TRUE.
*               Lets add our extra HTML lines in here
                append '' to HtmlTable.

*               Global data hyperlink
                copyOfcurrentLine = '<div class="codeComment">*       <a href ="' .
                lowercaseLink = functionName.
                translate lowercaseLink to lower case.
*               If we are running on a non UNIX environment we will need to remove forward slashes
                if frontendOpSystem = NON_UNIX.
                  translate lowercaseLink using '/_'.
                endif.

                concatenate copyOfCurrentLine 'global-' lowercaseLink  "functionName
                            PERIOD htmlExtension '">' 'Global data declarations' '</a>' into copyOfCurrentLine.

                concatenate copyOfCurrentLine '</div><br />' into copyOfCurrentLine.

                append copyOfCurrentLine to HtmlTable.

*               Documentation hyperlink.
                if functionDocumentationExists = TRUE.
                  copyOfcurrentLine = '<div class="codeComment">*       <a href ="'.

                  lowercaseLink = functionName.
                  translate lowercaseLink to lower case.
*                 If we are running on a non UNIX environment we will need to remove forward slashes
                  if frontendOpSystem = NON_UNIX.
                    translate lowercaseLink using '/_'.
                  endif.

                  concatenate copyOfCurrentLine
                              'docs-'
                              lowercaseLink  "functionName
                              PERIOD htmlExtension '">'
                              'Function module documentation'
                              '</a>'
                              into copyOfCurrentLine.

                  concatenate copyOfCurrentLine '</div><br />' into copyOfCurrentLine.
                  append copyOfCurrentLine to HtmlTable.
                endif.

                foundAsterix = FALSE.
                ignoreFutureLines = TRUE.
              endif.
            endif.
          endif.
        endif.
      endif.

*     Carry on as normal
      if not ( waContent is initial ).
        while ( waContent cs '<' or waContent cs '>' ).
          replace '<' in waContent with LT.
          replace '>' in waContent with GT.
        endwhile.

        if waContent+0(1) <> ASTERIX.
          if myTabix = 1.
            append `   <div class="code">` to htmlTable.
            incommentMode = FALSE.
          else.
            if inCommentMode = TRUE.
              append `   </div>` to htmlTable.
              inCommentMode = FALSE.
              append `   <div class="code">` to htmlTable.
            endif.
          endif.

          currentLineLength = strlen( waContent ).

*         Don't hyperlink anything for files of type documentation
          if sourceCodeType <> IS_DOCUMENTATION.
*         Check for any functions to highlight
            if ( waContent cs CALLFUNCTION ) and ( waContent <> 'DESTINATION' ).
              nextLine = myTabix + 1.
              read table iContents into waNextLine index nextLine.
              translate waNextLine to upper case.
              if waNextLine ns 'DESTINATION'.
                copyOfCurrentLine = waContent.
                shift copyOfCurrentLine left deleting leading space.

                copyLineLength = strlen( copyofCurrentLine ).

                split copyOfCurrentLine at space into head tail.
                split tail at space into head tail.
                split tail at space into head tail.
*               Function name is now in head
                translate head using ''' '.
                shift head left deleting leading space.

                try.
                  if head+0(1) = 'Y' or head+0(1) = 'Z' or head+0(1) = 'y' or head+0(1) = 'z' or head cs customerNameRange.

*                   Definately a customer function module
                    hyperLinkName = head.

                    if sourceCodeType = IS_FUNCTION.
                      copyOfCurrentLine = 'call function <a href ="../'.
                    else.
                      copyOfCurrentLine = 'call function <a href ="'.
                    endif.

                    lowercaseLink = hyperlinkName.
                    translate lowercaseLink to lower case.
*                   If we are running on a non UNIX environment we will need to remove forward slashes
                    if frontendOpSystem = NON_UNIX.
                      translate lowercaseLink using '/_'.
                    endif.

                    concatenate copyOfCurrentLine
                                lowercaseLink     "hyperlinkName
                                '/'
                                lowercaseLink     "hyperlinkName
                                Period htmlExtension '">'
                                ''''
                                hyperlinkName
                                ''''
                                '</a>'
                                tail into copyOfCurrentLine.

*                   Pad the string back out with spaces
                    while copyLineLength < currentLineLength.
                      shift copyOfCurrentLine right by 1 places.
                      copyLineLength = copyLineLength + 1.
                    endwhile.

                    waContent = copyOfCurrentLine.
                  endif.
                  catch cx_sy_range_out_of_bounds into objRuntimeError.
                endtry.
              endif.
            endif.
          endif.

*         Check for any customer includes to hyperlink
          if waContent cs INCLUDE or waContent cs LOWINCLUDE.
            copyOfCurrentLine = waContent.

            shift copyOfCurrentLine left deleting leading space.
            copyLineLength = strlen( copyOfCurrentLine ).

            split copyOfCurrentLine at space into head tail.
            shift tail left deleting leading space.

            try.
              if ( tail+0(1) = 'Y' or tail+0(1) = 'Z' or tail+0(1) = 'y' or tail+0(1) = 'z'
                   or tail cs customerNameRange or tail+0(2) = 'lz' or tail+0(2) = 'LZ'
                                                or tail+0(2) = 'mz' or tail+0(2) = 'MZ' ) and not getIncludes is initial.

*               Hyperlink for program includes
                clear waContent.
                shift tail left deleting leading space.
                split tail at PERIOD into hyperlinkName tail.
                copyOfCurrentLine = 'include <a href ="'.

                lowercaseLink = hyperlinkName.
                translate lowercaseLink to lower case.
*               If we are running on a non UNIX environment we will need to remove forward slashes
                if frontendOpSystem = NON_UNIX.
                  translate lowercaseLink using '/_'.
                endif.

                concatenate copyOfCurrentLine
                            lowercaseLink       "hyperlinkName
                            PERIOD htmlExtension '">'
                            hyperlinkName
                            '</a>'
                            PERIOD tail into copyOfCurrentLine.

*               Pad the string back out with spaces
                while copyLineLength < currentLineLength.
                  shift copyOfCurrentLine right by 1 places.
                  copyLineLength = copyLineLength + 1.
                endwhile.
                waContent = copyOfCurrentLine.
              else.
                if not getDictStructures is initial.
*                 Hyperlink for structure include
                  copyLineLength = strlen( copyofCurrentLine ).
                  split copyOfCurrentLine at space into head tail.
                  shift tail left deleting leading space.
                  split tail at space into head tail.

                  try.
                    if tail+0(1) = 'Y' or tail+0(1) = 'Z' or tail+0(1) = 'y' or tail+0(1) = 'z' or tail cs customerNameRange.
                      clear waContent.
                      shift tail left deleting leading space.
                      split tail at PERIOD into hyperlinkName tail.
                      copyOfCurrentLine = 'include structure <a href ='.

                      lowercaseLink = hyperlinkName.
                      translate lowercaseLink to lower case.
*                     If we are running on a non UNIX environment we will need to remove forward slashes
                      if frontendOpSystem = NON_UNIX.
                        translate lowercaseLink using '/_'.
                      endif.

                      concatenate copyOfCurrentLine
                                  '"'
                                  lowercaseLink    "hyperlinkName
                                  '/'
                                  'dictionary-'
                                  lowercaseLink    "hyperlinkName
                                  PERIOD htmlExtension
                                  '">'
                                  hyperlinkName
                                  '</a>'
                                  PERIOD tail into copyOfCurrentLine.

*                     Pad the string back out with spaces
                      while copyLineLength < currentLineLength.
                        shift copyOfCurrentLine right by 1 places.
                        copyLineLength = copyLineLength + 1.
                      endwhile.
                      waContent = copyOfCurrentLine.
                    endif.
                    catch cx_sy_range_out_of_bounds into objRuntimeError.
                  endtry.
                endif.
              endif.
              catch cx_sy_range_out_of_bounds into objRuntimeError.
           endtry.
         endif.
       else.
         if waContent+0(1) = ASTERIX.
           if myTabix = 1.
             append `   <div class="codeComment">` to htmlTable.
             inCommentMode = TRUE.
           else.
             if inCommentMode = FALSE.
               append `   </div>` to htmlTable.
               append `   <div class="codeComment">` to htmlTable.
               inCommentMode = TRUE.
             endif.
           endif.
         endif.
       endif.

       htmlTable = waContent.

       try.
         if htmlTable+0(1) = ` `.
           while htmlTable cs ` `.
             replace ` ` with '&nbsp;' into htmlTable.
             if sy-subrc <> 0.
               exit.
             endif.
           endwhile.
         endif.
       catch cx_sy_range_out_of_bounds into objRuntimeError.
       endtry.

      else.
        htmlTable = ''.
      endif.
      concatenate htmlTable '<br />' into htmlTable.
      append htmlTable.
    endloop.

    append `            </div>` to htmlTable.
    append `          </td>`  to htmlTable.
    append `        </tr>` to htmlTable.
    append `      </table>` to htmlTable.
    append `      </td>` to htmlTable.
    append `      </tr>` to htmlTable.

*   Add a html footer to the table
    perform addHtmlFooter using htmlTable[].

    iContents[] = htmlTable[].
  endform.                                                                                         "convertFunctionToHtml

*  ----------------------------------------------------------------------------------------------------------------------
*    buildColumnHeaders... build table column names
*  ----------------------------------------------------------------------------------------------------------------------
  form buildDDColumnHeaders using iLocColumnCaptions like dumiHtml[].

    append 'Row' to iLocColumnCaptions.
    append 'Field name' to iLocColumnCaptions.
    append 'Position' to iLocColumnCaptions.
    append 'Key' to iLocColumnCaptions.
    append 'Data element' to iLocColumnCaptions.
    append 'Domain' to iLocColumnCaptions.
    append 'Datatype' to iLocColumnCaptions.
    append 'Length' to iLocColumnCaptions.
    append 'Lowercase' to iLocColumnCaptions.
    append 'Domain text' to iLocColumnCaptions.
  endform.                                                                                            "buildColumnHeaders

*  ----------------------------------------------------------------------------------------------------------------------
*    buildDomColumnHeaders... build table column names
*  ----------------------------------------------------------------------------------------------------------------------
  form buildDomColumnHeaders using iLocColumnCaptions like dumiHtml[].

    append 'Domain Name' to iLocColumnCaptions.
    append 'Value Low' to iLocColumnCaptions.
    append 'Value High' to iLocColumnCaptions.
    append 'Text' to iLocColumnCaptions.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*    buildColumnHeadersTableTypes
*  ----------------------------------------------------------------------------------------------------------------------
  form buildColumnHeadersTableTypes  using iLocColumnCaptions like dumIhtml[].

    append 'Row' to iLocColumnCaptions.
    append 'Name of table type' to iLocColumnCaptions.
    append 'Name of row type for table types' to iLocColumnCaptions.
    append 'Category of table type (range or general table type)' to iLocColumnCaptions.
    append 'Elem. type of LOW and HIGH components of a Ranges type' to iLocColumnCaptions.
    append 'Type of Object Referenced' to ilocColumnCaptions.
    append 'Initial Line Number for Table Types' to iLocColumnCaptions.
    append 'Description' to iLocColumnCaptions.
  endform.                                                                                 " BUILDCOLUMNHEADERSTABLETYPES


*  ----------------------------------------------------------------------------------------------------------------------
*   addHTMLHeader...  add a html formatted header to our output table
*  ----------------------------------------------------------------------------------------------------------------------
  form addHTMLHeader using iLocHeader like dumiHtml[]
                           value(title)
                           value(addBackground) type abap_bool
                           value(stylesheetType) type char1.

  data: waHeader type string.

    append '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">' to iLocHeader.
    append '<html xmlns="http://www.w3.org/1999/xhtml">' to iLocHeader.
    append '<head>' to iLocHeader.
*       append '<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1" />' to iLocHeader.
    append '<meta http-equiv="Content-Type" content="application/xhtml+xml; charset=ISO-8859-1" />' to iLocHeader.

    concatenate '<title>' title '</title>' into waHeader.
    append waHeader to ilocHeader.

    case stylesheetType.
      when SS_CODE.
        perform addCodeStyles using iLocHeader
                                    addBackground.
      when SS_TABLE.
        perform addTableStyles using iLocHeader
                                     addBackground.
    endcase.

    perform addGenericStyles using iLocHeader
                                   addBackground.

    append '</head>' to iLocHeader.
  endform.                                                                                                 "addHTMLHeader

*  ----------------------------------------------------------------------------------------------------------------------
*   addCodeStyles... Add the stylesheets needed for HTML output
*  ----------------------------------------------------------------------------------------------------------------------
  form addCodeStyles using iLocHeader like dumiHtml[]
                           value(addBackground) type abap_bool.

    append '<style type="text/css">' to iLocHeader.
    append `.code{ font-family:"Courier New", Courier, monospace; color:#000; font-size:14px; background-color:#F2F4F7 }` to iLocHeader.
    append `  .codeComment {font-family:"Courier New", Courier, monospace; color:#0000F0; font-size:14px; background-color:#F2F4F7 }` to iLocHeader.
    append `  .normalBold{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:12px; font-weight:800 }` to iLocHeader.
    append `  .normalBoldLarge{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:16px; font-weight:800 }` to iLocHeader.
    append '</style>' to iLocHeader.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*   addTableStyles... Add the stylesheets needed for HTML output
*  ----------------------------------------------------------------------------------------------------------------------
  form addTableStyles using iLocHeader like dumiHtml[]
                            value(addBackground) type abap_bool.

    append '<style type="text/css">' to iLocHeader.
    append `  th{text-align:left}` to iLocHeader.

    append `  .cell{` to iLocHeader.
    append `     font-family:"Courier New", Courier, monospace;` to iLocHeader.
    append `     color:#000;` to iLocHeader.
    append `     font-size:12px;` to iLocHeader.
    append `     background-color:#F2F4F7;` to iLocHeader.
    append `  }` to iLocHeader.

    append `  .cell td { border: thin solid #ccc; }` to iLocHeader.
    append `</style>` to iLocHeader.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*   addTableStyles... Add the stylesheets needed for HTML output
*  ----------------------------------------------------------------------------------------------------------------------
  form addGenericStyles using iLocHeader like dumiHtml[]
                            value(addBackground) type abap_bool.

    append '<style type="text/css">' to iLocHeader.

    append `  .normal{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:12px }` to iLocHeader.
    append `  .footer{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:12px; text-align: center }` to iLocHeader.
    append `  h2{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:16px; font-weight:800 }` to iLocHeader.
    append `  h3{ font-family:Arial, Helvetica, sans-serif; color:#000; font-size:14px; font-weight:800 }` to iLocHeader.

    append `  .outerTable{` to iLocHeader.
      if not addBackground is initial.
        append `   background-color:#E0E7ED;` to iLocHeader.
      endif.
    append `   width:100%;` to iLocHeader.
    append `   border-top-width: thin;` to iLocHeader.
    append `   border-right-width: thin;` to iLocHeader.
    append `   border-right-width: thin;` to iLocHeader.
    append `   border-left-width: thin;` to iLocHeader.
    append `   border-top-style: solid;` to iLocHeader.
    append `   border-right-style: solid;` to iLocHeader.
    append `   border-bottom-style: solid;` to iLocHeader.
    append `   border-left-style: solid;` to iLocHeader.
    append `  }` to iLocHeader.

    append `  .innerTable{` to iLocHeader.
      if not addBackground is initial.
        append `   background-color:#F2F4F7;` to iLocHeader.
      endif.
    append `   width:100%;` to iLocHeader.
    append `   border-top-width: thin;` to iLocHeader.
    append `   border-right-width: thin;` to iLocHeader.
    append `   border-bottom-width: thin;` to iLocHeader.
    append `   border-left-width: thin;` to iLocHeader.
    append `   border-top-style: solid;` to iLocHeader.
    append `   border-right-style: solid;` to iLocHeader.
    append `   border-bottom-style: solid;` to iLocHeader.
    append `   border-left-style: solid;` to iLocHeader.
    append `  }` to iLocHeader.
    append '</style>' to iLocHeader.
  endform.

*  ----------------------------------------------------------------------------------------------------------------------
*   addHTMLFooter...  add a html formatted footer to our output table
*  ----------------------------------------------------------------------------------------------------------------------
  form addHTMLFooter using iLocFooter like dumiHtml[].

  data: footerMessage type string.
  data: waFooter type string.

    perform buildFooterMessage using footerMessage.

    append `   <tr>` to iLocFooter.
    concatenate '<td class="footer">' footerMessage '</td>' into waFooter.
    append waFooter to iLocFooter.
    append `   </tr>` to iLocFooter.
    append `</table>` to iLocFooter.
    append '</body>' to iLocFooter.
    append '</html>' to iLocFooter.
  endform.                                                                                                 "addHTMLFooter

*  ----------------------------------------------------------------------------------------------------------------------
*   buildFooterMessage...Returns a footer message based on the output file type.
*  ----------------------------------------------------------------------------------------------------------------------
  form buildFooterMessage using returnMessage.

    concatenate `Extracted by Mass Download version `
                VERSIONNO ` - E.G.Mellodew. 1998-`
                sy-datum+0(4) `. Sap Release ` sy-saprl into returnMessage.
  endform.                                                                                            "buildFooterMessage

*  **********************************************************************************************************************
*  *******************************************DISPLAY ROUTINES***********************************************************
*  **********************************************************************************************************************

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodeTables...
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodeTables using iLocDictionary like iDictionary[]
                                iLocTreeDisplay like iTreeDisplay[]
                                value(runTime).

  data: tableLines type i.
  data: waTreeDisplay like sNodeText.
  field-symbols: <waDictionary> type tDictTable.
  data: tableLinesString type string.
  data: runtimeChar(10).
  data: subLevel type string.

    tableLines = lines( iLocDictionary ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString 'table downloaded' into waTreeDisplay-text2 separated by space.
    else.
      concatenate tableLinesString 'tables downloaded' into waTreeDisplay-text2 separated by space.
    endif.

    write runTime to runtimeChar.
    concatenate waTreeDisplay-text2 '- runtime' runTimeChar into waTreeDisplay-text2 separated by space.

*   include header display record.
    waTreeDisplay-tlevel = '1'.
    waTreeDisplay-tlength2  = 60.
    waTreeDisplay-tcolor2    = 1.
    append waTreeDisplay to iLocTreeDisplay.

    loop at iLocDictionary assigning <waDictionary>.
      waTreeDisplay-tlevel = '2'.
      waTreeDisplay-text2 = <waDictionary>-tablename.
      waTreeDisplay-tcolor2    = 3.
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 3.
      waTreeDisplay-tpos3      = 60.
      concatenate 'Dictionary:' <waDictionary>-tableTitle into waTreeDisplay-text3 separated by space.

      append waTreeDisplay to iLocTreeDisplay.
    endloop.
  endform.                                                                                            "fillTreeNodeTables

*  -------------------------------------------------------------------------------------------------------
*     fillTreeNodeXslt...
*  -------------------------------------------------------------------------------------------------------
  form fillTreeNodeXslt using iLocTransformations like iTransformations[]
                              iLocTreeDisplay like iTreeDisplay[]
                              value(runtime).

  data: tableLines type i.
  data: waTreeDisplay like snodetext.
  field-symbols: <waTransformation> type tTransformation.
  data: tableLinesString type string.
  data: runtimeChar(10).

    tableLines = lines( iLocTransformations ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString ` XSLT program downloaded` into waTreeDisplay-text2.
    else.
      concatenate tableLinesString ` XSLT programs downloaded` into waTreeDisplay-text2.
    endif.

    write runtime to runtimeChar.

    concatenate waTreeDisplay-text2 ` - runtime ` runtimeChar into waTreeDisplay-text2.
*   include header display record.
    waTreeDisplay-tLevel = '2'.
    waTreeDisplay-tLength2  = 60.
    waTreeDisplay-tColor2    = 1.
    append waTreeDisplay to iTreeDisplay.

    loop at iLocTransformations assigning <waTransformation>.
*     Main programs.
      waTreeDisplay-tlevel = '3'.
      waTreeDisplay-text2 = <waTransformation>-xsltname.
      waTreeDisplay-tcolor2    = 1.
*     Description
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 1.
      waTreeDisplay-tpos3      = 60.
      concatenate `XSLT: ` <waTransformation>-xsltdesc into waTreeDisplay-text3.
      append waTreeDisplay to iTreeDisplay.
    endloop.
  endform.                                                                           " FILLTREENODEXSLT

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodeMessages...
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodeMessages using iLocMessages like iMessages[]
                                  iLocTreeDisplay like iTreeDisplay[]
                                  value(runTime).

  data: tableLines type i.
  data: waTreeDisplay like sNodeText.
  field-symbols: <waMessage> type tMessage.
  data: tableLinesString type string.
  data: runtimeChar(10).

    sort iLocMessages ascending by arbgb.

    loop at iLocMessages assigning <waMessage>.
      at new arbgb.
        tableLines = tableLines + 1.
      endat.
    endloop.
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString 'message class downloaded' into waTreeDisplay-text2 separated by space.
    else.
      concatenate tableLinesString 'message classes downloaded' into waTreeDisplay-text2 separated by space.
    endif.

    write runTime to runTimeChar.
    concatenate waTreeDisplay-text2 '- runtime' runTimeChar into waTreeDisplay-text2 separated by space.

*   include header display record.
    waTreeDisplay-tlevel = '1'.
    waTreeDisplay-tlength2 = 60.
    waTreeDisplay-tcolor2 = 1.
    append waTreeDisplay to iLocTreeDisplay.

    loop at iLocMessages assigning <waMessage>.
      at new arbgb.
        waTreeDisplay-tlevel = '2'.
        waTreeDisplay-text2 = <waMessage>-arbgb.
        waTreeDisplay-tcolor2    = 5.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 5.
        waTreeDisplay-tpos3      = 60.
        waTreeDisplay-text3 = <waMessage>-sText.
        concatenate 'Message class:'  waTreeDisplay-text3 into waTreeDisplay-text3 separated by space.
        append waTreeDisplay to iLocTreeDisplay.
      endat.
    endloop.
  endform.                                                                                          "fillTreeNodeMessages

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodeTableTypes
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodeTableTypes using iLocTableTypes like iTableTypes[]
                                    iLocTreeDisplay like iTreeDisplay[]
                                    value(runtime).

    data: tableLines type i.
    data: waTreeDisplay like snodetext.
    field-symbols: <waTableType> like line of iLocTableTypes.
    data: tableLinesString type string.
    data: runtimeChar(10).
    data: subLevel type string.

    tableLines = lines( iLocTableTypes ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString 'table type downloaded' into waTreeDisplay-text2 separated by space.
    else.
      concatenate tableLinesString 'table types downloaded' into waTreeDisplay-text2 separated by space.
    endif.

    write runtime to runtimeChar.
    concatenate waTreeDisplay-text2 '- runtime' runtimechar into waTreeDisplay-text2 separated by space.

*   include header display record.
    waTreeDisplay-tLevel = '2'.
    waTreeDisplay-tLength2  = 60.
    waTreeDisplay-tColor2    = 1.
    append waTreeDisplay to iLocTreeDisplay.

    loop at iLocTableTypes assigning <waTableType>.
      waTreeDisplay-tLevel = '3'.
      waTreeDisplay-text2 = <watabletype>-typename.
      waTreeDisplay-tcolor2    = 3.
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 3.
      waTreeDisplay-tpos3      = 60.
      concatenate 'Dictionary:' <waTableType>-ddtext into waTreeDisplay-text3 separated by space.

      append waTreeDisplay to iLocTreeDisplay.
    endloop.
  endform.                                                                                        " FILLTREENODETABLETYPES

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodeFunctions...
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodeFunctions using iLocFunctions like iFunctions[]
                                   iLocTreeDisplay like iTreeDisplay[]
                                   value(runTime).

  data: tableLines type i.
  data: waTreeDisplay like sNodeText.
  field-symbols: <waFunction> type tFunction.
  field-symbols: <waScreen> type tScreenFlow.
  field-symbols: <waGUITitle> type tGUITitle.
  field-symbols: <waDictionary> type tDictTable.
  field-symbols: <waInclude> type tInclude.
  field-symbols: <waMessage> type tMessage.
  field-symbols: <waTableType> type tTableType.
  field-symbols: <waTransformation> type tTransformation.
  data: tableLinesString type string.
  data: runtimeChar(10).

    sort iLocFunctions ascending by functionName.

    tableLines = lines( iLocFunctions ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString ` function downloaded` into waTreeDisplay-text2.
    else.
      concatenate tableLinesString ` functions downloaded` into waTreeDisplay-text2.
    endif.

    write runTime to runTimeChar.

    concatenate waTreeDisplay-text2 ` - runtime ` runTimeChar into waTreeDisplay-text2.
*   include header display record.
    waTreeDisplay-tlevel = '1'.
    waTreeDisplay-tlength2  = 60.
    waTreeDisplay-tcolor2    = 1.
    append waTreeDisplay to iLocTreeDisplay.

*   Lets fill the detail in
    loop at iLocFunctions assigning <wafunction>.
      waTreeDisplay-tlevel = 2.
      waTreeDisplay-text2 = <wafunction>-functionName.
      waTreeDisplay-tcolor2    = 7.
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 7.
      waTreeDisplay-tpos3      = 60.
      concatenate `Function: ` <wafunction>-functionName into waTreeDisplay-text3.
      append waTreeDisplay to iLocTreeDisplay.

*     Screens.
      loop at <waFunction>-iScreenFlow assigning <waScreen>.
        waTreeDisplay-tlevel = '2'.
        waTreeDisplay-text2 = <waScreen>-screen.
        waTreeDisplay-tcolor2    = 6.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 6.
        waTreeDisplay-tpos3      = 60.
        waTreeDisplay-text3 = 'Screen'.
        append waTreeDisplay to iTreeDisplay.
      endloop.

*     GUI Title.
      loop at <waFunction>-iGUITitle assigning <waGUITitle>.
        waTreeDisplay-tlevel = '2'.
        waTreeDisplay-text2 = <waGUITitle>-obj_code.
        waTreeDisplay-tcolor2    = 6.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 6.
        waTreeDisplay-tpos3      = 60.
        waTreeDisplay-text3 = 'GUI Title'.
        append waTreeDisplay to iTreeDisplay.
      endloop.

*     Fill in the tree with include information
      loop at <waFunction>-iIncludes assigning <waInclude>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waInclude>-includeName.
        waTreeDisplay-tcolor2    = 4.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 4.
        waTreeDisplay-tpos3      = 60.
        concatenate `Include:   ` <waInclude>-includeTitle into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with dictionary information
      loop at <waFunction>-iDictStruct assigning <waDictionary>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waDictionary>-tablename.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Dictionary:` <waDictionary>-tableTitle into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with Table type information
      loop at <waFunction>-iTableTypes assigning <waTableType>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTableType>-typeName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTableType>-ddtext into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with transformation information
      loop at <waFunction>-iTransformations[] assigning <waTransformation>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTRansformation>-xsltName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTransformation>-xsltDesc into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with message information
      sort <waFunction>-iMessages[] ascending by arbgb.
      loop at <waFunction>-iMessages assigning <waMessage>.
        at new arbgb.
          waTreeDisplay-tlevel = 3.
          waTreeDisplay-text2 = <waMessage>-arbgb.
          waTreeDisplay-tcolor2    = 5.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 5.
          waTreeDisplay-tpos3      = 60.

*         Select the message class text if we do not have it already
          if <waMessage>-sText is initial.
            select single stext from t100a
                                into <waMessage>-stext
                                where arbgb = <waMessage>-arbgb.
          endif.

          waTreeDisplay-text3 = <waMessage>-sText.
          concatenate `Message class: `  waTreeDisplay-text3 into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endat.
      endloop.
    endloop.
  endform.                                                                                         "fillTreeNodeFunctions

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodePrograms
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodePrograms using iLocPrograms like iPrograms[]
                                  iLocFunctions like iFunctions[]
                                  iLocTreeDisplay like iTreeDisplay[]
                                  value(runTime).

  data: tableLines type i.
  data: waTreeDisplay like sNodeText.
  field-symbols: <waProgram> type tProgram.
  field-symbols: <waScreen> type tScreenFlow.
  field-symbols: <waFunction> type tFunction.
  field-symbols: <waDictionary> type tDictTable.
  field-symbols: <waInclude> type tInclude.
  field-symbols: <waMessage> type tMessage.
  field-symbols: <waTableType> type tTableType.
  field-symbols: <waTransformation> type tTransformation.
  data: tableLinesString type string.
  data: runtimeChar(10).

    tableLines = lines( iLocPrograms ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString ` program downloaded` into waTreeDisplay-text2.
    else.
      concatenate tableLinesString ` programs downloaded` into waTreeDisplay-text2.
    endif.

    write runTime to runTimeChar.

    concatenate waTreeDisplay-text2 ` - runtime ` runTimeChar into waTreeDisplay-text2.
*   include header display record.
    waTreeDisplay-tlevel = '1'.
    waTreeDisplay-tlength2  = 60.
    waTreeDisplay-tcolor2    = 1.
    append waTreeDisplay to iTreeDisplay.

    loop at iLocPrograms assigning <waProgram>.
*     Main programs.
      waTreeDisplay-tlevel = '2'.
      waTreeDisplay-text2 = <waProgram>-progName.
      waTreeDisplay-tcolor2    = 1.
*     Description
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 1.
      waTreeDisplay-tpos3      = 60.
      concatenate `Program: ` <waProgram>-programTitle into waTreeDisplay-text3.
      append waTreeDisplay to iTreeDisplay.
*     Screens.
      loop at <waProgram>-iScreenFlow assigning <waScreen>.
        waTreeDisplay-tlevel = '3'.
        waTreeDisplay-text2 = <waScreen>-screen.
        waTreeDisplay-tcolor2    = 6.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 6.
        waTreeDisplay-tpos3      = 60.
        waTreeDisplay-text3 = 'Screen'.
        append waTreeDisplay to iTreeDisplay.
      endloop.
*     fill in the tree with message information
      sort <waProgram>-iMessages[] ascending by arbgb.
      loop at <waProgram>-iMessages assigning <waMessage>.
        at new arbgb.
          waTreeDisplay-tlevel = 3.
          waTreeDisplay-text2 = <waMessage>-arbgb.
          waTreeDisplay-tcolor2    = 5.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 5.
          waTreeDisplay-tpos3      = 60.

*         Select the message class text if we do not have it already
          if <waMessage>-sText is initial.
            select single stext from t100a
                                into <waMessage>-stext
                                where arbgb = <waMessage>-arbgb.
          endif.

          waTreeDisplay-text3 = <waMessage>-sText.
          concatenate `Message class: `  waTreeDisplay-text3 into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endat.
      endloop.
*     Fill in the tree with include information
      loop at <waProgram>-iIncludes assigning <waInclude>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waInclude>-includeName.
        waTreeDisplay-tcolor2    = 4.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 4.
        waTreeDisplay-tpos3      = 60.
        concatenate `Include:   ` <waInclude>-includeTitle into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.
*     fill in the tree with dictionary information
      loop at <waProgram>-iDictStruct assigning <waDictionary>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waDictionary>-tablename.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Dictionary:    ` <waDictionary>-tableTitle into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with Table type information
      loop at <waProgram>-iTableTypes assigning <waTableType>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTableType>-typeName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTableType>-ddtext into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with transformation information
      loop at <waProgram>-iTransformations assigning <waTransformation>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTRansformation>-xsltName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTransformation>-xsltDesc into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     Function Modules
      loop at iLocFunctions assigning <wafunction> where programLinkName = <waProgram>-progname.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 = <wafunction>-functionName.
        waTreeDisplay-tcolor2    = 7.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 7.
        waTreeDisplay-tpos3      = 60.
        concatenate `Function:      ` <wafunction>-functionName into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.

*       Fill in the tree with include information
        loop at <waFunction>-iIncludes assigning <waInclude>.
          waTreeDisplay-tlevel = 4.
          waTreeDisplay-text2 =  <waInclude>-includeName.
          waTreeDisplay-tcolor2    = 4.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 4.
          waTreeDisplay-tpos3      = 60.
          concatenate `Include:       ` <waInclude>-includeTitle into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endloop.

*       fill in the tree with dictionary information
        loop at <waFunction>-iDictStruct assigning <waDictionary>.
          waTreeDisplay-tlevel = 4.
          waTreeDisplay-text2 =  <waDictionary>-tablename.
          waTreeDisplay-tcolor2    = 3.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 3.
          waTreeDisplay-tpos3      = 60.
          concatenate `Dictionary:    ` <wadictionary>-tableTitle into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endloop.

*       fill in the tree with message information
        sort <waFunction>-iMessages[] ascending by arbgb.
        loop at <waFunction>-iMessages assigning <waMessage>.
          at new arbgb.
            waTreeDisplay-tlevel = 4.
            waTreeDisplay-text2 = <waMessage>-arbgb.
            waTreeDisplay-tcolor2    = 5.
            waTreeDisplay-tlength3   = 80.
            waTreeDisplay-tcolor3    = 5.
            waTreeDisplay-tpos3      = 60.

*           Select the message class text if we do not have it already
            if <waMessage>-sText is initial.
              select single stext from t100a
                                  into <waMessage>-stext
                                  where arbgb = <waMessage>-arbgb.
            endif.

            waTreeDisplay-text3 = <waMessage>-sText.
            concatenate `Message class:  `  waTreeDisplay-text3 into waTreeDisplay-text3.
            append waTreeDisplay to iLocTreeDisplay.
          endat.
        endloop.
      endloop.
    endloop.
  endform.                                                                                          "fillTreeNodePrograms

*  ----------------------------------------------------------------------------------------------------------------------
*    fillTreeNodeClasses
*  ----------------------------------------------------------------------------------------------------------------------
  form fillTreeNodeClasses using iLocClasses like iClasses[]
                                 iLocFunctions like iFunctions[]
                                 iLocTreeDisplay like iTreeDisplay[]
                                 value(runTime).

  data: tableLines type i.
  data: waTreeDisplay like sNodeText.
  field-symbols: <waClass> type tClass.
  field-symbols: <waMethod> type tMethod.
  field-symbols: <waFunction> type tFunction.
  field-symbols: <waDictionary> type tDictTable.
  field-symbols: <waInclude> type tInclude.
  field-symbols: <waMessage> type tMessage.
  field-symbols: <waTableType> type tTableType.
  field-symbols: <waTransformation> type tTransformation.
  data: tableLinesString type string.
  data: runtimeChar(10).

    tableLines = lines( iLocClasses ).
    tableLinesString = tableLines.

    if tableLines = 1.
      concatenate tableLinesString ` class downloaded` into waTreeDisplay-text2.
    else.
      concatenate tableLinesString ` classes downloaded` into waTreeDisplay-text2.
    endif.

    write runTime to runTimeChar.

    concatenate waTreeDisplay-text2 ` - runtime ` runTimeChar into waTreeDisplay-text2.
*   include header display record.
    waTreeDisplay-tlevel = '1'.
    waTreeDisplay-tlength2  = 60.
    waTreeDisplay-tcolor2    = 1.
    append waTreeDisplay to iTreeDisplay.

    loop at iLocClasses assigning <waClass>.
*     Main Class.
      waTreeDisplay-tlevel = '2'.
      waTreeDisplay-text2 = <waClass>-clsName.
      waTreeDisplay-tcolor2    = 1.
*     Description
      waTreeDisplay-tlength3   = 80.
      waTreeDisplay-tcolor3    = 1.
      waTreeDisplay-tpos3      = 60.
      concatenate `Class:    ` <waClass>-descript into waTreeDisplay-text3.
      append waTreeDisplay to iTreeDisplay.

*     fill in the tree with method information
      loop at <waClass>-iMethods[] assigning <waMethod>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waMethod>-cmpname.
        waTreeDisplay-tcolor2    = 2.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 2.
        waTreeDisplay-tpos3      = 60.
        concatenate `Method:   ` <waMethod>-descript into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with message information
      sort <waClass>-iMessages[] ascending by arbgb.
      loop at <waClass>-iMessages assigning <waMessage>.
        at new arbgb.
          waTreeDisplay-tlevel = 3.
          waTreeDisplay-text2 = <waMessage>-arbgb.
          waTreeDisplay-tcolor2    = 5.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 5.
          waTreeDisplay-tpos3      = 60.

*         Select the message class text if we do not have it already
          if <waMessage>-sText is initial.
            select single stext from t100a
                                into <waMessage>-stext
                                where arbgb = <waMessage>-arbgb.
          endif.

          waTreeDisplay-text3 = <waMessage>-sText.
          concatenate `Message class: `  waTreeDisplay-text3 into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endat.
      endloop.

*     fill in the tree with dictionary information
      loop at <waClass>-iDictStruct assigning <waDictionary>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waDictionary>-tablename.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Dictionary:    ` <waDictionary>-tableTitle into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with Table type information
      loop at <waClass>-iTableTypes assigning <waTableType>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTableType>-typeName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTableType>-ddtext into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     fill in the tree with transformation information
      loop at <waClass>-iTransformations assigning <waTransformation>.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 =  <waTRansformation>-xsltName.
        waTreeDisplay-tcolor2    = 3.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 3.
        waTreeDisplay-tpos3      = 60.
        concatenate `Table Type:    ` <waTransformation>-xsltDesc into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.
      endloop.

*     Function Modules
      loop at iLocFunctions assigning <wafunction> where programLinkName = <waClass>-clsname.
        waTreeDisplay-tlevel = 3.
        waTreeDisplay-text2 = <wafunction>-functionName.
        waTreeDisplay-tcolor2    = 7.
        waTreeDisplay-tlength3   = 80.
        waTreeDisplay-tcolor3    = 7.
        waTreeDisplay-tpos3      = 60.
        concatenate `Function:      ` <wafunction>-functionName into waTreeDisplay-text3.
        append waTreeDisplay to iLocTreeDisplay.

*       Fill in the tree with include information
        loop at <waFunction>-iIncludes assigning <waInclude>.
          waTreeDisplay-tlevel = 4.
          waTreeDisplay-text2 =  <waInclude>-includeName.
          waTreeDisplay-tcolor2    = 4.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 4.
          waTreeDisplay-tpos3      = 60.
          concatenate `Include:       ` <waInclude>-includeTitle into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endloop.

*       fill in the tree with dictionary information
        loop at <waFunction>-iDictStruct assigning <waDictionary>.
          waTreeDisplay-tlevel = 4.
          waTreeDisplay-text2 =  <waDictionary>-tablename.
          waTreeDisplay-tcolor2    = 3.
          waTreeDisplay-tlength3   = 80.
          waTreeDisplay-tcolor3    = 3.
          waTreeDisplay-tpos3      = 60.
          concatenate `Dictionary:    ` <wadictionary>-tableTitle into waTreeDisplay-text3.
          append waTreeDisplay to iLocTreeDisplay.
        endloop.

*       fill in the tree with message information
        sort <waFunction>-iMessages[] ascending by arbgb.
        loop at <waFunction>-iMessages assigning <waMessage>.
          at new arbgb.
            waTreeDisplay-tlevel = 4.
            waTreeDisplay-text2 = <waMessage>-arbgb.
            waTreeDisplay-tcolor2    = 5.
            waTreeDisplay-tlength3   = 80.
            waTreeDisplay-tcolor3    = 5.
            waTreeDisplay-tpos3      = 60.

*           Select the message class text if we do not have it already
            if <waMessage>-sText is initial.
              select single stext from t100a
                                  into <waMessage>-stext
                                  where arbgb = <waMessage>-arbgb.
            endif.

            waTreeDisplay-text3 = <waMessage>-sText.
            concatenate `Message class:  `  waTreeDisplay-text3 into waTreeDisplay-text3.
            append waTreeDisplay to iLocTreeDisplay.
          endat.
        endloop.
      endloop.
    endloop.
  endform.                                                                                           "fillTreeNodeClasses

*  ----------------------------------------------------------------------------------------------------------------------
*   displayTree...
*  ----------------------------------------------------------------------------------------------------------------------
  form displayTree using iLocTreeDisplay like iTreeDisplay[].

  data: waTreeDisplay type snodetext.

*   build up the tree from the internal table node
    call function 'RS_TREE_CONSTRUCT'
         tables
              nodetab            = iTreeDisplay
         exceptions
              tree_failure       = 1
              id_not_found       = 2
              wrong_relationship = 3
              others             = 4.

*   get the first index and expand the whole tree
    read table iLoctreeDisplay into waTreeDisplay index 1.
    call function 'RS_TREE_EXPAND'
         exporting
              node_id   = waTreeDisplay-id
              all       = 'X'
         exceptions
              not_found = 1
              others    = 2.

*   now display the tree
    call function 'RS_TREE_LIST_DISPLAY'
         exporting
              callback_program      = sy-cprog
              callback_user_command = 'CB_USER_COMMAND'
              callback_text_display = 'CB_text_DISPLAY'
              callback_top_of_page  = 'TOP_OF_PAGE'
         exceptions
              others                = 1.
  endform.                                                                                                   "displayTree

*  ----------------------------------------------------------------------------------------------------------------------
*    topOfPage... for tree display routines.
*  ----------------------------------------------------------------------------------------------------------------------
  form topOfPage.

  endform.
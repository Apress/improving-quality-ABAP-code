*&---------------------------------------------------------------------*
*& Report  ZIQAC_ALV_TEMPLATE
*&---------------------------------------------------------------------*
* This is a generic template for an ALV report complete with a section
* for unit tests.
* The program is splt up into several INCLUDES and uses local classes
* It is a matter of personal taste if you want to use global classes
* instead, even if they are application specific and non-reusable
* That is a very emotive subject and has caused huge debates on the
* internet
*----------------------------------------------------------------------*
* ZXXXX : Report Name (The ZXXXXX is the transaction code)
* Creation Date DD/MM/YYYY SY-UNAME
*--------------------------------------------------------------------*

* Help Desk Call XXXX Business Requestor Mr/Ms X Top Dog Manager
*                     IT Requestor       Mr/Ms Y Business Analayst
* Description of help desk call / issue
* Here you add a description fo the actual business problem this
* program solves i.e. WHY the program was created.
* Also the idea is to add user facing documentation by taking
* the Goto -> Documentation option from here so a blue "I" icon will
* appear on the selction screen the user can press to view
* documentation which will help them use this report.

*--------------------------------------------------------------------*
* Data Declarations
*--------------------------------------------------------------------*
INCLUDE ziqac_alv_template_top.

*--------------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------------*
* Block One
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_bname FOR gs_selection_screen-bname.

SELECTION-SCREEN END OF BLOCK blk1.

* Block Two
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN END OF BLOCK blk2.

* Display Options
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK blk3.

*--------------------------------------------------------------------*
* Initialisation
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initalisation.

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_application=>main( NEW lcl_selections(
      is_bname = s_bname[]
      ip_vari  = p_vari ) ).

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  lcl_application=>f4_layouts(
    EXPORTING id_restrict = cl_salv_layout=>restrict_none
    CHANGING  cd_layout   = p_vari ).

*----------------------------------------------------------------------*
* Class Implementations
*----------------------------------------------------------------------*
INCLUDE ziqac_alv_template_cio1.

*----------------------------------------------------------------------*
* Test Doubles / Test Classes
* For a function group the include ends in T99
* For a class the internally generated include name ends in CCAU
* There is no convention for executable programs so I made one up
*----------------------------------------------------------------------*
INCLUDE ziqac_alv_template_tcau."Stands for Test Class ABAP Unit

*&---------------------------------------------------------------------*
*&      Form  INITALISATION
*&---------------------------------------------------------------------*
* This is for setting default values for the selection-screen
* Thus this is 100% procedural and can stay in a FORM routine
* If you really wanted to be a purist you could have this as a method
* of LCL_APPLICATION and nominate what selection-screen fields it
* changes in the signature, but that is verging on the ridiculous
*----------------------------------------------------------------------*
FORM initalisation.

  INSERT VALUE #(
  option = 'EQ'
  sign   = 'I'
  low    = sy-uname ) INTO TABLE s_bname.

ENDFORM.                    " INITALISATION

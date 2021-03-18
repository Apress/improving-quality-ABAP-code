class ZCL_CI_SYSUBRC_IS_INITIAL definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public .

*"* public components of class ZCL_CI_SYSUBRC_IS_INITIAL
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods RUN
    redefinition .
protected section.
*"* protected components of class ZCL_CI_SYSUBRC_IS_INITIAL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_CI_SYSUBRC_IS_INITIAL
*"* do not include other source files here!!!

  methods GET_CLASS_DESCRIPTION
    returning
      value(RESULT) type STRING .
ENDCLASS.



CLASS ZCL_CI_SYSUBRC_IS_INITIAL IMPLEMENTATION.


METHOD constructor.
*--------------------------------------------------------------------*
* Listing 10.2: CONSTRUCTOR Method of Custom Check Class
*--------------------------------------------------------------------*
  super->constructor( ).

  description = get_class_description( ).
  category    = 'ZCL_CI_CATEGORY_CHUCKLES'.
  version     = '000'.

*  HAS_ATTRIBUTES = 'X'.                        "optional
*  ATTRIBUTES_OK  = 'X' or ' '.                 "optional

ENDMETHOD.                                                 "CONSTRUCTOR


  METHOD get_class_description.

    TRY.
        result = NEW cl_oo_class( myname )->class-descript.
      CATCH cx_class_not_existent.
        "This should be impossible
        "We are talking about the name of the current class
        result = 'Description Not Available'.
    ENDTRY.

  ENDMETHOD.


METHOD get_message_text.
*--------------------------------------------------------------------*
* Listing 10.5: Defining a Code Inspector Message
*--------------------------------------------------------------------*
  p_text = 'Prefer SY-SUBRC = 0 to SY-SUBRC IS INITIAL'(003).

ENDMETHOD.                                                 "GET_MESSAGE_TEXT


  METHOD if_ci_test~display_documentation.
*--------------------------------------------------------------------*
* Listing 10.3: Redefined DISPLAY_DOCUMENTATION Method
* The idea is to display the documentation created in SE24 against
* the check class rather than duplicating that documentation elsewhere
*--------------------------------------------------------------------*
    DATA: header          TYPE thead,
          text_lines      TYPE tline_tab,
          dummy_info      TYPE help_info,
          dummy_functions TYPE STANDARD TABLE OF editexcl,
          object          TYPE dokhl-object,
          language        TYPE sylangu.

    object(30) = myname.

    CALL FUNCTION 'DOCU_GET_FOR_F1HELP'
      EXPORTING
        id     = 'CL' "i.e. the documentation defined in SE24
        langu  = sy-langu
        object = object
      IMPORTING
        head   = header
      TABLES
        line   = text_lines
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      SELECT SINGLE masterlang
        FROM  tadir INTO language
        WHERE pgmid   = 'R3TR'
        AND  object   = 'CLAS'
        AND  obj_name =  myname.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'DOCU_GET_FOR_F1HELP'
          EXPORTING
            id     = 'CL'
            langu  = language
            object = object
          IMPORTING
            head   = header
          TABLES
            line   = text_lines
          EXCEPTIONS
            OTHERS = 1.
      ENDIF.
    ENDIF.

    IF sy-subrc  = 0.
      CALL FUNCTION 'HELP_DOCULINES_SHOW'
        EXPORTING
          help_infos        = dummy_info
          overlay_header    = header
          classic_sapscript = classic_sapscript
        TABLES
          excludefun        = dummy_functions
          helplines         = text_lines
        EXCEPTIONS
          OTHERS            = 1.
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE
        ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        DISPLAY LIKE sy-msgty.
    ENDIF.

  ENDMETHOD.


METHOD run.
*--------------------------------------------------------------------*
* Listing 10.4: Code for Custom Check
*--------------------------------------------------------------------*
  CONSTANTS: lc_code TYPE sci_errc VALUE '0001'.

  DATA: error_count     TYPE sci_errcnt,
        line_as_string  TYPE string,
        subrc_found     TYPE abap_bool,
        subrc_position  TYPE sy-tabix,
        column_in_error TYPE token_col.

  "Make sure we have some source code to check!
  IF ref_scan IS INITIAL.
    CHECK get( ) = abap_true.
  ENDIF.

  IF ref_scan->subrc NE 0.
    "Something really bad has happened
    RETURN.
  ENDIF.

  "Look at every statement to get the row/column number
  "NB you have to loop into STATEWNT_WA - standard SAP code assumes you do
  LOOP AT ref_scan->statements INTO statement_wa.
    CHECK statement_wa-from <= statement_wa-to.
    DATA(statement_position) = sy-tabix.
    "Looking for SY-SUBRC
    CLEAR: subrc_found,subrc_position.
    LOOP AT ref_scan->tokens ASSIGNING FIELD-SYMBOL(<token>) FROM statement_wa-from TO statement_wa-to.
      IF <token>-str = 'SY-SUBRC'.
        subrc_found    = abap_true.
        subrc_position = sy-tabix.
        EXIT."From Loop
      ENDIF.
    ENDLOOP.
    IF subrc_found EQ abap_false.
      "Nothing to see here, move on
      CONTINUE."With next statement
    ENDIF.

    DATA(token_plus_1) = subrc_position + 1.
    DATA(token_plus_2) = subrc_position + 2.

    READ TABLE ref_scan->tokens ASSIGNING <token> INDEX token_plus_1.

    IF sy-subrc NE 0.
      CONTINUE.
    ELSEIF <token>-str NE 'IS'.
      CONTINUE.
    ENDIF.

    READ TABLE ref_scan->tokens ASSIGNING <token> INDEX token_plus_2.

    IF sy-subrc NE 0.
      CONTINUE.
    ELSEIF <token>-str NE 'INITIAL'.
      CONTINUE.
    ENDIF.

    "If we have got here, the problem has been detected
    DATA(bad_token_number) = subrc_position.

    DATA(include_program) = get_include( p_ref_scan = ref_scan ).
    DATA(line_in_error)   = get_line_abs( bad_token_number ).
    column_in_error       = get_column_abs( bad_token_number ).
    error_count           = error_count + 1.
    line_as_string        = line_in_error.                 "Type Conversion
    "Let the world know something is wrong
    "You have to INFORM them
    inform( p_sub_obj_type = c_type_include                "PROG (that's the value of C_TYPE_INCLUDE)
            p_sub_obj_name = include_program
            p_position     = statement_position
            p_line         = line_in_error
            p_column       = column_in_error
            p_errcnt       = error_count
            p_kind         = c_note                        "It's a comment/note as opposed to an error
            p_test         = myname                        "Is Michael Caine
            p_code         = lc_code                       "Dummy Value
            p_suppress     = '”#EC GET_A_LIFE'
            p_param_1      = line_as_string ).

  ENDLOOP."Statements

ENDMETHOD.
ENDCLASS.

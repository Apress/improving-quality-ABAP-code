class ZCL_CI_CATEGORY_CHUCKLES definition
  public
  inheriting from CL_CI_CATEGORY_ROOT
  final
  create public .

*"* public components of class ZCL_CI_CATEGORY_CHUCKLES
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .
protected section.
*"* protected components of class CL_CI_CATEGORY_SLIN
*"* do not include other source files here!!!

  methods GET_CLASS_DESCRIPTION
    returning
      value(RESULT) type STRING .
private section.
*"* private components of class ZCL_CI_CATEGORY_CHUCKLES
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_CI_CATEGORY_CHUCKLES IMPLEMENTATION.


METHOD constructor.
*--------------------------------------------------------------------*
* Listing 10.1: Redefined CONSTRUCTOR (from CL_CI_CATEGORY_ROOT)
*--------------------------------------------------------------------*
  super->constructor( ).
  description = get_class_description( ).
  category    = 'CL_CI_CATEGORY_TOP'.                      "#EC NOTEXT
  position    = '998'.

ENDMETHOD.


  METHOD get_class_description.
*--------------------------------------------------------------------*
* Listing 10.1: Dynamically Reading the Current Class Description
*--------------------------------------------------------------------*
    TRY.
        result = NEW cl_oo_class( myname )->class-descript.
      CATCH cx_class_not_existent.
        "This should be impossible
        "We are talking about the name of the current class
        result = 'Description Not Available'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

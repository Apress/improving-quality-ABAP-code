*&---------------------------------------------------------------------*
*&  Include           ZIQAC_ALV_TEMPLATE_TCAU
*&---------------------------------------------------------------------*
* Defintions and Implementations for Test Doubles and Test Classes
*----------------------------------------------------------------------*
* TEST DOUBLES
*----------------------------------------------------------------------*
CLASS ltd_pers_layer DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES lif_persistency_layer.

    ALIASES: derive_data FOR lif_persistency_layer~derive_data.

    DATA: mt_usr05 TYPE SORTED TABLE OF usr05 WITH NON-UNIQUE KEY bname.

ENDCLASS.

CLASS ltd_pers_layer IMPLEMENTATION.

  METHOD derive_data.

    "There is no need at all for the square brackets, but I think it makes it obvious we are dealing with tables
    rt_output_data[] = mt_usr05[].

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* TEST CLASSES
*----------------------------------------------------------------------*
CLASS ltc_model DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA:
      "What class are we testing? (Class Under Test = CUT)
      mo_cut             TYPE REF TO lcl_model,
      "Test Doubles
      mo_mock_pers_layer TYPE REF TO ltd_pers_layer.
      "Global Variables & Constants for Test Methods

    METHODS:
      setup,
      "IT (whatever is being tested) SHOULD.....
      "<------------30-------------->
      "Comments listing the requirements, each such comment turns into a test method
      derive_user_pids FOR TESTING,
      "GIVEN / WHEN / THEN Methods
      given_user_has_pids,
      when_database_is_read,
      then_correct_pids_returned.

ENDCLASS.

CLASS ltc_model IMPLEMENTATION.

  "The SETUP methd runs automatically just before every test method
  METHOD setup.
*--------------------------------------------------------------------*
* Listing 2.7: Test Class SETUP Method that use Injection
*--------------------------------------------------------------------*
    "Prepare Test Doubles
    mo_mock_pers_layer = NEW ltd_pers_layer( ).

    DATA(lo_injector) = NEW lcl_dependency_injector( ).

    lo_injector->inject_pers_layer( mo_mock_pers_layer ).

    "In real life you would mock the selection screen class as well
    "I am just hard coding it (to be blank) for this example
    DATA: lr_bname TYPE RANGE OF usr05-bname.

    DATA(lo_selections) = NEW lcl_selections(
      is_bname = lr_bname[]
      ip_vari  = '1234' ).

    "Create New Instance of Class Under Test (CUT)
    "for every test method
    mo_cut = NEW #( lo_selections ).

  ENDMETHOD.

  METHOD derive_user_pids.
*--------------------------------------------------------------------*
* This is supposed to read like a test script i.e. Plain English
* with no ABAP specific keywords e.g. INITIAL that a business analyst
* might not understand
*--------------------------------------------------------------------*
    given_user_has_pids( ).

    when_database_is_read( ).

    then_correct_pids_returned( ).

  ENDMETHOD.

  METHOD given_user_has_pids.

    mo_mock_pers_layer->mt_usr05 = VALUE #(
    ( bname = 'BLOGGSJ' parid = 'VKO' parva = '1234' )
    ( bname = 'BLOGGSJ' parid = 'WRK' parva = '4321' ) ).

  ENDMETHOD.

  METHOD when_database_is_read.

    "This is a really silly example, just to show all the moving parts of a test class
    "In real life your ALV report will fill with business logic very quickly and then you will create sensible
    "tests based on the non-stop stream of new requirements
    mo_cut->data_retrieval( ).

  ENDMETHOD.

  METHOD then_correct_pids_returned.

    cl_abap_unit_assert=>assert_equals(
    msg = 'PID Data has not been Returned'(005)
    exp = abap_true
    act = xsdbool( mo_cut->mt_output_data[] IS NOT INITIAL ) ).

  ENDMETHOD.

ENDCLASS.

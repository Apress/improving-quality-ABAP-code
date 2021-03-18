*"* use this source file for your ABAP unit test classes
*--------------------------------------------------------------------*
* Listing 2.6: Local Test Double Definition and Implementation
* The important points are as follows:-
* The test double has to implement the interfaxe that the real class
* uses
* That interface has to have every method set to "emulate empty implementation"
* - that way creating new methods in the interfaxe will not cause synatx
* warnings in the test double
*--------------------------------------------------------------------*
CLASS ltd_pers_layer DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_database_reader.

    ALIASES: derive_customer_details FOR zif_database_reader~derive_customer_details.

    METHODS: set_account_group IMPORTING id_customer TYPE kunag
                                         id_group    TYPE ktokd.

  PRIVATE SECTION.
    DATA: mt_kna1 TYPE HASHED TABLE OF kna1 WITH UNIQUE KEY kunnr.

ENDCLASS.

CLASS ltd_pers_layer IMPLEMENTATION.

  METHOD set_account_group.
*--------------------------------------------------------------------*
* Listing 2.14: Method to Inject Data into a Test Double
* We do not want to rely on having any actual data in the development
* system so we use an internal table as opposed to reading the
* actual dtabase table. Here we insert a value into our fake "database"
*--------------------------------------------------------------------*
    READ TABLE mt_kna1 ASSIGNING FIELD-SYMBOL(<ls_kna1>)
    WITH TABLE KEY kunnr = id_customer.

    IF sy-subrc EQ 0.
      <ls_kna1>-ktokd = id_group.
    ELSE.
      INSERT VALUE #(
      kunnr = id_customer
      ktokd = id_group ) INTO TABLE mt_kna1.
    ENDIF.

  ENDMETHOD.

  METHOD derive_customer_details.

    READ TABLE mt_kna1 INTO rs_header_details
    WITH TABLE KEY kunnr = id_customer_number.

    IF sy-subrc Ne 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
*------------------------------------------------------------*
*       CLASS ltc_model DEFINITION
*------------------------------------------------------------*
*
*------------------------------------------------------------*
CLASS ltc_model DEFINITION DEFERRED.

CLASS zcl_octopus_model DEFINITION LOCAL FRIENDS ltc_model.

*--------------------------------------------------------------------*
* Listing 2.8: Comments indicating what test methods will be created
* Those comments are gradually replaced with actual test method
* defintions. Due to the 30 character limit on method names you often
* need to retain the comment to give a proper description of the test
*--------------------------------------------------------------------*
CLASS ltc_model DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA:
      "What class are we testing? (Class Under Test = CUT)
      mo_cut             TYPE REF TO zcl_octopus_model,
      "Test Doubles
      mo_mock_pers_layer TYPE REF TO ltd_pers_layer.
      "Global Variables for Test Methods

    CONSTANTS:
               normal_customer TYPE kna1-kunnr VALUE '0000123456',
               normal          TYPE kna1-ktokd VALUE 'NORM'.

    METHODS:
      setup,
      "IT (SCREEN) SHOULD.....
      "<------------30-------------->
      "Fill the customer group on the sales data subscreen
      derive_customer_group FOR TESTING,
      "Format the customer name and address properly
      "Only show delivery address belonging to the customer
      "Format the delivery address properly
      "Etc..
      "GIVEN / WHEN / THEN Methods
      given_account_group   IMPORTING for_customer TYPE kunag
                                      is_group     TYPE ktokd,
      given_customer_number IMPORTING id_customer  TYPE kunag,
      when_screen_is_shown,
      then_cust_account_group_is IMPORTING id_group TYPE ktokd.

ENDCLASS.

CLASS ltc_model IMPLEMENTATION.

  "The SETUP methd runs automatically just before every test method
  METHOD setup.
*--------------------------------------------------------------------*
* Listing 2.7: Test Class SETUP Method that use Injection
*--------------------------------------------------------------------*
    "Prepare Test Doubles
    mo_mock_pers_layer = NEW ltd_pers_layer( ).

    DATA(lo_injector) = NEW zcl_dependency_injector( ).

    lo_injector->inject_database_reader( mo_mock_pers_layer ).

    "Create New Instance of Class Under Test (CUT)
    "for every test method
    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD derive_customer_group.
*--------------------------------------------------------------------*
* Listing 2.9: An Example Test Method
* The methods defined as FOR TESTING methods are supposed to read like
* the test script, so the business analyst can understand them
*--------------------------------------------------------------------*
    given_account_group( for_customer = normal_customer
                         is_group     = normal ).

    given_customer_number( normal_customer  ).

    when_screen_is_shown(  ).

    then_cust_account_group_is( normal ).

  ENDMETHOD.

  METHOD given_account_group.
*--------------------------------------------------------------------*
* Listing 2.13: Implementation of a GIVEN Method
* Configuration is a dependency. The test has to presume the
* configuration is correct as we are not testing the configuration
* but rather the business logic that reacts to the configuration.
* Thus we fake the configuration.
*--------------------------------------------------------------------*
    mo_mock_pers_layer->set_account_group(
      id_customer = for_customer
      id_group    = is_group ).

  ENDMETHOD.

  METHOD given_customer_number.
*--------------------------------------------------------------------*
* GIVEN methods either put values into test doubles or prepare data
* that the class under test (CUT) is going to use durectly in the method being
* tested. This is an example of the latter.
*--------------------------------------------------------------------*
    mo_cut->customer_number = id_customer.

  ENDMETHOD.

  METHOD when_screen_is_shown.
*--------------------------------------------------------------------*
* Listing 2.15: Example WHEN Method
* Here we have no paramters and set and get values from the CUT
* in the GIVEN and THEN methods
* If the method being tested had parameters the IMPORTING ones would
* use member variables set in GIVEN methods and the EXPORTING ones
* would fill member cariales evaluated in the THEN method
* All such member variables would be blanked out during the SETUP method
*--------------------------------------------------------------------*
    mo_cut->process_main_screen( ).

  ENDMETHOD.

  METHOD then_cust_account_group_is.
*--------------------------------------------------------------------*
* Listing 2.10: An Example THEN Method
* This is coded first to cause the test to FAIL. At that point the
* RED stage of the TDD cycle is complete
*--------------------------------------------------------------------*
    cl_abap_unit_assert=>assert_equals(
    msg = 'Customer Account Group not Set Properly'
    exp = normal
    act = mo_cut->customer_account_group ).

  ENDMETHOD.

ENDCLASS.

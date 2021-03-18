*&---------------------------------------------------------------------*
*& Report ZAPRESS_IACQ_EXAMPLES
*&---------------------------------------------------------------------*
*& This program does nothing at all. It is just a container for some
*& of the code examples given in the book.
*&---------------------------------------------------------------------*
REPORT ziqac_examples.
*--------------------------------------------------------------------*
* Assorted global varibales for use in one or more examples
*--------------------------------------------------------------------*
DATA: ilfa1 TYPE HASHED TABLE OF lfa1
      WITH UNIQUE KEY lifnr WITH HEADER LINE.

TYPES: BEGIN OF g_typ_truck_type,
         vbeln TYPE likp-vbeln,
         eqart TYPE equi-eqart,
       END OF g_typ_truck_type.
DATA: gt_truck_types TYPE HASHED TABLE OF g_typ_truck_type WITH UNIQUE KEY vbeln.

TYPES: BEGIN OF g_typ_all_costs,
         vbeln TYPE vbak-vbeln,
         vkorg TYPE vbak-vkorg,
         tdlnr TYPE vttk-tdlnr,
         dlgrp TYPE lfa1-dlgrp,
         pltyp TYPE vbkd-pltyp,
         eqart TYPE equi-eqart,
       END OF   g_typ_all_costs.

DATA: all_costs TYPE STANDARD TABLE OF g_typ_all_costs WITH HEADER LINE.
*--------------------------------------------------------------------*
* Listing 4.14: Using a global structure as opposed to a TABLES declaration.
*--------------------------------------------------------------------*
DATA: BEGIN OF gs_selections,
        vbeln TYPE vbak-vbeln,
        werks TYPE vbap-werks,
        lfdat TYPE likp-lfdat,
      END OF gs_selections.

SELECT-OPTIONS: s_vbeln FOR gs_selections-vbeln,
                s_werks FOR gs_selections-werks,
                s_lfdat FOR gs_selections-lfdat.

AT SELECTION-SCREEN.
  PERFORM check_for_big_range.

START-OF-SELECTION.
  PERFORM do_nothing.

*--------------------------------------------------------------------*
* Sub-Routines
*--------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  L01_01_ELEPHANT_FUN
*&---------------------------------------------------------------------*
* Listing 1.1: How RETURNING Parameters make code shorter
*----------------------------------------------------------------------*
* This is procedural code so you have to declare the variable which
* is going to stire the result and use a CHANGING parameter in the
* caller to get that result. Neither is needed in the OO equivalent.
*----------------------------------------------------------------------*
FORM l01_01_elephant_fun.

  DATA: elephant_number TYPE i VALUE '1',
        trunk_size      TYPE i.

  PERFORM derive_trunk_size USING    elephant_number
                            CHANGING trunk_size.

ENDFORM.

FORM l01_02_circus_runaway.
*--------------------------------------------------------------------*
* Listing 1.2: Caller being forced to specify parameter names
*--------------------------------------------------------------------*
* In procedural code you can pass parameters in any order to a
* routine and there is nothing in the calling code to say which
* target variable in the calling routine is getting filled.
*--------------------------------------------------------------------*
  DATA: elephant_name TYPE string VALUE 'Nelly',
        circus_name   TYPE string VALUE 'Horrible Circus',
        run_away_flag TYPE abap_bool.

  PERFORM derive_run_away USING    elephant_name
                                   circus_name
                          CHANGING run_away_flag.

ENDFORM.

*--------------------------------------------------------------------*
* Listing 1.3: Incorrect value being passed into a FORM routine
*--------------------------------------------------------------------*
* The parameter in the FORM routine has no TYPE so you can pass
* anything in at all and get no syntax error.
*--------------------------------------------------------------------*
FORM i_want_an_integer USING id_integer.

ENDFORM.

FORM l01_03_passing_wrong_type.

  PERFORM i_want_an_integer USING 'BANANA'.

ENDFORM.

FORM l02_17_duplicate_code.
*--------------------------------------------------------------------*
* Listing 2.17: Cutting and Pasting Code into a new branch
* What is demonstrated here is the traditional approach to adding
* new code, which is to take already existing code and cut and paste
* it into a new part of the program, usually a new branch of an IF or CASE
* construcct
*--------------------------------------------------------------------*
* Here I am just setting up some dummy structures to mirror what exists
* in a big application
  DATA: lw_cvbkd                  TYPE vbkd,
        lw_vbkdvb                 TYPE vbkdvb,
        gcv_it_zterm_not_from_ref TYPE abap_bool,
        cvbkd                     TYPE STANDARD TABLE OF vbkd WITH HEADER LINE.

  DATA: BEGIN OF t_vbap,
          vbeln TYPE vbeln,
          posnr TYPE posnr,
          bstkd TYPE vbkd-bstkd,
          vgbel TYPE vbap-vgbel,
          vgpos TYPE vbap-vgpos,
        END OF t_vbap.

  TABLES: vbkd.

  CLEAR lw_cvbkd.
  READ TABLE cvbkd INTO lw_cvbkd
                   WITH KEY vbeln = t_vbap-vgbel
                            posnr = t_vbap-vgpos.

* Begin of Insertion (i.e. cut and paste)
  IF gcv_it_zterm_not_from_ref IS INITIAL.
    IF NOT lw_cvbkd IS INITIAL.
      MOVE lw_cvbkd TO lw_vbkdvb.                          "Copied from Below
      lw_vbkdvb-posnr = t_vbap-posnr.                      "Copied from Below
      lw_vbkdvb-bstkd = t_vbap-bstkd.                      "Copied from Below
    ELSE.                                                  "Copied from Below
      MOVE-CORRESPONDING vbkd TO lw_vbkdvb.                "Copied from Below
      lw_vbkdvb-posnr = t_vbap-posnr.                      "Copied from Below
      lw_vbkdvb-bstkd = t_vbap-bstkd.                      "Copied from Below
    ENDIF.                                                 "Copied from Below
  ELSE.
* End of Insertion

    "This was here already
    IF ( sy-subrc IS INITIAL ) AND
       ( NOT lw_cvbkd-vsart IS INITIAL ).
      "Case for shipping type VSART
      MOVE lw_cvbkd TO lw_vbkdvb.                          "Copied Above
      lw_vbkdvb-posnr = t_vbap-posnr.                      "Copied Above
      lw_vbkdvb-bstkd = t_vbap-bstkd.                      "Copied Above
    ELSE.                                                  "Copied Above
      MOVE-CORRESPONDING vbkd TO lw_vbkdvb.                "Copied Above
      lw_vbkdvb-posnr = t_vbap-posnr.                      "Copied Above
      lw_vbkdvb-bstkd = t_vbap-bstkd.                      "Copied Above
    ENDIF.                                                 "Copied Above

* Begin of Insertion
  ENDIF.
* End of Insertion

* There were "only" eight lines copied in this example. In real life it is usually a lot more
* The point is that every time you do this it makes your program weaker, especially if you do not have
* automated unit tests
ENDFORM.

FORM l03_08_contradiction.
*--------------------------------------------------------------------*
* Listing 3.8 – Contradictory Instructions
*--------------------------------------------------------------------*
* This used to give no wanring at all
* As of ABAP 7.50 you get a warning in procedural programs and a hard
* syntax error in OO methods
*--------------------------------------------------------------------*
  DATA: lt_plants TYPE STANDARD TABLE OF t001w,
        ls_plants LIKE LINE OF lt_plants.

  READ TABLE lt_plants INTO ls_plants
  WITH KEY werks = '1234'
  TRANSPORTING NO FIELDS.

ENDFORM.

FORM l03_10_complicating_a_routine USING if_ucomm TYPE sy-ucomm.
*--------------------------------------------------------------------*
* Listing 3.10 – USER_COMMAND code with assorted flaws
*--------------------------------------------------------------------*
* Blank Variables just to get the code to compile
  DATA: gf_not_apple_plant      TYPE abap_bool,
        gd_total_deliveries     TYPE sy-tabix,
        gd_day_total_deliveries TYPE sy-tabix,
        gd_quota_deliveries     TYPE sy-tabix,
        gd_day_quota_deliveries TYPE sy-tabix,
        gf_hour_restricted      TYPE abap_bool,
        gf_day_restricted       TYPE abap_bool,
        lv_msg                  TYPE string,
        gd_time_remaining       TYPE char02.

*** Original Version
  CASE if_ucomm.
    WHEN '&WEIGH'.
      PERFORM weigh_banana_delivery USING if_ucomm.
    WHEN '&REVERSE'.
      PERFORM reverse_banana_delivery.
      "Several hundred other banana relates user commands
    WHEN OTHERS.
      RETURN.
  ENDCASE.

*** Version with added logic.
  CASE if_ucomm.
    WHEN '&WEIGH'.
      "check for apple deliveries
      "this changes GF_NOT_APPLE_PLANT but you cannot tell as the signature does not mention this
      PERFORM check_apple_deliveries.
      IF gf_not_apple_plant EQ abap_false.                 "Double Negative
        IF ( gd_total_deliveries GE gd_quota_deliveries )
        AND gf_hour_restricted = abap_true.
          lv_msg = TEXT-431.
          REPLACE '&1' IN lv_msg WITH gd_time_remaining.
          MESSAGE lv_msg TYPE 'I'.
* The below comments will not auto-align if the identation of the code changes
*         MESSAGE 'Cannot ticket this delivery.  No of ticketed
*         deliveries reached for current hour.
*         Reprocess this delivery in the next &1 Min.'(431)
*         TYPE 'I' .
        ELSEIF ( gd_day_total_deliveries GE gd_day_quota_deliveries )
        AND gf_day_restricted = abap_true.
          lv_msg = TEXT-531.
          MESSAGE lv_msg TYPE 'I'.
*         MESSAGE Cannot ticket this delivery.
*         The max no of ticketed deliveries for today has been reached.
*         TYPE 'I' .
        ELSE.
          PERFORM weigh_banana_delivery USING if_ucomm.
        ENDIF.
      ELSE.
        PERFORM weigh_banana_delivery USING if_ucomm.
      ENDIF.

    WHEN '&REVERSE'.
      PERFORM reverse_banana_delivery.
      "Several hundred other banana relates user commands
    WHEN OTHERS.
      RETURN.
  ENDCASE.

*** What SHOULD have happened
  CASE if_ucomm.
    WHEN '&WEIGH'.
      "Keep the top level at one routine per branch
      "Create a new routine which contains all the added code above
      "That way the calling routine has not become more complicated
      PERFORM weigh_bd_with_checks USING if_ucomm.
    WHEN '&REVERSE'.
      PERFORM reverse_banana_delivery.
      "Several hundred other banana relates user commands
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDFORM.

FORM l03_29_meaningless_comments.
*--------------------------------------------------------------------*
* Listing 3.29 – Meaningless Comments
* A comment hat mirrors the oruitne name exactly adds no value at all
*--------------------------------------------------------------------*
* populate the old material number
  PERFORM populate_old_material_number.

* populate the material description
  PERFORM populate_material_description.

* populate the plant and vendor descriptions.
  PERFORM populate_plant_vendor_descr.

* populate the tare weights
  PERFORM populate_tare_weights.

ENDFORM.

FORM l04_05_wrong_code_working.
*--------------------------------------------------------------------*
* Listing 4.5: Incorrect Code that still works
*--------------------------------------------------------------------*
  DATA: BEGIN OF items_b OCCURS 0,
          material  TYPE matnr,
          entry_qnt TYPE mseg-menge,
          entry_uom TYPE mara-meins,
        END OF   items_b.

  DATA: BEGIN OF t_stock OCCURS 0,
          matnr TYPE matnr,
          stock TYPE mseg-menge,
          meins TYPE mara-meins,
        END OF   t_stock.

* The below code works, but only by accident
  LOOP AT items_b.
    READ TABLE t_stock WITH KEY matnr = items_b-material.
    IF sy-subrc = 0.
      items_b-entry_qnt = t_stock-stock.
      items_b-entry_uom = t_stock-meins.
      MODIFY items_b INDEX sy-tabix.                       "This line should not work!
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM l04_12_reading_vbak.
*--------------------------------------------------------------------*
* Listing 4.12: Bad and Good way to read VBAK
*--------------------------------------------------------------------*
  DATA: ld_contract        TYPE vbak-vbeln VALUE '1234567890',
        ls_contract_header TYPE vbak.

  TABLES: vbak.

  "The next two statements are identical - in both cases work area VBAK is filled
  SELECT SINGLE * FROM vbak WHERE vbeln = ld_contract.
  SELECT SINGLE * FROM vbak INTO vbak WHERE vbeln = ld_contract.

  "Here a local structure is filled rather than a global work area
  "Moreover is LS_CONTRACT_HEADER is not typed as VBAK but rather only the fields you actually want from VBAK
  "then the database is clever enough to only request those fields from the database, not every single field,
  "when you use a SELECT *.
  SELECT SINGLE * FROM vbak INTO ls_contract_header
  WHERE vbeln = ld_contract.

ENDFORM.

FORM l05_01_nested_loops_bad.
*--------------------------------------------------------------------*
* Listing 5.1: Nested Loops
*--------------------------------------------------------------------*
  DATA: BEGIN OF irepo OCCURS 0,
          vbeln TYPE vbeln,
          posnr TYPE posnr,
          rfmng TYPE vbfa-rfmng,                           "Quantity consumed thus far
        END OF   irepo.

  DATA: gt_vbfa TYPE STANDARD TABLE OF vbfa WITH HEADER LINE.

* IREPO   has 35,557 records  (it contains contract items)
* GT_VBFA has 276,813 records (it contains orders related to the contract items)

  LOOP AT irepo.
    DATA(lv_tabix) = sy-tabix.
    LOOP AT gt_vbfa WHERE vbelv = irepo-vbeln
                      AND posnv = irepo-posnr.

      SELECT SINGLE abgru INTO @DATA(lv_abgru)
      FROM  vbap
      WHERE vbeln = @gt_vbfa-vbeln
      AND   posnr = @gt_vbfa-posnn.

      IF sy-subrc EQ 0 AND lv_abgru IS INITIAL.
        "Only add quantities from non-rejected items
        irepo-rfmng = irepo-rfmng + gt_vbfa-rfmng.
      ENDIF.

    ENDLOOP."GT_VBFA
  ENDLOOP."IREPO

ENDFORM.

FORM l05_02_nested_loops_good.

  DATA: BEGIN OF irepo OCCURS 0,
          vbeln TYPE vbeln,
          posnr TYPE posnr,
          rfmng TYPE vbfa-rfmng,                           "Quantity consumed thus far
        END OF irepo.
*--------------------------------------------------------------------*
* Listing 5.2: Defining a table as SORTED
*--------------------------------------------------------------------*
  TYPES: BEGIN OF l_typ_vbfa,
           vbelv TYPE vbfa-vbelv,
           posnv TYPE vbfa-posnv,
           vbeln TYPE vbfa-vbeln,
           posnn TYPE vbfa-posnn,
           rfmng TYPE vbfa-rfmng,
           erdat TYPE vbfa-erdat,
           abgru TYPE vbap-abgru,
         END OF   l_typ_vbfa.

  DATA: lt_orders TYPE SORTED TABLE OF l_typ_vbfa WITH NON-UNIQUE KEY vbelv posnv vbeln posnn.

*--------------------------------------------------------------------*
* Listing 5.3: Getting all the Rejection Reasons in One Hit
*--------------------------------------------------------------------*
  DATA: lt_sales_key TYPE STANDARD TABLE OF sales_key.

  "Build unique list of contract numbers
  LOOP AT irepo INTO DATA(ls_contract_items).
    INSERT VALUE #(
    vbeln = ls_contract_items-vbeln )
    INTO TABLE lt_sales_key.
  ENDLOOP.

  "Primary key of VBFA is VBELV
  SELECT vbfa~vbelv, vbfa~posnv, vbfa~vbeln, vbfa~posnn, vbfa~rfmng, vbfa~erdat,
         vbap~abgru
    FROM vbfa
    INNER JOIN vbap
    ON  vbap~vbeln EQ vbfa~vbeln
    AND vbap~posnr EQ vbfa~posnn
    INTO TABLE @lt_orders
    FOR ALL ENTRIES IN @lt_sales_key
    WHERE vbfa~vbelv   EQ @lt_sales_key-vbeln
    "VBTYP_N = Document category of subsequent document i.e. sales order in this case
    AND   vbfa~vbtyp_n EQ 'C'
    AND   vbfa~rfmng   NE 0.

*--------------------------------------------------------------------*
* Listing 5.4: Nested Loop code with no Performance Problem
*--------------------------------------------------------------------*
  "Looping into a FIELD-SYMBOL improves performance
  LOOP AT irepo ASSIGNING FIELD-SYMBOL(<ls_repo>).
    "SORTED table so no problem with nested loops
    LOOP AT lt_orders ASSIGNING FIELD-SYMBOL(<ls_orders>)
      WHERE vbelv EQ <ls_repo>-vbeln
      AND   posnv EQ <ls_repo>-posnr
      AND   abgru IS INITIAL."i.e. not a rejected order item
      <ls_repo>-rfmng = <ls_repo>-rfmng + <ls_orders>-rfmng.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

FORM l05_12_multiple_reads_01.
*--------------------------------------------------------------------*
* Listing 5.12: Getting contract data in a Loop
*--------------------------------------------------------------------*
  TYPES: BEGIN OF l_typ_contracts,
           vbeln TYPE vbak-vbeln,
           ktext TYPE vbak-vbeln,
         END OF   l_typ_contracts.

  DATA: t_contract TYPE STANDARD TABLE OF l_typ_contracts WITH HEADER LINE.

  SORT t_contract.
  DELETE ADJACENT DUPLICATES FROM t_contract.

  LOOP AT t_contract WHERE vbeln IS NOT INITIAL.
    SELECT SINGLE ktext INTO t_contract-ktext FROM  vbak
           WHERE  vbeln  = t_contract-vbeln.

    MODIFY t_contract INDEX sy-tabix.

  ENDLOOP.

ENDFORM.

FORM l05_13_multiple_reads_02.
*--------------------------------------------------------------------*
* Listing 5.13: Getting the contract data all at once
*--------------------------------------------------------------------*
  TYPES: BEGIN OF l_typ_contracts,
           vbeln TYPE vbak-vbeln,
           ktext TYPE vbak-vbeln,
         END OF   l_typ_contracts.

  DATA: t_contract TYPE STANDARD TABLE OF l_typ_contracts WITH HEADER LINE.

  SORT t_contract.
  DELETE ADJACENT DUPLICATES FROM t_contract.

* Now is the time for a self-referential SELECT as opposed to a SELECT in a loop
  SELECT vbeln ktext
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE t_contract
    FOR ALL ENTRIES IN t_contract
    WHERE vbeln EQ t_contract-vbeln
    ORDER BY PRIMARY KEY.

ENDFORM.

FORM l05_14_shipment_costs_bad.
*--------------------------------------------------------------------*
* Listing 5.14: Code that worked but ruined the programs performance
*--------------------------------------------------------------------*

  DATA: lt_vbkd   TYPE HASHED TABLE OF vbkd
                  WITH UNIQUE KEY vbeln posnr
                  WITH NON-UNIQUE SORTED KEY vbeln COMPONENTS vbeln.

  LOOP AT all_costs.
* Get SAPG's
    PERFORM get_vendor USING    all_costs-tdlnr
                       CHANGING all_costs-dlgrp.
* Set pricelist type
    READ TABLE lt_vbkd INTO DATA(ls_vbkd)
    WITH TABLE KEY vbeln COMPONENTS vbeln = all_costs-vbeln.
    IF sy-subrc = 0.
      all_costs-pltyp = ls_vbkd-pltyp.
    ENDIF.

    "Get Truck Type
    PERFORM get_truck_type USING    all_costs-vbeln
                           CHANGING all_costs-eqart.

    MODIFY all_costs.
  ENDLOOP.

ENDFORM.

FORM l05_15_prefill_vendor_buffer.
  PERFORM fill_vendor_buffer.
ENDFORM.

FORM l05_16_prefill_truck_buffer.
  PERFORM fill_truck_type_buffer.
ENDFORM.

FORM l05_17_shipment_costs_good.
*--------------------------------------------------------------------*
* Listing 5.17: Reading from internal tables not the database
*--------------------------------------------------------------------*
* Changing thousands of individual SELECTS into severa big ones
* reduced the proram runtime for five hours to ten minutes
*--------------------------------------------------------------------*
  DATA: lt_vbkd TYPE HASHED TABLE OF vbkd
                WITH UNIQUE KEY vbeln posnr
                WITH NON-UNIQUE SORTED KEY vbeln COMPONENTS vbeln.

  "Looping into a FIELD-SYMBOL gives a small performance improvement
  LOOP AT all_costs ASSIGNING FIELD-SYMBOL(<ls_all_costs>).
    "Get SAPG's
    PERFORM get_vendor USING    <ls_all_costs>-tdlnr
                       CHANGING <ls_all_costs>-dlgrp.

    "Get pricelist type
    READ TABLE lt_vbkd INTO DATA(ls_vbkd)
    WITH TABLE KEY vbeln COMPONENTS vbeln = <ls_all_costs>-vbeln.
    IF sy-subrc = 0.
      <ls_all_costs>-pltyp = ls_vbkd-pltyp.
    ENDIF.

    "Get Truck Type
    READ TABLE gt_truck_types INTO DATA(ls_truck_types)
    WITH TABLE KEY vbeln = <ls_all_costs>-vbeln.
    IF sy-subrc EQ 0.
      <ls_all_costs>-eqart = ls_truck_types-eqart.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM l06_02_process_on_value_help.
*--------------------------------------------------------------------*
* Listing 6.2: Three ways of calling a custom F1 help
*--------------------------------------------------------------------*
* These are for when a screen field does not reference a DDIC data
* element directly or the data element has no meaningful long text
*--------------------------------------------------------------------*
  MESSAGE 'Does this order line item require testing?'(030) TYPE 'I'.

  "You could define a custom message which explains the fields nature
  CALL FUNCTION 'SWF_HELP_LONGTEXT_SHOW'
    EXPORTING
      in_workarea = 'ZSOMETHING'
      in_message  = '123'.

  "Perhaps there is a checkbox which says "search by sales organisation" and you
  "want the checkbox to say what a sales organisation is when someone presses the F1 help
  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
    EXPORTING
      doklangu         = sy-langu
      called_for_tab   = 'VBAK'
      called_for_field = 'VKORG'
    EXCEPTIONS
      object_not_found = 1
      sapscript_error  = 2
      OTHERS           = 3.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DERIVE_TRUNK_SIZE
*&---------------------------------------------------------------------*
FORM derive_trunk_size  USING    pud_elephant_number TYPE i
                        CHANGING pcd_trunk_size      TYPE i.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DERIVE_RUN_AWAY
*&---------------------------------------------------------------------*
FORM derive_run_away  USING    pud_elephant_name TYPE string
                               pud_circus_name   TYPE string
                      CHANGING pcf_run_away_flag TYPE abap_bool.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DO_NOTHING
*&---------------------------------------------------------------------*
FORM do_nothing.

  MESSAGE 'This program does not do anything!' TYPE 'I'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WEIGH_BANANA_DELIVERY
*&---------------------------------------------------------------------*
FORM weigh_banana_delivery USING if_ucomm TYPE sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REVERSE_BANANA_DELIVERY
*&---------------------------------------------------------------------*
FORM reverse_banana_delivery.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_APPLE_DELIVERIES
*&---------------------------------------------------------------------*
FORM check_apple_deliveries.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WEIGH_BD_WITH_CHECKS
*&---------------------------------------------------------------------*
FORM weigh_bd_with_checks USING if_ucomm TYPE sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_OLD_MATERIAL_NUMBER
*&---------------------------------------------------------------------*
FORM populate_old_material_number.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_MATERIAL_DESCRIPTION
*&---------------------------------------------------------------------*
FORM populate_material_description.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_PLANT_VENDOR_DESCR
*&---------------------------------------------------------------------*
FORM populate_plant_vendor_descr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POPULATE_TARE_WEIGHTS
*&---------------------------------------------------------------------*
FORM populate_tare_weights.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR
*&---------------------------------------------------------------------*
FORM get_vendor  USING    pud_lifnr TYPE vttk-tdlnr
                 CHANGING pcd_dlgrp TYPE lfa1-dlgrp.
* Local Variables
  DATA: ls_lfa1 TYPE lfa1.
* Clear Return Parameter
  CLEAR pcd_dlgrp.
* Check input variable is filled
  CHECK pud_lifnr IS NOT INITIAL.
* check buffer table
  READ TABLE ilfa1 WITH TABLE KEY lifnr = pud_lifnr.
  IF sy-subrc EQ 0.
    pcd_dlgrp = ilfa1-dlgrp.
    RETURN.
  ENDIF.
* If we are here, go to the database
  CALL FUNCTION 'LFA1_SINGLE_READ'
    EXPORTING
      lfa1_lifnr = pud_lifnr
    IMPORTING
      wlfa1      = ls_lfa1
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    CLEAR ls_lfa1.
  ENDIF.

* Win or lose, add record
  pcd_dlgrp   = ls_lfa1-dlgrp.
  ilfa1-lifnr = pud_lifnr.
  ilfa1-dlgrp = pcd_dlgrp.
  INSERT TABLE ilfa1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TRUCK_TYPE
*&---------------------------------------------------------------------*
FORM get_truck_type  USING    pud_vbeln TYPE vbak-vbeln
                     CHANGING pcd_eqart TYPE equi-eqart.

* This would do a buffered single read on LIPS using a standard SAP fuction module

* Then using another standard SAP function module a single read on VBAP was done to
* retrieve all the VBAP fields and then use one of them to fill the EQART result field

* For the vast amounts of records in the loop, this was disastrous

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_VENDOR_BUFFER
*&---------------------------------------------------------------------*
FORM fill_vendor_buffer.
*--------------------------------------------------------------------*
* Listing 5.15: Getting all the Vendor data in one hit
*--------------------------------------------------------------------*
  TYPES: BEGIN OF l_typ_all_costs,
           vbeln TYPE vbak-vbeln,
           tdlnr TYPE vttk-tdlnr,
           dlgrp TYPE lfa1-dlgrp,
           pltyp TYPE vbkd-pltyp,
           eqart TYPE equi-eqart,
         END OF   l_typ_all_costs.

  DATA: all_costs TYPE STANDARD TABLE OF l_typ_all_costs WITH HEADER LINE.

  DATA: lr_lifnr TYPE RANGE OF lfa1-lifnr.

* Preconditions
  CHECK all_costs[] IS NOT INITIAL.

  REFRESH ilfa1.

  LOOP AT all_costs
  ASSIGNING FIELD-SYMBOL(<ls_all_costs>) WHERE tdlnr IS NOT INITIAL.
    INSERT VALUE #(
    option = 'EQ'
    sign   = 'I'
    low    = <ls_all_costs>-tdlnr ) INTO TABLE lr_lifnr.
  ENDLOOP.

  IF lr_lifnr[] IS INITIAL.
    RETURN.
  ELSE.
    SORT lr_lifnr BY low.
    DELETE ADJACENT DUPLICATES FROM lr_lifnr COMPARING ALL FIELDS.
  ENDIF.

* LFA1 - Primary Key = LIFNR
  SELECT lifnr dlgrp
    FROM lfa1
    INTO CORRESPONDING FIELDS OF TABLE ilfa1
    WHERE lifnr IN lr_lifnr.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_TRUCK_TYPE_BUFFER
*&---------------------------------------------------------------------*
FORM fill_truck_type_buffer.
*--------------------------------------------------------------------*
* Listing 5.16: Joining LIPS and VBAP as opposed to reading in a loop
*--------------------------------------------------------------------*
* Local Variables
  DATA: lr_vbeln TYPE RANGE OF likp-vbeln,
        ld_first TYPE lips-posnr VALUE '000010'.

  "VKORG indicates a sales order based delivery
  LOOP AT all_costs
  ASSIGNING FIELD-SYMBOL(<ls_all_costs>) WHERE vkorg IS NOT INITIAL.
    INSERT VALUE #(
    option = 'EQ'
    sign   = 'I'
    low    = <ls_all_costs>-vbeln ) INTO TABLE lr_vbeln.
  ENDLOOP.

  IF lr_vbeln[] IS INITIAL.
    RETURN.
  ENDIF.

  SORT lr_vbeln BY low.
  DELETE ADJACENT DUPLICATES FROM lr_vbeln COMPARING ALL FIELDS.

* LIPS - Primary Key = VBELN / POSNR
* VBAP - Primary Key = VBELN / POSNR
  SELECT lips~vbeln
         vbap~bonus AS eqart "Just being silly, pretnding this field means truck type. In reality it's a Z field
    INTO CORRESPONDING FIELDS OF TABLE gt_truck_types
    FROM lips
    INNER JOIN vbap
    ON  lips~vgbel = vbap~vbeln
    AND lips~vgpos = vbap~posnr
    WHERE lips~vbeln IN lr_vbeln
    AND   lips~posnr EQ ld_first.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_BIG_RANGE
*&---------------------------------------------------------------------*
FORM check_for_big_range.
*--------------------------------------------------------------------*
* Listing 6.3: Checking for Excessive User Selections
*--------------------------------------------------------------------*
  "Local variables
  DATA: l_size   TYPE i,
        l_limit  TYPE i,
        ls_lfdat LIKE LINE OF s_lfdat.

  "Check makes no sense when we are running in background
  CHECK sy-batch EQ space.
  "Only make check if the user has pressed "execute" or "execute in background"
  IF sy-ucomm = 'ONLI'.     "On-Line    -  5 Months
    l_limit = 150.
  ELSEIF sy-ucomm = 'SJOB'. "Background - 24 months
    l_limit = 730.
  ELSE.
    RETURN.                "User has not pressed a processing option
  ENDIF.

  SELECT COUNT( * )        "Count the plants selected
    FROM t001w
    INTO l_size
   WHERE werks IN s_werks.

  l_size = l_size / 5.

  READ TABLE s_lfdat INTO ls_lfdat INDEX 1.

  l_size = l_size + ls_lfdat-high - ls_lfdat-low.

  IF l_size > l_limit.
    IF sy-ucomm = 'SJOB'.
      MESSAGE 'Selection too big even for background processing' TYPE 'I'.
    ENDIF.
    MESSAGE 'For large selections run report in Background' TYPE 'E'.
  ENDIF.

ENDFORM.

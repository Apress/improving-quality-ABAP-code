class ZCL_IQAC_EXAMPLES definition
  public
  create public .

public section.

  types:
    BEGIN OF m_typ_vmast.
      INCLUDE TYPE vmast.
  TYPES:   posnr   TYPE posnr_va.
  TYPES: END OF m_typ_vmast .
protected section.
private section.

  data MO_ALV_GRID type ref to CL_SALV_TABLE .
  data MO_COLUMN type ref to CL_SALV_COLUMN_TABLE .
  data MO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .

  methods DERIVE_MATERIAL_2_BOM_LINK
    importing
      !ID_MATNR type MATNR
      !ID_WERKS type WERKS_D
      !ID_STLAN type MAST-STLAN
      !ID_STLNR type MAST-STLNR
    returning
      value(RS_BOM_LINKS) type MAST .
  methods L01_01_ELEPHANT_FUN
    importing
      !ID_ELEPHANT_NUMBER type INT4 .
  methods L01_02_CIRCUS_RUNAWAY .
  methods L01_04_GETTING_DRUNK .
  methods L02_18_AVOID_DUPLICATE_CODE .
  methods L03_06_TEXT_SYMBOLS_BAD .
  methods L03_07_TEXT_SYMBOLS_GOOD .
  methods L03_09_MISLEADING_DECLARATIONS .
  methods L03_24_BAD_CONSTANTS .
  methods L04_02_BAD_RETURNING_TYPE
    importing
      !ID_BOM_USAGE type MAST-STLAN
    exporting
      value(ES_BOM_LINKS) type M_TYP_VMAST .
  methods L04_04_GOOD_RETURNING_TYPE
    importing
      !ID_BOM_USAGE type MAST-STLAN
    exporting
      value(ES_BOM_LINKS) type M_TYP_VMAST .
  methods L04_18_FIELD_SYMBOLS .
  methods L04_19_CATCHING_EXCEPTIONS .
  methods L04_20_SELF_REPAIR .
  methods L05_05_INTERNAL_BUFFERING_V1
    importing
      !ID_SHIP_TO type KUNWE
    exporting
      !ED_PROJECT type KUNNR
      !ED_PROJECT_NAME type KNA1-NAME1 .
  methods L05_06_INTERNAL_BUFFERING_V2
    importing
      !ID_SHIP_TO type KUNWE
    exporting
      !ED_PROJECT type KUNNR
      !ED_PROJECT_NAME type KNA1-NAME1 .
  methods L05_07_STUTTERING_01 .
  methods L05_08_STUTTERING_02 .
  methods L05_09_EXISTENCE_CHECK .
  methods L05_30_SET_BUFFER_SIZE .
  methods L05_31_PREFILL_EKKO_BUFFER .
  methods L05_32_SELECT_STAR .
  methods L06_04_ALV_FILTER_FIX .
  methods L10_XX_CUSTOM_ATC_CHECK .
  methods GET_DRUNK
    importing
      !IN_PUB type STRING
      !BY_DRINKING type STRING .
  methods FAVORITE_PUB_OF
    importing
      !ID_USER_NAME type SY-UNAME
    returning
      value(RD_FAVORITE_PUB) type STRING .
  methods FAVORITE_DRINK_OF
    importing
      !ID_USER_NAME type SY-UNAME
    returning
      value(RD_FAVORITE_DRINK) type STRING .
  methods TALK_ON_THE_HOUSE_PHONE .
  methods CONTACT_INSURANCE_AGENT .
ENDCLASS.



CLASS ZCL_IQAC_EXAMPLES IMPLEMENTATION.


  METHOD contact_insurance_agent.

  ENDMETHOD.


  METHOD derive_material_2_bom_link.
* Ininetionally left blank, so the RETURNING parameter will be INITIAL
  ENDMETHOD.


  METHOD FAVORITE_DRINK_OF.

    rd_favorite_drink = 'MARSDENS PALE ALE'.

  ENDMETHOD.


  METHOD FAVORITE_PUB_OF.

    rd_favorite_pub = 'HORSE AND JOCKEY'.

  ENDMETHOD.


  method GET_DRUNK.
  endmethod.


  METHOD L01_01_ELEPHANT_FUN.
*--------------------------------------------------------------------*
* Listing 1.1: How RETURNING Parameters make code shorter
*
* This is the OO vesrion where the caller does not have to specify
* the name of the RETURNING parameter
*--------------------------------------------------------------------*
    DATA(elephant) = NEW lcl_elephant( ).

    DATA(trunk_size) =
    elephant->derive_trunk_size( id_elephant_number ).

  ENDMETHOD.


  METHOD L01_02_CIRCUS_RUNAWAY.
*--------------------------------------------------------------------*
* Listing 1.2: Caller being forced to specify parameter names
*
* This is the OO version where you have to explictly say what value
* is being passed to what parameter. This will make it more obvious
* when a value is being passed to the wrong place
*--------------------------------------------------------------------*
    DATA: elephant_name TYPE string VALUE 'Nelly',
          circus_name   TYPE string VALUE 'Horrible Circus'.

    DATA(elephant) = NEW lcl_elephant( ).

    DATA(run_away_flag) = elephant->derive_run_away(
      id_elephant_name = elephant_name
      id_circus_name   = circus_name  ).

  ENDMETHOD.


  METHOD L01_04_GETTING_DRUNK.
*--------------------------------------------------------------------*
* Listing 1.4: Passing Formulas into Methods
*--------------------------------------------------------------------*
* In procedural programming you have to pass constants or variables
* to routine parameters
* In OO programming you can also pass formulas - in this example the
* result of a call to a functional method
* In functional programming (not supported by ABAP as yet) you can
* also pass an entire function (routine) as an importing or exporting
* parameter - this is not like passing an object instance and takes
* a very long time to get ones head around
*--------------------------------------------------------------------*

    me->get_drunk(
    in_pub      = me->favorite_pub_of( sy-uname )
    by_drinking = me->favorite_drink_of( sy-uname ) ).

  ENDMETHOD.


  METHOD L02_18_AVOID_DUPLICATE_CODE.
*--------------------------------------------------------------------*
* Listing 2.18: Alternative to Cutting and Pasting
*--------------------------------------------------------------------*
* What this is demonstrating is instead of cutting and pasting large
* chunks of code it is better to set flags and then call the code
* only once.
* Instead of a flag you could also call a method with the code instead
* Whichever you choose cutting and pasting the code to make dozens of
* identical copies is not the way forward.
*--------------------------------------------------------------------*
* The reason this was in chapter two is because if you have tests in
* place then when adding your new branch you can prove the new branch
* works and you have not broken the other branches.
* Your program has become stronger as a result of the change in that
* in can now do more than before and the test coverage has increased.
*--------------------------------------------------------------------*
* Empty variables just to get the code to compile
    DATA: lf_use_header              TYPE abap_bool,
          item_pterm_not_from_refdoc TYPE abap_bool,
          ls_ref_doc_business_item   TYPE vbkd.

    IF ls_ref_doc_business_item IS INITIAL.
      lf_use_header = abap_false.
* Begin of Insertion
    ELSEIF item_pterm_not_from_refdoc EQ abap_false.
      "Payment terms copy from contract item into SO item
      lf_use_header = abap_false.
* End of Insertion
    ELSEIF ls_ref_doc_business_item-vsart IS INITIAL.
      lf_use_header = abap_true.
    ENDIF.

* Below goes code which would normally have been copied again and again into new branches
    IF lf_use_header EQ abap_true.
      "Code to copy data from the header
    ELSE.
      "Code to copy data from the item
    ENDIF.

  ENDMETHOD.


  METHOD l03_06_text_symbols_bad.
*--------------------------------------------------------------------*
* Listing 3.6 – Bad Usage of Text Symbols
*--------------------------------------------------------------------*
* Anyone reading this would be totally lost
*--------------------------------------------------------------------*

    DATA(lf_cash_sale) = abap_false.

    IF ( lf_cash_sale NE 'X' ).
      "Cash sales not on
      FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
      WRITE: / icon_message_error AS ICON.
      SKIP.
      SKIP.
      FORMAT COLOR COL_NEGATIVE  .
      WRITE:/2   TEXT-116 .
      WRITE:/2   TEXT-117 .
      SKIP.
      FORMAT COLOR COL_KEY .
      WRITE:/2   TEXT-118 .
      WRITE:/2   TEXT-119 .
      SKIP.
      SKIP.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE.
      WRITE:/2   TEXT-120 .
      WRITE:/2   TEXT-121 .

    ENDIF.

  ENDMETHOD.


  METHOD l03_07_text_symbols_good.
*--------------------------------------------------------------------*
* Listing 3.7 – Good Usage of Text Symbols
*--------------------------------------------------------------------*
* Leaving aside the usage of WRITE statements anyone reading this
* can tell what is being output to the screen
*--------------------------------------------------------------------*

    DATA(lf_cash_sale) = abap_false.

    IF lf_cash_sale NE 'X'.
      "Cash sales not on
      FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
      WRITE: / icon_message_error AS ICON.
      SKIP.
      SKIP.
      FORMAT COLOR COL_NEGATIVE.
      WRITE:/2
  '1. Orders must not be delivered unless pricing is OK.'(116).
      WRITE:/2
  '   Please check details and adjust if necessary.'(117).
      SKIP.
      FORMAT COLOR COL_KEY.
      WRITE:/2
  '2. Have you obtained authorisation and entered details on'(118).
      WRITE:/2
  '   to the Order?'(119).
      SKIP.
      SKIP.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED ON INVERSE.
      WRITE:/2
  'If you have problems with the above please contact your'(120).
      WRITE:/2
  'coordinator or responsible manager immediately.'(121).

    ENDIF.

  ENDMETHOD.


  METHOD l03_09_misleading_declarations.
*--------------------------------------------------------------------*
* Listing 3.9 – Misleading Data Declarations
*--------------------------------------------------------------------*

    DATA: lt_plants TYPE STANDARD TABLE OF t001w.

    LOOP AT lt_plants INTO DATA(ls_plants).
      DATA: something TYPE string VALUE 'OCTOPUS'.
    ENDLOOP.

    IF 1 = 2.
      DATA(ld_something) = abap_true.
    ENDIF.

* In other languages data declarations inside LOOPS and IF/ENDIF constructs are local to the loop/construct
* In ABAP they are not, so the following statement would output the values of the variables
    WRITE:/ something,
            ld_something.

  ENDMETHOD.


  METHOD l03_24_bad_constants.
*--------------------------------------------------------------------*
* Listing 3.24 – Bad Constant Names #1
*--------------------------------------------------------------------*
* Blank variables to make the code compile
    DATA: ght_vbup TYPE vbup.

    CONSTANTS: c_pick_status_1    TYPE c VALUE 'A',
               c_pick_status_2    TYPE c VALUE 'B',
               c_picking_complete TYPE c VALUE 'C'.

    "LFSTA = Delivery Status of Sales Document
    CHECK ght_vbup-lfsta = c_pick_status_1 OR
          ght_vbup-lfsta = c_pick_status_2 OR
          "LFGSA = Overall Delivery Status for all items
          ght_vbup-lfgsa = c_pick_status_1 OR
          ght_vbup-lfgsa = c_pick_status_2.

  ENDMETHOD.


  METHOD l04_02_bad_returning_type.
*--------------------------------------------------------------------*
* Listing 4.2: Checking if ES_BOM_LINKS is INITIAL.
*--------------------------------------------------------------------*
* Variables just to get the code to compile
    TYPES: BEGIN OF l_typ_item,
             matnr TYPE matnr,
             werks TYPE werks_d,
             stlnr TYPE mast-stlnr,
           END OF l_typ_item.

    DATA: is_item_details TYPE l_typ_item.

    CLEAR: es_bom_links."NB at this stage ES_BOM_LINKS is INITIAL

    CHECK is_item_details-stlnr IS NOT INITIAL.

    "The method is blank, so the RETURNING parameter is INITIAL
    es_bom_links = derive_material_2_bom_link(
              id_matnr = is_item_details-matnr
              id_werks = is_item_details-werks
              id_stlan = id_bom_usage
              id_stlnr = is_item_details-stlnr ).

    "At this point ES_BOM_LINKS is NOT INITIAL so processing will continue, with unforseeen results
    "This is because the RETURNING parameter from the called method is typed as MAST whereas ES_BOM_LINKS
    "has a different type, with a flag and POSNR on the end, and these get filled with spaces. If POSNR contains
    "spaces it is NOT INITIAL as it's INITIAL value is 000000
    IF es_bom_links IS INITIAL.
      RETURN.
    ENDIF.

* Do lots of things using the values in ES_BOM_LINKS

  ENDMETHOD.


  METHOD L04_04_GOOD_RETURNING_TYPE.
*--------------------------------------------------------------------*
* Listing 4.4: Corrected Code
*--------------------------------------------------------------------*
* Variables just to get the code to compile
    TYPES: BEGIN OF l_typ_item,
             matnr TYPE matnr,
             werks TYPE werks_d,
             stlnr TYPE mast-stlnr,
           END OF l_typ_item.

    DATA: is_item_details TYPE l_typ_item.

    CLEAR: es_bom_links."NB at this stage ES_BOM_LINKS is INITIAL

    CHECK is_item_details-stlnr IS NOT INITIAL.

    "The method is blank, so the RETURNING parameter is INITIAL, so LS_BOM_LINKS is INITIAL
    DATA(ls_bom_links) = derive_material_2_bom_link(
              id_matnr = is_item_details-matnr
              id_werks = is_item_details-werks
              id_stlan = id_bom_usage
              id_stlnr = is_item_details-stlnr ).


    "After this assignment any fields in ES_BOM_LINKS not in LS_BOM_LINKS will remain INITIAL
    MOVE-CORRESPONDING ls_bom_links TO es_bom_links.

    "At this point ES_BOM_LINKS is  INITIAL so processing will terminate as intended
    IF es_bom_links IS INITIAL.
      RETURN.
    ENDIF.

* Do lots of things using the values in ES_BOM_LINKS

  ENDMETHOD.


  METHOD l04_18_field_symbols.
*--------------------------------------------------------------------*
* Listing 4.18: Problem with IS ASSIGNED
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <field> TYPE any.

    TYPES: BEGIN OF l_typ_equi_structure,
             equnr TYPE equi-equnr,
             eqart TYPE equi-eqart,
           END OF   l_typ_equi_structure.

    DATA: ls_target_structure TYPE l_typ_equi_structure.

    DATA(ls_equi) = VALUE equi(
    equnr = 'OCTOPUS'
    eqart = '01' ).

    ASSIGN COMPONENT 'EQUNR' OF STRUCTURE ls_target_structure TO <field>.
    IF sy-subrc = 0.
      "The target structure has field EQUNR so that is set to value OCTOPUS
      <field> = ls_equi-equnr.
    ENDIF.

    ASSIGN COMPONENT 'EQTYP' OF STRUCTURE ls_target_structure TO <field>.
    "The target structure has no EQTYP feild, but the <FIELD> is still assigned
    IF <field> IS ASSIGNED.
      "<FIELD> still points to the EQUNR field in the target structure, and thus the
      "value of that field is changed from OCTOPUS to 01 which is not what was intended
      <field> = ls_equi-eqtyp.
    ENDIF.

  ENDMETHOD.


  METHOD l04_19_catching_exceptions.
*--------------------------------------------------------------------*
* Listing 4.19: Catching an Exception
*--------------------------------------------------------------------*

    TRY.
        me->talk_on_the_house_phone( ).
      CATCH lcx_lightning_strike.
        me->contact_insurance_agent( ).
    ENDTRY.

  ENDMETHOD.


  METHOD l04_20_self_repair.
*--------------------------------------------------------------------*
* Listing 4.20: Recovering from an Unexpected Situation
*--------------------------------------------------------------------*
    DATA: lt_hdr      TYPE STANDARD TABLE OF cdhdr,
          lr_objectid TYPE RANGE OF cdhdr-objectid.

    CHECK lr_objectid[] IS NOT INITIAL."It is in this example class, but that's not the point

    TRY.
        SELECT objectclas objectid changenr FROM cdhdr
          INTO TABLE lt_hdr
         WHERE objectclas = 'VERKBELEG'
           AND objectid   IN lr_objectid
           AND change_ind = 'U'.
      CATCH cx_sy_open_sql_db.
        COMMIT WORK.
        "Possibly too many entries in lr_objectid.
        "Try 'for all entries' instead.
        SELECT objectclas objectid changenr FROM cdhdr
          INTO TABLE lt_hdr
           FOR ALL ENTRIES IN lr_objectid
         WHERE objectclas = 'VERKBELEG'
           AND objectid   = lr_objectid-low
           AND change_ind = 'U'.
    ENDTRY.

  ENDMETHOD.


  METHOD l05_05_internal_buffering_v1.
*--------------------------------------------------------------------*
* Listing 5.5: Projects – Using Internal Buffering – V1
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_project,
             kunn2 TYPE knvp-kunn2,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
           END OF   l_typ_project.

    DATA: gt_proj TYPE STANDARD TABLE OF l_typ_project.

    READ TABLE gt_proj INTO DATA(ls_proj)
    WITH KEY kunn2 = id_ship_to.

    IF sy-subrc = 0.
      ed_project      = ls_proj-kunnr.
      ed_project_name = ls_proj-name1.
    ELSE.
      SELECT kunn2 knvp~kunnr kna1~name1 UP TO 1 ROWS
        INTO (ls_proj-kunn2, ls_proj-kunnr, ls_proj-name1)
        FROM knvp
          INNER JOIN kna1
            ON knvp~kunnr = kna1~kunnr
          WHERE knvp~parvw = 'WE'
            AND knvp~kunn2 = id_ship_to
            AND kna1~ktokd = 'ZPJT'.
      ENDSELECT.

      IF sy-subrc = 0.
        ed_project      = ls_proj-kunnr.
        ed_project_name = ls_proj-name1.
        APPEND ls_proj TO gt_proj.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD l05_06_internal_buffering_v2.
*--------------------------------------------------------------------*
* Listing 5.6: Projects – Using Internal Buffering – V2
*--------------------------------------------------------------------*
* NB 99.99999% of ship-tos do not have a project associated with them
* Thus it is quite important to buffer negative results
*--------------------------------------------------------------------*

    TYPES: BEGIN OF l_typ_project,
             kunn2 TYPE knvp-kunn2,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
           END OF   l_typ_project.

    "Chnage table type from SRANDARD to HASHED
    DATA: gt_proj TYPE HASHED TABLE OF l_typ_project
                  WITH UNIQUE KEY kunn2.

* Clear Exporting Parameters
    CLEAR: ed_project,ed_project_name.

    READ TABLE gt_proj INTO DATA(ls_proj)
    WITH KEY kunn2 = id_ship_to.

    "Putting this in it's own IF/ENDIF block containing a RETURN reduces the IF/ENDIF nesting depth
    IF sy-subrc = 0.
      ed_project      = ls_proj-kunnr.
      ed_project_name = ls_proj-name1.
      RETURN.
    ENDIF.

    SELECT kunn2 knvp~kunnr kna1~name1 UP TO 1 ROWS
      INTO (ls_proj-kunn2, ls_proj-kunnr, ls_proj-name1)
      FROM knvp
        INNER JOIN kna1
          ON knvp~kunnr = kna1~kunnr
        WHERE knvp~parvw = 'WE'
          AND knvp~kunn2 = id_ship_to
          AND kna1~ktokd = 'ZPJT'.
    ENDSELECT.

    IF sy-subrc <> 0.
      "Buffer negative result
      ls_proj-kunn2 = id_ship_to.
      INSERT ls_proj INTO TABLE gt_proj.
    ELSE.
      ed_project      = ls_proj-kunnr.
      ed_project_name = ls_proj-name1.
      "Buffer positive result
      INSERT ls_proj INTO TABLE gt_proj.
    ENDIF.

  ENDMETHOD.


  METHOD l05_07_stuttering_01.
*--------------------------------------------------------------------*
* Listing 5.7: Getting all Related Entries
*--------------------------------------------------------------------*
    DATA: delivery_list TYPE STANDARD TABLE OF lips.

    SELECT *
      FROM vbak
      INTO TABLE @DATA(parent_orders_header_data)
      FOR ALL ENTRIES IN @delivery_list
      WHERE vbeln EQ @delivery_list-vgbel.

  ENDMETHOD.


  METHOD l05_08_stuttering_02.
*--------------------------------------------------------------------*
* Listing 5.8: Ensuring a Unique Selection List
*--------------------------------------------------------------------*

    DATA: delivery_list      TYPE STANDARD TABLE OF lips.
    DATA: lt_selection_table TYPE STANDARD TABLE OF sales_key.

    CHECK delivery_list[] IS NOT INITIAL.

    LOOP AT delivery_list ASSIGNING FIELD-SYMBOL(<delivery_data>).
      INSERT VALUE #(
      vbeln = <delivery_data>-vgbel )
      INTO TABLE lt_selection_table.
    ENDLOOP.

    SORT lt_selection_table BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_selection_table
    COMPARING vbeln.

    SELECT *
      FROM vbak
      INTO TABLE @DATA(parent_orders_header_data)
      FOR ALL ENTRIES IN @lt_selection_table
      WHERE vbeln EQ @lt_selection_table-vbeln.

  ENDMETHOD.


  METHOD l05_09_existence_check.

    DATA: contract_number TYPE vbak-vbeln VALUE '1234567890'.

*--------------------------------------------------------------------*
* Listing 5.9: Getting all records and columns as an existence check
*--------------------------------------------------------------------*
* This is BAD
*--------------------------------------------------------------------*
    SELECT *
      FROM vbuv
      INTO TABLE @DATA(incompletion_details)
      WHERE vbeln = @contract_number.

*--------------------------------------------------------------------*
* Listing 5.10: Using COUNT as an existence check
*--------------------------------------------------------------------*
* This is the best way prior to ABAP 7.50
*--------------------------------------------------------------------*
    SELECT COUNT( * )
      FROM vbuv
      WHERE vbeln = contract_number.

*--------------------------------------------------------------------*
* Listing 5.11: Best way to do an existence check
*--------------------------------------------------------------------*
* From 7.50 and up this is the best way to do an existence check
*--------------------------------------------------------------------*
    SELECT @abap_true
      FROM vbuv UP TO 1 ROWS
      INTO @DATA(record_exists)
      WHERE vbeln = @contract_number.
    ENDSELECT.

  ENDMETHOD.


  METHOD l05_30_set_buffer_size.
*--------------------------------------------------------------------*
* Listing 5.30: Setting the maximum buffer size for KNA1_SINGLE_READER
*--------------------------------------------------------------------*
    "Set buffer size for customer reads - picked up by Function Module KNA1_SINGLE_READER
    DATA ld_value TYPE xuvalue VALUE '5000'.
    SET PARAMETER ID 'VS_GEN_READ_BUFFER' FIELD ld_value.

  ENDMETHOD.


  METHOD l05_31_prefill_ekko_buffer.
*--------------------------------------------------------------------*
* Listing 5.31: Prefilling the EKKO Buffer
*--------------------------------------------------------------------*

    DATA: lt_selection_table TYPE STANDARD TABLE OF ekko_key,
          gt_all_sources     TYPE STANDARD TABLE OF eord.

    CHECK gt_all_sources[] IS NOT INITIAL.

    LOOP AT gt_all_sources ASSIGNING FIELD-SYMBOL(<ls_sources>).
      INSERT VALUE #(
      ebeln = <ls_sources>-ebeln )
      INTO TABLE lt_selection_table.
    ENDLOOP.

    SORT lt_selection_table BY ebeln.
    DELETE ADJACENT DUPLICATES FROM lt_selection_table
    COMPARING ebeln.

    CALL FUNCTION 'ME_EKKO_ARRAY_READ'
      TABLES
        pti_ekko_keytab          = lt_selection_table[]
      EXCEPTIONS
        err_no_records_requested = 1
        err_no_records_found     = 2
        OTHERS                   = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD l05_32_select_star.
*--------------------------------------------------------------------*
* Listing 5.32: Two ways of reading fields from table MARA
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_material,
             matnr TYPE mara-matnr,
             matkl TYPE mara-matkl,
             meins TYPE mara-meins,
           END   OF l_typ_material.

    DATA: ls_material TYPE l_typ_material.

* Use Field List
    SELECT SINGLE matnr matkl meins
      FROM mara
      INTO ls_material
      WHERE matnr = '00000000000001086'.

* Use INTO CORRESPONDING
    SELECT SINGLE *
     FROM mara
     INTO CORRESPONDING FIELDS OF ls_material
     WHERE matnr = '00000000000001086'.

* The SQL trace from ST05 will be identical for both statements therefore they will have identical performance
* The database is clever enough to transform the * into a list of fields in the target structure
* So only use SELECT * if you have a target structure with just the fields you actually need

  ENDMETHOD.


  METHOD l06_04_alv_filter_fix.
*--------------------------------------------------------------------*
* Listing 6.4: Forcing an ALV column to accept lower case values
*--------------------------------------------------------------------*
* After the below even if the field in the internal table refers to
* a case insensitive domain, the built in ALV filter will work
*--------------------------------------------------------------------*
    TRY.
        mo_columns = mo_alv_grid->get_columns( ).

        mo_column ?= mo_columns->get_column( 'BNAME' ).

        mo_column->set_lowercase( abap_true ).

      CATCH cx_salv_not_found.
        MESSAGE 'Report in Trouble' TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD L10_XX_CUSTOM_ATC_CHECK.
*--------------------------------------------------------------------*
* This is an example of some code which will trigger the custom ATC
* check defined in Chapter 10
*--------------------------------------------------------------------*
    SELECT SINGLE @abap_true
      FROM t001w
      INTO @DATA(record_exists)
      WHERE werks = 'ABCD'.

    IF sy-subrc IS INITIAL."#EC_GET_A_LIFE
      WRITE:/ 'No Such Plant'.
    ENDIF.

  ENDMETHOD.


  METHOD talk_on_the_house_phone.

  ENDMETHOD.
ENDCLASS.

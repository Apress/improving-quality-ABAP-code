*&---------------------------------------------------------------------*
*&  Include           ZVPDH_TEMPLATE_TOP
*&---------------------------------------------------------------------*
REPORT  ziqac_alv_template."The REPORT statement goes here to supress false syntax errors
*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
TYPES: BEGIN OF g_typ_alv_output.
        INCLUDE STRUCTURE usr05.
TYPES: END OF   g_typ_alv_output.

TYPES: g_tt_alv_output TYPE STANDARD TABLE OF g_typ_alv_output WITH EMPTY KEY.

TYPES: BEGIN OF g_typ_selections,
         bname TYPE usr05-bname,
       END OF   g_typ_selections.

*--------------------------------------------------------------------*
* Constants
*--------------------------------------------------------------------*
CONSTANTS: gc_drill_down TYPE sy-ucomm VALUE '&IC1'.

*--------------------------------------------------------------------*
* Global Variables
* There is gereally only one global variable you should ever need in
* an ALV style report program
*--------------------------------------------------------------------*
DATA: gs_selection_screen TYPE g_typ_selections ##needed."To avoid the need for TABLES

*--------------------------------------------------------------------*
* Local Classes
*--------------------------------------------------------------------*
INCLUDE ziqac_alv_template_cd01.

DATA: go_selections TYPE REF TO lcl_selections ##NEEDED.

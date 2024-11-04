*&---------------------------------------------------------------------*
*& Report ZRELATORIO_FABRICA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio_fabrica.

INCLUDE: z_relatorio_topOO,
z_relatorio_selOO,
z_relatorio_class.

START-OF-SELECTION.

  DATA: go_relatorio TYPE REF TO z_relatorio.

  go_relatorio =  NEW z_relatorio( ).

  go_relatorio->display_data_salv( ).

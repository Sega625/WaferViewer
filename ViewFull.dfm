object FullChipsDlg: TFullChipsDlg
  Left = 339
  Top = 121
  Width = 367
  Height = 156
  BorderStyle = bsSizeToolWin
  Caption = #1042#1089#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = AnyCompMouseDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ParamsSG: TStringGrid
    Left = 6
    Top = 6
    Width = 339
    Height = 107
    Color = clMenu
    Ctl3D = False
    DefaultRowHeight = 18
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentCtl3D = False
    TabOrder = 0
    OnDrawCell = ParamsSGDrawCell
    OnMouseDown = ParamsSGMouseDown
    OnMouseUp = ParamsSGMouseUp
    OnMouseWheelDown = ParamsSGMouseWheelDown
    OnMouseWheelUp = ParamsSGMouseWheelUp
    OnSelectCell = ParamsSGSelectCell
    RowHeights = (
      18
      18
      18
      18
      18)
  end
end

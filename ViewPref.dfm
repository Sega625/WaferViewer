object PrefDlg: TPrefDlg
  Left = 528
  Top = 220
  BorderStyle = bsToolWindow
  Caption = ' '#1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 351
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object InfoGroup: TGroupBox
    Left = 7
    Top = 179
    Width = 260
    Height = 132
    BiDiMode = bdLeftToRight
    Caption = ' '#1056#1072#1089#1087#1088#1077#1076#1077#1083#1077#1085#1080#1077' '
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentBiDiMode = False
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    object ChipsColLab: TLabel
      Tag = 2
      Left = 10
      Top = 24
      Width = 20
      Height = 20
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = '  '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object Label2: TLabel
      Left = 38
      Top = 25
      Width = 190
      Height = 16
      Caption = #1062#1074#1077#1090' '#1087#1086#1076#1089#1074#1077#1090#1082#1080' '#1082#1088#1080#1089#1090#1072#1083#1083#1086#1074
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ViewFlatRB: TRadioButton
      Left = 13
      Top = 58
      Width = 169
      Height = 17
      Caption = #1055#1083#1086#1089#1082#1080#1077' '#1089#1090#1086#1083#1073#1094#1099
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = ColumnsRBClick
    end
    object View3D1RB: TRadioButton
      Tag = 1
      Left = 13
      Top = 82
      Width = 177
      Height = 17
      Caption = #1054#1073#1098#1077#1084#1085#1099#1077' '#1089#1090#1086#1083#1073#1094#1099' 1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = ColumnsRBClick
    end
    object View3D2RB: TRadioButton
      Tag = 2
      Left = 13
      Top = 106
      Width = 177
      Height = 17
      Caption = #1054#1073#1098#1077#1084#1085#1099#1077' '#1089#1090#1086#1083#1073#1094#1099' 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = ColumnsRBClick
    end
  end
  object MainGroup: TGroupBox
    Left = 7
    Top = 6
    Width = 260
    Height = 163
    BiDiMode = bdLeftToRight
    Caption = ' '#1054#1089#1085#1086#1074#1085#1099#1077' '
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentBiDiMode = False
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 1
    object BackGColLab: TLabel
      Left = 11
      Top = 24
      Width = 20
      Height = 20
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = '  '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object Label3: TLabel
      Left = 39
      Top = 25
      Width = 72
      Height = 16
      Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CadreColLab: TLabel
      Tag = 1
      Left = 11
      Top = 51
      Width = 20
      Height = 20
      AutoSize = False
      BiDiMode = bdLeftToRight
      Caption = '  '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object Label4: TLabel
      Left = 39
      Top = 52
      Width = 136
      Height = 16
      Caption = #1062#1074#1077#1090' '#1075#1088#1072#1085#1080#1094' '#1082#1072#1076#1088#1086#1074
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ShowAllRB: TRadioButton
      Left = 13
      Top = 88
      Width = 204
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1074#1089#1102' '#1087#1083#1072#1089#1090#1080#1085#1091
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = ShowRBClick
    end
    object ShowChipsRB: TRadioButton
      Tag = 1
      Left = 13
      Top = 112
      Width = 235
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1082#1088#1080#1089#1090#1072#1083#1083#1099
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = ShowRBClick
    end
    object ShowGridChB: TCheckBox
      Left = 13
      Top = 137
      Width = 150
      Height = 17
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1089#1077#1090#1082#1091
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = ShowGridChBClick
    end
  end
  object BitBtn1: TBitBtn
    Left = 7
    Top = 318
    Width = 261
    Height = 26
    Caption = #1047#1072#1082#1088#1099#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    Kind = bkOK
  end
end

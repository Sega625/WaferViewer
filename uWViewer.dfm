object Form1: TForm1
  Left = 454
  Top = 150
  Width = 983
  Height = 826
  AlphaBlend = True
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = 1573
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object WafInfoGroup: TGroupBox
    Left = 5
    Top = -7
    Width = 223
    Height = 640
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 1
    object CodeLab: TLabel
      Left = 55
      Top = 45
      Width = 161
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NLotLab: TLabel
      Left = 80
      Top = 320
      Width = 136
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NLotLab0: TLabel
      Left = 19
      Top = 320
      Width = 54
      Height = 18
      Caption = #1055#1072#1088#1090#1080#1103':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NWafLab0: TLabel
      Left = 19
      Top = 340
      Width = 51
      Height = 18
      Caption = #1053#1086#1084#1077#1088':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NWafLab: TLabel
      Left = 71
      Top = 340
      Width = 144
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLab0: TLabel
      Left = 20
      Top = 290
      Width = 45
      Height = 18
      Caption = #1048#1085#1092#1086':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLab: TLabel
      Left = 68
      Top = 290
      Width = 149
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object WPlaceLab: TLabel
      Left = 103
      Top = 360
      Width = 113
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object WPlaceLab0: TLabel
      Left = 19
      Top = 360
      Width = 82
      Height = 18
      Caption = #1056#1072#1073'. '#1084#1077#1089#1090#1086':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object OperatorLab0: TLabel
      Left = 19
      Top = 380
      Width = 72
      Height = 18
      Caption = #1054#1087#1077#1088#1072#1090#1086#1088':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object OperatorLab: TLabel
      Left = 95
      Top = 380
      Width = 121
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
    end
    object DateLab: TLabel
      Left = 63
      Top = 400
      Width = 153
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XX.XX.XXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DateLab0: TLabel
      Left = 19
      Top = 400
      Width = 39
      Height = 18
      Caption = #1044#1072#1090#1072':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NMeasLab0: TLabel
      Left = 20
      Top = 470
      Width = 75
      Height = 18
      Caption = #1048#1079#1084#1077#1088#1077#1085#1086':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NMeasLab: TLabel
      Left = 105
      Top = 470
      Width = 112
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NOKLab: TLabel
      Left = 85
      Top = 490
      Width = 132
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NOKLab0: TLabel
      Left = 20
      Top = 490
      Width = 57
      Height = 18
      Caption = #1043#1086#1076#1085#1099#1093':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object OKCol: TLabel
      Left = 6
      Top = 491
      Width = 11
      Height = 20
      AutoSize = False
      Caption = '  '
      Color = clLime
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object NCCol: TLabel
      Tag = 1
      Left = 6
      Top = 511
      Width = 11
      Height = 20
      AutoSize = False
      Caption = '  '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object SCCol: TLabel
      Tag = 2
      Left = 6
      Top = 531
      Width = 11
      Height = 20
      AutoSize = False
      Caption = '  '
      Color = clRed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object FCCol: TLabel
      Tag = 3
      Left = 6
      Top = 551
      Width = 11
      Height = 20
      AutoSize = False
      Caption = '  '
      Color = clBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      OnClick = ColLabClick
    end
    object NFailNCLab0: TLabel
      Left = 20
      Top = 510
      Width = 92
      Height = 18
      Caption = #1041#1088#1072#1082#1072' '#1087#1086' '#1053#1050':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NFailNCLab: TLabel
      Left = 115
      Top = 510
      Width = 102
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NFailSCLab: TLabel
      Left = 116
      Top = 530
      Width = 102
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NFailSCLab0: TLabel
      Left = 20
      Top = 530
      Width = 92
      Height = 18
      Caption = #1041#1088#1072#1082#1072' '#1087#1086' '#1057#1050':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NFailFCLab0: TLabel
      Left = 20
      Top = 550
      Width = 94
      Height = 18
      Caption = #1041#1088#1072#1082#1072' '#1087#1086' '#1060#1050':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NFailFCLab: TLabel
      Left = 116
      Top = 550
      Width = 102
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object PathLab0: TLabel
      Left = 20
      Top = 605
      Width = 50
      Height = 18
      Caption = #1054#1073#1093#1086#1076':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DirBox: TPaintBox
      Left = 82
      Top = 603
      Width = 28
      Height = 28
      OnPaint = DirBoxPaint
    end
    object CutLab: TLabel
      Left = 64
      Top = 270
      Width = 153
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CutLab0: TLabel
      Left = 20
      Top = 270
      Width = 39
      Height = 18
      Caption = #1057#1088#1077#1079':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InformationLab0: TLabel
      Left = 50
      Top = 4
      Width = 113
      Height = 19
      Caption = ' '#1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object NTotLab0: TLabel
      Left = 20
      Top = 450
      Width = 45
      Height = 18
      Caption = #1042#1089#1077#1075#1086':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object NTotLab: TLabel
      Left = 72
      Top = 450
      Width = 145
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DiamLab0: TLabel
      Left = 20
      Top = 210
      Width = 66
      Height = 18
      Caption = #1044#1080#1072#1084#1077#1090#1088':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StepXLab0: TLabel
      Left = 20
      Top = 230
      Width = 66
      Height = 18
      Caption = #1064#1072#1075' '#1087#1086' X:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StepYLab0: TLabel
      Left = 20
      Top = 250
      Width = 65
      Height = 18
      Caption = #1064#1072#1075' '#1087#1086' Y:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StepYLab: TLabel
      Left = 88
      Top = 250
      Width = 128
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StepXLab: TLabel
      Left = 88
      Top = 230
      Width = 129
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DiamLab: TLabel
      Left = 90
      Top = 210
      Width = 126
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object OKRLab0: TLabel
      Left = 20
      Top = 25
      Width = 36
      Height = 18
      Caption = #1054#1050#1056':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CodeLab0: TLabel
      Left = 20
      Top = 45
      Width = 32
      Height = 18
      Caption = #1050#1086#1076':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MPWLab0: TLabel
      Left = 20
      Top = 65
      Width = 42
      Height = 18
      Caption = 'MPW:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DeviceLab0: TLabel
      Left = 20
      Top = 105
      Width = 65
      Height = 18
      Caption = #1048#1079#1076#1077#1083#1080#1077':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MPWLab: TLabel
      Left = 63
      Top = 65
      Width = 153
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object OKRLab: TLabel
      Left = 55
      Top = 25
      Width = 161
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DeviceLab: TLabel
      Left = 87
      Top = 105
      Width = 129
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CondLab0: TLabel
      Left = 18
      Top = 420
      Width = 64
      Height = 18
      Caption = #1059#1089#1083#1086#1074#1080#1103':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object CondLab: TLabel
      Left = 86
      Top = 420
      Width = 129
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MSystemLab0: TLabel
      Left = 20
      Top = 160
      Width = 82
      Height = 18
      Caption = #1048#1079#1084'. '#1089#1090#1077#1085#1076':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MSystemLab: TLabel
      Left = 105
      Top = 160
      Width = 112
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DscrDevLab0: TLabel
      Left = 20
      Top = 125
      Width = 72
      Height = 18
      Caption = #1054#1087#1080#1089#1072#1085#1080#1077':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object DscrDevLab: TLabel
      Left = 95
      Top = 125
      Width = 121
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ProberLab0: TLabel
      Left = 20
      Top = 180
      Width = 40
      Height = 18
      Caption = #1047#1086#1085#1076':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ProberLab: TLabel
      Left = 61
      Top = 180
      Width = 156
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MPWPosLab0: TLabel
      Left = 20
      Top = 85
      Width = 118
      Height = 18
      Caption = #1055#1086#1079#1080#1094#1080#1103' '#1074' MPW:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MPWPosLab: TLabel
      Left = 140
      Top = 85
      Width = 75
      Height = 18
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'XXXXXX'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object AdjGroup: TGroupBox
    Left = 5
    Top = 635
    Width = 223
    Height = 128
    Color = clBtnFace
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    object AdjustLab0: TLabel
      Left = 60
      Top = 4
      Width = 94
      Height = 19
      Caption = ' '#1053#1072#1089#1090#1088#1086#1081#1082#1080' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object TransBtn: TSpeedButton
      Left = 155
      Top = 99
      Width = 23
      Height = 22
      OnMouseDown = TransBtnMouseDown
      OnMouseUp = TransBtnMouseUp
    end
    object TransTBar: TTrackBar
      Left = 157
      Top = 19
      Width = 22
      Height = 80
      BorderWidth = 1
      Ctl3D = False
      Max = 255
      Min = 50
      Orientation = trVertical
      ParentCtl3D = False
      Position = 50
      TabOrder = 0
      TabStop = False
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = TransTBarChange
    end
    object StaticText1: TStaticText
      Left = 128
      Top = 95
      Width = 25
      Height = 4
      AutoSize = False
      TabOrder = 1
    end
  end
  object MainMenu: TMainMenu
    Images = MainImList
    Left = 234
    Top = 64
    object mmFile: TMenuItem
      Caption = '    '#1060#1072#1081#1083
      object mOpen: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1100
        ImageIndex = 0
        ShortCut = 16463
        OnClick = mOpenClick
      end
      object mAdd: TMenuItem
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100
        ImageIndex = 18
        ShortCut = 16452
        OnClick = mAddClick
      end
      object mSave: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1082' ...'
        ImageIndex = 15
        ShortCut = 16467
        OnClick = mSaveClick
      end
      object mPrint: TMenuItem
        Caption = #1055#1077#1095#1072#1090#1100
        ImageIndex = 8
        ShortCut = 16464
        OnClick = mPrintClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mPrMap: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1088#1090#1091' '#1086#1073#1093#1086#1076#1072
        ImageIndex = 16
        ShortCut = 16459
        OnClick = mPrMapClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mAdjust: TMenuItem
        Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
        ImageIndex = 17
        ShortCut = 16449
        OnClick = mAdjustClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mExit: TMenuItem
        Caption = #1042#1099#1093#1086#1076
        ImageIndex = 3
        OnClick = mExitClick
      end
    end
    object mmAnalize: TMenuItem
      Caption = '   '#1040#1085#1072#1083#1080#1079
      object mExcel: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1100' '#1074' Excel'
        ImageIndex = 21
        OnClick = mExcelClick
      end
      object mFull: TMenuItem
        Caption = #1055#1086#1083#1085#1072#1103' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1103
        ImageIndex = 20
        OnClick = mFullClick
      end
      object mFails: TMenuItem
        Caption = #1056#1072#1079#1073#1088#1072#1082#1086#1074#1082#1072
        ImageIndex = 4
        ShortCut = 16466
        OnClick = mFailsClick
      end
      object mData: TMenuItem
        Caption = #1047#1085#1072#1095#1077#1085#1080#1103
        ImageIndex = 14
        ShortCut = 16474
        OnClick = mDataClick
      end
      object mOK: TMenuItem
        Caption = #1056#1072#1089#1087#1088#1077#1076#1077#1083#1077#1085#1080#1077
        ImageIndex = 13
        ShortCut = 16453
        OnClick = mOKClick
      end
      object mPD: TMenuItem
        Caption = #1055#1044
        ImageIndex = 5
        ShortCut = 16468
        OnClick = mPDClick
      end
    end
    object mmAbout: TMenuItem
      Caption = '   '#1055#1086#1084#1086#1097#1100
      object mHelp: TMenuItem
        Caption = ' '#1055#1086#1084#1086#1097#1100
        ImageIndex = 10
        ShortCut = 112
        OnClick = mHelpClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mAbout: TMenuItem
        Caption = ' '#1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
        ImageIndex = 9
        OnClick = mAboutClick
      end
    end
  end
  object MainImList: TImageList
    Left = 264
    Top = 64
    Bitmap = {
      494C010116001800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
      00000000000000000000000000000000000000000000000000009F804B009776
      4100977641009776410097764100977641009776410097764100977641009776
      410097764100987343009873430000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A07F50000000
      0000F9FFFF000000000000000000000000000000000000000000000000000000
      000000000000000000008C65240000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFC9D6BEFF89A572FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AC875500E7FC
      FF000087FA00BBE4EC00CB8C6900BB876700BA886700B8866500B7856600B37B
      5A00D2AD9C00F5F7F0008C65240000000000000000FFF8FAF7FFB3C6A5FF6D91
      51FF44721FFF44721FFF44721FFF44721FFF4E782BFF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AF9162000000
      0000FEF6F100FDF6F100FAF9F500FAFBF900FAFBF900FBFCF700F7FBF200F8F9
      F500FFFFFE00F1EBE4008C65240000000000A4BA92FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF487424FFA3A89FFFCCD2C6FFCCD2
      C6FFCCD2C6FFCCD2C6FF6D8B55FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BB986900D9F4
      FF000087FA00BAE2E900CF906C00BF896900C08C6C00C08B6C00C28C6E00BD82
      5F00D7B39F00F5F3ED008C65240000000000A4BA92FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF42701EFF305114FF476F28FF3A65
      18FF3B661AFF8EA47CFF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B09465000000
      0000FEF6F100FEF6F200FBF9F400F8FBF800FBFDFA00FBFBFA00FFFEFF00FEFC
      FB0000000000F1ECE3008C65240000000000A4BA92FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF467222FF7C8B71FFA1B293FF9BAD
      8CFF9CAE8DFFBFC9B8FF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9986700D8F5
      FF000087FA00BAE2E800CE8E6B00BE886900C38B6E00C5916C00C5917200BF83
      6200D5B09A00F6F6F0008C65240000000000A4BA92FF44721FFF598138FFE6EC
      E1FF44721FFFFFFFFFFF44721FFF44721FFF42701EFF315216FF446C25FF3B66
      1AFF3D671CFF8EA47DFF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B0946B000000
      0000FEF6EF00FBF6F000F9F9F400FCFAFC00FEFFFE000000000000000000F8F6
      F300F9F7F300F2EDE6008C65240000000000A4BA92FF44721FFF44721FFFF4F6
      F2FFC7D5BCFF6D9150FF44721FFF44721FFF477324FF959D8EFFBDC7B5FFBAC5
      B2FFBAC5B2FFCFD5CBFF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B9A17D00E3F7
      FF000087FA00BFEAED00D1916F00BF8B6C00C28C6E00C08C6E00BB876400AE75
      5400C6A38D00ECE4D9008C65240000000000A4BA92FF44721FFF44721FFF6F93
      53FFFFFFFFFF44721FFF44721FFF44721FFF42701EFF315216FF466E27FF3B66
      1AFF3D671CFF8EA47DFF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BCA686000000
      0000FFFFFE00FFFFFC0000000000FEFFFF0000000000FAFEF800F6F8F400EDEB
      E500EAE3DC00D8CDB7008C65240000000000A4BA92FF44721FFF44721FFFFEFE
      FEFF96B081FF94AE7FFF44721FFF44721FFF447120FF516A3DFF6D8B56FF6585
      4CFF66864DFFA4B497FF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CCB39000E1F3
      FB000087FA00BEE8F300D0917100C08C6A00BF8A6700BB866400A66C44009854
      2700AB7A5600C3B08E008C65240000000000A4BA92FF44721FFF4E792BFFC8D5
      BDFF44721FFFFEFEFEFF44721FFF44721FFF447120FF516A3CFF6C8A54FF6484
      4AFF65854CFFA3B496FF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8B595000000
      0000000000000000000000000000FCFFF800F9FAF800E4DDD200D3C6A900F6F7
      F300E8E4D500B79D73008C65240000000000A4BA92FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF42701EFF315216FF486F29FF3B66
      1AFF3D671CFF8EA47DFF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CFB69A00DFF3
      F9000087FA00C0EAF100CA8C6700B2795A00B67B5C00985E3700CCB99C000000
      0000CFBAA000A07D4600FBFAF70000000000A4BA92FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF487424FF9CA397FFC6CEBFFFC3CC
      BCFFC3CCBDFFD4D8D0FF769160FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7B79A000000
      000000000000FFFDF700FCFDFB00EFF1ED00EEE9DD00D4C5A900BCAC8900D7C9
      B50099733500000000000000000000000000B4C7A6FF44721FFF44721FFF4472
      1FFF44721FFF44721FFF44721FFF44721FFF42701EFF305115FF3C661BFF3C66
      1BFF3C661BFF3C661BFF426B23FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CBBDA1000000
      000000000000F9F7F700F0E7DE00DFD7C700D8C5B000C1AA8500AA916100A17E
      470000000000000000000000000000000000000000FF000000FF000000FF0000
      00FFCAD7C0FF84A26CFF467321FF44721FFF4E782BFF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4B29200CEBD
      A300CCBBA000C9B69A00C8B69900C4AF8C00C3AB8400AA916100936C32000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00395A0000007B2900005A1800005A1800005A0000185A0000F7FF
      FF00000000000000000000000000000000000000000000000000D66B5A00D66B
      5A00D66B5A00D66B5A00D66B5A00D66B5A00D66B5A00D66B5A00D66B5A00D66B
      5A00D66B5A00D66B5A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6E7E7004A947B004A947B004A947B004A947B00D6E7E700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7FFFF00297B00006BFF6B0000BD390000BD390000BD3900185A0000F7FF
      FF00000000000000000000000000000000000000000000000000D6735A00FFEF
      E700FFEFE700FFEFE700FFE7DE00FFE7DE00FFE7DE00FFE7DE00FFE7DE00FFDE
      D600FFDED600D66B5A0000000000000000000000000000000000000000000000
      00000000000000000000005AAD000063BD00005AA500004A8C0000529C00004A
      8C00000000000000000000000000000000000000000000000000000000000000
      0000BDD6CE00005A3100006B4200006B420000633100BDD6CE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000297B00006BFF6B0000DE420000BD3900009C3100185A0000F7FF
      FF00000000000000000000000000000000000000000000000000D6736300FFEF
      E700DE948400DE948400DE948400DE948400DE948400DE948400DE948400DE94
      8400FFDED600D66B5A0000000000000000000000000000000000000000000000
      00000000000000529C001073CE003194E700318CCE00106BBD00107BCE000863
      B500004A8C000000000000000000000000000000000000000000000000000000
      0000BDD6CE0008734A00187B5200187B520008734A00BDD6CE00000000000000
      000000000000000000000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00393939006BFF6B0000DE420000BD3900009C3100005A1800FFFF
      FF0000000000FFFFFF0000000000FFFFFF000000000000000000D67B6300FFEF
      EF00FFEFEF00FFEFEF00FFEFE700FFEFE700FFE7DE00FFE7DE00FFE7DE00FFE7
      DE00FFE7DE00D66B5A000000000000000000000000000000000000000000005A
      AD00005AAD00085AAD004294D6007BC6FF006BBDF70042A5EF0052ADF7003994
      DE00085AA5000000000000000000000000000000000000000000000000000000
      0000C6DED60008734A00187B5A00187B5A0008734A00C6DED600000000000000
      000000000000000000000000000000000000EFEFEF00EFEFEF00FFFFFF00FFFF
      FF00EFEFEF00005A39006BFF6B0000DE420000DE420000BD3900005A1800EFEF
      EF00EFEFEF00FFFFFF00F7FFFF00EFEFEF000000000000000000DE7B6B00FFF7
      EF00DE948400DE948400DE948400DE948400DE948400DE948400DE948400DE94
      8400FFE7DE00D66B5A000000000000000000000000000000000000000000006B
      CE00218CDE004294DE005AADE7006BBDF70052B5F7004AADF70052B5FF003194
      DE00086BB500005294000000000000000000DEEFE700BDD6CE00BDDECE00C6DE
      D60094BDAD00107B520021845A0021846300107B520094BDAD00C6DED600C6DE
      CE00BDD6CE00DEEFE700000000000000000052525200007B2900005A1800185A
      0000007B2900005A18004AFF840000DE420000DE4200009C3100005A0000005A
      1800005A1800005A1800005A1800007B29000000000000000000DE846B00FFF7
      EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFEFE700FFEFE700FFE7E700FFE7
      E700FFE7DE00D66B5A0000000000000000000000000000000000000000001073
      C6005AADEF0084C6FF005AADE7003184C6001863A5001873B5003194E700299C
      F700188CE700086BBD0000000000000000005AA58C00087B4A0018845A00187B
      5A0018845A00298C6300298C6300298C6300298C630018845A0018845A001884
      5A00087B5200529C84000000000000000000009C310000BD390000BD390000BD
      390000BD390000BD39004AFF840000DE420000DE420000BD390000BD3900009C
      310000BD390000BD3900009C3100007B29000000000000000000DE847300FFF7
      F700DE948400DE948400DE948400DE948400DE948400DE948400DE948400DE94
      8400FFEFE700D66B5A000000000000000000000000000000000000000000106B
      BD004A9CDE006BBDF700398CCE000000000000000000004A8C00187BC600189C
      F700088CE700006BB500000000000000000063A58C0029946300319473003194
      6B0031946B0031946B0031946B0031946B0031946B0031946B0031946B003194
      7300299463005AA58C000000000000000000009C310021FF6B0000BD7B0000DE
      420000BD7B0000BD7B0021FF6B0000BD7B0021FF6B0000DE420000DE420021FF
      6B0000DE420000DE420000DE4200007B29000000000000000000DE8C7300FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7EF00FFEFE700FFEF
      E700FFEFE700D66B5A0000000000000000000000000000000000086BBD002184
      D6004AADEF0052ADEF002173BD0000529C0000529C000052A500107BCE001094
      EF000073C60000529C00000000000000000063AD9400319C6B00399C7B00399C
      7300399C7300399C7300399C7300399C7300399C7300399C7300399C7300399C
      7300319C6B0063A58C000000000000000000009C310000DE940000DE940000DE
      940000BD7B0021FF6B0021FF6B0000BD7B0000BD7B0000DE420000BD7B0000DE
      420000BD7B0000BD390000DE4200007B29000000000000000000E78C7B00FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7EF00EFBDB500EFBDAD00EFBD
      AD00EFBDAD00D66B5A0000000000000000000000000000000000297BC60063AD
      E7006BBDFF004AADEF00187BC600006BBD000063BD00086BC600108CE7000894
      F700007BD60000000000000000000000000063AD8C00219C6B0031A57300319C
      730031A5730042A57B0042A57B0042A57B0042A57B0031A5730031A5730031A5
      7300299C6B005AA58C000000000000000000009C6B004AFF84004AFF84006BFF
      9C004AFF84004AFF840021FF6B0021FF6B0000BD7B0000BD7B0021FF6B004AFF
      84004AFF84004AFF84004AFF8400009C31000000000000000000E7947B00FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFE7DE00DE8C7300E7A59C00E7A5
      9400EFAD9C00D66B5A000000000000000000000000000000000000000000428C
      CE00429CDE0042A5EF00319CE7001884D6001084D600108CE7000884DE00007B
      D600006BBD00000000000000000000000000DEEFE700C6DED600C6DED600C6DE
      D60094C6B50039A573004AAD84004AAD840039A5730094C6B500C6DED600C6DE
      D600C6DED600DEEFE7000000000000000000009C3100009C6B00009C3100009C
      3100009C310000BD39004AFF840021FF6B0000BD7B0000BD7B00007B2900009C
      3100009C3100007B2900009C3100007B29000000000000000000E7948400FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700F7DED600DE948400FFEFEF00FFEF
      E700FFEFEF00D6735A0000000000000000000000000000000000000000000000
      00001873BD00399CE70039ADFF00299CEF00189CF7001094EF000073C6000052
      9C00000000000000000000000000000000000000000000000000000000000000
      0000CEDEDE0039A57B0052B58C0052B58C0039A57B00CEDED600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7FFFF00009C31006BFF9C0021FF6B0000DE940000BD7B00009C3100F7FF
      FF00000000000000000000000000000000000000000000000000EF9C8400FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700F7DED600E7948400FFF7F700FFF7
      F700D67363000000000000000000000000000000000000000000000000000000
      0000106BB500318CD600319CE700187BCE00087BCE00087BCE00005AAD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6DED60042AD7B005ABD94005ABD940042AD7B00BDDECE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7FFFF00009C31006BFF9C0000DE940000DE940000BD7B00009C3100F7FF
      FF00000000000000000000000000000000000000000000000000EF9C8C00FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700F7DED600DE948400FFF7F700DE7B
      6B00000000000000000000000000000000000000000000000000000000000000
      000000000000000000001873BD0000000000005AAD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDDECE0042AD7B005AC694005AC6940039AD7B00BDD6CE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000009C31006BFF9C0000DE940000DE940000BD7B00009C31000000
      0000000000000000000000000000000000000000000000000000EFA58C00EFA5
      8C00EF9C8400E79C8400E7947B00E7947B00E78C7300E78C7300DE8473000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DEE7E7006BBD9C007BC6A5007BC6A5006BBD9C00D6E7E700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7FFFF00007B52006BFF9C0000DE940000DE940021FF6B00009C3100F7FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7FFFF00009C31004AFF4A0021FF6B0000BD7B0000BD3900009C3100FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000080852001818630018186B00181873001818730018186B0018186B000000
      52000000000000000000000000000000000000000000A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500ADAD
      AD00ADADAD00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      52002929B5002929FF002929FF002929FF002929FF002929FF002929FF002929
      AD000000520000000000000000000000000000000000A5A5A500FFFFFF00F7F7
      F700F7F7F700F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000080808000808
      0800101010001010100018181800181818001818180018181800181818001010
      1000101010001010100008080800000000000000000000000000000052002121
      AD001818F7001818EF002121F7002121FF002121FF002121FF001010F7001818
      F7002121A50000005200000000000000000000000000A5A5A50084FFFF001094
      10001094100084FFFF0000BD210008CE080084FFFF00109410001094100084FF
      FF0084FFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63000039390000393900003939000039390000393900005A5A007B7B00007B7B
      00007B7B00007B7B00007B7B0000000000000000000008080800080000001010
      0800A5A5A500B5ADAD00ADADAD00A5A5A500A5A5A500A5A5A500ADADAD003931
      3100080000001008080008080000000000000000000000005200181894000808
      CE003131D6007373E7001010DE001010E7001010E7001818E7007373E7002929
      DE000808D60018189400000052000000000000000000A5A5A50084FFFF001094
      100010941000C6D6CE0008AD100000B50000BDDEC6000073080000730800C6D6
      CE0084FFFF00ADADAD0000000000000000007B5A6300009C0000009C0000009C
      0000003939006BCEFF00007B7B00007B7B00007B7B00005A5A00FFFF6B00F7F7
      0000F7F70000F7F700007B7B0000000000000000000021181800211810002921
      21009C9C9C0031312900211810008C8C8C009C9C9C0094949400A5A5A500524A
      4A00181010002118180021181800000000000000520010107B000808AD003131
      BD00B5B5CE00D6CEBD008484D6000000C6000808CE008C8CDE00D6D6BD00ADAD
      D6002929BD000808AD0018107B000000520000000000A5A5A50084FFFF000073
      08000073080084FFFF0010941000008C000084FFFF000073080000730800CEEF
      CE0084FFFF00ADADAD0000000000000000007B5A6300009C0000009C0000009C
      00000039390094B5FF00007B7B00007B7B00007B7B00005A5A00FFFF6B00F7F7
      0000F7F70000F7F700007B7B0000000000000000000021211800212118003129
      2900A5A5A50042393900292121009C9C9400A5A5A500A5A5A500ADADAD005252
      4A0021181800292118002921180000000000080852000000940000008C004242
      A500CECEC600C6C6C600D6D6CE007B7BCE008484CE00D6D6CE00C6C6C600C6C6
      C60039399C0000009400080894000808520000000000A5A5A50084FFFF000073
      080000730800CEEFCE00317B3900317B3900C6DECE000073080000730800C6C6
      C60084FFFF00ADADAD0000000000000000007B5A6300009C0000009C0000009C
      00000039390094B5FF0094B5FF006BCEFF006BCEFF00005A5A00FFFF6B00FFFF
      6B00FFFF6B00FFFF6B007B7B0000000000000000000029212100292121003931
      3100B5B5B5004242390031292100A5A5A500B5B5B500B5B5B500BDBDBD00635A
      5A002921210029212100292121000000000000004A0000006B00000073000000
      730042429400D6D6D600D6D6D600DEDEDE00DEDED600D6D6D600CECECE003939
      940000007B00000073000000730000004A0000000000A5A5A50084FFFF000073
      08000073080084FFFF00317B3900317B390084FFFF0010941000317B3900CEEF
      CE0084FFFF00A5A5A50000000000000000007B5A63007B5A63007B5A63007B5A
      63007B007B007B007B007B007B007B007B007B007B007B007B000008AD000008
      AD000008AD000008AD000008AD00000000000000000031292900312929003931
      31009C9C9C00CECECE00CEC6C600C6C6C600CECECE00CECECE00A5A5A5005A52
      52002921210029212100312921000000000000004A0000004200000052000000
      6B0000007B004A4AA500E7E7E700E7E7E700E7E7E700DEDEDE004242A5000000
      8C000000840000006B0000005A0000004A0000000000A5A5A50084FFFF001094
      1000317B3900CEEFCE000073080000730800C6D6CE0018BD21004ABD5200C6C6
      C60084FFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42007B007B00FF94FF00F700F700F700F700F700F7007B007B008CA5FF000842
      FF000839FF000039FF000008AD00000000000000000039312900312929003129
      2900312929004239310042393900423131004231310039313100393129003129
      29003129290039312900312929000000000000004A0018185A0018186B001010
      840010109C009494D600F7F7F700F7F7F700F7F7F700F7F7F7008C8CDE001010
      B5001818A50018189400181884000000520000000000A5A5A50084FFFF0018BD
      21004ABD520084FFFF00007308000073080084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42007B007B00FF94FF00F700F700F700F700F700F7007B007B008CADFF00104A
      FF000842FF000839FF000008AD00000000000000000039312900BDB5B500EFE7
      E700EFE7E700EFE7E700EFE7E700EFEFE700EFEFE700EFE7E700EFE7E700EFE7
      E700CECEC6004239390039312900000000000808520021218C00212194002121
      AD00A5A5DE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF009C9C
      E7002121BD002121B5002121AD000000520000000000A5A5A50084FFFF0029BD
      310021AD2900CEEFCE000073080000730800C6C6C600CECECE00CECECE00CECE
      CE0084FFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42007B007B00FF94FF00FF94FF00FF94FF00FF94FF007B007B008CADFF008CA5
      FF008CA5FF0084A5FF000008AD00000000000000000039312900E7DEDE00EFE7
      DE00E7DED600E7DED600E7DED600E7DED600E7DED600E7DED600E7DED600EFDE
      D600F7EFE700524A4200393131000000000008085A003131BD002121BD007373
      CE00FFFFFF00FFFFFF00EFEFF7005A5ACE006363CE00F7F7F700FFFFFF00FFFF
      FF006B6BD6003131CE003939CE000808520000000000A5A5A50084FFFF0084FF
      FF0084FFFF0084FFFF0010941000317B390084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63007B007B007B007B007B007B007B007B007B007B007B007B000008AD000008
      AD000008AD000008AD000008AD00000000000000000042313100E7DEDE00EFE7
      DE00E7DED600E7DED600E7DED600E7DED600E7DED600E7DED600E7DED600E7DE
      DE00EFEFEF005A4A4A004239310000000000080852008C8CD6004242E7003939
      D600A5A5D600EFEFEF006B6BD6004242F7004242F7007373CE00EFEFEF009C9C
      D6004A4AD6005252E7008C8CD6000808520000000000A5A5A50084FFFF00C6C6
      C600C6C6C600C6C6C60018BD21004ABD5200C6C6C600CECECE00CECECE00CECE
      CE0084FFFF00ADADAD0000000000000000007B5A6300F7000000F7000000F700
      00007B5A630000BDBD0000BDBD0000BDBD0000BDBD007B5A630021FFFF0021FF
      FF0021FFFF0021FFFF007B5A6300000000000000000042393900E7DEDE00F7EF
      E700EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7
      E700F7EFEF005A525200423939000000000000000000080852009C9CEF005A5A
      FF005252E7005A5ACE005A5AFF006363FF006B6BFF005A5AFF005A5ACE005A5A
      EF006B6BFF009C9CE700080852000000000000000000A5A5A50084FFFF0084FF
      FF0084FFFF0084FFFF0029BD310021AD290084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF00ADADAD0000000000000000007B5A6300F7000000F7000000F700
      00007B5A630000BDBD0000BDBD0000BDBD0000BDBD007B5A630021FFFF0021FF
      FF0021FFFF0021FFFF007B5A630000000000000000004A423900E7E7DE00F7F7
      EF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EF
      EF00F7F7F700635A52004A42390000000000000000000000000010105A00ADAD
      F7007373FF007373FF007B7BFF00847BFF00847BFF007B7BFF007B7BFF008484
      FF00ADADF70010105A00000000000000000000000000A5A5A50084FFFF00CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE0084FFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A
      63007B5A63007B5A63007B5A630000000000080000004A424200E7E7E700FFFF
      FF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700FFF7F7006B5A5A004A424200000000000000000000000000000000001818
      6300BDBDF700C6C6FF00CEC6FF00CEC6FF00CEC6FF00C6C6FF00C6C6FF00BDBD
      F70010105A0000000000000000000000000000000000A5A5A50084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000008000000524A4200E7E7E700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFF7F7006B635A0052424200000000000000000000000000000000000000
      00001818630029297B0029297B0031297B0031297B0029297B0029297B001818
      63000000000000000000000000000000000000000000A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500ADAD
      AD00ADADAD00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000080808005A524A00948C8C00ADAD
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5
      A500A59C9C00736B630029292100000000000000000000000000000000000000
      0000313131003939390039393900393939003939390039393900393939003939
      3900313131000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005252520000000000000000000000000000000000525252000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002929
      2900E7E7E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E7E7E7001818180000000000000000009CBDD6000052B5000052B500005A
      BD000863BD00106BBD001873BD002173BD002173BD002173BD00186BBD000863
      B5000063C600005AC600086BCE009CBDD6000000000000000000000000008484
      840084848400E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700A5A5A5005252
      5200525252000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000080808003131
      3100CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C60029292900080808004A4A4A009CBDD6000073F7000084FF000094
      FF0021ADFF0042BDFF005ACEFF006BCEFF006BCEFF005ACEFF0042BDFF0021AD
      FF000894FF000094FF000084FF009CBDD6000000000000000000B5B5B500E7E7
      E700FFFFFF00E7E7E700CE9C9C00CE9C9C00CECECE00F7F7F700F7F7F700E7E7
      E7004A4A4A005252520000000000000000000000000000000000C6BDC6009494
      94007B737B005A525A00312931001010100018101800313131005A5A5A008C8C
      8C00B5B5B500C6BDC60000000000000000000000000063636300B5B5B5009C9C
      9C0094949400F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F7008C8C8C00A5A5A500B5B5B50073737300000000009CBDD6000084FF00007B
      FF000894FF0021A5FF0029A5FF009CD6FF008CD6FF0029ADFF0021A5FF001094
      FF00088CFF000894FF009CBDD6000000000000000000B5B5B500EFEFEF00F7FF
      FF00CE9C9C00CE310000CE633100CE9C9C00CE633100CE633100CE9C9C00FFFF
      FF00E7E7E7003939390052525200000000004239420039313900292129002118
      1800101010000800080000000000080008002118210018081000100808001008
      0800100810002921210039393900524A52000000000063636300BDBDBD009494
      94004A4A4A0094949C0094949C0094949C0094949C0094949C0094949C009494
      9C004A4A4A0094949400BDBDBD0063636300000000009CBDD600299CEF00088C
      FF000084FF000894FF001094FF00EFFFFF00E7F7FF001094FF000894FF00008C
      FF000894FF000884EF00000000000000000000000000E7E7E700FFFFFF00CE63
      6300CE310000CE630000CE9C9C00FFFFFF00CE9C6300CE310000CE310000CE9C
      9C00FFFFFF00DEDEDE0000000000000000002931290029292900393139003931
      3100211818001010100008000800100810004A424A00524A5200423939003129
      3100292121001810100010080800000000000000000063636300C6C6C6007B7B
      7B00101010001818180018181800181818001818180018181800181818001818
      18001010100084848C00C6C6C6006363630000000000000000009CBDD60039AD
      FF001894FF000084FF000073FF001094FF00088CFF00007BFF000084FF00008C
      FF00089CFF009CBDD6000000000000000000B5B5B500FFFFFF00CE9C9C00CE31
      0000CE633100CE633100CE633100CE9C6300CE633100CE633100CE633100CE31
      0000CECECE00EFEFEF007373730052525200526B5A00424A42004A4A4A00524A
      4A00312929001010100008080800080008009C949C00D6D6D600ADADB5009494
      94007B737B006B5A630039313900080008000000000063636300D6D6D6009494
      9400212121001818180018181800181818001818180018181800181818001818
      180021212100A5A5A500D6D6D6006363630000000000000000009CBDD600399C
      E70031A5F700219CF7000084FF00EFFFFF00EFF7FF000063FF000084FF000094
      FF001084EF00000000000000000000000000DEDEDE00FFFFFF00CE633100CE63
      3100CE633100CE633100CE9C6300EFEFEF00CE633100CE633100CE633100CE63
      3100CE636300FFFFFF00CECECE0000000000637B6B00525A5200524A52005A52
      520021182100080808000800080008000800A5A5A500E7E7DE00C6C6BD00BDB5
      B500CED6D600EFEFEF008C848C00000000000000000063636300E7E7E700E7E7
      E700DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDE
      DE00DEDEDE00E7E7E700E7E7E700636363000000000000000000000000009CBD
      D60042A5F7002994EF00188CEF00F7FFFF00E7F7FF000052EF00007BFF000894
      FF009CBDD600000000000000000000000000E7E7E700DEDEDE00CE633100CE63
      3100CE633100CE633100CE9C6300FFFFFF00FF9C9C00CE310000CE633100CE63
      3100CE633100F7F7F700DEDEDE00000000006B8C73005A635A0042313900524A
      4A006B636B00292129001818180000000800ADA5A500EFE7C600CEBD9C00BDA5
      8C00BDAD9400DECEBD009C949C00080008000000000063636300DEDEDE00CECE
      C600AD8C4A00AD844200AD844200AD844200AD844200AD844200AD844200AD84
      4200AD8C4A00CECECE00DEDEDE00636363000000000000000000000000000000
      00004294DE00319CEF00218CE700EFF7FF00E7F7FF000073F700008CFF00087B
      E70000000000000000000000000000000000E7E7E700FFCECE00CE633100CE63
      3100CE633100CE633100CE633100CECE9C00FFFFFF00CE9C6300CE633100CE63
      3100CE633100F7F7F700DEDEDE0000000000738C7B006B736B004A424A00635A
      63005A525A00523131003910180008000000A59C9C00DED6BD00BDA58C00AD94
      730094735A009C7B6B0073737300080808000000000063636300EFEFEF00E7E7
      DE00AD845200AD7B5200AD7B5200AD7B5200AD7B5200AD7B5200AD7B5200AD7B
      5200AD845A00E7E7E700EFEFEF00636363000000000000000000000000000000
      00009CBDD6004AADEF003194E700EFF7FF00E7F7FF001094FF0029ADFF009CBD
      D60000000000000000000000000000000000DEDEDE00EFEFEF00CE633100CE63
      3100CE633100CE633100CE630000CE310000CECECE00FFFFFF00CE633100CE63
      3100CE633100FFFFFF00DEDEDE002929290073947B007B847B004A2929005A31
      3100523939004263730021849C00081829009C948C00D6C6AD00B5947B009C7B
      6B00847373006B5A5200524A4A0029212900000000006363630094949400948C
      8C005A2110006B210800631808006318080063180800632108006B2110006B21
      1000632918009494940094949400636363000000000000000000000000000000
      0000000000004294D60042A5E70094CEF70084CEF70029ADFF00218CD6000000
      000000000000000000000000000000000000DEDEDE00FFFFFF00FF9C6300CE63
      3100CE9C6300EFEFEF00CE636300CE310000CE9C6300FFFFFF00CE9C6300CE63
      0000CE9C6300FFFFFF00C6C6C600525252007B9484008C948400397B8C0029A5
      D60031B5E70000EFF70000CEEF00082939009C8C8C00CEB5A500AD8C7300A584
      7B00B5BDBD007B7B7B005A4A52004A3942000000000000000000000000000000
      000052525200EFEFEF00EFE7E700EFE7E700EFE7E700EFE7E700EFE7E700EFEF
      EF00312929000000000000000000000000000000000000000000000000000000
      0000000000009CBDD60052ADEF00319CEF0029A5FF0042B5FF009CBDD6000000
      00000000000000000000000000000000000000000000F7F7F700FFEFCE00FF9C
      3100CE9C6300F7FFFF00FFFFFF00F7CEA500FFFFFF00F7FFFF00CE633100FF63
      3100F7F7F700EFEFEF0052525200000000007B9C8400A5B5AD0000BDCE0010A5
      CE003173A5006B847B009C7B6300312118009C949400CEAD9400AD847300A573
      6B00946B6300846B6B00635A6300393131000000000000000000000000000000
      00005A5A5A00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700EFEFEF00EFEF
      EF00313131000000000000000000000000000000000000000000000000000000
      000000000000000000004294D6004AB5FF004ABDFF002984CE00000000000000
      00000000000000000000000000000000000000000000DEDEDE00FFFFFF00FFEF
      CE00FF9C6300FF9C9C00E7E7E700F7F7F700E7E7E700FF9C6300FF9C6300FFEF
      CE00FFFFFF00DEDEDE000000000000000000637B6B0094AD9C008C5A4A008C73
      5A00A5A58400FFA56300E79C6B00312929008C8C8C00C6ADA500C6A59400B584
      7B009C5A4A009C6B630073737B00312929000000000000000000000000000000
      00005A5A5A00FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700313131000000000000000000000000000000000000000000000000000000
      000000000000000000009CBDD6005ABDFF0052BDFF009CBDD600000000000000
      0000000000000000000000000000000000000000000000000000DEDEDE00FFFF
      FF00FFFFFF00FFEFCE00FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFFFFF00FFFF
      FF00DEDEDE000000000000000000000000005A6B6300849C8C00D6845200A58C
      6B006B847B00735A52004A424200181018005A525A007B848400A5A5A500C6BD
      C600CEBDBD00D6C6C6006B6B7300312929000000000000000000000000000000
      00004A4A4A00949494008C8C8C008C8C8C008C8C8C008C8C8C008C8C8C008C8C
      8C004A4A4A000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004294DE004294DE0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEDE
      DE00EFEFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7E700DEDE
      DE00000000000000000000000000000000004A4A4A00393939004A424A004242
      4200423942004A424A004A4A4A002921210029212900524A52005A5A5A006363
      6300737B7B0094949400635A6300312931000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BDE7F700BDE7F70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFCECE00000000000000
      0000000000000000000000000000000000004A4A4A004A424200737B7B00A5A5
      A500C6BDC600000000000000000000000000000000000000000000000000C6BD
      C600A5A5A500737B7B00524A52004239420000000000A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500ADAD
      AD00ADADAD00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008CD694008CD6
      94008CD694000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A5A500FFFFFF00F7F7
      F700F7F7F700F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484FF001010E7000000
      E7000808E7000000D6000000D60000318C0010843100007B180000731800398C
      520000731800008418000084180000000000B5A59400BDB5AD00BDB5AD00BDB5
      AD00BDB5A500BDADA500BDADA500B5ADA500BDADA500BDB5A500BDB5AD00BDB5
      AD00BDB5AD00BDB5AD00B5AD9C000000000000000000A5A5A500DED6D600FF84
      5200FF634A00DED6C60000BD210008CE0800B5D6BD004A31FF004242FF00ADAD
      E700FFFFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63000008AD000008AD000008AD000008AD000008AD000008AD000008AD000008
      AD000008AD000008AD000008AD0000000000000000000808CE000000CE000000
      C6008484DE000000C6000000CE0000218400007B1800008C18000094210063BD
      730000942100008C21000084180000000000C6BDAD0010390000103900001039
      0000103900001039000010390000103900001039000010390000103900001039
      00001039000010390000CEC6AD000000000000000000A5A5A500EFEFEF00E763
      3100E7523900E7D6CE0008AD100000B50000BDDEC6003121FF002929FF00ADB5
      DE00FFFFFF00ADADAD0000000000000000007B5A630000DE420000DE420000DE
      42000008AD008CA5FF00104AFF000842FF000839FF000008AD0084A5FF00215A
      FF00215AFF00215AFF000008AD0000000000000000001818B5000000BD000000
      E7008484FF000000EF000000CE0018297B0031943900009C180000B5210063E7
      840000BD290000A529000094210000000000C6BDA500103900000000F7000000
      F7001039000000F7000000F70000103900000000F7000000F7001039000000F7
      000000F7000010390000CEC6AD000000000000000000A5A5A500F7F7F700C642
      1800C6422900E7DED60010941000008C0000C6E7CE002118DE001818CE00B5B5
      E700FFFFFF00ADADAD0000000000000000007B5A630000DE420000DE420000DE
      42000008AD008CADFF001852FF00104AFF000842FF000008AD0084A5FF00215A
      FF00215AFF00215AFF000008AD0000000000000000002929C6002929C6000000
      DE008484FF001008D6001000B500213173004AC642004AC6420000C618005AEF
      7B0000C6210000B52900009C210000000000C6BDA500103900000000F7000000
      F7001039000000F7000000F70000103900000000F7000000F7001039000000F7
      000000F7000010390000CEC6AD000000000000000000A5A5A500F7F7F7009C29
      0800AD312100E7DECE00106B1000086B0800C6DECE001818CE0021216B00ADAD
      D600FFFFFF00ADADAD0000000000000000007B5A630000DE420000DE420000DE
      42000008AD0094ADFF008CADFF008CADFF008CA5FF000008AD0084A5FF0084A5
      FF0084A5FF0084A5FF000008AD0000000000000000002929BD003939D6001810
      FF007B7BFF000000FF000000FF000029A50021D64A004AEF5A0021EF4A0042EF
      630000D6210000C6210000AD210000000000C6BDA50010390000103900001039
      0000103900001039000010390000103900001039000010390000103900001039
      00001039000010390000D6C6AD000000000000000000A5A5A500F7F7F7008421
      080063292100DEDED60010521000104A1000C6D6CE003129EF0039316300ADB5
      CE00FFFFFF00A5A5A50000000000000000007B5A63007B5A63007B5A63007B5A
      63000008AD000008AD000008AD000008AD000008AD000008AD000008AD000008
      AD000008AD000008AD000008AD0000000000000000002929CE003131E7003131
      F7006373FF001039FF000000FF00635284008CB5310063BD4A006BBD73008CD6
      940052DE6B0031D6520010C6390000000000C6BDA5001039000000F7000000F7
      00001039000000F7000000F700001039000000F7000000F700001039000000F7
      000000F7000010390000CEC6AD000000000000000000A5A5A500FFFFFF00B539
      18006B313100DEDED6001039100018311000C6D6CE004A4AFF003942D600ADAD
      E700FFFFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42000008AD0094B5FF002163FF00215AFF001852FF000008AD008CA5FF000842
      FF000839FF000039FF000008AD0000000000000000006373FF005252E7005A63
      EF008473AD00C68C4A00EF9C2100FF941800FFAD7B00FF7B1800C68C39009C94
      420042CE6300000000000000000000000000C6B5A5001039000000F7000000F7
      00001039000000F7000000F700001039000000F7000000F700001039000000F7
      000000F7000010390000CEBDAD000000000000000000A5A5A500F7F7F700DE6B
      4200CE635200DED6CE001031100021212100CEEFCE00F7F7F700FFFFFF00FFFF
      FF00FFFFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42000008AD0094B5FF002963FF002163FF00215AFF000008AD008CADFF00104A
      FF000842FF000839FF000008AD00000000000000000000000000000000000000
      0000B5846B00E79C4200F7A52900FFBD3900FFDE8C00FFAD3900DE943100C673
      210000000000000000000000000000000000C6B5A50010390000103900001039
      0000103900001039000010390000103900001039000010390000103900001039
      00001039000010390000CEBDAD000000000000000000A5A5A500EFEFEF00FF7B
      5200FF634A00E7DED6000852180029422900C6C6C600CECECE00CECECE00CECE
      CE00FFFFFF00A5A5A50000000000000000007B5A630000DE420000DE420000DE
      42000008AD0094B5FF0094B5FF0094B5FF0094B5FF000008AD008CADFF008CA5
      FF008CA5FF0084A5FF000008AD00000000000000000000000000000000000000
      0000E79C3900E7AD6300F7A53900FFB53100FFDE9400FFAD3900DE943100C67B
      290000000000000000000000000000000000C6B5A5001039000000F7000000F7
      00001039000000F7000000F700001039000000F7000000F700001039000000F7
      000000F7000010390000CEBDAD000000000000000000A5A5A500F7F7F700F7F7
      F700F7F7F700F7F7F70000730800317B3900CEEFCE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63000008AD000008AD000008AD000008AD000008AD000008AD000008AD000008
      AD000008AD000008AD000008AD00000000000000000000000000000000000000
      0000DE9C4200EFB56300FFBD6300FFB54200FFD68C00FFAD3100E79C3100CE84
      290000000000000000000000000000000000C6BDA5001039000000F7000000F7
      00001039000000F7000000F700001039000000F7000000F700001039000000F7
      000000F7000010390000CEBDAD000000000000000000A5A5A500E7E7E700C6C6
      C600C6C6C600C6C6C60018BD21004ABD5200C6C6C600CECECE00CECECE00CECE
      CE00FFFFFF00ADADAD0000000000000000007B5A630000DE420000DE420000DE
      42007B5A630000DE420000DE420000DE420000DE42007B5A630000DE420000DE
      420000DE420000DE42007B5A6300000000000000000000000000000000000000
      0000E7A54A00F7BD6300FFC66B00FFC67B00FFC67B00FFB55200F7A53900DE8C
      290000000000000000000000000000000000C6BDA50010390000103900001039
      0000103900001039000010390000103900001039000010390000103900001039
      00001039000010390000CEBDAD000000000000000000A5A5A500EFEFEF00F7F7
      F700F7F7F700FFFFFF0029BD310021AD2900CEEFCE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ADADAD0000000000000000007B5A630000DE420000DE420000DE
      42007B5A630000DE420000DE420000DE420000DE42007B5A630000DE420000DE
      420000DE420000DE42007B5A6300000000000000000000000000000000000000
      000000000000FFC67300FFCE8400FFCE8C00FFCE9400FFCE8C00FFC67300F7AD
      520000000000000000000000000000000000BDB59C00C6BDAD00C6BDAD00C6BD
      AD00C6BDAD00C6BDAD00C6B5AD00C6B5AD00C6B5AD00C6B5AD00C6B5AD00C6BD
      AD00C6BDAD00C6BDAD00C6B59C000000000000000000A5A5A500DEDEDE00CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE00FFFFFF00ADADAD0000000000000000007B5A63007B5A63007B5A63007B5A
      63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A63007B5A
      63007B5A63007B5A63007B5A6300000000000000000000000000000000000000
      0000000000000000000000000000FFC67300FFCE7B0000000000000000000000
      000000000000000000000000000000000000BDB59C00E7DECE00E7DECE00E7D6
      C600E7D6C600DED6C600E7D6C600E7D6C600E7D6CE00DED6C600E7D6C600E7D6
      C600DED6C600E7D6C600C6B5A5000000000000000000A5A5A500EFEFEF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDB59C00F7E7DE00F7E7DE00EFE7
      D600EFE7D600EFE7D600EFDECE00EFDECE00EFDECE00EFDECE00EFDECE00EFE7
      D600EFE7D600EFE7DE00C6B5A5000000000000000000A5A5A500A5A5A500A5A5
      A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500A5A5A500ADAD
      AD00ADADAD00ADADAD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDB5A500C6BDAD00C6BDAD00C6BD
      AD00C6BDAD00C6BDAD00C6B5AD00C6B5AD00C6B5AD00C6B5AD00C6B5AD00C6BD
      AD00C6BDAD00C6BDAD00C6BDAD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004A4A
      4A00525252005252520052525200525252005252520052525200525252005252
      5200525252000000000000000000000000000000000000000000000000004A4A
      AD006B6BFF000000000000000000000000000000000000000000000000003139
      F7003942B50000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BDBD
      BD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CECECE0000000000000000000000000000000000000000000000BD002929
      FF002929A5000000EF00000000000000000000000000000000000000E7001010
      A5001810FF000000CE000000000000000000FFFFFF008494DE000831B5001031
      BD001839BD001839B5001839BD001039BD001031BD000831C6000831C6000029
      BD000029C6000021B5007B94D600FFFFFF000000000000000000000000000000
      0000000000006363630063636300636363006363630063636300636363000000
      000000000000000000000000000000000000000000000000000000000000BDBD
      BD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C600000000000000000000000000000000000810C6000000D6000000
      DE000000FF001010A5000000E70000000000000000000000E7001810A5000000
      FF000000DE000808DE001818CE0000000000FFFFFF001039D6001842DE00294A
      DE002952E7002952E7002952E7002952E700214ADE00214AE7001042E7001042
      E7000039E7000031D6000021B500FFFFFF000000000000000000000000006363
      63006363630063636300525A8C00104AFF00BD847300104AFF00636363006363
      63000000000000000000000000000000000000000000B5B5B500ADADAD00BDBD
      BD00D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D6009C9C9C00ADADAD00B5B5B5000000000000000000948CFF000000CE000000
      BD000000D6000000F7001010A5000808BD000810B5000810A5000000F7000000
      D6000000BD000000CE005252EF0000000000FFFFFF001842DE002952E700315A
      E7004263E7007B94EF00E7EFFF00FFFFFF00FFFFFF00DEE7FF007394F700104A
      EF00084AEF000039E7000029BD00FFFFFF000000000000000000638CC600104A
      FF00104AFF00104AFF003184FF00EFBD9C00BD847300C6B5B5003184FF00104A
      FF006363630063636300000000000000000039393900636363006B6B6B005A5A
      5A00636363006363630063636300636363006363630063636300636363006363
      63005A5A5A00636363006B6B6B003939390000000000000000000808BD000000
      CE000000B5000000C6000000DE003131CE003939CE000000DE000000C6000000
      B5000000D6000810C6000000000000000000FFFFFF00214ADE00395AE7004263
      E700A5B5F700FFFFFF00BDCEF700738CEF006B8CEF00BDCEFF00FFFFFF00A5B5
      F700084AEF000839E7000029C600FFFFFF0000000000000000003184FF00DEBD
      AD00FFDEAD00FFE7BD00FFEFCE00FFEFD600DE9C7300FFE7BD00FFF7EF007BA5
      FF006373940063636300636363000000000039393900BDBDBD00B5B5B5005A5A
      5A004A4A4A005252520052525200525252005252520052525200525252005252
      5200524A5200B5ADAD00CECEC600393939000000000000000000000000001810
      CE000000C60000009C000000AD000000C6000000C6000000AD000000A5000000
      C6001010C600000000000000000000000000FFFFFF002952E7004263E7008C9C
      EF00FFFFFF00849CEF00426BE7004263E7003163EF00295AEF007394F700FFFF
      FF006B8CF7001042E7000831C600FFFFFF00000000005A8CCE004284F700FFDE
      BD00FFDEBD00FFEFCE00FFF7E700EFC6AD00FFCE9400FFE7C600FFF7EF00FFFF
      FF007BA5FF007BB5FF006363630063636300A5A5A500D6D6D600CECECE007B7B
      7B0063636B00636363005A5A63005A5A5A00525252004A4A52004A4A4A004242
      420039393900CECECE00E7E7DE00A5A5A5000000000000000000000000000000
      00001818BD001010BD0000008C0000008C0000009400000084000808B5001818
      BD0000000000000000000000000000000000FFFFFF00315AE7004A6BE700EFEF
      FF00BDC6F7004A6BE7004A6BE700FFFFFF00FFFFFF00295AEF002152EF00B5C6
      FF00DEE7FF00214AE7001039C600FFFFFF0000000000429CFF00429CFF00FFEF
      D600FFEFDE00FFFFF700FFFFFF00DE9C7300FFDEAD00FFEFD600FFFFF700FFFF
      FF00FFFFFF004A9CFF005A639C0063636300A5A5A500DEDEDE00D6D6D600A5A5
      AD00949C9C00949494008C8C8C00848484007B7B7B007B737B006B6B73006363
      63005A5A5A00D6D6CE00E7E7E700A5A5A5000000000000000000000000000000
      000029217B001818C60000009C000000940000009400000094001010C6001818
      730000000000000000000000000000000000FFFFFF00395AE700526BE700FFFF
      FF008494EF00526BE7004A6BE700FFFFFF00FFFFFF00295AE7002152E7006384
      EF00FFFFFF00214ADE001839C600FFFFFF00000000003994FF00FFE7CE00FFF7
      E700FFFFF700FFFFFF00F7DECE00F7CE9C00FFEFCE00FFFFF700FFFFFF00FFFF
      FF00EFF7FF002994FF008494AD0063636300A5A5A500D6D6D600DEDEDE00BDBD
      BD00C6C6C600CECECE00C6C6C600BDBDBD00BDBDBD00C6BDBD00CEC6C600C6BD
      C600ADADAD00DEDEDE00DEDEDE00A5A5A5000000000000000000000000000000
      10000808C6000000B50000009C0000009C0000009C000000A5000000B5000000
      B50000001000000000000000000000000000FFFFFF004263E7005A73EF00FFFF
      FF008494EF00526BE7004A63E700FFFFFF00FFFFFF002952E7002152E7006384
      EF00FFFFFF00214ADE001839BD00FFFFFF00639CD600429CFF00FFFFF700FFFF
      F700FFFFF700FFFFF700DE9C7300FFDEAD00FFF7E700FFFFFF00FFFFFF00FFFF
      FF008CC6FF005AADFF006363630000000000000000009C9C9C00E7E7E700BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00B5B5B500E7E7E700A5A5A500000000000000000000000000000000001010
      DE000000FF000000C6000000C6002921F7001818EF000000C6000000C6000000
      F7000000DE00000000000000000000000000FFFFFF004A6BE700637BEF00EFF7
      FF00B5C6F700526BE7004A63E700FFFFFF00FFFFFF002952E700214AE700B5BD
      F700E7E7FF002952E7002142BD00FFFFFF00429CFF00E7CEB500FFF7E700FFF7
      EF00FFFFFF00F7EFE700EFCEA500FFEFCE00FFF7EF00FFFFF700FFFFFF00FFFF
      FF00399CFF008C84BD0063636300000000000000000000000000636363007B7B
      7B00FFFFFF00FFF7F700FFF7F700FFF7F700FFF7F700F7F7F700F7F7F700FFFF
      FF00ADADAD005A5A5A00000000000000000000000000181000003131D6000808
      FF000000EF001008EF002121FF00736BEF005A52E7002118FF001010EF000800
      EF000808FF001818E7000800000000000000FFFFFF00526BE7006B84EF0094AD
      F700FFFFFF008494EF004A63E7004263E700395AE7003152E7006B84EF00FFFF
      FF00738CEF002952E7002142BD00FFFFFF00429CFF00FFEFDE00FFFFFF00FFFF
      FF00CEDEFF009CBDFF00B5BDDE00FFEFDE00FFFFFF00FFFFFF00FFFFFF00A5D6
      FF0052ADFF006363630000000000000000000000000000000000000000007B7B
      7B00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700FFFF
      FF00ADADAD0000000000000000000000000000000000C6ADFF009C8CFF004A42
      FF005A4AFF009484FF002929D60000000000000000003131D6008C7BFF005A4A
      FF005A4AFF008473FF006B5AD60000000000FFFFFF00637BEF007B94EF00738C
      EF00A5B5F700FFFFFF00B5BDF7007384EF006B84EF00B5BDF700FFFFFF0094AD
      F7003152E7002952E7002142BD00FFFFFF003194FF007BC6FF00B5BDDE00B5BD
      DE00000000000000000000000000C6DEFF00C6DEFF00FFFFFF00FFFFFF0052B5
      FF00637394006363630000000000000000000000000000000000000000004242
      4200FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700FFFF
      FF00A5A5A500000000000000000000000000000000000000000031319C00DECE
      FF00DED6FF003942DE00000000000000000000000000000000004A42DE00E7D6
      FF00DECEFF004A429C000000000000000000FFFFFF006B84EF008CA5EF008494
      EF006B8CEF0094ADF700EFF7FF00FFFFFF00FFFFFF00EFEFFF008C9CEF004263
      E700395AE7003152E7001839BD00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000B5BDDE00ADD6FF007BC6FF0063C6
      FF00636363000000000000000000000000000000000000000000000000004242
      4200FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700FFFF
      FF00A5A5A500000000000000000000000000000000000000000000000000EFE7
      FF00BDBDFF00000000000000000000000000000000000000000000000000A5A5
      FF00BDB5FF00000000000000000000000000FFFFFF00738CEF009CADF7008CA5
      EF007B94EF00738CEF006B84EF00637BEF00637BEF00637BEF005273E7004A6B
      E7004263E7002952E7001839BD00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007BC6FF007BB5
      DE00000000000000000000000000000000000000000000000000000000004242
      4200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00ADADAD000000000000000000000000000000000000000000000000009C8C
      FF00EFE7FF00000000000000000000000000000000000000000000000000EFE7
      FF009C8CFF00000000000000000000000000FFFFFF00B5BDF700738CEF006384
      EF005A73EF005273E7004A6BE7004A6BE7004A6BE7004263E7004263E700395A
      E7003152E7002142DE008C9CDE00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005A5A
      5A005A5A5A005A5A5A005A5A5A005A5A5A005A5A5A005A5A5A005A5A5A006363
      63005A5A5A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF00C001FFFF00000000D7FDFE7F00000000
      C001807F00000000D001000100000000C001000100000000D009000100000000
      C001000100000000D061000100000000C001000100000000D281000100000000
      C001000100000000DE01000100000000C011000100000000D807000100000000
      D80FF07F00000000C01FFFFF00000000FFFFFFFFFFFFF00FC003FFFFF03FF00F
      C003FC0FF03FF80FC003F807F03F400AC003E007F03F0000C003E00300030000
      C003E00300030000C003E18300030000C003C00300030000C003C00700030000
      C003E00700030000C003F00FF03FF00FC007F01FF03FF00FC00FFD7FF03FF81F
      C01FFFFFF03FF00FFFFFFFFFFFFFF00FF00F8003FFFFFFFFE0078003FFFF8001
      C003800300010001800180030001000100008003000100010000800300010001
      0000800300010001000080030001000100008003000100010000800300010001
      000080030001000100008003000100018001800300010001C003800300010001
      E0078003FFFF0001F00F8003FFFF0001F007FFFFF81FFFFFE0030000E007FFFF
      80000000C003C003800080018001000080008003800100008000C00300000000
      8000C007000000008000E007000000008000F00F000000008000F00F00000000
      8000F81F00000000F007F81F80010000F007FC3F80030000F007FC3FC0070000
      F007FE7FE00F0000FFFFFE7FF83F07E08003FFFFFFC7FFFF8003FFFF80010001
      8003000180010001800300018001000180030001800100018003000180010001
      8003000180010001800300018007000180030001F00F000180030001F00F0001
      80030001F00F000180030001F00F000180030001F80F000180030001FE7F0001
      8003FFFFFFFF00018003FFFFFFFF0001FFFFE007E7E78001FFFFE007C3C30000
      F81FE00781810000E00F800180010000C0030000C0030000C0010000E0070000
      80000000F00F000080000000F00F000080000000E007000000018001C0030000
      0001C003800100000003E007818100000E03E007C3C30000FF07E007E7E70000
      FFCFE007E7E70000FFFFE007FFFF800100000000000000000000000000000000
      000000000000}
  end
  object XPManifest1: TXPManifest
    Left = 296
    Top = 64
  end
end

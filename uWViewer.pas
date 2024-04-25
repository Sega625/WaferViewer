unit uWViewer;

interface

uses
  Windows, Forms, Dialogs, Messages, Graphics, SysUtils, Controls, StdCtrls,
  Buttons, Classes, Statistica, ExtCtrls, ComCtrls, Menus, ToolWin, ViewFails,
  ViewData, ViewOK, ViewPD, ViewPref, ViewFull, Structs, About, Help, JoyBtn, XPMan, Registry,
  ShellAPI, ImgList, Gauge, ActiveX, ComObj, Variants;

type
  TForm1 = class(TForm)
    MainMenu: TMainMenu;
    mmFile: TMenuItem;
    mOpen: TMenuItem;
    mPrint: TMenuItem;
    mExit: TMenuItem;
    mmAnalize: TMenuItem;
    mPD: TMenuItem;
    mFails: TMenuItem;
    mmAbout: TMenuItem;
    mHelp: TMenuItem;
    mAbout: TMenuItem;
    AdjGroup: TGroupBox;
    TransTBar: TTrackBar;
    MainImList: TImageList;
    WafInfoGroup: TGroupBox;
    CodeLab0: TLabel;
    CodeLab: TLabel;
    NLotLab: TLabel;
    NLotLab0: TLabel;
    NWafLab0: TLabel;
    NWafLab: TLabel;
    DeviceLab: TLabel;
    DeviceLab0: TLabel;
    InfoLab0: TLabel;
    InfoLab: TLabel;
    WPlaceLab: TLabel;
    WPlaceLab0: TLabel;
    OperatorLab0: TLabel;
    OperatorLab: TLabel;
    DateLab: TLabel;
    DateLab0: TLabel;
    NMeasLab0: TLabel;
    NMeasLab: TLabel;
    NOKLab: TLabel;
    NOKLab0: TLabel;
    OKCol: TLabel;
    NCCol: TLabel;
    SCCol: TLabel;
    FCCol: TLabel;
    NFailNCLab0: TLabel;
    NFailNCLab: TLabel;
    NFailSCLab: TLabel;
    NFailSCLab0: TLabel;
    NFailFCLab0: TLabel;
    NFailFCLab: TLabel;
    PathLab0: TLabel;
    DirBox: TPaintBox;
    CutLab: TLabel;
    CutLab0: TLabel;
    XPManifest1: TXPManifest;
    InformationLab0: TLabel;
    AdjustLab0: TLabel;
    NTotLab0: TLabel;
    NTotLab: TLabel;
    mOK: TMenuItem;
    mData: TMenuItem;
    mSave: TMenuItem;
    TransBtn: TSpeedButton;
    StaticText1: TStaticText;
    N2: TMenuItem;
    mPrMap: TMenuItem;
    N4: TMenuItem;
    mAdjust: TMenuItem;
    DiamLab0: TLabel;
    StepXLab0: TLabel;
    StepYLab0: TLabel;
    StepYLab: TLabel;
    StepXLab: TLabel;
    DiamLab: TLabel;
    mAdd: TMenuItem;
    mFull: TMenuItem;
    OKRLab0: TLabel;
    OKRLab: TLabel;
    MPWLab0: TLabel;
    MPWLab: TLabel;
    CondLab0: TLabel;
    CondLab: TLabel;
    MSystemLab0: TLabel;
    MSystemLab: TLabel;
    DscrDevLab0: TLabel;
    DscrDevLab: TLabel;
    ProberLab0: TLabel;
    ProberLab: TLabel;
    MPWPosLab0: TLabel;
    MPWPosLab: TLabel;
    mExcel: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure JoyDown(const TypeBtn: byte);
    procedure JoyUp  (const TypeBtn: byte);
    procedure DirBoxPaint(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure mOpenClick(Sender: TObject);
    procedure mSaveClick(Sender: TObject);
    procedure mPrintClick(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure mFailsClick(Sender: TObject);
    procedure mOKClick(Sender: TObject);
    procedure mPDClick(Sender: TObject);
    procedure mDataClick(Sender: TObject);
    procedure mHelpClick(Sender: TObject);
    procedure mAboutClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TransTBarChange(Sender: TObject);
    procedure TransBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TransBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure WMDropFiles (var Msg: TMessage); message WM_DROPFILES;
    procedure ColLabClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mPrMapClick(Sender: TObject);
    procedure mAdjustClick(Sender: TObject);
    procedure mAddClick(Sender: TObject);
    procedure mFullClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mExcelClick(Sender: TObject);
  private
    LabLang: byte; // 0 - русский, остальные - английский
    Count: WORD;
//    Statistica: TStatistica;
    DirBmp: TBitmap;
    StatPath: TFileName;
    MainCapt: String;
    JoyBtn: TJoyBtn;
    Gauge: TMGauge;
    TransBtnEnabled: Boolean;
    StatTransBMP: TBitmap;
    InfoTransBMP: TBitmap;

    HintName: THintWindow;
    HintRect: TRect;

    procedure CreateAndSaveMap(const fName: TFileName);

    procedure SetLabelsLang(const Lang: byte);
    procedure FillLabels;
    procedure ClearLabels;

    procedure PaintWafer(Max: WORD);
    procedure ProcessMessagesEx;
    function  LoadFile(const fName: TFileName): Boolean;
    function  AddFile (const fName: TFileName): Boolean;
    function  DetectFileType(const fName: TFileName): byte;

    procedure ViewOK_Repaint_Event(var Mes: TMessage); message MESS_REPAINT_VIEW_OK;
  public
    Statistica: TStatistica;

    procedure CloseAllDialogs;
  end;

var
  Form1: TForm1;

implementation

uses Types;

{$R *.dfm}

///////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormCreate(Sender: TObject);                                                        //
                                                                                                     //
/////////////////////////////////////////////////////////////////////////////                        //
  function GetVersion(var Major, Minor, Release, Build: byte): Boolean;    //                        //
  var                                                                      //                        //
    info: Pointer;                                                         //                        //
    infosize: DWORD;                                                       //                        //
    fileinfo: PVSFixedFileInfo;                                            //                        //
    fileinfosize: DWORD;                                                   //                        //
    tmp: DWORD;                                                            //                        //
  begin                                                                    //                        //
    Result := True;
    
    Major   := 1;
    Minor   := 31;
    Release := 0;
    Build   := 0;

//    Exit;

    infosize := GetFileVersionInfoSize(PChar(Application.ExeName), tmp);   //                        //
    Result := infosize <> 0;                                               //                        //
                                                                           //                        //
    if Result then                                                         //                        //
    begin                                                                  //                        //
      info := AllocMem(infosize);                                          //                        //
      try                                                                  //                        //
        GetFileVersionInfo(PChar(Application.ExeName), 0, infosize, info); //                        //
        VerQueryValue(info, nil, Pointer(fileinfo), fileinfosize);         //                        //
        Major   := fileinfo.dwProductVersionMS shr 16;                     //                        //
        Minor   := fileinfo.dwProductVersionMS and $FFFF;                  //                        //
        Release := fileinfo.dwProductVersionLS shr 16;                     //                        //
        Build   := fileinfo.dwProductVersionLS and $FFFF;                  //                        //
      finally                                                              //                        //
        FreeMem(info, fileinfosize);                                       //                        //
      end;                                                                 //                        //
    end;                                                                   //                        //
  end;                                                                     //                        //
/////////////////////////////////////////////////////////////////////////////                        //
                                                                                                     //
var                                                                                                  //
  Rct: TRect;                                                                                        //
  Ini: TRegIniFile;                                                                                  //
  lpVersionInformation: TOSVersionInfo;                                                              //
  SideBorder, UpBorder: byte;                                                                        //
  Rgn: HRGN;                                                                                         //
  Major, Minor, Release, Build: byte;                                                                //
  TmpColorChips, TmpColorBkgrn, TmpColorGrid: TColor;                                                //
  TmpViewOK_3D: byte;                                                                                //
  TmpViewAll: Boolean;                                                                               //
  TmpViewGrid: Boolean;                                                                              //
begin                                                                                                //
  DecimalSeparator := '.';                                                                           //
                                                                                                     //
  if GetVersion(Major, Minor, Release, Build) then                                                   //
    VersionStr := IntToStr(Major)+'.'+IntToStr(Minor)+'.'+IntToStr(Release)+'.'+IntToStr(Build)      //
  else                                                                                               //
    VersionStr := '0.0.0.0';                                                                         //
                                                                                                     //
  WafInfoGroup.Top := 0;                                                                             //
  AdjGroup.Top := WafInfoGroup.Top+WafInfoGroup.Height+5;                                            //
  AdjGroup.Width := WafInfoGroup.Width;                                                              //
                                                                                                     //
  SideBorder := (self.Width-self.ClientWidth) div 2;                                                 //
  UpBorder   := self.Height-self.ClientHeight-SideBorder;                                            //
                                                                                                     //
  with Screen.WorkAreaRect do                                                                        //
  begin                                                                                              //
    Constraints.MinWidth  := 700;                                                                    //
    Constraints.MaxWidth  := Right-Left;                                                             //
    Constraints.MinHeight := AdjGroup.Top+AdjGroup.Height+UpBorder+SideBorder+4;                     //
    Constraints.MaxHeight := Bottom-Top;                                                             //
  end;                                                                                               //
                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                                     //
  try                                                                                                //
    Ini := TRegIniFile.Create('');                                                                   //
    try                                                                                              //
      Ini.OpenKey('Software\WaferViewer', False);                                                    //
      if Ini.ReadBinaryData('FormPosition', Rct, SizeOf(TRect)) = 0 then Rct := Screen.WorkAreaRect; //
      BoundsRect := Rct;                                                                             //
      if Ini.ReadBool('', 'Maximized', False) then self.WindowState := wsMaximized;                  //
      StatPath := Ini.ReadString('', 'StatPath', 'D:\Statistica');                                   //
      TmpColorChips := Ini.ReadInteger('', 'ShowChipsColor', clFuchsia);                             //
      TmpColorBkgrn := Ini.ReadInteger('', 'BkgrnColor', clSkyBlue);                                 //
      TmpColorGrid  := Ini.ReadInteger('', 'GridColor', clYellow);                                   //
      TmpViewOK_3D  := Ini.ReadInteger('', 'ViewOK_3D', 2);                                          //
      TmpViewAll    := Ini.ReadBool('', 'ViewAll', True);                                            //
      TmpViewGrid   := Ini.ReadBool('', 'ViewGrid', False);                                          //
      Ini.CloseKey;                                                                                  //
                                                                                                     //
//      Ini.RootKey := HKEY_CLASSES_ROOT;                                                              //
//      Ini.WriteString('.sts', '', '!sts');                                                           //
//      Ini.WriteString('!sts\DefaultIcon', '', ParamStr(0)+', 0');                                    //
//      Ini.WriteString('!sts\shell\open\command', '', ParamStr(0)+' "%1"');                           //
    finally                                                                                          //
      Ini.Free;                                                                                      //
    end;                                                                                             //
  except                                                                                             //
  end;                                                                                               //
                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                                     //
  JoyBtn := TJoyBtn.Create(AdjGroup);                                                                //
  with JoyBtn do                                                                                     //
  begin                                                                                              //
    Top  := 32;                                                                                      //
    Left := 20;                                                                                      //
                                                                                                     //
    OnJoyDown := JoyDown;                                                                            //
    OnJoyUp   := JoyUp;                                                                              //
  end;                                                                                               //
                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                                     //
  Gauge := TMGauge.Create(self);                                                                     //
  with Gauge do                                                                                      //
  begin                                                                                              //
    Top  := 575;                                                                                     //
    Left := 11;                                                                                      //
                                                                                                     //
    OKColor   := $00C9DA78;                                                                          //
    FailColor := $008B88FB;                                                                          //
                                                                                                     //
    Width  := 212;                                                                                   //
    Height := 22;                                                                                    //
  end;                                                                                               //
                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////
  Statistica := TStatistica.Create(self);                                                            //
  with Statistica do                                                                                 //
  begin                                                                                              //
    Top  := 0;                                                                                       //
    Left := WafInfoGroup.Left+WafInfoGroup.Width+5;                                                  //
    Width  := self.ClientWidth-Left-5;                                                               //
    Height := self.ClientHeight-Top-5;                                                               //
                                                                                                     //
    clShowChips := TmpColorChips;                                                                    //
    Color       := TmpColorBkgrn;                                                                    //
    clGrid      := TmpColorGrid;                                                                     //
    clNotChip   := Color;                                                                            //
    ViewOK_3D   := TmpViewOK_3D;                                                                     //
    ViewAll     := TmpViewAll;                                                                       //
    ViewGrid    := TmpViewGrid;                                                                      //
                                                                                                     //
    OnWaferPainted := PaintWafer;                                                                    //
  end;                                                                                               //
                                                                                                     //
  DirBmp := TBitmap.Create;                                                                          //
  with DirBmp do                                                                                     //
  begin                                                                                              //
    Width  := 476;                                                                                   //
    Height := 28;                                                                                    //
    LoadFromResourceName(HInstance, 'DIR_BMP');                                                      //
  end;                                                                                               //
                                                                                                     //
  TransBtnEnabled := True;                                                                           //
                                                                                                     //
  lpVersionInformation.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);                                //
  GetVersionEx(lpVersionInformation);                                                                //
  if lpVersionInformation.dwMajorVersion < 5 then // Ниже Windows 2000                               //
  begin                                                                                              //
    TransTBar.Enabled := False;                                                                      //
    TransBtnEnabled   := False;                                                                      //
  end;                                                                                               //
  if lpVersionInformation.dwMajorVersion > 5 then deltaW7 := 5;                                      //
                                                                                                     //
  OKCol.ControlStyle := OKCol.ControlStyle+[csOpaque];                                               //
  NCCol.ControlStyle := NCCol.ControlStyle+[csOpaque];                                               //
  SCCol.ControlStyle := SCCol.ControlStyle+[csOpaque];                                               //
  FCCol.ControlStyle := FCCol.ControlStyle+[csOpaque];                                               //
                                                                                                     //
  Rgn := CreateRectRgn(2, 2, 20, 95);                                                                //
  SetWindowRgn(TransTBar.Handle, Rgn, True);                                                         //
                                                                                                     //
  TransparentColor := True;                                                                          //
  TransparentColorValue := $00000625;                                                                //
  DoubleBuffered := True;                                                                            //
                                                                                                     //
  WafInfoGroup.DoubleBuffered   := True;                                                             //
//  ChipsInfoGroup.DoubleBuffered := True;
//  ChipsInfoGroup.Visible := False;
  AdjGroup.DoubleBuffered  := True;                                                                  //
                                                                                                     //
  StatTransBMP := TBitmap.Create;                                                                    //
  with StatTransBMP do                                                                               //
  begin                                                                                              //
    Width  := Statistica.Width;                                                                      //
    Height := Statistica.Height;                                                                     //
    Canvas.Brush.Color := $00000625;                                                                 //
    Canvas.Pen.Color := $00000625;                                                                   //
    ControlStyle := ControlStyle+[csOpaque];                                                         //
    Canvas.Rectangle(0,0, Width,Height);                                                             //
  end;                                                                                               //
  InfoTransBMP := TBitmap.Create;                                                                    //
  with InfoTransBMP do                                                                               //
  begin                                                                                              //
    Width  := WafInfoGroup.Width;                                                                    //
    Height := WafInfoGroup.Height;                                                                   //
    Canvas.Brush.Color := $00000625;                                                                 //
    Canvas.Pen.Color := $00000625;                                                                   //
    ControlStyle := ControlStyle+[csOpaque];                                                         //
    Canvas.Rectangle(0,0,Width,Height);                                                              //
  end;                                                                                               //
                                                                                                     //
  DragAcceptFiles(Handle, True);                                                                     //
  MainCapt := 'WaferViewer '+VersionStr;                                                             //
  Count := 0;                                                                                        //
                                                                                                     //
  LabLang := 0; // 0 - русский, остальные - английский                                               //
  SetLabelsLang(LabLang);                                                                            //
  ClearLabels;                                                                                       //
  Sleep(100);                                                                                        //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormShow(Sender: TObject);                                                          //
begin                                                                                                //
  if ParamCount > 0 then LoadFile(ParamStr(1));                                                      //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormDestroy(Sender: TObject);                                                       //
var                                                                                                  //
  Rct: TRect;                                                                                        //
  Ini: TRegIniFile;                                                                                  //
begin                                                                                                //
  DragAcceptFiles(Handle, False);                                                                    //
                                                                                                     //
  Rct := BoundsRect;                                                                                 //
  try                                                                                                //
    Ini := TRegIniFile.Create('Software\WaferViewer');                                               //
    try                                                                                              //
      Ini.WriteBinaryData('FormPosition', Rct, SizeOf(TRect));                                       //
      Ini.WriteBool   ('', 'Maximized', self.WindowState = wsMaximized);                             //
      Ini.WriteString ('', 'StatPath', StatPath);                                                    //
      Ini.WriteInteger('', 'ShowChipsColor', Statistica.clShowChips);                                //
      Ini.WriteInteger('', 'BkgrnColor', Statistica.Color);                                          //
      Ini.WriteInteger('', 'GridColor', Statistica.clGrid);                                          //
      Ini.WriteInteger('', 'ViewOK_3D', Statistica.ViewOK_3D);                                       //
      Ini.WriteBool   ('', 'ViewAll', Statistica.ViewAll);                                           //
      Ini.WriteBool   ('', 'ViewGrid', Statistica.ViewGrid);                                         //
    finally                                                                                          //
      Ini.Free;                                                                                      //
    end;                                                                                             //
  except                                                                                             //
  end;                                                                                               //
                                                                                                     //
  JoyBtn.Free;                                                                                       //
  Gauge.Free;                                                                                        //
  DirBmp.Free;                                                                                       //
  FreeAndNil(Statistica);                                                                            //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////
procedure TForm1.FormPaint(Sender: TObject);                     //
begin                                                            //
  Canvas.Draw(Statistica.Left,Statistica.Top,  StatTransBMP);    //
  Canvas.Draw(WafInfoGroup.Left,WafInfoGroup.Top, InfoTransBMP); //
end;                                                             //
///////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
procedure TForm1.FormResize(Sender: TObject);           //
begin                                                   //
  if Statistica <> nil then                             //
  begin                                                 //
    with Statistica do                                  //
    begin                                               //
      Width  := self.ClientWidth-Left-5;                //
      Height := self.ClientHeight-Top-5;                //
                                                        //
      if StatTransBMP <> nil then                       //
      begin                                             //
        StatTransBMP.Width  := Width;                   //
        StatTransBMP.Height := Height;                  //
      end;                                              //
    end;                                                //
  end;                                                  //
end;                                                    //
//////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //
begin                                                                             //
  case Key of                                                                     //
    37: Left := Left-1;                                                           //
    38: Top  := Top-1;                                                            //
    39: Left := Left+1;                                                           //
    40: Top  := Top+1;                                                            //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mOpenClick(Sender: TObject);                                     //
var                                                                               //
  OpenDlg: TOpenDialog;                                                           //
begin                                                                             //
  OpenDlg := TOpenDialog.Create(self);                                            //
  with OpenDlg do                                                                 //
  begin                                                                           //
    InitialDir := StatPath;                                                       //
    Filter := 'Файлы статистики (*.sts, *.txt, *.map)|*.sts;*.txt;*.map;*';       //
    Title := 'Открыть файл статистики';                                           //
                                                                                  //
    if Execute then LoadFile(FileName);                                           //
                                                                                  //
    Free;                                                                         //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mAddClick(Sender: TObject);                                      //
var                                                                               //
  OpenDlg: TOpenDialog;                                                           //
begin                                                                             //
  OpenDlg := TOpenDialog.Create(self);                                            //
  with OpenDlg do                                                                 //
  begin                                                                           //
    InitialDir := StatPath;                                                       //
    Filter := 'Файлы статистики (*.sts, *.txt, *.map)|*.sts;*.txt;*.map;*';       //
    Title := 'Открыть файл статистики';                                           //
                                                                                  //
    if Execute then AddFile(FileName);                                            //
                                                                                  //
    Free;                                                                         //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mSaveClick(Sender: TObject);                                     //
var                                                                               //
  SaveDlg: TSaveDialog;                                                           //
begin                                                                             //
  SaveDlg := TSaveDialog.Create(self);                                            //
  with SaveDlg do                                                                 //
  begin                                                                           //
    FileName  := Statistica.Wafer.fName;                                          //
    InitialDir := StatPath;                                                       //
    Filter := 'Файлы статистики (*.sts)|*.sts';                                   //
    Title := 'Сохранить файл статистики';                                         //
    Options := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt];               //
                                                                                  //
    if Execute then                                                               //
      if Pos('.sts', FileName) <> 0 then Statistica.SaveSTS(FileName)             //
                                    else Statistica.SaveSTS(FileName+'.sts');     //
    Free;                                                                         //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mPrintClick(Sender: TObject);                                    //
var                                                                               //
  PrintDlg: TPrintDialog;                                                         //
begin                                                                             //
  PrintDlg := TPrintDialog.Create(self);                                          //
  with PrintDlg do                                                                //
  begin                                                                           //
    if Execute then Statistica.PrintWafer;                                        //
                                                                                  //
    Free;                                                                         //
  end;                                                                            //
                                                                                  //
  Statistica.Repaint;                                                             //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mPrMapClick(Sender: TObject);                                    //
var                                                                               //
  SaveDlg: TSaveDialog;                                                           //
begin                                                                             //
  SaveDlg := TSaveDialog.Create(self);                                            //
  with SaveDlg do                                                                 //
  begin                                                                           //
    FileName  := Statistica.Wafer.Code+'_Prober690Map';                           //
    InitialDir := StatPath;                                                       //
    Filter := 'Файлы статистики (*.txt)|*.txt';                                   //
    Title := 'Сохранить карту обхода для зонда 690';                              //
    Options := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt];               //
                                                                                  //
    if Execute then CreateAndSaveMap(FileName+'.txt');                            //
                                                                                  //
    Free;                                                                         //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mAdjustClick(Sender: TObject);                                   //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left+deltaW7;                            //
  Y := self.GetClientOrigin.Y+Statistica.Top+deltaW7;                             //
                                                                                  //
  PrefDlg := TPrefDlg.Create(self, @Statistica);                                  //
  with PrefDlg do                                                                 //
  begin                                                                           //
    PrefDlg.Left := X;                                                            //
    PrefDlg.Top := Y;                                                             //
                                                                                  //
    ShowModal;                                                                    //
                                                                                  //
    Free;                                                                         //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mExitClick(Sender: TObject);                                     //
begin                                                                             //
  Close;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mExcelClick(Sender: TObject);                                    //
var                                                                               //
  XL: variant;                                                                    //
  FData, Sheet, Range: Variant;                                                   //
  m, n: DWORD;                                                                    //
begin                                                                             //
  XL := CreateOleObject(GetExcelAppName2());
//  XL := CreateOleObject(ClassIDToProgID(CLASS_WordApplication))as _Application);
  XL.DisplayAlerts := False;
  XL.WorkBooks.Add;

  with Statistica.Wafer do
  begin
    FData := VarArrayCreate([1, NMeased+1, 1, Length(TestsParams)], varVariant);

    for m := 1 to VarArrayHighBound(FData, 2) do FData[1, m] := TestsParams[m-1].Name; // первая строка для названий тестов
    for n := 2 to VarArrayHighBound(FData, 1) do // NMeased
      if Length(Chip[ChipN[n-2].Y, ChipN[n-2].X].ChipParams) > 0 then
        for m := 1 to VarArrayHighBound(FData, 2) do // NTestParams
          if Chip[ChipN[n-2].Y, ChipN[n-2].X].ChipParams[m-1].Value <> NotSpec then
            FData[n, m] := FormatFloat('0.000', Chip[ChipN[n-2].Y, ChipN[n-2].X].ChipParams[m-1].Value)
          else
            FData[n, m] := 'нет';
  end;

{
  m := 1; // первая строка для названий тестов
  with Statistica.Wafer do
  begin
    for n := 0 to Length(TestsParams)-1 do
      XL.WorkBooks[1].WorkSheets[1].Cells[1, n+1] := TestsParams[n].Name;

    for k := 0 to NMeased-1 do
    begin
      Inc(m);
      if Length(Chip[ChipN[k].Y, ChipN[k].X].ChipParams) > 0 then
        for n := 0 to Length(Chip[ChipN[k].Y, ChipN[k].X].ChipParams)-1 do
          if Chip[ChipN[k].Y, ChipN[k].X].ChipParams[n].Value <> NotSpec then
            XL.WorkBooks[1].WorkSheets[1].Cells[m, n+1] := FormatFloat('0.000', Chip[ChipN[k].Y, ChipN[k].X].ChipParams[n].Value)
          else XL.WorkBooks[1].WorkSheets[1].Cells[m, n+1] := 'нет';
    end;
  end;
}

  Sheet := XL.ActiveWorkBook.Sheets[1];
  Sheet.Activate;
  Range := Sheet.Range[Sheet.Cells[1, 1], Sheet.Cells[VarArrayHighBound(FData, 1), VarArrayHighBound(FData, 2)]];

  Range.Value := FData;

  XL.Visible := True;
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mFullClick(Sender: TObject);                                     //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left;                                    //
  Y := self.GetClientOrigin.Y+Statistica.Top;                                     //
                                                                                  //
  if Statistica.Wafer <> nil then                                                 //
    with Statistica.Wafer do                                                      //
    begin                                                                         //
      if Length(TestsParams) = 0 then Exit;                                       //
                                                                                  //
      try                                                                         //
        if FullChipsDlg <> nil then FreeAndNil(FullChipsDlg);                     //
      except                                                                      //
      end;                                                                        //
      FullChipsDlg := TFullChipsDlg.Create(self, @Statistica);                    //
      FullChipsDlg.Left := 10;                                                    //
      FullChipsDlg.Top  := 10;                                                    //
      FullChipsDlg.Show;                                                          //
    end;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mFailsClick(Sender: TObject);                                    //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left+deltaW7;                            //
  Y := self.GetClientOrigin.Y+Statistica.Top+deltaW7;                             //
                                                                                  //
  if Statistica.Wafer <> nil then                                                 //
    with Statistica.Wafer do                                                      //
    begin                                                                         //
      try                                                                         //
        if FailsDlg <> nil then                                                   //
        begin                                                                     //
          X := FailsDlg.Left;                                                     //
          Y := FailsDlg.Top;                                                      //
          FreeAndNil(FailsDlg);                                                   //
        end;                                                                      //
      except                                                                      //
      end;                                                                        //
      FailsDlg := TFailsDlg.Create(self, @Statistica);                            //
      FailsDlg.Left := X;                                                         //
      FailsDlg.Top  := Y;                                                         //
      FailsDlg.Show;                                                              //
    end;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mDataClick(Sender: TObject);                                     //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left;                                    //
  Y := self.GetClientOrigin.Y+Statistica.Top;                                     //
                                                                                  //
  if Statistica.Wafer <> nil then                                                 //
    with Statistica.Wafer do                                                      //
    begin                                                                         //
      if Length(TestsParams) = 0 then Exit;                                       //
                                                                                  //
      try                                                                         //
        if DataDlg <> nil then FreeAndNil(DataDlg);                               //
      except                                                                      //
      end;                                                                        //
      DataDlg := TDataDlg.Create(self, @Statistica, Application.ExeName);         //
      DataDlg.Left := X;                                                          //
      DataDlg.Top  := Y;                                                          //
      DataDlg.Show;                                                               //
    end;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mOKClick(Sender: TObject);                                       //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left;                                    //
  Y := self.GetClientOrigin.Y+Statistica.Top;                                     //
                                                                                  //
  if Statistica.Wafer <> nil then                                                 //
    with Statistica.Wafer do                                                      //
    begin                                                                         //
      if Length(TestsParams) = 0 then Exit;                                       //
                                                                                  //
      try                                                                         //
        if OKDlg <> nil then FreeAndNil(OKDlg);                                   //
      except                                                                      //
      end;                                                                        //
      OKDlg := TOKDlg.Create(self, @Statistica);                                  //
      OKDlg.Left := X;                                                            //
      OKDlg.Top  := Y;                                                            //
      OKDlg.Show;                                                                 //
    end;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mPDClick(Sender: TObject);                                       //
var                                                                               //
  X, Y: Integer;                                                                  //
begin                                                                             //
  X := self.GetClientOrigin.X+Statistica.Left+deltaW7;                            //
  Y := self.GetClientOrigin.Y+Statistica.Top+deltaW7;                             //
                                                                                  //
  if Statistica.Wafer <> nil then                                                 //
    with Statistica.Wafer do                                                      //
    begin                                                                         //
      if Length(Statistica.Wafer.PD) = 0 then                                     //
      begin                                                                       //
        ErrMess(Handle, 'Размер кадра не известен!');                             //
        Exit;                                                                     //
      end;                                                                        //
                                                                                  //
      try                                                                         //
        if PDDlg <> nil then                                                      //
        begin                                                                     //
          X := PDDlg.Left;                                                        //
          Y := PDDlg.Top;                                                         //
          FreeAndNil(PDDlg);                                                      //
        end;                                                                      //
      except                                                                      //
      end;                                                                        //
      PDDlg := TPDDlg.Create(self, @Statistica);                                  //
      PDDlg.Left := X;                                                            //
      PDDlg.Top  := Y;                                                            //
      PDDlg.Show;                                                                 //
    end;                                                                          //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mHelpClick(Sender: TObject);                                     //
begin                                                                             //
  try                                                                             //
    if HelpDlg <> nil then FreeAndNil(HelpDlg);                                   //
    except                                                                        //
    end;                                                                          //
  HelpDlg := THelpDlg.Create(self);                                               //
  HelpDlg.Top  := self.Top+deltaW7;                                               //
  HelpDlg.Left := self.GetClientOrigin.X+self.ClientWidth-HelpDlg.Width;          //
  HelpDlg.Height := self.Height;                                                  //
  HelpDlg.Show;                                                                   //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.mAboutClick(Sender: TObject);                                    //
begin                                                                             //
  AboutDlg := TAboutDlg.Create(self);                                             //
  AboutDlg.Top  := self.GetClientOrigin.Y+Statistica.Top+Statistica.Height div 4; //
  AboutDlg.Left := self.GetClientOrigin.X+Statistica.Left+Statistica.Width div 4; //
  AboutDlg.ShowModal;                                                             //
  AboutDlg.Free;                                                                  //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////
procedure TForm1.ColLabClick(Sender: TObject);                                  //
var                                                                             //
  n: WORD;                                                                      //
  ColorDlg: TColorDialog;                                                       //
begin                                                                           //
  ColorDlg := TColorDialog.Create(self);                                        //
  ColorDlg.Color := TLabel(Sender).Color;                                       //
  if ColorDlg.Execute then                                                      //
  begin                                                                         //
    TLabel(Sender).Color := ColorDlg.Color;                                     //
                                                                                //
    case TLabel(Sender).Tag of                                                  //
      0: clOK     := ColorDlg.Color;                                            //
                                                                                //
      1: clFailNC := ColorDlg.Color;                                            //
                                                                                //
      2: begin                                                                  //
           clFailSC := ColorDlg.Color;                                          //
                                                                                //
           if Statistica.Wafer <> nil then                                      //
             with Statistica.Wafer do                                           //
               if Length(FailsSC) > 0 then                                      //
                 for n := 0 to Length(FailsSC)-1 do FailsSC[n].Col := clFailSC; //
         end;                                                                   //
                                                                                //
      3:  begin                                                                 //
           clFailFC := ColorDlg.Color;                                          //
                                                                                //
           if Statistica.Wafer <> nil then                                      //
             with Statistica.Wafer do                                           //
               if Length(FailsFC) > 0 then                                      //
                 for n := 0 to Length(FailsFC)-1 do FailsFC[n].Col := clFailFC; //
         end;                                                                   //
    end;                                                                        //
                                                                                //
    try                                                                         //
      if FailsDlg <> nil then FailsDlg.Refresh;                                 //
    except                                                                      //
    end;                                                                        //
                                                                                //
    Statistica.Repaint;                                                         //
  end;                                                                          //
  ColorDlg.Free;                                                                //
end;                                                                            //
//////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////
procedure TForm1.JoyDown(const TypeBtn: byte); //
begin                                          //
  with Statistica do                           //
    case TypeBtn of                            //
      0: begin                                 //
           SizeChipX := 0;                     //
           SizeChipY := 0;                     //
         end;                                  //
      1: begin                                 //
           RotateWafer;                        //
           FillLabels;                         //
         end;                                  //
      2: SizeChipX := SizeChipX+1;             //
      3: IncSizeChipXY;                        //
      4: SizeChipY := SizeChipY+1;             //
      5: Statistica.ShowBaseChip;              //
      6: SizeChipX := SizeChipX-1;             //
      7: DecSizeChipXY;                        //
      8: SizeChipY := SizeChipY-1;             //
    end;                                       //
end;                                           //
/////////////////////////////////////////////////
/////////////////////////////////////////////////
procedure TForm1.JoyUp(const TypeBtn: byte);   //
begin                                          //
  with Statistica do                           //
    case TypeBtn of                            //
      5: Statistica.HideBaseChip;              //
    end;                                       //
end;                                           //
/////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); //
begin                                                                                                                              //
  Handled := True;                                                                                                                 //
                                                                                                                                   //
  if Shift = [ssCtrl] then // Крутим колесо с Ctrl                                                                                 //
  begin                                                                                                                            //
    if WheelDelta < 0 then Statistica.IncSizeChipXY                                                                                //
                      else Statistica.DecSizeChipXY;                                                                               //
    Exit;                                                                                                                          //
  end;                                                                                                                             //
                                                                                                                                   //
  if Shift = [ssShift] then // Крутим колесо с Shift                                                                               //
  begin                                                                                                                            //
    with Statistica.HorzScrollBar do                                                                                               //
      if WheelDelta > 0 then Position := ScrollPos-20                                                                              //
                        else Position := ScrollPos+20;                                                                             //
    Exit;                                                                                                                          //
  end;                                                                                                                             //
                                                                                                                                   //
  with Statistica.VertScrollBar do // Крутим колесо                                                                                //
    if WheelDelta > 0 then Position := ScrollPos-20                                                                                //
                      else Position := ScrollPos+20;                                                                               //
end;                                                                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////
procedure TForm1.WMDropFiles(var Msg: TMessage);     //
var                                                  //
  hDrop: THandle;                                    //
  fName: String;                                     //
  fNameLen: Integer;                                 //
begin                                                //
  hDrop:= Msg.wParam;                                //
                                                     //
  fNameLen := DragQueryFile(hDrop, 0, nil, 0)+1;     //
  SetLength(fName, fNameLen);                        //
                                                     //
  DragQueryFile(hDrop, 0, Pointer(fName), fNameLen); //
  LoadFile(fName);                                   //
  DragFinish(hDrop);                                 //
end;                                                 //
///////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.DirBoxPaint(Sender: TObject);                                         //
begin                                                                                  //
  with Statistica, DirBox.Canvas do                                                    //
    if Wafer <> nil then                                                               //
      BitBlt(Handle, 0, 0, 28, 28, DirBmp.Canvas.Handle, 28*Wafer.Direct, 0, SRCCOPY); //
end;                                                                                   //
/////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.TransTBarChange(Sender: TObject);                                                            //
begin                                                                                                         //
  with TransTBar do AlphaBlendValue := Max-Position+Min;                                                      //
end;                                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.TransBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
begin                                                                                                         //
  if TransBtnEnabled then                                                                                     //
  begin                                                                                                       //
    Statistica.Visible   := False;                                                                            //
    WafInfoGroup.Visible := False;                                                                            //
  end;                                                                                                        //
end;                                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.TransBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   //
begin                                                                                                         //
  if TransBtnEnabled then                                                                                     //
  begin                                                                                                       //
    Statistica.Visible   := True;                                                                             //
    WafInfoGroup.Visible := True;                                                                             //
  end;                                                                                                        //
end;                                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////
procedure TForm1.PaintWafer(Max: WORD); //
begin                                   //
  FillLabels;                           //
                                        //
  Count := 0;                           //
end;                                    //
//////////////////////////////////////////


////////////////////////////////////////////////////////////////
function TForm1.LoadFile(const fName: TFileName): Boolean;    //
begin                                                         //
  Result := False;                                            //
                                                              //
  case DetectFileType(fName) of                               //
     0: ErrMess(Handle, 'Неизвестный формат файла: '+fName);  //
     1: if Statistica.LoadSTS   (fName) then Result := True;  //
    20: if Statistica.LoadNI    (fName) then Result := True;  //
    21: if Statistica.LoadNI2   (fName) then Result := True;  //
     3: if Statistica.LoadXML   (fName) then Result := True;  //
     4: if Statistica.LoadAGL   (fName) then Result := True;  //
     5: if Statistica.LoadXLS2  (fName) then Result := True;  //
     6: if Statistica.LoadXLSPxn(fName) then Result := True;  //
  end;                                                        //
                                                              //
  if Result then self.Caption := MainCapt+'   '+fName         //
            else ClearLabels;                                 //
  CloseAllDialogs;                                            //
end;                                                          //
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
function TForm1.AddFile(const fName: TFileName): Boolean;     //
begin                                                         //
  Result := False;                                            //
                                                              //
  case DetectFileType(fName) of                               //
     0: ErrMess(Handle, 'Неизвестный формат файла: '+fName);  //
     1: if Statistica.AddSTS(fName) then Result := True;      //
    20: if Statistica.AddNI (fName) then Result := True;      //
    21: if Statistica.AddNI2(fName) then Result := True;      //
     3: if Statistica.AddXML(fName) then Result := True;      //
     4: if Statistica.AddAGL(fName) then Result := True;      //
     5: if Statistica.AddXLS(fName) then Result := True;      //
  end;                                                        //
                                                              //
  if Result then self.Caption := MainCapt+'   '+fName;        //
  CloseAllDialogs;                                            //
end;                                                          //
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
function TForm1.DetectFileType(const fName: TFileName): byte; //
var                                                           //
  FS : TFileStream;                                           //
  Str, tmpStr: AnsiString;                                    //
  Chars: array[0..1999] of char;                              //
  SL: TStringList;                                            //
  n: WORD;                                                    //
begin                                                         //
  Result := 0;                                                //
                                                              //
  StatPath := ExtractFilePath(fName);                         //
                                                              //
  FS := TFileStream.Create(fName, fmOpenRead);                //
  FS.Position := 0;                                           //
  FS.Read(Chars[0], 2000);                                    //
  Str := AnsiLowerCase(string(Chars));                        //
  FS.Free;                                                    //
                                                              //
  if Pos('[main]', Str) <> 0 then                             //
  begin                                                       //
    Result := 1; // STS                                       //
    Exit;                                                     //
  end;                                                        //
  if Pos('изделие', Str) <> 0 then // NI                      //
  begin                                                       //
    SL := TStringList.Create;                                 //
    SL.LoadFromFile(fName);                                   //
    for n := 0 to SL.Count-1 do                               //
    begin
//      if (Pos('=', SL.Strings[n]) <> 0) or                    //
//         (Pos('#', SL.Strings[n]) <> 0) then Continue         //
      tmpStr := Trim(SL.Strings[n]);
      try StrToInt(tmpStr[1])
      except
        Continue;
      end;                                                        //
//      begin                                                   //
        Str := Trim(SL.Strings[n]);                                 //
        Break;                                                //
//      end;                                                    //
    end;
    SL.Free;                                                  //
                                                              //
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер кристалла   //
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер теста       //
    Delete(Str, 1, Pos(#9, Str)); // Удалим имя параметра     //
    Str := Trim(Copy(Str, 1, Pos(#9, Str)));                  //
                                                              //
    try                                                       //
      StrToFloat(Str);                                        //
      Result := 20; // Основной вариант NI                    //
    except                                                    //
      try                                                     //
        DecimalSeparator := ',';                              //
        StrToFloat(Str);                                      //
        Result := 20; // Основной вариант NI                  //
      except                                                  //
        Result := 21; // не сновной вариант NI                //
      end;                                                    //
      Exit;                                                   //
    end;                                                      //
  end;                                                        //
  if Pos('<ibis_wafer_data>', Str) <> 0 then                  //
  begin                                                       //
    Result := 3; // Зонд 6290                                 //
    Exit;                                                     //
  end;                                                        //
  if Pos('testflow started', Str) <> 0 then                   //
  begin                                                       //
    Result := 4; // Agilent93K (Verigy93K)                    //
    Exit;                                                     //
  end;                                                        //
  if (Pos('pk', Str) <> 0) or (Pos('рп', Str) <> 0) then // XLS
  begin                                                       //
    Result := Statistica.DetectXLS(fName); // Пиксан или Form //
//    Exit;                                                     //
  end;                                                        //
end;                                                          //
////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////
procedure TForm1.CreateAndSaveMap(const fName: TFileName);   //
type                                                         //
  TZone = record                                             //
    ZBeginX : WORD;                                          //
    ZEndX   : WORD;                                          //
    ZBeginY : WORD;                                          //
    ZHeight : WORD;                                          //
  end;                                                       //
                                                             //
var                                                          //
  X, Y, n: WORD;                                             //
  Zone: array of TZone;                                      //
  BegFlag: Boolean;                                          //
  Str: String;                                               //
  F: TextFile;                                               //
begin                                                        //
  SetLength(Zone, 50);                                       //
  n := 0;                                                    //
  BegFlag := False;                                          //
                                                             //
  with Statistica.Wafer do                                   //
    for Y := 0 to Length(Chip)-1 do                          //
    begin                                                    //
      for X := 0 to Length(Chip[0])-1 do                     //
        if IsChip(Chip[Y, X].Status) then                    //
          if BegFlag then Zone[n].ZEndX := 500-BaseChip.X+X  //
          else                                               //
          begin                                              //
            Zone[n].ZBeginX := 500-BaseChip.X+X;             //
            Zone[n].ZBeginY := 500-BaseChip.Y+Y;             //
            Zone[n].ZHeight := 1;                            //
            BegFlag := True;                                 //
          end;                                               //
                                                             //
                                                             //
      if n > 0 then                                          //
        if (Zone[n].ZBeginX = Zone[n-1].ZBeginX) and         //
           (Zone[n].ZEndX   = Zone[n-1].ZEndX) then          //
        begin                                                //
          Inc(Zone[n-1].ZHeight);                            //
          Dec(n);                                            //
        end;                                                 //
                                                             //
      Inc(n);                                                //
      if Length(Zone) < n+1 then SetLength(Zone, n+1);       //
      BegFlag := False;                                      //
    end;                                                     //
  SetLength(Zone, n);                                        //
                                                             //
  if Length(Zone) > 0 then                                   //
  begin                                                      //
    AssignFile(F, fName);                                    //
    Rewrite(F);                                              //
                                                             //
    WriteLn(F, 'Код '+Statistica.Wafer.Code);                //
    WriteLn(F, '');                                          //
                                                             //
    Str := 'Номер зоны'+#9+                                  //
           'Смещение по Y точки 1'+#9+                       //
           'Смещение по Y точки 2'+#9+                       //
           'Смещение по X точки 3'+#9+                       //
           'Смещение по X точки 4';                          //
    WriteLn(F, Str);                                         //
                                                             //
    for n := 0 to Length(Zone)-1 do                          //
    begin                                                    //
      Str := IntToStr(n+1)+#9+                               //
             IntToStr(Zone[n].ZBeginY)+#9+                   //
             IntToStr(Zone[n].ZBeginY+Zone[n].ZHeight-1)+#9+ //
             IntToStr(Zone[n].ZBeginX)+#9+                   //
             IntToStr(Zone[n].ZEndX);                        //
      WriteLn(F, Str);                                       //
    end;                                                     //
                                                             //
    CloseFile(F);                                            //
  end                                                        //
  else ErrMess(Handle, 'Ошибка создания карты обхода!');     //
end;                                                         //
///////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
procedure TForm1.SetLabelsLang(const Lang: byte);                   //
begin                                                               //
  if Lang <> 0 then                                                 //
  begin                                                             //
    MainMenu.Items[0].Caption := 'File';                            //
    MainMenu.Items[1].Caption := 'Analysis';                        //
    MainMenu.Items[2].Caption := 'Help';                            //
                                                                    //
    InformationLab0.Caption := 'Information';                       //
                                                                    //
    OKRLab0.Caption         := 'Product code:';                     //
    CodeLab0.Caption        := 'Production code:';                  //
    MPWLab0.Caption         := 'MPW:';                              //
    MPWPosLab0.Caption      := 'Quantity in MPW:';                  //
    DeviceLab0.Caption      := 'Product:';                          //
    DscrDevLab0.Caption     := 'Product category:';                 //
                                                                    //
    MSystemLab0.Caption     := 'Tester:';                           //
    ProberLab0.Caption      := 'Prober:';                           //
                                                                    //
    DiamLab0.Caption        := 'Wafer diameter:';                   //
    StepXLab0.Caption       := 'Pace on X-axis:';                   //
    StepYLab0.Caption       := 'Pace on Y-axis:';                   //
    CutLab0.Caption         := 'Flat:';                             //
    InfoLab0.Caption        := 'Tech #:';                           //
                                                                    //
    NLotLab0.Caption        := 'Lot #:';                            //
    NWafLab0.Caption        := 'Wafer #:';                          //
    WPlaceLab0.Caption      := 'Working place:';                    //
    OperatorLab0.Caption    := 'Operator:';                         //
    DateLab0.Caption        := 'Date:';                             //
    CondLab0.Caption        := 'Special condition:';                //
                                                                    //
    NTotLab0.Caption        := 'Total dice:';                       //
    NMeasLab0.Caption       := 'Tested dice:';                      //
    NOKLab0.Caption         := 'Good dice:';                        //
    NFailNCLab0.Caption     := 'Failed by NC dice:';                //
    NFailSCLab0.Caption     := 'Failed by SC dice:';                //
    NFailFCLab0.Caption     := 'Failed by FC dice:';                //
                                                                    //
    PathLab0.Caption        := 'Scan dir:';                         //
                                                                    //
    AdjustLab0.Caption      := 'Adjust';                            //
  end;                                                              //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TForm1.FillLabels;                                        //
var                                                                 //
  Tmp: Double;                                                      //
  Str: string;                                                      //
  ClsID: TCLSID;                                                    //
begin                                                               //
  if LabLang = 0 then Str := 'мм.' else Str := 'mm.';               //
                                                                    //
  if Statistica.Wafer <> nil then                                   //
    with Statistica.Wafer do                                        //
    begin                                                           //
      OKRLab.Caption      := OKR;                                   //
      CodeLab.Caption     := Code;                                  //
      MPWLab.Caption      := MPW;                                   //
      MPWPosLab.Caption   := MPWPos;                                //
      DeviceLab.Caption   := Device;                                //
      DscrDevLab.Caption  := DscrDev;                               //
                                                                    //
      MSystemLab.Caption  := MeasSystem;                            //
      ProberLab.Caption   := Prober;                                //
                                                                    //
      DiamLab.Caption     := IntToStr(Diameter)+Str;                //
      StepXLab.Caption    := FormatFloat('0.000', StepX)+Str;       //
      StepYLab.Caption    := FormatFloat('0.000', StepY)+Str;       //
      InfoLab.Caption     := Info;                                  //
                                                                    //
      NLotLab.Caption     := NLot;                                  //
      NWafLab.Caption     := Num;                                   //
                                                                    //
      WPlaceLab.Caption   := IntToStr(NWPlace);                     //
      if LabLang = 0 then OperatorLab.Caption := Operator;          //
      DateLab.Caption     := TimeDate;                              //
      CondLab.Caption     := Condition;                             //
                                                                    //
      NTotLab.Caption     := IntToStr(NTotal);                      //
      NMeasLab.Caption    := IntToStr(NMeased);                     //
      NOKLab.Caption      := IntToStr(NOK);                         //
      NFailNCLab.Caption  := IntToStr(NFailNC);                     //
      NFailSCLab.Caption  := IntToStr(NFailSC);                     //
      NFailFCLab.Caption  := IntToStr(NFailFC);                     //
                                                                    //
      if NMeased <> 0 then Tmp := 100*NOK/(NMeased)                 //
                      else Tmp := -1;                               //
      Gauge.Position := Tmp;                                        //
                                                                    //
      DirBox.Repaint;                                               //
                                                                    //
      if LabLang = 0 then // Русский                                //
      begin                                                         //
        case CutSide of                                             //
          1: CutLab.Caption := 'вверху';                            //
          2: CutLab.Caption := 'слева';                             //
          3: CutLab.Caption := 'внизу';                             //
          4: CutLab.Caption := 'справа';                            //
        else CutLab.Caption := 'нет сведений';                      //
        end;                                                        //
      end                                                           //
      else                                                          //
      begin                                                         //
        case CutSide of                                             //
          1: CutLab.Caption := 'Up';                                //
          2: CutLab.Caption := 'Left';                              //
          3: CutLab.Caption := 'Down';                              //
          4: CutLab.Caption := 'Right';                             //
        else CutLab.Caption := 'Unknown';                           //
        end;                                                        //
      end;                                                          //
                                                                    //
      mAdd.Enabled   := True;                                       //
      mSave.Enabled  := True;                                       //
      mPrint.Enabled := True;                                       //
      mPrMap.Enabled := True;                                       //

//      ShowMessage(GetExcelAppName2());

      if CLSIDFromProgID(PWideChar(WideString(GetExcelAppName2())), ClsID) = S_OK then
        mExcel.Enabled := True;                                     //
      mFull.Enabled  := True;                                       //
      mFails.Enabled := True;                                       //
      mData.Enabled  := True;                                       //
      mOK.Enabled    := True;                                       //
      if Length(PD) > 0 then mPD.Enabled := True                    //
                        else mPD.Enabled := False;                  //
    end;                                                            //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TForm1.ClearLabels;                                       //
begin                                                               //
  OKRLab.Caption      := '';                                        //
  CodeLab.Caption     := '';                                        //
  MPWLab.Caption      := '';                                        //
  MPWPosLab.Caption   := '';                                        //
  NLotLab.Caption     := '';                                        //
  NWafLab.Caption     := '';                                        //
  DeviceLab.Caption   := '';                                        //
  DscrDevLab.Caption  := '';                                        //
                                                                    //
  MSystemLab.Caption  := '';                                        //
  ProberLab.Caption   := '';                                        //
                                                                    //
  DiamLab.Caption     := '';                                        //
  StepXLab.Caption    := '';                                        //
  StepYLab.Caption    := '';                                        //
                                                                    //
  WPlaceLab.Caption   := '';                                        //
  OperatorLab.Caption := '';                                        //
  DateLab.Caption     := '';                                        //
  CondLab.Caption     := '';                                        //
                                                                    //
  NTotLab.Caption     := '';                                        //
  NMeasLab.Caption    := '';                                        //
  NOKLab.Caption      := '';                                        //
  NFailNCLab.Caption  := '';                                        //
  NFailSCLab.Caption  := '';                                        //
  NFailFCLab.Caption  := '';                                        //
                                                                    //
  Gauge.Position := -1;                                             //
                                                                    //
  DirBox.Repaint;                                                   //
  CutLab.Caption  := '';                                            //
  InfoLab.Caption := '';                                            //
                                                                    //
  Caption := MainCapt;                                              //
                                                                    //
  mAdd.Enabled   := False;                                          //
  mSave.Enabled  := False;                                          //
  mPrint.Enabled := False;                                          //
  mPrMap.Enabled := False;                                          //
  mExcel.Enabled := False;                                          //
  mFull.Enabled  := False;                                          //
  mFails.Enabled := False;                                          //
  mData.Enabled  := False;                                          //
  mOK.Enabled    := False;                                          //
  mPD.Enabled    := False;                                          //
end;                                                                //
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
procedure TForm1.CloseAllDialogs;                       //
begin                                                   //
  if FailsDlg     <> nil then FreeAndNil(FailsDlg);     //
  if PDDlg        <> nil then FreeAndNil(PDDlg);        //
  if OKDlg        <> nil then FreeAndNil(OKDlg);        //
  if DataDlg      <> nil then FreeAndNil(DataDlg);      //
  if FullChipsDlg <> nil then FreeAndNil(FullChipsDlg); //
end;                                                    //
//////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////
procedure TForm1.ProcessMessagesEx;                    //
                                                       //
/////////////////////////////////////////////////////  //
  function ProcessMessage(var Msg: TMsg): Boolean; //  //
  begin                                            //  //
    Result := False;                               //  //
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then   //  //
    begin                                          //  //
      Result := True;                              //  //
      if Msg.Message <> WM_QUIT then               //  //
      begin                                        //  //
        TranslateMessage(Msg);                     //  //
        DispatchMessage(Msg);                      //  //
      end                                          //  //
      else Halt(0);                                //  //
    end;                                           //  //
  end;                                             //  //
/////////////////////////////////////////////////////  //
                                                       //
var                                                    //
  Msg: TMsg;                                           //
begin                                                  //
  while ProcessMessage(Msg) do {loop};                 //
end;                                                   //
/////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
procedure TForm1.ViewOK_Repaint_Event(var Mes: TMessage);                     //
begin                                                                         //
  if OKDlg <> nil then SendMessage(OKDlg.Handle, MESS_REPAINT_VIEW_OK, 0, 0); //
end;                                                                          //
////////////////////////////////////////////////////////////////////////////////


end.

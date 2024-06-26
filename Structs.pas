unit Structs;

interface

uses
  Windows, Graphics, SysUtils, Messages, Classes, Registry;

const
  MESS_REPAINT_VIEW_OK = WM_USER+1;

  dURightToLeft = 0;
  dURightSnake  = 1;
  dULeftToRight = 2;
  dULeftSnake   = 3;
  dLUpToDown    = 4;
  dLUpSnake     = 5;
  dLDownToUp    = 6;
  dLDownSnake   = 7;
  dDLeftToRight = 8;
  dDLeftSnake   = 9;
  dDRightToLeft = 10;
  dDRightSnake  = 11;
  dRDownToUp    = 12;
  dRDownSnake   = 13;
  dRUpToDown    = 14;
  dRUpSnake     = 15;

  SizeChipMax = 200;

  NotSpec: Single=987654321.0;

//  ExApp = 'Excel.Application.16';

type
  TOnPaintWafer   = procedure(Max: WORD) of object; // ������� - �������� ���������
  TOnDataTransmit = procedure            of object; // ������� - ����������� ��������� ����������
  TOnChipDlgClose = procedure            of object; // ������� - �������� ������� ���������� ���������

/////////////////////////////////////////

  TColorParams = record
    Num: WORD;
    Min: Single;
    Max: Single;
    Col: TColor;
  end;

/////////////////////////////////////////

  TCadre = record
    StartX: WORD;
    StartY: WORD;
    ScaleX: WORD;
    ScaleY: WORD;
  end;

/////////////////////////////////////////

  TChipParams = record
    Value: Single;
    Stat : byte; // 0-?; 1-������; 2-���� �����; 3-���� �����
  end;

  TChip = record
    ID        : DWORD;
    Status    : WORD;
    Coord     : TPoint;
    ChipParams: array of TChipParams;
    ShowGr    : WORD;
  end;
  PChip = ^TChip;
  TChips = array of array of TChip;

/////////////////////////////////////////

  TFail = record
    Status  : WORD;
    Name    : String[40];
    Quantity: WORD;
    Col     : TColor;
  end;
  TFails = array of TFail;

/////////////////////////////////////////

  TNorma = record
    Min: Single;
    Max: Single;
  end;

  TTestParams = record
    Name  : String[40];
    Norma : TNorma;
    Status: WORD;
  end;
  TTestsParams = array of TTestParams;
  PTestsParams = ^TTestsParams;

/////////////////////////////////////////

  TQuantity = record
    OK  : WORD;
    Fail: WORD;
    Meas: Boolean;
  end;

  TPD = array of array of TQuantity;

/////////////////////////////////////////

  TEdgeCoords = record
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
    X3: Single;
    Y3: Single;
  end;

/////////////////////////////////////////

var
  VersionStr: String;

  deltaW7: byte=0; // �������� ��� WinVista, Win7

  clRepper     : TColor=clPurple;
  clMRepper    : TColor=clFuchsia;
  clNotTested  : TColor=clWhite;
  clNot4Testing: TColor=clSilver;
  cl4Mark      : TColor=$00000040;//clMaroon;
  clNotChip    : TColor=clGray;
  clOK         : TColor=clLime;
  clFailNC     : TColor=clBlack;
  clFailSC     : TColor=clRed;
  clFailFC     : TColor=clBlue;
  clBaseChip   : TColor=clAqua;
  clCurChip    : TColor=clYellow;

  function  IsChip    (const Status: WORD): Boolean;
  function  IsFailChip(const Status: WORD): Boolean;
  function  EqualStatus(const Status1, Status2 : WORD): Boolean;
  function  GetMainColor(const Status: WORD): TColor;
  function  GetPrnColor(const Status: WORD): TColor;
  function  GetStatusString(const Status: WORD): String;
  function  GetWinName:  String;
  function  GetWinName2: String;
  function  GetExcelAppName (): string;
  function  GetExcelAppName2(): string;
  procedure ErrMess(Handle: THandle; const ErrMes: String);
  function  QuestMess(Handle: THandle; const QStr: String): Integer;

implementation

////////////////////////////////////////////////////
function IsChip(const Status: WORD): Boolean;     //
begin                                             //
  Result := False;                                //
                                                  //
  case Status of                                  //
    0         : Result := True;                   //
    1         : Result := True;                   //
    2         : ;                                 //
    3         : ;                                 //
    4         : ;//Result := True;                   //
    5         : ;                                 //
    7         : Result := True;                   //
    10..1500  : Result := True;                   //
    2000..3000: Result := True;                   //
    3500..4500: Result := True;                   //
  end;                                            //
end;                                              //
////////////////////////////////////////////////////
////////////////////////////////////////////////////
function IsFailChip(const Status: WORD): Boolean; //
begin                                             //
  Result := False;                                //
                                                  //
  case Status of                                  //
    0         : ;                                 //
    1         : ;                                 //
    2         : ;                                 //
    3         : ;                                 //
    4         : ;                                 //
    5         : ;                                 //
    7         : ;                                 //
    10..1500  : Result := True;                   //
    2000..3000: Result := True;                   //
    3500..4500: Result := True;                   //
  end;                                            //
end;                                              //
////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
function EqualStatus(const Status1, Status2 : WORD): Boolean;           //
begin                                                                   //
  Result := False;                                                      //
                                                                        //
  if Status1 = Status2 then Result := True                              //
  else                                                                  //
    if IsFailChip(Status1) and IsFailChip(Status1) then Result := True; //
end;                                                                    //
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////
function GetMainColor(const Status: WORD): TColor; //
begin                                              //
  case Status of                                   //
    0         : Result := clNotTested;             //
    1         : Result := clOK;                    //
    2         : Result := clNotChip;               //
    3         : Result := clNot4Testing;           //
    4         : Result := cl4Mark;                 //
    5         : Result := clRepper;                //
    7         : Result := clMRepper;               //
    10..1500  : Result := clFailNC;                //
    2000..3000: Result := clFailSC;                //
    3500..4500: Result := clFailFC;                //
  else          Result := clGray;                  //
  end;                                             //
end;                                               //
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
function GetPrnColor(const Status: WORD): TColor;  //
begin                                              //
  case Status of                                   //
    0         : Result := clWhite;                 //
    1         : Result := clWhite;                 //
    2         : Result := clWhite;                 //
    3         : Result := clWhite;                 //
    4         : Result := clBlack;                 //
    5         : Result := clBlack;                 //
    7         : Result := clBlack;                 //
    10..1500  : Result := clBlack;                 //
    2000..3000: Result := clSilver;                //
    3500..4500: Result := clGray;                  //
  else          Result := clWhite;                 //
  end;                                             //
end;                                               //
/////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
function GetStatusString(const Status: WORD): String;                //
begin                                                                //
  case Status of                                                     //
    0         : Result := '������';                                  //
    1         : Result := '�������� �����';                          //
    2         : Result := '������';                                  //
    3         : Result := '������';                                  //
    4         : Result := '����������� �������������';               //
    5         : Result := '�����';                                   //
    7         : Result := '�����';                                   //
    10..1500  : Result := '��������� �� ������ '+IntToStr(Status-9); //
    2000..3000: Result := '���� ��'+IntToStr(Status-1999);           //
    3500..4500: Result := '���� ��'+IntToStr(Status-3499);           //
  else          Result := '������';                                  //
  end;                                                               //
end;                                                                 //
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////
function GetWinName: String;                                                             //
var                                                                                      //
  lpVersionInformation: TOSVersionInfo;                                                  //
begin                                                                                    //
  Result := '������������';                                                              //
                                                                                         //
  lpVersionInformation.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);                    //
  if GetVersionEx(lpVersionInformation) then                                             //
    with lpVersionInformation do                                                         //
      case DwMajorVersion of                                                             //
        3: if dwMinorVersion = 51 then Result := 'Windows NT 3.51';                      //
                                                                                         //
        4: case dwMinorVersion of                                                        //
             0 : if dwPlatformId = VER_PLATFORM_WIN32_NT then Result := 'Windows NT 4.0' //
                                                         else Result := 'Windows 95';    //
             10: Result := 'Windows 98';                                                 //
             90: Result := 'Windows ME';                                                 //
           end;                                                                          //
                                                                                         //
        5: case dwMinorVersion of                                                        //
             0: Result := 'Windows 2000';                                                //
             1: Result := 'Windows XP';                                                  //
             2: Result := 'Windows 2003';                                                //
           end;                                                                          //
                                                                                         //
        6: case dwMinorVersion of                                                        //
             0: Result := 'Windows Vista';                                               //
             1: Result := 'Windows 7';                                                   //
           end;                                                                          //
      end;                                                                               //
end;                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////
function GetWinName2: String;                                                            //
var
  reg: TRegistry;
begin
  Result := '';

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion', False) then
    begin
      Result := '�������: '+reg.ReadString('ProductName');
//      Result := Result+Format('Version: %d.%d.%s', [reg.ReadInteger('CurrentMajorVersionNumber'),
//                                                    reg.ReadInteger('CurrentMinorVersionNumber'),
//                                                    reg.ReadString('CurrentBuildNumber')]);
//      Result := Result+'Build: ' + reg.ReadString('CurrentBuildNumber');
    end;
  finally
    reg.Free;
  end;
end;

//////////////////////////////////////////////////////////////
function GetExcelAppName(): string;                         

  function GetExcelPath: string;
  begin
    result := '';
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\excel.exe', false) then
          result := ReadString('Path') + 'excel.exe';
      finally
        Free;
      end;
  end;


  function FileVersion(const FileName: TFileName): String;
  var
    VerInfoSize: Cardinal;
    VerValueSize: Cardinal;
    Dummy: Cardinal;
    PVerInfo: Pointer;
    PVerValue: PVSFixedFileInfo;
  begin
    Result := '';

    VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
    if VerInfoSize > 0 then
    begin
      GetMem(PVerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
        begin
          if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
            with PVerValue^ do
              Result := Format('%d.%d.%d build %d', [
                HiWord(dwFileVersionMS), //Major
                LoWord(dwFileVersionMS), //Minor
                HiWord(dwFileVersionLS), //Release
                LoWord(dwFileVersionLS)]); //Build
        end;

      finally
        FreeMem(PVerInfo, VerInfoSize);
      end;
    end;
  end;


var                                                         //
  reg: TRegistry;                                           //
  Str: string;
begin                                                       //
  Result := '';                                             //
                                                            //
  reg := TRegistry.Create;                                  //
  reg.RootKey := HKEY_CLASSES_ROOT;                         //
  try                                                       //
    if reg.OpenKey('\Excel.Application\CurVer', False) then //
    begin                                                   //
      Result := reg.ReadString('');                         //
      reg.CloseKey;                                         //
    end                                                     //
  finally                                                   //
    reg.Free;                                               //
  end;                                                      //

  Str := FileVersion(GetExcelPath);
  Result := 'Excel.Application.'+Copy(Str, 1, Pos('.', Str)-1);
end;                                                        //
//////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////
function GetExcelAppName2(): string;               //
var                                                //
  reg: TRegistry;                                  //
  SL: TStringList;                                 //
  n: DWORD;                                        //
begin                                              //
  Result := '';                                    //
                                                   //
  reg := TRegistry.Create;                         //
  reg.RootKey := HKEY_CLASSES_ROOT;                //
  try                                              //
    if reg.OpenKeyReadOnly('') then                //
    begin                                          //
      SL := TStringList.Create();                  //
      reg.GetKeyNames(SL);                         //
      reg.CloseKey;                                //
    end                                            //
  finally                                          //
    reg.Free;                                      //
  end;                                             //
                                                   //
  if SL.Count > 0 then                             //
    for n := 0 to SL.Count-1 do                    //
      if Pos('Excel.Application', SL[n]) <> 0 then //
      begin                                        //
        Result := SL[n];                           //
        Break;                                     //
      end;                                         //
  SL.Free();                                       //
end;                                               //
/////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////
procedure ErrMess(Handle: THandle; const ErrMes: String);                                //
begin                                                                                    //
  MessageBox(Handle, PChar(ErrMes), '������!!!', MB_ICONERROR+MB_OK);                    //
end;                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////
function QuestMess(Handle: THandle; const QStr: String): Integer;                        //
begin                                                                                    //
  Result := MessageBox(Handle, PChar(Qstr), '�������������!', MB_ICONQUESTION+MB_YESNO); //
end;                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////


end.

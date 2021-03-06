unit Structs;

interface

uses
  Windows, Graphics, SysUtils, Messages;

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

type
  TOnPaintWafer   = procedure(Max: WORD) of object; // ������� - �������� ���������
  TOnDataTransmit = procedure            of object; // ������� - ����������� ��������� ����������
  TOnChipDlgClose = procedure            of object; // ������� - �������� ������� ���������� ���������

/////////////////////////////////////////

  TColorParams = packed record
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

  TDynArray = array of Single;
  TChipValues = TDynArray;
  
  TChip = packed record
    ID    : DWORD;
    Status: WORD;
    Coord : TPoint;
    Value : TChipValues;
    ShowGr: WORD;
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
  function  GetWinName: String;
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
    4         : Result := True;                   //
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

unit Statistica;

interface

uses
  Windows, Forms, Messages, Buttons, Classes, SysUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  Structs, Inifiles, Spin, ViewChipParams, Printers, Math, msxmldom, XMLDoc, XMLIntf, ComCtrls, ComObj;

type
  TWafer = class
  public
    Condition : String[10]; // Условия измерения (НУ, Т+, Т-)
    Direct    : byte;
    CutSide   : byte;
    OKR       : String[25]; // Название ОКРа
    Code      : String[10]; // Номер кристалла
    MPW       : String[20]; // MPW
    MPWPos    : String[20]; // Позиция в MPW
    Device    : String[20];
    DscrDev   : String[20]; // Описание изделия
    MeasSystem: String[25]; // Измерительная система
    Prober    : String[20]; // Зондовая установка
    Info      : String[20];
    NWPlace   : WORD;
    Operator  : String[20];
    NLot      : String[20];
    Num       : String[5];
    NTotal    : WORD;
    NMeased   : WORD;
    NOK       : WORD;
    NFailNC   : WORD;
    NFailSC   : WORD;
    NFailFC   : WORD;
    TimeDate  : String[10];
    Diameter  : WORD;
    LDiameter : Single;
    Radius    : Single;
    LRadius   : Single;
    Chord     : Single;
    StepX     : Single;
    StepY     : Single;

    fName   : TFileName;
    Cadre   : TCadre;
    BaseChip: TPoint;
    HLChip  : TPoint;
    PD      : TPD;
    FailsFC, FailsSC: TFails;

    Chip : TChips;
    ChipN: array of TPoint; // Координаты чипов в порядке измерения

    TestsParams  : TTestsParams;
    StatusNamesSL: TStringList;
    ColorParams  : array of TColorParams;

    constructor Create;
    destructor  Destroy; override;

    function  LoadSTSHeader: Boolean;
    function  LoadNIHeader : WORD;
    function  LoadNI2Header: WORD;
    function  LoadAGLHeader: Boolean;
    function  AddAGLHeader : Boolean;
    procedure Normalize; // Обрезка лишних(не значащих) ячеек(чипов)
    procedure Rotate;
    procedure CalcChips;
    procedure SetChipsID;
    function  IsWafer: Boolean; // Пластина или корпус?
  private
    function GetStatusName(const Status: WORD): String;
  end;

  PStatistica = ^TStatistica;
  TStatistica = class(TScrollingWinControl)
  public
    Wafer: TWafer;
    LastShowMode: byte;
    clShowChips: TColor;
    clGrid     : TColor;
    ShowGroup: WORD;
    ViewOK_3D: byte;
    ViewAll: Boolean;
    ViewGrid: Boolean;

    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    procedure   Repaint(const ShowMode: byte=0);

    function  LoadSTS   (const STSfName: TFileName): Boolean;
    function  AddSTS    (const STSfName: TFileName): Boolean;
    function  SaveSTS   (const STSfName: TFileName): Boolean;
    function  LoadNI    (const NIfName : TFileName): Boolean;
    function  AddNI     (const NIfName : TFileName): Boolean;
    function  LoadNI2   (const NIfName : TFileName): Boolean;
    function  AddNI2    (const NIfName : TFileName): Boolean;
    function  LoadXML   (const XMLfName: TFileName): Boolean;
    function  AddXML    (const XMLfName: TFileName): Boolean;
    function  LoadAGL   (const AGLfName: TFileName): Boolean;
    function  AddAGL    (const AGLfName: TFileName): Boolean;
    function  DetectXLS (const fName   : TFileName): byte;
    function  LoadXLS   (const XLSfName: TFileName): Boolean;
    function  LoadXLS2  (const XLSfName: TFileName): Boolean;
    function  LoadXLSPxn(const XLSfName: TFileName): Boolean;
    function  AddXLS    (const XLSfName: TFileName): Boolean;

    function  GetChipParamsStat(Val, Min, Max: Single): byte;
    procedure RotateWafer;
    function  PrintWafer: Boolean;
    procedure IncSizeChipXY;
    procedure DecSizeChipXY;
    procedure ShowBaseChip;
    procedure HideBaseChip;
    function  GetColor(const Chp: PChip; const ShowMode: byte=0): TColor;

    procedure DrawChip(const XY: TPoint; const cCol: TColor; const bCol: TColor=clGray);
  private
    fOnPaintWafer: TOnPaintWafer;

    WBitmap: TBitmap;
    PBox: TPaintBox;
    fSizeChipX, fSizeChipY: WORD;
    PrevEdgeCoords: TEdgeCoords;

    procedure Init;
    function  GetChipCoord(const X, Y: Integer): TPoint;
    procedure DrawWafer(const ShowMode: byte=0);
    //procedure DrawChip(const XY: TPoint; const cCol: TColor; const bCol: TColor=clGray);
    procedure DrawCadre;
    procedure DrawEdge(const EdgeCoords: TEdgeCoords);
    procedure SetSizeChipX(const Value: WORD);
    procedure SetSizeChipY(const Value: WORD);

    procedure PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBoxPaint(Sender: TObject);

    procedure ChipDlgClose;
  published
    property SizeChipX: WORD read fSizeChipX write SetSizeChipX;
    property SizeChipY: WORD read fSizeChipY write SetSizeChipY;

    property Color;

    property OnWaferPainted: TOnPaintWafer   read fOnPaintWafer write fOnPaintWafer;
  end;

implementation

uses StrUtils;

{$R *.res}

{ TWafer }

/////////////////////////////////////////
constructor TWafer.Create;             //
begin                                  //
  StatusNamesSL := TStringList.Create; //
                                       //
  HLChip.X := -1;                      //
  HLChip.Y := -1;                      //
end;                                   //
/////////////////////////////////////////
/////////////////////////////////////////
destructor TWafer.Destroy;             //
begin                                  //
  StatusNamesSL.Free;                  //
                                       //
  inherited;                           //
end;                                   //
/////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////
function TWafer.LoadSTSHeader: Boolean;                     //
var                                                         //
  INIfName: TIniFile;                                       //
  X, Y, n: WORD;                                            //
  P: byte;                                                  //
  tmpSL: TStringList;                                       //
  Str: String;                                              //
begin                                                       //
  Result := True;                                           //
                                                            //
  INIfName := TIniFile.Create(fName);                       //
  with INIfName do                                          //
  begin                                                     //
    OKR        := ReadString ('Main', 'OKR', '-');          //
    Code       := ReadString ('Main', 'Code', '0');         //
    MPW        := ReadString ('Main', 'MPW', '-');          //
    MPWPos     := ReadString ('Main', 'MPWPos', '-');       //
    Device     := ReadString ('Main', 'Device', '-');       //
    DscrDev    := ReadString ('Main', 'DscrDev', '-');      //
    MeasSystem := ReadString ('Main', 'MSystem', '-');      //
    Prober     := ReadString ('Main', 'Prober', '-');       //
                                                            //
    Diameter   := ReadInteger('Main', 'Diametr', 0);        //
    StepX      := ReadInteger('Main', 'ChipSizeX', 0)/1000; //
    StepY      := ReadInteger('Main', 'ChipSizeY', 0)/1000; //
    NWPlace    := ReadInteger('Main', 'WorkPlace', 0);      //
    Operator   := ReadString ('Main', 'Operator', '');      //
    NLot       := ReadString ('Main', 'Lot', '0');          //
    P := Pos('-', NLot);                                    //
    if P > 0 then Delete(NLot, 1, P);                       //
    Num        := ReadString ('Main', 'Wafer', '0');        //
    TimeDate   := ReadString ('Main', 'Date', '00.00.00');  //
    Condition  := ReadString ('Main', 'Condition', '-');    //
    Info       := ReadString ('Main', 'Info', '');          //
                                                            //
    Cadre.StartX := ReadInteger('Add', 'OffsetX', 0);       //
    Cadre.StartY := ReadInteger('Add', 'OffsetY', 0);       //
    Cadre.ScaleX := ReadInteger('Add', 'CadreX', 0);        //
    Cadre.ScaleY := ReadInteger('Add', 'CadreY', 0);        //
    X := ReadInteger('Add', 'MaxX', 0);                     //
    Y := ReadInteger('Add', 'MaxY', 0);                     //
    BaseChip.X := ReadInteger('Add', 'BaseChipX', 0);       //
    BaseChip.Y := ReadInteger('Add', 'BaseChipY', 0);       //
    Direct  := ReadInteger('Add', 'Path', 0);               //
    CutSide := ReadInteger('Add', 'Cut', 0);                //
                                                            //
    if (X = 0) or (Y = 0) or (Code = '0') then              //
    begin                                                   //
      Result := False;                                      //
      Exit;                                                 //
    end;                                                    //
                                                            //
    SetLength(Chip, 0, 0);                                  //
    SetLength(Chip, Y, X);                                  //
    for Y := 0 to Length(Chip)-1 do      // Очистим         //
      for X := 0 to Length(Chip[0])-1 do // массив          //
      begin                              //                 //
        Chip[Y, X].Status := 2;          //                 //
        Chip[Y, X].ID     := 0;          //                 //
        Chip[Y, X].ShowGr := 0;          //                 //
      end;                               //                 //
                                                            //

    ReadSectionValues('StatusNames', StatusNamesSL);

    tmpSL := TStringList.Create;
    ReadSectionValues('TestsParams', tmpSL);

    if tmpSL.Count > 0 then
    begin
      SetLength(TestsParams, tmpSL.Count);

      for n := 0 to tmpSL.Count-1 do
      begin
        P := Pos('=', tmpSL.Strings[n]);
        if P <> 0 then
        begin
          Str := Trim(Copy(tmpSL.Strings[n], P+1, Length(tmpSL.Strings[n])-P));
          P := Pos(';', Str);
          if P <> 0 then
          begin
            try
              DecimalSeparator := ',';
              TestsParams[n].Norma.Min := StrToFloat(Trim(Copy(Str, 1, P-1)));
              DecimalSeparator := '.';
            except
              try
                DecimalSeparator := '.';
                TestsParams[n].Norma.Min := StrToFloat(Trim(Copy(Str, 1, P-1)));
              except
                TestsParams[n].Norma.Min := -NotSpec;
              end
            end;
            System.Delete(Str, 1, P);
            P := Pos(';', Str);
            if P <> 0 then
            begin
              try
                DecimalSeparator := ',';
                TestsParams[n].Norma.Max := StrToFloat(Trim(Copy(Str, 1, P-1)));
                DecimalSeparator := '.';
              except
                try
                  DecimalSeparator := '.';
                  TestsParams[n].Norma.Max := StrToFloat(Trim(Copy(Str, 1, P-1)));
                except
                   TestsParams[n].Norma.Max := NotSpec;
                end;
              end;
              System.Delete(Str, 1, P);
              try
                TestsParams[n].Status := StrToInt(Trim(Copy(Str, 1, Length(Str)))); //
              except                                                                //
                TestsParams[n].Status := 0;                                         //
              end;                                                                  //
            end;                                                                    //
          end;                                                                      //
        end;                                                                        //
      end;                                                                          //
    end;                                                                            //
    tmpSL.Free;                                                                     //
                                                                                    //
    Free;                                                                           //
  end;                                                                              //
                                                                                    //
  LDiameter := Diameter;                                                            //
  if Diameter = 150 then LDiameter := 144.25;                                       //
  Radius  := Diameter/2;                                                            //
  LRadius := Radius-(Diameter-LDiameter);                                           //
  Chord   := Sqrt(Radius*Radius-LRadius*LRadius);                                   //
end;                                                                                //
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
function TWafer.LoadNIHeader: WORD;                         //
var                                                         //
  SL: TStringList;                                          //
  n, TotalChips: DWORD;                                     //
  P: byte;                                                  //
  X, Y: WORD;                                               //
  NumChip, PrevChip, Str, tmpStr: AnsiString;
  FirstTime: Boolean;
begin
  Result := 0;

  SL := TStringList.Create;
  SL.LoadFromFile(fName);

  FirstTime := True;
  X := 0;
  TotalChips := 1;
  for n := 0 to SL.Count-1 do
  begin
    if Trim(SL.Strings[n]) = '' then Continue;
//    if Trim(SL.Strings[n])[1] = '/' then Continue;
    Inc(Result);

//    P := Pos('=', SL.Strings[n]);
//    if P <> 0 then
    tmpStr := Trim(SL.Strings[n]);
    try StrToInt(tmpStr[1])
    except
      begin
        P := Pos('=', SL.Strings[n]);
        if Pos('Изделие',       SL.Strings[n]) <> 0 then Device   := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
        if Pos('Дата',          SL.Strings[n]) <> 0 then TimeDate := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
  //      if Pos('Время', SL.Strings[n])         <> 0 then TimeDate := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
        if Pos('Вид испытаний', SL.Strings[n]) <> 0 then info := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))))+' ';
        if Pos('Условия',       SL.Strings[n]) <> 0 then Condition := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))))+' ';
        if Pos('Оператор', SL.Strings[n])      <> 0 then Operator := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));

        Continue;
      end;
    end;

    Dec(Result);

    Str := SL.Strings[n];
//    if Pos(',', Str) = 0 then DecimalSeparator := '.'
//                         else DecimalSeparator := ',';
    NumChip := Trim(Copy(Str, 1, Pos(#9, Str)));
    if FirstTime then
    begin
      PrevChip := NumChip;
      FirstTime := False;
    end;
    if NumChip <> PrevChip then
    begin
      X := 0;
      Inc(TotalChips);
    end;
    if X >= Length(TestsParams) then SetLength(TestsParams, X+1);
    tmpStr := '';
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер кристалла
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер теста
    tmpStr := Trim(Copy(Str, 1, Pos(#9, Str))); // Запомним имя параметра
    Delete(Str, 1, Pos(#9, Str)); // Удалим название параметра
    Delete(Str, 1, Pos(#9, Str)); // Удалим параметр
    tmpStr := tmpStr+' ('+Trim(Copy(Str, 1, Pos(#9, Str)))+')'; // Запомним полное имя параметра
    TestsParams[X].Name := tmpStr;
    Delete(Str, 1, Pos(#9, Str)); // Удалим параметр
    Delete(Str, 1, Pos(#9, Str)); // Удалим метку брака

    tmpStr := Trim(Copy(Str, 1, Pos(#9, Str))); // Выделим нижний предел
    if Pos('.', tmpStr) <> 0 then DecimalSeparator := '.'
                             else DecimalSeparator := ',';
    try
      TestsParams[X].Norma.Min := StrToFloat(tmpStr); // Запишем нижний предел
//      DecimalSeparator := ',';
      tmpStr := FormatFloat('0.000', TestsParams[X].Norma.Min);
//      DecimalSeparator := '.';
    except
      TestsParams[X].Norma.Min := -NotSpec;
      tmpStr := tmpStr+'N';
    end;
    Delete(Str, 1, Pos(#9, Str)); // Удалим мин. норму
    Delete(Str, 1, Pos(#9, Str)); // Удалим мин. норму

    try
      TestsParams[X].Norma.Max := StrToFloat(Trim(Copy(Str, 1, Pos(#9, Str)))); // Запишем верхний предел
//      DecimalSeparator := ',';
      tmpStr := tmpStr+';'+FormatFloat('0.000', TestsParams[X].Norma.Max);
//      DecimalSeparator := '.';
    except
      TestsParams[X].Norma.Max := NotSpec;
      tmpStr := tmpStr+';N';
    end;
    TestsParams[X].Status := 2000+X;
//    if TestsParamsSL.Count < Length(TestsParams) then TestsParamsSL.Add(IntToStr(TestsParamsSL.Count)+'='+tmpStr+';'+IntToStr(TestsParams[X].Status));

    PrevChip := NumChip;

    Inc(X);
  end;

  MeasSystem := 'NI';

  X := Ceil(sqrt(TotalChips));
  Y := X;

  SetLength(Chip, 0, 0);
  SetLength(Chip, Y, X);
    for Y := 0 to Length(Chip)-1 do      // Очистим
      for X := 0 to Length(Chip[0])-1 do // массив
      begin                              //
        Chip[Y, X].Status := 2;          //
        Chip[Y, X].ID     := 0;          //
        Chip[Y, X].ShowGr := 0;          //
        SetLength(Chip[Y, X].ChipParams, Length(TestsParams));
      end;                               //
  Direct := 2;

  SL.Free;
end;                                                        //
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
function TWafer.LoadNI2Header: WORD;                        //
var                                                         //
  SL: TStringList;                                          //
  n, TotalChips: DWORD;                                     //
  P: byte;                                                  //
  X, Y: WORD;                                               //
  NumChip, PrevChip, Str, tmpStr, tmpStr2: String;
  FirstTime: Boolean;
begin
  Result := 0;

  SL := TStringList.Create;
  SL.LoadFromFile(fName);

  FirstTime := True;
  X := 0;
  TotalChips := 1;
  for n := 0 to SL.Count-1 do
  begin
    if Trim(SL.Strings[n]) = '' then Continue;
//    if Trim(SL.Strings[n])[1] = '/' then Continue;
    Inc(Result);

    P := Pos('=', SL.Strings[n]);
    if P <> 0 then
    begin
      if Pos('Изделие',       SL.Strings[n]) <> 0 then Device   := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
      if Pos('Дата',          SL.Strings[n]) <> 0 then TimeDate := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
//      if Pos('Время', SL.Strings[n])         <> 0 then TimeDate := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));
      if Pos('Вид испытаний', SL.Strings[n]) <> 0 then info := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))))+' ';
      if Pos('Условия',       SL.Strings[n]) <> 0 then Condition := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))))+' ';
      if Pos('Оператор', SL.Strings[n])      <> 0 then Operator := Trim(Copy(SL.Strings[n], P+1, Length((SL.Strings[n]))));

      Continue;
    end;

    Dec(Result);

    Str := SL.Strings[n];
//    if Pos(',', Str) = 0 then DecimalSeparator := '.'
//                         else DecimalSeparator := ',';
    NumChip := Trim(Copy(Str, 1, Pos(#9, Str)));
    if FirstTime then
    begin
      PrevChip := NumChip;
      FirstTime := False;
    end;
    if NumChip <> PrevChip then
    begin
      X := 0;
      Inc(TotalChips);
    end;
    if X >= Length(TestsParams) then SetLength(TestsParams, X+1);
    tmpStr := '';
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер кристалла
    Delete(Str, 1, Pos(#9, Str)); // Удалим номер теста
    tmpStr := Trim(Copy(Str, 1, Pos(#9, Str))); // Запомним имя параметра
    Delete(Str, 1, Pos(#9, Str)); // Удалим название параметра
//    Delete(Str, 1, Pos(#9, Str)); // Удалим параметр
    tmpStr := tmpStr+' ('+Trim(Copy(Str, 1, Pos(#9, Str)))+')'; // Запомним полное имя параметра
    TestsParams[X].Name := tmpStr;
    Delete(Str, 1, Pos(#9, Str)); // Удалим ед. измерения
    Delete(Str, 1, Pos(#9, Str)); // Удалим параметр
    Delete(Str, 1, Pos(#9, Str)); // Удалим метку брака

    tmpStr := Trim(Copy(Str, 1, Pos(#9, Str))); // Выделим нижний предел
    if Pos('.', tmpStr) <> 0 then DecimalSeparator := '.'
                             else DecimalSeparator := ',';
    try
      TestsParams[X].Norma.Min := StrToFloat(tmpStr); // Запишем нижний предел
//      DecimalSeparator := ',';
      tmpStr := FormatFloat('0.000', TestsParams[X].Norma.Min);
//      DecimalSeparator := '.';
    except
      TestsParams[X].Norma.Min := -NotSpec;
      tmpStr := tmpStr+'N';
    end;
    Delete(Str, 1, Pos(#9, Str)); // Удалим мин. норму
//    Delete(Str, 1, Pos(#9, Str)); // Удалим мин. норму

    P := Pos(#9, Str);
    if P = 0 then P := Length(Str);
    tmpStr2 := Trim(Copy(Str, 1, P)); // Выделим верхний предел
    if Pos('.', tmpStr2) <> 0 then DecimalSeparator := '.'
                              else DecimalSeparator := ',';
    try
      TestsParams[X].Norma.Max := StrToFloat(tmpStr2); // Запишем верхний предел
//      DecimalSeparator := ',';
      tmpStr := tmpStr+';'+FormatFloat('0.000', TestsParams[X].Norma.Max);
//      DecimalSeparator := '.';
    except
      TestsParams[X].Norma.Max := NotSpec;
      tmpStr := tmpStr+';N';
    end;
    TestsParams[X].Status := 2000+X;
//    if TestsParamsSL.Count < Length(TestsParams) then TestsParamsSL.Add(IntToStr(TestsParamsSL.Count)+'='+tmpStr+';'+IntToStr(TestsParams[X].Status));

    PrevChip := NumChip;

    Inc(X);
  end;

  MeasSystem := 'NI';

  X := Ceil(sqrt(TotalChips));
  Y := X;

  SetLength(Chip, 0, 0);
  SetLength(Chip, Y, X);
    for Y := 0 to Length(Chip)-1 do      // Очистим
      for X := 0 to Length(Chip[0])-1 do // массив
      begin                              //
        Chip[Y, X].Status := 2;          //
        Chip[Y, X].ID     := 0;          //
        Chip[Y, X].ShowGr := 0;          //
        SetLength(Chip[Y, X].ChipParams, Length(TestsParams));
      end;                               //
  Direct := 2;

  SL.Free;
end;                                                        //
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
function TWafer.LoadAGLHeader: Boolean;                     //
var                                                         //
  SL, NKKSL, NFCSL: TStringList;                            //
  n, TotalChips: DWORD;                                     //
  P, eP: byte;                                              //
  X, Y, NFC, NKK: WORD;                                     //
  Str, tmpStr1, tmpStr2: String;                            //
begin                                                       //
  Result := True;                                           //

  NKKSL := TStringList.Create;
  NFCSL := TStringList.Create;
  SL := TStringList.Create;
  SL.LoadFromFile(fName);

//  X := 0;
  TotalChips := 0;
  for n := 0 to SL.Count-1 do
  begin
    Str := Trim(SL.Strings[n]);

    if Str = '' then Continue;

    if Pos('TESTFLOW STARTED', UpperCase(Str)) <> 0 then
    begin
      NFC := 0;
      NKK := 0;
      X := 0;
      Inc(TotalChips);

      if TotalChips = 1 then
      begin
        Str := UpperCase(Str);
        P := Pos('ON', Str)+2;
        Str := Trim(Copy(Str, P, Pos('AT', Str)-P));
        TimeDate := Copy(Str, 4, 2)+'.'+Copy(Str, 1, 2)+'.'+Copy(Str, 7, Length(Str)-6);
      end;

      Continue;
    end;

    if Str[1] = '1' then // Если параметр
    begin
      Delete(Str, 1, Pos('`', Str)); // Удалим номер сайта
      P := Pos('`', Str);
      eP := PosEx('`', Str, P+1);
      tmpStr1 := Trim(Copy(Str, 1, P-1));      // Запомним имя параметра
      tmpStr2 := Trim(Copy(Str, P+1, eP-P-1)); // Запомним другое имя параметра

      if (Pos('CONTINUITY', UpperCase(Str)) <> 0) or
         (Pos('CONTAKT',    UpperCase(Str)) <> 0) or
         (Pos('CONTACT',    UpperCase(Str)) <> 0) then
      begin
        if NKK >= NKKSL.Count then
        begin
          NKKSL.Add(IntToStr(10+NKK)+'='+tmpStr2);
          Inc(NKK);
        end;
        Continue;
      end;
      if (Pos('FUNCTIONAL', UpperCase(Str)) <> 0) or
         (Pos('FUNCTION',   UpperCase(Str)) <> 0) or
         (Pos('FK',         UpperCase(Str)) <> 0) then
      begin
        if NFC >= NFCSL.Count then
        begin
          NFCSL.Add(IntToStr(3500+NFC)+'='+tmpStr1);
          Inc(NFC);
        end;
        Continue;
      end;

      Inc(X);
      if X > Length(TestsParams) then // Если не полный список параметров - дополним
      begin
        SetLength(TestsParams, X);
        Delete(Str, 1, Pos('`', Str)); // Удалим название параметра
        //tmpStr := tmpStr+' - '+Trim(Copy(Str, 1, Pos('`', Str)-1)); // Запомним полное имя параметра
        StatusNamesSL.Add(IntToStr(2000+X-1)+'='+Trim(Copy(Str, 1, Pos('`', Str)-1)));
        Delete(Str, 1, Pos('`', Str)); // Удалим полное имя параметра
        TestsParams[X-1].Name := tmpStr1+' - '+tmpStr2; // Запишем полное имя параметра

        Delete(Str, 1, Pos('`', Str)); // Удалим passed/FAILED
        Str := Trim(Str);
        try
          TestsParams[X-1].Norma.Min := StrToFloat(Trim(Copy(Str, 1, Pos(' ', Str)-1))); // Запишем нижний предел
          DecimalSeparator := ',';
          tmpStr1 := FormatFloat('0.000', TestsParams[X-1].Norma.Min);
          DecimalSeparator := '.';
        except
          TestsParams[X-1].Norma.Min := -NotSpec;
          tmpStr1 := tmpStr1+'N';
        end;
        Delete(Str, 1, Pos(' ', Str)); // Удалим нижний предел
        TestsParams[X-1].Name := TestsParams[X-1].Name+' ('+Trim(Copy(Str, 1, Pos(' ', Str)-1)+')'); // Добавим единицу измерения
        Delete(Str, 1, Pos('`', Str)); // Удалим единицу измерения и остальное
        Delete(Str, 1, Pos('`', Str)); // Удалим значение
        Str := Trim(Str);
        Delete(Str, 1, Pos(' ', Str)); // Удалим остальное
        Str := Trim(Str);
        try
          TestsParams[X-1].Norma.Max := StrToFloat(Trim(Copy(Str, 1, Pos(' ', Str)-1))); // Запишем верхний предел
          DecimalSeparator := ',';
          tmpStr1 := tmpStr1+';'+FormatFloat('0.000', TestsParams[X-1].Norma.Max);
          DecimalSeparator := '.';
        except
          TestsParams[X-1].Norma.Max := NotSpec;
          tmpStr1 := tmpStr1+';N';
        end;
        TestsParams[X-1].Status := 2000+X-1;
      end;
    end;
  end;
  if NKKSL.Count > 0 then for n := 0 to NKKSL.Count-1 do StatusNamesSL.Add(NKKSL.Strings[n]);
  if NFCSL.Count > 0 then for n := 0 to NFCSL.Count-1 do StatusNamesSL.Add(NFCSL.Strings[n]);

  MeasSystem := 'Verigy93K';
  Direct := 2;

  X := Ceil(sqrt(TotalChips));
  Y := X;

  SetLength(Chip, 0, 0);
  SetLength(Chip, Y, X);
    for Y := 0 to Length(Chip)-1 do      // Очистим
      for X := 0 to Length(Chip[0])-1 do // массив
      begin                              //
        Chip[Y, X].Status := 2;          //
        Chip[Y, X].ID     := 0;          //
        Chip[Y, X].ShowGr := 0;          //
        SetLength(Chip[Y, X].ChipParams, Length(TestsParams));
      end;                               //

  SL.Free;
  NKKSL.Free;
  NFCSL.Free;
end;

//////////////////////////////////////////////////////////////
function TWafer.AddAGLHeader: Boolean;                      //
var                                                         //
  SL, NKKSL, NFCSL: TStringList;                            //
  n, TotalChips: DWORD;                                     //
  P, eP: byte;                                              //
  X, Y, NFC, NKK: WORD;                                     //
  Str, tmpStr1, tmpStr2: String;                            //
begin                                                       //
  Result := True;                                           //

  NKKSL := TStringList.Create;
  NFCSL := TStringList.Create;
  SL := TStringList.Create;
  SL.LoadFromFile(fName);


//  X := 0;
  TotalChips := 0;
  for n := 0 to SL.Count-1 do
  begin
    Str := Trim(SL.Strings[n]);

    if Str = '' then Continue;

    if Pos('TESTFLOW STARTED', UpperCase(Str)) <> 0 then
    begin
      NFC := 0;
      NKK := 0;
      X := 0;
      Inc(TotalChips);

      if TotalChips = 1 then
      begin
        Str := UpperCase(Str);
        P := Pos('ON', Str)+2;
        Str := Trim(Copy(Str, P, Pos('AT', Str)-P));
        TimeDate := Copy(Str, 4, 2)+'.'+Copy(Str, 1, 2)+'.'+Copy(Str, 7, Length(Str)-6);
      end;

      Continue;
    end;

    if Str[1] = '1' then // Если параметр
    begin
      Delete(Str, 1, Pos('`', Str)); // Удалим номер сайта
      P := Pos('`', Str);
      eP := PosEx('`', Str, P+1);
      tmpStr1 := Trim(Copy(Str, 1, P-1));      // Запомним имя параметра
      tmpStr2 := Trim(Copy(Str, P+1, eP-P-1)); // Запомним другое имя параметра

      if (Pos('CONTINUITY', UpperCase(Str)) <> 0) or
         (Pos('CONTAKT',    UpperCase(Str)) <> 0) or
         (Pos('CONTACT',    UpperCase(Str)) <> 0) then
      begin
        if NKK >= NKKSL.Count then
        begin
          NKKSL.Add(IntToStr(10+NKK)+'='+tmpStr2);
          Inc(NKK);
        end;
        Continue;
      end;
      if (Pos('FUNCTIONAL', UpperCase(Str)) <> 0) or
         (Pos('FUNCTION',   UpperCase(Str)) <> 0) or
         (Pos('FK',         UpperCase(Str)) <> 0) then
      begin
        if NFC >= NFCSL.Count then
        begin
          NFCSL.Add(IntToStr(3500+NFC)+'='+tmpStr1);
          Inc(NFC);
        end;
        Continue;
      end;

      Inc(X);
      if X > Length(TestsParams) then // Если не полный список параметров - дополним
      begin
        SetLength(TestsParams, X);
        Delete(Str, 1, Pos('`', Str)); // Удалим название параметра
        //tmpStr := tmpStr+' - '+Trim(Copy(Str, 1, Pos('`', Str)-1)); // Запомним полное имя параметра
        StatusNamesSL.Add(IntToStr(2000+X-1)+'='+Trim(Copy(Str, 1, Pos('`', Str)-1)));
        Delete(Str, 1, Pos('`', Str)); // Удалим полное имя параметра
        TestsParams[X-1].Name := tmpStr2; // Запишем полное имя параметра

        Delete(Str, 1, Pos('`', Str)); // Удалим passed/FAILED
        Str := Trim(Str);
        try
          TestsParams[X-1].Norma.Min := StrToFloat(Trim(Copy(Str, 1, Pos(' ', Str)-1))); // Запишем нижний предел
          DecimalSeparator := ',';
          tmpStr1 := FormatFloat('0.000', TestsParams[X-1].Norma.Min);
          DecimalSeparator := '.';
        except
          TestsParams[X-1].Norma.Min := -NotSpec;
          tmpStr1 := tmpStr1+'N';
        end;
        Delete(Str, 1, Pos(' ', Str)); // Удалим нижний предел
        TestsParams[X-1].Name := TestsParams[X-1].Name+' ('+Trim(Copy(Str, 1, Pos(' ', Str)-1)+')'); // Добавим единицу измерения
        Delete(Str, 1, Pos('`', Str)); // Удалим единицу измерения и остальное
        Delete(Str, 1, Pos('`', Str)); // Удалим значение
        Str := Trim(Str);
        Delete(Str, 1, Pos(' ', Str)); // Удалим остальное
        Str := Trim(Str);
        try
          TestsParams[X-1].Norma.Max := StrToFloat(Trim(Copy(Str, 1, Pos(' ', Str)-1))); // Запишем верхний предел
          DecimalSeparator := ',';
          tmpStr1 := tmpStr1+';'+FormatFloat('0.000', TestsParams[X-1].Norma.Max);
          DecimalSeparator := '.';
        except
          TestsParams[X-1].Norma.Max := NotSpec;
          tmpStr1 := tmpStr1+';N';
        end;
        TestsParams[X-1].Status := 2000+X-1;
      end;
    end;
  end;
  if NKKSL.Count > 0 then for n := 0 to NKKSL.Count-1 do StatusNamesSL.Add(NKKSL.Strings[n]);
  if NFCSL.Count > 0 then for n := 0 to NFCSL.Count-1 do StatusNamesSL.Add(NFCSL.Strings[n]);

  MeasSystem := 'Verigy93K';

//  MessageBox(0, PAnsiChar(IntToStr(Length(TestsParams))), PAnsiChar(IntToStr(Length(TestsParams))), 0);

  for Y := 0 to Length(Chip)-1 do      // Очистим
    for X := 0 to Length(Chip[0])-1 do // массив
    begin                              // от брака
      if Chip[Y, X].Status > 9 then Chip[Y, X].Status := 0;
      SetLength(Chip[Y, X].ChipParams, Length(TestsParams));
    end;

  SL.Free;
  NKKSL.Free;
  NFCSL.Free;
end;



////////////////////////////////////////////////////////////////////////
procedure TWafer.Normalize;                                           //
label                                                                 //
  X0_End, X1_End, Y0_End, Y1_End;                                     //
var                                                                   //
  tmpChip: TChips;                                                    //
  X, Y, X0, X1, Y0, Y1: WORD;                                         //
begin                                                                 //
  X0 := 0;                                                            //
  for X := 0 to Length(Chip[0])-1 do                                  //
    for Y := 0 to Length(Chip)-1 do                                   //
      if Chip[Y, X].Status <> 2 then                                  //
      begin                                                           //
        X0 := X;                                                      //
        Goto X0_End;                                                  //
      end;                                                            //
X0_End:                                                               //
                                                                      //
  X1 := 0;                                                            //
  for X := Length(Chip[0])-1 downto 0 do                              //
    for Y := 0 to Length(Chip)-1 do                                   //
      if Chip[Y, X].Status <> 2 then                                  //
      begin                                                           //
        X1 := X;                                                      //
        Goto X1_End;                                                  //
      end;                                                            //
X1_End:                                                               //
                                                                      //
  Y0 := 0;                                                            //
  for Y := 0 to Length(Chip)-1 do                                     //
    for X := 0 to Length(Chip[0])-1 do                                //
      if Chip[Y, X].Status <> 2 then                                  //
      begin                                                           //
        Y0 := Y;                                                      //
        Goto Y0_End;                                                  //
      end;                                                            //
Y0_End:                                                               //
                                                                      //
  Y1 := 0;                                                            //
  for Y := Length(Chip)-1 downto 0 do                                 //
    for X := 0 to Length(Chip[0])-1 do                                //
      if Chip[Y, X].Status <> 2 then                                  //
      begin                                                           //
        Y1 := Y;                                                      //
        Goto Y1_End;                                                  //
      end;                                                            //
Y1_End:                                                               //
                                                                      //
  SetLength(tmpChip, Y1-Y0+1, X1-X0+1);                               //
  for Y := Y0 to Y1 do                                                //
    for X := X0 to X1 do                                              //
    begin                                                             //
      tmpChip[Y-Y0, X-X0] := Chip[Y, X];                              //
//      SetLength(tmpChip[Y-Y0, X-X0].Value, Length(Chip[Y, X].Value)); //
//      if Length(tmpChip[Y-Y0, X-X0].Value) > 0 then                   //
//        for n := 0 to Length(tmpChip[Y-Y0, X-X0].Value)-1 do          //
//          tmpChip[Y-Y0, X-X0].Value[n] := Chip[Y, X].Value[n];        //
    end;                                                              //
                                                                      //
  Chip := tmpChip;                                                    //
//  SetLength(Chip, Length(TmpChip), Length(TmpChip[0]));               //
//  tmpChip := nil;                                                     //
                                                                      //
  BaseChip.X := BaseChip.X-X0;                                        //
  BaseChip.Y := BaseChip.Y-Y0;                                        //
end;                                                                  //
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////
procedure TWafer.Rotate;                                                              //
var                                                                                   //
  X, Y: WORD;                                                                         //
  TmpChip: TChips;                                                                    //
  TmpInt: Integer;                                                                    //
  TmpSingle: Single;                                                                  //
begin                                                                                 //
  TmpInt := BaseChip.X;                                                               //
  BaseChip.X := BaseChip.Y;                                                           //
  BaseChip.Y := Length(Chip[0])-TmpInt-1;                                             //
                                                                                      //
  TmpInt := Cadre.StartX;                                                             //
  Cadre.StartX := Cadre.StartY;                                                       //
  if Cadre.ScaleX <> 0 then Cadre.StartY := (Length(Chip[0])-TmpInt) mod Cadre.ScaleX //
                       else Cadre.StartY := 0;                                        //
                                                                                      //
  TmpInt := Cadre.ScaleX;                                                             //
  Cadre.ScaleX := Cadre.ScaleY;                                                       //
  Cadre.ScaleY := TmpInt;                                                             //
                                                                                      //
  SetLength(TmpChip, Length(Chip[0]), Length(Chip));                                  //
                                                                                      //
  for Y := 0 to Length(Chip)-1 do                                                     //
    for X := 0 to Length(Chip[0])-1 do                                                //
    begin                                                                             //
      TmpChip[Length(Chip[0])-X-1, Y] := Chip[Y, X];                                  //
      SetLength(TmpChip[Length(Chip[0])-X-1, Y].ChipParams, Length(Chip[Y, X].ChipParams));     //
    end;                                                                              //
                                                                                      //
  Chip := TmpChip;                                                                    //
  SetLength(Chip, Length(TmpChip), Length(TmpChip[0]));                               //
  TmpChip := nil;                                                                     //
                                                                                      //
  if CutSide <> 0 then                                                                //
    if CutSide < 4 then Inc(CutSide) else CutSide := 1;                               //
                                                                                      //
  if Direct > 11 then Direct := Direct-12                                             //
                 else Inc(Direct, 4);                                                 //
                                                                                      //
  if (StepX <> 0) and (StepY <> 0) then                                               //
  begin                                                                               //
    TmpSingle := StepX;                                                               //
    StepX := StepY;                                                                   //
    StepY := TmpSingle;                                                               //
  end;                                                                                //
                                                                                      //
  SetChipsID;                                                                         //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////
procedure TWafer.CalcChips;                                                                  //
                                                                                             //
///////////////////////////////////////////////////                                          //
  function GetPDCoord(const X, Y: WORD): TPoint; //                                          //
  begin                                          //                                          //
    with Cadre do                                //                                          //
    begin                                        //                                          //
      Result.X := (X+ScaleX-StartX) mod ScaleX;  //                                          //
      Result.Y := (Y+ScaleY-StartY) mod ScaleY;  //                                          //
    end;                                         //                                          //
  end;                                           //                                          //
///////////////////////////////////////////////////                                          //
                                                                                             //
//////////////////////////////////////////                                                   //
procedure SortFails(var Fails: TFails); //                                                   //
var                                     //                                                   //
  n, m, b_val, b_m: WORD;               //                                                   //
  TmpFail: TFail;                       //                                                   //
begin                                   //                                                   //
  if Length(Fails) < 2 then Exit;       //                                                   //
                                        //                                                   //
  for n := 0 to Length(Fails)-2 do      //                                                   //
  begin                                 //                                                   //
    b_val := Fails[n].Status;           //                                                   //
    b_m := n;                           //                                                   //
    for m := n+1 to Length(Fails)-1 do  //                                                   //
      if Fails[m].Status < b_val then   //                                                   //
      begin                             //                                                   //
        b_val := Fails[m].Status;       //                                                   //
        b_m := m;                       //                                                   //
      end;                              //                                                   //
    TmpFail := Fails[b_m];              //                                                   //
    Fails[b_m] := Fails[n];             //                                                   //
    Fails[n] := TmpFail;                //                                                   //
  end;                                  //                                                   //
end;                                    //                                                   //
//////////////////////////////////////////                                                   //
                                                                                             //
var                                                                                          //
  X, Y, n: WORD;                                                                             //
  TmpPoint: TPoint;                                                                          //
  PDFlag, flS, flF: Boolean;                                                                 //
begin                                                                                        //
  if Length(Chip) = 0 then Exit;

  SetLength(PD, 0, 0);                                                                       //
  PDFlag := False;                                                                           //
  if not ((Cadre.ScaleX = 0) or (Cadre.ScaleY = 0)) then                                     //
  begin                                                                                      //
    PDFlag := True;                                                                          //
    SetLength(PD, Cadre.ScaleY, Cadre.ScaleX);                                               //
    for Y := 0 to Length(PD)-1 do                                                            //
      for X := 0 to Length(PD[0])-1 do                                                       //
      begin                                                                                  //
        PD[Y, X].OK   := 0;                                                                  //
        PD[Y, X].Fail := 0;                                                                  //
        PD[Y, X].Meas := False;                                                              //
      end;                                                                                   //
  end;                                                                                       //
                                                                                             //
  NOK     := 0;                                                                              //
  NFailNC := 0;                                                                              //
  NFailSC := 0;                                                                              //
  NFailFC := 0;                                                                              //
  NTotal  := 0;                                                                              //
  NMeased := 0;                                                                              //
  for Y := 0 to Length(Chip)-1 do                                                            //
    for X := 0 to Length(Chip[0])-1 do                                                       //
    begin                                                                                    //
      if not (Chip[Y, X].Status in [2,3,5]) then Inc(NTotal);                                //
                                                                                             //
      case Chip[Y, X].Status of                                                              //
        1         : begin                                                                    //
                      Inc(NOK);                                                              //
                      Inc(NMeased);                                                          //
                                                                                             //
                      if PDFlag then                                                         //
                      begin                                                                  //
                        TmpPoint := GetPDCoord(X, Y);                                        //
                        Inc(PD[TmpPoint.Y, TmpPoint.X].OK);                                  //
                        PD[TmpPoint.Y, TmpPoint.X].Meas := True;                             //
                      end;                                                                   //
                    end;                                                                     //
                                                                                             //
        10..1500  : begin                                                                    //
                      Inc(NFailNC);                                                          //
                      Inc(NMeased);                                                          //
                                                                                             //
                      if PDFlag then                                                         //
                      begin                                                                  //
                        TmpPoint := GetPDCoord(X, Y);                                        //
                        Inc(PD[TmpPoint.Y, TmpPoint.X].Fail);                                //
                        PD[TmpPoint.Y, TmpPoint.X].Meas := True;                             //
                      end;                                                                   //
                    end;                                                                     //
                                                                                             //
        2000..3000: begin                                                                    //
                      Inc(NFailSC);                                                          //
                      Inc(NMeased);                                                          //
                                                                                             //
                      flS := True;                                                           //
                      if Length(FailsSC) > 0 then                                            //
                        for n := 0 to Length(FailsSC)-1 do                                   //
                          if FailsSC[n].Status = Chip[Y, X].Status then                      //
                          begin                                                              //
                            flS := False;                                                    //
                            Inc(FailsSC[n].Quantity);                                        //
                            Break;                                                           //
                          end;                                                               //
                                                                                             //
                      if flS then                                                            //
                      begin                                                                  //
                        SetLength(FailsSC, Length(FailsSC)+1);                               //
                        FailsSC[Length(FailsSC)-1].Status := Chip[Y, X].Status;              //
                        FailsSC[Length(FailsSC)-1].Quantity := 1;                            //
                        FailsSC[Length(FailsSC)-1].Name := GetStatusName(Chip[Y, X].Status); //
                        FailsSC[Length(FailsSC)-1].Col := clFailSC;                          //
                      end;                                                                   //
                                                                                             //
                      if PDFlag then                                                         //
                      begin                                                                  //
                        TmpPoint := GetPDCoord(X, Y);                                        //
                        Inc(PD[TmpPoint.Y, TmpPoint.X].Fail);                                //
                        PD[TmpPoint.Y, TmpPoint.X].Meas := True;                             //
                      end;                                                                   //
                    end;                                                                     //
                                                                                             //
        3500..4500: begin                                                                    //
                      Inc(NFailFC);                                                          //
                      Inc(NMeased);                                                          //
                                                                                             //
                      flF := True;                                                           //
                      if Length(FailsFC) > 0 then                                            //
                        for n := 0 to Length(FailsFC)-1 do                                   //
                          if FailsFC[n].Status = Chip[Y, X].Status then                      //
                          begin                                                              //
                            flF := False;                                                    //
                            Inc(FailsFC[n].Quantity);                                        //
                            Break;                                                           //
                          end;                                                               //
                                                                                             //
                      if flF then                                                            //
                      begin                                                                  //
                        SetLength(FailsFC, Length(FailsFC)+1);                               //
                        FailsFC[Length(FailsFC)-1].Status := Chip[Y, X].Status;              //
                        FailsFC[Length(FailsFC)-1].Quantity := 1;                            //
                        FailsFC[Length(FailsFC)-1].Name := GetStatusName(Chip[Y, X].Status); //
                        FailsFC[Length(FailsFC)-1].Col := clFailFC;                          //
                      end;                                                                   //
                                                                                             //
                      if PDFlag then                                                         //
                      begin                                                                  //
                        TmpPoint := GetPDCoord(X, Y);                                        //
                        Inc(PD[TmpPoint.Y, TmpPoint.X].Fail);                                //
                        PD[TmpPoint.Y, TmpPoint.X].Meas := True;                             //
                      end;                                                                   //
                    end;                                                                     //
      end;                                                                                   //
    end;                                                                                     //
                                                                                             //
  if Length(FailsSC) > 1 then SortFails(FailsSC);                                            //
  if Length(FailsFC) > 1 then SortFails(FailsFC);                                            //
end;                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////
procedure TWafer.SetChipsID;                                                      //
var                                                                               //
  N: DWORD;                                                                       //
  X, Y, XY: WORD;                                                                 //
  tmp: byte;                                                                      //
  MassXY: array of WORD; // Массив не пустых строк                                //
begin                                                                             //
  if Length(Chip) = 0 then Exit;

  N := 0;                                                                         //
                                                                                  //
  if Direct in [0,1,2,3,8,9,10,11] then // Горизонтальный обход                   //
  begin                                                                           //
    SetLength(MassXY, Length(Chip));                                              //
    for Y := 0 to Length(Chip)-1 do                                               //
      for X := 0 to Length(Chip[0])-1 do                                          //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          if N = 0 then                                                           //
          begin                                                                   //
            MassXY[N] := Y;                                                       //
            Inc(N);                                                               //
          end                                                                     //
          else                                                                    //
            if MassXY[N-1] <> Y then                                              //
            begin                                                                 //
              MassXY[N] := Y;                                                     //
              Inc(N);                                                             //
            end;                                                                  //
        end;                                                                      //
  end                                                                             //
  else                                  // Вертикальный обход                     //
  begin                                                                           //
    SetLength(MassXY, Length(Chip[0]));                                           //
    for X := 0 to Length(Chip[0])-1 do                                            //
      for Y := 0 to Length(Chip)-1 do                                             //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          if N = 0 then                                                           //
          begin                                                                   //
            MassXY[N] := X;                                                       //
            Inc(N);                                                               //
          end                                                                     //
          else                                                                    //
            if MassXY[N-1] <> X then                                              //
            begin                                                                 //
              MassXY[N] := X;                                                     //
              Inc(N);                                                             //
            end;                                                                  //
        end;                                                                      //
  end;                                                                            //
                                                                                  //
  SetLength(MassXY, N);                                                           //
                                                                                  //
  N := 0;                                                                         //
  SetLength(ChipN, 0);                                                            //
  SetLength(ChipN, Length(Chip[0])*Length(Chip));                                 //
                                                                                  //
////////////////////////// * Справа налево (сверху) * //////////////////////////////
                                                                                  //
  if Direct = dURightToLeft then                                                  //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      for X := Length(Chip[0])-1 downto 0 do                                      //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
/////////////////////////// * Слева направо (сверху) * /////////////////////////////
                                                                                  //
  if Direct = dULeftToRight then                                                  //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      for X := 0 to Length(Chip[0])-1 do                                          //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
////////////////////// * Правая и левая змейки (сверху) * //////////////////////////
                                                                                  //
  if Direct in [dURightSnake, dULeftSnake] then                                   //
  begin                                                                           //
    if Direct = dURightSnake then tmp := 1                                        //
                             else tmp := 0;                                       //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      if (XY mod 2) = tmp then                                                    //
      begin                                                                       //
        for X := 0 to Length(Chip[0])-1 do                                        //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
      end                                                                         //
      else                                                                        //
        for X := Length(Chip[0])-1 downto 0 do                                    //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
    end;                                                                          //
  end;                                                                            //
                                                                                  //
////////////////////////////////////////////////////////////////////////////////////
                                                                                  //
//////////////////////////// * Сверху вниз (слева) * ///////////////////////////////
                                                                                  //
  if Direct = dLUpToDown then                                                     //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      for Y := 0 to Length(Chip)-1 do                                             //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
//////////////////////////// * Снизу вверх (слева) * ///////////////////////////////
                                                                                  //
  if Direct = dLDownToUp then                                                     //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      for Y := Length(Chip)-1 downto 0 do                                         //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
////////////////////// * Верхняя и нижняя змейки (слева) * /////////////////////////
                                                                                  //
  if Direct in [dLUpSnake, dLDownSnake] then                                      //
  begin                                                                           //
    if Direct = dLUpSnake then tmp := 0                                           //
                          else tmp := 1;                                          //
    for XY := 0 to Length(MassXY)-1 do                                            //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      if (XY mod 2) = tmp then                                                    //
      begin                                                                       //
        for Y := 0 to Length(Chip)-1 do                                           //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
      end                                                                         //
      else                                                                        //
        for Y := Length(Chip)-1 downto 0 do                                       //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
    end;                                                                          //
  end;                                                                            //
                                                                                  //
////////////////////////////////////////////////////////////////////////////////////
                                                                                  //
/////////////////////////// * Справа налево (снизу) * //////////////////////////////
                                                                                  //
  if Direct = dDRightToLeft then                                                  //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      for X := Length(Chip[0])-1 downto 0 do                                      //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
/////////////////////////// * Слева направо (снизу) * //////////////////////////////
                                                                                  //
  if Direct = dDLeftToRight then                                                  //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      for X := 0 to Length(Chip[0])-1 do                                          //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
/////////////////////// * Правая и левая змейки (снизу) * //////////////////////////
                                                                                  //
  if Direct in [dDRightSnake, dDLeftSnake] then                                   //
  begin                                                                           //
    if Direct = dDRightSnake then tmp := 0                                        //
                             else tmp := 1;                                       //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      Y := MassXY[XY];                                                            //
      if (XY mod 2) = tmp then                                                    //
      begin                                                                       //
        for X := 0 to Length(Chip[0])-1 do                                        //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
      end                                                                         //
      else                                                                        //
        for X := Length(Chip[0])-1 downto 0 do                                    //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
    end;                                                                          //
  end;                                                                            //
                                                                                  //
////////////////////////////////////////////////////////////////////////////////////
                                                                                  //
/////////////////////////// * Сверху вниз (справа) * ///////////////////////////////
                                                                                  //
  if Direct = dRUpToDown then                                                     //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      for Y := 0 to Length(Chip)-1 do                                             //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
//////////////////////////// * Снизу вверх (справа) * //////////////////////////////
                                                                                  //
  if Direct = dRDownToUp then                                                     //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      for Y := Length(Chip)-1 downto 0 do                                         //
        if IsChip(Chip[Y, X].Status) then                                         //
        begin                                                                     //
          Inc(N);                                                                 //
          Chip[Y, X].ID := N; // номер кристалла                                  //
          ChipN[N-1] := Point(X, Y);                                              //
        end;                                                                      //
    end;                                                                          //
                                                                                  //
////////////////////// * Верхняя и нижняя змейки (справа) * ////////////////////////
                                                                                  //
  if Direct in [dRUpSnake, dRDownSnake] then                                      //
  begin                                                                           //
    if Direct = dRUpSnake then tmp := 1                                           //
                          else tmp := 0;                                          //
    for XY := Length(MassXY)-1 downto 0 do                                        //
    begin                                                                         //
      X := MassXY[XY];                                                            //
      if (XY mod 2) = tmp then                                                    //
      begin                                                                       //
        for Y := 0 to Length(Chip)-1 do                                           //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
      end                                                                         //
      else                                                                        //
        for Y := Length(Chip)-1 downto 0 do                                       //
          if IsChip(Chip[Y, X].Status) then                                       //
          begin                                                                   //
            Inc(N);                                                               //
            Chip[Y, X].ID := N; // номер кристалла                                //
            ChipN[N-1] := Point(X, Y);                                            //
          end;                                                                    //
    end;                                                                          //
  end;                                                                            //
                                                                                  //
  SetLength(ChipN, N);                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////
function TWafer.IsWafer: Boolean; //
begin                             //
  Result := Diameter <> 0;        //
end;                              //
////////////////////////////////////


///////////////////////////////////////////////////////////////////////
function TStatistica.GetChipParamsStat(Val, Min, Max: Single): byte; //
begin                                                                //
//  Result := 0;                                                       //
                                                                     //
//  if Val <> NotSpec then                                             //
  begin                                                              //
    Result := 1;                                                     //
                                                                     //
    if (Min = NotSpec)  and (Max = NotSpec) then Result := 0;        //
                                                                     //
    if (Min = NotSpec)  and (Max <> NotSpec) then                    //
      if Val > Max then Result := 3;                                 //
                                                                     //
    if (Min <> NotSpec) and (Max = NotSpec)  then                    //
      if Val < Min then Result := 2;                                 //
                                                                     //
    if (Min <> NotSpec) and (Max <> NotSpec) then                    //
    begin                                                            //
      if Val < Min then Result := 2;                                 //
      if Val > Max then Result := 3;                                 //
    end;                                                             //
  end                                                                //
end;                                                                 //
///////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////
function TWafer.GetStatusName(const Status: WORD): String;         //
var                                                                //
  n, Tmp: WORD;                                                    //
  P: byte;                                                         //
begin                                                              //
  Result := '';                                                    //
                                                                   //
  if StatusNamesSL.Count = 0 then Exit;                            //
                                                                   //
  with StatusNamesSL do                                            //
    for n := 0 to Count-1 do                                       //
      if Trim(Strings[n]) <> '' then                               //
      begin                                                        //
        P := Pos('=', Strings[n]);                                 //
        if P <> 0 then                                             //
        begin                                                      //
          try                                                      //
            Tmp := StrToInt(Trim(Copy(Strings[n], 1, P-1)));       //
          except                                                   //
            Continue;                                              //
          end;                                                     //
          if Status = Tmp then                                     //
          begin                                                    //
            Result := Copy(Strings[n], P+1, Length(Strings[n])-P); //
            Break;                                                 //
          end;                                                     //
        end;                                                       //
      end;                                                         //
end;                                                               //
/////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


{ TStatistica }

//////////////////////////////////////////////////////
constructor TStatistica.Create(AOwner: TComponent); //
begin                                               //
  inherited Create(AOwner);                         //
                                                    //
  Parent := TWinControl(AOwner);                    //
  Top    := 0;                                      //
  Left   := 154;                                    //
  Width  := 300;                                    //
  Height := 300;                                    //
  Color := clSilver;                                //
  DoubleBuffered := True;                           //
  ControlStyle := ControlStyle+[csOpaque];          //
                                                    //
  fSizeChipX := 0;                                  //
  fSizeChipY := 0;                                  //
                                                    //
  PBox := TPaintBox.Create(self);                   //
  with PBox do                                      //
  begin                                             //
    Parent := self;                                 //
    Top  := 0;                                      //
    Left := 0;                                      //
    Width  := self.Width;                           //
    Height := self.Height;                          //
    ControlStyle := ControlStyle+[csOpaque];        //
                                                    //
    OnMouseDown := PBoxMouseDown;                   //
    OnPaint     := PBoxPaint;                       //
  end;                                              //
  WBitmap := TBitmap.Create;                        //
                                                    //
  LastShowMode := 0;                                //
  clShowChips := clFuchsia;                         //
end;                                                //
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
destructor TStatistica.Destroy;                     //
begin                                               //
  if Wafer <> nil then Wafer.Free;                  //
  WBitmap.Free;                                     //
  PBox.Free;                                        //
  if ChipsDlg <> nil then ChipsDlg.Free;            //
                                                    //

  inherited;                                        //
end;                                                //
//////////////////////////////////////////////////////

////////////////////////////////////////////
procedure TStatistica.Init;               //
begin                                     //
  if Wafer <> nil then FreeAndNil(Wafer); //
                                          //
  WBitmap.Canvas.Brush.Color := Color;    //
  WBitmap.Width  := 0;                    //
  WBitmap.Height := 0;                    //
                                          //
  Repaint;                                //
end;                                      //
////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadSTS(const STSfName: TFileName): Boolean;                                    //
var                                                                                                  //
  i, X, Y: WORD;                                                                                     //
  n, Count: DWORD;                                                                                   //
                                                                                                     //
  SL: TStringList;                                                                                   //
  Str, S: String;                                                                                    //
  P: byte;                                                                                           //
  Mass: array of Single;                                                                             //
  Stat: WORD;                                                                                        //
begin                                                                                                //
  Result := False;                                                                                   //
                                                                                                     //
  if Wafer <> nil then FreeAndNil(Wafer);                                                            //
  Wafer := TWafer.Create;                                                                            //
  Wafer.fName := STSfName;                                                                           //
                                                                                                     //
  if not Wafer.LoadSTSHeader then                                                                    //
  begin                                                                                              //
    ErrMess(Handle, 'Ошибка загрузки заголовка!');                                                   //
    Init;                                                                                            //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  SL := TStringList.Create;                                                                          //
  SL.LoadFromFile(STSfName);                                                                         //
                                                                                                     //
  Count := 0;                                                                                        //
  n := 0;                                                                                            //
  while (Trim(SL.Strings[0]) <> '[ChipsParams]') do                                                  //
  begin                                                                                              //
    if SL.Count = 1 then                                                                             //
    begin                                                                                            //
      ErrMess(Handle, 'Не найдено поле [ChipsParams]!');                                             //
      Init;                                                                                          //
      SL.Free;                                                                                       //
      Exit;                                                                                          //
    end;                                                                                             //
    SL.Delete(0);                                                                                    //
    Inc(Count);                                                                                      //
  end;                                                                                               //
  SL.Delete(0);         //                                                                           //
  Inc(Count);           //                                                                           //
  Str := SL.Strings[0]; // Удалим поле [ChipsParams]                                                 //
  SL.Delete(0);         //                                                                           //
  Inc(Count);           //                                                                           //
                                                                                                     //
  if SL.Count = 0 then                                                                               //
  begin                                                                                              //
    ErrMess(Handle, 'Обход пустой!');                                                                //
    Init;                                                                                            //
    SL.Free;                                                                                         //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  try                                                                                                //
    P := Pos(#9, Str);                                                                               //
    S := Copy(Str, 1, P-1);                                                                          //
    n := 0;                                                                                          //
    while Trim(S) <> 'Status' do                        //                                           //
    begin                                               //                                           //
      Wafer.TestsParams[n].Name := S;                   //                                           //
      Inc(n);                                           // Считываем                                 //
      Delete(Str, 1, P);                                // название                                  //
      P := Pos(#9, Str);                                // столбцов,                                 //
      S := Copy(Str, 1, P-1);                           //                                           //
      if S = '' then                                    //                                           //
      begin                                             //                                           //
        ErrMess(Handle, 'Не найден столбец статуса !'); //                                           //
        SL.Free;                                        //                                           //
        Exit;                                           //                                           //
      end;                                              //                                           //
    end;                                                //                                           //
    SetLength(Mass, Length(Wafer.TestsParams));                                                      //
                                                                                                     //
    DecimalSeparator := ',';                                                                         //
                                                                                                     //
    if SL.Count > 0 then                                                                             //
      for n := 0 to SL.Count-1 do                                                                    //
      begin                                                                                          //
        Str := SL.Strings[n];                                                                        //
        if Trim(Str) = '' then Continue; // Пропустим пустую строку                                  //
        if Length(Wafer.TestsParams) > 0 then                                                        //
          for i := 0 to Length(Wafer.TestsParams)-1 do                                               //
          begin                                                                                      //
            P := Pos(#9, Str);                                                                       //
            S := Copy(Str, 1, P-1);                                                                  //
            Mass[i] := StrToFloat(S);                                                                //
            Delete(Str, 1, P);                                                                       //
          end;                                                                                       //
                                                                                                     //
        P := Pos(#9, Str);                                                                           //
        S := Copy(Str, 1, P-1);                                                                      //
        Stat := StrToInt(S);                                                                         //
        Delete(Str, 1, P);                                                                           //
                                                                                                     //
        P := Pos(#9, Str);                                                                           //
        S := Copy(Str, 1, P-1);                                                                      //
        X := StrToInt(S);                                                                            //
        Delete(Str, 1, P);                                                                           //
        if X > (Length(Wafer.Chip[0])-1) then SetLength(Wafer.Chip[0], X+1);                         //
                                                                                                     //
        Y := StrToInt(Str);                                                                          //
        Delete(Str, 1, P);                                                                           //
        if Y > (Length(Wafer.Chip)-1) then SetLength(Wafer.Chip, Y+1);                               //
                                                                                                     //
        Wafer.Chip[Y, X].Status := Stat;                                                             //
                                                                                                     //
        SetLength(Wafer.Chip[Y, X].ChipParams, Length(Wafer.TestsParams));                           //
        if Length(Wafer.TestsParams) > 0 then                                                        //
          for i := 0 to Length(Wafer.TestsParams)-1 do                                               //
          begin                                                                                      //
            Wafer.Chip[Y, X].ChipParams[i].Value := Mass[i];
            Wafer.Chip[Y, X].ChipParams[i].Stat  := GetChipParamsStat(Mass[i], Wafer.TestsParams[i].Norma.Min, Wafer.TestsParams[i].Norma.Max);
          end;
                                                                                                     //
        if Trim(SL.Strings[n]) = '' then Break;                                                      //
      end;                                                                                           //
                                                                                                     //
      DecimalSeparator := '.';                                                                       //
                                                                                                     //
  except                                                                                             //
    ErrMess(Handle, 'Ошибка в строке '+IntToStr(n+Count+1));                                         //
    Init;                                                                                            //
    SL.Free;                                                                                         //
    DecimalSeparator := '.';                                                                         //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  Result := True;                                                                                    //
                                                                                                     //
  Wafer.SetChipsID;                                                                                  //
  Wafer.CalcChips;                                                                                   //
                                                                                                     //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                      //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                            //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                           //
                                                                                                     //
  fSizeChipX := 0;                                                                                   //
  fSizeChipY := 0;                                                                                   //
  DrawWafer;                                                                                         //
  PBox.Repaint;                                                                                      //
                                                                                                     //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                //
                                                                                                     //
  SL.Free;                                                                                           //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.AddSTS(const STSfName: TFileName): Boolean;                                     //
var                                                                                                  //
  i, X, Y: WORD;                                                                                     //
  n, Count: DWORD;                                                                                   //
  SL: TStringList;                                                                                   //
  Str, S: String;                                                                                    //
  P: byte;                                                                                           //
  Mass: array of Single;                                                                             //
  Stat: WORD;                                                                                        //
  tmpWafer: TWafer;                                                                                  //
begin                                                                                                //
  Result := False;                                                                                   //
                                                                                                     //
  if Wafer = nil then Exit;                                                                          //
                                                                                                     //
  tmpWafer := TWafer.Create;                                                                         //
  tmpWafer.fName := STSfName;                                                                        //
                                                                                                     //
  DecimalSeparator := ',';                                                                           //
                                                                                                     //
  if not tmpWafer.LoadSTSHeader then                                                                 //
  begin                                                                                              //
    ErrMess(Handle, 'Ошибка загрузки заголовка!');                                                   //
    tmpWafer.Free;                                                                                   //
    Exit;                                                                                            //
  end;                                                                                               //
  if tmpWafer.Code <> Wafer.Code then                                                                //
  begin                                                                                              //
    ErrMess(Handle, 'Несовпадают коды!');                                                            //
    tmpWafer.Free;                                                                                   //
    Exit;                                                                                            //
  end;                                                                                               //
  if tmpWafer.NLot <> Wafer.NLot then                                                                //
  begin                                                                                              //
    ErrMess(Handle, 'Несовпадают партии!');                                                          //
    tmpWafer.Free;                                                                                   //
    Exit;                                                                                            //
  end;                                                                                               //
//  if tmpWafer.Num <> Wafer.Num then                                                                  //
//  begin                                                                                              //
//    ErrMess(Handle, 'Несовпадают номера пластин!');                                                  //
//    tmpWafer.Free;                                                                                   //
//    Exit;                                                                                            //
//  end;                                                                                               //
                                                                                                     //
  SL := TStringList.Create;                                                                          //
  SL.LoadFromFile(STSfName);                                                                         //
                                                                                                     //
  Count := 0;                                                                                        //
  n := 0;                                                                                            //
  while (Trim(SL.Strings[0]) <> '[ChipsParams]') do                                                  //
  begin                                                                                              //
    if SL.Count = 1 then                                                                             //
    begin                                                                                            //
      ErrMess(Handle, 'Не найдено поле [ChipsParams]!');                                             //
      tmpWafer.Free;                                                                                 //
      SL.Free;                                                                                       //
      Exit;                                                                                          //
    end;                                                                                             //
    SL.Delete(0);                                                                                    //
    Inc(Count);                                                                                      //
  end;                                                                                               //
  SL.Delete(0);         //                                                                           //
  Inc(Count);           //                                                                           //
  Str := SL.Strings[0]; // Удалим поле [ChipsParams]                                                 //
  SL.Delete(0);         //                                                                           //
  Inc(Count);           //                                                                           //
                                                                                                     //
  try                                                                                                //
    P := Pos(#9, Str);                                                                               //
    S := Copy(Str, 1, P-1);                                                                          //
    n := 0;                                                                                          //
    while Trim(S) <> 'Status' do                                                                     //
    begin                                                                                            //
      tmpWafer.TestsParams[n].Name := S;                //                                           //
      Inc(n);                                           // Считываем                                 //
      Delete(Str, 1, P);                                // название                                  //
      P := Pos(#9, Str);                                // столбцов                                  //
      S := Copy(Str, 1, P-1);                           //                                           //
      if S = '' then                                    //                                           //
      begin                                             //                                           //
        ErrMess(Handle, 'Не найден столбец статуса !'); //                                           //
        tmpWafer.Free;                                  //                                           //
        SL.Free;                                        //                                           //
        Exit;                                           //                                           //
      end;                                              //                                           //
    end;                                                //                                           //
    SetLength(Mass, Length(tmpWafer.TestsParams));                                                   //
                                                                                                     //
    if SL.Count > 0 then                                                                             //
      for n := 0 to SL.Count-1 do                                                                    //
      begin                                                                                          //
        Str := SL.Strings[n];                                                                        //
        if Trim(Str) = '' then Continue;                                                             //
        if Length(tmpWafer.TestsParams) > 0 then // Пропустим пустую строку                          //
          for i := 0 to Length(tmpWafer.TestsParams)-1 do                                            //
          begin                                                                                      //
            P := Pos(#9, Str);                                                                       //
            S := Copy(Str, 1, P-1);                                                                  //
            Mass[i] := StrToFloat(S);                                                                //
            Delete(Str, 1, P);                                                                       //
          end;                                                                                       //
                                                                                                     //
        P := Pos(#9, Str);                                                                           //
        S := Copy(Str, 1, P-1);                                                                      //
        Stat := StrToInt(S);                                                                         //
        Delete(Str, 1, P);                                                                           //
                                                                                                     //
        P := Pos(#9, Str);                                                                           //
        S := Copy(Str, 1, P-1);                                                                      //
        X := StrToInt(S);                                                                            //
        Delete(Str, 1, P);                                                                           //
        if X > (Length(tmpWafer.Chip[0])-1) then SetLength(tmpWafer.Chip[0], X+1);                   //
                                                                                                     //
        Y := StrToInt(Str);                                                                          //
        Delete(Str, 1, P);                                                                           //
        if Y > (Length(tmpWafer.Chip)-1) then SetLength(tmpWafer.Chip, Y+1);                         //
                                                                                                     //
        tmpWafer.Chip[Y, X].Status := Stat;                                                          //
        if (tmpWafer.Chip[Y, X].Status < 10) or (tmpWafer.Chip[Y, X].Status > 1500) then             //
        begin                                                                                        //
          SetLength(tmpWafer.Chip[Y, X].ChipParams, Length(tmpWafer.TestsParams));                   //
          if Length(tmpWafer.TestsParams) > 0 then                                                   //
            for i := 0 to Length(tmpWafer.TestsParams)-1 do                                          //
            begin                                                                                    //
              tmpWafer.Chip[Y, X].ChipParams[i].Value := Mass[i];
              tmpWafer.Chip[Y, X].ChipParams[i].Stat  := GetChipParamsStat(Mass[i], tmpWafer.TestsParams[i].Norma.Min, tmpWafer.TestsParams[i].Norma.Max);
            end;
        end;                                                                                         //
                                                                                                     //
        if Trim(SL.Strings[n]) = '' then Break;                                                      //
      end;                                                                                           //
                                                                                                     //
      DecimalSeparator := '.';                                                                       //
  except                                                                                             //
    ErrMess(Handle, 'Ошибка в строке '+IntToStr(n+Count+1));                                         //
    tmpWafer.Free;                                                                                   //
    SL.Free;                                                                                         //
    DecimalSeparator := '.';                                                                         //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  if Wafer.CutSide <> 0 then                                     // Подгоним                         //
     while tmpWafer.CutSide <> Wafer.CutSide do tmpWafer.Rotate; // срез пластины                    //
  if (Length(tmpWafer.Chip[0]) <> Length(Wafer.Chip[0])) and                                         //
     (Length(tmpWafer.Chip)    <> Length(Wafer.Chip))    then                                        //
  begin                                                                                              //
    ErrMess(Handle, 'Несовпадает размерность пластин!     '+IntToStr(Length(tmpWafer.Chip))+' ..... '+IntToStr(Length(Wafer.Chip)));                                             //
//    tmpWafer.Free;                                                                                   //
//    Exit;                                                                                            //
  end;                                                                                               //
  if Length(tmpWafer.TestsParams) <> Length(Wafer.TestsParams) then                                  //
  begin                                                                                              //
    ErrMess(Handle, 'Несовпадает количество параметров!');                                           //
//    tmpWafer.Free;                                                                                   //
//    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  for Y := 0 to Length(tmpWafer.Chip)-1 do                                                           //
    for X := 0 to Length(tmpWafer.Chip[0])-1 do                                                      //
      case tmpWafer.Chip[Y,X].Status of                                                              //
        1,                                                                                           //
        10..1500,                                                                                    //
        2000..3000,                                                                                  //
        3500..4500: begin                                                                            //
                      Wafer.Chip[Y,X].Status := tmpWafer.Chip[Y,X].Status;                           //
                        if Length(tmpWafer.Chip[Y,X].ChipParams) > 0 then                                     //
                        begin                                                                                 //
                          if Length(Wafer.Chip[Y,X].ChipParams) <> Length(tmpWafer.Chip[Y,X].ChipParams) then //
                              SetLength(Wafer.Chip[Y,X].ChipParams, Length(tmpWafer.Chip[Y,X].ChipParams));   //
                                                                                                              //
                          for n := 0 to Length(tmpWafer.Chip[Y,X].ChipParams)-1 do                            //
                            Wafer.Chip[Y,X].ChipParams[n].Value := tmpWafer.Chip[Y,X].ChipParams[n].Value;    //
                        end;                                                                         //
                    end;                                                                             //
      end;                                                                                           //
  tmpWafer.Free;                                                                                     //
                                                                                                     //
  Result := True;                                                                                    //
                                                                                                     //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                      //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                            //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                           //
                                                                                                     //
  Wafer.SetChipsID;                                                                                  //
  Wafer.CalcChips;                                                                                   //
                                                                                                     //
  fSizeChipX := 0;                                                                                   //
  fSizeChipY := 0;                                                                                   //
  DrawWafer;                                                                                         //
  PBox.Repaint;                                                                                      //
                                                                                                     //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                //
                                                                                                     //
  SL.Free;                                                                                           //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.SaveSTS(const STSfName: TFileName): Boolean;                                    //
var                                                                                                  //
  INIfName: TIniFile;                                                                                //
  X, Y: WORD;                                                                                        //
  n: WORD;                                                                                           //
  FS: TFileStream;                                                                                   //
  DateTime: TDateTime;                                                                               //
  Str: String;                                                                                       //
begin                                                                                                //
  Result := True;                                                                                    //
                                                                                                     //
  if FileExists(STSfName) then DeleteFile(STSfName);                                                 //
                                                                                                     //
  DecimalSeparator := ',';                                                                           //
                                                                                                     //
  INIfName := TIniFile.Create(STSfName);                                                             //
  with Wafer do                                                                                      //
  begin                                                                                              //
    with INIfName do                                                                                 //
    begin                                                                                            //
      WriteString ('Main', 'OKR', OKR);                                                              //
      WriteString ('Main', 'Code', Code);                                                            //
      WriteString ('Main', 'MPW', MPW);                                                              //
      WriteString ('Main', 'MPWPos', MPWPos);                                                        //
      WriteString ('Main', 'Device', Device);                                                        //
      WriteString ('Main', 'DscrDev', DscrDev);                                                      //
      WriteString ('Main', 'MSystem', MeasSystem);                                                   //
      WriteString ('Main', 'Prober', Prober);                                                        //
                                                                                                     //
      WriteInteger('Main', 'Diametr', Diameter);                                                     //
      WriteInteger('Main', 'ChipSizeX', Round(StepX*1000));                                          //
      WriteInteger('Main', 'ChipSizeY', Round(StepY*1000));                                          //
                                                                                                     //
      WriteString ('Main', 'Lot', NLot);                                                             //
      WriteString ('Main', 'Wafer', Num);                                                            //
      WriteInteger('Main', 'WorkPlace', NWPlace);                                                    //
      WriteString ('Main', 'Operator', Operator);                                                    //
      WriteString ('Main', 'Date', TimeDate);                                                        //
      WriteString ('Main', 'Condition', Condition);                                                  //
      WriteString ('Main', 'Info', Info);                                                            //
                                                                                                     //
      WriteInteger('Add', 'CadreX',  Cadre.ScaleX);                                                  //
      WriteInteger('Add', 'CadreY',  Cadre.ScaleY);                                                  //
      WriteInteger('Add', 'OffsetX', Cadre.StartX);                                                  //
      WriteInteger('Add', 'OffsetY', Cadre.StartY);                                                  //
      WriteInteger('Add', 'MaxX', Length(Chip[0]));                                                  //
      WriteInteger('Add', 'MaxY', Length(Chip));                                                     //
      WriteInteger('Add', 'BaseChipX', BaseChip.X);                                                  //
      WriteInteger('Add', 'BaseChipY', BaseChip.Y);                                                  //
      WriteInteger('Add', 'Path', Direct);                                                           //
      WriteInteger('Add', 'Cut', CutSide);                                                           //
                                                                                                     //
      Free;                                                                                          //
    end;                                                                                             //
                                                                                                     //
    FS := TFileStream.Create(STSfName, fmOpenWrite+fmShareDenyNone);                                 //
    FS.Position := FS.Size;                                                                          //
    FS.Write(#13#10, 2);                                                                             //
    Str := '[StatusNames]';                                                                          //
    FS.Write(Pointer(Str)^, Length(Str));                                                            //
    FS.Write(#13#10, 2);                                                                             //
    if StatusNamesSL.Count > 0 then                                                                  //
      for n := 0 to StatusNamesSL.Count-1 do                                                         //
      begin                                                                                          //
        FS.Write(Pointer(StatusNamesSL.Strings[n])^, Length(StatusNamesSL.Strings[n]));              //
        FS.Write(#13#10, 2);                                                                         //
      end;                                                                                           //
                                                                                                     //
    if Length(TestsParams) > 0 then                                                                  //
    begin                                                                                            //
      FS.Write(#13#10, 2);                                                                           //
      Str := '[TestsParams]';                                                                        //
      FS.Write(Pointer(Str)^, Length(Str));                                                          //
      FS.Write(#13#10, 2);                                                                           //
      for n := 0 to Length(TestsParams)-1 do                                                         //
      begin                                                                                          //
        if TestsParams[n].Norma.Min <> -NotSpec then                                                 //
          Str := IntToStr(n)+'='+FormatFloat('0.000', TestsParams[n].Norma.Min)+';'                  //
        else Str := IntToStr(n)+'=N;';                                                               //
        if TestsParams[n].Norma.Max <> NotSpec then                                                  //
          Str := Str+FormatFloat('0.000', TestsParams[n].Norma.Max)+';'                              //
        else Str := Str+'N;';                                                                        //
        Str := Str+IntToStr(TestsParams[n].Status);                                                  //
        FS.Write(Pointer(Str)^, Length(Str));                                                        //
        FS.Write(#13#10, 2);                                                                         //
      end;                                                                                           //
    end;                                                                                             //
                                                                                                     //
    FS.Write(#13#10, 2);                                                                             //
    Str := '[ChipsParams]';                                                                          //
    FS.Write(Pointer(Str)^, Length(Str));                                                            //
    FS.Write(#13#10, 2);                                                                             //
    if Length(TestsParams) > 0 then                                                                  //
      for n := 0 to Length(TestsParams)-1 do                                                         //
      begin                                                                                          //
        Str := TestsParams[n].Name+#9;                                                               //
        FS.Write(Pointer(Str)^, Length(Str));                                                        //
      end;                                                                                           //
    Str := 'Status'+#9;                                                                              //
    FS.Write(Pointer(Str)^, Length(Str));                                                            //
    Str := 'X'+#9;                                                                                   //
    FS.Write(Pointer(Str)^, Length(Str));                                                            //
    Str := 'Y'+#13#10;                                                                               //
    FS.Write(Pointer(Str)^, Length(Str));                                                            //
                                                                                                     //
    for Y := 0 to Length(Chip)-1 do                                                                  //
      for X := 0 to Length(Chip[0])-1 do                                                             //
        with Chip[Y, X] do                                                                           //
          if Status <> 2 then                                                                        //
          begin                                                                                      //
            Str := '';                                                                               //
            if Length(ChipParams) > 0 then                                                           //
              for n := 0 to Length(ChipParams)-1 do Str := Str+FormatFloat('0.000', ChipParams[n].Value)+#9 //
            else                                                                                     //
              if Length(TestsParams) > 0 then                                                        //
                for n := 0 to Length(TestsParams)-1 do Str := Str+FormatFloat('0.000', 0.0)+#9;      //
            Str := Str+IntToStr(Status)+#9+IntToStr(X)+#9+IntToStr(Y);                               //
            FS.Write(Pointer(Str)^, Length(Str));                                                    //
            FS.Write(#13#10, 2);                                                                     //
          end;                                                                                       //
                                                                                                     //
    FS.Free;                                                                                         //
  end;                                                                                               //
                                                                                                     //
  DateTime := StrToDateTime(Wafer.TimeDate+' 12:00:00');                                             //
  FileSetDate(STSfName, DateTimeToFileDate(DateTime));                                               //
                                                                                                     //
  DecimalSeparator := '.';                                                                           //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadNI(const NIfName: TFileName): Boolean;                                      //
var                                                                                                  //
  n, m: DWORD;                                                                                       //
  Str, NumChip, PrevChip: String;                                                                    //
  SL: TStringList;                                                                                   //
  HeaderCount, X, Y, k: WORD;                                                                        //
  FirstTime: Boolean;                                                                                //
begin                                                                                                //
  Result := False;                                                                                   //
                                                                                                     //
  if Wafer <> nil then FreeAndNil(Wafer);                                                            //
  Wafer := TWafer.Create;                                                                            //
  Wafer.fName := NIfName;                                                                            //
                                                                                                     //
  HeaderCount := Wafer.LoadNIHeader;                                                                 //
  if HeaderCount = 0 then                                                                            //
  begin                                                                                              //
    ErrMess(Handle, 'Ошибка загрузки заголовка!');                                                   //
    Init;                                                                                            //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  SL := TStringList.Create;                                                                          //
  SL.LoadFromFile(Wafer.fName);                                                                      //
                                                                                                     //
  for n := 0 to HeaderCount-1 do SL.Delete(0); // Удалим заголовок                                   //
                                                                                                     //
  FirstTime := True;                                                                                 //
  m := 0;                                                                                            //
  for Y := 0 to Length(Wafer.Chip)-1 do                                                              //
    for X := 0 to Length(Wafer.Chip[0])-1 do                                                         //
      for n := 0 to Length(Wafer.TestsParams) do                                                     //
      begin                                                                                          //
        if m = SL.Count then Break;                                                                  //
                                                                                                     //
        Str := Trim(SL.Strings[m]);                                                                  //
        if Str = '' then Continue;                                                                   //
                                                                                                     //
        NumChip := Copy(Str, 1, Pos(#9, Str)-1);                                                     //
        Delete(Str, 1, Pos(#9, Str)); // Удалим номер кристалла                                      //
        Delete(Str, 1, Pos(#9, Str)); // Удалим номер теста                                          //
        Delete(Str, 1, Pos(#9, Str)); // Удалим название параметра                                   //
                                                                                                     //
        if FirstTime then                                                                            //
        begin                                                                                        //
          PrevChip := NumChip;                                                                       //
          FirstTime := False;                                                                        //
        end;                                                                                         //
                                                                                                     //
        if NumChip <> PrevChip then                                                                  //
        begin                                                                                        //
          PrevChip := NumChip;                                                                       //
                                                                                                     //
          for k := n to Length(Wafer.TestsParams)-1 do                                               //
            with Wafer do                                                                            //
              Chip[Y, X].ChipParams[k].Stat := GetChipParamsStat(Chip[Y, X].ChipParams[k].Value, TestsParams[k].Norma.Min, TestsParams[k].Norma.Max);
                                                                                                     //
          Break;                                                                                     //
        end;                                                                                         //
                                                                                                     //
        try                                                                                          //
          Wafer.Chip[Y, X].ChipParams[n].Value := StrToFloat(Trim(Copy(Str, 1, Pos(#9, Str))));      //
        except                                                                                       //
          Wafer.Chip[Y, X].ChipParams[n].Value := NotSpec;                                           //
        end;                                                                                         //
                                                                                                     //
        Inc(m);                                                                                      //
                                                                                                     //
        with Wafer do                                                                                //
          Chip[Y, X].ChipParams[n].Stat := GetChipParamsStat(Chip[Y, X].ChipParams[n].Value, TestsParams[n].Norma.Min, TestsParams[n].Norma.Max);
                                                                                                     //
        if Wafer.Chip[Y, X].Status < 2000 then                                                       //
          if Wafer.Chip[Y, X].ChipParams[n].Stat <> 1 then Wafer.Chip[Y, X].Status := 2000+n         //
          else Wafer.Chip[Y, X].Status := 1;                                                         //
                                                                                                     //
        PrevChip := NumChip;                                                                         //
      end;                                                                                           //
                                                                                                     //
  Result := True;                                                                                    //
                                                                                                     //
  Wafer.SetChipsID;                                                                                  //
  Wafer.CalcChips;                                                                                   //
                                                                                                     //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                      //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                            //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                           //
                                                                                                     //
  fSizeChipX := 0;                                                                                   //
  fSizeChipY := 0;                                                                                   //
  DrawWafer;                                                                                         //
  PBox.Repaint;                                                                                      //
                                                                                                     //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                //
                                                                                                     //
  SL.Free;                                                                                           //
                                                                                                     //
  DecimalSeparator := '.';                                                                           //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.AddNI(const NIfName: TFileName): Boolean;                                       //
begin                                                                                                //
  //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadNI2(const NIfName: TFileName): Boolean;                                     //
var                                                                                                  //
  n, m: DWORD;                                                                                       //
  Str, NumChip, PrevChip, tmpStr: String;                                                            //
  SL: TStringList;                                                                                   //
  HeaderCount, X, Y, k: WORD;                                                                        //
  FirstTime: Boolean;                                                                                //
begin                                                                                                //
  Result := False;                                                                                   //
                                                                                                     //
  if Wafer <> nil then FreeAndNil(Wafer);                                                            //
  Wafer := TWafer.Create;                                                                            //
  Wafer.fName := NIfName;                                                                            //
                                                                                                     //
  HeaderCount := Wafer.LoadNI2Header;                                                                //
  if HeaderCount = 0 then                                                                            //
  begin                                                                                              //
    ErrMess(Handle, 'Ошибка загрузки заголовка!');                                                   //
    Init;                                                                                            //
    Exit;                                                                                            //
  end;                                                                                               //
                                                                                                     //
  SL := TStringList.Create;                                                                          //
  SL.LoadFromFile(Wafer.fName);                                                                      //
                                                                                                     //
  for n := 0 to HeaderCount-1 do SL.Delete(0); // Удалим заголовок                                   //
                                                                                                     //
  FirstTime := True;                                                                                 //
  m := 0;                                                                                            //
  for Y := 0 to Length(Wafer.Chip)-1 do                                                              //
    for X := 0 to Length(Wafer.Chip[0])-1 do                                                         //
      for n := 0 to Length(Wafer.TestsParams) do                                                     //
      begin                                                                                          //
        if m = SL.Count then Break;                                                                  //
                                                                                                     //
        Str := Trim(SL.Strings[m]);                                                                  //
        if Str = '' then Continue;                                                                   //
                                                                                                     //
        NumChip := Copy(Str, 1, Pos(#9, Str)-1);                                                     //
        Delete(Str, 1, Pos(#9, Str)); // Удалим номер кристалла                                      //
        Delete(Str, 1, Pos(#9, Str)); // Удалим номер теста                                          //
        Delete(Str, 1, Pos(#9, Str)); // Удалим название параметра                                   //
        Delete(Str, 1, Pos(#9, Str)); // Единицу измерения                                           //
                                                                                                     //
        if FirstTime then                                                                            //
        begin                                                                                        //
          PrevChip := NumChip;                                                                       //
          FirstTime := False;                                                                        //
        end;                                                                                         //
                                                                                                     //
        if NumChip <> PrevChip then                                                                  //
        begin                                                                                        //
          PrevChip := NumChip;                                                                       //
                                                                                                     //
          for k := n to Length(Wafer.TestsParams)-1 do                                               //
            with Wafer do                                                                            //
              Chip[Y, X].ChipParams[k].Stat := GetChipParamsStat(Chip[Y, X].ChipParams[k].Value, TestsParams[k].Norma.Min, TestsParams[k].Norma.Max);
                                                                                                     //
          Break;                                                                                     //
        end;                                                                                         //
                                                                                                     //
        tmpStr := Trim(Copy(Str, 1, Pos(#9, Str)));                                                  //
        if Pos('.', tmpStr) <> 0 then DecimalSeparator := '.'                                        //
                                 else DecimalSeparator := ',';                                       //
        try                                                                                          //
          Wafer.Chip[Y, X].ChipParams[n].Value := StrToFloat(tmpStr);                                //
        except                                                                                       //
          Wafer.Chip[Y, X].ChipParams[n].Value := NotSpec;                                           //
        end;                                                                                         //
                                                                                                     //
        Inc(m);                                                                                      //
                                                                                                     //
        Wafer.Chip[Y, X].ChipParams[n].Stat := GetChipParamsStat(Wafer.Chip[Y, X].ChipParams[n].Value, Wafer.TestsParams[n].Norma.Min, Wafer.TestsParams[n].Norma.Max);
                                                                                                     //
        if Wafer.Chip[Y, X].Status < 2000 then                                                       //
          if Wafer.Chip[Y, X].ChipParams[n].Stat <> 1 then Wafer.Chip[Y, X].Status := 2000+n         //
          else Wafer.Chip[Y, X].Status := 1;                                                         //
                                                                                                     //
        PrevChip := NumChip;                                                                         //
      end;                                                                                           //
                                                                                                     //
  Result := True;                                                                                    //
                                                                                                     //
  Wafer.SetChipsID;                                                                                  //
  Wafer.CalcChips;                                                                                   //
                                                                                                     //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                      //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                            //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                           //
                                                                                                     //
  fSizeChipX := 0;                                                                                   //
  fSizeChipY := 0;                                                                                   //
  DrawWafer;                                                                                         //
  PBox.Repaint;                                                                                      //
                                                                                                     //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                //
                                                                                                     //
  SL.Free;                                                                                           //
                                                                                                     //
  DecimalSeparator := '.';                                                                           //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.AddNI2(const NIfName: TFileName): Boolean;                                      //
begin                                                                                                //
  //
end;                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadXML(const XMLfName: TFileName): Boolean;                                             //
var                                                                                                           //
  n: DWORD;                                                                                                   //
  Str: String;                                                                                                //
  X, Y: WORD;                                                                                                 //
  P1, P2, P3: byte;                                                                                           //
  XMLDoc1: IXMLDocument;                                                                                      //
  SL: TStringList;                                                                                            //
begin                                                                                                         //
  Result := False;                                                                                            //
                                                                                                              //
  if Wafer <> nil then FreeAndNil(Wafer);                                                                     //
  Wafer := TWafer.Create;                                                                                     //
  Wafer.fName := XMLfName;                                                                                    //
                                                                                                              //
  DecimalSeparator := '.';                                                                                    //
                                                                                                              //
  try                                                                                                         //
    SL := TStringList.Create;                                                                                 //
    SL.LoadFromFile(Wafer.fName);                                                                             //
    SL.Strings[0] := '<?xml version="1.0" encoding="windows-1251"?>';                                         //
                                                                                                              //
    XMLDoc1 := TXMLDocument.Create(nil);                                                                      //
    XMLDoc1.XML := SL;                                                                                        //
    XMLDoc1.Active := True;                                                                                   //
                                                                                                              //
    with Wafer do                                                                                             //
    begin                                                                                                     //
      Device := XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_BATCH_ID'].Text;               //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_OCR_ID'].Text);              //
      P1 := Pos('-', Str);                                                                                    //
      if P1 <> 0 then                                                                                         //
      begin                                                                                                   //
        NLot := Copy(Str, 1, P1-1);                                                                           //
        Num  := Copy(Str, P1+1, Length(Str)-P1);                                                              //
      end                                                                                                     //
      else NLot := Str;                                                                                       //
      Diameter := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_SIZE'].Text)); //
      case StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['FLAT_LOCATION'].Text)) of   //
          0: CutSide := 3;                                                                                    //
         90: CutSide := 2;                                                                                    //
        180: CutSide := 1;                                                                                    //
        270: CutSide := 4;                                                                                    //
      end;                                                                                                    //
      LDiameter := Diameter;                                                                                  //
      Radius  := Diameter/2;                          //                                                      //
      LRadius := Radius-(Diameter-LDiameter);         //                                                      //
      Chord   := Sqrt(Radius*Radius-LRadius*LRadius); //                                                      //
                                                                                                              //
      StepX := StrToFloat(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['XSTEP'].Text));       //
      StepY := StrToFloat(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['YSTEP'].Text));       //
                                                                                                              //
      X := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['COLUMN_COUNT'].Text));      //
      Y := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['ROW_COUNT'].Text));         //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['FIRST_DIE'].Text);                 //
      P1 := Pos(',', Str);                                                                                    //
      if P1 <> 0 then                                                                                         //
      begin                                                                                                   //
        BaseChip.X := StrToInt(Copy(Str, 1, P1-1));                                                           //
        BaseChip.Y := StrToInt(Copy(Str, P1+1, Length(Str)-P1));                                              //
      end;                                                                                                    //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['PROBE_START_DATETIME'].Text);      //
      P1 := PosEx('-', Str);                                                                                  //
      TimeDate := '.'+Copy(Str, 1, P1-1);                                                                     //
      P2 := PosEx('-', Str, P1+1);                                                                            //
      TimeDate := '.'+Copy(Str, P1+1, P2-P1-1)+TimeDate;                                                      //
      P3 := PosEx(' ', Str, P2+1);                                                                            //
      TimeDate := Copy(Str, P2+1, P3-P2-1)+TimeDate;                                                          //
      Prober := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['PROBE_DEVICE_NAME'].Text);      //
      Direct := 2; // Для зонда 6290                                                                          //
      Str := XMLDoc1.DocumentElement.ChildNodes['WAFER_MAP'].Text;                                            //
      SetLength(TestsParams, 0);                                                                              //
                                                                                                              //
      SetLength(Chip, 0, 0);                                                                                  //
      SetLength(Chip, Y, X);                                                                                  //
      n := 1;                                                                                                 //
      for Y := 0 to Length(Chip)-1 do                                                                         //
        for X := 0 to Length(Chip[0])-1 do                                                                    //
        begin                                                                                                 //
          Chip[Y, X].Status := 2;                                                                             //
                                                                                                              //
          case Str[n] of                                                                                      //
            '.': Chip[Y, X].Status := 2;                                                                      //
            ':': Chip[Y, X].Status := 3;                                                                      //
            'X': Chip[Y, X].Status := 2000;                                                                   //
            '1': Chip[Y, X].Status := 1;                                                                      //
            '-': Chip[Y, X].Status := 4;                                                                      //
            '/': Chip[Y, X].Status := 4;                                                                      //
            'a': begin                                                                                        //
                   Chip[Y, X].Status := 10; // Базовый будет неконтакт                                        //
                   BaseChip.X := X;                                                                           //
                   BaseChip.Y := Y;                                                                           //
                 end;                                                                                         //
          end;                                                                                                //
          Chip[Y, X].ShowGr := 0;                                                                             //
          SetLength(Chip[Y, X].ChipParams, 0);                                                                //
                                                                                                              //
          Inc(n);                                                                                             //
        end;                                                                                                  //
    end;                                                                                                      //
                                                                                                              //
    XMLDoc1.Active := False;                                                                                  //
    SL.Free;                                                                                                  //
  except                                                                                                      //
    XMLDoc1.Active := False;                                                                                  //
    SL.Free;                                                                                                  //
    ErrMess(Handle, 'Ошибка загрузки файла!');                                                                //
    Init;                                                                                                     //
    Exit;                                                                                                     //
  end;                                                                                                        //
                                                                                                              //
  Result := True;                                                                                             //
                                                                                                              //
  Wafer.Normalize;                                                                                            //
  Wafer.SetChipsID;                                                                                           //
  Wafer.CalcChips;                                                                                            //
                                                                                                              //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                               //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                                     //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                                    //
                                                                                                              //
  fSizeChipX := 0;                                                                                            //
  fSizeChipY := 0;                                                                                            //
  DrawWafer;                                                                                                  //
  PBox.Repaint;                                                                                               //
                                                                                                              //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                         //
end;                                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.AddXML(const XMLfName: TFileName): Boolean;                                              //
var                                                                                                           //
  n, ErrCount: DWORD;                                                                                         //
  Str: String;                                                                                                //
  X, Y: WORD;                                                                                                 //
  P1, P2, P3: byte;                                                                                           //
  XMLDoc1: IXMLDocument;                                                                                      //
  SL: TStringList;                                                                                            //
  tmpWafer: TWafer;                                                                                           //
begin                                                                                                         //
  Result := False;                                                                                            //
                                                                                                              //
  if Wafer = nil then Exit;                                                                                   //
                                                                                                              //
  tmpWafer := TWafer.Create;                                                                                  //
  tmpWafer.fName := XMLfName;                                                                                 //
                                                                                                              //
  DecimalSeparator := '.';                                                                                    //
                                                                                                              //
  try                                                                                                         //
    SL := TStringList.Create;                                                                                 //
    SL.LoadFromFile(tmpWafer.fName);                                                                          //
    SL.Strings[0] := '<?xml version="1.0" encoding="windows-1251"?>';                                         //
                                                                                                              //
    XMLDoc1 := TXMLDocument.Create(nil);                                                                      //
    XMLDoc1.XML := SL;                                                                                        //
    XMLDoc1.Active := True;                                                                                   //
                                                                                                              //
    with tmpWafer do                                                                                          //
    begin                                                                                                     //
      Device := XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_BATCH_ID'].Text;               //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_OCR_ID'].Text);              //
      P1 := Pos('-', Str);                                                                                    //
      if P1 <> 0 then                                                                                         //
      begin                                                                                                   //
        NLot := Copy(Str, 1, P1-1);                                                                           //
        Num  := Copy(Str, P1+1, Length(Str)-P1);                                                              //
      end                                                                                                     //
      else NLot := Str;                                                                                       //
      Diameter := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['WAFER_SIZE'].Text)); //
      case StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['FLAT_LOCATION'].Text)) of   //
          0: CutSide := 3;                                                                                    //
         90: CutSide := 2;                                                                                    //
        180: CutSide := 1;                                                                                    //
        270: CutSide := 4;                                                                                    //
      end;                                                                                                    //
      LDiameter := Diameter;                                                                                  //
      Radius  := Diameter/2;                          //                                                      //
      LRadius := Radius-(Diameter-LDiameter);         //                                                      //
      Chord   := Sqrt(Radius*Radius-LRadius*LRadius); //                                                      //
                                                                                                              //
      StepX := StrToFloat(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['XSTEP'].Text));       //
      StepY := StrToFloat(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['YSTEP'].Text));       //
                                                                                                              //
//      StepX := StepX/1.27;  /////////////////////////////////////////                                         //
//      StepY := StepY/1.40;  //////////////////////////////////////////////                                    //
                                                                                                              //
      X := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['COLUMN_COUNT'].Text));      //
      Y := StrToInt(Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['ROW_COUNT'].Text));         //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['FIRST_DIE'].Text);                 //
      P1 := Pos(',', Str);                                                                                    //
      if P1 <> 0 then                                                                                         //
      begin                                                                                                   //
        BaseChip.X := StrToInt(Copy(Str, 1, P1-1));                                                           //
        BaseChip.Y := StrToInt(Copy(Str, P1+1, Length(Str)-P1));                                              //
      end;                                                                                                    //
      Str := Trim(XMLDoc1.DocumentElement.ChildNodes['HEADER'].ChildNodes['PROBE_START_DATETIME'].Text);      //
      P1 := PosEx('-', Str);                                                                                  //
      TimeDate := '.'+Copy(Str, 1, P1-1);                                                                     //
      P2 := PosEx('-', Str, P1+1);                                                                            //
      TimeDate := '.'+Copy(Str, P1+1, P2-P1-1)+TimeDate;                                                      //
      P3 := PosEx(' ', Str, P2+1);                                                                            //
      TimeDate := Copy(Str, P2+1, P3-P2-1)+TimeDate;                                                          //
      Direct := 2; // Для зонда 6510                                                                          //
      Str := XMLDoc1.DocumentElement.ChildNodes['WAFER_MAP'].Text;                                            //
      SetLength(TestsParams, 0);                                                                              //
                                                                                                              //
      SetLength(Chip, 0, 0);                                                                                  //
      SetLength(Chip, Y, X);                                                                                  //
      n := 1;                                                                                                 //
      for Y := 0 to Length(Chip)-1 do                                                                         //
        for X := 0 to Length(Chip[0])-1 do                                                                    //
        begin                                                                                                 //
          Chip[Y, X].Status := 2;                                                                             //
                                                                                                              //
          case Str[n] of                                                                                      //
            '.': Chip[Y, X].Status := 2;                                                                      //
            ':': Chip[Y, X].Status := 3;                                                                      //
            'X': Chip[Y, X].Status := 2000;                                                                   //
            '1': Chip[Y, X].Status := 1;                                                                      //
            '-': Chip[Y, X].Status := 4;                                                                      //
            '/': Chip[Y, X].Status := 4;                                                                      //
            'a': begin                                                                                        //
//                   Chip[Y, X].Status := 0;                                                                    //
//                   BaseChip.X := X;                                                                           //
//                   BaseChip.Y := Y;                                                                           //
                 end;                                                                                         //
          end;                                                                                                //
          Chip[Y, X].ShowGr := 0;                                                                             //
          SetLength(Chip[Y, X].ChipParams, 0);                                                                //
                                                                                                              //
          Inc(n);                                                                                             //
        end;                                                                                                  //
    end;                                                                                                      //
                                                                                                              //
    XMLDoc1.Active := False;                                                                                  //
    SL.Free;                                                                                                  //
  except                                                                                                      //
    XMLDoc1.Active := False;                                                                                  //
    SL.Free;                                                                                                  //
    ErrMess(Handle, 'Ошибка загрузки файла!');                                                                //
    Init;                                                                                                     //
    Exit;                                                                                                     //
  end;                                                                                                        //
                                                                                                              //
  Result := True;                                                                                             //
                                                                                                              //
  tmpWafer.Normalize;                                                                                         //
  tmpWafer.SetChipsID;                                                                                        //
  tmpWafer.CalcChips;                                                                                         //
                                                                                                              //
  if tmpWafer.NMeased <> Wafer.NMeased then                                                                     //
    if QuestMess(Handle, 'Нужно '+IntToStr(Wafer.NTotal)+' кристаллов, получено '+IntToStr(tmpWafer.NMeased)+#13#10+'Все равно продолжить?') = IDNO then
    begin
      tmpWafer.Free;
      Exit;
    end;

  ErrCount := 0;
  for n := 0 to Length(tmpWafer.ChipN)-1 do
    if n < Length(Wafer.ChipN) then
    begin
      Y := tmpWafer.ChipN[n].Y;
      X := tmpWafer.ChipN[n].X;

      if not EqualStatus(tmpWafer.Chip[Y, X].Status, Wafer.Chip[Wafer.ChipN[n].Y,  Wafer.ChipN[n].X].Status) then Inc(ErrCount);
      
      tmpWafer.Chip[tmpWafer.ChipN[n].Y, tmpWafer.ChipN[n].X].Status := Wafer.Chip[Wafer.ChipN[n].Y, Wafer.ChipN[n].X].Status;
      tmpWafer.Chip[Y, X].ChipParams :=  Wafer.Chip[Wafer.ChipN[n].Y,  Wafer.ChipN[n].X].ChipParams;
    end;
  if ErrCount > 0 then ErrMess(Handle, IntToStr(ErrCount)+' несовпадений!');

  Wafer.Chip := tmpWafer.Chip;                                                                                //
  Wafer.Diameter  := tmpWafer.Diameter;                                                                       //
  Wafer.LDiameter := tmpWafer.LDiameter;                                                                      //
  Wafer.Radius    := tmpWafer.Radius;                                                                         //
  Wafer.LRadius   := tmpWafer.LRadius;                                                                        //
  Wafer.Chord     := tmpWafer.Chord;                                                                          //
  Wafer.StepX     := tmpWafer.StepX;                                                                          //
  Wafer.StepY     := tmpWafer.StepY;                                                                          //
  Wafer.CutSide   := tmpWafer.CutSide;                                                                        //
  Wafer.Direct    := tmpWafer.Direct;                                                                         //

//  Wafer.TestsParams   := tmpWafer.TestsParams;
//  Wafer.StatusNamesSL := tmpWafer.StatusNamesSL;
//  Wafer.TestsParamsSL := tmpWafer.TestsParamsSL;
//  Wafer.ColorParams   := tmpWafer.ColorParams;

  Wafer.NLot := tmpWafer.NLot;
  Wafer.Num  := tmpWafer.Num;

  Wafer.SetChipsID;                                                                                           //
//  Wafer.CalcChips; // Проанализировать!!!!                                                                                           //
                                                                                                              //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                                               //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                                     //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                                    //
                                                                                                              //
  fSizeChipX := 0;                                                                                            //
  fSizeChipY := 0;                                                                                            //
  DrawWafer;                                                                                                  //
  PBox.Repaint;                                                                                               //
                                                                                                              //
  tmpWafer.Free;                                                                                              //
  tmpWafer := nil;                                                                                            //
                                                                                                              //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                                         //
end;                                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadAGL(const AGLfName: TFileName): Boolean;                             //
var                                                                                           //
  SL: TStringList;                                                                            //
  m, Y, X: DWORD;                                                                             //
  Str: string;                                                                                //
  n, NFC, NKK: WORD;                                                                          //
  OK_param: Boolean;
begin                                                                                         //
  Result := False;                                                                            //
                                                                                              //
  if Wafer <> nil then FreeAndNil(Wafer);                                                     //
  Wafer := TWafer.Create;                                                                     //
  Wafer.fName := AGLfName;                                                                    //
                                                                                              //
  if not Wafer.LoadAGLHeader then                                                             //
  begin                                                                                       //
    ErrMess(Handle, 'Ошибка загрузки файла!');                                                //
    Init;                                                                                     //
    Exit;                                                                                     //
  end;                                                                                        //
                                                                                              //
  SL := TStringList.Create;                                                                   //
  SL.LoadFromFile(Wafer.fName);                                                               //
                                                                                              //
  m := 0;                                                                                     //
  for Y := 0 to Length(Wafer.Chip)-1 do                                                       //
    for X := 0 to Length(Wafer.Chip[0])-1 do                                                  //
    begin                                                                                     //
      if m >= SL.Count then Break;                                                            //
                                                                                              //
      repeat                                                                                  //
        Str := Trim(SL.Strings[m]);                                                           //
        Inc(m);                                                                               //
                                                                                              //
        if m = SL.Count then Break;                                                           //
      until Pos('TESTFLOW STARTED', UpperCase(Str)) <> 0;                                     //
                                                                                              //
      if m >= SL.Count then Break;                                                            //
                                                                                              //
      NFC := 0;                                                                               //
      NKK := 0;                                                                               //
      n := 0;                                                                                 //
      OK_param := False;
      repeat                                                                                  //
        Str := Trim(SL.Strings[m]);                                                           //
        Inc(m);                                                                               //
                                                                                              //
        if Str = '' then Continue;                                                            //
                                                                                              //
        if Str[1] = '1' then                                                                  //
        begin                                                                                 //
          if Pos('FAILED', UpperCase(Str)) <> 0 then OK_param := False // Параметр годный
                                                else OK_param := True; // Параметр брак

          if (Pos('CONTINUITY', UpperCase(Str)) <> 0) or                                      //
             (Pos('CONTAKT',    UpperCase(Str)) <> 0) or                                      //
             (Pos('CONTACT',    UpperCase(Str)) <> 0) then                                    //
          begin                                                                               //
            if not OK_param then Wafer.Chip[Y, X].Status := 10+NKK;                           //
            Inc(NKK);                                                                         //
            Continue;                                                                         //
          end;                                                                                //
          if (Pos('FUNCTIONAL', UpperCase(Str)) <> 0) or                                      //
             (Pos('FUNCTION',   UpperCase(Str)) <> 0) or                                      //
             (Pos('FK',         UpperCase(Str)) <> 0) then                                    //
          begin                                                                               //
            if not OK_param then Wafer.Chip[Y, X].Status := 3500+NFC;                         //
            Inc(NFC);                                                                         //
            Continue; // Наверно здесь убрать, чтобы внести ФК в статистику
          end;                                                                                //
                                                                                              //
          if n < Length(Wafer.Chip[Y, X].ChipParams) then                                     //
          begin                                                                               //
            Delete(Str, 1, Pos('`', Str)); // Удалим номер сайта                              //
            Delete(Str, 1, Pos('`', Str)); // Удалим название параметра                       //
            Delete(Str, 1, Pos('`', Str)); // Удалим полное имя параметра                     //
                                                                                              //
            Delete(Str, 1, Pos('`', Str)); // Удалим passed/FAILED                            //
            Delete(Str, 1, Pos('`', Str)); // Удалим нижний предел                            //
            Str := Trim(Str);                                                                 //
            Str := Trim(Copy(Str, 1, Pos(' ', Str)-1));                                       //
            try                                                                               //
              Wafer.Chip[Y, X].ChipParams[n].Value := StrToFloat(Str);                        //
            except                                                                            //
              Wafer.Chip[Y, X].ChipParams[n].Value := NotSpec;                                //
            end;                                                                              //
                                                                                              //
            if Wafer.Chip[Y, X].ChipParams[n].Value <> NotSpec then                           //
            begin                                                                             //
              if Wafer.Chip[Y, X].Status < 10 then // Если не брак NK и FK                    //
                if (Wafer.Chip[Y, X].ChipParams[n].Value < Wafer.TestsParams[n].Norma.Min) or //
                   (Wafer.Chip[Y, X].ChipParams[n].Value > Wafer.TestsParams[n].Norma.Max)    //
                then Wafer.Chip[Y, X].Status := 2000+n                                        //
                else Wafer.Chip[Y, X].Status := 1;                                            //

              Wafer.Chip[Y, X].ChipParams[n].Stat := GetChipParamsStat(Wafer.Chip[Y, X].ChipParams[n].Value, Wafer.TestsParams[n].Norma.Min, Wafer.TestsParams[n].Norma.Max);
            end;


            if not OK_param then
              if ((Wafer.Chip[Y, X].ChipParams[n].Value = 0.0) and
                  (Wafer.TestsParams[n].Norma.Min = 0.0) and
                  (Wafer.TestsParams[n].Norma.Max = 0.0))
                  or
                 ((Wafer.Chip[Y, X].ChipParams[n].Value = 0.0) and
                  (Wafer.TestsParams[n].Norma.Min = -NotSpec) and
                  (Wafer.TestsParams[n].Norma.Max = NotSpec)) then
              begin
                Wafer.Chip[Y, X].Status := 3500+NFC; // считаем, что это брак ФК
                Inc(NFC);
                Continue;// ?
              end;
          end;                                                                                //
                                                                                              //
          Inc(n);                                                                             //
        end;                                                                                  //
      until Pos('TESTFLOW ENDED', UpperCase(Str)) <> 0;                                       //
    end;                                                                                      //
                                                                                              //
  Result := True;                                                                             //
                                                                                              //
  Wafer.SetChipsID;                                                                           //
  Wafer.CalcChips;                                                                            //
                                                                                              //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                               //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                     //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                    //
                                                                                              //
  fSizeChipX := 0;                                                                            //
  fSizeChipY := 0;                                                                            //
  DrawWafer;                                                                                  //
  PBox.Repaint;                                                                               //
                                                                                              //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                         //
                                                                                              //
  SL.Free;                                                                                    //
end;                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.AddAGL(const AGLfName: TFileName): Boolean;                              //
var                                                                                           //
  SL: TStringList;                                                                            //
  m, Y, X, Nm: DWORD;                                                                         //
  Str: string;                                                                                //
  n, NFC, NKK: WORD;                                                                          //
begin                                                                                         //
  Result := False;                                                                            //
                                                                                              //
  Wafer.fName := AGLfName;                                                                    //
                                                                                              //
  if not Wafer.AddAGLHeader then                                                              //
  begin                                                                                       //
    ErrMess(Handle, 'Ошибка загрузки файла!');                                                //
    Init;                                                                                     //
    Exit;                                                                                     //
  end;                                                                                        //
                                                                                              //
  SL := TStringList.Create;                                                                   //
  SL.LoadFromFile(AGLfName);                                                                  //
                                                                                              //
  m := 0;                                                                                     //
  for Nm := 0 to Length(Wafer.ChipN)-1 do                                                     //
  begin                                                                                       //
    X := Wafer.ChipN[Nm].X;                                                                   //
    Y := Wafer.ChipN[Nm].Y;                                                                   //
                                                                                              //
    if m >= SL.Count then Break;                                                              //
                                                                                              //
//      if not IsChip(Wafer.Chip[Y, X].Status) then Continue;                                   //
                                                                                              //
    repeat                                                                                    //
      Str := Trim(SL.Strings[m]);                                                             //
      Inc(m);                                                                                 //
                                                                                              //
      if m = SL.Count then Break;                                                             //
    until Pos('TESTFLOW STARTED', UpperCase(Str)) <> 0;                                       //
                                                                                              //
    if m >= SL.Count then Break;                                                              //
                                                                                              //
    NFC := 0;                                                                                 //
    NKK := 0;                                                                                 //
    n := 0;                                                                                   //
    repeat                                                                                    //
      Str := Trim(SL.Strings[m]);                                                             //
      Inc(m);                                                                                 //
                                                                                              //
      if Str = '' then Continue;                                                              //
                                                                                              //
      if Str[1] = '1' then                                                                    //
      begin                                                                                   //
        if (Pos('CONTINUITY', UpperCase(Str)) <> 0) or                                        //
           (Pos('CONTAKT',    UpperCase(Str)) <> 0) or                                        //
           (Pos('CONTACT',    UpperCase(Str)) <> 0) then                                      //
        begin                                                                                 //
          if Pos('FAILED', UpperCase(Str)) <> 0 then Wafer.Chip[Y, X].Status := 10+NKK;       //
          Inc(NKK);                                                                           //
          Continue;                                                                           //
        end;                                                                                  //
        if (Pos('FUNCTIONAL', UpperCase(Str)) <> 0) or                                        //
           (Pos('FUNCTION',   UpperCase(Str)) <> 0) or                                        //
           (Pos('FK',         UpperCase(Str)) <> 0) then                                      //
        begin                                                                                 //
          if Pos('FAILED', UpperCase(Str)) <> 0 then Wafer.Chip[Y, X].Status := 3500+NFC;     //
          Inc(NFC);                                                                           //
          Continue;                                                                           //
        end;                                                                                  //
                                                                                              //
        if n < Length(Wafer.Chip[Y, X].ChipParams) then                                       //
        begin                                                                                 //
          Delete(Str, 1, Pos('`', Str)); // Удалим номер сайта                                //
          Delete(Str, 1, Pos('`', Str)); // Удалим название параметра                         //
          Delete(Str, 1, Pos('`', Str)); // Удалим полное имя параметра                       //
                                                                                              //
          Delete(Str, 1, Pos('`', Str)); // Удалим passed/FAILED                              //
          Delete(Str, 1, Pos('`', Str)); // Удалим нижний предел                              //
          Str := Trim(Str);                                                                   //
          Str := Trim(Copy(Str, 1, Pos(' ', Str)-1));                                         //
          try                                                                                 //
            Wafer.Chip[Y, X].ChipParams[n].Value := StrToFloat(Str);                          //
          except                                                                              //
            Wafer.Chip[Y, X].ChipParams[n].Value := NotSpec;                                  //
          end;                                                                                //
                                                                                              //
          if Wafer.Chip[Y, X].ChipParams[n].Value <> NotSpec then                             //
          begin                                                                               //
            if Wafer.Chip[Y, X].Status < 10 then // Если не брак NK и FK                      //
              if (Wafer.Chip[Y, X].ChipParams[n].Value < Wafer.TestsParams[n].Norma.Min) or   //
                 (Wafer.Chip[Y, X].ChipParams[n].Value > Wafer.TestsParams[n].Norma.Max)      //
              then Wafer.Chip[Y, X].Status := 2000+n                                          //
              else Wafer.Chip[Y, X].Status := 1;                                              //

            Wafer.Chip[Y, X].ChipParams[n].Stat := GetChipParamsStat(Wafer.Chip[Y, X].ChipParams[n].Value, Wafer.TestsParams[n].Norma.Min, Wafer.TestsParams[n].Norma.Max);
          end;
        end;                                                                                  //
                                                                                              //
        Inc(n);                                                                               //
      end;                                                                                    //
    until Pos('TESTFLOW ENDED', UpperCase(Str)) <> 0;                                         //
  end;                                                                                        //
                                                                                              //
  Result := True;                                                                             //
                                                                                              //
  Wafer.SetChipsID;                                                                           //
  Wafer.CalcChips;                                                                            //
                                                                                              //
  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);                                               //
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);                                     //
  ChipsDlg.OnChipDlgClose := ChipDlgClose;                                                    //
                                                                                              //
  fSizeChipX := 0;                                                                            //
  fSizeChipY := 0;                                                                            //
  DrawWafer;                                                                                  //
  PBox.Repaint;                                                                               //
                                                                                              //
  if Assigned(OnWaferPainted) then OnWaferPainted(1);                                         //
                                                                                              //
  SL.Free;                                                                                    //
end;                                                                                          //
////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.DetectXLS(const fName: TFileName): byte;                                                               //
var                                                                                                                         //
  Ap: OleVariant;                                                                                                           //
begin                                                                                                                       //
  Result := 0;                                                                                                              //
                                                                                                                            //
  try                                                                                                                       //
    Ap := CreateOleObject('Excel.Application');                                                                             //
  except                                                                                                                    //
    MessageBox(Handle, 'Не удалось запустить MS Excel.', 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);                       //
    Exit;                                                                                                                   //
  end;                                                                                                                      //
                                                                                                                            //
  Ap.DisplayAlerts := False;                                                                                                //
  Ap.Workbooks.Open(fName, 0, True);                                                                                        //
                                                                                                                            //
  if AnsiLowerCase(Ap.Workbooks[1].Sheets[1].UsedRange.Cells[1, 1].Value) = 'pixan' then Result := 6                        //
  else                                                                                                                      //
    if (AnsiLowerCase(Ap.Workbooks[1].Sheets[1].UsedRange.Cells[1, 2].Value) = 'контакт') and                               //
       (AnsiLowerCase(Ap.Workbooks[1].Sheets[1].UsedRange.Cells[1, 8].Value) = 'параметр') then Result := 5; // Formula HF3 //
                                                                                                                            //
  Ap.Quit;                                                                                                                  //
end;                                                                                                                        //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function TStatistica.LoadXLS(const XLSfName: TFileName): Boolean;
var
  Ap, ActSheet, FData: OleVariant;
  aRows, SheetsCount, i, m, k, StrCount, ContCount, X, Y, X1, Y1: Integer;
  TmpStr: string;
begin
  Result := False;

  try
    Ap := CreateOleObject('Excel.Application');
  except
    MessageBox(Handle, 'Не удалось запустить MS Excel.', 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
    Exit;
  end;

  if Wafer <> nil then FreeAndNil(Wafer);
  Wafer := TWafer.Create;
  Wafer.fName := XLSfName;
  Wafer.MeasSystem := 'Formula HF3';
  Wafer.Direct := 2;

  Ap.DisplayAlerts := False;
  Ap.Workbooks.Open(XLSfName, 0, True);

  X := 0;
  Y := 0;
  StrCount := 0;
  SheetsCount := Ap.Workbooks[1].Sheets.Count;
  for i := 0 to SheetsCount-1 do
  begin
    ActSheet := Ap.Workbooks[1].Sheets[i+1]; // Кол-во листов
    aRows := ActSheet.UsedRange.Rows.Count;  // Кол-во строк
    FData := ActSheet.UsedRange.Value; // Считаем лист в массив

    ContCount := 0;
    with Wafer do
    begin
      if i = 0 then
      begin
        if SheetsCount = 1 then // Если однолистный файл
        begin
          for m := 3 to aRows do
            if FData[m, 1] = '0' then
            begin
              StrCount := m-2; // Найдем кол-во строк на чип
              Break;
            end;

          NTotal := aRows div StrCount; // Вычислим кол-во чипов
        end
        else                    // Если многолистный файл
        begin
          NTotal := SheetsCount;        // Вычислим кол-во чипов
          StrCount := aRows-2;
        end;
        SetLength(TestsParams, StrCount-1);


        X1 := Ceil(sqrt(NTotal));
        Y1 := X1;

        SetLength(Chip, 0, 0);
        SetLength(Chip, Y1, X1);
        for Y1 := 0 to Length(Chip)-1 do      // Очистим
          for X1 := 0 to Length(Chip[0])-1 do // массив
          begin                               // чипов
            Chip[Y1, X1].Status := 2;         //
            Chip[Y1, X1].ID     := 0;         //
            Chip[Y1, X1].ShowGr := 0;         //
            SetLength(Chip[Y1, X1].ChipParams, Length(TestsParams));
          end;                                //
      end;

      m := 2;
      while m < aRows do
      begin
        if Device = '' then Device := FData[m, 13]
        else
//          TmpStr := Device;
//          if TmpStr = '' then

          TmpStr := FData[m, 13];
          if TmpStr = '' then

          if Device <> FData[m, 13] then
          begin
            MessageBox(Handle, PChar('Несовпадение изделия на листе №'+IntToStr(i+1)), 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
//            if m = 0 then ;
//            if TmpStr = '' then

            Ap.Quit;
            Init;
            Exit;
          end;
        if NLot = '' then NLot := FData[m, 15]
        else
          if NLot <> FData[m, 15] then
          begin
//            MessageBox(Handle, PChar('Несовпадение № партии на листе №'+IntToStr(i+1)), 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
//            Ap.Quit;
//            Init;
//            Exit;
          end;
       if Num = '' then Num := FData[m, 16]
        else
          if Num <> FData[m, 16] then
          begin
//            MessageBox(Handle, PChar('Несовпадение № пластины на листе №'+IntToStr(i+1)), 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
//            Ap.Quit;
//            Init;
//            Exit;
          end;

        Inc(m);

        for k := 0 to Length(TestsParams)-1 do
        begin
          if (X = 0) and (Y = 0) then
          begin
            TestsParams[m-3].Name := FData[m, 3];

            TmpStr := Trim(FData[m, 4]);
            if TmpStr <> '' then TestsParams[m-3].Norma.Min := StrToFloat(TmpStr)
                            else TestsParams[m-3].Norma.Min := -NotSpec;
            TmpStr := Trim(FData[m, 5]);
            if TmpStr <> '' then TestsParams[m-3].Norma.Max := StrToFloat(TmpStr)
                            else TestsParams[m-3].Norma.Max := NotSpec;

          end;
          {
          try
            Chip[Y, X].ChipParams[k].Value := FData[m, 6];
          except
            Chip[Y, X].ChipParams[k].Value := NotSpec;
          end;
          }
          TmpStr := Trim(FData[m, 6]);
          if TmpStr <> '' then Chip[Y, X].ChipParams[k].Value := StrToFloat(TmpStr)
                          else Chip[Y, X].ChipParams[k].Value := NotSpec;

          if Chip[Y, X].ChipParams[k].Value <> NotSpec then
          begin
            if Chip[Y, X].Status < 2000 then
              if (Chip[Y, X].ChipParams[k].Value < TestsParams[k].Norma.Min) or
                 (Chip[Y, X].ChipParams[k].Value > TestsParams[k].Norma.Max)
              then Chip[Y, X].Status := 2000+k
              else Chip[Y, X].Status := 1;

            Chip[Y, X].ChipParams[k].Stat := GetChipParamsStat(Chip[Y, X].ChipParams[k].Value, TestsParams[k].Norma.Min, TestsParams[k].Norma.Max);
          end;

          Inc(m);
        end;

        if X = Length(Chip[0])-1 then
        begin
          Inc(Y);
          X := 0;
        end
        else Inc(X);
      end;
    end;
  end;

//  Str := Ap.Range['B1'];
//  Str := FData[1, 2];
//  MessageBox(0, PChar(Str), '123', MB_OK);
//  MessageBox(0, PChar('Sheets = '+IntToStr(SheetsCount)+'    Rows = '+IntToStr(aRows)+'    Columns = '+IntToStr(aColumns)), '123', MB_OK);

  Ap.Quit;

  Result := True;

  Wafer.SetChipsID;
  Wafer.CalcChips;

  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);
  ChipsDlg.OnChipDlgClose := ChipDlgClose;

  fSizeChipX := 0;
  fSizeChipY := 0;
  DrawWafer;
  PBox.Repaint;

  if Assigned(OnWaferPainted) then OnWaferPainted(1);
end;


function TStatistica.AddXLS(const XLSfName: TFileName): Boolean;
begin
  Result := False;
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadXLS2(const XLSfName: TFileName): Boolean;
var
  Ap, ActSheet, FData: OleVariant;
  aRows, SheetsCount, i, m, k, n, nStr, StrCount, ContCount, ChipsCount, X, Y, X1, Y1: Integer;
  TmpStr: string;
begin
  Result := False;

  try
    Ap := CreateOleObject('Excel.Application');
  except
    MessageBox(Handle, 'Не удалось запустить MS Excel.', 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
    Exit;
  end;

  if Wafer <> nil then FreeAndNil(Wafer);
  Wafer := TWafer.Create;
  Wafer.fName := XLSfName;
  Wafer.MeasSystem := 'Formula HF3';
  Wafer.Direct := 2;

  Ap.DisplayAlerts := False;
  Ap.Workbooks.Open(XLSfName, 0, True);

  X := 0;
  Y := 0;
  StrCount := 0;
  SheetsCount := Ap.Workbooks[1].Sheets.Count; // Кол-во листов
  for i := 0 to SheetsCount-1 do
  begin
    ActSheet := Ap.Workbooks[1].Sheets[i+1];
    aRows := ActSheet.UsedRange.Rows.Count;  // Кол-во строк
    FData := ActSheet.UsedRange.Value; // Считаем лист в массив

    Wafer.Device   := FData[2, 13];
    Wafer.NLot     := FData[2, 15];
    Wafer.Num      := FData[2, 16];
    Wafer.TimeDate := FData[2, 20];

    nStr := 0;
    ContCount := 0;
    n := 0;
    StrCount := 0;
    k := 0;
    ChipsCount := 0;
    for m := 2 to aRows do
    begin
      if Pos('контакт', AnsiLowerCase(FData[m, 3])) <> 0 then Inc(n); // Подсчет контактирований
      Inc(k); // Подсчет параметров

      if FData[m, 1] = '1' then
      begin
        if n > ContCount then ContCount := n; // Найдем макс. кол-во
        n := 0;                               // контактирований

        Inc(ChipsCount); // Найдем кол-во чипов

        if k > StrCount then
        begin
          StrCount := k; // Найдем макс. кол-во параметров
          nStr := m; // и номер строки с которой их считать
        end;
        k := 0;
      end;
    end;

    Wafer.NTotal := ChipsCount; // Кол-во чипов

    SetLength(Wafer.TestsParams, StrCount-1);
    for m := nStr-StrCount to nStr do
    begin
      Wafer.TestsParams[m-nStr+StrCount].Name := FData[m, 3];

      TmpStr := Trim(FData[m, 4]);
      if TmpStr <> '' then Wafer.TestsParams[m-nStr+StrCount].Norma.Min := StrToFloat(TmpStr)
                      else Wafer.TestsParams[m-nStr+StrCount].Norma.Min := -NotSpec;
      TmpStr := Trim(FData[m, 5]);
      if TmpStr <> '' then Wafer.TestsParams[m-nStr+StrCount].Norma.Max := StrToFloat(TmpStr)
                      else Wafer.TestsParams[m-nStr+StrCount].Norma.Max := NotSpec;
    end;

    X1 := Ceil(sqrt(Wafer.NTotal));
    Y1 := X1;
    SetLength(Wafer.Chip, 0, 0);
    SetLength(Wafer.Chip, Y1, X1);
    for Y1 := 0 to Length(Wafer.Chip)-1 do      // Очистим
      for X1 := 0 to Length(Wafer.Chip[0])-1 do // массив
      begin                                     // чипов
        Wafer.Chip[Y1, X1].Status := 2;         //
        Wafer.Chip[Y1, X1].ID     := 0;         //
        Wafer.Chip[Y1, X1].ShowGr := 0;         //
        SetLength(Wafer.Chip[Y1, X1].ChipParams, Length(Wafer.TestsParams));
      end;

    n := 0;
    for m := 3 to aRows do
    begin
      if FData[m, 1] = '1' then
      begin
        n := 0;

      end;



    end;








      m := 2;
      while m < aRows do
      begin
        if Wafer.Device = '' then Wafer.Device := FData[m, 13]
        else
          if Wafer.Device <> FData[m, 13] then
          begin
            MessageBox(Handle, PChar('Несовпадение изделия на листе №'+IntToStr(i+1)), 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);

            Ap.Quit;
            Init;
            Exit;
          end;

        Inc(m);

        for k := 0 to Length(Wafer.TestsParams)-1 do
        begin
          if (X = 0) and (Y = 0) then
          begin
            Wafer.TestsParams[m-3].Name := FData[m, 3];

            TmpStr := Trim(FData[m, 4]);
            if TmpStr <> '' then Wafer.TestsParams[m-3].Norma.Min := StrToFloat(TmpStr)
                            else Wafer.TestsParams[m-3].Norma.Min := -NotSpec;
            TmpStr := Trim(FData[m, 5]);
            if TmpStr <> '' then Wafer.TestsParams[m-3].Norma.Max := StrToFloat(TmpStr)
                            else Wafer.TestsParams[m-3].Norma.Max := NotSpec;

          end;
          {
          try
            Chip[Y, X].ChipParams[k].Value := FData[m, 6];
          except
            Chip[Y, X].ChipParams[k].Value := NotSpec;
          end;
          }
          TmpStr := Trim(FData[m, 6]);
          if TmpStr <> '' then Wafer.Chip[Y, X].ChipParams[k].Value := StrToFloat(TmpStr)
                          else Wafer.Chip[Y, X].ChipParams[k].Value := NotSpec;

          if Wafer.Chip[Y, X].ChipParams[k].Value <> NotSpec then
          begin
            if Wafer.Chip[Y, X].Status < 2000 then
              if (Wafer.Chip[Y, X].ChipParams[k].Value < Wafer.TestsParams[k].Norma.Min) or
                 (Wafer.Chip[Y, X].ChipParams[k].Value > Wafer.TestsParams[k].Norma.Max)
              then Wafer.Chip[Y, X].Status := 2000+k
              else Wafer.Chip[Y, X].Status := 1;

            Wafer.Chip[Y, X].ChipParams[k].Stat := GetChipParamsStat(Wafer.Chip[Y, X].ChipParams[k].Value, Wafer.TestsParams[k].Norma.Min, Wafer.TestsParams[k].Norma.Max);
          end;

          Inc(m);
        end;

        if X = Length(Wafer.Chip[0])-1 then
        begin
          Inc(Y);
          X := 0;
        end
        else Inc(X);
      end;
  end;

//  Str := Ap.Range['B1'];
//  Str := FData[1, 2];
//  MessageBox(0, PChar(Str), '123', MB_OK);
//  MessageBox(0, PChar('Sheets = '+IntToStr(SheetsCount)+'    Rows = '+IntToStr(aRows)+'    Columns = '+IntToStr(aColumns)), '123', MB_OK);

  Ap.Quit;

  Result := True;

  Wafer.SetChipsID;
  Wafer.CalcChips;

  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);
  ChipsDlg.OnChipDlgClose := ChipDlgClose;

  fSizeChipX := 0;
  fSizeChipY := 0;
  DrawWafer;
  PBox.Repaint;

  if Assigned(OnWaferPainted) then OnWaferPainted(1);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.LoadXLSPxn(const XLSfName: TFileName): Boolean;
var
  Ap, ActSheet, FData: OleVariant;
  aRows, aColumn, n, nChip, MinPos, MaxPos, X, Y: DWORD;
  tmpStr1, tmpStr2: string;
  P: byte;
begin
  Result := False;

  try
    Ap := CreateOleObject('Excel.Application');
  except
    MessageBox(Handle, 'Не удалось запустить MS Excel.', 'Ошибка!', MB_OK+MB_ICONERROR+MB_APPLMODAL);
    Exit;
  end;

  if Wafer <> nil then FreeAndNil(Wafer);
  Wafer := TWafer.Create;
  Wafer.fName := XLSfName;
  Wafer.MeasSystem := 'Пиксан';
//  Wafer.Direct := 2;
  Wafer.TimeDate := DateToStr(FileDateToDateTime(FileAge(XLSfName)));

  tmpStr1 := ExtractFileName(XLSfName);
  Delete(tmpStr1, Pos('.', tmpStr1), Length(tmpStr1));
  P := LastDelimiter('-', tmpStr1);
  if P > 0 then
    Wafer.NLot := Copy(tmpStr1, 1, P-1);
  for n := P+1 to Length(tmpStr1) do
    if not (tmpStr1[n] in ['0'..'9']) then Break;
  if n > P then
    Wafer.Num := Copy(tmpStr1, P+1, n-P-1);

//  Wafer.Diameter := 150;
//  Wafer.Device := '';

  Ap.DisplayAlerts := False;
  Ap.Workbooks.Open(XLSfName, 0, True);

  ActSheet := Ap.Workbooks[1].Sheets[1];
  aRows := ActSheet.UsedRange.Rows.Count; // Кол-во строк
  FData := ActSheet.UsedRange.Value; // Считаем лист в массив
  aColumn := ActSheet.UsedRange.Columns.Count ; // Кол-во столбцов

  SetLength(Wafer.TestsParams, aColumn-3);
  MinPos := 2;
  MaxPos := 3;
  tmpStr1 := FData[2, 1];
  tmpStr2 := FData[3, 1];
  if Pos('max', AnsiLowerCase(tmpStr1)) <> 0 then // Вверху Max
  begin
    MaxPos := 2;
    MinPos := 3;
  end;

  for n := 0 to Length(Wafer.TestsParams)-1 do
  begin
    Wafer.TestsParams[n].Name := FData[4, n+3]; // Строка с названиями параметров (x и y поменяны)

    TmpStr1 := Trim(FData[MinPos, n+3]); // Мин. норма параметра
    try
      Wafer.TestsParams[n].Norma.Min := StrToFloat(TmpStr1);
    except
      Wafer.TestsParams[n].Norma.Min := -NotSpec;
    end;

    TmpStr2 := Trim(FData[MaxPos, n+3]);
    try
      Wafer.TestsParams[n].Norma.Max := StrToFloat(TmpStr2); // Макс. норма параметра
    except
      Wafer.TestsParams[n].Norma.Max := NotSpec;
    end;
  end;

  Wafer.NTotal := aRows-4; // Кол-во чипов

  X := Ceil(sqrt(Wafer.NTotal)); // Сделаем квадратную
  Y := X;                        // карту обхода
  SetLength(Wafer.Chip, 0, 0);
  SetLength(Wafer.Chip, Y, X);
  nChip := 0;
  for Y := 0 to Length(Wafer.Chip)-1 do      // Очистим
    for X := 0 to Length(Wafer.Chip[0])-1 do // массив
    begin                                    // чипов
      Wafer.Chip[Y, X].Status := 2;          //
      Wafer.Chip[Y, X].ID     := 0;          //
      Wafer.Chip[Y, X].ShowGr := 0;          //
      Inc(nChip);
      if nChip <= Wafer.NTotal then
        SetLength(Wafer.Chip[Y, X].ChipParams, Length(Wafer.TestsParams));
    end;

  nChip := 0;
  for Y := 0 to Length(Wafer.Chip)-1 do
    for X := 0 to Length(Wafer.Chip[0])-1 do
    begin
      if nChip < Wafer.NTotal then
        for n := 0 to Length(Wafer.TestsParams)-1 do
        begin
          TmpStr1 := Trim(FData[nChip+5, n+3]);
          try
            Wafer.Chip[Y, X].ChipParams[n].Value := StrToFloat(TmpStr1);
          except
            Wafer.Chip[Y, X].ChipParams[n].Value := NotSpec;
          end;

          Wafer.Chip[Y, X].ChipParams[n].Stat := GetChipParamsStat(Wafer.Chip[Y, X].ChipParams[n].Value, Wafer.TestsParams[n].Norma.Min, Wafer.TestsParams[n].Norma.Max);

          if Wafer.Chip[Y, X].Status < 2000 then
            if Wafer.Chip[Y, X].ChipParams[n].Stat <> 1 then Wafer.Chip[Y, X].Status := 2000+n
            else Wafer.Chip[Y, X].Status := 1;
        end;

      Inc(nChip);
    end;

//  MessageBox(Handle, PAnsiChar(DateToStr(FileDateToDateTime(FileAge(XLSfName)))), '111!', MB_OK);

  Ap.Quit;

  Result := True;

  Wafer.SetChipsID;
  Wafer.CalcChips;

  if ChipsDlg <> nil then FreeAndNil(ChipsDlg);
  ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams);
  ChipsDlg.OnChipDlgClose := ChipDlgClose;

  fSizeChipX := 0;
  fSizeChipY := 0;
  DrawWafer;
  PBox.Repaint;

  if Assigned(OnWaferPainted) then OnWaferPainted(1);
end;
///////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////
procedure TStatistica.RotateWafer;                          //
begin                                                       //
  if Wafer <> nil then                                      //
  begin                                                     //
    Wafer.Rotate;                                           //
    fSizeChipX := 0;                                        //
    fSizeChipY := 0;                                        //
                                                            //
    if ChipsDlg <> nil then FreeAndNil(ChipsDlg);           //
    ChipsDlg := TChipsDlg.Create(self, @Wafer.TestsParams); //
    ChipsDlg.OnChipDlgClose := ChipDlgClose;                //
                                                            //
    DrawWafer;                                              //
    PBox.Repaint;                                           //
  end;                                                      //
end;                                                        //
//////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.PrintWafer: Boolean;                                                                                                  //
var                                                                                                                                        //
  Y, X, deltaX, deltaY, prnSizeChipX, prnSizeChipY, RealX, RealY, Max, MaxX, MaxY, Waf_Width, Waf_Height: WORD;                            //
  OffsX, OffsX1, OffsY, OffsY1: Integer;                                                                                                   //
  Tmp, Kx, Ky, X1, X2, Y1, Y2: Real;                                                                                                       //
  TmpStr: String[12];                                                                                                                      //
begin                                                                                                                                      //
  Result := True;                                                                                                                          //
                                                                                                                                           //
  MaxX := Length(Wafer.Chip[0]);                                                                                                           //
  MaxY := Length(Wafer.Chip);                                                                                                              //
                                                                                                                                           //
  Waf_Width  := Trunc(Printer.PageWidth-Printer.PageWidth/5);                                                                              //
  Waf_Height := Trunc(Printer.PageHeight/1.7);                                                                                             //
                                                                                                                                           //
  with Wafer do                                                                                                                            //
  begin                                                                                                                                    //
    deltaX := 0;                                                                                                                           //
    deltaY := 0;                                                                                                                           //
    if Cadre.ScaleX <> 0 then deltaX := MaxX div Cadre.ScaleX-1;                                                                           //
    if Cadre.ScaleY <> 0 then deltaY := MaxY div Cadre.ScaleY-1;                                                                           //
                                                                                                                                           //
    if (StepX <> 0) and (StepY <> 0) then                                                                                                  //
    begin                                                                                                                                  //
      prnSizeChipX := Trunc((Waf_Width -4*deltaX)*StepX/Diameter);                                                                         //
      prnSizeChipY := Trunc((Waf_Height-4*deltaY)*StepY/Diameter);                                                                         //
    end                                                                                                                                    //
    else                                                                                                                                   //
    begin                                                                                                                                  //
      prnSizeChipX := (Waf_Width -4*deltaX) div MaxX;                                                                                      //
      prnSizeChipY := (Waf_Height-4*deltaY) div MaxY;                                                                                      //
    end;                                                                                                                                   //
                                                                                                                                           //
    OffsX1 := 0;                                                                                                                           //
    OffsY1 := 0;                                                                                                                           //
    if (StepX <> 0) and (StepY <> 0) and (ViewAll) then                                                                                    //
    begin                                                                                                                                  //
      Kx := prnSizeChipX/StepX;                                                                                                            //
      Ky := prnSizeChipY/StepY;                                                                                                            //
                                                                                                                                           //
      case Wafer.CutSide of                                                                                                                //
        1: begin                                                                                                                           //
             X1 := Radius-Chord;                                                                                                           //
             X2 := Radius+Chord;                                                                                                           //
             Y1 := Radius-LRadius;                                                                                                         //
             Y2 := Radius-LRadius;                                                                                                         //
             OffsX1 := Round(Kx*((Diameter-MaxX*StepX)/2));                                                                                //
             OffsY1 := Round(Ky*((Diameter-(LDiameter+MaxY*StepY)/2)));                                                                    //
           end;                                                                                                                            //
        2: begin                                                                                                                           //
             X1 := Radius-LRadius;                                                                                                         //
             X2 := Radius-LRadius;                                                                                                         //
             Y1 := Radius+Chord;                                                                                                           //
             Y2 := Radius-Chord;                                                                                                           //
             OffsX1 := Round(Kx*((Diameter-(LDiameter+MaxX*StepX)/2)));                                                                    //
             OffsY1 := Round(Ky*((Diameter-MaxY*StepY)/2));                                                                                //
           end;                                                                                                                            //
        3: begin                                                                                                                           //
             X1 := Radius+Chord;                                                                                                           //
             X2 := Radius-Chord;                                                                                                           //
             Y1 := Radius+LRadius;                                                                                                         //
             Y2 := Radius+LRadius;                                                                                                         //
             OffsX1 := Round(Kx*((Diameter -MaxX*StepX)/2));                                                                               //
             OffsY1 := Round(Ky*((LDiameter-MaxY*StepY)/2));                                                                               //
           end;                                                                                                                            //
        4: begin                                                                                                                           //
             X1 := Radius+LRadius;                                                                                                         //
             X2 := Radius+LRadius;                                                                                                         //
             Y1 := Radius-Chord;                                                                                                           //
             Y2 := Radius+Chord;                                                                                                           //
             OffsX1 := Round(Kx*((LDiameter-MaxX*StepX)/2));                                                                               //
             OffsY1 := Round(Ky*((Diameter -MaxY*StepY)/2));                                                                               //
           end;                                                                                                                            //
      end;                                                                                                                                 //
    end;                                                                                                                                   //
  end;                                                                                                                                     //
                                                                                                                                           //
  OffsX := Round(Printer.PageWidth/10);                                                                                                    //
  OffsY := Round(Printer.PageHeight/30);                                                                                                   //
  try                                                                                                                                      //
    Printer.PrinterIndex := Printer.PrinterIndex;                                                                                          //
    Printer.Orientation := poPortrait;                                                                                                     //
                                                                                                                                           //
    Printer.BeginDoc;                                                                                                                      //
                                                                                                                                           //
    with Wafer, Printer.Canvas do                                                                                                          //
    begin                                                                                                                                  //
      if (StepX <> 0) and (StepY <> 0) then                                                                                                //
      begin                                                                                                                                //
        Pen.Color := clBlack;                                                                                                              //
        Pen.Width := 4;                                                                                                                    //
        Brush.Style := bsClear;                                                                                                            //
        Chord(OffsX,OffsY, OffsX+Round(Diameter*Kx)+4*deltaX,OffsY+Round(Diameter*Ky)+4*deltaY,                                            //
              OffsX+Round(Kx*X1)+4*deltaX div 2+1,OffsY+Round(Ky*Y1)+4*deltaY div 2+1,                                                     //
              OffsX+Round(Kx*X2)+4*deltaX div 2,  OffsY+Round(Ky*Y2)+4*deltaY div 2);                                                      //
        Brush.Style := bsSolid;                                                                                                            //
      end;                                                                                                                                 //
                                                                                                                                           //
      Font.Name := 'Courier New';                                                                                                          //
      Font.Color := clBlack;                                                                                                               //
      Font.Size := 10;                                                                                                                     //
      TextOut(TextWidth('WW'), 0, 'Файл: '+Wafer.fName);                                                                                   //
                                                                                                                                           //
/////////////////////////////////////////////////////// * Печать изображения пластины * /////////////////////////////////////////////////////
                                                                                                                                           //
      Font.Size := 6;                                                                                                                      //
      Pen.Width := 4;                                                                                                                      //
      Pen.Color := clBlack;                                                                                                                //
                                                                                                                                           //
      deltaX := 0;                                                                                                                         //
      deltaY := 0;                                                                                                                         //
      OffsX := OffsX+OffsX1;                                                                                                               //
      OffsY := OffsY+OffsY1;                                                                                                               //
      for Y := 0 to MaxY-1 do                                                                                                              //
      begin                                                                                                                                //
        if Cadre.ScaleY <> 0 then if (Y mod Cadre.ScaleY) = Cadre.StartY then Inc(deltaY, Pen.Width);                                      //
        if Y = 0 then deltaY := 0;                                                                                                         //
                                                                                                                                           //
        for X := 0 to MaxX-1 do                                                                                                            //
        begin                                                                                                                              //
          if Cadre.ScaleX <> 0 then if (X mod Cadre.ScaleX) = Cadre.StartX then Inc(deltaX, Pen.Width);                                    //
          if X = 0 then deltaX := 0;                                                                                                       //
                                                                                                                                           //
          if Chip[Y, X].Status <> 2 then // Печатаем только кристаллы                                                                      //
          begin                                                                                                                            //
            RealX := OffsX+deltaX+prnSizeChipX*X;                                                                                          //
            RealY := OffsY+deltaY+prnSizeChipY*Y;                                                                                          //
                                                                                                                                           //
            Brush.Color := GetPrnColor(Chip[Y, X].Status);                                                                                 //
            Rectangle(RealX, RealY, RealX+prnSizeChipX+1, RealY+prnSizeChipY+1);                                                           //
                                                                                                                                           //
            if prnSizeChipX > TextWidth('888') then // Если циферки умещаются, то печатаем                                                 //
            begin                                                                                                                          //
              RealX := RealX+prnSizeChipX div 15;                                                                                          //
              RealY := RealY+prnSizeChipY div 15;                                                                                          //
              if (Chip[Y, X].Status >= 2000) and (Chip[Y, X].Status <= 3000) then TextOut(RealX, RealY, IntToStr(Chip[Y, X].Status-1999)); //
              if (Chip[Y, X].Status >= 3500) and (Chip[Y, X].Status <= 4500) then TextOut(RealX, RealY, IntToStr(Chip[Y, X].Status-3499)); //
            end;                                                                                                                           //
          end;                                                                                                                             //
        end;                                                                                                                               //
      end;                                                                                                                                 //
                                                                                                                                           //
/////////////////////////////////////////////////////// * Печать данных по пластине * ///////////////////////////////////////////////////////
                                                                                                                                           //
      Font.Size := 8;                                                                                                                      //
      Brush.Color := clWhite;                                                                                                              //
                                                                                                                                           //
      OffsX := 7*TextWidth('W');                                                                                                           //
      OffsY := RealY+prnSizeChipY+2*TextHeight('W');                                                                                       //
                                                                                                                                           //
      TextOut(OffsX, OffsY,                    'Код:        '+Code);                                                                       //
      TextOut(OffsX, OffsY+ 1*TextHeight('W'), 'Изделие:    '+Device);                                                                     //
      TextOut(OffsX, OffsY+ 2*TextHeight('W'), 'Диаметр:    '+IntToStr(Diameter)+'мм');                                                    //
      TextOut(OffsX, OffsY+ 3*TextHeight('W'), 'Шаг по X:   '+FormatFloat('0.000', StepX)+'мм');                                           //
      TextOut(OffsX, OffsY+ 4*TextHeight('W'), 'Шаг по Y:   '+FormatFloat('0.000', StepY)+'мм');                                           //
      case CutSide of                                                                                                                      //
        1: TmpStr := 'вверху';                                                                                                             //
        2: TmpStr := 'слева';                                                                                                              //
        3: TmpStr := 'внизу';                                                                                                              //
        4: TmpStr := 'справа';                                                                                                             //
      else TmpStr := 'нет сведений';                                                                                                       //
      end;                                                                                                                                 //
      TextOut(OffsX, OffsY+ 5*TextHeight('W'), 'Срез:       '+TmpStr);                                                                     //
      TextOut(OffsX, OffsY+ 6*TextHeight('W'), 'Инфо:       '+Info);                                                                       //
      TextOut(OffsX, OffsY+ 7*TextHeight('W'), 'Партия:     '+NLot);                                                                       //
      TextOut(OffsX, OffsY+ 8*TextHeight('W'), 'Номер:      '+Num);                                                                        //
                                                                                                                                           //
      TextOut(OffsX, OffsY+10*TextHeight('W'), 'Раб. место: '+IntToStr(NWPlace));                                                          //
      TextOut(OffsX, OffsY+11*TextHeight('W'), 'Оператор:   '+Operator);                                                                   //
      TextOut(OffsX, OffsY+12*TextHeight('W'), 'Дата:       '+TimeDate);                                                                   //
                                                                                                                                           //
      TextOut(OffsX, OffsY+14*TextHeight('W'), 'Измерено:   '+IntToStr(NMeased));                                                          //
                                                                                                                                           //
      Brush.Color := clWhite;                                                                                                              //
      Rectangle(OffsX-3*TextWidth('W'), OffsY+15*TextHeight('W'), OffsX-TextWidth('W'), OffsY+16*TextHeight('W'));                         //
      Brush.Color := clWhite;                                                                                                              //
      TextOut(OffsX, OffsY+15*TextHeight('W'), 'Годные:     '+IntToStr(NOK));                                                              //
                                                                                                                                           //
      Brush.Color := clBlack;                                                                                                              //
      Rectangle(OffsX-3*TextWidth('W'), OffsY+16*TextHeight('W'), OffsX-TextWidth('W'), OffsY+17*TextHeight('W'));                         //
      Brush.Color := clWhite;                                                                                                              //
      TextOut(OffsX, OffsY+16*TextHeight('W'), 'Брак по НК: '+IntToStr(NFailNC));                                                          //
                                                                                                                                           //
      Brush.Color := clSilver;                                                                                                             //
      Rectangle(OffsX-3*TextWidth('W'), OffsY+17*TextHeight('W'), OffsX-TextWidth('W'), OffsY+18*TextHeight('W'));                         //
      Brush.Color := clWhite;                                                                                                              //
      TextOut(OffsX, OffsY+17*TextHeight('W'), 'Брак по СК: '+IntToStr(NFailSC));                                                          //
                                                                                                                                           //
      Brush.Color := clGray;                                                                                                               //
      Rectangle(OffsX-3*TextWidth('W'), OffsY+18*TextHeight('W'), OffsX-TextWidth('W'), OffsY+19*TextHeight('W'));                         //
      Brush.Color := clWhite;                                                                                                              //
      TextOut(OffsX, OffsY+18*TextHeight('W'), 'Брак по ФК: '+IntToStr(NFailFC));                                                          //
                                                                                                                                           //
      if NMeased-NFailNC <> 0 then Tmp := 100*NOK/(NMeased-NFailNC) else Tmp := 0.0;                                                       //
      TextOut(OffsX, OffsY+19*TextHeight('W'), 'Годн/Брак:  '+FormatFloat('0.00', Tmp)+'%');                                               //
                                                                                                                                           //
///////////////////////////////////////////////////////////// * Печать ПД * /////////////////////////////////////////////////////////////////
                                                                                                                                           //
      if Length(Wafer.PD) <> 0 then                                                                                                        //
      begin                                                                                                                                //
        OffsX := OffsX-3*TextWidth('W');                                                                                                   //
        OffsY := OffsY+21*TextHeight('W');                                                                                                 //
                                                                                                                                           //
        Font.Size := 6;                                                                                                                    //
        Pen.Width := 2;                                                                                                                    //
        prnSizeChipX := TextWidth('8888');                                                                                                 //
        prnSizeChipY := Round(1.1*TextHeight('8'));                                                                                        //
                                                                                                                                           //
        for Y := 0 to Length(PD) do                                                                                                        //
          for X := 0 to Length(PD[0]) do                                                                                                   //
          begin                                                                                                                            //
            RealX := 1+OffsX+X*prnSizeChipX;                                                                                               //
            RealY := 1+OffsY+Y*prnSizeChipY;                                                                                               //
                                                                                                                                           //
            if Y = 0 then                                                                                                                  //
            begin                                                                                                                          //
              RealX := RealX+prnSizeChipX div 4;                                                                                           //
              RealY := RealY+prnSizeChipY div 15;                                                                                          //
                                                                                                                                           //
              Font.Color  := clBlack;                                                                                                      //
              Brush.Color := clWhite;                                                                                                      //
              TextOut(RealX, RealY, IntToStr(X));                                                                                          //
              Continue;                                                                                                                    //
            end;                                                                                                                           //
            if X = 0 then                                                                                                                  //
            begin                                                                                                                          //
              RealX := RealX+prnSizeChipX div 4;                                                                                           //
              RealY := RealY+prnSizeChipY div 15;                                                                                          //
                                                                                                                                           //
              Font.Color  := clBlack;                                                                                                      //
              Brush.Color := clWhite;                                                                                                      //
              TextOut(RealX, RealY, IntToStr(Y));                                                                                          //
              Continue;                                                                                                                    //
            end;                                                                                                                           //
                                                                                                                                           //
            if PD[Y-1, X-1].OK = 0 then                                                                                                    //
            begin                                                                                                                          //
              Font.Color  := clWhite;                                                                                                      //
              Brush.Color := clBlack;                                                                                                      //
            end                                                                                                                            //
            else                                                                                                                           //
            begin                                                                                                                          //
              Font.Color  := clBlack;                                                                                                      //
              Brush.Color := clWhite;                                                                                                      //
            end;                                                                                                                           //
            Rectangle(RealX, RealY, RealX+prnSizeChipX, RealY+prnSizeChipY);                                                               //
                                                                                                                                           //
            RealX := RealX+prnSizeChipX div 15;                                                                                            //
            RealY := RealY+prnSizeChipY div 15;                                                                                            //
            TextOut(RealX, RealY, IntToStr(PD[Y-1, X-1].OK));                                                                              //
          end;                                                                                                                             //
      end;                                                                                                                                 //
                                                                                                                                           //
////////////////////////////////////////////////////// * Печать Браков по статике * /////////////////////////////////////////////////////////
                                                                                                                                           //
      Font.Size := 8;                                                                                                                      //
      Font.Color  := clBlack;                                                                                                              //
      Brush.Color := clWhite;                                                                                                              //
                                                                                                                                           //
      OffsX  := 40*TextWidth('W');                                                                                                         //
      OffsY  := OffsY-16*TextHeight('W');                                                                                                  //
      OffsX1 := OffsX;                                                                                                                     //
      OffsY1 := OffsY;                                                                                                                     //
      deltaX := TextWidth(' '); // Левое смещение символов относительно клетки                                                             //
      deltaY := TextHeight('W') div 12; // Верхнее смещение символов относительно клетки                                                   //
      prnSizeChipY := Round(1.3*TextHeight('W')); // Высота клетки                                                                         //
                                                                                                                                           //
      TextOut(OffsX+5*TextWidth('W'), OffsY-2*TextHeight('W'), 'Браки по СК ('+IntToStr(NFailSC)+')');                                     //
                                                                                                                                           //
      Max := TextWidth(' Название  ');                                                                                                     //
      if Length(FailsSC) > 0 then                                                                                                          //
      begin                                                                                                                                //
        for X := 0 to Length(FailsSC)-1 do                                                                                                 //
        begin                                                                                                                              //
          if FailsSC[X].Name = '' then FailsSC[X].Name := GetStatusString(FailsSC[X].Status);                                              //
          if TextWidth(FailsSC[X].Name)+TextWidth('   ') > Max then Max := TextWidth(FailsSC[X].Name)+TextWidth('   ');                    //
        end;                                                                                                                               //
      end;                                                                                                                                 //
                                                                                                                                           //
      Brush.Color := clSilver;                                                                                                             //
                                                                                                                                           //
      Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' N  '), OffsY1+prnSizeChipY);                                                            //
      TextOut(OffsX1+deltaX, OffsY1+deltaY, 'N');                                                                                          //
                                                                                                                                           //
      OffsX1 := OffsX1+TextWidth(' N  ');                                                                                                  //
      Rectangle(OffsX1, OffsY1, OffsX1+Max, OffsY1+prnSizeChipY);                                                                          //
      TextOut(OffsX1+deltaX,  OffsY1+deltaY, ' Название');                                                                                 //
                                                                                                                                           //
      OffsX1 := OffsX1+Max;                                                                                                                //
      Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' Кол-во  '), OffsY1+prnSizeChipY);                                                       //
      TextOut(OffsX1+deltaX, OffsY1+deltaY, ' Кол-во');                                                                                    //
                                                                                                                                           //
      Brush.Color := clWhite;                                                                                                              //
      if Length(FailsSC) > 0 then                                                                                                          //
        for X := 0 to Length(FailsSC)-1 do                                                                                                 //
        begin                                                                                                                              //
          OffsX1 := OffsX;                                                                                                                 //
          OffsY1 := OffsY1+prnSizeChipY;                                                                                                   //
          Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' N  '), OffsY1+prnSizeChipY);                                                        //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, IntToStr(FailsSC[X].Status-1999));                                                         //
                                                                                                                                           //
          OffsX1 := OffsX1+TextWidth(' N  ');                                                                                              //
          Rectangle(OffsX1, OffsY1, OffsX1+Max, OffsY1+prnSizeChipY);                                                                      //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, ' '+FailsSC[X].Name);                                                                      //
                                                                                                                                           //
          OffsX1 := OffsX1+Max;                                                                                                            //
          Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' Кол-во  '), OffsY1+prnSizeChipY);                                                   //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, ' '+IntToStr(FailsSC[X].Quantity));                                                        //
        end;                                                                                                                               //
                                                                                                                                           //
//////////////////////////////////////////////////////// * Печать Браков по ФК * ////////////////////////////////////////////////////////////
                                                                                                                                           //
      OffsX  := OffsX1+TextWidth(' Кол-во  ')+2*TextWidth('W');                                                                            //
      OffsX1 := OffsX;                                                                                                                     //
      OffsY1 := OffsY;                                                                                                                     //
                                                                                                                                           //
      TextOut(OffsX+5*TextWidth('W'), OffsY-2*TextHeight('W'), 'Браки по ФК ('+IntToStr(NFailFC)+')');                                     //
                                                                                                                                           //
      Max := TextWidth(' Название  ');                                                                                                     //
      if Length(FailsFC) > 0 then                                                                                                          //
      begin                                                                                                                                //
        for X := 0 to Length(FailsFC)-1 do                                                                                                 //
        begin                                                                                                                              //
          if FailsFC[X].Name = '' then FailsFC[X].Name := GetStatusString(FailsFC[X].Status);                                              //
          if TextWidth(FailsFC[X].Name)+TextWidth('   ') > Max then Max := TextWidth(FailsFC[X].Name)+TextWidth('   ');                    //
        end;                                                                                                                               //
      end;                                                                                                                                 //
                                                                                                                                           //
      Brush.Color := clGray;                                                                                                               //
                                                                                                                                           //
      Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' N  '), OffsY1+prnSizeChipY);                                                            //
      TextOut(OffsX1+deltaX, OffsY1+deltaY, 'N');                                                                                          //
                                                                                                                                           //
      OffsX1 := OffsX1+TextWidth(' N  ');                                                                                                  //
      Rectangle(OffsX1, OffsY1, OffsX1+Max, OffsY1+prnSizeChipY);                                                                          //
      TextOut(OffsX1+deltaX,  OffsY1+deltaY, ' Название');                                                                                 //
                                                                                                                                           //
      OffsX1 := OffsX1+Max;                                                                                                                //
      Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' Кол-во  '), OffsY1+prnSizeChipY);                                                       //
      TextOut(OffsX1+deltaX, OffsY1+deltaY, ' Кол-во');                                                                                    //
                                                                                                                                           //
      Brush.Color := clWhite;                                                                                                              //
      if Length(FailsFC) > 0 then                                                                                                          //
        for X := 0 to Length(FailsFC)-1 do                                                                                                 //
        begin                                                                                                                              //
          OffsX1 := OffsX;                                                                                                                 //
          OffsY1 := OffsY1+prnSizeChipY;                                                                                                   //
          Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' N  '), OffsY1+prnSizeChipY);                                                        //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, IntToStr(FailsFC[X].Status-3499));                                                         //
                                                                                                                                           //
          OffsX1 := OffsX1+TextWidth(' N  ');                                                                                              //
          Rectangle(OffsX1, OffsY1, OffsX1+Max, OffsY1+prnSizeChipY);                                                                      //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, ' '+FailsFC[X].Name);                                                                      //
                                                                                                                                           //
          OffsX1 := OffsX1+Max;                                                                                                            //
          Rectangle(OffsX1, OffsY1, OffsX1+TextWidth(' Кол-во  '), OffsY1+prnSizeChipY);                                                   //
          TextOut(OffsX1+deltaX, OffsY1+deltaY, ' '+IntToStr(FailsFC[X].Quantity));                                                        //
        end;                                                                                                                               //
    end;                                                                                                                                   //
                                                                                                                                           //
  finally                                                                                                                                  //
    Printer.EndDoc;                                                                                                                        //
  end;                                                                                                                                     //
end;                                                                                                                                       //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.DrawWafer(const ShowMode: byte=0);                                    //
var                                                                                         //
  y, x, MaxX, MaxY: WORD;                                                                   //
  OffsX, OffsY: Integer;                                                                    //
  Kx, Ky: Single;                                                                           //
  EdgeCoords: TEdgeCoords;                                                                  //
begin                                                                                       //
  if Length(Wafer.Chip) = 0 then Exit;

  MaxX := Length(Wafer.Chip[0]);                                                            //
  MaxY := Length(Wafer.Chip);                                                               //
                                                                                            //
  if (MaxX = 0) or (MaxY = 0) then Exit;                                                    //
                                                                                            //
  LastShowMode := ShowMode;                                                                 //
                                                                                            //
  with Wafer, EdgeCoords do                                                                 //
  begin                                                                                     //
//////////////////////////////////// * Autosize * ////////////////////////////////////////////
                                                                                            //
    if (fSizeChipX = 0) or (fSizeChipY = 0) then                                            //
    begin                                                                                   //
      if (StepX <> 0) and (StepY <> 0) and (ViewAll) then                                   //
      begin                                                                                 //
        fSizeChipX := Trunc(Width *StepX/Diameter);                                         //
        fSizeChipY := Trunc(Height*StepY/Diameter);                                         //
      end                                                                                   //
      else                                                                                  //
      begin                                                                                 //
        fSizeChipX := Width  div MaxX;                                                      //
        fSizeChipY := Height div MaxY;                                                      //
      end;                                                                                  //
                                                                                            //
      if fSizeChipX > SizeChipMax then fSizeChipX := SizeChipMax;                           //
      if fSizeChipX < 2 then fSizeChipX := 2;                                               //
      if fSizeChipY > SizeChipMax then fSizeChipY := SizeChipMax;                           //
      if fSizeChipY < 2 then fSizeChipY := 2;                                               //
    end;                                                                                    //
                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////
                                                                                            //
    if (StepX <> 0) and (StepY <> 0) and (ViewAll) then                                     //
    begin                                                                                   //
      Kx := fSizeChipX/StepX;                                                               //
      Ky := fSizeChipY/StepY;                                                               //
      WBitmap.Width  := Round(Diameter*Kx)+4;                                               //
      WBitmap.Height := Round(Diameter*Ky)+4;                                               //
                                                                                            //
      case CutSide of                                                                       //
        1: begin                                                                            //
             if Diameter = 150 then                                                         //
             begin                                                                          //
               X1 := Kx*(Radius-Chord);                                                     //
               X2 := Kx*(Radius+Chord);                                                     //
               Y1 := Ky*(Radius-LRadius);                                                   //
               Y2 := Y1;                                                                    //
             end;                                                                           //
             if Diameter = 200 then                                                         //
             begin                                                                          //
               X1 := WBitmap.Width div 2-WBitmap.Width div 60;                              //
               Y1 := 2;                                                                     //
               X2 := WBitmap.Width div 2+WBitmap.Width div 60;                              //
               Y2 := 2;                                                                     //
               X3 := WBitmap.Width div 2;                                                   //
               Y3 := 2+WBitmap.Height div 60;                                               //
             end;                                                                           //
             OffsX := Round(Kx*((Diameter-MaxX*StepX)/2))+1;                                //
             OffsY := Round(Ky*((Diameter-(LDiameter+MaxY*StepY)/2)))+1;                    //
           end;                                                                             //
        2: begin                                                                            //
             if Diameter = 150 then                                                         //
             begin                                                                          //
               X1 := Kx*(Radius-LRadius);                                                   //
               X2 := X1;                                                                    //
               Y1 := Ky*(Radius+Chord);                                                     //
               Y2 := Ky*(Radius-Chord);                                                     //
             end;                                                                           //
             if Diameter = 200 then                                                         //
             begin                                                                          //
               X1 := 2;                                                                     //
               Y1 := WBitmap.Height div 2+WBitmap.Height div 60;                            //
               X2 := 2;                                                                     //
               Y2 := WBitmap.Height div 2-WBitmap.Height div 60;                            //
               X3 := 2+WBitmap.Width div 60;                                                //
               Y3 := WBitmap.Height div 2;                                                  //
             end;                                                                           //
             OffsX := Round(Kx*((Diameter-(LDiameter+MaxX*StepX)/2)))+1;                    //
             OffsY := Round(Ky*((Diameter-MaxY*StepY)/2))+1;                                //
           end;                                                                             //
        3: begin                                                                            //
             if Diameter = 150 then                                                         //
             begin                                                                          //
               X1 := Kx*(Radius+Chord);                                                     //
               X2 := Kx*(Radius-Chord);                                                     //
               Y1 := Ky*(Radius+LRadius);                                                   //
               Y2 := Y1;                                                                    //
             end;                                                                           //
             if Diameter = 200 then                                                         //
             begin                                                                          //
               X1 := WBitmap.Width div 2+WBitmap.Width div 60;                              //
               Y1 := WBitmap.Height-2;                                                      //
               X2 := WBitmap.Width div 2-WBitmap.Width div 60;                              //
               Y2 := WBitmap.Height-2;                                                      //
               X3 := WBitmap.Width div 2;                                                   //
               Y3 := WBitmap.Height-2-WBitmap.Height div 60;                                //
             end;                                                                           //
             OffsX := Round(Kx*((Diameter -MaxX*StepX)/2))+1;                               //
             OffsY := Round(Ky*((LDiameter-MaxY*StepY)/2))+1;                               //
           end;                                                                             //
        4: begin                                                                            //
             if Diameter = 150 then                                                         //
             begin                                                                          //
               X1 := Kx*(Radius+LRadius);                                                   //
               X2 := X1;                                                                    //
               Y1 := Ky*(Radius-Chord);                                                     //
               Y2 := Ky*(Radius+Chord);                                                     //
             end;                                                                           //
             if Diameter = 200 then                                                         //
             begin                                                                          //
               X1 := WBitmap.Width-2;                                                       //
               Y1 := WBitmap.Height div 2-WBitmap.Height div 60;                            //
               X2 := WBitmap.Width-2;                                                       //
               Y2 := WBitmap.Height div 2+WBitmap.Height div 60;                            //
               X3 := WBitmap.Width-2-WBitmap.Width div 60;                                  //
               Y3 := WBitmap.Height div 2;                                                  //
             end;                                                                           //
             OffsX := Round(Kx*((LDiameter-MaxX*StepX)/2))+1;                               //
             OffsY := Round(Ky*((Diameter -MaxY*StepY)/2))+1;                               //
           end;                                                                             //
      end;                                                                                  //
    end                                                                                     //
    else                                                                                    //
//////////////////////////////////////////////////////////////////////////////////////////////
    begin                                                                                   //
      OffsX := 2;                                                                           //
      OffsY := 2;                                                                           //
      WBitmap.Width  := MaxX*fSizeChipX+4;                                                  //
      WBitmap.Height := MaxY*fSizeChipY+4;                                                  //
    end;                                                                                    //
                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////
                                                                                            //
    with WBitmap.Canvas do                                                                  //
    begin                                                                                   //
      Pen.Color   := Color;                                                                 //
      Pen.Width   := 1;                                                                     //
      Brush.Color := Color;                                                                 //
      Rectangle(Rect(0, 0, WBitmap.Width, WBitmap.Height));                                 //
    end;                                                                                    //
    PBox.Width  := WBitmap.Width;                                                           //
    PBox.Height := WBitmap.Height;                                                          //
                                                                                            //
/////////////////////////////////// * Рисуем кристаллы * /////////////////////////////////////
                                                                                            //
    for y := 0 to MaxY-1 do                                                                 //
      for x := 0 to MaxX-1 do                                                               //
      begin                                                                                 //
        Chip[y, x].Coord.X := OffsX+fSizeChipX*x;                                           //
        Chip[y, x].Coord.Y := OffsY+fSizeChipY*y;                                           //
        DrawChip(Point(x, y), GetColor(@Chip[y, x], ShowMode));                             //
      end;                                                                                  //
                                                                                            //
////////////////////////////////////// * Рисуем кадры * //////////////////////////////////////
                                                                                            //
    DrawCadre;                                                                              //
                                                                                            //
//////////////////////////////// * Рисуем границы пластины * /////////////////////////////////
                                                                                            //
    PrevEdgeCoords := EdgeCoords;                                                           //
    DrawEdge(PrevEdgeCoords);                                                               //
  end;                                                                                      //
end;                                                                                        //
//////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.DrawChip(const XY: TPoint; const cCol: TColor; const bCol: TColor=clGray);                                 //
begin                                                                                                                            //
  if not ViewGrid then                                                                                                           //
    if Wafer.Chip[XY.y, XY.x].Status in [2] then Exit;                                                                           //
                                                                                                                                 //
  with WBitmap.Canvas, Wafer do                                                                                                  //
  begin                                                                                                                          //
    Brush.Color := cCol;                                                                                                         //
    Pen.Color   := bCol;                                                                                                         //
    Pen.Width   := 1;                                                                                                            //
    if Chip[XY.y, XY.x].Status in [2] then Brush.Style := bsClear                                                                //
                                      else Brush.Style := bsSolid;                                                               //
    Rectangle(Chip[XY.y, XY.x].Coord.X,              Chip[XY.y, XY.x].Coord.Y,                                                   //
              Chip[XY.y, XY.x].Coord.X+fSizeChipX+1, Chip[XY.y, XY.x].Coord.Y+fSizeChipY+1);                                     //
                                                                                                                                 //
    if cCol <> clYellow then                                                                                                     //
      if (fSizeChipX > 13) and (fSizeChipY > 13) then                                                                            //
        case Chip[XY.y, XY.x].Status of                                                                                          //
          0         : ;                                                                                                          //
          1         : ; //TextOut(Chip[XY.y, XY.x].Coord.X+3, Chip[XY.y, XY.x].Coord.Y+2, 'G');                                    //
          2         : ;                                                                                                          //
          3         : ;                                                                                                          //
          4         : begin                                                                                                      //
                        Font.Height := 11;                                                                                       //
                        Font.Color := clYellow;                                                                                  //
                        Brush.Style := bsClear;                                                                                  //
                        TextOut(Chip[XY.y, XY.x].Coord.X+3, Chip[XY.y, XY.x].Coord.Y+2, 'M');                                    //
                        Brush.Style := bsSolid;                                                                                  //
                      end;                                                                                                       //
          5, 7      : begin                                                                                                      //
                        Font.Height := 11;                                                                                       //
                        Font.Color := clBlack;                                                                                   //
                        Brush.Style := bsClear;                                                                                  //
                        TextOut(Chip[XY.y, XY.x].Coord.X+3, Chip[XY.y, XY.x].Coord.Y+2, 'R');                                    //
                        Brush.Style := bsSolid;                                                                                  //
                      end;                                                                                                       //
          10..1500  : ;                                                                                                          //
          2000..3000: begin                                                                                                      //
                        Font.Height := 11;                                                                                       //
                        Font.Color := clWhite;                                                                                   //
                        Brush.Style := bsClear;                                                                                  //
                        TextOut(Chip[XY.y, XY.x].Coord.X+3, Chip[XY.y, XY.x].Coord.Y+2, IntToStr(Chip[XY.y, XY.x].Status-1999)); //
                        Brush.Style := bsSolid;                                                                                  //
                      end;                                                                                                       //
          3500..4500: begin                                                                                                      //
                        Font.Height := 11;                                                                                       //
                        Font.Color := clWhite;                                                                                   //
                        Brush.Style := bsClear;                                                                                  //
                        TextOut(Chip[XY.y, XY.x].Coord.X+3, Chip[XY.y, XY.x].Coord.Y+2, IntToStr(Chip[XY.y, XY.x].Status-3499)); //
                        Brush.Style := bsSolid;                                                                                  //
                      end;                                                                                                       //
        end;                                                                                                                     //
  end;                                                                                                                           //
end;                                                                                                                             //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.DrawCadre;                                                                  //
var                                                                                               //
  y, x, MaxY, MaxX: WORD;                                                                         //
begin                                                                                             //
  MaxX := Length(Wafer.Chip[0]);                                                                  //
  MaxY := Length(Wafer.Chip);                                                                     //
                                                                                                  //
  if (MaxX = 0) or (MaxY = 0) then Exit;                                                          //
  if (Wafer.Cadre.ScaleY = 0) or (Wafer.Cadre.ScaleX = 0) then Exit;                              //
                                                                                                  //
  with WBitmap.Canvas, Wafer do                                                                   //
  begin                                                                                           //
    x := Cadre.StartX;                                                                            //
    y := Cadre.StartY;                                                                            //
    with WBitmap.Canvas do                                                                        //
    begin                                                                                         //
      Pen.Color := clGrid;                                                                        //
      Pen.Width := 1;                                                                             //
                                                                                                  //
      while x < MaxX do                                                                           //
      begin                                                                                       //
        MoveTo(Chip[0, x].Coord.X, Chip[0, x].Coord.Y);                                           //
        LineTo(Chip[MaxY-1, x].Coord.X, Chip[MaxY-1, x].Coord.Y+fSizeChipY);                      //
                                                                                                  //
        Inc(x, Cadre.ScaleX);                                                                     //
      end;                                                                                        //
      if x = MaxX then                                                                            //
      begin                                                                                       //
        MoveTo(Chip[0, MaxX-1].Coord.X+fSizeChipX, Chip[0, MaxX-1].Coord.Y);                      //
        LineTo(Chip[MaxY-1, MaxX-1].Coord.X+fSizeChipX, Chip[MaxY-1, MaxX-1].Coord.Y+fSizeChipY); //
      end;                                                                                        //
                                                                                                  //
      while y < MaxY do                                                                           //
      begin                                                                                       //
        MoveTo(Chip[y, 0].Coord.X, Chip[y, 0].Coord.Y);                                           //
        LineTo(Chip[y, MaxX-1].Coord.X+fSizeChipX, Chip[y, MaxX-1].Coord.Y);                      //
                                                                                                  //
        Inc(y, Cadre.ScaleY);                                                                     //
      end;                                                                                        //
      if y = MaxY then                                                                            //
      begin                                                                                       //
        MoveTo(Chip[MaxY-1, 0].Coord.X, Chip[MaxY-1, 0].Coord.Y+fSizeChipY);                      //
        LineTo(Chip[MaxY-1, MaxX-1].Coord.X+fSizeChipX, Chip[MaxY-1, MaxX-1].Coord.Y+fSizeChipY); //
      end;                                                                                        //
    end;                                                                                          //
  end;                                                                                            //
end;                                                                                              //
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
procedure TStatistica.DrawEdge(const EdgeCoords: TEdgeCoords);    //
begin                                                             //
  if (Wafer.StepX <> 0) and (Wafer.StepY <> 0) and (ViewAll) then //
    with WBitmap.Canvas, EdgeCoords do                            //
    begin                                                         //
      Pen.Color   := $005E0007;                                   //
      Brush.Style := bsClear;                                     //
      Pen.Width := 2;                                             //
      case Wafer.Diameter of                                      //
        150: begin                                                //
               Chord(2,2, WBitmap.Width-2,WBitmap.Height-2,       //
                     Round(X1)+2,Round(Y1)+2,                     //
                     Round(X2)+2,Round(Y2)+2);                    //
             end;                                                 //
        200: begin                                                //
               Arc(2,2, WBitmap.Width-2,WBitmap.Height-2,         //
                   Round(X1),Round(Y1),                           //
                   Round(X2),Round(Y2));                          //
               MoveTo(Round(X1), Round(Y1));                      //
               LineTo(Round(X3), Round(Y3));                      //
               LineTo(Round(X2), Round(Y2));                      //
             end;                                                 //
      end;                                                        //
                                                                  //
      if not ViewGrid then                                        //
      begin                                                       //
        Brush.Color := Color;                                     //
        FloodFill(1,1, $005E0007, fsBorder);                      //
      end;                                                        //
    end;                                                          //
end;                                                              //
////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////
function TStatistica.GetColor(const Chp: PChip; const ShowMode: byte=0): TColor;  //
                                                                                  //
///////////////////////////////////////////////////////////////////////////////// //
  function GetColorParams(const Ch: PChip; const DefCol: TColor): TColor;      // //
  var                                                                          // //
    n: WORD;                                                                   // //
  begin                                                                        // //
    Result := DefCol;                                                          // //
                                                                               // //
    if Length(Wafer.ColorParams) > 0 then                                      // //
      for n := 0 to Length(Wafer.ColorParams)-1 do                             // //
        with Wafer.ColorParams[n] do                                           // //
        begin                                                                  // //
          if (Min = -NotSpec) and (Max <> NotSpec) then                        // //
            if Ch^.ChipParams[Num].Value > Max then Continue;                             // //
                                                                               // //
          if (Min <> -NotSpec) and (Max = NotSpec) then                        // //
            if Ch^.ChipParams[Num].Value < Min then Continue;                             // //
                                                                               // //
          if (Min <> -NotSpec) and (Max <> NotSpec) then                       // //
            if (Ch^.ChipParams[Num].Value < Min) or (Ch^.ChipParams[Num].Value > Max) then Continue; // //
                                                                               // //
          Result := Col;                                                       // //
        end;                                                                   // //
  end;                                                                         // //
///////////////////////////////////////////////////////////////////////////////// //
                                                                                  //
var                                                                               //
  n: WORD;                                                                        //
begin                                                                             //
  case Chp^.Status of                                                             //
    0         : Result := clNotTested;                                            //
    1         : begin                                                             //
                  Result := clOK;                                                 //
                                                                                  //
                  if ShowMode = 1 then Result := GetColorParams(Chp, Result);     //
                  if ShowMode = 2 then                                            //
                    if Chp^.ShowGr = ShowGroup then Result := clShowChips;        //
                end;                                                              //
    2         : Result := clNotChip;                                              //
    3         : Result := clNot4Testing;                                          //
    4         : Result := cl4Mark;                                                //
    5         : Result := clRepper;                                               //
    7         : Result := clMRepper;                                              //
    10..1500  : Result := clFailNC;                                               //
    2000..3000: begin                                                             //
                  Result := clFailSC;                                             //
                                                                                  //
                  if ShowMode = 1 then                                            //
                  begin                                                           //
                    Result := GetColorParams(Chp, Result);                        //
                    Exit;                                                         //
                  end;                                                            //
                  if ShowMode = 2 then                                            //
                  begin                                                           //
                    if Chp^.ShowGr = ShowGroup then Result := clShowChips;        //
                    Exit;                                                         //
                  end;                                                            //
                                                                                  //
                  if Length(Wafer.FailsSC) > 0 then                               //
                    for n := 0 to Length(Wafer.FailsSC)-1 do                      //
                      if Chp^.Status = Wafer.FailsSC[n].Status then               //
                      begin                                                       //
                        Result := Wafer.FailsSC[n].Col;                           //
                        Exit;                                                     //
                      end;                                                        //
                end;                                                              //
    3500..4500: begin                                                             //
                  Result := clFailFC;                                             //
                                                                                  //
                  if ShowMode = 1 then                                            //
                  begin                                                           //
                    Result := GetColorParams(Chp, Result);                        //
                    Exit;                                                         //
                  end;                                                            //
                  if ShowMode = 2 then                                            //
                  begin                                                           //
                    if Chp^.ShowGr = ShowGroup then Result := clShowChips;        //
                    Exit;                                                         //
                  end;                                                            //
                                                                                  //
                  if Length(Wafer.FailsFC) > 0 then                               //
                    for n := 0 to Length(Wafer.FailsFC)-1 do                      //
                      if Chp^.Status = Wafer.FailsFC[n].Status then               //
                      begin                                                       //
                        Result := Wafer.FailsFC[n].Col;                           //
                        Exit;                                                     //
                      end;                                                        //
                end;                                                              //
    else        Result := clGray;                                                 //
  end;                                                                            //
end;                                                                              //
////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////
function TStatistica.GetChipCoord(const X, Y: Integer): TPoint;                          //
var                                                                                      //
  XX, YY: WORD;                                                                          //
begin                                                                                    //
  Result.X := -1;                                                                        //
  Result.Y := -1;                                                                        //
                                                                                         //
  with Wafer do                                                                          //
    for YY := 0 to Length(Wafer.Chip)-1 do                                               //
      for XX := 0 to Length(Wafer.Chip[0])-1 do                                          //
        if (Chip[YY, XX].Coord.X-1 < X) and (Chip[YY, XX].Coord.X+fSizeChipX+1 > X) and  //
           (Chip[YY, XX].Coord.Y-1 < Y) and (Chip[YY, XX].Coord.Y+fSizeChipY+1 > Y) then //
        begin                                                                            //
          Result.X := XX;                                                                //
          Result.Y := YY;                                                                //
          Exit;                                                                          //
        end;                                                                             //
end;                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////
procedure TStatistica.ShowBaseChip;                                //
begin                                                              //
  if Wafer <> nil then                                             //
  begin                                                            //
    DrawChip(Wafer.BaseChip, clBaseChip, clFuchsia);               //
                                                                   //
    PBox.Repaint;                                                  //
  end;                                                             //
end;                                                               //
/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
procedure TStatistica.HideBaseChip;                                //
begin                                                              //
  if Wafer <> nil then                                             //
    with Wafer do                                                  //
    begin                                                          //
      DrawChip(BaseChip, GetColor(@Chip[BaseChip.Y, BaseChip.X])); //
      DrawCadre;                                                   //
      DrawEdge(PrevEdgeCoords);                                    //
                                                                   //
      PBox.Repaint;                                                //
    end;                                                           //
end;                                                               //
/////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.SetSizeChipX(const Value: WORD);                             //
begin                                                                              //
  if Wafer <> nil then                                                             //
  begin                                                                            //
    if (Value = 1) or (Value > SizeChipMax) then Exit;                             //
                                                                                   //
    fSizeChipX := Value;                                                           //
                                                                                   //
    Repaint(LastShowMode);                                                         //
  end;                                                                             //
end;                                                                               //
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.SetSizeChipY(const Value: WORD);                             //
begin                                                                              //
  if Wafer <> nil then                                                             //
  begin                                                                            //
    if (Value = 1) or (Value > SizeChipMax) then Exit;                             //
                                                                                   //
    fSizeChipY := Value;                                                           //
                                                                                   //
    Repaint(LastShowMode);                                                         //
  end;                                                                             //
end;                                                                               //
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.IncSizeChipXY;                                               //
begin                                                                              //
  if Wafer <> nil then                                                             //
  begin                                                                            //
    if (fSizeChipX > (SizeChipMax-1)) or (fSizeChipY > (SizeChipMax-1)) then Exit; //
                                                                                   //
    fSizeChipX := fSizeChipX+1;                                                    //
    fSizeChipY := fSizeChipY+1;                                                    //
                                                                                   //
    Repaint(LastShowMode);                                                         //
  end;                                                                             //
end;                                                                               //
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.DecSizeChipXY;                                               //
begin                                                                              //
  if Wafer <> nil then                                                             //
  begin                                                                            //
    if (fSizeChipX < 3) or (fSizeChipY < 3) then Exit;                             //
                                                                                   //
    fSizeChipX := fSizeChipX-1;                                                    //
    fSizeChipY := fSizeChipY-1;                                                    //
                                                                                   //
    Repaint(LastShowMode);                                                         //
  end;                                                                             //
end;                                                                               //
/////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);                           //
                                                                                                                                         //
////////////////////////////////////////////////////////////////////////////////////////////                                             //
  function CalculateLeft(TP: TPoint; DWidth: Integer): Integer;                           //                                             //
  var                                                                                     //                                             //
    T: Integer;                                                                           //                                             //
  begin                                                                                   //                                             //
    if X > (self.Width/2) then T := -(DWidth)                                             //                                             //
                          else T := SizeChipX;                                            //                                             //
    Result := GetClientOrigin.X+Wafer.Chip[TP.Y, TP.X].Coord.X+T-HorzScrollBar.ScrollPos; //                                             //
  end;                                                                                    //                                             //
////////////////////////////////////////////////////////////////////////////////////////////                                             //
////////////////////////////////////////////////////////////////////////////////////////////                                             //
  function CalculateTop(TP: TPoint; DHeigh: Integer): Integer;                            //                                             //
  var                                                                                     //                                             //
    T: Integer;                                                                           //                                             //
  begin                                                                                   //                                             //
    if Y > (self.Height/2) then T := -(DHeigh)                                            //                                             //
                           else T := SizeChipY;                                           //                                             //
    Result := GetClientOrigin.Y+Wafer.Chip[TP.Y, TP.X].Coord.Y+T-VertScrollBar.ScrollPos; //                                             //
  end;                                                                                    //                                             //
////////////////////////////////////////////////////////////////////////////////////////////                                             //
                                                                                                                                         //
var                                                                                                                                      //
  TmpPoint, WidthAndHeight: TPoint;                                                                                                      //
begin                                                                                                                                    //
  if Wafer <> nil then                                                                                                                   //
  begin                                                                                                                                  //
    if Shift = [ssLeft] then // Левая кнопка                                                                                             //
      with Wafer do                                                                                                                      //
      begin                                                                                                                              //
        TmpPoint := GetChipCoord(X, Y);                                                                                                  //
        if TmpPoint.X = -1 then Exit;                                                                                                    //
        if Chip[TmpPoint.Y, TmpPoint.X].Status in [0,2,3,4,5,7] then Exit;                                                               //
                                                                                                                                         //
        if (HLChip.X <> -1) and (HLChip.Y <> -1) then                                                                                    //
        begin                                                                                                                            //
          DrawChip(HLChip, GetColor(@Chip[HLChip.Y, HLChip.X], LastShowMode));                                                           //
          DrawCadre;                                                                                                                     //
          DrawEdge(PrevEdgeCoords);                                                                                                      //
        end;                                                                                                                             //
                                                                                                                                         //
        DrawChip(TmpPoint, clCurChip, clFuchsia);                                                                                        //
        HLChip := TmpPoint;                                                                                                              //
                                                                                                                                         //
        PBox.Repaint;                                                                                                                    //
                                                                                                                                         //
        with ChipsDlg do                                                                                                                 //
        begin                                                                                                                            //
          Caption := 'Кристалл N'+IntToStr(Chip[TmpPoint.Y, TmpPoint.X].ID)+' ('+IntToStr(TmpPoint.X+1)+', '+IntToStr(TmpPoint.Y+1)+')'; //
          WidthAndHeight := PreShowChip(Chip[TmpPoint.Y, TmpPoint.X], GetStatusName(Chip[TmpPoint.Y, TmpPoint.X].Status));               //
          if not ChipsDlg.Visible then                                                                                                   //
          begin                                                                                                                          //
            Left := CalculateLeft(TmpPoint, WidthAndHeight.X);                                                                           //
            Top  := CalculateTop (TmpPoint, WidthAndHeight.Y);                                                                           //
          end;                                                                                                                           //
                                                                                                                                         //
          ChipsDlg.Visible := True;                                                                                                      //
          SetForegroundWindow(ChipsDlg.Handle);                                                                                          //
          ChipsDlg.SetFocus;                                                                                                             //
        end;                                                                                                                             //
      end;                                                                                                                               //
                                                                                                                                         //
    if Shift = [ssRight] then ; // Правая кнопка                                                                                         //
  end;                                                                                                                                   //
end;                                                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TStatistica.PBoxPaint(Sender: TObject);                                                                                        //
begin                                                                                                                                    //
  PBox.Canvas.Draw(0,0, WBitmap);                                                                                                        //
end;                                                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////
procedure TStatistica.Repaint(const ShowMode: byte=0); //
begin                                                  //
  if Wafer <> nil then DrawWafer(ShowMode);            //
                                                       //
  inherited Repaint;                                   //
end;                                                   //
/////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////
procedure TStatistica.ChipDlgClose;                          //
begin                                                        //
  if Wafer <> nil then                                       //
    with Wafer do                                            //
    begin                                                    //
      DrawChip(HLChip, GetColor(@Chip[HLChip.Y, HLChip.X])); //
      DrawCadre;                                             //
      DrawEdge(PrevEdgeCoords);                              //
                                                             //
      PBox.Repaint;                                          //
                                                             //
      HLChip.X := -1;                                        //
      HLChip.Y := -1;                                        //
    end;                                                     //
end;                                                         //
///////////////////////////////////////////////////////////////


end.

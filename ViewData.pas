unit ViewData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Structs, Statistica, Grids, StdCtrls, Buttons, ExtCtrls, EditData;
//  DoubleTrackBar;

type
  TStParams = record
    Min: Single;
    Avr: Single;
    Max: Single;
  end;

  TAddParams = record
    All: TStParams;
    OK : TStParams
  end;

  TDataDlg = class(TForm)
    HandlerSB: TScrollBox;
    HTestsLab: TStaticText;
    HMinLab: TStaticText;
    HColLab: TStaticText;
    MainSB: TScrollBox;
    HMaxLab: TStaticText;
    TestsCB_0: TComboBox;
    MinLab_0 : TStaticText;
    MaxLab_0 : TStaticText;
    MinLabl_0: TStaticText;
    MaxLabl_0: TStaticText;
    ColLab_0 : TStaticText;
    AddBtn: TSpeedButton;
    DelBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    LoadBtn: TSpeedButton;
    AcceptBtn: TSpeedButton;
    ResetBtn: TSpeedButton;
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure MinMaxLab_Click(Sender: TObject);
    procedure ColLab_Click(Sender: TObject);
    procedure TestsCB_Change(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
  private
    pStatistica: PStatistica;
    fFilePath: TFileName;

    AddParams: array of TAddParams;

    TestsCB: array of TComboBox;
    MinLabl, MinLab, MaxLabl, MaxLab, ColLab: array of TStaticText;
//    DTrackBar: array of TDoubleTrackBar;
    TotalHeight: WORD;

    procedure Init;
    procedure FreeAll;
    function  AddNextParams: WORD;
    function  DeleteLastParams: WORD;
    procedure CalcParams;

    procedure CopyCBParams(var CB: TComboBox);
    procedure CopyLabParams(var Lab: TStaticText; const LabType: byte);

    function NormToStr(const Val: Single): String;
    function StrToMaxNorm(const Str: String): Single;
    function StrToMinNorm(const Str: String): Single;
  public
    constructor Create(Sender: TObject; pStat: PStatistica; const fName: TFileName);
    destructor  Destroy; override;
  end;

var
  DataDlg: TDataDlg;

implementation

{$R *.dfm}

{ TDataDlg }

////////////////////////////////////////////////////////////////////////////////////////////
constructor TDataDlg.Create(Sender: TObject; pStat: PStatistica; const fName: TFileName); //
var                                                                                       //
  n: WORD;                                                                                //
begin                                                                                     //
  inherited Create(TComponent(Sender));                                                   //
                                                                                          //
  pStatistica := pStat;                                                                   //
  fFilePath := ExtractFilePath(fName);                                                    //
                                                                                          //
  Init;                                                                                   //
                                                                                          //
  with pStatistica^.Wafer do                                                              //
    if Length(ColorParams) > 0 then                                                       //
      for n := 0 to Length(ColorParams)-1 do                                              //
      begin                                                                               //
        if n <> 0 then AddNextParams;                                                     //
                                                                                          //
        TestsCB[n].ItemIndex := ColorParams[n].Num;                                       //
        MinLab[n].Caption := NormToStr(ColorParams[n].Min);                               //
        MaxLab[n].Caption := NormToStr(ColorParams[n].Max);                               //
        ColLab[n].Color   := ColorParams[n].Col;                                          //
      end                                                                                 //
    else                                                                                  //
    begin                                                                                 //
      MinLab[0].Caption := NormToStr(TestsParams[0].Norma.Min);                           //
      MaxLab[0].Caption := NormToStr(TestsParams[0].Norma.Max);                           //
      ColLab[0].Color   := clLime;                                                        //
    end;                                                                                  //
                                                                                          //
  CalcParams;                                                                             //
end;                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
destructor TDataDlg.Destroy;                                                              //
begin                                                                                     //
  FreeAll;                                                                                //
                                                                                          //
  TestsCB[0] := nil;                                                                      //
  MinLabl[0] := nil;                                                                      //
  MinLab[0]  := nil;                                                                      //
  MaxLabl[0] := nil;                                                                      //
  MaxLab[0]  := nil;                                                                      //
  ColLab[0]  := nil;                                                                      //
                                                                                          //
  pStatistica := nil;                                                                     //
                                                                                          //
  inherited Destroy;                                                                      //
end;                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////
procedure TDataDlg.TestsCB_Change(Sender: TObject);              //
var                                                              //
  NumP, n: WORD;                                                 //
begin                                                            //
  NumP := TComboBox(Sender).ItemIndex;                           //
  n    := TComboBox(Sender).Tag;                                 //
                                                                 //
  with pStatistica^.Wafer do                                     //
  begin                                                          //
    MinLab[n].Caption := NormToStr(TestsParams[NumP].Norma.Min); //
    MaxLab[n].Caption := NormToStr(TestsParams[NumP].Norma.Max); //
    ColLab[n].Color   := clLime;                                 //
  end;                                                           //
end;                                                             //
///////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
procedure TDataDlg.MinMaxLab_Click(Sender: TObject);                     //
var                                                                      //
  NumP, n: WORD;                                                         //
begin                                                                    //
  n    := TStaticText(Sender).Tag;                                       //
  NumP := TestsCB[n].ItemIndex;                                          //
                                                                         //
  EDitDataDlg := TEDitDataDlg.Create(self);                              //
  with EDitDataDlg do                                                    //
  begin                                                                  //
    if MinLab[n].Caption = 'нет' then MinEdit.Text := ''                 //
                                 else MinEdit.Text := MinLab[n].Caption; //
    if MaxLab[n].Caption = 'нет' then MaxEdit.Text := ''                 //
                                 else MaxEdit.Text := MaxLab[n].Caption; //
    with pStatistica^.Wafer do                                           //
    begin                                                                //
      NormaMinLab.Caption := NormToStr(TestsParams[NumP].Norma.Min);     //
      NormaMaxLab.Caption := NormToStr(TestsParams[NumP].Norma.Max);     //
    end;                                                                 //
    OKMinLab.Caption  := FormatFloat('0.000', AddParams[NumP].OK.Min);   //
    OKAvrLab.Caption  := FormatFloat('0.000', AddParams[NumP].OK.Avr);   //
    OKMaxLab.Caption  := FormatFloat('0.000', AddParams[NumP].OK.Max);   //
    AllMinLab.Caption := FormatFloat('0.000', AddParams[NumP].All.Min);  //
    AllAvrLab.Caption := FormatFloat('0.000', AddParams[NumP].All.Avr);  //
    AllMaxLab.Caption := FormatFloat('0.000', AddParams[NumP].All.Max);  //
                                                                         //
    Top  := self.Top+self.Height;                                        //
    Left := self.Left;                                                   //
                                                                         //
    ShowModal;                                                           //
                                                                         //
    if StrMinVal <> 'NE' then                                            //
      if StrMinVal <> '' then MinLab[n].Caption := StrMinVal             //
                         else MinLab[n].Caption := 'нет';                //
                                                                         //
    if StrMaxVal <> 'NE' then                                            //
      if StrMaxVal <> '' then MaxLab[n].Caption := StrMaxVal             //
                         else MaxLab[n].Caption := 'нет';                //
    Free;                                                                //
  end;                                                                   //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
procedure TDataDlg.ColLab_Click(Sender: TObject);                        //
var                                                                      //
  ColorDlg: TColorDialog;                                                //
begin                                                                    //
  ColorDlg := TColorDialog.Create(self);                                 //
  with ColorDlg do                                                       //
  begin                                                                  //
    Color := TLabel(Sender).Color;                                       //
    if Execute then TLabel(Sender).Color := Color;                       //
    Free;                                                                //
  end;                                                                   //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
procedure TDataDlg.AddBtnClick(Sender: TObject);                    //
var                                                                 //
  Num: WORD;                                                        //
begin                                                               //
  Num := AddNextParams;                                             //
                                                                    //
  TestsCB_Change(TestsCB[Num]);                                     //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TDataDlg.DelBtnClick(Sender: TObject);                    //
begin                                                               //
  DeleteLastParams;                                                 //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TDataDlg.LoadBtnClick(Sender: TObject);                   //
                                                                    //
/////////////////////////////////////////////////////////////////   //
  function GetColorParams(Str: String): TColorParams;          //   //
  begin                                                        //   //
    if Pos('=', Str) > 0 then Delete(Str, 1, Pos('=', Str));   //   //
    try                                                        //   //
      Result.Num := StrToInt(Copy(Str, 1, Pos(';', Str)-1));   //   //
    except                                                     //   //
      Result.Num := 65535;                                     //   //
      Exit;                                                    //   //
    end;                                                       //   //
    Delete(Str, 1, Pos(';', Str));                             //   //
    try                                                        //   //
      Result.Min := StrToFloat(Copy(Str, 1, Pos(';', Str)-1)); //   //
    except                                                     //   //
      Result.Num := 65535;                                     //   //
      Exit;                                                    //   //
    end;                                                       //   //
    Delete(Str, 1, Pos(';', Str));                             //   //
    try                                                        //   //
      Result.Max := StrToFloat(Copy(Str, 1, Pos(';', Str)-1)); //   //
    except                                                     //   //
      Result.Num := 65535;                                     //   //
      Exit;                                                    //   //
    end;                                                       //   //
    Delete(Str, 1, Pos(';', Str));                             //   //
    try                                                        //   //
      Result.Col := StrToInt(Copy(Str, 1, Pos(';', Str)-1));   //   //
    except                                                     //   //
      Result.Num := 65535;                                     //   //
      Exit;                                                    //   //
    end;                                                       //   //
  end;                                                         //   //
/////////////////////////////////////////////////////////////////   //
                                                                    //
var                                                                 //
  OpenDlg: TOpenDialog;                                             //
  SL: TStringList;                                                  //
  n, m: WORD;                                                       //
  tmpClrParams: TColorParams;                                       //
begin                                                               //
  OpenDlg := TOpenDialog.Create(self);                              //
  with OpenDlg do                                                   //
  begin                                                             //
//    InitialDir := fFilePath;                                        //
    InitialDir := ExtractFilePath(pStatistica^.Wafer.fName);        //
    Filter := 'Файлы цветовых схем (*.clr)|*.clr';                  //
    Title := 'Загрузить цветовую схему параметров';                 //
                                                                    //
    if Execute then                                                 //
    begin                                                           //
      SL := TStringList.Create;                                     //
      SL.LoadFromFile(FileName);                                    //
                                                                    //
      if SL.Count > 0 then                                          //
      begin                                                         //
        Init;                                                       //
                                                                    //
        m := 0;                                                     //
        for n := 1 to SL.Count-1 do                                 //
        begin                                                       //
          tmpClrParams := GetColorParams(SL.Strings[n]);            //
                                                                    //
          if tmpClrParams.Num <> 65535 then                         //
          begin                                                     //
            if m <> 0 then AddNextParams;                           //
                                                                    //
            TestsCB[m].ItemIndex := tmpClrParams.Num;               //
            MinLab[m].Caption := NormToStr(tmpClrParams.Min);       //
            MaxLab[m].Caption := NormToStr(tmpClrParams.Max);       //
            ColLab[m].Color   := tmpClrParams.Col;                  //
            Inc(m);                                                 //
          end;                                                      //
        end;                                                        //
      end;                                                          //
    end;                                                            //
                                                                    //
    Free;                                                           //
  end;                                                              //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TDataDlg.SaveBtnClick(Sender: TObject);                   //
var                                                                 //
  SaveDlg: TSaveDialog;                                             //
  SL: TStringList;                                                  //
  n: WORD;                                                          //
begin                                                               //
  SaveDlg := TSaveDialog.Create(self);                              //
  with SaveDlg do                                                   //
  begin                                                             //
    FileName  := pStatistica^.Wafer.Code+'.clr';                    //
//    InitialDir := fFilePath;                                        //
    InitialDir := ExtractFilePath(pStatistica^.Wafer.fName);        //
    Filter := 'Файлы цветовых схем (*.clr)|*.clr';                  //
    Title := 'Сохранить цветовую схему параметров';                 //
    Options := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt]; //
                                                                    //
    if Execute then                                                 //
    begin                                                           //
      SL := TStringList.Create;                                     //
                                                                    //
      Sl.Add('[ColorParams]');                                      //
      for n := 0 to Length(TestsCB)-1 do                            //
        SL.Add(IntToStr(n)+'='+                                     //
          IntToStr(TestsCB[n].ItemIndex)+';'+                       //
          FormatFloat('0.000', StrToMinNorm(MinLab[n].Caption))+';'+ //
          FormatFloat('0.000', StrToMaxNorm(MaxLab[n].Caption))+';'+ //
          IntToStr(ColLab[n].Color)+';');                           //
                                                                    //
      SL.SaveToFile(FileName);                                      //
      SL.Free;                                                      //
    end;                                                            //
                                                                    //
    Free;                                                           //
  end;                                                              //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TDataDlg.AcceptBtnClick(Sender: TObject);                 //
var                                                                 //
  n: byte;                                                          //
begin                                                               //
  with pStatistica^.Wafer do                                        //
  begin                                                             //
    SetLength(ColorParams, 0);                                      //
    SetLength(ColorParams, Length(TestsCB));                        //
    for n := 0 to Length(TestsCB)-1 do                              //
    begin                                                           //
      ColorParams[n].Num := TestsCB[n].ItemIndex;                   //
      ColorParams[n].Min := StrToMinNorm(MinLab[n].Caption);        //
      ColorParams[n].Max := StrToMaxNorm(MaxLab[n].Caption);        //
      ColorParams[n].Col := ColLab[n].Color;                        //
    end;                                                            //
  end;                                                              //
                                                                    //
  pStatistica^.Repaint(1);                                          //
end;                                                                //
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
procedure TDataDlg.ResetBtnClick(Sender: TObject);                  //
begin                                                               //
  pStatistica^.Repaint(0);                                          //
end;                                                                //
//////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////
procedure TDataDlg.Init;                                                              //
var                                                                                   //
  n: WORD;                                                                            //
begin                                                                                 //
  FreeAll;                                                                            //
                                                                                      //
  SetLength(TestsCB, 1);                                                              //
  TestsCB[0] := TestsCB_0;                                                            //
  with pStatistica^.Wafer do                                                          //
    for n := 0 to Length(TestsParams)-1 do TestsCB[0].Items.Add(TestsParams[n].Name); //
  TestsCB[0].ItemIndex := 0;                                                          //
                                                                                      //
  SetLength(MinLabl, 1);                                                              //
  MinLabl[0] := MinLabl_0;                                                            //
  SetLength(MinLab, 1);                                                               //
  MinLab[0] := MinLab_0;                                                              //
                                                                                      //
  SetLength(MaxLabl, 1);                                                              //
  MaxLabl[0] := MaxLabl_0;                                                            //
  SetLength(MaxLab, 1);                                                               //
  MaxLab[0] := MaxLab_0;                                                              //
                                                                                      //
  SetLength(ColLab, 1);                                                               //
  ColLab[0] := ColLab_0;                                                              //
{                                                                                      //
  SetLength(DTrackBar, 1);
  DTrackBar[0] := TDoubleTrackBar.Create(self);
  with DTrackBar[0] do
  begin
    Parent := MainSB;

    Left := 2;
    Top  := TestsCB[0].Height+4;
  end;
}
  TotalHeight := TestsCB_0.Height+2;//DTrackBar[0].Height+4;

  self.Height := 121;//+DTrackBar[0].Height+2;
  MainSB.Height := TotalHeight+2;
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
procedure TDataDlg.FreeAll;                                                           //
var                                                                                   //
  n: WORD;                                                                            //
begin                                                                                 //
//  if Length(DTrackBar) > 0 then  DTrackBar[0].Free;
  if Length(TestsCB) > 1 then                                                         //
    for n := 1 to Length(TestsCB)-1 do                                                //
    begin                                                                             //
      TestsCB[n].Free;                                                                //
      MinLabl[n].Free;                                                                //
      MinLab[n].Free;                                                                 //
      MaxLabl[n].Free;                                                                //
      MaxLab[n].Free;                                                                 //
      ColLab[n].Free;                                                                 //
//      DTrackBar[n].Free;
    end;                                                                              //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////
function TDataDlg.AddNextParams: WORD;               //
begin                                                //
  Result := Length(TestsCB);                         //
                                                     //
  if (Result = 0) or (Result > 15) then              //
  begin                                              //
    Result := 0;                                     //
    Exit;                                            //
  end;                                               //
                                                     //
  self.Height := self.Height+TotalHeight;            //
  MainSB.Height := MainSB.Height+TotalHeight;        //
                                                     //
  SetLength(TestsCB, Result+1);                      //
  TestsCB[Result] := TComboBox.Create(self);         //
  TestsCB[Result].Tag := Result;                     //
  CopyCBParams(TestsCB[Result]);                     //
                                                     //
  SetLength(MinLabl, Result+1);                      //
  MinLabl[Result] := TStaticText.Create(self);       //
  MinLabl[Result].Tag := Result;                     //
  CopyLabParams(MinLabl[Result], 0);                 //
  SetLength(MinLab, Result+1);                       //
  MinLab[Result] := TStaticText.Create(self);        //
  MinLab[Result].Tag := Result;                      //
  CopyLabParams(MinLab[Result], 1);                  //
                                                     //
  SetLength(MaxLabl, Result+1);                      //
  MaxLabl[Result] := TStaticText.Create(self);       //
  MaxLabl[Result].Tag := Result;                     //
  CopyLabParams(MaxLabl[Result], 2);                 //
  SetLength(MaxLab, Result+1);                       //
  MaxLab[Result] := TStaticText.Create(self);        //
  MaxLab[Result].Tag := Result;                      //
  CopyLabParams(MaxLab[Result], 3);                  //
                                                     //
  SetLength(ColLab, Result+1);                       //
  ColLab[Result] := TStaticText.Create(self);        //
  ColLab[Result].Tag := Result;                      //
  CopyLabParams(ColLab[Result], 4);                  //
{
  SetLength(DTrackBar, Result+1);
  DTrackBar[Result] := TDoubleTrackBar.Create(self);
  with DTrackBar[Result] do
  begin
    Parent := MainSB;

    Left := 2;
    Top  := TestsCB[Result].Top+TestsCB[Result].Height+2;
  end;
}
end;                                                 //
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
function TDataDlg.DeleteLastParams: WORD;            //
begin                                                //
  Result := Length(TestsCB);                         //
  if Result < 2 then Exit;                           //
                                                     //
  TestsCB[Result-1].Free;                            //
  SetLength(TestsCB, Result-1);                      //
                                                     //
  MinLabl[Result-1].Free;                            //
  SetLength(MinLabl, Result-1);                      //
  MinLab[Result-1].Free;                             //
  SetLength(MinLab, Result-1);                       //
                                                     //
  MaxLabl[Result-1].Free;                            //
  SetLength(MaxLabl, Result-1);                      //
  MaxLab[Result-1].Free;                             //
  SetLength(MaxLab, Result-1);                       //
                                                     //
  ColLab[Result-1].Free;                             //
  SetLength(ColLab, Result-1);                       //
                                                     //
//  DTrackBar[Result-1].Free;                          //
//  SetLength(DTrackBar, Result-1);                    //
                                                     //
  MainSB.Height := MainSB.Height-TotalHeight;        //
  self.Height := self.Height-TotalHeight;            //
end;                                                 //
///////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TDataDlg.CalcParams;                                                                            //
                                                                                                          //
//////////////////////////////////////////////////////////////////////                                    //
  function GetStatus(const Chip: TChip; const NP: WORD): byte;      //                                    //
  begin                                                             //                                    //
    Result := 0;                                                    //                                    //
                                                                    //                                    //
    if Chip.Status = pStatistica^.Wafer.TestsParams[NP].Status then //                                    //
      case Chip.Status of                                           //                                    //
        2000..3000: Result := 2;                                    //                                    //
        3500..4500: Result := 3;                                    //                                    //
      end;                                                          //                                    //
                                                                    //                                    //
    if Chip.Status = 1 then Result := 1;                            //                                    //
  end;                                                              //                                    //
//////////////////////////////////////////////////////////////////////                                    //
                                                                                                          //
var                                                                                                       //
  X, Y, n, FailCount: WORD;                                                                               //
  FailSum, OKSum: Extended;                                                                               //
  Res: byte;                                                                                              //
  FirstTimeOK, FirstTimeFail: Boolean;                                                                    //
begin                                                                                                     //
  with pStatistica^.Wafer do                                                                              //
  begin                                                                                                   //
    SetLength(AddParams, Length(TestsParams));                                                            //
    if Length(AddParams) > 0 then                                                                         //
      for n := 0 to Length(AddParams)-1 do                                                                //
      begin                                                                                               //
        FirstTimeOK   := True;                                                                            //
        FirstTimeFail := True;                                                                            //
        FailSum := 0.0;                                                                                   //
        OKSum   := 0.0;                                                                                   //
        FailCount:= 0;                                                                                    //
                                                                                                          //
        for Y := 0 to Length(Chip)-1 do                                                                   //
          for X := 0 to Length(Chip[0])-1 do                                                              //
          begin                                                                                           //
            Res := GetStatus(Chip[Y, X], n);                                                              //
                                                                                                          //
            if Res = 1 then                                                                               //
              with Chip[Y, X] do                                                                          //
                if FirstTimeOK then                                                                       //
                begin                                                                                     //
                  AddParams[n].OK.Min := ChipParams[n].Value;                                                        //
                  AddParams[n].OK.Max := ChipParams[n].Value;                                                        //
                  OKSum := ChipParams[n].Value;                                                                      //
                                                                                                          //
                  FirstTimeOK := False;                                                                   //
                end                                                                                       //
                else                                                                                      //
                begin                                                                                     //
                  if ChipParams[n].Value < AddParams[n].OK.Min then AddParams[n].OK.Min := ChipParams[n].Value;                 //
                  if ChipParams[n].Value > AddParams[n].OK.Max then AddParams[n].OK.Max := ChipParams[n].Value;                 //
                  OKSum := OKSum+ChipParams[n].Value;                                                                //
                end;                                                                                      //
                                                                                                          //
                                                                                                          //
            if Res in [2,3] then                                                                          //
              with Chip[Y, X] do                                                                          //
                if FirstTimeFail then                                                                     //
                begin                                                                                     //
                  AddParams[n].All.Min := ChipParams[n].Value;                                                       //
                  AddParams[n].All.Max := ChipParams[n].Value;                                                       //
                  FailSum := ChipParams[n].Value;                                                                    //
                  Inc(FailCount);                                                                         //
                                                                                                          //
                  FirstTimeFail := False;                                                                 //
                end                                                                                       //
                else                                                                                      //
                begin                                                                                     //
                  if ChipParams[n].Value < AddParams[n].All.Min then AddParams[n].All.Min := ChipParams[n].Value;               //
                  if ChipParams[n].Value > AddParams[n].All.Max then AddParams[n].All.Max := ChipParams[n].Value;               //
                  FailSum := FailSum+ChipParams[n].Value;                                                            //
                  Inc(FailCount);                                                                         //
                end;                                                                                      //
          end;                                                                                            //
                                                                                                          //
          if AddParams[n].OK.Min < AddParams[n].All.Min then AddParams[n].All.Min := AddParams[n].OK.Min; //
          if AddParams[n].OK.Max > AddParams[n].All.Max then AddParams[n].All.Max := AddParams[n].OK.Max; //
                                                                                                          //
          if NOK <> 0 then AddParams[n].OK.Avr  := OKSum/NOK                                              //
                      else AddParams[n].OK.Avr := 0.0;                                                    //
                                                                                                          //
          if (FailCount+NOK) <> 0 then AddParams[n].All.Avr := (FailSum+OKSum)/(FailCount+NOK)            //
                                  else AddParams[n].All.Avr := 0.0;                                       //
      end;                                                                                                //
  end;                                                                                                    //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
procedure TDataDlg.CopyCBParams(var CB: TComboBox);                          //
var                                                                          //
  n: WORD;                                                                   //
begin                                                                        //
  with CB do                                                                 //
  begin                                                                      //
    Parent := MainSB;                                                        //
                                                                             //
    Left := TestsCB[0].Left;                                                 //
    Top  := TestsCB[Tag-1].Top+TotalHeight;                                  //
    Width := TestsCB[0].Width;                                               //
    Height := TestsCB[0].Height;                                             //
                                                                             //
    Font.Height := 16;                                                       //
    Font.Style := [fsBold];                                                  //
    with pStatistica^.Wafer do                                               //
      for n := 0 to Length(TestsParams)-1 do Items.Add(TestsParams[n].Name); //
                                                                             //
    BevelKind := bkFlat;                                                     //
    BevelOuter := bvNone;                                                    //
    Style := csDropDownList;                                                 //
                                                                             //
    OnChange := TestsCB_Change;                                              //
                                                                             //
    ItemIndex := 0;                                                          //
  end;                                                                       //
end;                                                                         //
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
procedure TDataDlg.CopyLabParams(var Lab: TStaticText; const LabType: byte); //
begin                                                                        //
  with Lab do                                                                //
  begin                                                                      //
    Parent := MainSB;                                                        //
                                                                             //
    AutoSize := False;                                                       //
    Alignment := taCenter;                                                   //
    Color := clWhite;                                                        //
    Font.Height := 16;                                                       //
    Font.Style := [fsBold];                                                  //
                                                                             //
    case LabType of                                                          //
      0: begin                                                               //
           Top    := MinLabl[Tag-1].Top+TotalHeight;                         //
           Left   := MinLabl[0].Left;                                        //
           Width  := MinLabl[0].Width;                                       //
           Height := MinLabl[Tag-1].Height;                                  //
         end;                                                                //
                                                                             //
      1: begin                                                               //
           Top    := MinLab[Tag-1].Top+TotalHeight;                          //
           Left   := MinLab[0].Left;                                         //
           Width  := MinLab[0].Width;                                        //
           Height := MinLab[Tag-1].Height;                                   //
                                                                             //
           OnClick := MinMaxLab_Click;                                       //
         end;                                                                //
      2: begin                                                               //
           Top    := MaxLabl[Tag-1].Top+TotalHeight;                         //
           Left   := MaxLabl[0].Left;                                        //
           Width  := MaxLabl[0].Width;                                       //
           Height := MaxLabl[Tag-1].Height;                                  //
         end;                                                                //
                                                                             //
      3: begin                                                               //
           Top    := MaxLab[Tag-1].Top+TotalHeight;                          //
           Left   := MaxLab[0].Left;                                         //
           Width  := MaxLab[0].Width;                                        //
           Height := MaxLab[Tag-1].Height;                                   //
                                                                             //
           OnClick := MinMaxLab_Click;                                       //
         end;                                                                //
                                                                             //
      4: begin                                                               //
           Top    := ColLab[Tag-1].Top+TotalHeight;                          //
           Left   := ColLab[0].Left;                                         //
           Width  := ColLab[0].Width;                                        //
           Height := ColLab[Tag-1].Height;                                   //
                                                                             //
           Color := clLime;                                                  //
                                                                             //
           OnClick := ColLab_Click;                                          //
         end;                                                                //
    end;                                                                     //
                                                                             //
    Transparent := False;                                                    //
  end;                                                                       //
end;                                                                         //
///////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
function TDataDlg.NormToStr(const Val: Single): String;       //
begin                                                         //
  if (Val = NotSpec) or (Val = -NotSpec) then Result := 'нет' //
  else Result := FormatFloat('0.000', Val);                   //
end;                                                          //
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
function TDataDlg.StrToMaxNorm(const Str: String): Single;    //
begin                                                         //
  if Str = 'нет' then Result := NotSpec                       //
                 else Result := StrToFloat(Str);              //
end;                                                          //
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
function TDataDlg.StrToMinNorm(const Str: String): Single;    //
begin                                                         //
  if Str = 'нет' then Result := -NotSpec                      //
                 else Result := StrToFloat(Str);              //
end;                                                          //
////////////////////////////////////////////////////////////////



end.

unit ViewChipParams;

interface

uses
  Windows, SysUtils, Messages, Graphics, Forms, Structs, Grids, Buttons, Controls, StdCtrls, Classes;

type
  TChipsDlg = class(TForm)
    ChipLab:     TLabel;
    ChipLabInfo: TLabel;
    ChipSB: TScrollBox;
    HandlerSB: TScrollBox;
    NLab0: TLabel;
    ValNameLab0: TLabel;
    MinLab0: TLabel;
    ValLab0: TLabel;
    MaxLab0: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnyCompMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent; pTstsParams: PTestsParams);

    function PreShowChip(Chip: TChip; StatusName: String): TPoint;
  private
    pTestsParams: PTestsParams;
    fOnChipDlgClose: TOnChipDlgClose;
    fNTests: WORD;
    NLab, ValNameLab, MinLab, MaxLab, ValLab: array of TLabel;
    BlankLab1: TStaticText;
  published
    property OnChipDlgClose: TOnChipDlgClose read fOnChipDlgClose write fOnChipDlgClose;
  end;

var
  ChipsDlg: TChipsDlg;


implementation

{$R *.dfm}

//////////////////////////////////////////////////////////////////////////////////////////////////
constructor TChipsDlg.Create(AOwner: TComponent; pTstsParams: PTestsParams);                    //
                                                                                                //
/////////////////////////////////////////                                                       //
  procedure CloneLab(var Lab: TLabel); //                                                       //
  begin                                //                                                       //
    Lab := TLabel.Create(self);        //                                                       //
    with Lab do                        //                                                       //
    begin                              //                                                       //
      Parent := ChipSB;                //                                                       //
      ParentColor := False;            //                                                       //
      ParentFont  := False;            //                                                       //
      Transparent := False;            //                                                       //
      AutoSize := False;               //                                                       //
      Alignment := NLab0.Alignment;    //                                                       //
      Height := NLab0.Height;          //                                                       //
      Font.Size  := 10;                //                                                       //
      Color := clWhite;                //                                                       //
    end;                               //                                                       //
  end;                                 //                                                       //
/////////////////////////////////////////                                                       //
                                                                                                //
var                                                                                             //
  n: WORD;                                                                                      //
  Max, Offs: byte;                                                                              //
begin                                                                                           //
  inherited Create(AOwner);                                                                     //
                                                                                                //
  ControlStyle := ControlStyle+[csOpaque];                                                      //
                                                                                                //
  pTestsParams := pTstsParams;                                                                  //
                                                                                                //
  Offs := self.Height-self.ClientHeight;                                                        //
                                                                                                //
  fNTests := Length(pTestsParams^);                                                             //
                                                                                                //
  SetLength(NLab, fNTests);                                                                     //
  SetLength(ValNameLab, fNTests);                                                               //
  SetLength(MinLab, fNTests);                                                                   //
  SetLength(ValLab, fNTests);                                                                   //
  SetLength(MaxLab, fNTests);                                                                   //
                                                                                                //
  DecimalSeparator := '.';                                                                      //
                                                                                                //
  if fNTests = 0 then                                                                           //
  begin                                                                                         //
    self.Width  := 200;                                                                         //
    self.Height := 56;                                                                          //
                                                                                                //
    self.Constraints.MinHeight := self.Height;                                                  //
    self.Constraints.MaxHeight := self.Height;                                                  //
                                                                                                //
    ChipLab.Width := self.Width-25;      //                                                     //
    self.VertScrollBar.Visible := False; // Для эстетики                                        //
    self.HorzScrollBar.Visible := False; //                                                     //
  end                                                                                           //
  else                                                                                          //
  begin                                                                                         //
    Max := 0;                                                                                   //
    for n := 0 to fNTests-1 do                                                                  //
      if Length(pTestsParams^[n].Name) > Max then Max := Length(pTestsParams^[n].Name);         //
                                                                                                //
    if (Max*9) > 80 then ValNameLab0.Width := Max*9;                                            //
    MinLab0.Left := ValNameLab0.Left+ValNameLab0.Width+2;                                       //
    ValLab0.Left := MinLab0.Left+MinLab0.Width+2;                                               //
    MaxLab0.Left := ValLab0.Left+ValLab0.Width+2;                                               //
    ChipSB.Width := NLab0.Width+MinLab0.Width+ValNameLab0.Width+ValLab0.Width+MaxLab0.Width+29; //
    HandlerSB.Width := ChipSB.Width-17;                                                         //
    ChipLab.Width := ChipSB.Width;                                                              //
    ChipLabInfo.Width := ChipSB.Width;                                                          //
                                                                                                //
    for n := 0 to fNTests-1 do                                                                  //
    begin                                                                                       //
      CloneLab(NLab[n]);                                                                        //
      with NLab[n] do                                                                           //
      begin                                                                                     //
        Left := NLab0.Left;                                                                     //
        Top  := 2+n*20;                                                                         //
        Width := NLab0.Width;                                                                   //
        Caption := IntToStr(n+1);                                                               //
      end;                                                                                      //
                                                                                                //
      CloneLab(ValNameLab[n]);                                                                  //
      with ValNameLab[n] do                                                                     //
      begin                                                                                     //
        Left := ValNameLab0.Left;                                                               //
        Top  := 2+n*20;                                                                         //
        Width := ValNameLab0.Width;                                                             //
        Caption := ' '+pTestsParams^[n].Name;                                                   //
        Alignment := taLeftJustify;                                                             //
      end;                                                                                      //
                                                                                                //
      CloneLab(MinLab[n]);                                                                      //
      with MinLab[n] do                                                                         //
      begin                                                                                     //
        Left := MinLab0.Left;                                                                   //
        Top  := 2+n*20;                                                                         //
        Width := MinLab0.Width;                                                                 //
        if pTestsParams^[n].Norma.Min <> -NotSpec then                                          //
          Caption := FormatFloat('0.000', pTestsParams^[n].Norma.Min)                           //
        else                                                                                    //
          Caption := 'нет';                                                                     //
        Font.Color := clRed;                                                                    //
      end;                                                                                      //
                                                                                                //
      CloneLab(ValLab[n]);                                                                      //
      with ValLab[n] do                                                                         //
      begin                                                                                     //
        Left := ValLab0.Left;                                                                   //
        Top  := 2+n*20;                                                                         //
        Width := ValLab0.Width;                                                                 //
        Font.Color := clNavy;                                                                   //
      end;                                                                                      //
                                                                                                //
      CloneLab(MaxLab[n]);                                                                      //
      with MaxLab[n] do                                                                         //
      begin                                                                                     //
        Left := MaxLab0.Left;                                                                   //
        Top  := 2+n*20;                                                                         //
        Width := MaxLab0.Width;                                                                 //
        if pTestsParams^[n].Norma.Max <> NotSpec then                                           //
          Caption := FormatFloat('0.000', pTestsParams^[n].Norma.Max)                           //
        else                                                                                    //
          Caption := 'нет';                                                                     //
        Font.Color := clRed;                                                                    //
      end;                                                                                      //
    end;                                                                                        //
                                                                                                //
    BlankLab1 := TStaticText.Create(self);                                                      //
    BlankLab1.Parent := ChipSB;                                                                 //
    BlankLab1.Left := NLab0.Left;                                                               //
    BlankLab1.Top := MaxLab[Length(MaxLab)-1].Top+NLab0.Height;                                 //
    BlankLab1.AutoSize := False;                                                                //
    BlankLab1.Height := 2;                                                                      //
                                                                                                //
    self.ClientWidth := ChipSB.Left+ChipSB.Width+2;                                             //
                                                                                                //
    self.Constraints.MinHeight := ChipSB.Top+5*(NLab0.Height+2)+Offs+6;                         //
    self.Constraints.MaxHeight := ChipSB.Top+(fNTests)*(NLab0.Height+2)+Offs+6;                 //
                                                                                                //
    ChipLab.Width := 327;               // На                                                   //
    self.VertScrollBar.Visible := True; // всякий                                               //
    self.HorzScrollBar.Visible := True; // случай                                               //
  end;                                                                                          //
                                                                                                //
  self.Constraints.MinWidth := self.Width;                                                      //
  self.Constraints.MaxWidth := self.Width;                                                      //
                                                                                                //
  self.Height := self.Constraints.MinHeight;                                                    //
end;                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
procedure TChipsDlg.FormClose(Sender: TObject; var Action: TCloseAction);                       //
begin                                                                                           //
  if Assigned(OnChipDlgClose) then OnChipDlgClose;                                              //
end;                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
procedure TChipsDlg.FormDestroy(Sender: TObject);                                               //
var                                                                                             //
  n: WORD;                                                                                      //
begin                                                                                           //
  if fNTests <> 0 then                                                                          //
  begin                                                                                         //
    for n := 0 to fNTests-1 do                                                                  //
    begin                                                                                       //
      NLab[n].Free;                                                                             //
      ValNameLab[n].Free;                                                                       //
      MinLab[n].Free;                                                                           //
      ValLab[n].Free;                                                                           //
      MaxLab[n].Free;                                                                           //
    end;                                                                                        //
    BlankLab1.Free;                                                                             //
  end;                                                                                          //
end;                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////
procedure TChipsDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //
begin                                                                                //
  if Key = VK_ESCAPE then Close;                                                     //
end;                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////
procedure TChipsDlg.FormResize(Sender: TObject);   //
begin                                              //
  ChipSB.Height := self.ClientHeight-ChipSB.Top-4; //
                                                   //
  if ChipSB.VertScrollBar.IsScrollBarVisible then  //
    ChipSB.Width := HandlerSB.Width+17             //
  else                                             //
    ChipSB.Width := HandlerSB.Width;               //
end;                                               //
/////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TChipsDlg.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); //
begin                                                                                                                                 //
  if WheelDelta < 0 then ChipSB.VertScrollBar.Position := ChipSB.VertScrollBar.Position+20                                            //
                    else ChipSB.VertScrollBar.Position := ChipSB.VertScrollBar.Position-20;                                           //
end;                                                                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TChipsDlg.AnyCompMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
begin                                                                                                           //
  ReleaseCapture;                                                                                               //
  self.Perform(WM_SYSCOMMAND, $F012, 0);                                                                        //
end;                                                                                                            //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////
function TChipsDlg.PreShowChip(Chip: TChip; StatusName: String): TPoint; //
                                                                         //
///////////////////////////////////////////////////////////////          //
  function IsFailParam(const Num: WORD): Boolean;            //          //
  begin                                                      //          //
    Result := False;                                         //          //
                                                             //          //
   if (pTestsParams^[Num].Status > 1999) and                 //          //
      (pTestsParams^[Num].Status < 4501) then                //          //
     if Length(pTestsParams^) > 0 then                       //          //
       with pTestsParams^[Num].Norma do                      //          //
         if Chip.ChipParams[Num].Value <> NotSpec then                  //          //
         begin                                               //          //
           if (Min = -NotSpec)  and (Max <> NotSpec) then    //          //
             if Chip.ChipParams[Num].Value > Max then Result := False;  //          //
                                                             //          //
           if (Min <> -NotSpec) and (Max = NotSpec)  then    //          //
             if Chip.ChipParams[Num].Value < Min then Result := False;  //          //
                                                             //          //
           if (Min <> -NotSpec) and (Max <> NotSpec) then    //          //
             if (Chip.ChipParams[Num].Value < Min) or                   //          //
                (Chip.ChipParams[Num].Value > Max) then Result := True; //          //
         end                                                 //          //
         else Result := True;                                //          //
  end;                                                       //          //
///////////////////////////////////////////////////////////////          //
                                                                         //
var                                                                      //
  n: WORD;                                                               //
  Col, TmpCol: TColor;                                                   //
begin                                                                    //
  ChipLab.Caption := GetStatusString(Chip.Status);                       //
                                                                         //
  ChipLab.Font.Color := GetMainColor(Chip.Status);                       //
  if ChipLab.Font.Color = clLime then ChipLab.Font.Color := clGreen;     //
  ChipLabInfo.Caption := StatusName;                                     //
  ChipLabInfo.Font.Color := ChipLab.Font.Color;                          //
                                                                         //
  Col := $00EAEAEA;                                                      //
  if fNTests > 0 then                                                    //
    for n := 0 to fNTests-1 do                                           //
    begin                                                                //
      if Col = clWhite then Col := $00EAEAEA                             //
                       else Col := clWhite;                              //
                                                                         //
      if Length(Chip.ChipParams) > 0 then                                //
      begin                                                              //
        TmpCol := Col;                                                   //
//        if IsFailParam(n) then Col := $00BFBFFF; // Розовый, если брак   //
        if Chip.ChipParams[n].Stat in [2,3] then Col := $00BFBFFF; // Розовый, если брак   //
                                                                         //
        NLab[n].Color       := Col;                                      //
        ValNameLab[n].Color := Col;                                      //
        ValLab[n].Color     := Col;                                      //
        MinLab[n].Color     := Col;                                      //
        MaxLab[n].Color     := Col;                                      //
        Col := TmpCol;                                                   //
                                                                         //
        if Chip.ChipParams[n].Value <> NotSpec then                      //
          ValLab[n].Caption := FormatFloat('0.000', Chip.ChipParams[n].Value)       //
        else ValLab[n].Caption := 'нет';                                 //
      end                                                                //
      else                                                               //
      begin                                                              //
        NLab[n].Color       := Col;                                      //
        ValNameLab[n].Color := Col;                                      //
        ValLab[n].Color     := Col;                                      //
        MinLab[n].Color     := Col;                                      //
        MaxLab[n].Color     := Col;                                      //
                                                                         //
        ValLab[n].Caption := 'XXXX';                                     //
      end;                                                               //
    end;                                                                 //
                                                                         //
  Result.X := self.Width;                                                //
  Result.Y := self.Height;                                               //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////


end.

unit ViewFails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Grids, Statistica, Structs;

type
  TFailsDlg = class(TForm)
    FailSCLab: TLabel;
    FailFCLab: TLabel;
    SHandlerSB: TScrollBox;
    SQuantLab0: TStaticText;
    SNameLab0: TStaticText;
    SNumLab0: TStaticText;
    SChipSB: TScrollBox;
    FHandlerSB: TScrollBox;
    FQuantLab0: TStaticText;
    FNameLab0: TStaticText;
    FNumLab0: TStaticText;
    FChipSB: TScrollBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    SCColorLab: array of TLabel;
    FCColorLab: array of TLabel;
    pStatistica: PStatistica;
    SNumLab, SNameLab, SQuantLab, FNumLab, FNameLab, FQuantLab: array of TStaticText;

    procedure SCLabClick(Sender: TObject);
    procedure FCLabClick(Sender: TObject);
  public
    constructor Create(Sender: TObject; pStat: PStatistica);

    procedure Refresh;
  end;

var
  FailsDlg: TFailsDlg;

implementation

{$R *.dfm}

/////////////////////////////////////////////////////////////////////////////////////////////////
constructor TFailsDlg.Create(Sender: TObject; pStat: PStatistica);                             //
                                                                                               //
//////////////////////////////////////////////                                                 //
  procedure CloneLab(var Lab: TStaticText); //                                                 //
  begin                                     //                                                 //
    Lab := TStaticText.Create(self);        //                                                 //
    with Lab do                             //                                                 //
    begin                                   //                                                 //
      ParentColor := False;                 //                                                 //
      ParentFont  := False;                 //                                                 //
      Transparent := False;                 //                                                 //
      AutoSize := False;                    //                                                 //
                                            //                                                 //
      Alignment := SNumLab0.Alignment;      //                                                 //
                                            //                                                 //
      Height := SNumLab0.Height;            //                                                 //
                                            //                                                 //
      Font.Color := clBlack;                //                                                 //
      Font.Size  := SNumLab0.Font.Size;     //                                                 //
      Font.Style := SNumLab0.Font.Style;    //                                                 //
    end;                                    //                                                 //
  end;                                      //                                                 //
//////////////////////////////////////////////                                                 //
                                                                                               //
var                                                                                            //
  n, Max: WORD;                                                                                //
  Col: TColor;                                                                                 //
begin                                                                                          //
  inherited Create(TComponent(Sender));                                                        //
                                                                                               //
  pStatistica := pStat;                                                                        //
                                                                                               //
  with pStatistica^.Wafer do                                                                   //
  begin                                                                                        //
                                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                               //
    Max := 0;                                                                                  //
    if Length(FailsSC) > 0 then                                                                //
    begin                                                                                      //
      SetLength(SCColorLab, Length(FailsSC));                                                  //
      SetLength(SNumLab,   Length(FailsSC));                                                   //
      SetLength(SNameLab,  Length(FailsSC));                                                   //
      SetLength(SQuantLab, Length(FailsSC));                                                   //
                                                                                               //
      SChipSB.Height := Length(FailsSC)*20+2;                                                  //
                                                                                               //
      for n := 0 to Length(FailsSC)-1 do                                                       //
      begin                                                                                    //
        if FailsSC[n].Name = '' then FailsSC[n].Name := GetStatusString(FailsSC[n].Status);    //
        if Length(FailsSC[n].Name) > Max then Max := Length(FailsSC[n].Name);                  //
      end;                                                                                     //
      if (Max*8+5) > 80 then SNameLab0.Width := Max*8+5;                                       //
      SQuantLab0.Left := SNameLab0.Left+SNameLab0.Width+2;                                     //
      SHandlerSB.Width := SQuantLab0.Left+SQuantLab0.Width+2;                                  //
                                                                                               //
      for n := 0 to Length(FailsSC)-1 do                                                       //
      begin                                                                                    //
        SCColorLab[n] := TLabel.Create(self);                                                  //
        with SCColorLab[n] do                                                                  //
        begin                                                                                  //
          Parent := self;                                                                      //
          Top := 49+20*n;                                                                      //
          Left := 5;                                                                           //
          Width  := 12;                                                                        //
          Height := 17;                                                                        //
          AutoSize := False;                                                                   //
          Color := FailsSC[n].Col;                                                             //
          Transparent := False;                                                                //
          Tag := n;                                                                            //
                                                                                               //
          OnClick := SCLabClick;                                                               //
        end;                                                                                   //
                                                                                               //
        if Col = clWhite then Col := $00EAEAEA else Col := clWhite;                            //
                                                                                               //
        CloneLab(SNumLab[n]);                                                                  //
        with SNumLab[n] do                                                                     //
        begin                                                                                  //
          Parent := SChipSB;                                                                   //
          Left := SNumLab0.Left;                                                               //
          Top  := 2+n*20;                                                                      //
          Width := SNumLab0.Width;                                                             //
//          Caption := IntToStr(n+1);                                                            //
          Caption := IntToStr(FailsSC[n].Status-1999);                                         //
          Color := Col;                                                                        //
        end;                                                                                   //
                                                                                               //
        CloneLab(SNameLab[n]);                                                                 //
        with SNameLab[n] do                                                                    //
        begin                                                                                  //
          Parent := SChipSB;                                                                   //
          Left := SNameLab0.Left;                                                              //
          Top  := SNumLab[n].Top;                                                              //
          Width := SNameLab0.Width;                                                            //
          Caption := ' '+FailsSC[n].Name;                                                      //
          Color := Col;                                                                        //
          Alignment := taLeftJustify;                                                          //
        end;                                                                                   //
                                                                                               //
        CloneLab(SQuantLab[n]);                                                                //
        with SQuantLab[n] do                                                                   //
        begin                                                                                  //
          Parent := SChipSB;                                                                   //
          Left := SQuantLab0.Left;                                                             //
          Top  := SNumLab[n].Top;                                                              //
          Width := SQuantLab0.Width;                                                           //
          Caption := IntToStr(FailsSC[n].Quantity);                                            //
          Color := Col;                                                                        //
        end;                                                                                   //
      end;                                                                                     //
    end;                                                                                       //
                                                                                               //
    if Length(SCColorLab) > 0 then SHandlerSB.Left := SCColorLab[0].Left+SCColorLab[0].Width+2 //
    else                                                                                       //
    begin                                                                                      //
      SHandlerSB.Left := 5;                                                                    //
      SChipSB.Height := 2;                                                                     //
    end;                                                                                       //
                                                                                               //
    SChipSB.Width := SHandlerSB.Width;                                                         //
    SChipSB.Left := SHandlerSB.Left;                                                           //
                                                                                               //
    FailSCLab.Left  := SHandlerSB.Left;                                                        //
    FailSCLab.Width := SHandlerSB.Width;                                                       //
    FailSCLab.Caption := 'Браки по СК ('+IntToStr(NFailSC)+')';                                //
                                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                               //
    Max := 0;                                                                                  //
    if Length(FailsFC) > 0 then                                                                //
    begin                                                                                      //
      SetLength(FCColorLab, Length(FailsFC));                                                  //
      SetLength(FNumLab,   Length(FailsFC));                                                   //
      SetLength(FNameLab,  Length(FailsFC));                                                   //
      SetLength(FQuantLab, Length(FailsFC));                                                   //
                                                                                               //
      FChipSB.Height := Length(FailsFC)*20+2;                                                  //
                                                                                               //
      for n := 0 to Length(FailsFC)-1 do                                                       //
      begin                                                                                    //
        if FailsFC[n].Name = '' then FailsFC[n].Name := GetStatusString(FailsFC[n].Status);    //
        if Length(FailsFC[n].Name) > Max then Max := Length(FailsFC[n].Name);                  //
      end;                                                                                     //
      if (Max*8+5) > 80 then FNameLab0.Width := Max*8+5;                                       //
      FQuantLab0.Left := FNameLab0.Left+FNameLab0.Width+2;                                     //
      FHandlerSB.Width := FQuantLab0.Left+FQuantLab0.Width+2;                                  //
                                                                                               //
      for n := 0 to Length(FailsFC)-1 do                                                       //
      begin                                                                                    //
        FCColorLab[n] := TLabel.Create(self);                                                  //
        with FCColorLab[n] do                                                                  //
        begin                                                                                  //
          Parent := self;                                                                      //
          Top := 49+20*n;                                                                      //
          Left := SHandlerSB.Left+SHandlerSB.Width+5;                                          //
          Width  := 12;                                                                        //
          Height := 17;                                                                        //
          AutoSize := False;                                                                   //
          Color := FailsFC[n].Col;                                                             //
          Transparent := False;                                                                //
          Tag := n;                                                                            //
                                                                                               //
          OnClick := FCLabClick;                                                               //
        end;                                                                                   //
                                                                                               //
        if Col = clWhite then Col := $00EAEAEA else Col := clWhite;                            //
                                                                                               //
        CloneLab(FNumLab[n]);                                                                  //
        with FNumLab[n] do                                                                     //
        begin                                                                                  //
          Parent := FChipSB;                                                                   //
          Left := FNumLab0.Left;                                                               //
          Top  := 2+n*20;                                                                      //
          Width := FNumLab0.Width;                                                             //
//          Caption := IntToStr(n+1);                                                            //
          Caption := IntToStr(FailsFC[n].Status-3499);                                         //
          Color := Col;                                                                        //
        end;                                                                                   //
                                                                                               //
        CloneLab(FNameLab[n]);                                                                 //
        with FNameLab[n] do                                                                    //
        begin                                                                                  //
          Parent := FChipSB;                                                                   //
          Left := FNameLab0.Left;                                                              //
          Top  := FNumLab[n].Top;                                                              //
          Width := FNameLab0.Width;                                                            //
          Caption := ' '+FailsFC[n].Name;                                                      //
          Color := Col;                                                                        //
          Alignment := taLeftJustify;                                                          //
        end;                                                                                   //
                                                                                               //
        CloneLab(FQuantLab[n]);                                                                //
        with FQuantLab[n] do                                                                   //
        begin                                                                                  //
          Parent := FChipSB;                                                                   //
          Left := FQuantLab0.Left;                                                             //
          Top  := FNumLab[n].Top;                                                              //
          Width := FQuantLab0.Width;                                                           //
          Caption := IntToStr(FailsFC[n].Quantity);                                            //
          Color := Col;                                                                        //
        end;                                                                                   //
      end;                                                                                     //
    end;                                                                                       //
                                                                                               //
    if Length(FCColorLab) > 0 then FHandlerSB.Left := FCColorLab[0].Left+FCColorLab[0].Width+2 //
    else                                                                                       //
    begin                                                                                      //
      FHandlerSB.Left := SHandlerSB.Left+SHandlerSB.Width+5;                                   //
      FChipSB.Height := 2;                                                                     //
    end;                                                                                       //
                                                                                               //
    FChipSB.Width := FHandlerSB.Width;                                                         //
    FChipSB.Left := FHandlerSB.Left;                                                           //
                                                                                               //
    FailFCLab.Left  := FHandlerSB.Left;                                                        //
    FailFCLab.Width := FHandlerSB.Width;                                                       //
    FailFCLab.Caption := 'Браки по ФК ('+IntToStr(NFailFC)+')';                                //
                                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////
                                                                                               //
  end;                                                                                         //
                                                                                               //
  self.ClientWidth := FChipSB.Left+FChipSB.Width+5;                                            //
  if SChipSB.Height > FChipSB.Height then self.ClientHeight := SChipSB.Top+SChipSB.Height+5    //
                                     else self.ClientHeight := FChipSB.Top+FChipSB.Height+5;   //
end;                                                                                           //
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFailsDlg.FormClose(Sender: TObject; var Action: TCloseAction);                      //
begin                                                                                          //
//  Action := caFree;                                                                            //
end;                                                                                           //
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFailsDlg.FormDestroy(Sender: TObject);                                              //
var                                                                                            //
  n: WORD;                                                                                     //
begin                                                                                          //
  if Length(SCColorLab) > 0 then                                                               //
    for n := 0 to Length(SCColorLab)-1 do                                                      //
    begin                                                                                      //
      SNumLab[n].Free;                                                                         //
      SNameLab[n].Free;                                                                        //
      SQuantLab[n].Free;                                                                       //
      SCColorLab[n].Free;                                                                      //
    end;                                                                                       //
                                                                                               //
  if Length(FCColorLab) > 0 then                                                               //
    for n := 0 to Length(FCColorLab)-1 do                                                      //
    begin                                                                                      //
      FNumLab[n].Free;                                                                         //
      FNameLab[n].Free;                                                                        //
      FQuantLab[n].Free;                                                                       //
      FCColorLab[n].Free;                                                                      //
    end;                                                                                       //
end;                                                                                           //
/////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFailsDlg.Refresh;                                                                        //
var                                                                                                 //
  n: WORD;                                                                                          //
begin                                                                                               //
  if Length(SCColorLab) > 0 then                                                                    //
    for n := 0 to Length(SCColorLab)-1 do SCColorLab[n].Color := pStatistica^.Wafer.FailsSC[n].Col; //
                                                                                                    //
  if Length(FCColorLab) > 0 then                                                                    //
    for n := 0 to Length(FCColorLab)-1 do FCColorLab[n].Color := pStatistica^.Wafer.FailsFC[n].Col; //
end;                                                                                                //
//////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
procedure TFailsDlg.SCLabClick(Sender: TObject);                          //
var                                                                       //
  ColorDlg: TColorDialog;                                                 //
begin                                                                     //
  ColorDlg := TColorDialog.Create(self);                                  //
  ColorDlg.Color := TLabel(Sender).Color;                                 //
  if ColorDlg.Execute then                                                //
  begin                                                                   //
    TLabel(Sender).Color := ColorDlg.Color;                               //
    pStatistica^.Wafer.FailsSC[TLabel(Sender).Tag].Col := ColorDlg.Color; //
                                                                          //
    pStatistica^.Repaint;                                                 //
  end;                                                                    //
  ColorDlg.Free;                                                          //
end;                                                                      //
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
procedure TFailsDlg.FCLabClick(Sender: TObject);                          //
var                                                                       //
  ColorDlg: TColorDialog;                                                 //
begin                                                                     //
  ColorDlg := TColorDialog.Create(self);                                  //
  ColorDlg.Color := TLabel(Sender).Color;                                 //
  if ColorDlg.Execute then                                                //
  begin                                                                   //
    TLabel(Sender).Color := ColorDlg.Color;                               //
    pStatistica^.Wafer.FailsFC[TLabel(Sender).Tag].Col := ColorDlg.Color; //
                                                                          //
    pStatistica^.Repaint;                                                 //
  end;                                                                    //
  ColorDlg.Free;                                                          //
end;                                                                      //
////////////////////////////////////////////////////////////////////////////


end.

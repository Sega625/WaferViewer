unit ViewFull;

interface

uses
  Windows, SysUtils, Messages, Graphics, Forms, Structs, Grids, Buttons,
  Controls, StdCtrls, Statistica, Classes;

type
  TFullChipsDlg = class(TForm)
    ParamsSG: TStringGrid;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnyCompMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure ParamsSGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure ParamsSGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ParamsSGMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ParamsSGMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ParamsSGMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ParamsSGSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  public
    constructor Create(AOwner: TComponent; pStat: PStatistica);
  private
    pStatistica: PStatistica;
    fOnChipDlgClose: TOnChipDlgClose;
    fNTests: WORD;
    fNChips: DWORD;
    ActCol: Integer;
    ActRow: Integer;

    HintMin, HintMax, HintTName: THintWindow;
    HintRect: TRect;

    NoResize: Boolean;
    OffsetX, OffsetY: Integer;
  published
    property OnChipDlgClose: TOnChipDlgClose read fOnChipDlgClose write fOnChipDlgClose;
    procedure DlgMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

var
  FullChipsDlg: TFullChipsDlg;


implementation

{$R *.dfm}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TFullChipsDlg.Create(AOwner: TComponent; pStat: PStatistica);                                    //
var                                                                                                          //
  n, m, k: WORD;                                                                                             //
begin                                                                                                        //
  inherited Create(AOwner);                                                                                  //
                                                                                                             //
  ControlStyle := ControlStyle+[csOpaque];                                                                   //
                                                                                                             //
  pStatistica := pStat;                                                                                      //
                                                                                                             //
  NoResize := True;                                                                                          //
                                                                                                             //
  fNTests := Length(pStatistica^.Wafer.TestsParams);                                                         //
  fNChips := pStatistica^.Wafer.NMeased;                                                                     //
                                                                                                             //
  ParamsSG.ColCount := fNTests+1;                                                                            //
  ParamsSG.RowCount := fNChips+1;                                                                            //
                                                                                                             //
  ParamsSG.Cells[0, 0] := ' №';                                                                              //
  for n := 0 to fNTests do ParamsSG.Cells[n+1, 0] := pStatistica^.Wafer.TestsParams[n].Name;                 //
  for m := 0 to fNChips do ParamsSG.Cells[0, m+1] := IntToStr(m+1);                                          //
                                                                                                             //
  m := 0;                                                                                                    //
  with pStatistica^.Wafer do                                                                                 //
    for k := 0 to fNChips-1 do                                                                               //
    begin                                                                                                    //
      Inc(m);                                                                                                //
      if Length(Chip[ChipN[k].Y, ChipN[k].X].ChipParams) > 0 then                                            //
        for n := 0 to Length(Chip[ChipN[k].Y, ChipN[k].X].ChipParams)-1 do                                   //
          if Chip[ChipN[k].Y, ChipN[k].X].ChipParams[n].Value <> NotSpec then                                //
            ParamsSG.Cells[n+1, m] := FormatFloat('0.000', Chip[ChipN[k].Y, ChipN[k].X].ChipParams[n].Value) //
          else ParamsSG.Cells[n+1, m] := 'нет';                                                              //
    end;                                                                                                     //
                                                                                                             //
  OffsetX := GetSystemMetrics(SM_CXFRAME);     // Отступ слева                                               //
  OffsetY := GetSystemMetrics(SM_CYCAPTION)+4; // Отступ сверху                                              //
                                                                                                             //
  m := 0;                                                                                                    //
  for n := 0 to ParamsSG.ColCount-1 do m := m+ParamsSG.ColWidths[n];                                         //
  ParamsSG.Width := m+ParamsSG.ColCount+12;                                                                  //
  self.ClientWidth := ParamsSG.Width+ParamsSG.Left*2+OffsetX-1;                                              //
  ParamsSG.Height := (ParamsSG.RowCount+1)*ParamsSG.DefaultRowHeight;                                        //
  self.ClientHeight := ParamsSG.Height+OffsetY-4;                                                            //
                                                                                                             //
  self.Constraints.MinWidth := ParamsSG.ColWidths[0]+ParamsSG.ColWidths[1]+49;                               //
  if self.Width > (Screen.DesktopWidth-71) then self.Constraints.MaxWidth := Screen.DesktopWidth-71          //
                                           else self.Constraints.MaxWidth := self.Width;                     //
  self.Constraints.MinHeight := ParamsSG.RowHeights[0]+ParamsSG.RowHeights[1]+67;                            //
  self.Constraints.MaxHeight := Screen.DesktopHeight-64;                                                     //
//  if self.Height > (Screen.DesktopHeight-64) then self.Constraints.MaxHeight := Screen.DesktopHeight-64      //
//                                             else self.Constraints.MaxHeight := self.Height;                 //
                                                                                                             //
  HintMin := THintWindow.Create(self);                                                                       //
  HintMin.Canvas.Font.Name := 'Arial';                                                                       //
  HintMin.Canvas.Font.Height := 16;                                                                          //
  HintMin.Canvas.Font.Style := [fsBold, fsItalic];                                                           //
//  HintMin.Color := $00F5DEC9;                                                                                //
                                                                                                             //
  HintMax := THintWindow.Create(self);                                                                       //
  HintMax.Canvas.Font := HintMin.Canvas.Font;                                                                //
//  HintMax.Color := $00F5DEC9;                                                                                //

  HintTName := THintWindow.Create(self);                                                                     //
  HintTName.Canvas.Font := HintMin.Canvas.Font;                                                              //
//  HintTName.Color := $00F5DEC9;                                                                              //
                                                                                                             //
  ActCol := -1;                                                                                              //
  ActRow := -1;                                                                                              //
                                                                                                             //
  NoResize := False;                                                                                         //
end;                                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormClose(Sender: TObject; var Action: TCloseAction);                                //
begin                                                                                                        //
  if Assigned(OnChipDlgClose) then OnChipDlgClose;                                                           //
end;                                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormDestroy(Sender: TObject);                                                        //
begin                                                                                                        //
  HintMin.Free;                                                                                              //
  HintMax.Free;                                                                                              //
  HintTName.Free;                                                                                            //
end;                                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////
procedure TFullChipsDlg.FormResize(Sender: TObject); //
begin                                                //
  if not NoResize then                               //
  begin                                              //
    ParamsSG.Height := self.ClientHeight-12;         //
    ParamsSG.Width  := self.ClientWidth-12;          //
  end;                                               //
end;                                                 //
///////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //
begin                                                                                    //
  if Key = VK_ESCAPE then Close;                                                         //
end;                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.AnyCompMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
begin                                                                                                               //
  ReleaseCapture;                                                                                                   //
  self.Perform(WM_SYSCOMMAND, $F012, 0);                                                                            //
end;                                                                                                                //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


procedure TFullChipsDlg.ParamsSGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
const 
  clPaleGreen = TColor($BBFFBB);
  clPaleRed   = TColor($BBBBFF);
//  clPaleBlue  = clSkyBlue;//TColor($FFCCCC);
var
  X, Y: WORD;
begin
//  if (ActCol <> -1) or (ActRow <> -1) then Exit;

  with TStringGrid(Sender) do
  begin
{
    if (gdFocused in State) then
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.Font.Color := clWhite;
    end;
}
    InflateRect(Rect, 1, 1);
    Canvas.Pen.Color := clBlack;
//    Canvas.Brush.Color := clWhite;



    if ARow > 0 then
    begin
      X := pStatistica^.Wafer.ChipN[ARow-1].X;
      Y := pStatistica^.Wafer.ChipN[ARow-1].Y;

      if ACol > 0 then
      begin
        if pStatistica^.Wafer.Chip[Y, X].ChipParams[Acol-1].Stat = 1 then Canvas.Brush.Color := clPaleGreen
                                                                     else Canvas.Brush.Color := clPaleRed;
        case pStatistica^.Wafer.Chip[Y, X].Status of
          4,5,10..1500: begin
                          Canvas.Font.Color  := clSilver;
                          Canvas.Brush.Color := clSilver;
                        end;
        end;
      end;

      if ACol = 0 then
      begin
        Canvas.Brush.color := GetMainColor(pStatistica^.Wafer.Chip[Y, X].Status);
        if pStatistica^.Wafer.Chip[Y, X].Status = 1 then Canvas.Font.Color  := clBlack
                                                    else Canvas.Font.Color  := clWhite;
      end;
    end
    else Canvas.Brush.Color := $00BFEAEE;

    Canvas.Rectangle(Rect);
    Canvas.TextOut(Rect.Left+4, Rect.Top+4, Cells[ACol, ARow]);
  end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.ParamsSGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
var                                                                                                                  //
  TmpStr: String;                                                                                                    //
begin                                                                                                                //
  with TStringGrid(Sender) do                                                                                        //
  begin                                                                                                              //
    MouseToCell(X, Y, ActCol, ActRow);                                                                               //
                                                                                                                     //
    if Button = mbLeft then                                                                                          //
    begin                                                                                                            //
      if (ActCol = 0) and (ActRow > 0) then Cells[ActCol, ActRow] := IntToStr(ActRow);                               //
                                                                                                                     //
      if ActRow > 0 then                                                                                             //
      begin                                                                                                          //
        pStatistica^.DrawChip(pStatistica^.Wafer.ChipN[ActRow-1], clFuchsia, clFuchsia);                             //
        pStatistica^.Refresh;                                                                                        //
      end;                                                                                                           //
    end;                                                                                                             //
                                                                                                                     //
    if Button = mbRight then                                                                                         //
      if (ActCol > 0) and (ActRow > 0) then                                                                          //
      begin                                                                                                          //
        HintRect.Top    := self.Top+OffsetY+ParamsSG.Top+(ActRow-ParamsSG.TopRow+1)*19;                              //
        HintRect.Bottom := HintRect.Top+16;                                                                          //
        HintRect.Left  := self.Left+OffsetX+ParamsSG.Left-65+(ActCol-ParamsSG.LeftCol+1)*65;                         //
        HintRect.Right := HintRect.Left+65;                                                                          //
                                                                                                                     //
        if pStatistica^.Wafer.TestsParams[ActCol-1].Norma.Min <> -NotSpec then                                       //
          TmpStr := FormatFloat('0.000', pStatistica^.Wafer.TestsParams[ActCol-1].Norma.Min)                         //
        else TmpStr := 'нет';                                                                                        //
        HintMin.ActivateHint(HintRect, TmpStr);                                                                      //
                                                                                                                     //
        HintRect.Left  := self.Left+OffsetX+ParamsSG.Left+65+(ActCol-ParamsSG.LeftCol+1)*65;                         //
        HintRect.Right := HintRect.Left+65;                                                                          //
                                                                                                                     //
        if pStatistica^.Wafer.TestsParams[ActCol-1].Norma.Max <> NotSpec then                                        //
          TmpStr := FormatFloat('0.000', pStatistica^.Wafer.TestsParams[ActCol-1].Norma.Max)                         //
        else TmpStr := 'нет';                                                                                        //
        HintMax.ActivateHint(HintRect, TmpStr);                                                                      //
      end;                                                                                                           //
                                                                                                                     //
    if (ActCol > 0) and (ActRow = 0) then                                                                            //
    begin                                                                                                            //
      HintRect.Top    := self.Top+OffsetY+ParamsSG.Top+(ActRow-ParamsSG.TopRow+1)*19-20;                             //
      HintRect.Bottom := HintRect.Top+18;                                                                            //
      HintRect.Left  := self.Left+OffsetX+ParamsSG.Left+(ActCol-ParamsSG.LeftCol+1)*65;                              //
      TmpStr :=  pStatistica^.Wafer.TestsParams[ActCol-1].Name;                                                      //
      HintRect.Right := HintRect.Left+(Length(TmpStr)*8);                                                            //
                                                                                                                     //
      HintTName.ActivateHint(HintRect, TmpStr);                                                                      //
    end;                                                                                                             //
  end;                                                                                                               //
end;                                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.ParamsSGMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   //
begin                                                                                                                //
  if Button = mbLeft then                                                                                            //
    with TStringGrid(Sender) do                                                                                      //
    begin                                                                                                            //
      if (ActCol = 0) and (ActRow > 0) then Cells[ActCol, ActRow] := IntToStr(ActRow);                               //
                                                                                                                     //
      if ActRow > 0 then pStatistica^.Repaint(0);                                                                    //
                                                                                                                     //
      ActCol := -1;                                                                                                  //
      ActRow := -1;                                                                                                  //
    end;                                                                                                             //
                                                                                                                     //
  if Button = mbRight then                                                                                           //
  begin                                                                                                              //
    HintMin.ReleaseHandle;                                                                                           //
    HintMax.ReleaseHandle;                                                                                           //
  end;                                                                                                               //
                                                                                                                     //
  HintTName.ReleaseHandle;                                                                                           //
end;                                                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.ParamsSGMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); //
begin                                                                                                                        //
  if Shift <> [ssShift] then SendMessage(ParamsSG.Handle, WM_VSCROLL, SB_LINEDOWN, 0)                                        //
                        else SendMessage(ParamsSG.Handle, WM_HSCROLL, SB_LINERIGHT, 0);                                      //
end;                                                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.ParamsSGMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);   //
begin                                                                                                                        //
  if Shift <> [ssShift] then SendMessage(ParamsSG.Handle, WM_VSCROLL, SB_LINEUP, 0)                                          //
                        else SendMessage(ParamsSG.Handle, WM_HSCROLL, SB_LINELEFT, 0);                                       //
end;                                                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.ParamsSGSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean); //
begin                                                                                                     //
  CanSelect := False;                                                                                     //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.DlgMouseLeave(var Message: TMessage); //
begin                                                         //
  HintMin.ReleaseHandle;                                      //
  HintMax.ReleaseHandle;                                      //
  HintTName.ReleaseHandle;                                    //
end;                                                          //
////////////////////////////////////////////////////////////////


end.

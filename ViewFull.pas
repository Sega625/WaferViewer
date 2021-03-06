unit ViewFull;

interface

uses
  Windows, SysUtils, Messages, Graphics, Forms, Structs, Grids, Buttons, Controls, StdCtrls, Statistica, Classes;

type
  TFullChipsDlg = class(TForm)
    ParamsSG: TStringGrid;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnyCompMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ParamsSGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  public
    constructor Create(AOwner: TComponent; pStat: PStatistica);
  private
    pStatistica: PStatistica;
    fOnChipDlgClose: TOnChipDlgClose;
    fNTests: WORD;
    fNChips: DWORD;
  published
    property OnChipDlgClose: TOnChipDlgClose read fOnChipDlgClose write fOnChipDlgClose;
  end;

var
  FullChipsDlg: TFullChipsDlg;


implementation

{$R *.dfm}

///////////////////////////////////////////////////////////////////////////////////////////////
constructor TFullChipsDlg.Create(AOwner: TComponent; pStat: PStatistica);                    //
var                                                                                          //
  X, Y, n: WORD;                                                                             //
  m, k: DWORD;                                                                               //
begin                                                                                        //
  inherited Create(AOwner);                                                                  //
                                                                                             //
  ControlStyle := ControlStyle+[csOpaque];                                                   //
                                                                                             //
  pStatistica := pStat;                                                                      //
                                                                                             //
  fNTests := Length(pStatistica^.Wafer.TestsParams);                                         //
  fNChips := pStatistica^.Wafer.NTotal;                                                      //
                                                                                             //
  ParamsSG.ColCount := fNTests+1;                                                            //
  ParamsSG.RowCount := fNChips+1;                                                            //
                                                                                             //
  ParamsSG.Cells[0, 0] := ' �';                                                              //
  for n := 0 to fNTests do ParamsSG.Cells[n+1, 0] := pStatistica^.Wafer.TestsParams[n].Name; //
  for m := 0 to fNChips do ParamsSG.Cells[0, m+1] := IntToStr(m+1);                          //
{                                                                                             //
  m := 0;                                                                                    //
  with pStatistica^ do                                                                       //
    for Y := 0 to Length(Wafer.Chip)-1 do                                                    //
      for X := 0 to Length(Wafer.Chip[0])-1 do                                               //
      begin                                                                                  //
        if Wafer.Chip[Y, X].Status <> 2 then                                                 //
        begin                                                                                //
          Inc(m);                                                                            //
          if m > fNChips then Break;                                                         //
                                                                                             //
          if Length(Wafer.TestsParams) > 0 then                                              //
          for n := 0 to Length(Wafer.TestsParams)-1 do                                       //
            if  Wafer.Chip[Y, X].Value[n] <> NotSpec then                                    //
              ParamsSG.Cells[n+1, m] := FormatFloat('0.000', Wafer.Chip[Y, X].Value[n])      //
            else ParamsSG.Cells[n+1, m] := '���';                                            //
        end;                                                                                 //
      end;                                                                                   //
}                                                                                             //
  m := 0;
  with pStatistica^.Wafer do
    for k := 0 to fNChips-1 do
    begin
      Inc(m);
      if Length(Chip[ChipN[k].Y, ChipN[k].X].Value) > 0 then
        for n := 0 to Length(Chip[ChipN[k].Y, ChipN[k].X].Value)-1 do
          if Chip[ChipN[k].Y, ChipN[k].X].Value[n] <> NotSpec then                                //
            ParamsSG.Cells[n+1, m] := FormatFloat('0.000', Chip[ChipN[k].Y, ChipN[k].X].Value[n]) //
          else ParamsSG.Cells[n+1, m] := '���';                                            //
    end;


  m := 0;                                                                                    //
  for n := 0 to ParamsSG.ColCount-1 do m := m+ParamsSG.ColWidths[n];                         //
  ParamsSG.Width := m+ParamsSG.ColCount*2+12;                                                //
  self.ClientWidth := ParamsSG.Width+12;                                                     //
  self.Constraints.MinWidth  := ParamsSG.ColWidths[0]+ParamsSG.ColWidths[1]+49;              //
  self.Constraints.MaxWidth  := Screen.DesktopWidth-100;                                     //
  self.Constraints.MinHeight := ParamsSG.RowHeights[0]+ParamsSG.RowHeights[1]+49;            //
  self.Constraints.MaxHeight := Screen.DesktopHeight-100;                                    //
end;                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormClose(Sender: TObject; var Action: TCloseAction);                //
begin                                                                                        //
  if Assigned(OnChipDlgClose) then OnChipDlgClose;                                           //
end;                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormDestroy(Sender: TObject);                                        //
var                                                                                          //
  n: WORD;                                                                                   //
  m: DWORD;                                                                                  //
begin                                                                                        //
//    for n := 0 to fNTests-1 do                                                               //
//    begin                                                                                    //
//      NLab[n].Free;                                                                          //
//      for m := 0 to fNChips-1 do ParamLab[n][m].Free;
//  SetLength(ChipN, 0);
end;                                                                                         //
///////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //
begin                                                                                //
  if Key = VK_ESCAPE then Close;                                                     //
end;                                                                                 //
///////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////
procedure TFullChipsDlg.FormResize(Sender: TObject); //
begin                                                //
  ParamsSG.Height := self.ClientHeight-12;           //
  ParamsSG.Width  := self.ClientWidth-12;
                                                     //
//  if ParamsSG.VertScrollBar.IsScrollBarVisible then    //
//    ParamsSG.Width := HandlerSB.Width+17               //
//  else                                               //
//    ChipSB.Width := HandlerSB.Width;                 //
end;                                                 //
///////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TFullChipsDlg.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); //
begin                                                                                                                                     //
//  if WheelDelta < 0 then ChipSB.VertScrollBar.Position := ChipSB.VertScrollBar.Position+20                                                //
//                    else ChipSB.VertScrollBar.Position := ChipSB.VertScrollBar.Position-20;                                               //
end;                                                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
  clPaleBlue  = clSkyBlue;//TColor($FFCCCC);
var
  X, Y: WORD;
begin    
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
    Canvas.Pen.Color := clGray;
    Canvas.Brush.Color := clWhite;



    if ARow > 0 then
    begin
      X := pStatistica^.Wafer.ChipN[ARow-1].X;
      Y := pStatistica^.Wafer.ChipN[ARow-1].Y;

      case pStatistica^.Wafer.Chip[Y, X].Status of
        0         : ;
        1         : Canvas.Brush.color := clPaleGreen;
        2         : ;
        3         : ;
        4         : ;
        5, 7      : ;
        10..1500  : Canvas.Brush.Color := clGray;
        2000..3000: Canvas.Brush.Color := clPaleRed;
        3500..4500: Canvas.Brush.Color := clPaleBlue;
      end;
    end
    else Canvas.Brush.Color := clYellow;

    Canvas.Rectangle(Rect);
    Canvas.TextOut(Rect.Left+4, Rect.Top+4, Cells[ACol, ARow]);
  end;
end;


end.

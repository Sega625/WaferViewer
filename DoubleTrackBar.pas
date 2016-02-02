unit DoubleTrackBar;

interface

uses
  Windows, Classes, Graphics, Controls, Messages, SysUtils;

type
  TDoubleTrackBar = class(TCustomControl)
  private
    fType: byte;
    WBitmap, SelBmp: TBitmap;
    ThumbBmp: array[0..1] of TBitmap;
    ThumbPosX: array[0..1] of Integer;
    ThumbPosY: Integer;
    ThumbPressed: array[0..1] of Boolean;

    procedure Paint; override;

    procedure DrawFon(const withFocus: Boolean=False);
    procedure DrawThumb(const Num: byte);

    procedure DTBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DTBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DTBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DTBMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DTBMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DTBEnter(Sender: TObject);
    procedure DTBExit(Sender: TObject);

    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;

    function GetThumbNum(const X, Y: Integer): byte;
  public
    constructor Create(AOwner: TComponent; const tp: byte=0);
    destructor  Destroy; override;
  end;

implementation


{ TDoubleTrackBar }

////////////////////////////////////////////////////////////////////////////
constructor TDoubleTrackBar.Create(AOwner: TComponent; const tp: byte=0); //
var
  n: byte;
begin
  inherited Create(AOwner);

  Parent := TWinControl(AOwner);

  fType := tp;

  ControlStyle := ControlStyle+[csOpaque];
  TabStop := True;

  self.Width  := 332;
  self.Height := 40;

  OnKeyDown    := DTBKeyDown;
  OnMouseDown  := DTBMouseDown;
  OnMouseMove  := DTBMouseMove;
  OnMouseUp    := DTBMouseUp;
  OnMouseWheel := DTBMouseWheel;
  OnEnter      := DTBEnter;
  OnExit       := DTBExit;

  WBitmap := TBitmap.Create;
  WBitmap.Width  := self.Width;
  WBitmap.Height := self.Height;

  SelBmp := TBitmap.Create;
  SelBmp.Width  := self.Width;
  SelBmp.Height := 9;
  with SelBmp.Canvas do
  begin
    Pen.Color := $B4E6FF;
    Brush.Color := Pen.Color;
    Rectangle(0,0, SelBmp.Width, SelBmp.Height);
  end;

  ThumbPosY := self.Height div 2-7;
  for n := 0 to 1 do
  begin
    ThumbBmp[n] := TBitmap.Create;
    with ThumbBmp[n] do
    begin
      Width  := 8;
      Height := 16;

      LoadFromFile('D:\Add_Programs\WaferViewer\Adds\new\2\Thumb'+IntToStr(n)+'_1.bmp');

      TransparentColor := clFuchsia;
      Transparent := True;
    end;

    ThumbPosX[n] := (WBitmap.Width div 2)+n*50;
    ThumbPressed[n] := False;
  end;

  DrawFon;
end;
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
destructor TDoubleTrackBar.Destroy;
var
  n: byte;
begin
  for n := 0 to 1 do ThumbBmp[n].Free;

  SelBmp.Free;
  WBitmap.Free;

  inherited Destroy;
end;
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
procedure TDoubleTrackBar.Paint;
var
  dX: Integer;
begin
  inherited;

  dX := ThumbPosX[0]+ThumbBmp[0].Width;

  BitBlt(Canvas.Handle, 0,0, self.Width,self.Height, WBitmap.Canvas.Handle, 0,0, SRCCOPY); // Фон
  StretchBlt(Canvas.Handle, dX, WBitmap.Height div 2-4, ThumbPosX[1]-dX,SelBmp.Height, SelBmp.Canvas.Handle, 0,0, SelBmp.Width,SelBmp.Height,SRCAND);
  Canvas.Draw(ThumbPosX[0], ThumbPosY, ThumbBmp[0]);
  Canvas.Draw(ThumbPosX[1], ThumbPosY, ThumbBmp[1]);
end;
////////////////////////////////////////////////////////////////////////////


procedure TDoubleTrackBar.DTBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  n: byte;
begin
  SetFocus;

  n := GetThumbNum(X, Y);
  if n = 255 then Exit;

  ThumbPressed[n] := True;
end;

procedure TDoubleTrackBar.DTBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ThumbPressed[0] then
  begin
    if X > (ThumbPosX[1]-ThumbBmp[1].Width+1) then X := ThumbPosX[1]-ThumbBmp[1].Width+1;
    if X < 0 then X := 0;

    ThumbPosX[0] := X;
    self.Repaint;
  end;

  if ThumbPressed[1] then
  begin
    if X < (ThumbPosX[0]+ThumbBmp[0].Width-1) then X := ThumbPosX[0]+ThumbBmp[0].Width-1;
    if X > WBitmap.Width-ThumbBmp[1].Width then X := WBitmap.Width-ThumbBmp[1].Width;

    ThumbPosX[1] := X;
    self.Repaint;
  end;
end;

procedure TDoubleTrackBar.DTBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ThumbPressed[0] := False;
  ThumbPressed[1] := False;
end;

procedure TDoubleTrackBar.DTBMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //
end;




procedure TDoubleTrackBar.DrawFon(const withFocus: Boolean=False);
var
  X, Y: WORD;
  Rect: TRect;
begin
  X := WBitmap.Width div 2;
  Y := WBitmap.Height div 2;

  with WBitmap.Canvas do
  begin
    if withFocus then Pen.Color := $000080FF
                 else Pen.Color := clBtnFace;//$C19573;
    Brush.Color := clBtnFace;
//    Brush.Color := $FFEFE3;
    Rectangle(0,0, WBitmap.Width,WBitmap.Height);

    Pen.Color := $C19573;                       //
    Pen.Width := 17;                             // Желобок
    MoveTo(ThumbBmp[0].Width, Y);               //
    LineTo(WBitmap.Width-ThumbBmp[1].Width, Y); //

    Pen.Width := 1;                                                //
    Pen.Color := clGray;
    MoveTo(ThumbBmp[0].Width-1, WBitmap.Height div 4);             //
    LineTo(ThumbBmp[0].Width-1, WBitmap.Height);                   //
    MoveTo(WBitmap.Width-ThumbBmp[1].Width, WBitmap.Height div 4); // Засечки
    LineTo(WBitmap.Width-ThumbBmp[1].Width, WBitmap.Height);       //
    MoveTo(X-40, Y+8);                                             //
    LineTo(X-40, WBitmap.Height);                                  //
    MoveTo(X+40, Y+8);                                             //
    LineTo(X+40, WBitmap.Height);                                  //


    if fType = 0 then
    begin
      Pen.Color := $00AAD898;            //
      Brush.Color := Pen.Color;          // Норма
      Rectangle(X-40+1,Y+10, X+40,Y+15); //

      Pen.Color := $00B3A0FA;                                       //
      Brush.Color := Pen.Color;                                     // Брак
      Rectangle(ThumbBmp[0].Width,Y+10, X-40,Y+15);                 //
      Rectangle(X+40+1,Y+10, WBitmap.Width-ThumbBmp[1].Width,Y+15); //
    end;
    Font.Size := 7;
    Font.Color := clGray;
    Brush.Style := bsClear;
    Rect.Left := 1;
    Rect.Right := Rect.Left+TextWidth('000000');
    Rect.Top := 1;
    Rect.Bottom := Rect.Top+TextWidth('0')+4;
    DrawText(Handle, '0', -1, Rect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);

    Rect.Right := WBitmap.Width;
    Rect.Left := Rect.Right-TextWidth('000000');
    DrawText(Handle, '1000', -1, Rect, DT_SINGLELINE or DT_VCENTER or DT_RIGHT);

  end;
end;


procedure TDoubleTrackBar.DrawThumb(const Num: byte);
var
  PArray: array[0..3] of TPoint;
begin
  PArray[0] := Point(0,0);
  PArray[1] := Point(ThumbBmp[Num].Width-1,0);
  if Num = 0 then
  begin
    PArray[2] := Point(ThumbBmp[Num].Width-1,ThumbBmp[Num].Height-1);
    PArray[3] := Point(0, ThumbBmp[Num].Height-ThumbBmp[Num].Height div 3);
  end
  else
  begin
    PArray[2] := Point(ThumbBmp[Num].Width-1, ThumbBmp[Num].Height-ThumbBmp[Num].Height div 3);
    PArray[3] := Point(0,ThumbBmp[Num].Height-1);

  end;

  with ThumbBmp[Num].Canvas do
  begin
    Pen.Color := clBlack;
    Brush.Color := clGray;
    Polygon(PArray);
  end;
end;


function TDoubleTrackBar.GetThumbNum(const X, Y: Integer): byte;
var
  n: byte;
begin
  Result := 255;

  for n := 0 to 1 do
    if (X > ThumbPosX[n])                 and (X < ThumbPosX[n]+ThumbBmp[n].Width) and
       (Y > ThumbPosY) and (Y < ThumbPosY+ThumbBmp[n].Height) then
  begin
    Result := n;
    Break;
  end;
end;


procedure TDoubleTrackBar.DTBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  MouseDown(self, mbLeft, [], 0,0);
//  MouseMove(self, [], 0,0);
//  MouseUp(self, mbLeft, [], 0,0);
end;


procedure TDoubleTrackBar.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS; // Стрелки не переключают фокус
end;


procedure TDoubleTrackBar.DTBEnter(Sender: TObject);
begin
  DrawFon(True);
  RePaint;
end;

procedure TDoubleTrackBar.DTBExit(Sender: TObject);
begin
  DrawFon;
  RePaint;
end;

end.

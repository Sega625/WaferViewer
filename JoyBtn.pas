unit JoyBtn;

interface

uses
  Windows, Classes, Graphics, Controls, ExtCtrls, Messages, SysUtils;

type
  TOnJoyDown  = procedure(const TypeBtn: byte) of object;
  TOnJoyUp    = procedure(const TypeBtn: byte) of object;

  TAllBtn = (NotBtn, ABtn, RightUpBtn, RightBtn, RightDownBtn, DownBtn, LeftDownBtn, LeftBtn, LeftUpBtn, UpBtn);

  TJoyBtn = class(TGraphicControl)
  public
    constructor Create(AOwner: TComponent; BtnType: byte=0);
    destructor  Destroy; override;
  private
    fOnJoyDown: TOnJoyDown;
    fOnJoyUp  : TOnJoyUp;

    JoyRgn,
    AllBtnRgn,
    BtnARgn,
    RightUpBtnRgn,
    RightBtnRgn,
    RightDownBtnRgn,
    DownBtnRgn,
    LeftDownBtnRgn,
    LeftBtnRgn,
    LeftUpBtnRgn,
    UpBtnRgn: HRGN;

    MouseDowned: Boolean;
    MouseLeaved: Boolean;
    BtnState: byte;
    JoyBMP: TBitmap;
    JoyYelBMP,
    JoyGrnBMP: array[0..8] of TBitmap;

    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure BtnMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    function  GetBtnFromCoord(X, Y: Integer): TAllBtn;
  published
    property OnJoyDown: TOnJoyDown read fOnJoyDown write fOnJoyDown;
    property OnJoyUp  : TOnJoyUp   read fOnJoyUp   write fOnJoyUp;

    procedure Paint; override;
  end;

implementation

{$R *.RES}

{ TStatistica }

///////////////////////////////////////////////////////////////////////////
constructor TJoyBtn.Create(AOwner: TComponent; BtnType: byte=0);         //
var                                                                      //
  n: byte;                                                               //
  P: array[0..12] of TPoint;                                             //
  tP: array[0..3] of TPoint;                                             //
  JoyWidth, JoyHeight, BtnAWidth, BtnAHeight: WORD;                      //
begin                                                                    //
  inherited Create(AOwner);                                              //
                                                                         //
  Parent := TWinControl(AOwner);                                         //
  Top    := 0;                                                           //
  Left   := 0;                                                           //
  Width  := 80;                                                          //
  Height := 80;                                                          //
  Color := clBtnFace;                                                    //
  ControlStyle := ControlStyle+[csOpaque];                               //
  OnMouseDown := BtnMouseDown;                                           //
  OnMouseUp   := BtnMouseUp;                                             //
  OnMouseMove := BtnMouseMove;                                           //
                                                                         //
////////////////////////////                                             //
                                                                         //
  JoyWidth  := Width;                                                    //
  JoyHeight := Height;                                                   //
  BtnAWidth  := 37;                                                      //
  BtnAHeight := 37;                                                      //
                                                                         //
  P[0]  := Point(JoyWidth div 2, JoyHeight div 2); // Центр              //
  P[1]  := Point(P[0].X+JoyWidth div 4, 0);                              //
  P[2]  := Point(JoyWidth, 0);                                           //
  P[3]  := Point(JoyWidth, P[0].Y-JoyHeight div 4);                      //
  P[4]  := Point(JoyWidth, P[0].Y+JoyHeight div 4);                      //
  P[5]  := Point(JoyWidth, JoyHeight);                                   //
  P[6]  := Point(P[0].X+JoyWidth div 4, JoyHeight);                      //
  P[7]  := Point(P[0].X-JoyWidth div 4, JoyHeight);                      //
  P[8]  := Point(0, JoyHeight);                                          //
  P[9]  := Point(0, P[0].Y+JoyHeight div 4);                             //
  P[10] := Point(0, P[0].Y-JoyHeight div 4);                             //
  P[11] := Point(0, 0);                                                  //
  P[12] := Point(P[0].X-JoyWidth div 4, 0);                              //
                                                                         //
///////////////////                                                      //
                                                                         //
  JoyRgn := CreateRoundRectRgn(0, 0, JoyWidth, JoyHeight, 45, 45);       //
                                                                         //
///////////////////                                                      //
                                                                         //
  BtnARgn := CreateRoundRectRgn(0, 0, BtnAWidth, BtnAHeight, 20, 20);    //
  OffsetRgn(BtnARgn, P[0].X-BtnAWidth div 2, P[0].Y-BtnAHeight div 2);   //
                                                                         //
///////////////////                                                      //
                                                                         //
  AllBtnRgn := CreateRectRgn(0,0,0,0);                                   //
  CombineRgn(AllBtnRgn, JoyRgn, BtnARgn, RGN_XOR);                       //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[1];                                                         //
  tP[2] := P[2];                                                         //
  tP[3] := P[3];                                                         //
  RightUpBtnRgn := CreatePolygonRgn(tP, 4, WINDING);                     //
  CombineRgn(RightUpBtnRgn, RightUpBtnRgn, BtnARgn, RGN_DIFF);           //
  CombineRgn(RightUpBtnRgn, RightUpBtnRgn, AllBtnRgn, RGN_MIN);          //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[3];                                                         //
  tP[2] := P[4];                                                         //
  RightBtnRgn := CreatePolygonRgn(tP, 3, WINDING);                       //
  CombineRgn(RightBtnRgn, RightBtnRgn, BtnARgn, RGN_DIFF);               //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[4];                                                         //
  tP[2] := P[5];                                                         //
  tP[3] := P[6];                                                         //
  RightDownBtnRgn := CreatePolygonRgn(tP, 4, WINDING);                   //
  CombineRgn(RightDownBtnRgn, RightDownBtnRgn, BtnARgn, RGN_DIFF);       //
  CombineRgn(RightDownBtnRgn, RightDownBtnRgn, AllBtnRgn, RGN_MIN);      //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[6];                                                         //
  tP[2] := P[7];                                                         //
  DownBtnRgn := CreatePolygonRgn(tP, 3, WINDING);                        //
  CombineRgn(DownBtnRgn, DownBtnRgn, BtnARgn, RGN_DIFF);                 //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[7];                                                         //
  tP[2] := P[8];                                                         //
  tP[3] := P[9];                                                         //
  LeftDownBtnRgn := CreatePolygonRgn(tP, 4, WINDING);                    //
  CombineRgn(LeftDownBtnRgn, LeftDownBtnRgn, BtnARgn, RGN_DIFF);         //
  CombineRgn(LeftDownBtnRgn, LeftDownBtnRgn, AllBtnRgn, RGN_MIN);        //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[9];                                                         //
  tP[2] := P[10];                                                        //
  LeftBtnRgn := CreatePolygonRgn(tP, 3, WINDING);                        //
  CombineRgn(LeftBtnRgn, LeftBtnRgn, BtnARgn, RGN_DIFF);                 //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[10];                                                        //
  tP[2] := P[11];                                                        //
  tP[3] := P[12];                                                        //
  LeftUpBtnRgn := CreatePolygonRgn(tP, 4, WINDING);                      //
  CombineRgn(LeftUpBtnRgn, LeftUpBtnRgn, BtnARgn, RGN_DIFF);             //
  CombineRgn(LeftUpBtnRgn, LeftUpBtnRgn, AllBtnRgn, RGN_MIN);            //
                                                                         //
///////////////////                                                      //
                                                                         //
  tP[0] := P[0];                                                         //
  tP[1] := P[12];                                                        //
  tP[2] := P[1];                                                         //
  UpBtnRgn := CreatePolygonRgn(tP, 3, WINDING);                          //
  CombineRgn(UpBtnRgn, UpBtnRgn, BtnARgn, RGN_DIFF);                     //
                                                                         //
///////////////////////////////////////////////////////////////////////////
                                                                         //
  JoyBMP := TBitmap.Create;                                              //
  with JoyBMP do                                                         //
  begin                                                                  //
    Top    := 0;                                                         //
    Left   := 0;                                                         //
    Width  := self.Width;                                                //
    Height := self.Height;                                               //
    ControlStyle := ControlStyle+[csOpaque];                             //
    LoadFromResourceName(HINSTANCE, 'JSGRAY');                           //
    Transparent := True;                                                 //
  end;                                                                   //
                                                                         //
///////////////////////////////////////////////////////////////////////////
                                                                         //
  for n := 0 to Length(JoyYelBMP)-1 do                                   //
  begin                                                                  //
    JoyYelBMP[n] := TBitmap.Create;                                      //
    with JoyYelBMP[n] do                                                 //
    begin                                                                //
      Top    := 0;                                                       //
      Left   := 0;                                                       //
      Width  := self.Width;                                              //
      Height := self.Height;                                             //
      ControlStyle := ControlStyle+[csOpaque];                           //
      LoadFromResourceName(HINSTANCE, 'JSYEL'+IntToStr(n));              //
      Transparent := True;                                               //
    end;                                                                 //
  end;                                                                   //
                                                                         //
///////////////////////////////////////////////////////////////////////////
                                                                         //
  for n := 0 to Length(JoyYelBMP)-1 do                                   //
  begin                                                                  //
    JoyGrnBMP[n] := TBitmap.Create;                                      //
    with JoyGrnBMP[n] do                                                 //
    begin                                                                //
      Top    := 0;                                                       //
      Left   := 0;                                                       //
      Width  := self.Width;                                              //
      Height := self.Height;                                             //
      ControlStyle := ControlStyle+[csOpaque];                           //
      LoadFromResourceName(HINSTANCE, 'JSGRN'+IntToStr(n));              //
      Transparent := True;                                               //
    end;                                                                 //
  end;                                                                   //
                                                                         //
  DeleteObject(JoyRgn);                                                  //
  DeleteObject(AllBtnRgn);                                               //
                                                                         //
///////////////////////////////////////////////////////////////////////////
                                                                         //
  MouseDowned := False;                                                  //
  MouseLeaved := True;                                                   //
  BtnState := 255;                                                       //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
destructor TJoyBtn.Destroy;                                              //
var                                                                      //
  n: byte;                                                               //
begin                                                                    //
  JoyBMP.Free;                                                           //
                                                                         //
  for n := 0 to 8 do                                                     //
  begin                                                                  //
    JoyYelBMP[n].Free;                                                   //
    JoyGrnBMP[n].Free;                                                   //
  end;                                                                   //
                                                                         //
  DeleteObject(BtnARgn);                                                 //
  DeleteObject(RightUpBtnRgn);                                           //
  DeleteObject(RightBtnRgn);                                             //
  DeleteObject(RightDownBtnRgn);                                         //
  DeleteObject(DownBtnRgn);                                              //
  DeleteObject(LeftDownBtnRgn);                                          //
  DeleteObject(LeftBtnRgn);                                              //
  DeleteObject(LeftUpBtnRgn);                                            //
  DeleteObject(UpBtnRgn);                                                //
                                                                         //
  inherited Destroy;                                                     //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.Paint;                                                 //
begin                                                                    //
  if BtnState = 255       then Canvas.Draw(0,0, JoyBMP);                 //
  if BtnState in [0..8]   then Canvas.Draw(0,0, JoyYelBMP[BtnState]);    //
  if BtnState in [10..18] then Canvas.Draw(0,0, JoyGrnBMP[BtnState-10]); //
                                                                         //
  inherited Paint;                                                       //
end;                                                                     //
///////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
var                                                                                                       //
  prBtnState: byte;                                                                                       //
begin                                                                                                     //
  if Button <> mbLeft then Exit;                                                                          //
                                                                                                          //
  MouseDowned  := True;                                                                                   //
                                                                                                          //
  prBtnState := BtnState;                                                                                 //
                                                                                                          //
  case GetBtnFromCoord(X, Y) of                                                                           //
    ABtn        : begin                                                                                   //
                    BtnState := 10;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(0);                                             //
                  end;                                                                                    //
    RightUpBtn  : begin                                                                                   //
                    BtnState := 11;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(1);                                             //
                  end;                                                                                    //
    RightBtn    : begin                                                                                   //
                    BtnState := 12;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(2);                                             //
                  end;                                                                                    //
    RightDownBtn: begin                                                                                   //
                    BtnState := 13;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(3);                                             //
                  end;                                                                                    //
    DownBtn     : begin                                                                                   //
                    BtnState := 14;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(4);                                             //
                  end;                                                                                    //
    LeftDownBtn : begin                                                                                   //
                    BtnState := 15;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(5);                                             //
                  end;                                                                                    //
    LeftBtn     : begin                                                                                   //
                    BtnState := 16;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(6);                                             //
                  end;                                                                                    //
    LeftUpBtn   : begin                                                                                   //
                    BtnState := 17;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(7);                                             //
                  end;                                                                                    //
    UpBtn       : begin                                                                                   //
                    BtnState := 18;                                                                       //
                    if BtnState <> prBtnState then Repaint;                                               //
                    if Assigned(OnJoyDown) then OnJoyDown(8);                                             //
                  end;                                                                                    //  
    NotBtn      : ;                                                                                       //
  end;                                                                                                    //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   //
begin                                                                                                     //
  if Button <> mbLeft then Exit;                                                                          //
                                                                                                          //
  MouseDowned := False;                                                                                   //
                                                                                                          //
  if Assigned(OnJoyUp) then OnJoyUp(BtnState-10);                                                         //
                                                                                                          //
  if MouseLeaved then                                                                                     //
    if BtnState <> 255 then                                                                               //
      begin                                                                                               //
        BtnState := 255;                                                                                  //
        Repaint;                                                                                          //
      end;                                                                                                //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.BtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);                       //
var                                                                                                       //
  prBtnState: byte;                                                                                       //
begin                                                                                                     //
  if MouseDowned then Exit;                                                                               //
                                                                                                          //
  prBtnState := BtnState;                                                                                 //
                                                                                                          //
  case GetBtnFromCoord(X, Y) of                                                                           //
    ABtn        : begin                                                                                   //
                    BtnState := 0;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    RightUpBtn  : begin                                                                                   //
                    BtnState := 1;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    RightBtn    : begin                                                                                   //
                    BtnState := 2;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    RightDownBtn: begin                                                                                   //
                    BtnState := 3;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    DownBtn     : begin                                                                                   //
                    BtnState := 4;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    LeftDownBtn : begin                                                                                   //
                    BtnState := 5;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    LeftBtn     : begin                                                                                   //
                    BtnState := 6;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    LeftUpBtn   : begin                                                                                   //
                    BtnState := 7;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    UpBtn       : begin                                                                                   //
                    BtnState := 8;                                                                        //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
    NotBtn      : begin                                                                                   //
                    BtnState := 255;                                                                      //
                    if BtnState <> prBtnState then Repaint;                                               //
                  end;                                                                                    //
  end;                                                                                                    //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.BtnMouseLeave(var Message: TMessage);                                                   //
begin                                                                                                     //
  MouseLeaved := True;                                                                                    //
                                                                                                          //
  if not MouseDowned then                                                                                 //
    if BtnState <> 255 then                                                                               //
    begin                                                                                                 //
      BtnState := 255;                                                                                    //
      Repaint;                                                                                            //
    end;                                                                                                  //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TJoyBtn.BtnMouseEnter(var Message: TMessage);                                                   //
begin                                                                                                     //
  MouseLeaved := False;                                                                                   //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////
function TJoyBtn.GetBtnFromCoord(X, Y: Integer): TAllBtn;           //
begin                                                               //
  Result := NotBtn;                                                 //
                                                                    //
  if PtInRegion(BtnARgn, X, Y)         then Result := ABtn;         //
  if PtInRegion(RightUpBtnRgn, X, Y)   then Result := RightUpBtn;   //
  if PtInRegion(RightBtnRgn, X, Y)     then Result := RightBtn;     //
  if PtInRegion(RightDownBtnRgn, X, Y) then Result := RightDownBtn; //
  if PtInRegion(DownBtnRgn, X, Y)      then Result := DownBtn;      //
  if PtInRegion(LeftDownBtnRgn, X, Y)  then Result := LeftDownBtn;  //
  if PtInRegion(LeftBtnRgn, X, Y)      then Result := LeftBtn;      //
  if PtInRegion(LeftUpBtnRgn, X, Y)    then Result := LeftUpBtn;    //
  if PtInRegion(UpBtnRgn, X, Y)        then Result := UpBtn;        //
end;                                                                //
//////////////////////////////////////////////////////////////////////


end.

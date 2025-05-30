unit ViewPD;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Structs, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Statistica;

type
  TPDDlg = class(TForm)
    PBox: TPaintBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    WBitmap: TBitmap;
    SizeChipX, SizeChipY, OffsX, OffsY: byte;
    pStatistica: PStatistica;
    Flag: Boolean;

    procedure PreShowPD(const ShowOK: Boolean=True);
    procedure DrawChip(const XY: TPoint; const Count: WORD; const cCol: TColor);

    procedure PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBoxPaint(Sender: TObject);
  public
    constructor Create(Sender: TObject; pStat: PStatistica);
  end;

var
  PDDlg: TPDDlg;

implementation

{$R *.dfm}

{ TPDDlg }

/////////////////////////////////////////////////////////////////////////
constructor TPDDlg.Create(Sender: TObject; pStat: PStatistica);        //
begin                                                                  //
  inherited Create(TComponent(Sender));                                //
                                                                       //
  pStatistica := pStat;                                                //
                                                                       //
  PBox := TPaintBox.Create(self);                                      //
  with PBox do                                                         //
  begin                                                                //
    Parent := self;                                                    //
                                                                       //
    OnMouseDown  := PBoxMouseDown;                                     //
    OnPaint      := PBoxPaint;                                         //
  end;                                                                 //
  WBitmap := TBitmap.Create;                                           //
  Flag := True;                                                        //
                                                                       //
  PreShowPD;                                                           //
end;                                                                   //
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
procedure TPDDlg.FormClose(Sender: TObject; var Action: TCloseAction); //
begin                                                                  //
//  Action := caFree;                                                    //
end;                                                                   //
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
procedure TPDDlg.FormDestroy(Sender: TObject);                         //
begin                                                                  //
  WBitmap.Free;                                                        //
  PBox.Free;                                                           //
end;                                                                   //
/////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TPDDlg.PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
begin                                                                                                     //
  Flag := not Flag;                                                                                       //
  if Flag then self.Caption := ' Годных в кадре' else self.Caption := ' Брака в кадре';                   //
                                                                                                          //
  PreShowPD(Flag);                                                                                        //
end;                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////
procedure TPDDlg.PBoxPaint(Sender: TObject); //
begin                                        //
  PBox.Canvas.Draw(0,0, WBitmap);            //
end;                                         //
///////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TPDDlg.PreShowPD(const ShowOK: Boolean=True);                                             //
var                                                                                                 //
  X, Y, MaxOK: WORD;                                                                                //
  Col: TColor;                                                                                      //
  Count: DWORD;                                                                                     //
begin                                                                                               //
  SizeChipX := 30;                                                                                  //
  SizeChipY := 20;                                                                                  //
  OffsX := 15;                                                                                      //
  OffsY := 15;                                                                                      //
                                                                                                    //
  WBitmap.Canvas.Brush.Color := $0080FFFF;                                                          //
                                                                                                    //
  with pStatistica^.Wafer do                                                                        //
  begin                                                                                             //
    MaxOK := PD[0,0].OK;                                                                            //
    for Y := 0 to Length(PD)-1 do                                                                   //
      for X := 0 to Length(PD[0])-1 do                                                              //
        if PD[Y,X].OK > MaxOK then MaxOK := PD[Y,X].OK;                                             //
                                                                                                    //
    WBitmap.Width  := SizeChipX*Length(PD[0])+Round(1.5*OffsX);                                     //
    WBitmap.Height := SizeChipY*Length(PD)   +Round(1.5*OffsY);                                     //
    PBox.Width  := WBitmap.Width;                                                                   //
    PBox.Height := WBitmap.Height;                                                                  //
    PBox.Top  := 2;                                                                                 //
    PBox.Left := 2;                                                                                 //
//    self.Width  := PBox.Width+10;                                                                   //
//    self.Height := PBox.Height+28;                                                                  //
    self.ClientWidth  := PBox.Width+4;                                                              //
    self.ClientHeight := PBox.Height+4;                                                             //
                                                                                                    //
    with WBitmap.Canvas do                                                                          //
    begin                                                                                           //
      Font.Height := 11;                                                                            //
      Font.Color := clBlack;                                                                        //
      for X := 0 to Length(PD[0])-1 do                                                              //
      begin                                                                                         //
        MoveTo(OffsX+X*SizeChipX, OffsY);                                                           //
        LineTo(OffsX+X*SizeChipX, OffsY-6);                                                         //
        TextOut(SizeChipX*X+SizeChipX+1, 2, IntToStr(X+1));                                         //
      end;                                                                                          //
      MoveTo(OffsX+Length(PD[0])*SizeChipX, OffsY);                                                 //
      LineTo(OffsX+Length(PD[0])*SizeChipX, OffsY-6);                                               //
      for Y := 0 to Length(PD)-1 do                                                                 //
      begin                                                                                         //
        MoveTo(OffsX,   OffsY+Y*SizeChipY);                                                         //
        LineTo(OffsX-6, OffsY+Y*SizeChipY);                                                         //
        TextOut(3, SizeChipY*Y+SizeChipY+2, IntToStr(Y+1));                                         //
      end;                                                                                          //
      MoveTo(OffsX,   OffsY+Length(PD)*SizeChipY);                                                  //
      LineTo(OffsX-6, OffsY+Length(PD)*SizeChipY);                                                  //
      Font.Height := 13;                                                                            //
                                                                                                    //
      for Y := 0 to Length(PD)-1 do                                                                 //
        for X := 0 to Length(PD[0])-1 do                                                            //
        begin                                                                                       //
          Col := clSilver;                                                                          //
          Count := 0;                                                                               //
                                                                                                    //
          if PD[Y,X].Meas then                                                                      //
            if ShowOK then                                                                          //
            begin                                                                                   //
              if PD[Y,X].OK = 0 then Col := clRed else                                              //
                if PD[Y,X].OK < (MaxOK/5) then Col := $009F9FFF else Col := clLime;                 //
                                                                                                    //
              Count := PD[Y,X].OK;                                                                  //
            end                                                                                     //
            else                                                                                    //
            begin                                                                                   //
              if PD[Y,X].Fail = 0 then Col := clLime                                                //
                                  else Col := clRed;                                                //
              Count := PD[Y,X].Fail;                                                                //
            end;                                                                                    //
                                                                                                    //
          DrawChip(Point(OffsX+X*SizeChipX, OffsY+Y*SizeChipY), Count, Col);                        //
        end;                                                                                        //
                                                                                                    //
      Brush.Color := clBlack;                                                                       //
      FrameRect(Rect(OffsX+1, OffsY+1, OffsX+SizeChipX*Length(PD[0]), OffsY+SizeChipY*Length(PD))); //
      FrameRect(Rect(0, 0, WBitmap.Width, WBitmap.Height));                                         //
    end;                                                                                            //
  end;                                                                                              //
                                                                                                    //
  Repaint;                                                                                          //
end;                                                                                                //
//////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////
procedure TPDDlg.DrawChip(const XY: TPoint; const Count: WORD; const cCol: TColor); //
begin                                                                               //
  with WBitmap.Canvas do                                                            //
  begin                                                                             //
    Brush.Color := cCol;                                                            //
    Pen.Color   := clBlack;                                                         //
    if cCol = clLime then Font.Color := clBlack                                     //
                     else Font.Color := clWhite;                                    //
                                                                                    //
    Rectangle(XY.X, XY.Y, XY.X+SizeChipX+1, XY.Y+SizeChipY+1);                      //
                                                                                    //
    TextOut(XY.X+3, XY.Y+2, IntToStr(Count));                                       //
  end;                                                                              //
end;                                                                                //
//////////////////////////////////////////////////////////////////////////////////////


end.

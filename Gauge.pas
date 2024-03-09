unit Gauge;

interface

uses
  Windows, Messages, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Classes;

type
  TMGauge = class(TCustomControl)
  public
    OKColor, FailColor, BackColor, TxtColor: TColor;
    Max: Single;

    constructor Create(AOwner: TWinControl);
    destructor  Destroy; override;
  private
    fText: String;
    fPosition: Single;
    fOKLength: Word;

    procedure Paint; override;
    procedure SetPosition (const Value: Single);
  published
    property Canvas;
    property Position: Single read fPosition write SetPosition;
  end;

implementation

{ TGauge }

constructor TMGauge.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);

  Parent := AOwner;

  OKColor   := clLime;
  FailColor := clRed;
  BackColor := clWhite;
  TxtColor  := clBlack;
  Position := -1;
  Max := 100;

  Canvas.Font.Size := 10;
end;

destructor TMGauge.Destroy;
begin
  //

  inherited;
end;


///////////////////////////////////////////////////////////////
{procedure TMGauge.Paint;                                     //
begin                                                        //
  inherited Paint;                                           //
                                                             //
  with Canvas do                                             //
  begin                                                      //
    if fPosition < 0 then                                    //
    begin                                                    //
      Pen.Color := clBlack;                                  //
      Brush.Color := BackColor;                              //
      Rectangle(0, 0, Width, Height);                        //
    end                                                      //
    else                                                     //
    begin                                                    //
      Pen.Color := OKColor;                                  //
      Brush.Color := OKColor;                                //
      Rectangle(0, 0, fOKLength, Height);                    //
      Pen.Color := FailColor;                                //
      Brush.Color := FailColor;                              //
      Rectangle(fOKLength, 0, Width, Height);                //
                                                             //
      Canvas.Font.Color := TxtColor;                         //
      SetBkMode(self.Canvas.Handle, 1);                      //
      Canvas.TextOut(Width div 2-20, Height div 2-8, fText); //
    end;                                                     //
  end;                                                       //
end;      }                                                   //
///////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////
procedure TMGauge.Paint;                                     //
begin                                                        //
  inherited Paint;                                           //
                                                             //
  with Canvas do                                             //
  begin                                                      //
    Pen.Color := clBlack;                                    //
    Brush.Color := BackColor;                                //
    Rectangle(0, 0, Width, Height);                          //
    if fPosition > 0 then                                    //
    begin                                                    //
      Pen.Color := OKColor;                                  //
      Brush.Color := OKColor;                                //
      Rectangle(1, 1, fOKLength-1, Height-1);                //
      if fOKLength < Width then                              //
      begin                                                  //
        Pen.Color := FailColor;                              //
        Brush.Color := FailColor;                            //
        Rectangle(fOKLength-1, 1, Width-1, Height-1);        //
      end;                                                   //
                                                             //
      Canvas.Font.Color := TxtColor;                         //
      SetBkMode(self.Canvas.Handle, 1);                      //
      Canvas.TextOut(Width div 2-20, Height div 2-8, fText); //
    end;                                                     //
  end;                                                       //
end;                                                         //
///////////////////////////////////////////////////////////////


procedure TMGauge.SetPosition(const Value: Single);
begin
  fPosition := Value;

  if Value >= 0.0 then
  begin
    fOKLength := Round(Width*Value/Max);
    fText := FormatFloat('0.00', Value)+'%';
  end;

  Repaint;
end;


end.

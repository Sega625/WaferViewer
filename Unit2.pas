unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TForm2 = class(TForm)
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    procedure FormPaint(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    fpBMP: TBitmap;
  public
    constructor Create(Sender: TObject; pBMP: TBitmap);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TForm2 }

constructor TForm2.Create(Sender: TObject; pBMP: TBitmap);
begin
  inherited Create(TComponent(Sender));

  fpBMP := pBMP;
  PaintBox1.Width  := fpBMP.Width;
  PaintBox1.Height := fpBMP.Height;
  self.ClientWidth  := PaintBox1.Width;
  self.ClientHeight := PaintBox1.Height;
end;

procedure TForm2.FormPaint(Sender: TObject);
begin
//  BitBlt(PaintBox1.Canvas.Handle, 0, 0, fpBMP.Width, fpBMP.Height, fpBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TForm2.PaintBox1Paint(Sender: TObject);
begin
  BitBlt(PaintBox1.Canvas.Handle, 0, 0, fpBMP.Width, fpBMP.Height, fpBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

end.

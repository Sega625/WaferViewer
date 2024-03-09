unit ViewBmp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TTestBmp = class(TForm)
    PBox: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WBitmap: TBitmap;

    procedure PBoxPaint(Sender: TObject);
  public
    
    procedure PreShowBmp(Bmp: TBitmap);
  end;

var
  TestBmp: TTestBmp;

implementation

{$R *.dfm}

/////////////////////////////////////////////////////////////////////////
procedure TTestBmp.FormCreate(Sender: TObject);                        //
begin                                                                  //
  PBox := TPaintBox.Create(self);                                      //
  with PBox do                                                         //
  begin                                                                //
    Parent := self;                                                    //
                                                                       //
    OnPaint := PBoxPaint;                                              //
  end;                                                                 //
  WBitmap := TBitmap.Create;                                           //
end;                                                                   //
/////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////
procedure TTestBmp.FormDestroy(Sender: TObject);                       //
begin                                                                  //
  WBitmap.Free;                                                        //
  PBox.Free;                                                           //
end;                                                                   //
/////////////////////////////////////////////////////////////////////////

procedure TTestBmp.PreShowBmp(Bmp: TBitmap);
begin
  WBitmap := Bmp;
  PBox.Width  := Bmp.Width;
  PBox.Height := Bmp.Height;
end;

procedure TTestBmp.PBoxPaint(Sender: TObject);
begin
  PBox.Canvas.Draw(0,0, WBitmap);
end;


end.

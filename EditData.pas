unit EditData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Structs, ComCtrls;

type
  TEDitDataDlg = class(TForm)
    MinEdit: TEdit;
    AcceptBtn: TBitBtn;
    SHandlerSB: TScrollBox;
    HNormaMinLab: TStaticText;
    HNormaMaxLab: TStaticText;
    NormaMinLab: TStaticText;
    NormaMaxLab: TStaticText;
    ScrollBox1: TScrollBox;
    HOKMinLab: TStaticText;
    HOKAvrLab: TStaticText;
    OKMinLab: TStaticText;
    OKAvrLab: TStaticText;
    HOKMaxLab: TStaticText;
    OKMaxLab: TStaticText;
    ScrollBox2: TScrollBox;
    HAllMinLab: TStaticText;
    HAllAvrLab: TStaticText;
    AllMinLab: TStaticText;
    AllAvrLab: TStaticText;
    HAllMaxLab: TStaticText;
    AllMaxLab: TStaticText;
    MaxEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure AcceptBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    StrMinVal, StrMaxVal: String;
  end;

var
  EDitDataDlg: TEDitDataDlg;

implementation

{$R *.dfm}

{ TEDitDataDlg }

//////////////////////////////////////////////////////
procedure TEDitDataDlg.FormCreate(Sender: TObject); //
begin                                               //
  StrMinVal := 'NE';                                //
  StrMaxVal := 'NE';                                //
end;                                                //
//////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
procedure TEDitDataDlg.AcceptBtnClick(Sender: TObject); //
begin                                                   //
  StrMinVal := MinEdit.Text;                            //
  StrMaxVal := MaxEdit.Text;                            //
  Close;                                                //
end;                                                    //
//////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
procedure TEDitDataDlg.EditKeyPress(Sender: TObject; var Key: Char);    //
begin                                                                   //
  if Key = '-' then if Pos('-', TEdit(Sender).Text) > 0 then Key := #0; //
  if Key = '-' then if TEdit(Sender).SelStart > 0 then Key := #0;       //
  if Key = '.' then if Pos('.', TEdit(Sender).Text) > 0 then Key := #0; //
  if Key = '.' then if TEdit(Sender).SelStart = 0 then Key := #0;       //
  if Key = '.' then                                                     //
    if (Pos('-', TEdit(Sender).Text) > 0) and                           //
       (TEdit(Sender).SelStart = 1) then Key := #0;                     //
                                                                        //
  if not (Key in ['0'..'9', '-', '.', #8]) then Key := #0;              //
end;                                                                    //
//////////////////////////////////////////////////////////////////////////


end.

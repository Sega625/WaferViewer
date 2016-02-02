unit Help;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Structs, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Statistica, ComCtrls, OleCtrls, SHDocVw;

type
  THelpDlg = class(TForm)
    WB: TWebBrowser;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
  end;

var
  HelpDlg: THelpDlg;

implementation

{$R *.dfm}
{$R *.RES}

{ TPDDlg }

//////////////////////////////////////////////////////////////////////////////////////
procedure THelpDlg.FormCreate(Sender: TObject);                                     //
begin                                                                               //
  WB.Navigate('res://'+Application.ExeName+'/INDEX');                               //
end;                                                                                //
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
procedure THelpDlg.FormClose(Sender: TObject; var Action: TCloseAction);            //
begin                                                                               //
//  Action := caFree;                                                                 //
end;                                                                                //
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
procedure THelpDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); //
begin                                                                               //
  if Key = VK_ESCAPE then Close;                                                    //
end;                                                                                //
//////////////////////////////////////////////////////////////////////////////////////


end.

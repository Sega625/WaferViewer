unit About;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, Structs, StdCtrls, ExtCtrls, jpeg;

type
  TAboutDlg = class(TForm)
    FileVerLab: TLabel;
    Image1: TImage;
    Label2: TLabel;
    WinVerLab: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.dfm}
  
///////////////////////////////////////////////////////
procedure TAboutDlg.FormCreate(Sender: TObject);     //
begin                                                //
  WinVerLab.Caption := GetWinName;                   //
  FileVerLab.Caption := 'WaferViewer v '+VersionStr; //
end;                                                 //
///////////////////////////////////////////////////////

//////////////////////////////////////////////////
procedure TAboutDlg.FormClick(Sender: TObject); //
begin                                           //
  Close;                                        //
end;                                            //
//////////////////////////////////////////////////


end.

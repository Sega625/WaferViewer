unit ViewPref;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Structs, Statistica, Buttons;

type
  TPrefDlg = class(TForm)
    InfoGroup: TGroupBox;
    ChipsColLab: TLabel;
    Label2: TLabel;
    ViewFlatRB: TRadioButton;
    View3D1RB: TRadioButton;
    View3D2RB: TRadioButton;
    MainGroup: TGroupBox;
    ShowAllRB: TRadioButton;
    ShowChipsRB: TRadioButton;
    BitBtn1: TBitBtn;
    BackGColLab: TLabel;
    Label3: TLabel;
    ShowGridChB: TCheckBox;
    CadreColLab: TLabel;
    Label4: TLabel;

    procedure ColLabClick(Sender: TObject);
    procedure ColumnsRBClick(Sender: TObject);
    procedure ShowRBClick(Sender: TObject);
    procedure ShowGridChBClick(Sender: TObject);
  private
    pStatistica: PStatistica;
    fParHandle: THandle;
  public
    constructor Create(Sender: TObject; pStat: PStatistica);
  end;

var
  PrefDlg: TPrefDlg;

implementation

{$R *.dfm}

{ TPrefDlg }

/////////////////////////////////////////////////////////////////////
constructor TPrefDlg.Create(Sender: TObject; pStat: PStatistica);  //
begin                                                              //
  inherited Create(TComponent(Sender));                            //
                                                                   //
  fParHandle := TWinControl(Sender).Handle;                        //
                                                                   //
  pStatistica := pStat;                                            //
                                                                   //
  BackGColLab.ControlStyle := BackGColLab.ControlStyle+[csOpaque]; //
  CadreColLab.ControlStyle := CadreColLab.ControlStyle+[csOpaque]; //
  ChipsColLab.ControlStyle := ChipsColLab.ControlStyle+[csOpaque]; //
                                                                   //
  BackGColLab.Color := pStatistica^.Color;                         //
  CadreColLab.Color := pStatistica^.clGrid;                        //
  if pStatistica^.ViewAll  then ShowAllRB.Checked   := True        //
                           else ShowChipsRB.Checked := True;       //
  if pStatistica^.ViewGrid then ShowGridChB.Checked := True        //
                           else ShowGridChB.Checked := False;      //
                                                                   //
  ChipsColLab.Color := pStatistica^.clShowChips;                   //
  case pStatistica^.ViewOK_3D of                                   //
    0: ViewFlatRB.Checked := True;                                 //
    1: View3D1RB.Checked  := True;                                 //
    2: View3D2RB.Checked  := True;                                 //
  end;                                                             //
end;                                                               //
/////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////
procedure TPrefDlg.ShowRBClick(Sender: TObject);      //
begin                                                 //
  pStatistica^.ViewAll := ShowAllRB.Checked;          //
                                                      //
  pStatistica^.Repaint;                               //
end;                                                  //
////////////////////////////////////////////////////////
////////////////////////////////////////////////////////
procedure TPrefDlg.ShowGridChBClick(Sender: TObject); //
begin                                                 //
  pStatistica^.ViewGrid := ShowGridChB.Checked;       //
                                                      //
  pStatistica^.Repaint;                               //
end;                                                  //
////////////////////////////////////////////////////////


///////////////////////////////////////////////////
procedure TPrefDlg.ColLabClick(Sender: TObject); //
var                                              //
  ColorDlg: TColorDialog;                        //
begin                                            //
  ColorDlg := TColorDialog.Create(self);         //
  with ColorDlg do                               //
  begin                                          //
    Color := TLabel(Sender).Color;               //
    if Execute then                              //
      case TLabel(Sender).Tag of                 //
        0: begin                                 //
             pStatistica^.Color := Color;        //
             clNotChip := Color;                 //
                                                 //
             pStatistica^.Repaint;               //
           end;                                  //
        1: begin                                 //
             pStatistica^.clGrid := Color;       //
                                                 //
             pStatistica^.Repaint;               //
           end;                                  //
        2: pStatistica^.clShowChips := Color;    //
      end;                                       //
                                                 //
    TLabel(Sender).Color := Color;               //
                                                 //
    Free;                                        //
  end;                                           //
end;                                             //
///////////////////////////////////////////////////

/////////////////////////////////////////////////////////
procedure TPrefDlg.ColumnsRBClick(Sender: TObject);    //
begin                                                  //
  pStatistica^.ViewOK_3D := TRadioButton(Sender).Tag;  //
                                                       //
  SendMessage(fParHandle, MESS_REPAINT_VIEW_OK, 0, 0); //
end;                                                   //
/////////////////////////////////////////////////////////


end.

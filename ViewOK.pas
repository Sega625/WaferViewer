unit ViewOK;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Structs, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Statistica, math, Printers, Unit2;

type
  TDynArray = array of Single;

  TTRIVERTEX = packed record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: WORD;
  end;

  TGroup = record
    RealVal : Single; // Для точной сортировки
    Val     : Single; // Групповая величина параметра
    Quantity: DWORD;  // Кол-во чипов в группе
    Status  : byte;   // Статус группы
    Rect    : TRect;  // Координаты прямоугольника диаграммы
    ShowID  : WORD;   // Для отображение этой группы на пластине
  end;
  TGroups = record
    MaxQuantity: WORD;
    AvrVal     : Single;
    MedVal     : Single;
    MinVal     : Single;
    MaxVal     : Single;
    Group      : array of TGroup;
  end;

  TOKDlg = class(TForm)
    ScrollBox: TScrollBox;
    PBox: TPaintBox;
    MainSB: TScrollBox;
    HTestsLab: TStaticText;
    HMinLab: TStaticText;
    HAccLab: TStaticText;
    AccCB: TComboBox;
    MinLab0: TStaticText;
    HAvrLab: TStaticText;
    AvrLab0: TStaticText;
    HMaxLab: TStaticText;
    MedLab0: TStaticText;
    HFailPLab: TStaticText;
    FailPLab: TStaticText;
    FailPLab0: TStaticText;
    TestsCB: TComboBox;
    HOKPLab: TStaticText;
    OKPLab: TStaticText;
    OKPLab0: TStaticText;
    PrnImg: TImage;
    HMedLab: TStaticText;
    MaxLab0: TStaticText;
    HNMinLab: TStaticText;
    StaticText2: TStaticText;
    NMinLab: TStaticText;
    HNMaxLab: TStaticText;
    StaticText5: TStaticText;
    NMaxLab: TStaticText;
    AvrLab: TEdit;
    MedLab: TEdit;
    MaxLab: TEdit;
    MinLab: TEdit;
    
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TestsCBChange(Sender: TObject);
    procedure AccCBChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure LabMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PrnImgClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    WBitmap, XBitmap, YBitmap: TBitmap;
    pStatistica: PStatistica;
    Groups: TGroups;
    Hint: THintWindow;
    HintRect: TRect;
    TmpLeft: Integer;
    TmpBMP: TBitmap;
    FormWidth, FormHeight: Integer;
    RBMousePressed: Boolean;
    TmpShowMode: byte;
    OKP_Enabled, FlP_Enabled: Boolean;
    hLib: THandle;
    GradientFill: function(DC: HDC; pTriVertex: Pointer; dwNumVertex: DWORD; pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWORD; stdcall; // external in "msimg32.dll"

    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove (var Message: TMessage); message WM_EXITSIZEMOVE;
//    procedure WMMove(var Message: TMessage) ; message WM_MOVE;

    procedure ProcessOK;
    procedure PreShowOK(WinHeight: WORD=0);
    procedure PrintOK;

    procedure PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBoxMouseUp  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBoxMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure PBoxPaint(Sender: TObject);

    procedure SortGroupsByValue(var Grps: TGroups);
    procedure SortGroupsByStatus(var Grps: TGroups);
    procedure SortMassByValue(var Mass: TDynArray);
    function  GetNGroup(const X, Y: Integer): Integer;
    procedure Set_Lab_To(const N: byte; const Enabled: Boolean);
    procedure GradientRect(const startColor, endColor: TColor; Bmp: TBitmap; Rect: TRect; const FillMode: byte=0);

    procedure Repaint_Event(var Mes: TMessage); message MESS_REPAINT_VIEW_OK;
  public
    constructor Create(Sender: TObject; pStat: PStatistica);
  end;

    TCntrEdit = class(TEdit)
  protected
    procedure CreateParams(var Params: TCreateParams);override;
  end;

var
  OKDlg: TOKDlg;

implementation

uses Types, StrUtils;

{$R *.dfm}

{ TPDDlg }

////////////////////////////////////////////////////////////////////////////////////////
constructor TOKDlg.Create(Sender: TObject; pStat: PStatistica);                       //
var                                                                                   //
  n, Max: Word;                                                                       //
begin                                                                                 //
  inherited Create(TComponent(Sender));                                               //
                                                                                      //
  self.Constraints.MinHeight := 350;                                                  //
                                                                                      //
  pStatistica := pStat;                                                               //
                                                                                      //
  PBox := TPaintBox.Create(self);                                                     //
  with PBox do                                                                        //
  begin                                                                               //
    Parent := ScrollBox;                                                              //
                                                                                      //
    ControlStyle := ControlStyle+[csOpaque];                                          //
                                                                                      //
    OnMouseDown := PBoxMouseDown;                                                     //
    OnMouseUP   := PBoxMouseUp;                                                       //
//    OnMouseMove := PBoxMouseMove;                                                     //
    OnPaint     := PBoxPaint;                                                         //
  end;                                                                                //
  WBitmap := TBitmap.Create;                                                          //
  XBitmap := TBitmap.Create;                                                          //
  YBitmap := TBitmap.Create;                                                          //
                                                                                      //
  Hint := THintWindow.Create(self);                                                   //
//  Hint.Canvas.Font.Name := 'Arial';                                                   //
  Hint.Canvas.Font.Height := 16;                                                      //
  Hint.Canvas.Font.Style := [fsBold];                                                 //
  Hint.ActivateHint(HintRect, '');                                                    //
  Hint.Color := $00F5DEC9;                                                            //
                                                                                      //
  Max := 0;                                                                           //
  with pStatistica^.Wafer do                                                          //
  begin                                                                               //
    for n := 0 to Length(TestsParams)-1 do                                            //
      if Length(TestsParams[n].Name) > Max then Max := Length(TestsParams[n].Name);   //
                                                                                      //
    if (8*Max-33) > TestsCB.Width then SendMessage(TestsCB.Handle, 352, 8*Max-33, 0); //
                                                                                      //
    for n := 0 to Length(TestsParams)-1 do TestsCB.Items.Add(TestsParams[n].Name);    //
    TestsCB.ItemIndex := 0;                                                           //
  end;                                                                                //
                                                                                      //
  TmpBMP := TBitmap.Create;                                                           //
  RBMousePressed := False;                                                            //
  TmpShowMode := 0;                                                                   //
                                                                                      //
  Set_Lab_To(1, True);                                                                //
  Set_Lab_To(2, True);                                                                //
                                                                                      //
  hLib := 0;                                                                          //
  hLib := LoadLibrary('MSIMG32.DLL');                                                 //
  if hLib <> 0 then @GradientFill := GetProcAddress(hLib, 'GradientFill');            //
                                                                                      //
  pPointer(MinLab)^ := TCntrEdit;                                                     //
  TCntrEdit(MinLab).RecreateWnd;                                                      //
  pPointer(AvrLab)^ := TCntrEdit;                                                     //
  TCntrEdit(AvrLab).RecreateWnd;                                                      //
  pPointer(MedLab)^ := TCntrEdit;                                                     //
  TCntrEdit(MedLab).RecreateWnd;                                                      //
  pPointer(MaxLab)^ := TCntrEdit;                                                     //
  TCntrEdit(MaxLab).RecreateWnd;                                                      //
                                                                                      //
  MinLab.DoubleBuffered := True;                                                      //
  AvrLab.DoubleBuffered := True;                                                      //
  MedLab.DoubleBuffered := True;                                                      //
  MaxLab.DoubleBuffered := True;                                                      //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.FormShow(Sender: TObject);                                           //
begin                                                                                 //
  TestsCBChange(self);                                                                //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.FormClose(Sender: TObject; var Action: TCloseAction);                //
begin                                                                                 //
//  Action := caFree;                                                                   //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.FormDestroy(Sender: TObject);                                        //
begin                                                                                 //
  Hint.ReleaseHandle;                                                                 //
  Hint.Free;                                                                          //
                                                                                      //
  TmpBMP.Free;                                                                        //
  WBitmap.Free;                                                                       //
  XBitmap.Free;                                                                       //
  YBitmap.Free;                                                                       //
  PBox.Free;                                                                          //
                                                                                      //
  pStatistica := nil;                                                                 //
                                                                                      //
  if hLib <> 0 then FreeLibrary(hLib);                                                //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////
procedure TOKDlg.FormPaint(Sender: TObject); //
begin                                        //
  Canvas.Draw(0,0, YBitmap);                 //
end;                                         //
///////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); //
begin                                                                                                                              //
  with ScrollBox.HorzScrollBar do // Крутим колесо                                                                                 //
    if WheelDelta > 0 then Position := ScrollPos-20                                                                                //
                      else Position := ScrollPos+20;                                                                               //
end;                                                                                                                               //
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
procedure TOKDlg.WMEnterSizeMove(var Message: TMessage);      //
begin                                                         //
  FormWidth  := self.Width;                                   //
  FormHeight := self.Height;                                  //
end;                                                          //
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
procedure TOKDlg.WMExitSizeMove(var Message: TMessage);       //
begin                                                         //
  if self.Width <> FormWidth then // изменение разм-ра по X   //
  begin                                                       //
//    PreShowOK(self.Height);                                   //
//    ShowMessage('Resize on X');
  end;                                                        //
                                                              //
  if self.Height <> FormHeight then // изменение разм-ра по Y //
  begin                                                       //
    PreShowOK(self.Height);                                   //
//    ShowMessage('Resize on Y');
  end;                                                        //
end;                                                          //
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.PBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);                 //
var                                                                                                                       //
  Ngr: Integer;                                                                                                           //
  S: String[5];                                                                                                           //
  Str: String;                                                                                                            //
  dY: Integer;                                                                                                            //
begin                                                                                                                     //
  if Length(Groups.Group) = 0 then Exit;                                                                                  //
                                                                                                                          //
  Ngr := GetNGroup(X, Y);                                                                                                 //
  if Ngr = -1 then Exit;                                                                                                  //
                                                                                                                          //
  if Button = mbRight then                                                                                                //
  begin                                                                                                                   //
    with Groups.Group[Ngr] do                                                                                             //
    begin                                                                                                                 //
      TmpBMP.Width  := Rect.Right-Rect.Left;                                                                              //
      TmpBMP.Height := PBox.Height;                                                                                       //
      TmpLeft := Rect.Left;                                                                                               //
                                                                                                                          //
      BitBlt(TmpBMP.Canvas.Handle, 0, 0, TmpBMP.Width, TmpBMP.Height, WBitmap.Canvas.Handle, TmpLeft, PBox.Top, SRCCOPY); //
                                                                                                                          //
      WBitmap.Canvas.Pen.Color := $00660066;                                                                              //
      WBitmap.Canvas.Brush.Color := clFuchsia;                                                                            //
                                                                                                                          //
      if hLib <> 0 then                                                                                                   //
      begin                                                                                                               //
        GradientRect($00660066, clFuchsia, WBitmap, Types.Rect(TmpLeft, PBox.Top, TmpLeft+TmpBMP.Width, PBox.Height));    //
        dY := TmpBMP.Width div 10;                                                                                        //
                                                                                                                          //
        if pStatistica^.ViewOK_3D = 2 then                                                                                //
          WBitmap.Canvas.Ellipse(TmpLeft, PBox.Top-dY-1, TmpLeft+TmpBMP.Width, PBox.Top+dY);                              //
      end                                                                                                                 //
      else WBitmap.Canvas.Rectangle(TmpLeft, PBox.Top, TmpLeft+TmpBMP.Width, PBox.Height);                                //
                                                                                                                          //
      PBox.Repaint;                                                                                                       //
                                                                                                                          //
      pStatistica^.ShowGroup := ShowID; // Зададим группу, кристаллы которой отображаем                                   //
    end;                                                                                                                  //
    TmpShowMode := pStatistica^.LastShowMode; // Запомним предыдущий режим отображения пластины                           //
                                                                                                                          //
    RBMousePressed := True;                                                                                               //
                                                                                                                          //
    pStatistica^.Repaint(2); // Отобразим кристаллы заданной группы                                                       //
  end;                                                                                                                    //
                                                                                                                          //
  case AccCB.ItemIndex of                                                                                                 //
    0    : S := '0.000';                                                                                                  //
    1    : S := '0.00';                                                                                                   //
    2    : S := '0.0';                                                                                                    //
    3,4,5: S := '0';                                                                                                      //
  end;                                                                                                                    //
  with Groups.Group[Ngr] do                                                                                               //
    Str := ' Значение: '+FormatFloat(S, Val)+#13+' Кол-во: '+IntToStr(Quantity);                                          //
                                                                                                                          //
  HintRect.Left  := X+self.Left-ScrollBox.HorzScrollBar.ScrollPos;                                                        //
  HintRect.Right := HintRect.Left+Hint.Canvas.TextWidth(Str)-Hint.Canvas.TextWidth(' Кол-во: ');                          //
  HintRect.Top    := Y+self.Top;                                                                                          //
  HintRect.Bottom := HintRect.Top+2*(Hint.Canvas.TextHeight(Str)+2);                                                      //
                                                                                                                          //
  Hint.ActivateHint(HintRect, Str);                                                                                       //
end;                                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.PBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);                   //
begin                                                                                                                     //
  if Length(Groups.Group) = 0 then Exit;                                                                                  //
                                                                                                                          //
  Hint.ReleaseHandle;                                                                                                     //
                                                                                                                          //
  if RBMousePressed then                                                                                                  //
  begin                                                                                                                   //
    BitBlt(WBitmap.Canvas.Handle, TmpLeft, PBox.Top, TmpBMP.Width, TmpBMP.Height, TmpBMP.Canvas.Handle, 0, 0, SRCCOPY);   //
                                                                                                                          //
    PBox.Repaint;                                                                                                         //
                                                                                                                          //
    RBMousePressed := False;                                                                                              //
                                                                                                                          //
    pStatistica^.Repaint(TmpShowMode); // Восстановим предыдущий режим отображения пластины                               //
  end;                                                                                                                    //
end;                                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.PBoxMouseLeave(var Message: TMessage);                                                                   //
begin                                                                                                                     //
  if Length(Groups.Group) = 0 then Exit;                                                                                  //
                                                                                                                          //
  Hint.ReleaseHandle;                                                                                                     //
                                                                                                                          //
  if RBMousePressed then                                                                                                  //
  begin                                                                                                                   //
    BitBlt(WBitmap.Canvas.Handle, TmpLeft, PBox.Top, TmpBMP.Width, TmpBMP.Height, TmpBMP.Canvas.Handle, 0, 0, SRCCOPY);   //
                                                                                                                          //
    PBox.Repaint;                                                                                                         //
                                                                                                                          //
    RBMousePressed := False;                                                                                              //
                                                                                                                          //
    pStatistica^.Repaint(TmpShowMode); // Восстановим предыдущий режим отображения пластины                               //
  end;                                                                                                                    //
end;                                                                                                                      //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////
procedure TOKDlg.PBoxPaint(Sender: TObject);     //
begin                                            //
  PBox.Canvas.Draw(0,0, WBitmap);                //
  PBox.Canvas.Draw(0,WBitmap.Height+1, XBitmap); //
end;                                             //
///////////////////////////////////////////////////

///////////////////////////////////////////////////
procedure TOKDlg.TestsCBChange(Sender: TObject); //
begin                                            //
  ProcessOK;                                     //
  PreShowOK(self.Height);                        //
  ScrollBox.SetFocus;                            //
end;                                             //
///////////////////////////////////////////////////
///////////////////////////////////////////////////
procedure TOKDlg.AccCBChange(Sender: TObject);   //
begin                                            //
  ProcessOK;                                     //
  PreShowOK(self.Height);                        //
  ScrollBox.SetFocus;                            //
end;                                             //
///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.LabMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); //
begin                                                                                                    //
  case TStaticText(Sender).Tag of                                                                        //
    1: Set_Lab_To(1, not OKP_Enabled); // Годные по этому параметру кристаллы                            //
    2: Set_Lab_To(2, not FlP_Enabled); // Бракованные по этому параметру кристаллы                       //
  end;                                                                                                   //
                                                                                                         //
  ProcessOK;                                                                                             //
  PreShowOK(self.Height);                                                                                //
end;                                                                                                     //
///////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////
procedure TOKDlg.PrnImgClick(Sender: TObject); //
var                                            //
  PrintDlg: TPrintDialog;                      //
begin                                          //
  PrintDlg := TPrintDialog.Create(self);       //
  with PrintDlg do                             //
  begin                                        //
    if Execute then PrintOK;                   //
                                               //
    Free;                                      //
  end;                                         //
end;                                           //
/////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.ProcessOK;                                                                 //
var                                                                                         //
  X, Y, NGr: WORD;                                                                          //
  n, Count, NOKch, NFail: DWORD;                                                            //
  FirstTime: Boolean;                                                                       //
  Res, NDig: byte;                                                                          //
  TmpVal: Extended;                                                                         //
  TmpSingle, rTmpSingle: Single;                                                            //
  OldStyle : TBrushStyle;                                                                   //
  MedMass: TDynArray;                                                                       //
begin                                                                                       //
  NDig := AccCB.ItemIndex-3;                                                                //
                                                                                            //
  with PBox.Canvas do                                                                       //
  begin                                                                                     //
    Font.Color := clPurple;                                                                 //
    OldStyle := Brush.Style;                                                                //
    Brush.Style := bsClear;                                                                 //
    Font.Size := 24;                                                                        //
    Font.Style := [fsItalic];                                                               //
    TextOut(100+ScrollBox.HorzScrollBar.ScrollPos, (PBox.Height div 2)-24, 'Обработка...'); //
    Brush.Style := OldStyle;                                                                //
  end;                                                                                      //
                                                                                            //
  FirstTime := True;                                                                        //
  Count := 0;                                                                               //
  TmpVal := 0.0;                                                                            //
  NGr := 0;                                                                                 //
                                                                                            //
  SetLength(MedMass, 50);                                                                   //
  Groups.MaxQuantity := 0;                                                                  //
  SetLength(Groups.Group, 0);                                                               //
  SetLength(Groups.Group, 50);                                                              //
                                                                                            //
  NOKch  := 0;                                                                              //
  NFail  := 0;                                                                              //
  with pStatistica^.Wafer, Groups do                                                        //
  begin                                                                                     //
    for Y := 0 to Length(Chip)-1 do                                                         //
      for X := 0 to Length(Chip[0])-1 do                                                    //
      begin                                                                                 //
        if Chip[Y, X].Status in [0,2,3,4,5,7] then Continue;                                //
                                                                                            //
        try                                                                                 //
          Res := Chip[Y, X].ChipParams[TestsCB.ItemIndex].Stat;                             //
        except                                                                              //
          Res := 0;                                                                         //
        end;                                                                                //
                                                                                            //
        if Res = 3 then Res := 2;                                                           //
        if Res = 2 then Inc(NFail);                                                         //
        if Res = 1 then Inc(NOKch);                                                         //
                                                                                            //
        Chip[Y, X].ShowGr := 0;                                                             //
                                                                                            //
        if ((OKP_Enabled) and (Res = 1)) or                                                 //
           ((FlP_Enabled) and (Res = 2)) then                                               //
        begin                                                                               //
          rTmpSingle := Chip[Y, X].ChipParams[TestsCB.ItemIndex].Value;                     //
          try                                                                               //
            TmpSingle := RoundTo(rTmpSingle, NDig);                                         //
          except                                                                            //
            TmpSingle := rTmpSingle;                                                        //
          end;                                                                              //
                                                                                            //
          if Count = Length(MedMass) then SetLength(MedMass, Length(MedMass)+50);           //
          MedMass[Count] := rTmpSingle;                                                     //
                                                                                            //
          if FirstTime then                                                                 //
          begin                                                                             //
            Group[0].RealVal := rTmpSingle;                                                 //
            Group[0].Val := TmpSingle;                                                      //
            Group[0].Quantity := 1;                                                         //
            Group[0].Status := Res;                                                         //
            Group[0].ShowID := 1;                                                           //
            Chip[Y, X].ShowGr := 1;                                                         //
                                                                                            //
            NGr := 1;                                                                       //
            MaxQuantity := 1;                                                               //
            MinVal := rTmpSingle;                                                           //
            MaxVal := rTmpSingle;                                                           //
            TmpVal := rTmpSingle;                                                           //
            Inc(Count);                                                                     //
                                                                                            //
            FirstTime := False;                                                             //
          end                                                                               //
          else                                                                              //
          begin                                                                             //
            if rTmpSingle < MinVal then MinVal := rTmpSingle;                               //
            if rTmpSingle > MaxVal then MaxVal := rTmpSingle;                               //
            TmpVal := TmpVal+rTmpSingle;                                                    //
            Inc(Count);                                                                     //
                                                                                            //
            for n := 0 to Ngr-1 do                                                          //
              if Group[n].Val = TmpSingle then                                              //
              begin                                                                         //
                if Group[n].Status <> Res then Continue; // Разделим брак и годные          //
                                                                                            //
                Inc(Group[n].Quantity);                                                     //
                Group[n].Status := Res;                                                     //
                Group[n].ShowID := n+1;                                                     //
                Chip[Y, X].ShowGr := n+1;                                                   //
                if Group[n].Quantity > MaxQuantity then MaxQuantity := Group[n].Quantity;   //
                Break;                                                                      //
              end;                                                                          //
                                                                                            //
            if n = NGr then // Новая группа                                                 //
            begin                                                                           //
              Inc(NGr);                                                                     //
              if Length(Group) < NGr then SetLength(Group, Length(Group)+50);               //
                                                                                            //
              Group[n].RealVal := rTmpSingle;                                               //
              Group[n].Val := TmpSingle;                                                    //
              Group[n].Quantity := 1;                                                       //
              Group[n].Status := Res;                                                       //
              Group[n].ShowID := n+1;                                                       //
              Chip[Y, X].ShowGr := n+1;                                                     //
            end;                                                                            //
          end;                                                                              //
        end;                                                                                //
      end;                                                                                  //
                                                                                            //
    SetLength(Group, NGr);                                                                  //
    SetLength(MedMass, Count);                                                              //
                                                                                            //
    if Count <> 0 then                                                                      //
    begin                                                                                   //
      AvrVal := TmpVal/Count;                                                               //
      SortMassByValue(MedMass);                                                             //
      if Odd(Count) then MedVal :=  MedMass[ Count div 2]                                   //
                    else MedVal := (MedMass[(Count div 2)-1]+MedMass[Count div 2])/2;       //
    end                                                                                     //
    else                                                                                    //
    begin                                                                                   //
      AvrVal :=  0.0;                                                                       //
      MedVal :=  0.0;                                                                       //
    end;                                                                                    //
                                                                                            //
    DecimalSeparator := ',';                                                                //
                                                                                            //
    TmpSingle := TestsParams[TestsCB.ItemIndex].Norma.Min;                                  //
    if TmpSingle = -NotSpec then NMinLab.Caption := 'нет'                                   //
                            else NMinLab.Caption := FormatFloat('0.000', TmpSingle);        //
    TmpSingle := TestsParams[TestsCB.ItemIndex].Norma.Max;                                  //
    if TmpSingle = NotSpec  then NMaxLab.Caption := 'нет'                                   //
                            else NMaxLab.Caption := FormatFloat('0.000', TmpSingle);        //
    MinLab.Text := FormatFloat('0.000', MinVal);                                            //
    AvrLab.Text := FormatFloat('0.000', AvrVal);                                            //
    MedLab.Text := FormatFloat('0.000', MedVal);                                            //
    MaxLab.Text := FormatFloat('0.000', MaxVal);                                            //
    OKPLab.Caption   := IntToStr(NOKch);                                                    //
    FailPLab.Caption := IntToStr(NFail);                                                    //
                                                                                            //
    DecimalSeparator := '.';                                                                //
  end;                                                                                      //
                                                                                            //
  SortGroupsByValue(Groups);                                                                //
  SortGroupsByStatus(Groups);                                                               //
end;                                                                                        //
//////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.PreShowOK(WinHeight: WORD=0);                                        //
var                                                                                   //
  n, X, Y, dX, dY: Integer;                                                           //
  mX, mY: Double;                                                                     //
  Str, TmpString: String;                                                             //
  Hfont: THandle;                                                                     //
  LogFont: TLogFont;                                                                  //
  Col1, Col2: TColor;                                                                 //
  OldStyle: TBrushStyle;                                                              //
  HandleHeight: byte;                                                                 //
begin                                                                                 //
  dX := 30;                                                                           //
  dY := 5;                                                                            //
  X := 5;                                                                             //
  Y := 5;                                                                             //
                                                                                      //
  if Screen.PixelsPerInch = 96 then HandleHeight := 95                                //
                               else HandleHeight := 102;                              //
                                                                                      //
  if WinHeight = 0 then WinHeight := pStatistica^.Height;                             //
                                                                                      //
  self.Height := WinHeight;                                                           //
                                                                                      //
  ScrollBox.Left := dX;                                                               //
  ScrollBox.Top  := dY;                                                               //
  ScrollBox.Height := WinHeight-HandleHeight-deltaW7;                                 //
                                                                                      //
  PBox.Width := MainSB.Width-dX+X-1;                                                  //
  if PBox.Width < Length(Groups.Group)*12 then PBox.Width := Length(Groups.Group)*12; //
  if PBox.Width < Screen.Width-100 then ScrollBox.Width := PBox.Width                 //
                                   else ScrollBox.Width := Screen.Width-100;          //
  PBox.Top  := 0;                                                                     //
  PBox.Left := 0;                                                                     //
  PBox.Height := ScrollBox.Height-19;                                                 //
                                                                                      //
  WBitmap.Width  := PBox.Width;                                                       //
  WBitmap.Height := PBox.Height-(dX+10);                                              //
  XBitmap.Width  := WBitmap.Width;                                                    //
  XBitmap.Height := dX+10;                                                            //
  YBitmap.Width  := dX;                                                               //
  YBitmap.Height := ScrollBox.Top+WBitmap.Height;                                     //
                                                                                      //
  MainSB.Top := dY+ScrollBox.Height+Y-1;                                              //
  PrnImg.Top := MainSB.Top-20;                                                        //
                                                                                      //
  self.Constraints.MinWidth := 0;                                                     //
  self.Constraints.MaxWidth := 0;                                                     //
  self.Width := dX+ScrollBox.Width+X+11+deltaW7;                                      //
  self.ClientWidth := dX+ScrollBox.Width+X;                                           //
  self.Constraints.MinWidth := self.Width;                                            //
  self.Constraints.MaxWidth := self.Width;                                            //
                                                                                      //
//////////////////////////////////////////////////////////////                        //
                                                                                      //
  if Length(Groups.Group) = 0 then // Если нечего отображать                          //
  begin                                                                               //
    with WBitmap.Canvas do                                                            //
    begin                                                                             //
      Pen.Color := clBlack;                                                           //
      Brush.Color := clSilver;                                                        //
      Rectangle(0, 0, WBitmap.Width, WBitmap.Height);                                 //
                                                                                      //
      Font.Color := clRed;                                                            //
      OldStyle := Brush.Style;                                                        //
      Brush.Style := bsClear;                                                         //
      Font.Size := 24;                                                                //
      Font.Style := [fsItalic];                                                       //
      TextOut(65, WBitmap.Height div 2-24, 'Нет объектов для отображения');           //
      Brush.Style := OldStyle;                                                        //
    end;                                                                              //
    with XBitmap.Canvas do                                                            //
    begin                                                                             //
      Pen.Color := self.Color;                                                        //
      Brush.Color := self.Color;                                                      //
      Rectangle(0, 0, XBitmap.Width, XBitmap.Height);                                 //
    end;                                                                              //
    with YBitmap.Canvas do                                                            //
    begin                                                                             //
      Pen.Color := self.Color;                                                        //
      Brush.Color := self.Color;                                                      //
      Rectangle(0, 0, YBitmap.Width, YBitmap.Height);                                 //
    end;                                                                              //
                                                                                      //
    PBox.Repaint;                                                                     //
    self.Repaint;                                                                     //
                                                                                      //
    Exit;                                                                             //
  end;                                                                                //
                                                                                      //
//////////////////////////////////////////////////////////////                        //
                                                                                      //
  if (self.Left+self.Width) > Screen.Width then // Вылезает за экран                  //
    self.Left := (Screen.Width-self.Width ) div 2;                                    //
                                                                                      //
//////////////////////////////////////////////////////////////                        //
                                                                                      //
  with Logfont do                           //                                        //
  begin                                     //                                        //
    lfFaceName      := 'Tahoma';            //                                        //
    lfUnderline     := 0;                   //                                        //
    lfStrikeOut     := 0;                   //                                        //
    lfItalic        := 0;                   //                                        //
    lfWidth         := 4;                   //                                        //
    lfHeight        := 11;                  //                                        //
    lfEscapement    := 900;                 // Для поворота фонта на 90гр.            //
    lfcharset       := RUSSIAN_CHARSET;     //                                        //
    lfOutPrecision  := OUT_TT_ONLY_PRECIS;  //                                        //
    lfClipPrecision := CLIP_DEFAULT_PRECIS; //                                        //
    lfQuality       := PROOF_QUALITY;       //                                        //
    lfWeight := 400; // жирный = 700        //                                        //
  end;                                      //                                        //
  HFont := CreateFontIndirect(LogFont);     //                                        //
                                                                                      //
//////////////////////////////////////////////////////////////                        //
                                                                                      //
  with XBitmap.Canvas do                            //                                //
  begin                                             //                                //
    Pen.Color := self.Color;                        //                                //
    Brush.Color := self.Color;                      // Для                            //
    Rectangle(0, 0, XBitmap.Width, XBitmap.Height); // шкалы                          //
    Pen.Color := clBlack;                           // по X                           //
    Font.Height := 11;                              //                                //
  end;                                              //                                //
                                                                                      //
  with YBitmap.Canvas do                            //                                //
  begin                                             //                                //
    Pen.Color := self.Color;                        //                                //
    Brush.Color := self.Color;                      // Для                            //
    Rectangle(0, 0, YBitmap.Width, YBitmap.Height); // шкалы                          //
    Pen.Color := clBlack;                           // по Y                           //
    Font.Height := 11;                              //                                //
    Font.Name := 'Tahoma';                          //                                //
  end;                                              //                                //
                                                                                      //
  mX := (WBitmap.Width-1)/Length(Groups.Group);                                       //
  mY := (WBitmap.Height)/(Groups.MaxQuantity);                                        //
                                                                                      //
  with WBitmap.Canvas do                            //                                //
  begin                                             //                                //
    Pen.Color := clSilver;                          // Фон                            //
    Brush.Color := $0080FFFF;                       //                                //
    Rectangle(0, 0, WBitmap.Width, WBitmap.Height); //                                //
                                                                                      //
///////////////////////////////////////////////////////////////                       //
                                                                                      //
    case AccCB.ItemIndex of                                                           //
      0    : Str := '0.000';                                                          //
      1    : Str := '0.00';                                                           //
      2    : Str := '0.0';                                                            //
      3,4,5: Str := '0';                                                              //
    end;                                                                              //
                                                                                      //
    for n := 0 to Length(Groups.Group)-1 do                                           //
      with Groups.Group[n] do                                                         //
      begin                                                                           //
        Pen.Color := clSilver;                                                        //
                                                                                      //
        dX := Round(n*mX+mX);                                                         //
        MoveTo(dX, WBitmap.Height);                                                   //
        LineTo(dX, 0);                                                                //
                                                                                      //
        Rect.Left   := Round(n*mX);                                                   //
        Rect.Top    := WBitmap.Height-Round(mY*Quantity);                             //
        Rect.Right  := dX+1;                                                          //
        Rect.Bottom := WBitmap.Height;                                                //
                                                                                      //
        if Status = 2 then                                                            //
        begin                                                                         //
          Pen.Color := clSilver;                                                      //
          Brush.Color := $00D5D5FF;                                                   //
          Rectangle(Rect.Left, 0, Rect.Right, WBitmap.Height);                        //
        end;                                                                          //
                                                                                      //
        with XBitmap.Canvas do                                                        //
        begin                                                                         //
          if n = 0 then X := ((Rect.Right-Rect.Left-TextHeight('W')) div 2)-1;        //
                                                                                      //
          TmpString := FormatFloat(Str, Val);                                         //
          Selectobject(Handle, HFont); // Повернем фонт                               //
          TextOut(X+Rect.Left, TextWidth(TmpString)+1, TmpString);                    //
        end;                                                                          //
      end;                                                                            //
                                                                                      //
///////////// * Шкала и разметка по Y * //////////////////////                        //
                                                                                      //
    Pen.Color := clSilver;                                                            //
    Y := 0;                                                                           //
    n := Round(20/mY);                                                                //
    if n = 0 then n := 1;                                                             //
    while Y <= Groups.MaxQuantity do                                                  //
    begin                                                                             //
      if Y+n > Groups.MaxQuantity then Y := Groups.MaxQuantity;                       //
      if Y = 0 then YBitmap.Canvas.TextOut(22, WBitmap.Height-4, '0')                 //
      else                                                                            //
      begin                                                                           //
        TmpString := IntToStr(Y);                                                     //
        dX := YBitmap.Width-YBitmap.Canvas.TextWidth(TmpString)-2;                    //
        dY := WBitmap.Height-Round(Y*mY);                                             //
                                                                                      //
        YBitmap.Canvas.TextOut(dX, dY, TmpString);                                    //
        MoveTo(1, dY);                                                                //
        LineTo(WBitmap.Width-1, dY);                                                  //
      end;                                                                            //
                                                                                      //
      Inc(Y, n);                                                                      //
    end;                                                                              //
                                                                                      //
///////////////////// * Диаграмма * ///////////////////////////                       //
                                                                                      //
    dY := (Groups.Group[0].Rect.Right-Groups.Group[0].Rect.Left) div 10;              //
    for n := 0 to Length(Groups.Group)-1 do                                           //
      with Groups.Group[n] do                                                         //
      begin                                                                           //
        case Status of                                                                //
          1: begin Col1 := clGreen;  Col2 := clLime; end;                             //
          2: begin Col1 := clMaroon; Col2 := clRed;  end;                             //
        end;                                                                          //
                                                                                      //
        Pen.Color   := Col1;                                                          //
        Brush.Color := Col2;                                                          //
                                                                                      //
        if pStatistica^.ViewOK_3D in [1,2] then                                       //
          if hLib <> 0 then                                                           //
          begin                                                                       //
            GradientRect(Col1, Col2, WBitmap, Rect);                                  //
            if pStatistica^.ViewOK_3D = 2 then                                        //
              Ellipse(Rect.Left, Rect.Top-dY-1, Rect.Right, Rect.Top+dY);             //
          end                                                                         //
          else Rectangle(Rect)                                                        //
        else Rectangle(Rect);                                                         //
      end;                                                                            //
  end;                                                                                //
                                                                                      //
///////////////////////////////////////////////////////////////                       //
                                                                                      //
  Deleteobject(HFont);                                                                //
                                                                                      //
  PBox.Repaint;                                                                       //
  self.Repaint;                                                                       //
end;                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.PrintOK;                                                                                               //
var                                                                                                                     //
  Str, TmpString: String;                                                                                               //
  Mode: byte;                                                                                                           //
  n, Y, Count, MaxCount: WORD;                                                                                          //
  dX, dY, dYY: Integer;                                                                                                 //
  mX, mY: Double;                                                                                                       //
  TmpRect: TRect;                                                                                                       //
  tf: TFont;                                                                                                            //
  XRect, YRect, WRect: TRect;                                                                                           //
                                                                                                                        //
  Hfont: THandle;                                                                                                       //
  LogFont: TLogFont;                                                                                                    //
  tmpFont: TFont;                                                                                                       //
  TmpSingle: Single;                                                                                                    //
begin                                                                                                                   //
  if Length(Groups.Group) = 0 then Exit;                                                                                //
                                                                                                                        //
  Mode := 0;                                                                                                            //
  if OKP_Enabled then Mode := Mode+2;                                                                                   //
  if FlP_Enabled then Mode := Mode+4;                                                                                   //
                                                                                                                        //
  Str := 'Распределение ';                                                                                              //
  case Mode of                                                                                                          //
    0: Exit;                                                                                                            //
    1: Str := Str+'всех годных кристаллов. Параметр: '+TestsCB.Items[TestsCB.ItemIndex];                                //
    2: Str := Str+'годных только по параметру "'+TestsCB.Items[TestsCB.ItemIndex]+'" кристаллов.';                      //
    3: Exit;                                                                                                            //
    4: Str := Str+'бракованных только по параметру "'+TestsCB.Items[TestsCB.ItemIndex]+'" кристаллов.';                 //
    5: Str := Str+'всех годных и бракованных только по параметру "'+TestsCB.Items[TestsCB.ItemIndex]+'" кристаллов.';   //
    6: Str := Str+'годных и бракованных только по параметру "'+TestsCB.Items[TestsCB.ItemIndex]+'" кристаллов.';        //
    7: Exit;                                                                                                            //
  end;                                                                                                                  //
                                                                                                                        //
  Printer.Orientation := poLandscape;                                                                                   //
                                                                                                                        //
  YRect.Left := 0;                                                                                                      //
  YRect.Top  := 2*Printer.Canvas.TextHeight('W');                                                                       //
  YRect.Right  := Round(Printer.PageWidth*0.03);                                                                        //
  YRect.Bottom := Round(Printer.PageHeight*0.8)+YRect.Top;                                                              //
                                                                                                                        //
  XRect.Left := YRect.Right+1;                                                                                          //
  XRect.Top  := YRect.Bottom-Round(1.5*YRect.Right);                                                                    //
  XRect.Right  := Printer.PageWidth-1;                                                                                  //
  XRect.Bottom := YRect.Bottom;                                                                                         //
                                                                                                                        //
  WRect.Left := XRect.Left;                                                                                             //
  WRect.Top  := YRect.Top;                                                                                              //
  WRect.Right  := XRect.Right;                                                                                          //
  WRect.Bottom := XRect.Top-1;                                                                                          //
                                                                                                                        //
  mX := (WRect.Right-WRect.Left-1)/Length(Groups.Group);                                                                //
  mY := (WRect.Bottom-WRect.Top)/Groups.MaxQuantity;                                                                    //
                                                                                                                        //
  with Logfont do                           //                                                                          //
  begin                                     //                                                                          //
    lfFaceName      := 'Courier New';       //                                                                          //
    lfUnderline     := 0;                   //                                                                          //
    lfStrikeOut     := 0;                   //                                                                          //
    lfItalic        := 0;                   //                                                                          //
    lfHeight        := 60;                  //                                                                          //
    lfEscapement    := 900;                 // Повернем фонт на 90гр.                                                   //
    lfcharset       := RUSSIAN_CHARSET;     //                                                                          //
    lfOutPrecision  := OUT_TT_ONLY_PRECIS;  //                                                                          //
    lfClipPrecision := CLIP_DEFAULT_PRECIS; //                                                                          //
    lfQuality       := PROOF_QUALITY;       //                                                                          //
//    lfWeight := 400; // жирный - 700      //                                                                          //
  end;                                      //                                                                          //
  HFont := CreateFontIndirect(LogFont);     //                                                                          //
                                                                                                                        //
  Printer.PrinterIndex := Printer.PrinterIndex;                                                                         //
                                                                                                                        //
  Printer.BeginDoc;                                                                                                     //
                                                                                                                        //
  with Printer.Canvas do                                                                                                //
  begin                                                                                                                 //
    Font.Name := 'Tahoma';                                                                                              //
    Font.Color := clBlack;                                                                                              //
    Brush.Color := clWhite;                                                                                             //
    Font.Size := 12;                                                                                                    //
    Font.Style := [fsBold];                                                                                             //
    TextOut(TextWidth('WWWWWWWWW'), 0, Str);                                                                            //
                                                                                                                        //
    Font.Name := 'Courier New';                                                                                         //
    Font.Color := clBlack;                                                                                              //
    Font.Size := 6;                                                                                                     //
    Font.Style := [];                                                                                                   //
                                                                                                                        //
    tmpFont := Font; // Запомним старый фонт                                                                            //
    Selectobject(Handle, HFont); // Установим повернутый фонт                                                           //
                                                                                                                        //
//////////////////////////// Рисунок + X                                                                                //
                                                                                                                        //
    Count := 1;                                                                                                         //
    for n := 0 to Length(Groups.Group)-1 do                                                                             //
      with Groups.Group[n] do                                                                                           //
      begin                                                                                                             //
        case Status of                                                                                                  //
          1: Brush.Color := $00F5F5F5;                                                                                  //
          2: Brush.Color := clGray;                                                                                     //
          3: Brush.Color := clGray;                                                                                     //
        end;                                                                                                            //
                                                                                                                        //
        TmpRect.Left := Round(n*mX)+WRect.Left;                                                                         //
        TmpRect.Top  := (WRect.Bottom-Round(mY*Quantity));                                                              //
        TmpRect.Right  := Round(n*mX+mX+1)+WRect.Left;                                                                  //
        TmpRect.Bottom := WRect.Bottom;                                                                                 //
        Rectangle(TmpRect);                                                                                             //
                                                                                                                        //
        case AccCB.ItemIndex of                                                                                         //
          0    : Str := '0.000';                                                                                        //
          1    : Str := '0.00';                                                                                         //
          2    : Str := '0.0';                                                                                          //
          3,4,5: Str := '0';                                                                                            //
        end;                                                                                                            //
                                                                                                                        //
        if n = 0 then                                                                                                   //
        begin                                                                                                           //
          dX := ((TmpRect.Right-TmpRect.Left)-TextHeight('W')) div 2;                                                   //
          MaxCount := Trunc(TextHeight('W')/(TmpRect.Right-TmpRect.Left)); ////// !!!!!                                 //
          if MaxCount < 1 then MaxCount := 1;                                                                           //
        end;                                                                                                            //
                                                                                                                        //
        if Count < MaxCount then Inc(Count)                                                                             //
        else                                                                                                            //
        begin                                                                                                           //
          Brush.Color := clWhite;                                                                                       //
          TmpString := FormatFloat(Str, Val);                                                                           //
          dY := (XRect.Bottom-XRect.Top)-TextWidth(TmpString+'W');                                                      //
          TextOut(dX+TmpRect.Left, XRect.Bottom-dY, TmpString);                                                         //
          Count := 1;                                                                                                   //
        end;                                                                                                            //
      end;                                                                                                              //
                                                                                                                        //
//////////////////////////// Y + линии на рисунке                                                                       //
                                                                                                                        //
      Selectobject(Handle, tmpFont.Handle); // Вернем старый фонт                                                       //
                                                                                                                        //
      Brush.Color := clWhite;                                                                                           //
                                                                                                                        //
      dY := TextHeight('W');                                                                                            //
      Y := 0;                                                                                                           //
      n := Round(3*dY/mY);                                                                                              //
      if n = 0 then n := 1;                                                                                             //
      while Y <= Groups.MaxQuantity do                                                                                  //
      begin                                                                                                             //
        if Y+n > Groups.MaxQuantity then Y := Groups.MaxQuantity;                                                       //
                                                                                                                        //
        dYY := WRect.Bottom-Round(Y*mY);                                                                                //
                                                                                                                        //
        TmpRect := Rect(YRect.Left, dYY-dY div 2, YRect.Right-dY div 2, dYY+dY div 2);                                  //
        DrawText(Handle, PChar(IntToStr(Y)), -1, TmpRect, DT_SINGLELINE or DT_VCENTER or DT_RIGHT);                     //
                                                                                                                        //
        MoveTo(WRect.Left,  dYY);                                                                                       //
        LineTo(WRect.Right, dYY);                                                                                       //
                                                                                                                        //
        Inc(Y, n);                                                                                                      //
      end;                                                                                                              //
                                                                                                                        //
////////////////////////////                                                                                            //
                                                                                                                        //
      MoveTo(WRect.Left,  WRect.Top);                                                                                   //
      LineTo(WRect.Right, WRect.Top);                                                                                   //
      LineTo(WRect.Right, WRect.Bottom);                                                                                //
      LineTo(WRect.Left,  WRect.Bottom);                                                                                //
      LineTo(WRect.Left,  WRect.Top);                                                                                   //
                                                                                                                        //
////////////////////////////                                                                                            //
                                                                                                                        //
      Font.Color := clBlack;                                                                                            //
      Brush.Color := clWhite;                                                                                           //
      Font.Size := 10;                                                                                                  //
      Font.Style := [fsBold];                                                                                           //
                                                                                                                        //
      dX := TextWidth('W');                                                                                             //
      dY := TextHeight('W');                                                                                            //
                                                                                                                        //
      with pStatistica^.Wafer do                                                                                        //
      begin                                                                                                             //
        dX := 2*dX;                                                                                                     //
        TextOut(dX, XRect.Bottom+1*dY, 'Код     : '+Code);                                                              //
        TextOut(dX, XRect.Bottom+2*dY, 'Партия  : '+NLot);                                                              //
        TextOut(dX, XRect.Bottom+3*dY, 'Пластина: '+Num);                                                               //
        TextOut(dX, XRect.Bottom+4*dY, 'Дата    : '+TimeDate);                                                          //
        TextOut(dX, XRect.Bottom+5*dY, 'Годных  : '+IntToStr(NOK));                                                     //
                                                                                                                        //
        dX := WRect.Right div 3;                                                                                        //
        TextOut(dX, XRect.Bottom+1*dY, 'Параметр   : '+TestsCB.Items[TestsCB.ItemIndex]);                               //
        TextOut(dX, XRect.Bottom+2*dY, 'Мин.       : '+FormatFloat('0.000', Groups.MinVal));                            //
        TextOut(dX, XRect.Bottom+3*dY, 'Средн.     : '+FormatFloat('0.000', Groups.AvrVal));                            //
        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        TextOut(dX, XRect.Bottom+4*dY, 'Макс.      : '+FormatFloat('0.000', Groups.MaxVal));                            //
        TextOut(dX, XRect.Bottom+5*dY, 'Норма мин. : '+FormatFloat('0.000', TestsParams[TestsCB.ItemIndex].Norma.Min)); //
        TextOut(dX, XRect.Bottom+6*dY, 'Норма макс.: '+FormatFloat('0.000', TestsParams[TestsCB.ItemIndex].Norma.Max)); //
                                                                                                                        //
        dX := WRect.Right-WRect.Right div 4;                                                                            //
        TextOut(dX, XRect.Bottom+1*dY, 'Округление до: '+AccCB.Items[AccCB.ItemIndex]);                                 //
        TextOut(dX, XRect.Bottom+2*dY, 'Макс. кол-во : '+IntToStr(Groups.MaxQuantity));                                 //
        TextOut(dX, XRect.Bottom+3*dY, 'Годных (пар.): '+OKPLab.Caption);                                               //
        TextOut(dX, XRect.Bottom+4*dY, 'Брака (пар.) : '+FailPLab.Caption);                                             //
      end;                                                                                                              //
  end;                                                                                                                  //
                                                                                                                        //
  Printer.EndDoc;                                                                                                       //
                                                                                                                        //
  Deleteobject(HFont);                                                                                                  //
end;                                                                                                                    //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TOKDlg.GradientRect(const startColor, endColor: TColor; Bmp: TBitmap; Rect: TRect; const FillMode: byte=0); //
var                                                                                                                   //
  tv: array [0..1] of TTRIVERTEX;                                                                                     //
  gr: GRADIENT_RECT;                                                                                                  //
begin                                                                                                                 //
  gr.UpperLeft := 0;                                                                                                  //
  gr.LowerRight := 1;                                                                                                 //
                                                                                                                      //
  tv[0].x := Rect.Left;                                                                                               //
  tv[0].y := Rect.Top;                                                                                                //
  tv[1].x := Rect.Right;                                                                                              //
  tv[1].y := Rect.Bottom;                                                                                             //
                                                                                                                      //
  tv[0].Red   := (startColor and $000000FF) shl 8;                                                                    //
  tv[0].Green :=  startColor and $0000FF00;                                                                           //
  tv[0].Blue  := (startColor and $00FF0000) shr 8;                                                                    //
//  tv[0].Alpha := (startColor and $FF000000) shr 16;                                                                 //
                                                                                                                      //
  tv[1].Red   := (endColor and $000000FF) shl 8;                                                                      //
  tv[1].Green :=  endColor and $0000FF00;                                                                             //
  tv[1].Blue  := (endColor and $00FF0000) shr 8;                                                                      //
//  tv[1].Alpha := (startColor and $FF000000) shr 16;                                                                   //
                                                                                                                      //
  GradientFill(Bmp.Canvas.Handle, @tv, 2, @gr, 1, FillMode);                                                          //
end;                                                                                                                  //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////
procedure TOKDlg.SortGroupsByValue(var Grps: TGroups);               //
var                                                                  //
  n, m, b_m: WORD;                                                   //
  b_val: Single;                                                     //
  tmpGroup: TGroup;                                                  //
begin                                                                //
  with Grps do                                                       //
  begin                                                              //
    if Length(Group) < 2 then Exit;                                  //
                                                                     //
    for n := 0 to Length(Group)-2 do                                 //
    begin                                                            //
      b_val := Group[n].RealVal;                                     //
      b_m := n;                                                      //
      for m := n+1 to Length(Group)-1 do                             //
        if Group[m].RealVal < b_val then                             //
        begin                                                        //
          b_val := Group[m].RealVal;                                 //
          b_m := m;                                                  //
        end;                                                         //
      tmpGroup   := Group[b_m];                                      //
      Group[b_m] := Group[n];                                        //
      Group[n]   := tmpGroup;                                        //
    end;                                                             //
  end;                                                               //
end;                                                                 //
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
procedure TOKDlg.SortGroupsByStatus(var Grps: TGroups);              //
var                                                                  //
  n: WORD;                                                           //
  tmpGroup: TGroup;                                                  //
begin                                                                //
  with Grps do                                                       //
  begin                                                              //
    if Length(Group) < 3 then Exit;                                  //
                                                                     //
    for n := 1 to Length(Group)-1 do                                 //
      if Group[n].RealVal = Group[n-1].RealVal then                  //
      begin                                                          //
        if n < Length(Group)-1 then // Недопустимая комбинация Г=Б Г //
          if (Group[n-1].Status = 1) and                             //
             (Group[n].Status  <> 1) and                             //
             (Group[n+1].Status = 1) then                            //
          begin                                                      //
            tmpGroup := Group[n-1];                                  //
            Group[n-1] := Group[n];                                  //
            Group[n] := tmpGroup;                                    //
          end;                                                       //
                                                                     //
        if n > 1 then               // Недопустимая комбинация Г Б=Г //
          if (Group[n-2].Status  = 1) and                            //
             (Group[n-1].Status <> 1) and                            //
             (Group[n].Status    = 1) then                           //
          begin                                                      //
            tmpGroup := Group[n-1];                                  //
            Group[n-1] := Group[n];                                  //
            Group[n] := tmpGroup;                                    //
          end;                                                       //
      end;                                                           //
  end;                                                               //
end;                                                                 //
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
procedure TOKDlg.SortMassByValue(var Mass: TDynArray);               //
var                                                                  //
  n, m, b_m: WORD;                                                   //
  b_val, tmpVal: Single;                                             //
begin                                                                //
  if Length(Mass) < 2 then Exit;                                     //
                                                                     //
  for n := 0 to Length(Mass)-2 do                                    //
  begin                                                              //
    b_val := Mass[n];                                                //
    b_m := n;                                                        //
    for m := n+1 to Length(Mass)-1 do                                //
      if Mass[m] < b_val then                                        //
      begin                                                          //
        b_val := Mass[m];                                            //
        b_m := m;                                                    //
      end;                                                           //
    tmpVal    := Mass[b_m];                                          //
    Mass[b_m] := Mass[n];                                            //
    Mass[n]   := tmpVal;                                             //
  end;                                                               //
end;                                                                 //
///////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////
function TOKDlg.GetNGroup(const X, Y: Integer): Integer; //
var                                                      //
  n: WORD;                                               //
begin                                                    //
  Result := -1;                                          //
                                                         //
  for n := 0 to Length(Groups.Group)-1 do                //
    with Groups.Group[n] do                              //
      if (X > Rect.Left) and (X < Rect.Right) then       //
      begin                                              //
        Result := n;                                     //
        Break;                                           //
      end;                                               //
end;                                                     //
///////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////
procedure TOKDlg.Set_Lab_To(const N:byte; const Enabled: Boolean); //
begin                                                              //
  case N of                                                        //
    1: if Enabled then                                             //
       begin                                                       //
         OKP_Enabled := True;                                      //
         HOKPLab.Color := clLime;                                  //
         HOKPLab.Font.Color := clBlack;                            //
         OKPLab.Font.Color  := clBlack;                            //
       end                                                         //
       else                                                        //
       begin                                                       //
         OKP_Enabled := False;                                     //
         HOKPLab.Color := clGreen;                                 //
         HOKPLab.Font.Color := clGray;                             //
         OKPLab.Font.Color  := clGray;                             //
       end;                                                        //
    2: if Enabled then                                             //
       begin                                                       //
         FlP_Enabled := True;                                      //
         HFailPLab.Color := clRed;                                 //
         HFailPLab.Font.Color := clBlack;                          //
         FailPLab.Font.Color  := clBlack;                          //
       end                                                         //
       else                                                        //
       begin                                                       //
         FlP_Enabled := False;                                     //
         HFailPLab.Color := clMaroon;                              //
         HFailPLab.Font.Color := clGray;                           //
         FailPLab.Font.Color  := clGray;                           //
       end;                                                        //
  end;                                                             //
end;                                                               //
/////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////
procedure TOKDlg.Repaint_Event(var Mes: TMessage); //
begin                                              //
  PreShowOK;                                       //
end;                                               //
/////////////////////////////////////////////////////


{ TCntrEdit }

procedure TCntrEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_CENTER;
end;

end.

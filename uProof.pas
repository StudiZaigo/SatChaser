unit uProof;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmProof = class(TForm)
    btnGoBack: TButton;
    btnGoForward: TButton;
    btnExit: TButton;

    Timer1: TTimer;

    pnlStep1: TPanel;
    pnlStep2: TPanel;
    pnlStep3: TPanel;
    pnlStep4: TPanel;

    lblStep: TLabel;
    lblComment: TLabel;
    lblA: TLabel;
    lblB: TLabel;
    lblC: TLabel;
    lblD: TLabel;
    lblE: TLabel;
    lblF: TLabel;
    Label3: TLabel;
    Label11: TLabel;

    txtA: TEdit;
    txtB: TEdit;
    txtC: TEdit;
    txtD: TEdit;
    txtE: TEdit;
    txtF: TEdit;

    procedure btnGoForwardClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private 宣言 }
    z: integer;
    procedure SetPanelSize(panel1, Panel2: Tpanel);
    procedure SetLabelSize(Label1, Label2: TLabel);
    procedure SetEditSize(Edit1, Edit2: TEdit);
    procedure ChangePanel(Panel: TPanel);
    procedure Step1;
    procedure Step2;
    procedure Step3;
    procedure Step4;
  public
    { Public 宣言 }
  end;

var
  frmProof: TfrmProof;

implementation

uses uMain;

{$R *.dfm}

procedure TfrmProof.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer1.Enabled := false;
end;

procedure TfrmProof.FormCreate(Sender: TObject);
begin

  pnlStep1.BevelOuter  :=bvNone;
  SetPanelSize(pnlStep1, pnlStep2);
  SetPanelSize(pnlStep1, pnlStep3);
  SetPanelSize(pnlStep1, pnlStep4);
  SetLabelSize(lblA, lblC);
  SetLabelSize(lblA, lblD);
  SetLabelSize(lblA, lblF);
  SetLabelSize(lblB, lblE);
  SetEditSize(txtA, txtC);
  SetEditSize(txtA, txtD);
  SetEditSize(txtA, txtF);
  SetEditSize(txtB, txtE);

  z := 0;
  Timer1.Interval := 1500;
  Timer1.Enabled := false;
  Step1;
end;

procedure TfrmProof.SetPanelSize(panel1, Panel2: TPanel);
begin
  with Panel2 do
    begin
    Left        := Panel1.Left;
    Top         := Panel1.Top;
    Height      := Panel1.Height;
    Width       := panel1.Width;
    BevelOuter  := bvNone;
//    Enable      := false;
    Visible     := false;
    end;
end;

procedure TfrmProof.SetLabelSize(Label1, Label2: TLabel);
begin
  with Label2 do
    begin
    Left := left;
    Top  := top;
    Height := Label1.Height;
    Width  := Label1.Width;
    end;
end;

procedure TfrmProof.SetEditSize(Edit1, Edit2: TEdit);
begin
  with Edit2 do
    begin
    Left := Edit1.left;
    Top  := Edit1.top;
    Height := Edit1.Height;
    Width  := Edit1.Width;
    end;
end;

procedure TfrmProof.btnGoForwardClick(Sender: TObject);
begin
//  label1.Caption := '';
  Timer1.Enabled := false;
//  Gs23.ClearComm();
  case BtnGoForward.Tag of
  1: Step2;
  2: Step3;
  3: Step4;
  end;
end;

procedure TfrmProof.btnExitClick(Sender: TObject);
begin
  Timer1.Enabled := false;
//  Main.ClearComm();
  ModalResult := mrOK;
end;

procedure TfrmProof.Timer1Timer(Sender: TObject);
var
  l: integer;
  s: string;
begin
  case btnGoBack.Tag of
  1: begin
    l := 15;
//    s := Main.RecvComm(l);
    if Length(s) = l then
      begin
      txtA.Text := copy(s, 4,4);
      txtB.Text := copy(s, 10,4);
      end;
    end;
  2: begin
//    l := 6;
//    s := Main.RecvComm(l);
//    if Length(s) = l then
      begin
      txtA.Text := copy(s, 1,5);
      end;
    end;
  3: begin
    l := 28;
//    s := Main.RecvComm(l);
    if Length(s) = l then
      begin
      txtA.Text := copy(s, 18,4);
      txtB.Text := copy(s, 24,4);
      end;
    end;
  4: begin
    l := 11;
//    s := Main.RecvComm(l);
    if Length(s) = l then
      begin
      txtA.Text := copy(s, 7,4);
      end;
    end;
//  else l := 1;
  end;
   Inc(z);
//   Label2.Caption := IntToStr(z);
//   label1.Caption := s;
end;

procedure TfrmProof.Step1;
var
  s: string;
begin
  lblStep.Caption := 'Step1(水平オフセットの調整)';
  s := '・ローターの電源を入れ、方位角を0°に設定する。';
  s := s + #13#10#13#10;
  s := s + '・「A」の値と「B」の値が一致するよう、GS-23の基板上に「AZボリューム」を調整する。';
  s := s + #13#10;
  s := s + '・調整が終了したら、コントローラの電源を一旦切り、再び入れ直す。';
  lblComment.Caption := s;

  ChangePanel(pnlStep1);
  with btnGoBack do
    begin
    Enabled := false;
    Tag := 1;
    end;
  with btnGoforward do
    begin
    Enabled := true;
    Tag := 1;
    end;
  btnExit.Enabled := true;

//  Main.SendComm('');
//  Main.ClearComm();
//  Main.SendComm('O');
  Timer1.Enabled := true;
end;

procedure TfrmProof.Step2;
var
  s: string;
begin
  lblStep.Caption := 'Step2(水平フルスケールの調整)';
  s := '・ローターの電源を入れ、方位角を450°に設定する';
  s := s + #13#10#13#10;
  s := s + '・「A」の値が450になるよう、コントローラ背面の「OUT VOL ADJ」を調整する。';
  s := s + #13#10#13#10;
  s := s +'・調整が終了したら、コントローラの電源を一旦切り、再び入れなおす。';
  lblComment.Caption := s;

  ChangePanel(pnlStep2);
  with btnGoBack do
    begin
    Enabled := true;
    Tag := 2;
    end;
  with btnGoforward do
    begin
    Enabled := true;
    Tag := 2;
    end;
  btnExit.Enabled := true;

//  Main.SendComm('');
//  Main.ClearComm();
//  Main.SendComm('F');
  Timer1.Enabled := true;
end;

procedure TfrmProof.Step3;
var
  s: string;
begin
  lblStep.Caption := 'Step3(垂直オフセットの調整)';
  s := '・ローターの電源を入れ、仰角を0°に設定する。';
  s := s + #13#10#13#10;
  s := s + '・「A」の値と「B」の値が一致するよう、GS-23の基板上に「ELボリューム」を調整する。';
  s := s + #13#10#13#10;
  s := s + '・調整が終了したら、コントローラの電源を一旦切り、再び入れ直す。';
  lblComment.Caption := s;

  ChangePanel(pnlStep3);
  with btnGoBack do
    begin
    Enabled := true;
    Tag := 3;
    end;
  with btnGoforward do
    begin
    Enabled := true;
    Tag := 3;
    end;
  btnExit.Enabled := true;

//  Main.SendComm('');
//  Main.ClearComm();
//  Main.SendComm('O2');
  Timer1.Enabled := true;
end;

procedure TfrmProof.Step4;
var
  s: string;
begin
  lblStep.Caption := 'Step4(仰角フルスケールの調整)';
  s := '・ローターの電源を入れ、仰角を180°に設定する';
  s := s + #13#10#13#10;
  s := s + '・「A」の値が180になるよう、GX-500の「FULL SCALE ADJ」を調整する。';
  s := s + #13#10#13#10;
  s := s + '・調整が終了したら、コントローラの電源を一旦切り、再び入れなおす。';
  lblComment.Caption := s;

  ChangePanel(pnlStep4);
  with btnGoBack do
    begin
    Enabled := true;
    Tag := 4;
    end;
  with btnGoforward do
    begin
    Enabled := false;
    Tag := 4;
    end;
  btnExit.Enabled := true;

//  Main.SendComm('');
//  Main.ClearComm();
//  Main.SendComm('F2');
  Timer1.Enabled := true;
end;

procedure TfrmProof.ChangePanel(Panel: TPanel);
var
  i: integer;
begin
  With frmProof do
    begin
    for i := 0 to frmProof.ControlCount -1 do
      begin
      if frmProof.Controls[i] is TPanel then
        if Controls[i] = Panel then
          begin
          Visible := true;
          BringtoFront;
          end
        else
          Visible := false;
      end;
    end;
end;

end.

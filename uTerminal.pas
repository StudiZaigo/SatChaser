unit uTerminal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions,
  Vcl.ActnList,
  uMain;

type
  TTerminal = class(TForm)
    edtMsg: TEdit;
    memMsg: TMemo;
    btnClose: TButton;
    ActionList1: TActionList;
    actClose: TAction;
    btnClear: TButton;
    btnSend: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtMsgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure edtMsgKeyPress(Sender: TObject; var Key: Char);
    procedure btnSendClick(Sender: TObject);
  private
    procedure MsgSendAndRecv;
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Terminal: TTerminal;

implementation

{$R *.dfm}

procedure TTerminal.btnClearClick(Sender: TObject);
begin
  memMsg.Clear;
  edtMsg.Clear;
end;

procedure TTerminal.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TTerminal.btnSendClick(Sender: TObject);
begin
  MsgSendAndRecv;
end;

procedure TTerminal.edtMsgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    MsgSendAndRecv;
end;

procedure TTerminal.edtMsgKeyPress(Sender: TObject; var Key: Char);
begin
// OnKeyDownでBeep音を鳴らさないための処理
  if key = #13 then // #13 = Return
    Key := #0;
end;

procedure TTerminal.FormCreate(Sender: TObject);
begin
  btnClearClick(btnClear);
end;

procedure TTerminal.MsgSendAndRecv();
var
  s,t: string;
begin
  try
    begin
    edtMsg.Text := Trim(edtMsg.Text);
    if Length(edtMsg.Text) = 0 then
      exit;

    if edtMsg.Text = 'Z' then  //  Bufferクリアコマンド GS-232にはない
      begin
      Main.Gs232.DoCmd(edtMsg.Text);
      memMsg.Lines.Add('-->' + edtMsg.Text);
      exit;
      end;

    Main.Gs232.DoCmd(edtMsg.Text);
    memMsg.Lines.Add('-->' + edtMsg.Text);
    end;

    s := Copy(edtMsg.Text, 1, 1);
    if (s = 'C') or (s = 'N')  then     // 応答メッセージがあるコマンド
//      if (s = 'C') or (s = 'N') or (s = 'O') or (s = 'F') then     // 'O' 'F' 未対応
      begin
      t := Main.Gs232.DoRecv(20);
      t := StringReplace(t, #13#10, '', [rfReplaceAll]);
      if t <> '' then
        memMsg.Lines.Add('<--' + t);
      end
    else if (s = 'H') then             // ヘルプコマンド
      begin
      t := Main.Gs232.DoRecv(1024);
      if t <> '' then
        memMsg.Lines.Add(t);
      end;
  finally
    edtMsg.Clear;
    edtMSG.SetFocus;
  end;
end;

end.

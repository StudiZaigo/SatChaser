unit uOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  Registry, ExtCtrls, UITypes,
  XmlIniFile;


type
  TOptions = class(TForm)
    PageControl1: TPageControl;
    grpElevation: TGroupBox;
    grpParking: TGroupBox;

    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;

    tabApp: TTabSheet;
    tabCom: TTabSheet;
    tabGs232: TTabSheet;
    btnOk1: TButton;
    btnOk2: TButton;
    btnOk0: TButton;
    btnCancel1: TButton;
    btnCancel2: TButton;
    btnCancel0: TButton;
    btnGetPosition: TButton;
    btnFile1: TButton;
    btnFile2: TButton;
    cbxGoParking: TCheckBox;
    cmbPort: TComboBox;
    cmbBaudRate: TComboBox;
    cmbDataBits: TComboBox;
    cmbParityBits: TComboBox;
    cmbStopBits: TComboBox;
    cmbFlowControl: TComboBox;
    cmbApp: TComboBox;
    edtParkingAz: TEdit;
    edtParkingEl: TEdit;
    edtInifile: TEdit;
    edtElements: TEdit;

    OpenDialog1: TOpenDialog;
    rbtEl90: TRadioButton;
    grpAzimuth: TGroupBox;
    rbtAz450: TRadioButton;
    rbtAz360: TRadioButton;
    rbtEl180: TRadioButton;
    Label15: TLabel;
    cmbSpeed: TComboBox;
    Label8: TLabel;
    edtOffset: TEdit;
    edtRegKey: TEdit;

    procedure FormCreate(Sender: TObject);
    procedure btnOk0Click(Sender: TObject);
    procedure btnFile2Click(Sender: TObject);
    procedure btnGetPositionClick(Sender: TObject);
    procedure btnCancel0Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmbAppChange(Sender: TObject);
    procedure cmbPortChange(Sender: TObject);
  private
    { Private 宣言 }
    XMLIniFile: string;
    XMLIni: TXMLIniFile;

    procedure ReadIniFile;
    procedure WriteIniFile1;
    procedure WriteIniFile2;
    procedure GetComList();
    function CheckData: boolean;
  public
    { Public 宣言 }
  end;

var
  Options: TOptions;

implementation

uses
  uMain;

{$R *.dfm}

procedure TOptions.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckData;
end;

procedure TOptions.FormCreate(Sender: TObject);
begin
  edtRegKey.Visible := MAin.isDebug;
  XMLIniFile := ChangeFileExt(Application.ExeName,'.xml');
  GetComList();   // ReadInifileの前に処理が必要
  ReadInifile;
end;

procedure TOptions.btnGetPositionClick(Sender: TObject);
var
  AntAz,AntEl: integer;
begin
  Main.GS232.GetPos(AntAz, AntEl);
  if (AntAz <> StrToInt(edtParkingAz.Text))
  or (AntEl <> StrToInt(edtParkingEl.Text)) then
    begin
    edtParkingAz.Text := IntToStr(AntAz);
    edtParkingEl.Text := IntToStr(AntEl);
    end;
end;

procedure TOptions.btnOk0Click(Sender: TObject);
begin
  WriteIniFile1();
  WriteIniFile2();
end;

procedure TOptions.btnCancel0Click(Sender: TObject);
begin
  WriteIniFile1();
end;

procedure TOptions.btnFile2Click(Sender: TObject);
var
  s: string;
begin
  s := edtElements.Text;                     // 一時退避
  with OpenDialog1 do
    begin
    DefaultExt := '.TXT';
    FileName := edtElements.Text;
    Filter := 'Text file (*.*)|*.TXT';
    if Execute then
      begin
      edtElements.Text := FileName;
      end;
    end;
end;

procedure TOptions.cmbAppChange(Sender: TObject);
begin
  XMLIni := TXMLIniFile.Create(XMLIniFile);
  try
    XMLIni.OpenNode(cRegistry, false);
    edtRegKey.Text    := XMLIni.ReadString(cmbApp.Text, '');
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.cmbPortChange(Sender: TObject);
var
  i: integer;
begin
  i :=  cmbPort.Items.IndexOf(cmbPort.Text);
  if i = -1 then   // Items無いにない時-1が帰ってくる
    begin;
    MessageDlg('Com PortのPort No.が誤っています', mtError, [mbOK], 0);
    cmbPort.SetFocus;
    end;
end;

function TOptions.CheckData(): boolean;
var
  Reg: TRegistry;
  i: integer;
  function CheckFile(FN: string): boolean;
  begin
    result := true;
    if FN = '' then
      begin
      MessageDlg(format('%sのINIFILE指定が空白です', [edtIniFile.Text]),  mtError, [mbOK], 0);
      result := false;
      exit;
      end
    else
      begin
      if not FileExists(FN) then
        begin
        MessageDlg(format('%sの%s ファイルが見つかりません', [edtIniFile.Text, FN]),  mtError, [mbOK], 0);
        result := false;
        exit;
        end
      end;
  end;
begin
  result := false;
  if not CheckFile(edtInifile.Text) then
    exit;
  if not CheckFile(edtElements.Text) then
    exit;
  if edtRegKey.Text = '' then
    begin
    MessageDlg(format('%sのレジストリ指定が空白です', [edtIniFile.Text]),  mtError, [mbOK], 0);
    exit;
    end;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKey(edtRegKey.Text, False) then
      begin;
      MessageDlg(format('%sのレジストリが読めません', [edtIniFile.Text]), mtError, [mbOK], 0);
      Exit;
      end;
  finally
    FreeAndNil(Reg);
  end;
  i :=  cmbPort.Items.IndexOf(cmbPort.Text);
  if i = -1 then   // Items無いにない時-1が帰ってくる
    begin;
    MessageDlg('Com PortのPort No.が誤っています', mtError, [mbOK], 0);
    Exit;
    end;
  result := true;
end;

procedure TOptions.ReadIniFile();
begin
  XMLIni := TXMLIniFile.Create(XMLIniFile);
  try
    XMLIni.OpenNode(cNodeOptions, true);
    Self.Left := XmlIni.ReadInteger(cLeft, 0);
    Self.Top   := XmlIni.ReadInteger(cTop, 0);
    PageControl1.TabIndex   := XMLIni.ReadInteger(cTabIndex, 0);
    if (Self.Left = 0) and (Self.Top = 0) then
      begin
      Self.Left   := (Screen.Width - Self.Width)   Div 2;
      Self.Top := (Screen.Height - Self.Height) Div 2;
      end;

    XMLIni.OpenNode(cNodeApp, true);
    cmbApp.Text           := XMLIni.ReadString(cApp, 'CALSAT32');
    edtRegKey.Text        := XMLIni.ReadString(cRegKey, '');
    edtInifile.Text       := XMLIni.ReadString(cInifile, '');
    edtElements.Text      := XMLIni.ReadString(cElements, '');

    XMLIni.OpenNode(cNodeCom, true);
    cmbPort.Text          := XMLIni.ReadString(cPort, cmbPort.items.Strings[0]);
    cmbBaudRate.Text      := XMLIni.ReadString(cBaudRate, '9600');
    cmbDataBits.Text      := XMLIni.ReadString(cDataBits, '8');
    cmbParityBits.Text    := XMLIni.ReadString(cParity, 'None');
    cmbStopBits.Text      := XMLIni.ReadString(cStopBits, '1');
    cmbFlowControl.Text   := XMLIni.ReadString(cFlowControl, 'Hardware');

    XMLIni.OpenNode(cNodeGS232, true);
    rbtAz450.Checked      := XmlIni.ReadBool(cAzRotator, true);
    rbtEl180.Checked      := XmlIni.ReadBool(cElRotator, true);
    rbtAz360.Checked      := not rbtAz450.Checked;       // 本来は不要なはず？
    rbtEl90.Checked       := not rbtEl180.Checked;
    edtOffset.Text        := IntToStr(XMLIni.ReadInteger(cOffset, 0));
    cmbSpeed.ItemIndex    := XMLIni.ReadInteger(cRotateSpeed, 1);
    edtParkingAz.Text     := IntToStr(XMLIni.ReadInteger(cParkingAz, 0));
    edtParkingEl.Text     := IntToStr(XMLIni.ReadInteger(cParkingEl, 0));
    cbxGoParking.Checked  := XMLIni.ReadBool(cGoParking, true);
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.WriteIniFile1();
begin
  XMLIni := TXMLIniFile.Create(XMLIniFile);
  try
    XMLIni.OpenNode(cNodeOptions, true);
    XmlIni.WriteInteger(cLeft, Self.Left);
    XmlIni.WriteInteger(cTop, Self.Top);
    XMLIni.WriteInteger(cTabIndex, PageControl1.TabIndex);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.WriteIniFile2();
begin
  XMLIni := TXMLIniFile.Create(XMLIniFile);
  try
    XMLIni.OpenNode(cNodeApp, true);
    XMLIni.WriteString(cApp, cmbApp.Text);
    XMLIni.WriteString(cRegKey, edtRegKey.Text);
    XMLIni.WriteString(cInifile, edtInifile.Text);
    XMLIni.WriteString(cElements, edtElements.Text);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;

    XMLIni.OpenNode(cNodeCom, true);
    XMLIni.WriteString(cPort, cmbPort.Text);
    XMLIni.WriteString(cBaudRate, cmbBaudRate.Text);
    XMLIni.WriteString(cDataBits, cmbDataBits.Text);
    XMLIni.WriteString(cParity, cmbParityBits.Text);
    XMLIni.WriteString(cStopBits, cmbStopBits.Text);
    XMLIni.WriteString(cFlowControl, cmbFlowControl.Text);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;

    XMLIni.OpenNode(cNodeGS232, true);
    XmlIni.WriteBool(cAzRotator, rbtAz450.Checked);
    XmlIni.WriteBool(cElRotator, rbtEl180.Checked);
    if rbtAz450.Checked then
      XmlIni.WriteInteger(cGsMode, Integer(gsOverlap))
    else if rbtEl180.Checked then
      XmlIni.WriteInteger(cGsMode, Integer(gsFlip))
    else
      XmlIni.WriteInteger(cGsMode, Integer(gsNone));
    XMLIni.WriteInteger(cOffset, StrToInt(edtOffset.Text));
    XMLIni.WriteInteger(cRotateSpeed, cmbSpeed.ItemIndex);
    XMLIni.WriteInteger(cParkingAz, StrToInt(edtParkingAz.Text));
    XMLIni.WriteInteger(cParkingEl, StrToInt(edtParkingEl.Text));
    XMLIni.WriteBool(cGoParking, cbxGoParking.Checked);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.GetComList();
var
  i: integer;
  sl: TStringList;
  reg: TRegistry;
begin
  sl := TStringList.Create;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('HARDWARE\DEVICEMAP\SERIALCOMM', False) then
      begin
      reg.GetValueNames(sl);
      for i := 0 to Pred(sl.Count) do
        if reg.GetDataType(sl.Strings[i]) = rdString then
          cmbPort.Items.Append(reg.ReadString(sl.Strings[i]));
      end;
  finally
    reg.Free;
    sl.Free;
  end;
end;

end.



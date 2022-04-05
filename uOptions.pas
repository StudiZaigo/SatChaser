unit uOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  Registry, ExtCtrls, UITypes,
  XmlIniFile,

  uConstant;

type
  TOptions = class(TForm)
    btnCancel0: TButton;
    btnCancel1: TButton;
    btnCancel2: TButton;
    btnFile1: TButton;
    btnFile2: TButton;
    btnGetPosition: TButton;
    btnOk0: TButton;
    btnOk1: TButton;
    btnOk2: TButton;
    cbxGoParking: TCheckBox;
    cmbApp: TComboBox;
    cmbBaudRate: TComboBox;
    cmbDataBits: TComboBox;
    cmbFlowControl: TComboBox;
    cmbParity: TComboBox;
    cmbComPort: TComboBox;
    cmbRotateSpeed: TComboBox;
    cmbStopBits: TComboBox;
    edtAzOffset: TEdit;
    edtElements: TEdit;
    edtInifile: TEdit;
    edtIntervalTime: TEdit;
    edtParkingAz: TEdit;
    edtParkingEl: TEdit;
    edtRegKey: TEdit;
    grpAzimuth: TGroupBox;
    grpParking: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    rgpAzimuth: TRadioGroup;
    rgpElevation: TRadioGroup;
    tabApp: TTabSheet;
    tabCom: TTabSheet;
    tabGs232: TTabSheet;

    procedure btnCancel0Click(Sender: TObject);
    procedure btnFile2Click(Sender: TObject);
    procedure btnGetPositionClick(Sender: TObject);
    procedure btnOk0Click(Sender: TObject);
    procedure cmbAppChange(Sender: TObject);
    procedure cmbComPortChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnFile1Click(Sender: TObject);
  private
    { Private 宣言 }
    XMLIni: TXMLIniFile;
    XMLIniName: string;

    function CheckData: boolean;
    procedure ReadXMLIniFile1;
    procedure ReadXMLIniFile2;
    procedure WriteXMLIniFile1;
    procedure WriteXMLIniFile2;
    procedure GetComList();

  public
    { Public 宣言 }
  end;

var
  Options: TOptions;

implementation

{$R *.dfm}

uses
  uMain;

procedure TOptions.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckData;
end;

procedure TOptions.FormCreate(Sender: TObject);
begin
  XMLIniName := ChangeFileExt(Application.ExeName,'.xml');
  GetComList();       // ReadInifileの前に処理が必要
  ReadXMLInifile1;
  ReadXMLInifile2;
end;

procedure TOptions.btnGetPositionClick(Sender: TObject);
var
  Az, El: integer;
begin
  Main.GS232.GetPos(Az, El);
  if (Az <> StrToInt(edtParkingAz.Text))
  or (El <> StrToInt(edtParkingEl.Text)) then
    begin
    edtParkingAz.Text := IntToStr((Az + 3600) mod 360);
    edtParkingEl.Text := IntToStr((El + 1800) mod 180);
    end;
end;

procedure TOptions.btnOk0Click(Sender: TObject);
begin
  WriteXmlIniFile1();
  WriteXmlIniFile2();
end;

procedure TOptions.btnCancel0Click(Sender: TObject);
begin
  WriteXmlIniFile1();
end;

procedure TOptions.btnFile1Click(Sender: TObject);
var
  s: string;
begin
  s := edtInifile.Text;                     // 一時退避
  with OpenDialog1 do
    begin
    InitialDir  := ExtractFileDir(edtInifile.Text);
    FileName    := ExtractFileName(edtInifile.Text);
    DefaultExt := '.ini';
    Filter := 'Ini file (*.*)|*.ini';
    if Execute then
      begin
      edtInifile.Text := FileName
      end
    else
      edtInifile.Text := s;
    end;
end;

procedure TOptions.btnFile2Click(Sender: TObject);
var
  s: string;
begin
  s := edtElements.Text;                     // 一時退避
  with OpenDialog1 do
    begin
    InitialDir := ExtractFileDir(edtElements.Text);
    FileName := ExtractFileName(edtElements.Text);
    DefaultExt := '.TXT';
    Filter := 'Text file (*.*)|*.TXT';
    if Execute then
      edtElements.Text := FileName
    else
      edtElements.Text := s;
    end;
end;

procedure TOptions.cmbAppChange(Sender: TObject);
begin
  XMLIni := TXMLIniFile.Create(XMLIniName);
  try
    XMLIni.OpenNode(cnRegistry, false);
    edtRegKey.Text  := XMLIni.ReadString(cmbApp.Text, '');
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.cmbComPortChange(Sender: TObject);
var
  i: integer;
begin
  i :=  cmbComPort.Items.IndexOf(cmbComPort.Text);
  if i = -1 then   // Items無いにない時-1が帰ってくる
    begin;
    MessageDlg('ComPortのPort No.が誤っています', mtError, [mbOK], 0);
    cmbComPort.SetFocus;
    exit;
    end;
end;

function TOptions.CheckData(): boolean;
var
  Reg: TRegistry;
  i: integer;
  RootKey: integer;
  function CheckFile(a, b: string): boolean;
  begin
    result := true;
    if a = '' then
      begin
      MessageDlg(format('%sの指定が空白です', [a]),  mtError, [mbOK], 0);
      result := false;
      exit;
      end
    else
      begin
      if not FileExists(b) then
        begin
        MessageDlg(format('%sの%s ファイルが見つかりません', [a, b]),  mtError, [mbOK], 0);
        result := false;
        exit;
        end
      end;
  end;
begin
  result := false;
  if not CheckFile(cnInifile, XMLIniName) then
    exit;
  if not CheckFile(cnElements, edtElements.Text) then
    exit;
  if edtRegKey.Text = '' then
    begin
    MessageDlg(format('%sのレジストリ指定が空白です', [edtIniFile.Text]),  mtError, [mbOK], 0);
    exit;
    end;
  Reg := TRegistry.Create;
  try
    RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKey(edtRegKey.Text, False) then
      begin;
      MessageDlg(format('%sのレジストリが読めません', [edtIniFile.Text]), mtError, [mbOK], 0);
      Exit;
      end;
  finally
    FreeAndNil(Reg);
  end;
  i :=  cmbComPort.Items.IndexOf(cmbComPort.Text);
  if i = -1 then   // Items無いにない時-1が帰ってくる
    begin;
    MessageDlg('Com PortのPort No.が誤っています', mtError, [mbOK], 0);
    Exit;
    end;
  result := true;
end;

procedure TOptions.ReadXmlIniFile1();
begin
  XMLIni := TXMLIniFile.Create(XMLIniName);
  try
    XMLIni.OpenNode(cnNodeOptions, true);
    Self.Left := XmlIni.ReadInteger(cnLeft, 0);
    Self.Top   := XmlIni.ReadInteger(cnTop, 0);
    PageControl1.TabIndex   := XMLIni.ReadInteger(cnTabIndex, 0);
    if (Self.Left = 0) and (Self.Top = 0) then
      begin
      Self.Left   := (Screen.Width - Self.Width)   Div 2;
      Self.Top := (Screen.Height - Self.Height) Div 2;
      end;

  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.ReadXmlIniFile2();
begin
  XMLIni := TXMLIniFile.Create(XMLIniName);
  try
    XMLIni.OpenNode(cnNodeOptions, true);
    XMLIni.OpenNode(cnNodeApp, true);
    cmbApp.Text       := XMLIni.ReadString(cnApp, 'CALSAT32');
    edtInifile.Text   := XMLIni.ReadString(cnInifile, 'C:\Calsat32\CALSAT32.INI');
    edtRegKey.Text    := XMLIni.ReadString(cnRegKey, '\Software\VB and VBA Program Settings\JR1HUO\CALSAT32');
    edtElements.Text  := XMLIni.ReadString(cnElements, 'C:\Calsat32\ELEM.TXT');

    XMLIni.OpenNode(cnNodeCom, true);
    cmbComPort.Text       := XMLIni.ReadString(cnComPort, cmbComPort.items.Strings[0]);
    cmbBaudRate.Text      := IntToStr(XMLIni.ReadInteger(cnBaudRate, 9600));
    cmbDataBits.Text      := XMLIni.ReadString(cnDataBits, '8');
    cmbParity.Text        := XMLIni.ReadString(cnParity, 'None');
    cmbStopBits.Text      := XMLIni.ReadString(cnStopBits, '1');
    cmbFlowControl.Text   := XMLIni.ReadString(cnFlowControl, 'Hardware');

    XMLIni.OpenNode(cnNodeGS232, true);
    rgpAzimuth.ItemIndex      := XmlIni.ReadInteger(cnAzRotator, 0);
    rgpElevation.ItemIndex    := XmlIni.ReadInteger(cnElRotator, 0);
    edtIntervalTime.Text      := IntToStr(XmlIni.ReadInteger(cnInterval, 1000));
    edtAzOffset.Text          := IntToStr(XMLIni.ReadInteger(cnAzOffset, 0));
    cmbRotateSpeed.ItemIndex  := XMLIni.ReadInteger(cnRotateSpeed, 1);
    edtParkingAz.Text         := IntToStr(XMLIni.ReadInteger(cnParkingAz, 0));
    edtParkingEl.text         := IntToStr(XMLIni.ReadInteger(cnGoParking, 0));
    cbxGoParking.Checked      := XMLIni.ReadBool(cnGoParking, true);
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.WriteXmlIniFile1();
begin
  XMLIni := TXMLIniFile.Create(XMLIniName);
  try
    XMLIni.OpenNode(cnNodeOptions, true);
    XmlIni.WriteInteger(cnLeft, Self.Left);
    XmlIni.WriteInteger(cnTop, Self.Top);
    XMLIni.WriteInteger(cnTabIndex, PageControl1.TabIndex);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;
  finally
    FreeAndNil(XMLIni);
  end;
end;

procedure TOptions.WriteXmlIniFile2();
begin
  XMLIni := TXMLIniFile.Create(XMLIniName);
  try
    XMLIni.OpenNode(cnNodeApp, true);
    XMLIni.WriteString(cnApp, cmbApp.Text);
    XMLIni.WriteString(cnInifile, edtInifile.Text);
    XMLIni.WriteString(cnRegKey, edtRegKey.text);
    XMLIni.WriteString(cnElements, edtElements.Text);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;

    XMLIni.OpenNode(cnNodeCom, true);
    XMLIni.WriteString(cnComPort, cmbComPort.text);
    XMLIni.WriteInteger(cnBaudRate, StrToInt(cmbBaudRate.text));
    XMLIni.WriteString(cnDataBits, cmbDataBits.Text);
    XMLIni.WriteString(cnParity, cmbParity.text);
    XMLIni.WriteString(cnStopBits, cmbStopBits.text);
    XMLIni.WriteString(cnFlowControl, cmbFlowControl.text);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;

    XMLIni.OpenNode(cnNodeGS232, true);
    XmlIni.WriteInteger(cnInterval, StrToInt(edtIntervalTime.Text));
    XmlIni.WriteInteger(cnAzRotator, rgpAzimuth.ItemIndex);
    XmlIni.WriteInteger(cnElRotator, rgpElevation.ItemIndex);
    XMLIni.WriteInteger(cnAzOffset, StrToInt(EdtAzOffset.Text));
    XMLIni.WriteInteger(cnRotateSpeed, cmbRotateSpeed.ItemIndex);
    XMLIni.WriteInteger(cnParkingAz, StrToInt(edtParkingAz.Text));
    XMLIni.WriteInteger(cnParkingEl, StrToInt(edtParkingEl.Text));
    XMLIni.WriteBool(cnGoParking, cbxGoParking.Checked);
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
          cmbComPort.Items.Append(reg.ReadString(sl.Strings[i]));
      end;
  finally
    reg.Free;
    sl.Free;
  end;
end;


end.



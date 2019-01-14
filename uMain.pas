unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ExtCtrls, Buttons, ActnList, Menus,
  Math, DateUtils, UITypes,
  IniFiles, XmlIniFile,
  CPortX, Cport, System.Actions;

//**********************************************************************//
//                                                                      //
//  ��O�ʒm�p�̃N���X                                                  //
//                                                                      //
//**********************************************************************//
type
  EAppError = class(Exception);

type
  TAzAngle = (az360, az450);
  TElAngle = (el90, el180);
  TGsMode = (gsNone, gsOverlap, gsFlip);

//**********************************************************************//
//                                                                      //
//  �q���̋O���v�f�N���X                                                //
//                                                                      //
//**********************************************************************//
  TOrbitalElments = record
    Satellite: String;         // �q����
    Epoch: TDateTime;          // ����
    Inclination: double;       // i:  �O���X�Ίp�iRad)       Inclination
    Eccentricity: double;      // e:  ���S��                 Eccentricity
    MeanAnomaly: double;       // M or l:  ���ϋߓ_�p(Rad)   Mean Anomaly
    MeanMotion: double;        // n:  ���ω^���irad/���j     Mean Motion
    DecayRate: double;         // n1: ���㗦                 Decay Rate, Drug ���ω^���̕ω� �ۓ�1
    N2:    double;             // n2: ���㗦                 Decay Rate, Drug ���ω^���̕ω� �ۓ�1
    RAAN:  double;             // ��: ����_�Ԍo(�x)�@       Right Aceention of Ascending NODE
    ARGP:  double;             // ��: �ߒn�_����             Argument of Perigee
    MajorAxis: double;         // a:  �����a                 Semi Major Axis
    EpochRev:Integer;          // ����ԍ�
    Epoch_Y: Integer;
    Epoch_D: double;
    Epoch_MJD: Double;
  end;

//**********************************************************************//
//                                                                      //
//  �ϑ��n�_�i�^�p�n�_�j�N���X                                          //
//                                                                      //
//**********************************************************************//
  TObservationPoint = record
    Longitude: Double;         //  �o�x
    Latitude: Double;          //  �ܓx
    Height: Double;            //  ���x
    TimeOffset: double;        //  ����
    LocalTimeStr: string;      //  JST ���̕�����
  end;

//**********************************************************************//
//                                                                      //
//  CommX�̊g���N���X                                          //
//                                                                      //
//**********************************************************************//
type
  TCommX_Ex = class(TCPortX)
  private
    { Private �錾 }
    XmlIni: TXmlIniFile;
    CPortX: TCportX;
  public
    { Public �錾 }
//    procedure Open;
    function Open(FN: string): boolean;
    procedure Close;
    procedure Clear();
    function  Recv(MaxLen: integer):string;
    procedure Send(str: string);
  end;

//**********************************************************************//
//                                                                      //
//  GS-232�̃N���X                                                       //
//                                                                      //
//**********************************************************************//
type
  TGS232 = class(TComponent)
  private
    { Private �錾 }
    XmlIni: TXmlIniFile;
    XmlIniFile: string;

    OffSet: integer;
    ParkingAz: integer;
    ParkingEl: integer;
    GoParking: boolean;
    RotateSpeed: Integer;

    Az_p: integer;
    El_p: integer;
    isAos: boolean;
    isLos: boolean;

    CommX_Ex: TCommX_Ex;

    FOpend: boolean;
    FOverlap: boolean;
    FFlip: boolean;
    FAntennaAz: integer;
    FRotatorEl: integer;
    FRotatorAz: integer;
    FAntennaEl: integer;
    FMode: TGsMode;
    FZeroPathOrbit: boolean;
    FisCW: boolean;
    procedure OnPortException(Sender: TObject; TComException: TComExceptions;
      ComportMessage: String; WinError: Int64; WinMessage: String);

  public
    { Public �錾 }
    property Opend: boolean read FOpend;
    property AntennaAz: integer read FAntennaAz;
    property AntennaEl: integer read FAntennaEl;
    property RotatorAz: integer read FRotatorAz;
    property RotatorEl: integer read FRotatorEl;
    property Mode: TGsMode read FMode;
    property ZeroPathOrbit: boolean read FZeroPathOrbit;
    property isCW: boolean read FisCW;
    property isOverlap: boolean read FOverlap;
    property isFlip: boolean read FFlip;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function Move(Az, El: Integer): boolean;
    function  GetPos(var Az, El: Integer):boolean;
    function Open(FN: string): boolean;
    procedure Close(GoHP: boolean);
    procedure DoCmd(Cmd: string);
    procedure GotoParking();
    procedure Start(AosAz, MaxAz, LosAz: double);
    procedure Stop();
  end;

//**********************************************************************//
//                                                                      //
//  �O���v�Z�̃N���X                                                    //
//                                                                      //
//**********************************************************************//
type
  TOrbit = class(TComponent)
  private
    { Private �錾 }

//  �O���v�Z(AOS,Los,Max)�v�Z�̈�
    OrbitEle_N: TOrbitalElments;           //  �q���̌��݈ʒu

    Az_d, El_d: double;

//  �ϑ��n�_�̒��Ԍv�Z�l
    SinK, CosK, SinI, CosI: double;
    SinISinK, SinICosK, CosISinK, CosICosK: double;
    N: double;                          //  �ϑ��n�_�̓������ȗ����a
    UJ, VJ, WJ: double;                 //  �ϑ��n�_��J�n�n�S�������W
    U0, V0, W0: double;                 //  �ϑ��n�_��G�n�n�S�������W

    FObsPoint: TObservationPoint;
    FKeplerEle: TOrbitalElments;
    FAosAz, FMaxAz, FMaxEl, FLosAz: double;
    FEpochRev: Integer;

    procedure CalcObservationPoint();
//    procedure SetOrbitaEle(const Value: TOrbitalElments);
  public
    { Public �錾 }
    property ObsPoint: TObservationPoint read FObsPoint;
    property KeplerEle: TOrbitalElments read FKeplerEle;
    property AosAz: double read FAosAz;
    property MaxAz: double read FMaxAz;
    property MaxEl: double read FMaxEl;
    property LosAz: double read FLosAz;
    property EpochRev: integer read FEpochRev;
    function  SetObsPoint(App: string; FileName: string): boolean;
    function  SetOrbitaEle(FileName: string; Sat: string): boolean;
    procedure CalcAosLos(DT: TDateTime);
    procedure CalcOrbit(KE: TOrbitalElments; OB: TObservationPoint; Ut: TdateTime);
  end;


//**********************************************************************//
//                                                                      //
//  Main Form�̃N���X                                                   //
//                                                                      //
//**********************************************************************//
type
  TMain = class(TForm)
    ActionList1: TActionList;
    actAboutBox: TAction;
    actExit: TAction;
    actGotoParking: TAction;
    actOptions: TAction;

    MainMenu1: TMainMenu;
    ActAboutBox1: TMenuItem;
    GS231: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;

    Panel1: TPanel;
    GroupBox1: TGroupBox;
    grpAntenna: TGroupBox;
    Label8: TLabel;
    Label10: TLabel;
    lblAntennaAz: TLabel;
    lblAntennaEl: TLabel;
    lblYMD: TLabel;
    lblAOSAz: TLabel;

    Timer1: TTimer;
    grpRotator: TGroupBox;
    Label1: TLabel;
    lblRotatorAz: TLabel;
    Label5: TLabel;
    lblRotatorEl: TLabel;
    ImgFlipMode: TImage;
    ImgOverlap: TImage;
    ImgGreen: TImage;
    ImgRed: TImage;
    grpSatellite: TGroupBox;
    Label2: TLabel;
    lblSatelliteAz: TLabel;
    Label3: TLabel;
    lblSatelliteEl: TLabel;
    btnTest: TButton;
    btnTracking: TSpeedButton;
    btnStopTracking: TSpeedButton;
    bbLeft: TSpeedButton;
    bbDown: TSpeedButton;
    bbUp: TSpeedButton;
    bbRight: TSpeedButton;
    bbOff: TSpeedButton;

    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actGotoParkingExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actProofExecute(Sender: TObject);
    procedure btnTrackingClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bbGroupeDownClick(Sender: TObject);
    procedure bbGroupUpClick(Sender: TObject);
  private
    { Private �錾 }
    Init: boolean;

    XMlIni: TXMlIniFile;
    XmlIniFile: string;
    Reg: TRegistry;
    RegKey: string;
    ParentInifile: string;          // �eApp��Inifile��
    OrbitalFile: string;            // �q�����t�@�C����
    ParentApp: string;              // �e�A�v���P�[�V������

    EpochRev, EpochRev_P: integer;  // �Ώێ���ԍ��ƑO��̎���ԍ�

    AutoTracking: boolean;

//  �q���̌��݈ʒu
    Sat, Sat_P: string;             // �Ώۉq�����ƒ��O�̉q����
    SatelliteAz: integer;
    SatelliteEl: integer;

    Orbit: TOrbit;
    function TrackingStart(): boolean;
    procedure TrackingDo;
    procedure TrackingStop;
    function ShowOptions: boolean;
    procedure SetTracking(Tracking: boolean);
  public
    { Public �錾 }
//  �A���e�i�̌��݈ʒu
    AntennaAz: integer;
    AntennaEl: integer;
    Gs232: TGs232;
    isDebug: boolean;
  end;
function DateTimeToMJD(DT: TDateTime): double;
function MJDToDateTime(MJD: Double): TDateTime;
function Kepler(U0, E: double): Double;
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
function GLToDeg(GL: String; var Lon,Lat: Double): boolean;
procedure Wait(t: integer);

const
  Pi2: double = 2 * pi;                //  2 �~ PI
  RadDivDeg: double = 2 * pi / 360;    //  1�x������̃��W�A���l

  G: Double = 6.67428E-11;             //  ���L���͂̒萔�@ m3/s2/kg
  GE: Double = 3.986005E14;            //  �n�S�d�͒萔 or �n�S���͒萔 m3/s2
  GE23: double = 42241.09773;          //  �n�S�d�͒萔 �� 2/3��

  AE: double  = 6377.397155;           //  �n���̔��a km
  EE: double  = 0.01672;               //  �n���̗��S��
  EE2: double = 0.006674372230314;     //  �n���̗��S�� �� 2��@

  cNodeMain: string ='/Main';
  cRegistry: string = '/Registry';
  cNodeOptions: string = '/Options';
  cNodeApp: string = '/Options/App';
  cNodeCom: string = '/Options/Com';
  cNodeGs232: string = '/Options/GS232';
  cInterval: string = 'Interval';
  cApp: string ='App';
  cLeft: string ='Left';
  cTop: string = 'Top';
  cTabIndex: string = 'TabIndex';
  cInifile: string = 'Inifile';
  cElements: string = 'Elements';
  cRegKey: string = 'RegKey';
  cPort: string = 'Port';
  cBaudRate: string = 'BaudRate';
  cDataBits: string = 'DataBits';
  cParity: string = 'Parity';
  cStopBits: string = 'StopBits';
  cFlowControl: string = 'FlowControl';
  cAzRotator: string = 'AzRotator';
  cElRotator: string = 'ElRotator';
  cParkingAz: string = 'ParkingAz';
  cParkingEl: string = 'ParkingEl';
  cGoParking: string = 'GoParking';
  cRotateSpeed: string = 'RotateSpeed';
  cOffset: string = 'Offset';
  cGsMode: string = 'GsMode';

var
  Main: TMain;

implementation

uses uOptions, uProof, uAbout;

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  isDebug := false;
  btnTest.Visible := isDebug;
  OutputDebugString('FormCreate');

  grpSatellite.Caption    := 'Satllite';
  lblSatelliteAz.Caption  := '';
  lblSatelliteEl.Caption  := '';
  lblAntennaAz.Caption    := '';
  lblAntennaEl.Caption    := '';
  lblRotatorAz.Caption    := '';
  lblRotatorEl.Caption    := '';
  lblYMD.Caption          := '';
  btnStopTracking.Visible := false;
  bbOff.Visible           := false;
  bbOff.Down := true;

  XmlIniFile := ChangeFileExt(Application.ExeName,'.xml');
  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeMain, true);   // Form�ʒu�Č�
    Main.Left         := XmlIni.ReadInteger(cLeft, 0);
    Main.Top          := XmlIni.ReadInteger(cTop,  0);
    if (Main.Left = 0) and (Main.Top = 0) then
      begin
      Main.Left := (Screen.Width - Main.Width)   Div 2;
      Main.Top  := (Screen.Height - Main.Height) Div 2;
      end;
  finally
    FreeAndNil(XmlIni);
  end;
  Reg     := Tregistry.Create(KEY_READ);
  Gs232   := TGs232.Create(Self);
  Orbit   := TOrbit.Create(Self);
  Init := true;      // FormActivate�̏������̂���
end;

procedure TMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  OutputDebugString('FormKeyDown');
  if Shift = [ssCtrl] then
    case key of
    word('T'): btnTrackingClick(btnTracking);
    word('U'): bbUp.OnClick(bbUp);
    word('D'): bbDown.OnClick(bbDown);
    word('L'): bbLeft.OnClick(bbLeft);
    word('R'): bbRight.OnClick(bbRight);
    end;
end;

procedure TMain.FormActivate(Sender: TObject);
begin
  OutputDebugString('FormActivate');
  if not Init then    // APP�N����1�񂾂����s����
    exit;

  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    if not XmlIni.OpenNode(cNodeCom, false) then    // ���߂ċN���������A�I�v�b�V������ʂ�\������
      while not ShowOptions do
        begin

        end;
  finally
    FreeAndNil(XmlIni);
  end;
  TrackingStart;
  Init := false;    // FormActivate�̏������̂���
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OutputDebugString('FormClose');
  TrackingStop;
  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeMain, true);
    XmlIni.WriteInteger(cLeft, Main.Left);
    XmlIni.WriteInteger(cTop, Main.Top);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;
  finally
    FreeAndNil(XmlIni);
  end;
  GS232.Close(true);
  if Reg <> Nil then
    FreeAndNil(Reg);
  if Gs232 <> Nil then
    FreeAndNil(Gs232);
  if Orbit <> Nil then
    FreeAndNil(Orbit);
end;

procedure TMain.btnTrackingClick(Sender: TObject);
begin
  OutputDebugString('btnTrackingClick');
  SetTracking(not AutoTracking);
end;

////////////////////////////////////////////////////////////////////////////
//
//  �蓮�ǔ��̃{�^�������������̏���
//  (�{�^����Tag�Ƀ{�^�����Ƃ̒l���ݒ肵�Ă���)
//  (�{�^���������ƁA���̃{�^����Cancel=TRUE�ɂȂ�)
//
///////////////////////////////////////////////////////////////////////////
procedure TMain.bbGroupUpClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  OutputDebugString('bbGroupUpClick');
  bbOff.Down  := true;
  TSpeedButton(Sender).OnClick := bbGroupeDownClick;
  for i := 0 to 3 do       // �S�{�^�����g�p�\�ɂ���
    GroupBox1.Controls[i].Enabled := true;
  if TSpeedButton(Sender).Tag < 2 then
    s := 'A'              // Az��]��~
  else
    s := 'E';             // El��]��~
  Gs232.DoCmd(s);
end;

procedure TMain.bbGroupeDownClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  OutputDebugString('bbGroupeDownClick');
  TSpeedButton(Sender).OnClick := bbGroupUpClick;
  for i := 0 to 3 do        // Click���ꂽ�{�^���ȊO���g�p�s�\�ɂ���
    if GroupBox1.Controls[i].Tag <> TSpeedButton(Sender).Tag then
      begin
      GroupBox1.Controls[i].Enabled := false;
      end;
  case TSpeedButton(Sender).Tag of
    0: s := 'L';            // Left��]
    1: s := 'R';            // Right��]
    2: s := 'U';            // Up��]
    3: s := 'D';            // Down��]
    end;
  Gs232.DoCmd(s);
end;

procedure TMain.Timer1Timer(Sender: TObject);
begin
  TrackingDo;
end;

procedure TMain.actAboutBoxExecute(Sender: TObject);
var
  AboutBox: TAboutBox;
begin
  AboutBox := TAboutBox.Create(self);
  try
    AboutBox.ShowModal;
  finally
    FreeAndNil(AboutBox);
  end;
end;

procedure TMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.actGotoParkingExecute(Sender: TObject);
begin
  Gs232.GotoParking();
end;

//  �Ƃ肠�����g��Ȃ�
procedure TMain.actProofExecute(Sender: TObject);
var
  Proof: TfrmProof;
begin
//  Timer1.Enabled := False;
  Proof := TfrmProof.Create(self);
  try
    if Proof.ShowModal = mrOK then
      begin

      end;
  finally
    FreeAndNil(Proof);
  end;
end;

procedure TMain.actOptionsExecute(Sender: TObject);
begin
  TrackingStop;
  if ShowOptions then
    begin

    GS232.Close(false);
    Init := true;
    TrackingStart;
    end;
end;

function TMain.ShowOptions(): boolean;
//var
//  Options: TOptions;
begin
  result := false;
//  Options := TOptions.Create(self);
  try
    if Options.ShowModal = mrOK then
      begin
      result := true;
      end;
  finally
//    FreeAndNil(Options);
  end;
end;

procedure TMain.btnTestClick(Sender: TObject);
begin
  Timer1.Enabled := false;

//Case�@1
//  Gs232.Start(208,269,332);
//  gs232.Move(210, 2);
//  gs232.Move(269, 32);
//  gs232.Move(330, 2);

//Case�@2
//  Gs232.Start(332,269,208);
//  gs232.Move(330, 2);
//  gs232.Move(269, 32);
//  gs232.Move(210, 2);

//Case�@3
//  Gs232.Start(160,40,355);
//  gs232.Move(155, 2);
//  gs232.Move(40, 32);
//  gs232.Move(358, 2);

//Case�@4
//  Gs232.Start(355,40,160);
//  gs232.Move(358, 2);
//  gs232.Move(40, 32);
//  gs232.Move(155, 2);
end;

//////////////////////////////////////////////////////////////////////////
//
//  �����ǔ��̏���
//
//////////////////////////////////////////////////////////////////////////
function TMain.TrackingStart(): boolean;
begin
  OutputDebugString('TrackingStart');
  result := true;
  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeMain, true);
    Timer1.Interval   := XmlIni.ReadInteger(cInterval, 1000);
    XmlIni.OpenNode(cNodeApp, true);
    ParentApp         := XmlIni.ReadString(cApp, 'CALSAT32');
    ParentInifile     := XmlIni.ReadString(cInifile, '');
    OrbitalFile       := XmlIni.ReadString(cElements, '');
    RegKey            := XmlIni.ReadString(cRegKey, '');
  finally
    FreeAndNil(XmlIni);
  end;

//  �ϑ��n�_�̃f�[�^��ǂݍ���
  if not Orbit.SetObsPoint(ParentApp, ParentInifile)  then
    begin
    MessageDlg('�eAPP��Inifile���ǂ߂܂���', mtError, [mbOK], 0);
    result := false;
    Exit;
    end;

  reg.RootKey := HKEY_CURRENT_USER;
  if not Reg.OpenKey(RegKey, False) then
    begin;
    MessageDlg('CALSAT32�̃��W�X�g�����ǂ߂܂���', mtError, [mbOK], 0);
    Exit;
    end;

  if not GS232.Opend then
    begin
    if not GS232.Open(XmlIniFile) then
      begin
      Timer1.Enabled := false;
      exit;
      end;
    end;

  SetTracking(false);                // button������������
  Timer1.Enabled := True;            // ���̑���ŁA�ǔ����n�߂�
end;

procedure TMain.SetTracking(Tracking: boolean);
begin
  OutputDebugString('SetTracking');
  AutoTracking := Tracking;
  if Tracking then          //  �����ǔ����ɉ����ꂽ
    begin
    btnTracking.Caption := '&Tracking Off';
    btnTracking.Down    := true;
    bbUp.Enabled    := false;
    bbDown.Enabled  := false;
    bbLeft.Enabled  := false;
    bbRight.Enabled := false;
//    btnTracking.Font.Color := clWindowText;
    GS232.Start(Orbit.AosAz, Orbit.MaxAz, Orbit.LosAz); //AOS.Los���珉�ʒu���v�Z����
    Timer1.Enabled  := true;
    end
  else                          //  �����ǔ��łȂ����ɉ����ꂽ
    begin
    btnTracking.Caption := '&Tracking On';
    btnStopTracking.Down:= true;
    bbUp.Enabled    := true;
    bbDown.Enabled  := true;
    bbLeft.Enabled  := true;
    bbRight.Enabled := true;
    if GS232.Opend then
      GS232.Stop;
    end;
end;

procedure TMain.TrackingStop();
begin
  OutputDebugString('TrackingStop');
  SetTracking(false);
  Timer1.Enabled := false;
end;

//  Timer1�ɂ��A1�b��1�񏈗������
procedure TMain.TrackingDo();
var
  i: integer;
  s: string;
  nw: TDateTime;
begin
  OutputDebugString('TrackingDo');

//  Calsat������擾�E�\��
  Sat := Reg.ReadString('Satellite');   //  �ǔ��Ώۉq�������擾
  i   := Pos('[', Sat);                 //  �q����[Mode]�̉q�����݂̂�I�����邽��
  if i <> 0 then
    Sat := Copy(Sat, 1, i - 1);
    if ParentApp = 'CALSAT32' then
      EpochRev := StrToInt(Reg.ReadString('Revolution'))   //  CALSAT32�̎��A����ԍ����擾
    else
      EpochRev := 0;

//  ���ݓ��t�E������\��
  Nw      := MJDtoDateTime(StrToFloat(Reg.ReadString('MJD')));
  DateTimeToString(s, 'yy/mm/dd hh:nn:ss', Nw + Orbit.ObsPoint.TimeOffset);
  lblYMD.Caption := s + '(' + Orbit.ObsPoint.LocalTimeStr + ')';

//  �ǔ��Ώۉq���̌���Az,El��\��
  grpSatellite.Caption    := Sat;
  SatelliteAz   := round(StrToFloat(Reg.ReadString('Azimuth')));
  SatelliteEl   := round(StrToFloat(Reg.ReadString('Elevation')));
  lblSatelliteAz.Caption  := IntToStr(SatelliteAz)   + '��';
  lblSatelliteEl.Caption  := IntToStr(SatelliteEl) + '��';

  if (Sat <> Sat_P) or (EpochRev <> EpochRev_P) then  //  �ǔ��Ώۉq�����ς������
    begin
    Sat_P       := Sat;
    EpochRev_P  := EpochRev;
    if AutoTracking then                   //   CALSAT32�̉q�����ς������A�����ǔ����~����
      SetTracking(false);
    Orbit.SetOrbitaEle(OrbitalFile, Sat);    //  AOS,LOS,MAX��Az���v�Z
    Orbit.CalcAosLos(Nw);
    s := format('(Aos)Az=%3.0f  (Max)Az=%3.0f El=%2.0f  (Los)Az=%3.0f', [Orbit.AOSAz, Orbit.MaxAz, Orbit.MaxEl, Orbit.LOSAz]);
    lblAOSAz.Caption := s;
    end;

//  GS232����A���e�i��Az,El���擾
  if Gs232.Opend and GS232.GetPos(AntennaAz, AntennaEl) then      // AntAz�AAntEl�ɃA���e�i�̌��݈ʒu���A��
    begin
    lblAntennaAz.Caption  :=  IntToStr(AntennaAz) + '��';
    lblAntennaEl.Caption  :=  IntToStr(AntennaEl) + '��';
    lblRotatorAz.Caption  :=  IntToStr(Gs232.RotatorAz) + '��';
    lblRotatorEl.Caption  :=  IntToStr(Gs232.RotatorEl) + '��';
    if Gs232.RotatorAz > 360 then
      ImgOverlap.Picture := ImgRed.Picture
    else
      ImgOverlap.Picture := ImgGreen.Picture;
    if Gs232.RotatorEl > 90 then
      ImgFlipMode.Picture := ImgRed.Picture
    else
      ImgFlipMode.Picture := ImgGreen.Picture;
    end
  else
    begin
    lblAntennaAz.Caption  := '';
    lblAntennaEl.Caption  := '';
    lblRotatorAz.Caption  :=  '';
    lblRotatorEl.Caption  :=  '';
    if AutoTracking then                   //   Error�Ȃ�A�����ǔ����~����
      SetTracking(false);
    end;

//  �����ǔ��łȂ��Ȃ�I��
  if NOT AutoTracking then
    exit;

//  �A���e�i�Ɖq���̕����������Ă���Ȃ牽���������Ȃ�
//  �q���̕����ɃA���e�i�𓮂���
  if (SatelliteAz = AntennaAz) and (SatelliteEl = AntennaEl) then
    exit;
  if not GS232.Move(SatelliteAz, SatelliteEl)  then
    SetTracking(false);
end;

//////////////////////////////////////////////////////////////////////////
//
//    �Q�l����
//      �P�D�V�̂̈ʒu�v�Z �����   ���ҁF����H���@�@���s�F�n�l����
//      2.  Calsat32�̃z�[���؁[�W  ��ҁF���c�������iJR1HUO�j
//             http://homepage1.nifty.com/aida/jr1huo_calsat32/index.html
//
//////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//
//  �ϑ��n�_�̃f�[�^��ݒ肷��
//
//////////////////////////////////////////////////////////////////////////
function TOrbit.SetObsPoint(App: string; FileName: string): boolean;
var
  Ini: TIniFile;
  GridLoc: string;
begin
  result := true;
//  �t�@�C���̑��݊m�F
  if not FileExists(FileName) then
    begin
    result := false;
    MessageDlg(FileName + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;

//  �ϑ��n�_�̃f�[�^�ǂݍ���
  Ini :=TIniFile.Create(FileName);
  try
    with FObsPoint do
      begin
      if APP = 'CALSAT32' then
        begin
        Latitude      := DegToRad(Ini.ReadFloat('MYQTH', 'MyIdo', 0));     //
        Longitude     := DegToRad(Ini.ReadFloat('MYQTH', 'MyKeido', 0));  //
        Height        := Ini.ReadFloat('MYQTH', 'MyTakasa', 0) / 1000;           //  �P�ʁFkm
        TimeOffset    := Ini.ReadFloat('TIMEOFFSET', 'TimeOffset', 9) / 86400;  //  ���P�ʂ̎���
        LocalTimeStr  := Ini.ReadString('TIMEOFFSET', 'ChrLST', 'JST');
        end
      else if APP = 'CALSAT99' then
        begin
        GLToDeg(GridLoc, Longitude, Latitude);
        Latitude      := DegToRad(Latitude);
        Longitude     := DegToRad(Longitude);
        Height        := 0;
        TimeOffset    := 9;
        LocalTimeStr  := 'JST';
        end
      else if APP = 'KSAT' then
        begin
        Latitude      := DegToRad(Ini.ReadFloat('POSITION', 'Ido', 0));   //
        Longitude     := DegToRad(Ini.ReadFloat('POSITION', 'Keido', 0)); //
        Height        := Ini.ReadFloat('POSITION', 'TAKASA', 0) / 1000;   //  �P�ʁFkm
        TimeOffset    := Ini.ReadFloat('JIKAN', 'JISA', 9) / 86400;  //  ���P�ʂ̎���
        if TimeOffset >= 0 then
          LocalTimeStr  := 'UTC+' + FloatToStr(TimeOffset)
        else
          LocalTimeStr  := 'UTC' + FloatToStr(TimeOffset);
        end;

//  �e�X�g�p�f�[�^(1)(2)(3)
{
      Latitude      := DegToRad(35.7760287);     //   �ܓx
      Longitude     := DegToRad(139.634385);     //   �o�x
      Height        := 0;                        //   �����@�P�ʁFkm
      TimeOffset    := 9/24;
      LocalTimeStr  := 'JST';
}
      CalcObservationPoint;
      end;
  finally
    Ini.Free;
  end;

end;

///////////////////////////////////////////////////////////////////////////
//
//  �ϑ��n�_�̃f�[�^���v�Z����
//
///////////////////////////////////////////////////////////////////////////
procedure TOrbit.CalcObservationPoint();
var
  H, SinLat, CosLat, SinLon, CosLon: double;
begin
  with FObsPoint do
    begin
    SinK := Sin(Longitude);
    CosK := Cos(Longitude);
    SinI := Sin(Latitude);
    CosI := Cos(Latitude);
    SinICosK := SinI * CosK;
    SinISinK := SinI * SinK;
    CosICosK := CosI * CosK;
    CosISinK := CosI * SinK;

//  �������ȗ����a���v�Z (P184)
    N := AE / SQRT(1 - EE2 * Power(sin(Latitude), 2));

//  �ϑ��_��J�n�������W�n�֕ϊ� (P185)
    H := N + Height;
    SinLat := sin(Latitude);
    CosLat := cos(Latitude);
    SinLon := sin(Longitude);
    CosLon := cos(Longitude);
    UJ := H * cosLat * cosLon;
    VJ := H * cosLat * sinLon;
    WJ := (N * (1 - EE2) + Height) * sinLat;

//  �ϑ��_��G�n�������W�n�֕ϊ� (P188)
    U0 := UJ - 0.136;
    V0 := VJ + 0.521;
    W0 := WJ + 0.681;
  end;
end;

///////////////////////////////////////////////////////////////////////////
//
//  �T�e���C�g������NASA 2���C����ǂݍ���
//  �O���v�f�f�[�^��ݒ肷��B
//
///////////////////////////////////////////////////////////////////////////
function TOrbit.SetOrbitaEle(FileName: string; Sat: string): boolean;
var
  F: TextFile;
  Line1: string;
  Line2: string;
  s: string;
  i: integer;
begin
  result := true;

//  �t�@�C���̑��݂��m�F
  if not FileExists(FileName) then
    begin
    result := false;
    MessageDlg(FileName + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;

//  �O���v�f�̓ǂݍ���
  Line1 := '';
  Line2 := '';
  try
    AssignFile(F, FileName);   { File selected in dialog }
    Reset(F);
    while not Eof(F) do
      begin
      Readln(F, S);            { Read first line of file }
      if trim(s) = Sat then
        begin
        Readln(F, S);          { Read first line of file }
        Line1 := S;            { Put string in a TEdit control }
        Readln(F, S);          { Read first line of file }
        Line2 := S;            { Put string in a TEdit control }
        break;
        end;
      end;
    CloseFile(F);

    if (Line1 = '') or (line2 = '') then
      begin
      MessageDlg('ELEM.Txt Invalid format.', mtError, [mbOk], 0);
      result := false;
      Exit;
      end;

//  �e�X�g�p�f�[�^
//    FO-29
//    Line1 := '1 24278U 96046B   03232.95303700 -.00000056 00000-0 -24068-4 0 06270';
//    Line2 := '2 24278 098.5660 359.5477 0351513 101.3348 262.7487 13.52885302346104';
//

//  �f�[�^�͌ʓx�l(Radian)�ɕϊ�
    with FKeplerEle do
      begin
      i := StrToInt(Copy(Line1, 19, 02));
      if i > 70 then
        Epoch_Y     := i + 1900
      else  ;
        Epoch_Y     := i + 2000;
      Epoch_D       := StrToFloat(Copy(Line1, 21, 12));
      Epoch         := StrToDate(IntToStr(Epoch_Y) + '/1/1') + Epoch_D -1;
      Epoch_MJD     := DateTimeToMJD(Epoch);
      Inclination   := DegToRad(StrToFloat(Copy(Line2, 09, 08)));
      Eccentricity  := StrToFloat('0.' + Copy(Line2, 27, 07));
      RAAN          := DegToRad(StrToFloat(Copy(Line2, 18, 08)));
      Argp          := DegToRad(StrToFloat(Copy(Line2, 35, 08)));
      MeanAnomaly   := DegToRad(StrToFloat(Copy(Line2, 44, 08)));
      MeanMotion    := StrToFloat(Copy(Line2, 53, 11)) * Pi2;     //  Rad/Day
      DecayRate     := StrToFloat(Copy(Line1, 34, 10)) * Pi2;
      n2            := 0;
      EpochRev      := StrToInt(Copy(Line2, 64, 05));
      MajorAxis     := GE23 / power(MeanMotion / Pi2,  2/3);
      end;
  except
//    MessageDlg('ELEM.Txt Invalid format.', mtError, [mbOk], 0);
    result := false;
    Exit;
  end;
end;

procedure TOrbit.CalcAosLos(DT: TDateTime);
var
  UT: Tdatetime;
  i: integer;
  UpDown: boolean;
  Az, El: double;
const
  Sec: double = 1 / 1440;    // 1 / 24 / 60  1�b/��
begin
  FAosAz := 0; FMaxAz := 0; FLosAz := 0;
  FMaxEl := 0;
  UpDown := True;
  Ut := DT;
  for i := 0 to 3000 do       // 3000��(5����)��܂łōŏ���AOS�����v�Z����
    begin
    Ut := Ut + Sec;
    CalcOrbit(FKeplerEle, FObsPoint, Ut);
    Az := Trunc(Az_d);
    El := Trunc(EL_d);
    if El > FMaxEl then   //  Max El�̔��f
      begin
      FMaxEl := El;
      FMaxAz := Az;
      FEpochRev := OrbitEle_N.EpochRev;
      end;
    if UpDown then           //  AOS�̔��f
      begin
      if (FAosAz = 0) and (El >= 0) then
        begin
        FAosAz := Az;
        UpDown := false;
        end
      end
    else
      begin                     //  LOS�̔��f
      if (FLosAz = 0) and (El < 0) then
        begin
        FLosAz := Az;
        Exit;
        end;
      end;
    end;
end;

procedure TOrbit.CalcOrbit(KE: TOrbitalElments; OB: TObservationPoint; Ut: TdateTime);
var
  t, t2, Gt: double;            // �o�ߎ��ԁA�O���j�b�W�P����
  SinGt, CosGt: double;
  U, sinU, cosU: double;
  E, ED: double;
  r, x, y: double;
  ASs, Bss, Css: double;
  Us, Vs, Ws: double;
  Ud, Vd, Wd, Rd, Xd, Yd, Zd: double;
  Az, El: double;
  s: string;
  mm: double;
  w: double;
//  �q���̒��Ԍv�Z�l
  SinRAAN, CosRAAN, SinIncl, CosIncl, SinARGP, cosARGP: double;
  ASsX, ASsY, BSsX, BSsY, CSsX, CSsY: double;
begin
  with OrbitEle_N do
    begin

//  �o�ߎ��Ԃ��v�Z
    t  := (Ut - KE.Epoch);
    t2 := t * t;

//  �ϑ������̋O���v�f�v�Z
    Epoch             := KE.Epoch + t;
    Epoch_MJD         := DateTimeToMJD(FKeplerEle.Epoch);
    Inclination       := KE.Inclination;
    Eccentricity      := KE.Eccentricity;
    MeanMotion        := KE.MeanMotion + KE.DecayRate * t / 2;      // ���ω^�� (Rad/Day)
    MajorAxis         := GE23 / Power(MeanMotion / Pi2,  2/3);      // �����a
    W := 0.174 / Power((MajorAxis / AE), 3.5) * t;
    Argp              := KE.Argp + W * (2 -2.5 * SQR(Sin(Inclination)));
    Raan              := KE.Raan - W * Cos(Inclination);
    MeanAnomaly       := KE.MeanAnomaly + MeanMotion * t
                       + KE.DecayRate * t2 / 2
                       + KE.N2 * t2 * t / 3;
    MeanAnomaly       := Frac(MeanAnomaly / Pi2) * Pi2; // 360�x�ȓ��ɂ܂�߂�
                                                  // M or l:  ���ϋߓ_�p(Rad)
    DecayRate         := 0;
    n2                := 0;
//    EpochRev          := Trunc(KE.EpochRev + KE.MeanMotion * t);
    EpochRev          := Trunc(KE.EpochRev + (KE.MeanMotion / Pi2) * t);

//  �P�v���[�̕������ŕ��ϋߓ_�p�A���S�����痣�S�ߓ_�p(U)�����߂�(P147)
    U := Kepler(MeanAnomaly, Eccentricity);

//  �O���ʓ���X,Y���W�����߂� (P148)
    SinU := sin(U);
    CosU := cos(U);
    r := MajorAxis * (1 - Eccentricity * CosU);
    x := MajorAxis * (cosU - Eccentricity);
    y := MajorAxis * Sqrt(1 - Eccentricity * Eccentricity) * SinU;

//  �n�S�ԓ��������W�ɕϊ����邽�߂̎��O�v�Z
    SinRAAN   := Sin(Raan);
    CosRAAN   := Cos(Raan);
    SinIncl   := Sin(Inclination);
    CosIncl   := Cos(Inclination);
    SinARGP   := Sin(Argp);
    CosARGP   := Cos(Argp);
    ASsX      := CosRAAN * CosARGP - SinRAAN * CosIncl * SinARGP;
    ASsY      := CosRAAN * SinARGP + SinRAAN * CosIncl * CosARGP;
    BSsX      := SinRAAN * CosARGP + CosRAAN * CosIncl * SinARGP;
    BSsY      := SinRAAN * SinARGP - CosRAAN * CosIncl * CosARGP;
    CSsX      := SinIncl * SinARGP;
    CSsY      := SinIncl * CosARGP;

//  �n�S�ԓ��������W�ɕϊ� (P196)
    ASs := x * ASsX - y * ASsY;
    BSs := x * BSsX - y * BSsY;
    CSs := x * CSsX + y * CSsY;

//  �O���j�b�W�P���������߂�
    Gt := UtToSiderealTime(Ut, 0);  // ���o0�x�̍P���� ���ʂ́A�����ȉ��ŕԂ��Ă���
    Gt := Gt * Pi2;                 //  Radian�ɕϊ�����

//  G�n�n�S���𒼌����W�n�ɕϊ����� (P197)
    SinGt := Sin(Gt);
    CosGt := Cos(Gt);
    Us :=   ASs * CosGt + BSs * SinGt;
    Vs := - ASs * SinGt + BSs * CosGt;
    Ws :=   CSs;

//  G�n���S�������W�n�֕ϊ�  (P198)
    Ud := Us - U0;
    Vd := Vs - V0;
    Wd := Ws - W0;

//  �n���������W�n�ւ̕ϊ� (P199)
    Xd :=  Ud * SinICosK + Vd * SinIsinK - Wd * CosI;
    Yd := -Ud * SinK + Vd * cosK;
    Zd :=  Ud * CosICosK + Vd * CosISinK + Wd * SinI;
    Rd := Sqrt(Sqr(Xd) + Sqr(Yd) + Sqr(Zd));   //  �q���܂ł̋���

//  ���ʊp�ƍ��x�ɕϊ� (P199)
    Az := ArcTan2(Yd, -Xd);
    if Az < 0 then
      Az := Az + Pi2;
    El := Arcsin(Zd / Rd);

    Az_d := RadToDeg(Az);
    El_d := RadToDeg(El);
    end;
end;

///////////////////////////////////////////////////////////////////////////
//
//  Comm�|�[�g�̏���
//
///////////////////////////////////////////////////////////////////////////
function TCommX_Ex.Open(FN: string): boolean;
//procedure TCommX_Ex.Open;
begin
  result := True;
  XmlIni := TXmlIniFile.Create(FN);
  CPortX := TCPortX.Create(self);
  try
    XmlIni.OpenNode(cNodeCom, true);
    CPortX.Port          := XmlIni.ReadString(cPort, 'COM3');
    CPortX.BaudRate      := XmlIni.ReadString(cBaudRate, '9600');
    CPortX.DataBits      := XmlIni.ReadString(cDataBits, '8');
    CPortX.Parity        := XmlIni.ReadString(cParity, 'NONE');
    CPortX.StopBits      := XmlIni.ReadString(cStopBits, '1');
    CPortX.FlowControl   := XmlIni.ReadString(cFlowControl, 'NONE');
    try
      CPortX.Open;
    except
      MessageDlg(CPortX.Port + '��Open�ł��܂���', mtInformation, [mbOk], 0);
      result := False;
    end;
  finally
    FreeAndNil(XmlIni);
  end;
end;

procedure TCommX_Ex.Close();
begin
  CportX.Close;
  FreeAndNil(CportX);
end;

function TCommX_Ex.Recv(MaxLen: integer):string;
var
  s : string;
begin
  Result := '';
  s := CportX.RecvStr(MaxLen);
  try
    Result := s;
  except
    Result := '';
  end;
end;

procedure TCommX_Ex.Send(str: string);
begin
  CportX.SendStr(str + #13);
end;

procedure TCommX_Ex.Clear();
begin
  CPortx.Clear;
end;

///////////////////////////////////////////////////////////////////////////
//
//  GS-232
//
///////////////////////////////////////////////////////////////////////////
constructor TGs232.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  CommX_Ex  := TCommX_Ex.Create(self);
  FOpend    := false;
  FOverlap  := false;
  FFlip     := false;
end;

destructor TGs232.Destroy;
begin
  FreeAndNil(CommX_Ex);
  inherited Destroy;
end;

procedure TGs232.OnPortException(Sender:TObject; TComException:TComExceptions; ComportMessage:String;
                          WinError:Int64; WinMessage:String);
var
  s: string;
begin
    s := 'Comport error, confirm Options';
    MessageDlg(s, mtError, [mbOK], 0);
    Exit;
end;

function TGs232.Open(FN: string): boolean;
begin
  result := true;
  XmlIniFile    :=  FN;
  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeGS232, true);
    FMode         := TGsMode(XMLIni.ReadInteger(cGsMode, Integer(gsNone)));
    ParkingAz     := XMLIni.ReadInteger(cParkingAz, 0);
    ParkingEl     := XMLIni.ReadInteger(cParkingEl, 0);
    GoParking     := XMLIni.ReadBool(cGoParking, true);
    RotateSpeed   := XMLIni.ReadInteger(cRotateSpeed, 1);
    ParkingAz     := XMLIni.ReadInteger(cParkingAz, 0);
    OffSet        := XMLIni.ReadInteger(cOffSet, 0);
  finally
    FreeAndNil(XmlIni);
  end;

  CommX_Ex.OnException :=  OnPortException;
  if CommX_Ex.Open(FN) then
    begin
    CommX_Ex.Clear;
    CommX_Ex.Send('');         // GS-23��Buffer���N���A����
    CommX_Ex.Clear;
    CommX_Ex.Send('S');        // ����̒�~
    CommX_Ex.Send(format('X%.1d', [RotateSpeed + 1]));        // �X�s�[�h�̐ݒ�@1:�ᑬ 2:����1 3�F����2 4�F����
    FOpend := True;
    end
  else
    begin
    result := false
    end;
end;

procedure TGs232.DoCmd(Cmd: string);
begin
  CommX_Ex.Send(CMD);
end;

procedure TGs232.Start(AosAz, MaxAz, LosAz: double);
var
  Aos, Max,Los: integer;
begin
  Aos := trunc(AosAz) - Offset;
  Max := trunc(MaxAz) - Offset;
  Los := trunc(LosAz) - Offset;

  Aos := Aos Mod 360;
  Max := Max Mod 360;
  Los := Los Mod 360;
  FZeroPathOrbit :=  boolean(sign(Aos - Max)) xor boolean(sign(Max - Los));
  FisCW := (Max > AOS);   //  ���v���/�����v����
  isAos := false;
  isLos := false;
end;

function TGs232.Move(Az, El: Integer): boolean;
var
  s: string;
  D: integer;
  nAz,nEl: integer;
begin
  result := true;
  if isLOS and (El <= 0) then
    begin
    result := false;
    exit;
    end;
  if (Az = Az_p) and (El = El_p) then
    Exit;

  nAZ := (AZ - OffSet) mod 360;
  if (not isAOS)  and (EL <= 0) then
    nEl := 0
  else
    begin
    nEl := El;
    isAos := true;
    end;

  if ZeroPathOrbit then
    case mode of
    gsOverlap:
      begin
      D := trunc(nAz / 90);         //Az�̏ی��𔻒f
      if (D = 0) then
        nAz := nAz + 360
      end;
    gsFlip:
      begin
      nAZ := nAZ + 180;
      nAz := nAZ mod 360;
      nEl := 180 - nEl;
      end;
    end;

  if isAOS and (El <= 0) then           //  �����ǔ��I��
    isLOS := true;

  s := format('W%.3d %.3d', [nAz, nEl]);
  CommX_Ex.Send(s);
  Az_p := Az;
  El_p := El;
end;

procedure TGs232.Stop();
begin
  CommX_Ex.Send('S');
end;

procedure TGs232.GotoParking();
var
  s1, s2: string;
begin
  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeGS232, true);
    ParkingAz     := XMLIni.ReadInteger(cParkingAz, 0);
    ParkingEl     := XMLIni.ReadInteger(cParkingEl, 0);
  finally
    FreeAndNil(XmlIni);
  end;
  s1 := copy(IntToStr(ParkingAz - OffSet + 1000), 2,3);
  s2 := copy(IntToStr(ParkingEl + 1000), 2,3);
  CommX_Ex.send('W' + s1 + ' ' + s2);
end;

function TGs232.GetPos(var Az, El: integer):boolean;
var
  s,m: string;
  e1,e2: boolean;
  i, j: integer;
begin
  result := false;
  try
    CommX_Ex.Send('C2');                   // �����p�x�A�p�̓ǂݎ��w��
///////////////////////////////////////////////////////////////////////////////
///   �P��ڂ�Send�̎��ACrLf�����Ԃ��Ă��Ȃ�
///   �Q��ڈȍ~�͐���ɕԂ��Ă���
///    wait(10);                     //  ���Ă݂����A�ς�炸
///  AZ���܂� ���� 12���ȊO�̉����͖�������悤�ɏC������ 2018-12-24
    s := CommX_Ex.Recv(15);
    FAntennaAz := 0;
    FAntennaEl := 0;
    FRotatorAz := 0;
    FRotatorEl := 0;
    i := Pos('AZ=', s);
    if i <> 0 then                                      // ������AZ=xxx EL=yyyCrLf (15��)
      begin
      j := Pos('EL=', s);
      e1 := TryStrToInt(Trim(Copy(s,i+3, 3)), FRotatorAz);
      e2 := TryStrToInt(Trim(Copy(s,j+3, 3)), FRotatorEl);
      if not e1 or not e2  then
        begin
        raise EConvertError.Create(s);
        exit;
        end;
      end
    else if length(s) = 12 then
      begin
      e1 := TryStrToInt(Copy(s,3,3), FRotatorAz);       // ������+xxxx+yyyyCrLf (12��)
      e2 := TryStrToInt(Copy(s,8,3), FRotatorEl);
      if not e1 or not e2  then
        begin
        raise EConvertError.Create(s);
        exit;
        end;
      end;
    FAntennaAz := (FRotatorAz + Offset + 360) Mod 360;
    if FRotatorEl > 90 then
      begin
      FAntennaAz   := (FAntennaAz + 180) Mod 360;
      FAntennaEl   := 180 - RotatorEl;
      end
    else
      FAntennaEl   := RotatorEl;
    Az := AntennaAz;
    El := AntennaEl;
    result := true;
  except
    m := 'Error =' + s;
    MessageDlg(m, mtInformation, [mbOk], 0);
    Az := 999;
    El := 999;
  end;
end;

procedure TGs232.Close(GoHP: boolean);
begin
  if not FOpend then
    exit;

  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cNodeGS232, true);
    GoParking     := XMLIni.ReadBool(cGoParking, true);
  finally
    FreeAndNil(XmlIni);
  end;

  if GoHp and GoParking then
    begin
    GotoParking;
    wait(500);                     //  �z-���ʒu�ɖ߂��܂ł̒ʐM���Ԋm��
    end;
  CommX_Ex.Send('');
  CommX_Ex.Clear;
  CommX_Ex.Close;
  FOpend := false;
end;

///////////////////////////////////////////////////////////////////////////
//
//  Delphi �� TDateTime �l�����i�C���j�����E�X���iModeified Julian Day;MJD)�j�ɕϊ�����B
//
//  Delphi �� TDateTime �l�̐������͐��� 1899 �N 12 �� 30 ������̌o�ߓ����������܂��B
//  �������͂��̓��̌o�ߎ��ԁi24 ���Ԑ��j�ł��B
//
//  �������A�X���́A1858�N11��17��0�����N�Z�J�n���Ƃ����ʎZ��
//  �������A�X���@���@�����A�X�� �|�@2400000.5�@�̊֌W������
//
//  �ʏ� 1899�N12��30�� �ȑO���v�Z�ΏۂƂ��Ȃ���΁A����MJD�ɕϊ�����K�v�Ȃ�
//  1899�N12��30���ȑO�́ATDateTime�𒲂ׂȂ���Ε�����Ȃ� (���� ���v)
//
///////////////////////////////////////////////////////////////////////////
function DateTimeToMJD(DT: TDateTime): double;
var
  Y, M, D: word;
  MJD: double;
begin
  DecodeDate(DT, Y, M, D);
  if M <= 2 then
    begin
    M := M + 12;
    Y := Y - 1;
    end;
  MJD := int(365.25 * Y) + int(y / 400) - int(y / 100) + int(30.59 * (m - 2)) + d - 678912
       + Frac(DT);
  result := MJD;
end;

function MJDToDateTime(MJD: Double): TDateTime;
begin
  result := MJD - 15018;
end;

///////////////////////////////////////////////////////////////////////////
//
//  �P�v���[�̕������ŗ��S�ߓ_�p(U)�����߂�
//
///////////////////////////////////////////////////////////////////////////
function Kepler(U0, E: double): Double;
var
  U, UD: double;
begin
  U := U0;
  Repeat
    UD := (U0 - U + E * sin(U)) / (1- E * cos(U));
    U := U + UD
  Until IsZero(UD);
  result := U;
end;

///////////////////////////////////////////////////////////////////////////
//
//  Ut����O���j�b�W���ύP�����ɕϊ�����B (P27,151)
//
///////////////////////////////////////////////////////////////////////////
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
var
  Y,M,D: Integer;
  Lt, K, TU, Tg0: Double;
begin
//  1899/12/31 12:00 �k�̌o�ߓ���(K)�����߂�
//  ���̎���K�p�ł���̂́A1900/3/1�`2100/2/28�̊��Ԃ̂�
  Y := YearOf(Ut) - 1900;
  M := MonthOf(Ut);
  D := DayOf(Ut);
  if M < 3 then
    begin
    Y := Y - 1;
    M := M + 2;
    end;
  K := 365 * Y + 30 * M + D - 33.5 + Int(0.6 * (M + 1)) + Int(Y / 4);
//  1899/12/31 12:00 �k�̌o�ߓ���(K)��,36525����1�Ƃ����P�ʁiTU�j�����߂�
  TU := K / 36525;
//  UT 00:00�̃O���j�b�W�P���������߂�i24���Ԉȏ�́A�Ō�ɐ؂�̂ĂĂ���j
  Tg0 := (23925.836 + 8640184.542 * TU + 0.0929 * TU * TU) / 86400;
//  ���߂鎞���̃O���j�b�W�P���������߂�
//  ���ʂ́A���t�`���ł���̂ŁA�����ȉ��̌��̂ݗL��
  Lon := Lon / Pi2;
  Lt := Frac(Ut);
  result := frac(Tg0 + Lon + Lt + 0.00273791 * Lt);
end;

function GLToDeg(GL: String; var Lon,Lat: Double): boolean;
var
  s: string;
  w1,w2,w3: byte;
begin
//    Lon := 0;
//    Lat := 0;
//    if not CheckGridLoc(GL) then
//      begin
//      Result := false;
//      exit;
//      end;

    s := GL;
    if length(GL) = 4 then
      s := s + 'AA';
    w1 := ord(s[1]) - 65;
    w2 := ord(s[3]) - 48;
    w3 := ord(s[5]) - 65;
    Lon := w1 * 20 + w2 * 2 + (w3 / 12) - 180;

    w1 := ord(s[2]) - 65;
    w2 := ord(s[4]) - 48;
    w3 := ord(s[6]) - 65;
    Lat := w1 * 10 + w2 + (w3 / 24 ) - 90;
    Result := True;
end;

///////////////////////////////////////////////////////////////////////////
//
//  Appn��Wait��������
//
///////////////////////////////////////////////////////////////////////////
procedure Wait(t: integer);
var
  endtime : TDateTime; //�I������
begin
  endtime := Now + t / 86400 / 1000;
//�@�@while (Now < endtime) do ;
  while (Now < endtime) do
    Application.ProcessMessages;
end;

end.

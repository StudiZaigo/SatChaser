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
//  例外通知用のクラス                                                  //
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
//  衛星の軌道要素クラス                                                //
//                                                                      //
//**********************************************************************//
  TOrbitalElments = record
    Satellite: String;         // 衛星名
    Epoch: TDateTime;          // 元期
    Inclination: double;       // i:  軌道傾斜角（Rad)       Inclination
    Eccentricity: double;      // e:  離心率                 Eccentricity
    MeanAnomaly: double;       // M or l:  平均近点角(Rad)   Mean Anomaly
    MeanMotion: double;        // n:  平均運動（rad/日）     Mean Motion
    DecayRate: double;         // n1: 衰弱率                 Decay Rate, Drug 平均運動の変化 摂動1
    N2:    double;             // n2: 衰弱率                 Decay Rate, Drug 平均運動の変化 摂動1
    RAAN:  double;             // Ω: 昇交点赤経(度)　       Right Aceention of Ascending NODE
    ARGP:  double;             // ω: 近地点引数             Argument of Perigee
    MajorAxis: double;         // a:  長半径                 Semi Major Axis
    EpochRev:Integer;          // 周回番号
    Epoch_Y: Integer;
    Epoch_D: double;
    Epoch_MJD: Double;
  end;

//**********************************************************************//
//                                                                      //
//  観測地点（運用地点）クラス                                          //
//                                                                      //
//**********************************************************************//
  TObservationPoint = record
    Longitude: Double;         //  経度
    Latitude: Double;          //  緯度
    Height: Double;            //  高度
    TimeOffset: double;        //  時差
    LocalTimeStr: string;      //  JST 等の文字列
  end;

//**********************************************************************//
//                                                                      //
//  CommXの拡張クラス                                          //
//                                                                      //
//**********************************************************************//
type
  TCommX_Ex = class(TCPortX)
  private
    { Private 宣言 }
    XmlIni: TXmlIniFile;
    CPortX: TCportX;
  public
    { Public 宣言 }
//    procedure Open;
    function Open(FN: string): boolean;
    procedure Close;
    procedure Clear();
    function  Recv(MaxLen: integer):string;
    procedure Send(str: string);
  end;

//**********************************************************************//
//                                                                      //
//  GS-232のクラス                                                       //
//                                                                      //
//**********************************************************************//
type
  TGS232 = class(TComponent)
  private
    { Private 宣言 }
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
    { Public 宣言 }
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
//  軌道計算のクラス                                                    //
//                                                                      //
//**********************************************************************//
type
  TOrbit = class(TComponent)
  private
    { Private 宣言 }

//  軌道計算(AOS,Los,Max)計算の為
    OrbitEle_N: TOrbitalElments;           //  衛星の現在位置

    Az_d, El_d: double;

//  観測地点の中間計算値
    SinK, CosK, SinI, CosI: double;
    SinISinK, SinICosK, CosISinK, CosICosK: double;
    N: double;                          //  観測地点の東西線曲率半径
    UJ, VJ, WJ: double;                 //  観測地点のJ系地心直交座標
    U0, V0, W0: double;                 //  観測地点のG系地心直交座標

    FObsPoint: TObservationPoint;
    FKeplerEle: TOrbitalElments;
    FAosAz, FMaxAz, FMaxEl, FLosAz: double;
    FEpochRev: Integer;

    procedure CalcObservationPoint();
//    procedure SetOrbitaEle(const Value: TOrbitalElments);
  public
    { Public 宣言 }
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
//  Main Formのクラス                                                   //
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
    { Private 宣言 }
    Init: boolean;

    XMlIni: TXMlIniFile;
    XmlIniFile: string;
    Reg: TRegistry;
    RegKey: string;
    ParentInifile: string;          // 親AppのInifile名
    OrbitalFile: string;            // 衛星情報ファイル名
    ParentApp: string;              // 親アプリケーション名

    EpochRev, EpochRev_P: integer;  // 対象周回番号と前回の周回番号

    AutoTracking: boolean;

//  衛星の現在位置
    Sat, Sat_P: string;             // 対象衛星名と直前の衛星名
    SatelliteAz: integer;
    SatelliteEl: integer;

    Orbit: TOrbit;
    function TrackingStart(): boolean;
    procedure TrackingDo;
    procedure TrackingStop;
    function ShowOptions: boolean;
    procedure SetTracking(Tracking: boolean);
  public
    { Public 宣言 }
//  アンテナの現在位置
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
  Pi2: double = 2 * pi;                //  2 × PI
  RadDivDeg: double = 2 * pi / 360;    //  1度あたりのラジアン値

  G: Double = 6.67428E-11;             //  万有引力の定数　 m3/s2/kg
  GE: Double = 3.986005E14;            //  地心重力定数 or 地心引力定数 m3/s2
  GE23: double = 42241.09773;          //  地心重力定数 の 2/3乗

  AE: double  = 6377.397155;           //  地球の半径 km
  EE: double  = 0.01672;               //  地球の離心率
  EE2: double = 0.006674372230314;     //  地球の離心率 の 2乗　

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
    XmlIni.OpenNode(cNodeMain, true);   // Form位置再現
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
  Init := true;      // FormActivateの初期化のため
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
  if not Init then    // APP起動時1回だけ実行する
    exit;

  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    if not XmlIni.OpenNode(cNodeCom, false) then    // 初めて起動した時、オプッション画面を表示する
      while not ShowOptions do
        begin

        end;
  finally
    FreeAndNil(XmlIni);
  end;
  TrackingStart;
  Init := false;    // FormActivateの初期化のため
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
//  手動追尾のボタンを押した時の処理
//  (ボタンのTagにボタンごとの値が設定してある)
//  (ボタンを押すと、そのボタンのCancel=TRUEになる)
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
  for i := 0 to 3 do       // 全ボタンを使用可能にする
    GroupBox1.Controls[i].Enabled := true;
  if TSpeedButton(Sender).Tag < 2 then
    s := 'A'              // Az回転停止
  else
    s := 'E';             // El回転停止
  Gs232.DoCmd(s);
end;

procedure TMain.bbGroupeDownClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  OutputDebugString('bbGroupeDownClick');
  TSpeedButton(Sender).OnClick := bbGroupUpClick;
  for i := 0 to 3 do        // Clickされたボタン以外を使用不可能にする
    if GroupBox1.Controls[i].Tag <> TSpeedButton(Sender).Tag then
      begin
      GroupBox1.Controls[i].Enabled := false;
      end;
  case TSpeedButton(Sender).Tag of
    0: s := 'L';            // Left回転
    1: s := 'R';            // Right回転
    2: s := 'U';            // Up回転
    3: s := 'D';            // Down回転
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

//  とりあえず使わない
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

//Case　1
//  Gs232.Start(208,269,332);
//  gs232.Move(210, 2);
//  gs232.Move(269, 32);
//  gs232.Move(330, 2);

//Case　2
//  Gs232.Start(332,269,208);
//  gs232.Move(330, 2);
//  gs232.Move(269, 32);
//  gs232.Move(210, 2);

//Case　3
//  Gs232.Start(160,40,355);
//  gs232.Move(155, 2);
//  gs232.Move(40, 32);
//  gs232.Move(358, 2);

//Case　4
//  Gs232.Start(355,40,160);
//  gs232.Move(358, 2);
//  gs232.Move(40, 32);
//  gs232.Move(155, 2);
end;

//////////////////////////////////////////////////////////////////////////
//
//  自動追尾の処理
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

//  観測地点のデータを読み込み
  if not Orbit.SetObsPoint(ParentApp, ParentInifile)  then
    begin
    MessageDlg('親APPのInifileが読めません', mtError, [mbOK], 0);
    result := false;
    Exit;
    end;

  reg.RootKey := HKEY_CURRENT_USER;
  if not Reg.OpenKey(RegKey, False) then
    begin;
    MessageDlg('CALSAT32のレジストリが読めません', mtError, [mbOK], 0);
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

  SetTracking(false);                // buttonを初期化する
  Timer1.Enabled := True;            // この操作で、追尾を始める
end;

procedure TMain.SetTracking(Tracking: boolean);
begin
  OutputDebugString('SetTracking');
  AutoTracking := Tracking;
  if Tracking then          //  自動追尾中に押された
    begin
    btnTracking.Caption := '&Tracking Off';
    btnTracking.Down    := true;
    bbUp.Enabled    := false;
    bbDown.Enabled  := false;
    bbLeft.Enabled  := false;
    bbRight.Enabled := false;
//    btnTracking.Font.Color := clWindowText;
    GS232.Start(Orbit.AosAz, Orbit.MaxAz, Orbit.LosAz); //AOS.Losから初位置を計算する
    Timer1.Enabled  := true;
    end
  else                          //  自動追尾でない時に押された
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

//  Timer1により、1秒に1回処理される
procedure TMain.TrackingDo();
var
  i: integer;
  s: string;
  nw: TDateTime;
begin
  OutputDebugString('TrackingDo');

//  Calsatから情報取得・表示
  Sat := Reg.ReadString('Satellite');   //  追尾対象衛星名を取得
  i   := Pos('[', Sat);                 //  衛星名[Mode]の衛星名のみを選択するため
  if i <> 0 then
    Sat := Copy(Sat, 1, i - 1);
    if ParentApp = 'CALSAT32' then
      EpochRev := StrToInt(Reg.ReadString('Revolution'))   //  CALSAT32の時、周回番号を取得
    else
      EpochRev := 0;

//  現在日付・時刻を表示
  Nw      := MJDtoDateTime(StrToFloat(Reg.ReadString('MJD')));
  DateTimeToString(s, 'yy/mm/dd hh:nn:ss', Nw + Orbit.ObsPoint.TimeOffset);
  lblYMD.Caption := s + '(' + Orbit.ObsPoint.LocalTimeStr + ')';

//  追尾対象衛星の現在Az,Elを表示
  grpSatellite.Caption    := Sat;
  SatelliteAz   := round(StrToFloat(Reg.ReadString('Azimuth')));
  SatelliteEl   := round(StrToFloat(Reg.ReadString('Elevation')));
  lblSatelliteAz.Caption  := IntToStr(SatelliteAz)   + '°';
  lblSatelliteEl.Caption  := IntToStr(SatelliteEl) + '°';

  if (Sat <> Sat_P) or (EpochRev <> EpochRev_P) then  //  追尾対象衛星が変わったら
    begin
    Sat_P       := Sat;
    EpochRev_P  := EpochRev;
    if AutoTracking then                   //   CALSAT32の衛星が変わったら、自動追尾を停止する
      SetTracking(false);
    Orbit.SetOrbitaEle(OrbitalFile, Sat);    //  AOS,LOS,MAXのAzを計算
    Orbit.CalcAosLos(Nw);
    s := format('(Aos)Az=%3.0f  (Max)Az=%3.0f El=%2.0f  (Los)Az=%3.0f', [Orbit.AOSAz, Orbit.MaxAz, Orbit.MaxEl, Orbit.LOSAz]);
    lblAOSAz.Caption := s;
    end;

//  GS232からアンテナのAz,El情報取得
  if Gs232.Opend and GS232.GetPos(AntennaAz, AntennaEl) then      // AntAz、AntElにアンテナの現在位置が帰る
    begin
    lblAntennaAz.Caption  :=  IntToStr(AntennaAz) + '°';
    lblAntennaEl.Caption  :=  IntToStr(AntennaEl) + '°';
    lblRotatorAz.Caption  :=  IntToStr(Gs232.RotatorAz) + '°';
    lblRotatorEl.Caption  :=  IntToStr(Gs232.RotatorEl) + '°';
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
    if AutoTracking then                   //   Errorなら、自動追尾を停止する
      SetTracking(false);
    end;

//  自動追尾でないなら終了
  if NOT AutoTracking then
    exit;

//  アンテナと衛星の方向があっているなら何も処理しない
//  衛星の方向にアンテナを動かす
  if (SatelliteAz = AntennaAz) and (SatelliteEl = AntennaEl) then
    exit;
  if not GS232.Move(SatelliteAz, SatelliteEl)  then
    SetTracking(false);
end;

//////////////////////////////////////////////////////////////////////////
//
//    参考資料
//      １．天体の位置計算 増補版   著者：長沢工氏　　発行：地人書館
//      2.  Calsat32のホームぺージ  作者：相田政則氏（JR1HUO）
//             http://homepage1.nifty.com/aida/jr1huo_calsat32/index.html
//
//////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//
//  観測地点のデータを設定する
//
//////////////////////////////////////////////////////////////////////////
function TOrbit.SetObsPoint(App: string; FileName: string): boolean;
var
  Ini: TIniFile;
  GridLoc: string;
begin
  result := true;
//  ファイルの存在確認
  if not FileExists(FileName) then
    begin
    result := false;
    MessageDlg(FileName + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;

//  観測地点のデータ読み込み
  Ini :=TIniFile.Create(FileName);
  try
    with FObsPoint do
      begin
      if APP = 'CALSAT32' then
        begin
        Latitude      := DegToRad(Ini.ReadFloat('MYQTH', 'MyIdo', 0));     //
        Longitude     := DegToRad(Ini.ReadFloat('MYQTH', 'MyKeido', 0));  //
        Height        := Ini.ReadFloat('MYQTH', 'MyTakasa', 0) / 1000;           //  単位：km
        TimeOffset    := Ini.ReadFloat('TIMEOFFSET', 'TimeOffset', 9) / 86400;  //  日単位の時差
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
        Height        := Ini.ReadFloat('POSITION', 'TAKASA', 0) / 1000;   //  単位：km
        TimeOffset    := Ini.ReadFloat('JIKAN', 'JISA', 9) / 86400;  //  日単位の時差
        if TimeOffset >= 0 then
          LocalTimeStr  := 'UTC+' + FloatToStr(TimeOffset)
        else
          LocalTimeStr  := 'UTC' + FloatToStr(TimeOffset);
        end;

//  テスト用データ(1)(2)(3)
{
      Latitude      := DegToRad(35.7760287);     //   緯度
      Longitude     := DegToRad(139.634385);     //   経度
      Height        := 0;                        //   高さ　単位：km
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
//  観測地点のデータを計算する
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

//  東西線曲率半径を計算 (P184)
    N := AE / SQRT(1 - EE2 * Power(sin(Latitude), 2));

//  観測点をJ系直交座標系へ変換 (P185)
    H := N + Height;
    SinLat := sin(Latitude);
    CosLat := cos(Latitude);
    SinLon := sin(Longitude);
    CosLon := cos(Longitude);
    UJ := H * cosLat * cosLon;
    VJ := H * cosLat * sinLon;
    WJ := (N * (1 - EE2) + Height) * sinLat;

//  観測点をG系直交座標系へ変換 (P188)
    U0 := UJ - 0.136;
    V0 := VJ + 0.521;
    W0 := WJ + 0.681;
  end;
end;

///////////////////////////////////////////////////////////////////////////
//
//  サテライト名からNASA 2ラインを読み込み
//  軌道要素データを設定する。
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

//  ファイルの存在を確認
  if not FileExists(FileName) then
    begin
    result := false;
    MessageDlg(FileName + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;

//  軌道要素の読み込み
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

//  テスト用データ
//    FO-29
//    Line1 := '1 24278U 96046B   03232.95303700 -.00000056 00000-0 -24068-4 0 06270';
//    Line2 := '2 24278 098.5660 359.5477 0351513 101.3348 262.7487 13.52885302346104';
//

//  データは弧度値(Radian)に変換
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
  Sec: double = 1 / 1440;    // 1 / 24 / 60  1秒/日
begin
  FAosAz := 0; FMaxAz := 0; FLosAz := 0;
  FMaxEl := 0;
  UpDown := True;
  Ut := DT;
  for i := 0 to 3000 do       // 3000分(5時間)後までで最初のAOS等を計算する
    begin
    Ut := Ut + Sec;
    CalcOrbit(FKeplerEle, FObsPoint, Ut);
    Az := Trunc(Az_d);
    El := Trunc(EL_d);
    if El > FMaxEl then   //  Max Elの判断
      begin
      FMaxEl := El;
      FMaxAz := Az;
      FEpochRev := OrbitEle_N.EpochRev;
      end;
    if UpDown then           //  AOSの判断
      begin
      if (FAosAz = 0) and (El >= 0) then
        begin
        FAosAz := Az;
        UpDown := false;
        end
      end
    else
      begin                     //  LOSの判断
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
  t, t2, Gt: double;            // 経過時間、グリニッジ恒星時
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
//  衛星の中間計算値
  SinRAAN, CosRAAN, SinIncl, CosIncl, SinARGP, cosARGP: double;
  ASsX, ASsY, BSsX, BSsY, CSsX, CSsY: double;
begin
  with OrbitEle_N do
    begin

//  経過時間を計算
    t  := (Ut - KE.Epoch);
    t2 := t * t;

//  観測時刻の軌道要素計算
    Epoch             := KE.Epoch + t;
    Epoch_MJD         := DateTimeToMJD(FKeplerEle.Epoch);
    Inclination       := KE.Inclination;
    Eccentricity      := KE.Eccentricity;
    MeanMotion        := KE.MeanMotion + KE.DecayRate * t / 2;      // 平均運動 (Rad/Day)
    MajorAxis         := GE23 / Power(MeanMotion / Pi2,  2/3);      // 長半径
    W := 0.174 / Power((MajorAxis / AE), 3.5) * t;
    Argp              := KE.Argp + W * (2 -2.5 * SQR(Sin(Inclination)));
    Raan              := KE.Raan - W * Cos(Inclination);
    MeanAnomaly       := KE.MeanAnomaly + MeanMotion * t
                       + KE.DecayRate * t2 / 2
                       + KE.N2 * t2 * t / 3;
    MeanAnomaly       := Frac(MeanAnomaly / Pi2) * Pi2; // 360度以内にまるめる
                                                  // M or l:  平均近点角(Rad)
    DecayRate         := 0;
    n2                := 0;
//    EpochRev          := Trunc(KE.EpochRev + KE.MeanMotion * t);
    EpochRev          := Trunc(KE.EpochRev + (KE.MeanMotion / Pi2) * t);

//  ケプラーの方程式で平均近点角、離心率から離心近点角(U)を求める(P147)
    U := Kepler(MeanAnomaly, Eccentricity);

//  軌道面内のX,Y座標を求める (P148)
    SinU := sin(U);
    CosU := cos(U);
    r := MajorAxis * (1 - Eccentricity * CosU);
    x := MajorAxis * (cosU - Eccentricity);
    y := MajorAxis * Sqrt(1 - Eccentricity * Eccentricity) * SinU;

//  地心赤道直交座標に変換するための事前計算
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

//  地心赤道直交座標に変換 (P196)
    ASs := x * ASsX - y * ASsY;
    BSs := x * BSsX - y * BSsY;
    CSs := x * CSsX + y * CSsY;

//  グリニッジ恒星時を求める
    Gt := UtToSiderealTime(Ut, 0);  // 東経0度の恒星時 結果は、少数以下で返ってくる
    Gt := Gt * Pi2;                 //  Radianに変換する

//  G系地心直交直交座標系に変換する (P197)
    SinGt := Sin(Gt);
    CosGt := Cos(Gt);
    Us :=   ASs * CosGt + BSs * SinGt;
    Vs := - ASs * SinGt + BSs * CosGt;
    Ws :=   CSs;

//  G系測心直交座標系へ変換  (P198)
    Ud := Us - U0;
    Vd := Vs - V0;
    Wd := Ws - W0;

//  地平直交座標系への変換 (P199)
    Xd :=  Ud * SinICosK + Vd * SinIsinK - Wd * CosI;
    Yd := -Ud * SinK + Vd * cosK;
    Zd :=  Ud * CosICosK + Vd * CosISinK + Wd * SinI;
    Rd := Sqrt(Sqr(Xd) + Sqr(Yd) + Sqr(Zd));   //  衛星までの距離

//  方位角と高度に変換 (P199)
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
//  Commポートの処理
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
      MessageDlg(CPortX.Port + 'をOpenできません', mtInformation, [mbOk], 0);
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
    CommX_Ex.Send('');         // GS-23のBufferをクリアする
    CommX_Ex.Clear;
    CommX_Ex.Send('S');        // 動作の停止
    CommX_Ex.Send(format('X%.1d', [RotateSpeed + 1]));        // スピードの設定　1:低速 2:中速1 3：中速2 4：高速
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
  FisCW := (Max > AOS);   //  時計回り/反時計周り
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
      D := trunc(nAz / 90);         //Azの象限を判断
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

  if isAOS and (El <= 0) then           //  自動追尾終了
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
    CommX_Ex.Send('C2');                   // 水平角度、仰角の読み取り指示
///////////////////////////////////////////////////////////////////////////////
///   １回目のSendの時、CrLfしか返ってこない
///   ２回目以降は正常に返ってくる
///    wait(10);                     //  してみたが、変わらず
///  AZを含む 又は 12桁以外の応答は無視するように修正した 2018-12-24
    s := CommX_Ex.Recv(15);
    FAntennaAz := 0;
    FAntennaEl := 0;
    FRotatorAz := 0;
    FRotatorEl := 0;
    i := Pos('AZ=', s);
    if i <> 0 then                                      // 応答はAZ=xxx EL=yyyCrLf (15桁)
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
      e1 := TryStrToInt(Copy(s,3,3), FRotatorAz);       // 応答は+xxxx+yyyyCrLf (12桁)
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
    wait(500);                     //  ホ-ム位置に戻すまでの通信時間確保
    end;
  CommX_Ex.Send('');
  CommX_Ex.Clear;
  CommX_Ex.Close;
  FOpend := false;
end;

///////////////////////////////////////////////////////////////////////////
//
//  Delphi の TDateTime 値を準（修正）ユリウス日（Modeified Julian Day;MJD)）に変換する。
//
//  Delphi の TDateTime 値の整数部は西暦 1899 年 12 月 30 日からの経過日数を示します。
//  小数部はその日の経過時間（24 時間制）です。
//
//  準ユリアス日は、1858年11月17日0時を起算開始日とした通算日
//  準ユリアス日　＝　ユリアス日 －　2400000.5　の関係がある
//
//  通常 1899年12月30日 以前を計算対象としなければ、特にMJDに変換する必要ない
//  1899年12月30日以前は、TDateTimeを調べなければ分からない (多分 大丈夫)
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
//  ケプラーの方程式で離心近点角(U)を求める
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
//  Utからグリニッジ平均恒星時に変換する。 (P27,151)
//
///////////////////////////////////////////////////////////////////////////
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
var
  Y,M,D: Integer;
  Lt, K, TU, Tg0: Double;
begin
//  1899/12/31 12:00 殻の経過日数(K)を求める
//  この式を適用できるのは、1900/3/1～2100/2/28の期間のみ
  Y := YearOf(Ut) - 1900;
  M := MonthOf(Ut);
  D := DayOf(Ut);
  if M < 3 then
    begin
    Y := Y - 1;
    M := M + 2;
    end;
  K := 365 * Y + 30 * M + D - 33.5 + Int(0.6 * (M + 1)) + Int(Y / 4);
//  1899/12/31 12:00 殻の経過日数(K)を,36525日を1とした単位（TU）を求める
  TU := K / 36525;
//  UT 00:00のグリニッジ恒星時を求める（24時間以上は、最後に切り捨てている）
  Tg0 := (23925.836 + 8640184.542 * TU + 0.0929 * TU * TU) / 86400;
//  求める時刻のグリニッジ恒星時を求める
//  結果は、日付形式であるので、少数以下の桁のみ有効
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
//  AppnにWaitをかける
//
///////////////////////////////////////////////////////////////////////////
procedure Wait(t: integer);
var
  endtime : TDateTime; //終了時間
begin
  endtime := Now + t / 86400 / 1000;
//　　while (Now < endtime) do ;
  while (Now < endtime) do
    Application.ProcessMessages;
end;

end.

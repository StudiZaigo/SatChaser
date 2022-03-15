unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ExtCtrls, Buttons, ActnList, Menus,
  Math, DateUtils, UITypes,
  IniFiles, XmlIniFile,
  CPortX, Cport, System.Actions,
  uConstant;

  function EnumerateChildWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

//**********************************************************************//
//                                                                      //
//  例外通知用のクラス                                                  //
//                                                                      //
//**********************************************************************//
type
  EAppError = class(Exception);

//**********************************************************************//
//                                                                      //
//  衛星の可視時間帯クラス                                              //
//                                                                      //
//**********************************************************************//
  TVisibleTime = record
    AosAz: double;
    MaxAz: double;
    MaxEl: double;
    LosAz: double;
    AosDateTime: TDateTime;
    MaxDateTime: TDateTime;
    LosDateTime: TDateTime;
  end;

type
//**********************************************************************//
//                                                                      //
//  観測地点（運用地点）クラス                                          //
//                                                                      //
//**********************************************************************//
  TObservationPoint = record
    Longitude: double;         //  経度
    Latitude: double;          //  緯度
    Altitude: double;          //  標高
    TimeOffset: double;        //  時差
    LocalTimeStr: string;      //  JST等の文字列
  end;

  TAlert = (alNone, alPreAOS, alAOS, alLOS);

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

    CPortX: TCPortX;

    Az_p: integer;
    El_p: integer;
    isAos: boolean;
    isLos: boolean;

    FOpend: boolean;
    FOverlap: boolean;
    FFlip: boolean;
    FAzRotator: TAzRotator;
    FElRotator: TElRotator;
    FAntennaAz: integer;
    FAntennaEl: integer;
    FRotatorAz: integer;
    FRotatorEl: integer;
    FRotateMode: TRotateMode;
    FControlMode: TControlMode;
    FZeroPathOrbit: boolean;
    FisCW: boolean;
    FParkingAz: integer;
    FAzOffset: integer;
    FGpParking: boolean;
    FParkingEl: integer;
    FRotateSpeed: integer;

    procedure OnPortException(Sender: TObject; TComException: TComExceptions;
      ComportMessage: String; WinError: Int64; WinMessage: String);
    procedure SetAzRotator(const Value: TAzRotator);
    procedure SetElRotator(const Value: TElRotator);
    procedure ClearCPort;
    procedure CloseCPort;
    function OpenCPort(FN: string): boolean;
    function RecvFromCPort(MaxLen: integer): string;
    procedure SendToCPort(str: string);
    procedure CPortXOnRxChar(Sender: TObject; Count: Integer);
    procedure SetAzOffset(const Value: integer);
    procedure SetGoParking(const Value: boolean);
    procedure SetParkingAz(const Value: integer);
    procedure SetParkingEl(const Value: integer);
//    procedure SetRotatorSpeed(const Value: integer);
    procedure SetRotateSpeed(const Value: integer);

  public
    { Public 宣言 }
    property Opend: boolean read FOpend;
    property isOverlap: boolean read FOverlap;
    property AzRotator: TAzRotator read FAzRotator write SetAzRotator;
    property ElRotator: TElRotator read FElRotator write SetElRotator;
    property AntennaAz: integer read FAntennaAz;
    property AntennaEl: integer read FAntennaEl;
    property RotatorAz: Integer read FRotatorAz;  // ローテタの指針
    property RotatorEl: Integer read FRotatorEl;
    property ControlMode: TControlMode read FControlMode;
    property isCW: boolean read FisCW;
    property isFlip: boolean read FFlip;
    property RotateMode: TRotateMode read FRotateMode;
    property ZeroPathOrbit: boolean read FZeroPathOrbit;

    property ParkingAz: integer read FParkingAz write SetParkingAz;
    property ParkingEl: integer read FParkingEl write SetParkingEl;
    property AzOffset: integer read FAzOffset write SetAzOffset;
    property RotateSpeed: integer read FRotateSpeed write SetRotateSpeed;
    property GoParking: boolean read FGpParking write SetGoParking;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function Move(SatAz, SatEl: integer): boolean;
    function  GetPos(var Az, El: Integer):boolean;
    function Open(FN: string): boolean;
    procedure Close(ParkinAz, ParkingEl, AzOffset: integer; GoParking: boolean);
    procedure DoCmd(Cmd: string);
    function DoRecv(len: integer): string;
    procedure GotoParking();
    procedure JudgeRotatorMode(AosAz, MaxAz, LosAz: double; AzOffset: integer);
    procedure Stop();
  end;

//**********************************************************************//
//                                                                      //
//  Main Formのクラス                                                   //
//                                                                      //
//**********************************************************************//
type
  TMain = class(TForm)
    AboutBox1: TMenuItem;
    actAboutBox: TAction;
    actDebug: TAction;
    actExit: TAction;
    actGotoParking: TAction;
    ActionList1: TActionList;
    actOptions: TAction;
    actTerminal: TAction;
    bbOff: TSpeedButton;
    bbLeft: TSpeedButton;
    bbRight: TSpeedButton;
    bbUp: TSpeedButton;
    bbDown: TSpeedButton;
    btnStopTracking: TSpeedButton;
    btnTest: TButton;
    btnTracking: TSpeedButton;
    Exit1: TMenuItem;
    GotoParking1: TMenuItem;
    ImgFlipMode: TImage;
    ImgGreen: TImage;
    ImgOverlap: TImage;
    ImgRed: TImage;
    imgFocused: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblAntennaAz: TLabel;
    lblAntennaEl: TLabel;
    lblAOSAz: TLabel;
    lblRotateModeName: TLabel;
    lblRotatorAz: TLabel;
    lblRotatorEl: TLabel;
    lblSatellite: TLabel;
    lblSatelliteAz: TLabel;
    lblSatelliteEl: TLabel;
    lblYMD: TLabel;
    Options1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    Panel2: TPanel;
    N1: TMenuItem;
    Label4: TLabel;
    Label5: TLabel;

    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actAboutBoxExecute(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actGotoParkingExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure bbGroupeDownClick(Sender: TObject);
    procedure btnTrackingClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
//    procedure actDebug(Sender: TObject);
  private
    { Private 宣言 }
    Init: boolean;

    XMlIni: TXMlIniFile;
    XmlIniFile: string;
    Reg: TRegistry;
    RegKey: string;
    ParentApp: string;              // 親アプリケーション名
    ParentInifile: string;          // 親AppのInifile名
    ElementsFile: string;           // 衛星情報ファイル名
    wasFocused: boolean;

    AutoTracking: boolean;
    ControlMode: TControlMode;      // Manual/Tracking
    ClickedButton: TSpeedButton;

    aryAlert: array[0..5] of integer;
    PreAOSTime: integer;
    slAlert: TStringlist;
    Alert: TAlert;

    function DoLoop(): boolean;
    procedure DoTracking;
    procedure StopTracking;
    function ShowOptions: boolean;
    procedure SetTracking(Tracking: boolean);
    procedure MoveFormToBelow;
    procedure bbGroupeSet(Value: boolean);
    procedure ReadIniFile2;
    procedure CalcVisibleTime(Nw: TDateTime);
    function ReadParentInifile: boolean;
  protected
    { protected 宣言 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public 宣言 }
    isDebug: boolean;

    Gs232: TGs232;                // tano unitからも参照される
    VisibleTime: TVisibleTime;    // 衛星の可視時間
    VisibleTimeString: string;    // CALSAT32の衛星の当日可視時間
    ObsPoint: TObservationPoint;  // 緯度経度、時差
//  衛星の現在位置
    Sat, PSat: string;             // 対象衛星名と直前の衛星名
    EpochRev, PEpochRev: integer;  // 対象周回番号と前回の周回番号
    SatelliteAz: integer;
    SatelliteEl: integer;
//  アンテナの現在位置
    AntennaAz: integer;
    AntennaEl: integer;
    RotateMode: TRotateMode;
    RotateModeName: string;
  end;

var
  Main: TMain;

implementation

uses  uOptions, uProof, uAbout;

{$R *.dfm}

function EnumerateChildWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  ClassName: array[0..255] of char;
  Buffer: string;
  ID : LongInt;
begin
  result := true;
  GetClassName(hWnd, ClassName, 255);
  if ClassName = 'ThunderRT6TextBox' then
    begin
    ID := GetWindowLong(hWnd, GWL_ID);
    if (ID = 84) then     //  CALSAT32の計算された[可視時間帯]の判断
      begin
      SetLength(Buffer, 1024 + 1);
      SendMessage(hWnd, WM_GETTEXT, Length(Buffer), integer(Buffer));
      Main.VisibleTimeString := Buffer;
      result := false;    // 以降、見る必要がない
      end;
    end;
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
  while (Now < endtime) do
    Application.ProcessMessages;
end;

///////////////////////////////////////////////////////////////////////////
//
//  GS-232クラス
//
///////////////////////////////////////////////////////////////////////////
function TGs232.OpenCPort(FN: string): boolean;
begin
  result := True;
  CPortX := TCPortX.Create(self);
  XmlIni := TXmlIniFile.Create(FN);
  try
    XmlIni.OpenNode(cnNodeCom, true);
    CPortX.Port           := XmlIni.ReadString(cnComPort, 'COM3');
    CPortX.BaudRate       := XmlIni.ReadString(cnBaudRate, '9600');
    CPortX.DataBits       := XmlIni.ReadString(cnDataBits, '8');
    CPortX.Parity         := XmlIni.ReadString(cnParity, 'NONE');
    CPortX.StopBits       := XmlIni.ReadString(cnStopBits, '1');
    CPortX.FlowControl    := XmlIni.ReadString(cnFlowControl, 'NONE');
//    CportX.EventChar     := Char(13);
//    CportX.OnRxChar      := RecvFromCPort;
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

procedure TGs232.CloseCPort();
begin
  CportX.Close;
  FreeAndNil(CportX);
end;

procedure TGs232.CPortXOnRxChar(Sender: TObject; Count: Integer);
var
  Str: String;
  s1,s2: string;
begin
  CPortX.ReadStr(Str, Count);
  s1  := StringReplace(str, #13, '#h0D', []);
  s1  := StringReplace(s1, #10, '#h0A', []);
  s2  := FormatDateTime('yy-mm-dd hh:nn:ZZZ', Now);
end;

function TGs232.RecvFromCPort(MaxLen: integer):string;
var
  s : string;
begin
  Result := '';
  try
    s := CportX.RecvStr(MaxLen);
    Result := s;
  except
    Result := '';
    ShowMessage('Recv Error');
  end;
end;

procedure TGs232.SendToCPort(str: string);
begin
  try
    CportX.SendStr(str + #13);
  except
    ShowMessage('Send Error');
  end;
end;

procedure TGs232.ClearCPort();
begin
  CPortx.Clear;
end;

constructor TGs232.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  CPortX  := TCPortX.Create(self);
  FOpend        := false;
  FControlMode  := ctNone;
  FOverlap      := false;
  FFlip         := false;
end;

destructor TGs232.Destroy;
begin
  FreeAndNil(CPortX);
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
//  CPortX.OnException :=  OnPortException;
  if OpenCPort(FN) then
    begin
    ClearCPort;
    SendToCport('');         // GS-23のBufferをクリアする
    ClearCPort;
    SendToCPort('S');        // 動作の停止
    SendToCPort(format('%.1dX', [RotateSpeed + 1]));        // スピードの設定　1:低速 2:中速1 3：中速2 4：高速
    FOpend := True;
    end
  else
    begin
    FOpend := False;
    end;
  result := FOpend;
end;

procedure TGs232.Close(ParkinAz, ParkingEl, AzOffset: integer; GoParking: boolean);
begin
  if not FOpend then
    exit;

  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cnNodeGS232, true);
  finally
    FreeAndNil(XmlIni);
  end;

  if GoParking then
    begin
    GotoParking();
    wait(500);                     //  ホ-ム位置に戻すまでの通信時間確保
    end;
  SendToCport('');
  ClearCport;
  CloseCport;
  FOpend := false;
end;

procedure TGs232.DoCmd(Cmd: string);
begin
  SendToCPort(CMD);
end;

function TGs232.DoRecv(Len: integer):string;
var
  s, m: string;
begin
  result := '';
  try
    s := RecvFromCport(Len);
    trim(s);
    if Length(s) <> 0 then
      begin
      result := s;
      end;
  except
    m := 'Error =' + s;
    MessageDlg(m, mtInformation, [mbOk], 0);
  end;
end;

procedure TGS232.SetAzOffset(const Value: integer);
begin
  FAzOffset := Value;
end;

procedure TGS232.SetAzRotator(const Value: TAzRotator);
begin
  FAzRotator := Value;
end;

procedure TGS232.SetElRotator(const Value: TElRotator);
begin
  FElRotator := Value;
end;

procedure TGS232.SetGoParking(const Value: boolean);
begin
  FGpParking := Value;
end;

procedure TGS232.SetParkingAz(const Value: integer);
begin
  FParkingAz := Value;
end;

procedure TGS232.SetParkingEl(const Value: integer);
begin
  FParkingEl := Value;
end;

procedure TGS232.SetRotateSpeed(const Value: integer);
begin
  FRotateSpeed := Value;
end;

procedure TGs232.JudgeRotatorMode(AosAz, MaxAz, LosAz: double; AzOffSet: integer);
var
  Aos, Max, Los: integer;
begin
  Aos := (trunc(AosAz) - AzOffset) Mod 360;
  Max := (trunc(MaxAz) - AzOffset) Mod 360;
  Los := (trunc(LosAz) - AzOffset) Mod 360;

  if ((Aos >= Max) and (Max >= LOS))
  or ((Aos <= MAx) and (Max <= LOS)) then
    begin
    FZeroPathOrbit := False;
    FRotateMode := gsNormal;
    end
  else
    begin
    FZeroPathOrbit := true;

    if (FAzRotator =  az450)
    and ((Aos < 90) or (Los < 90)) then
      FRotateMode := gsOverlap
    else if (FElRotator =  el180) then
      FRotateMode := gsFlip
    else
      FRotateMode := gsCrossing
    end;
  FisCW := (Max > AOS);   //  時計回り/反時計周り
  isAos := false;
  isLos := false;
end;

function TGs232.Move(SatAz, SatEl: integer): boolean;
var
  s: string;
  D: integer;
  AzOnly: boolean;
  nAz,nEl: integer;
begin
  result := true;
  if isLOS and (SatEl < 0) then
    begin
    result := false;
    exit;
    end;

  if (SatAz = Az_p) and (SatEl = El_p) then
    Exit;

//  if SatEl = El_p then             // これを行うと、次のMでELが停止してしまう。
//    AzOnly := True
//  else
    AzOnly := false;

  nAZ := (SatAZ - AzOffSet) mod 360;
  if (FElRotator = el90) and (SatEl > 90) then
    nEl := 90
  else
    nEl := SatEL;
  if (not isAOS)  and (SatEL < 0) then
    begin
    nEl := 0;
    end
  else
    begin
    isAos := true;
    end;

  case RotateMode of
    gsOverlap:
      if ZeroPathOrbit then
        begin
        D := trunc(nAz / 90);         //Azの象限を判断
        if (D = 0) then
          nAz := nAz + 360
        end;
    gsFlip:
      if ZeroPathOrbit then
        begin
        nAZ := nAZ + 180;   // フリップモードで逆向き
        nAz := nAZ mod 360;
        nEl := 180 - nEl;
        end
      else
        begin              // フリップモードで順向き
        end;
    end;

  if isAOS and not isLOS and (SatEl < 0) then           //  自動追尾終了
    begin
    isLOS := true;
    end;

 if (ElRotator = elNone) or AzOnly then
    s := format('M%.3d', [nAz])
  else
    s := format('W%.3d %.3d', [nAz, nEl]);

  SendToCport(s);
  Az_p := SatAz;
  El_p := SatEl;
end;

procedure TGs232.Stop();
begin
  SendToCport('S');
end;

procedure TGs232.GotoParking();
begin
  if FParkingAz < 0 then
    ParkingAz := 0;
  if FParkingEl < 0 then
    ParkingEl := 0;
  SendToCport(format('W%.3d %.3d', [FParkingAz - FAzOffset, FParkingEl]));
end;

function TGs232.GetPos(var Az, El: integer):boolean;
var
  s,m: string;
  e1,e2: boolean;
  i, j: integer;
  cmd: string;
  len: integer;
begin
  result := false;
  try
///////////////////////////////////////////////////////////////////////////////
///   １回目のSendの時、CrLfしか返ってこない
///   ２回目以降は正常に返ってくる
///  AZを含む 又は 12桁以外の応答は無視するように修正した 2018-12-24
///////////////////////////////////////////////////////////////////////////////
    if ElRotator = elNone then
      cmd := 'C'                         // 水平角度のみの読み取り指示
    else
      cmd := 'C2';                      // 水平角度、仰角の読み取り指示
    SendToCPort(cmd);                // 読み取り実行

    s := RecvFromCport(20);             // 少し余分に読み取る
    len := Length(s);
    if cmd = 'C' then
      begin
      if (len <> 7) and (len <> 8) then   // 12はGD232A,8はGS-232B 未確認
        begin
        exit;
        end;
      end
    else
      begin
      if (len <> 12) and (len <> 15) then   // 12はGD232A,15haGS-232B
        begin
        exit;
        end;
      end;

    s := StringReplace(trim(s), #13#10, '', []);
    s := s + '0000000';                 //　12,15桁共通処理にするため
    FAntennaAz := 0;
    FAntennaEl := 0;
    FRotatorAz := 0;
    FRotatorEl := 0;
    if cmd = 'C' then
      begin
      i := 0;
      j := 7;
      if (Copy(s, i, 2) = 'AZ') or (Copy(s, j, 2) = 'EL') or (Copy(s, j, 2) = '00') then    // 応答はAZ=xxx EL=yyyCrLf (15桁)  for GS-232B
        begin
        e1 := TryStrToInt(Trim(Copy(s,i+3, 3)), FRotatorAz);
        e2 := TryStrToInt(Trim(Copy(s,j+3, 3)), FRotatorEl);
        end
      end
    else
      begin
      i := 0;
      j := 5;
      if (Copy(s, i, 2) = '+0') or (Copy(s, j, 2) = '+0')  or (Copy(s, j, 2) = '00') then
        begin
        e1 := TryStrToInt(Copy(s, i+3, 3), FRotatorAz);   // 応答は+xxxx+yyyyCrLf (12桁) for GS-232A,GS-23
        e2 := TryStrToInt(Copy(s, j+3, 3), FRotatorEl);
        end;
      end;
    if not e1 or not e2  then
      begin
      exit;
      end;

    FAntennaAz := (FRotatorAz + AzOffset + 360) Mod 360;
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
    m := 'Error712 =' + s;
    MessageDlg(m, mtInformation, [mbOk], 0);
    Az := 999;
    El := 999;
  end;
end;



///////////////////////////////////////////////////////////////////////////
//
//  Mainクラス
//
///////////////////////////////////////////////////////////////////////////
procedure TMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  isDebug := false;
  btnTest.Visible := isDebug;
  actTerminal.Enabled := false;

  Self.Height := 90;
  Panel2.Caption := '';
  lblSatellite.Caption    := 'Satllite';

  lblSatelliteAz.Caption  := '';
  lblSatelliteEl.Caption  := '';
  lblAntennaAz.Caption    := '';
  lblAntennaEl.Caption    := '';
  lblRotatorAz.Caption    := '';
  lblRotatorEl.Caption    := '';
  lblYMD.Caption          := '';
  lblAOSAz.Caption        := '';
  lblRotateModeName.Caption := '';

  btnStopTracking.Visible := false;
  bbOff.Visible           := false;
  bbOff.Down := true;

  Alert := alNone;
  slAlert := TStringList.Create;

  XmlIniFile := ChangeFileExt(Application.ExeName,'.xml');
  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cnNodeMain, true);   // Form位置再現
    Main.Left         := XmlIni.ReadInteger(cnLeft, 0);
    Main.Top          := XmlIni.ReadInteger(cnTop,  0);
    if (Main.Left = 0) and (Main.Top = 0) then
      begin
      Main.Left := (Screen.Width - Main.Width)   Div 2;
      Main.Top  := (Screen.Height - Main.Height) Div 2;
      end;

    XmlIni.OpenNode(cnNodeAlert, true);   // Alertの設定
    PreAOSTime        := XmlIni.ReadInteger(cnPreAOSTime, 3);
    slAlert.CommaText := XmlIni.ReadString(cnAlert,  '1000,2000,750,500,500,500') + ',0,0,0,0,0,0';
    for i := 0 to 5 do
      aryAlert[i] := StrToInt(slAlert[i]);
  finally
    FreeAndNil(XmlIni);
  end;

  Reg     := Tregistry.Create(KEY_READ);
  Gs232   := TGs232.Create(Self);
  ReadIniFile2();
  ReadParentInifile;
  Init := true;      // FormActivateの初期化のため
  MoveFormToBelow;   // CALSAT32の下にFormを移動する
end;

procedure TMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
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
  if not Init then    // APP起動時1回だけ実行する
    exit;

  XmlIni     := TXmlIniFile.Create(XmlIniFile);
  try
    if not XmlIni.OpenNode(cnNodeCom, false) then    // 初めて起動した時、オプッション画面を表示する
      while not ShowOptions do                      // XMLに'Com'のNodeが存在するかどうかで判断
        begin

        end;
  finally
    FreeAndNil(XmlIni);
  end;
  bbGroupeSet(true);
  DoLoop;
  Init := false;    // FormActivateの初期化のため
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  StopTracking;
  XmlIni := TXmlIniFile.Create(XmlIniFile);
  try
    XmlIni.OpenNode(cnNodeMain, true);
    XmlIni.WriteInteger(cnLeft, Main.Left);
    XmlIni.WriteInteger(cnTop, Main.Top);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;

    XmlIni.OpenNode(cnNodeAlert, true);
    XmlIni.WriteInteger(cnPreAOSTime, PreAOSTime);
    slAlert.Clear;
    for i := 0 to Length(aryAlert) - 1 do
      slAlert.Add(IntToStr(aryAlert[i]));
    XmlIni.WriteString(cnAlert,  slAlert.CommaText);
    XmlIni.CloseNode;
    XmlIni.UpdateFile;
  finally
    FreeAndNil(XmlIni);
  end;
  GS232.Close(gs232.ParkingAz, gs232.ParkingEl, gs232.AzOffset, true);
  FreeAndNil(slAlert);
  if Reg <> Nil then
    FreeAndNil(Reg);
  if Gs232 <> Nil then
    FreeAndNil(Gs232);
end;

procedure TMain.ReadIniFile2();
begin
  XMLIni := TXMLIniFile.Create(XMLIniFile);
  try
    XmlIni.OpenNode(cnNodeApp, true);      //  '/Options/App'に設定
    ParentApp         := XmlIni.ReadString(cnApp, 'CALSAT32');
    ParentInifile     := XmlIni.ReadString(cnInifile, '');
    ElementsFile      := XmlIni.ReadString(cnElements, '');
    RegKey            := XmlIni.ReadString(cnRegKey, '');

    XMLIni.OpenNode(cnNodeGS232, true);
    Timer1.Interval := XmlIni.ReadInteger(cnInterval, 2000);
    gs232.AzRotator       := TAzRotator(XmlIni.ReadInteger(cnAzRotator, 0));
    gs232.ElRotator       := TElRotator(XmlIni.ReadInteger(cnElRotator, 0));
    gs232.AzOffset        := XMLIni.ReadInteger(cnAzOffset, 0);
    gs232.RotateSpeed     := XMLIni.ReadInteger(cnRotateSpeed, 1);
    gs232.ParkingAz       := XMLIni.ReadInteger(cnParkingAz, 0);
    gs232.ParkingEl       := XMLIni.ReadInteger(cnParkingEl, 0);
    gs232.GoParking       := XMLIni.ReadBool(cnGoParking, true);
    RotateMode      := TRotateMode(XMLIni.ReadInteger(cnRotateMode, 0));
  finally
    FreeAndNil(XMLIni);
  end;
end;

function TMain.ReadParentInifile(): boolean;
var
  Ini: TIniFile;
begin
  result := true;
  if not FileExists(ParentInifile) then        //  ファイルの存在確認
    begin
    result := false;
    MessageDlg(ParentInifile + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;

//  観測地点のデータ読み込み
  Ini :=TIniFile.Create(ParentInifile);
  try
    with ObsPoint do
      begin
      if ParentAPP = cnCalsat then
        begin
        TimeOffset    := Ini.ReadFloat('TIMEOFFSET', 'TimeOffset', 32400) / 86400;  //  秒単位の時差を日単位に変換
        LocalTimeStr  := Ini.ReadString('TIMEOFFSET', 'ChrLST', 'JST');
        end
      else if ParentAPP = 'CALSAT99' then
        begin
        TimeOffset    := 9;
        LocalTimeStr  := 'JST';
        end
      else if ParentAPP = 'KSAT' then
        begin
        TimeOffset    := Ini.ReadFloat('JIKAN', 'JISA', 9) / 86400;       //  日単位の時差
        if TimeOffset >= 0 then
          LocalTimeStr  := 'UTC+' + FloatToStr(TimeOffset)
        else if TimeOffset <= 0 then
          LocalTimeStr  := 'UTC-' + FloatToStr(TimeOffset)
        else
          LocalTimeStr  := 'UTC';
        end;
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TMain.btnTrackingClick(Sender: TObject);
begin
  SetTracking(not AutoTracking);
end;

procedure TMain.CreateParams(var Params: TCreateParams);
begin
//  フォームのタイトルを表示しない
// （FormのBorderStyleをbsNoneにする必要あり）
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_BORDER;
  end;

procedure TMain.MoveFormToBelow;
var
  CalsatWnd: HWND;
  CalsatRect: TRect;
  FocusedWnd: HWND;
begin
//  CALSAT32の直下にFormを移動する
    CalsatWnd := FindWindow(nil, pWidecHAR(cnCALSATText));
    if CalsatWnd <> 0 then
      begin
      GetWindowRect(CalsatWnd, CalsatRect);
      if not CalsatRect.IsEmpty then
        begin
        if Left <> CalsatRect.Left   then
          Left := CalsatRect.Left + 1;
        if Top <> CALSATRECT.Bottom - 3  then
          Top := CALSATRECT.Bottom - 3;
        end;

      FocusedWnd := GetForegroundWindow;
      if (FocusedWnd <> CalsatWnd) and (FocusedWnd <> self.Handle) then
        begin
        wasFocused := false;
        end
      else if (FocusedWnd = CalsatWnd) then
        begin
        if not wasFocused then
          begin
          wasFocused := true;
          Windows.SetForegroundWindow(self.Handle);
          Windows.SetForegroundWindow(CalsatWnd);
          end;
        end
      else
        begin
        if not wasFocused then
          begin
          wasFocused := true;
          Windows.SetForegroundWindow(CalsatWnd);
          Windows.SetForegroundWindow(self.Handle);
          end;
        end;
      end;
end;

////////////////////////////////////////////////////////////////////////////
//
//  手動追尾のボタンを押した時の処理
//  (ボタンのTagにボタンごとの値が設定してある)
//  (ボタンを押すと、そのボタンのCancel=TRUEになる)
//
///////////////////////////////////////////////////////////////////////////
procedure TMain.bbGroupeDownClick(Sender: TObject);
var
  s: string;
begin
  if TSpeedButton(Sender).Down then
    begin
    ControlMode := ctManual;
    bbLeft.Enabled := false;
    bbRight.Enabled := false;
    bbDown.Enabled := false;
    bbUp.Enabled := false;
    TSpeedButton(Sender).Enabled := true;
    case TSpeedButton(Sender).Tag of
      0:  s := 'L';            // Left回転
      1:  s := 'R';            // Right回転
      2:  s := 'D';            // Down回転
      3:  s := 'U';            // Up回転
      end
    end
  else
    begin
    ControlMode := ctNone;
    bbLeft.Enabled := true;
    bbRight.Enabled := true;
    bbDown.Enabled := true;
    bbUp.Enabled := true;
    case TSpeedButton(Sender).Tag of
      0, 1: s := 'A';            // Left,Right回転停止
      2, 3: s := 'E';            // Up,Down回転停止
      end;
    end;
  ClickedButton := TSpeedButton(Sender);
  Gs232.DoCmd(s);
end;

procedure TMain.bbGroupeSet(Value: boolean);
begin
  if Options <> nil then
    begin
    bbLeft.Enabled := Value;
    bbRight.Enabled := Value;
    if gs232.ElRotator = elNone then
      begin
      bbUp.Enabled := false;
      bbDown.Enabled := false;
      end
    else
      begin
      bbUp.Enabled := Value;
      bbDown.Enabled := Value;
      end;
    ControlMode := ctNone;
    end;
end;

procedure TMain.Timer1Timer(Sender: TObject);
begin
  DoTracking;
  MoveFormToBelow;
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

procedure TMain.actDebugExecute(Sender: TObject);
var
  s: string;
begin
  Timer1.Enabled := false;
//  Debug.ShowModal;
//  s :=  Satellite.Orbit.Satellite;
//  s :=  #13 + FloatToStr(Satellite.Orbit.SatCoord.Azimuth);
//  s :=  #13 + FloatToStr(Satellite.Orbit.SatCoord.Elevation);
//  ShowMessage(s);
  Timer1.Enabled := true;
end;

procedure TMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.actGotoParkingExecute(Sender: TObject);
begin
  Gs232.GotoParking();
end;

procedure TMain.actOptionsExecute(Sender: TObject);
begin
  StopTracking;
  if ShowOptions then
    begin
    GS232.Close(Gs232.ParkingAz, Gs232.ParkingEl, Gs232.AzOffset, false);
    Init := true;
    DoLoop;
    end;
end;

function TMain.ShowOptions(): boolean;
var
  Options: TOptions;
begin
  result := false;
  Options := TOptions.Create(self);
  try
    if Options.ShowModal = mrOK then
      begin
      ReadIniFIle2();
      ReadParentInifile;
      result := true;
      end;
  finally
    FreeAndNil(Options);
  end;
end;

procedure TMain.btnTestClick(Sender: TObject);
//var
//  AosAz,AosEl,MaxEl,MaxAz,LosAz,LosEl: integer;
//  Offset: double;
begin
  Timer1.Enabled := false;

//Case　1
//  AosAz := 210; AosEl := 2;
//  MaxAz := 269; MaxEl := 32;
//  LosAz := 330; LosEl := 2;

//Case　2
//  AosAz := 333; AosEl := 2;
//  MaxAz := 269; MaxEl := 32;
//  LosAz := 208; LosEl := 2;

//Case　3
//  AosAz := 155; AosEl := 2;
//  MaxAz := 40; MaxEl := 32;
//  LosAz := 358; LosEl := 2;

//Case　4
//  AosAz := 358; AosEl := 2;
//  MaxAz := 40; MaxEl := 32;
//  LosAz := 155; LosEl := 2;

//  Gs232.Start(AosAz,AosEl,Offset);
//  gs232.Move(AosAz,AosEl,Offset);
//  gs232.Move(MaxAz,MaxEl,Offset);
//  gs232.Move(LosAz,LosEl,Offset);
end;

//////////////////////////////////////////////////////////////////////////
//
//  自動追尾の処理  (Timer1)
//
//////////////////////////////////////////////////////////////////////////
function TMain.DoLoop(): boolean;
var
  i: integer;
begin
  result := true;

//  軌道要素を読み込み
//  Satellite.ElementFile := OrbitalFile;
//-------------------
  reg.RootKey := HKEY_CURRENT_USER;
  if not Reg.OpenKey(RegKey, False) then
    begin;
    MessageDlg(cnCalsat + 'のレジストリが読めません', mtError, [mbOK], 0);
    Exit;
    end;

  Sat := Reg.ReadString('Satellite');   //  追尾対象衛星名を取得
  i   := Pos('[', Sat);                 //  衛星名[Mode]の衛星名のみを選択するため
  if i <> 0 then
    Sat := Copy(Sat, 1, i - 1);
  EpochRev := StrToInt(Reg.ReadString('Revolution'));   //  CALSAT32の時、周回番号を取得

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
  AutoTracking := Tracking;
  if Tracking then          //  自動追尾中に押された
    begin
    btnTracking.Caption := '&Tracking Off';
    btnTracking.Down    := true;
    bbUp.Enabled    := false;
    bbDown.Enabled  := false;
    bbLeft.Enabled  := false;
    bbRight.Enabled := false;
    ControlMode := ctTracking;
    Timer1.Enabled  := true;
    end
  else                          //  自動追尾でない時に押された
    begin
    btnTracking.Caption := '&Tracking On';
    btnStopTracking.Down:= true;
    if gs232.ElRotator <> elNone then
      begin
      bbUp.Enabled    := true;
      bbDown.Enabled  := true;
      end;
    bbLeft.Enabled  := true;
    bbRight.Enabled := true;
    ControlMode := ctNone;
    if GS232.Opend then
      GS232.Stop;
    end;
end;

procedure TMain.StopTracking();
begin
  SetTracking(false);
  Timer1.Enabled := false;
end;

//  Timer1により、規定値2秒に1回処理される。Intervalの値画設定される
procedure TMain.DoTracking;
var
  i: integer;
  s: string;
  nw: TDateTime;
  AosTime: string;
  MaxTime: string;
  LosTime: string;
begin
  imgFocused.Visible  :=  Main.Focused;

  Nw      := ModifiedJulianDateToDateTime(StrToFloat(Reg.ReadString('MJD')));  // Calsat32の現在時刻　UTCだろう
  DateTimeToString(s, 'mm/dd hh:nn:ss', Nw + ObsPoint.TimeOffset);
  lblYMD.Caption := s + '(' + ObsPoint.LocalTimeStr + ')';

  //  Calsatから情報取得・表示
  Sat := Reg.ReadString('Satellite');   //  追尾対象衛星名を取得
  i   := Pos('[', Sat);                 //  衛星名[Mode]の衛星名のみを選択するため
  if i <> 0 then
    Sat := Copy(Sat, 1, i - 1);
  if ParentApp = cnCalsat then
    EpochRev := StrToInt(Reg.ReadString('Revolution'))   //  CALSAT32の時、周回番号を取得
  else
    EpochRev := 0;

  if (Nw > VisibleTime.LosDateTime + 1 / 24 / 60) then      //  LOS経過1分後,強制的にRevが変わったことにする
    EpochRev := 0;

  if (Sat <> PSat)                                                // 追尾対象衛星画変わったら
  or (not AutoTracking and (EpochRev <> PEpochRev)) then          // 自動追尾中にエポックが変わったら
      begin
      Alert       := alNone;
      PSat        := Sat;
      PEpochRev   := EpochRev;
      if AutoTracking then                  //   CALSAT32の衛星が変わったら、自動追尾を停止する
        SetTracking(false);

      CalcVisibleTime(Nw);                  //　CALSAT32のFormから次に対象とするオービットデータを得る
      DateTimeToString(AosTime, 'hh:nn', VisibleTime.AosDateTime + ObsPoint.TimeOffset);
      DateTimeToString(MaxTime, 'hh:nn', VisibleTime.MaxDateTime + ObsPoint.TimeOffset);
      DateTimeToString(LosTime, 'hh:nn', VisibleTime.LosDateTime + ObsPoint.TimeOffset);
      s := format('(AOS) %s Az=%3.0f    (MAX) %s El=%2.0f Az=%3.0f    (LOS) %s Az=%3.0f',
              [AosTime, VisibleTime.AOSAz, MaxTime, VisibleTime.MaxEl, VisibleTime.MaxAz,
               LosTime, VisibleTime.LOSAz]);
      lblAOSAz.Caption := s;

      // Normal,Overlap,Flipモードの判断
      Gs232.JudgeRotatorMode(visibleTime.AosAz, VisibleTime.MaxAz, VisibleTime.LosAz, gs232.AzOffset);
      RotateModeName  := cnRotateModeName[Ord(Gs232.RotateMode)];
      lblRotateModeName.Caption:= 'Mode : ' + RotateModeName;
      end;

//  追尾対象衛星の現在Az,Elを表示
  lblSatellite.Caption    := Sat;
  SatelliteAz   := round(StrToFloat(Reg.ReadString('Azimuth')));    // Calsat32からの現時点のAz
  SatelliteEl   := round(StrToFloat(Reg.ReadString('Elevation')));  // Calsat32からの現時点のEl
  lblSatelliteAz.Caption  := IntToStr(SatelliteAz) + '°';
  lblSatelliteEl.Caption  := IntToStr(SatelliteEl) + '°';

//  アラートBeepの判断
  if AutoTracking then
    if (Alert < alLOS) and (Nw > VisibleTime.LosDateTime) then
      begin
      Windows.Beep(aryAlert[4], aryAlert[5]);
      Alert := alLOS;
      end
    else if (Alert < alAOS) and (Nw > VisibleTime.AosDateTime) and (Nw < VisibleTime.LosDateTime) then
      begin
      Windows.Beep(aryAlert[2], aryAlert[3]);
      Alert := alAOS;
      end;
  if (Alert < alPreAOS) and (Nw > VisibleTime.AosDateTime - PreAOSTime / 24 / 60) and (Nw < VisibleTime.AosDateTime)then
    begin
    Windows.Beep(aryAlert[0], aryAlert[1]);
    Alert := alPreAOS;
    end;

//  GS232からアンテナのAz,El情報取得
  if Gs232.Opend and GS232.GetPos(AntennaAz, AntennaEl) then      // AntAz、AntElにアンテナの現在位置が帰る
    begin
    lblAntennaAz.Caption  :=  IntToStr(gs232.AntennaAz) + '°';
    lblAntennaEl.Caption  :=  IntToStr(gs232.AntennaEl) + '°';
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
//    lblAntennaAz.Caption  :=  '';          // オートトラッキング中に停止する原因？
//    lblAntennaEl.Caption  :=  '';
//    lblRotatorAz.Caption  :=  '';
//    lblRotatorEl.Caption  :=  '';
//    if AutoTracking then                   //   Errorなら、自動追尾を停止する
//      begin
//      exit;
//      end;
    end;

  if ControlMode = ctManual then           //  マニュアルボタンが押されて時の限界で回転を止める
    begin
    case ClickedButton.Tag of
      0:
        if Gs232.FRotatorAz <= 0 then            // 左回転時
          begin
          ClickedButton.Down := False;
          ClickedButton.Click;
          end;
      1:
        begin
        if ((Gs232.AzRotator = az450) and (Gs232.RotatorAz >= 450))
        or ((Gs232.AzRotator = az360) and (Gs232.RotatorAz >= 360)) then
          ClickedButton.Down := False;
          ClickedButton.Click;
        end;
      2:
        begin
        if Gs232.RotatorEl <= 0 then
          ClickedButton.Down := False;
          ClickedButton.Click;
        end;
      3:
        begin
        if ((Gs232.ElRotator = el180) and (Gs232.RotatorEl >= 180))
        or ((Gs232.ElRotator = el90) and (Gs232.RotatorEl >= 90)) then
          ClickedButton.Down := False;
          ClickedButton.Click;
        end;
      end;
    end;

//  自動追尾でないなら終了
  if NOT AutoTracking then
    exit;

//  アンテナと衛星の方向があっているなら何も処理しない
//  衛星の方向にアンテナを動かす
  if (SatelliteAz = Gs232.AntennaAz) and (SatelliteEl = Gs232.AntennaEl) then
    exit;

  GS232.Move(SatelliteAz, SatelliteEl);
  if GS232.isLos  then
    begin
    SetTracking(false);     // LOSしたとき
    end;
end;

procedure TMain.CalcVisibleTime(Nw: TDateTime);
  var
    CalsatWnd: HWND;
    hData: THandle;
    SL: TStringList;
    i, j: integer;
    s: string;
    Dt: TdateTime;
    RecCnt: integer;
  begin
    CalsatWnd := FindWindow(nil, PWideChar(cnCalsatText));
    if CalsatWnd <> 0 then
      begin
      hData := 0;
      EnumChildWindows(CalsatWnd, @EnumerateChildWindows, hData);
      Sl := TStringList.Create;
      try
        SL.Delimiter := ' ';
        SL.DelimitedText := VisibleTimeString;
        if SL.Count = 0 then
          begin
          VisibleTime.AOSDateTime := Nw;
          VisibleTime.MaxDateTime := Nw;
          VisibleTime.LosDateTime := Nw;
          exit;
          end;
        RecCnt := (sl.Count div 11);
        for i := 0 to RecCnt - 1 do
          begin
          j := i * 11;
          s := sl[j] + ' ' + sl[j + 7];   // LOSの時間
          dt := strToDateTime(s) - ObsPoint.TimeOffset;
          if (dt >= Nw) or (i = RecCnt- 1) then  // LOSが現在時より大きい時、対象 OR 最後のレコード時
            begin
            VisibleTime.LosDateTime := dt;
            s := sl[j] + ' ' + sl[j + 1];   // AOSの時間
            VisibleTime.AOSDateTime := strToDateTime(s) - ObsPoint.TimeOffset;
            s := sl[j] + ' ' + sl[j + 4];   // Maxの時間
            VisibleTime.MaxDateTime  := strToDateTime(s) - ObsPoint.TimeOffset;
            VisibleTime.AosAz := StrToFloat(sl[j + 2]);
            VisibleTime.MaxAz := StrToFloat(sl[j + 6]);
            VisibleTime.MaxEl := StrToFloat(sl[j + 5]);
            VisibleTime.LosAz := StrToFloat(sl[j + 8]);
            exit;
            end;
          end;
          s := 'Error = Not found orbit data';  //   ここに来ないはず

      finally
        FreeAndNil(SL);
      end;
      end;
end;

end.


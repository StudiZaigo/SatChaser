unit Gs232;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  Dialogs, Vcl.ExtCtrls,
  ShlObj,  // for debug
  Math,
  Cport, CPortX;

Type
  TAzimuthAngle = (az360, az450);
  TCenterPosition = (cpNorth, cpSouth);
  TBoundPosition = (bpNone, bpCWBound, bpCCWBound);

type
  TReachPresetEvent   = procedure(Sender: TObject) of object;
  TReachBoundEvent    = procedure(Sender: TObject; Bound: TBoundPosition) of object;
  TChangeAzElEvent    = procedure(Sender: TObject) of object;
  TChangeRelaysEvent  = procedure(Sender: TObject) of object;
  TChangeRelayEvent   = procedure(Sender: TObject; Index: Integer; State: boolean) of object;

Type
  TRelayArray = class(TComponent)
  private
    FRelay: array[0..15] of boolean;
    FEnable: boolean;
    FCount: Integer;
    FChangeRelay: TChangeRelayEvent;
    FChangeRelays: TChangeRelaysEvent;

    function GetRelay(Index: integer): boolean;
    procedure SetRelay(Index: integer; const Value: boolean);
    procedure SetCount(const Value: Integer);
  public
    property Count: Integer read FCount write SetCount;
    property Enable: boolean read FEnable;
    property Relay[Index: integer]: boolean read GetRelay write SetRelay;
    property ChangeRelays: TChangeRelaysEvent read FChangeRelays write FChangeRelays;
    property ChangeRelay: TChangeRelayEvent read FChangeRelay write FChangeRelay;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

//**********************************************************************//
//                                                                      //
//  GS-23のクラス                                                       //
//                                                                      //
//**********************************************************************//
type
  TGs232 = class(TComponent)
  private
    { Private 宣言 }
    bInitialization: boolean;

    ComPort: TCPortX;
    Timer1: TTimer;

    iAzBefore: integer;  // for Event when Azimuth Data change
    iElBefore: integer;  // for Event when Elevation Data change
    iAzCwBound: integer;
    iAzCcwBound: Integer;

//    tSl: TStringList;   // for debug;

    FActive: boolean;
    FAzimuthD: integer;
    FAzimuthR: double;
    FAzOffsetD: integer;
    FAzOffsetR: double;
    FAzPresetD: integer;
    FAzPresetR: double;
    FAzHomeD: integer;
    FAzHomeR: double;
    FElevationD: integer;
    FElevationR: double;
    FElOffsetD: integer;
    FElOffsetR: double;
    FElPresetD: integer;
    FElPresetR: double;
    FElHomeD: integer;
    FElHomeR: double;
    FAzimuthAngle: TAzimuthAngle;
    FElEnable: boolean;
    FCenterPosition: TCenterPosition;
    FReturnHome: boolean;
    FRotationSpeed: string;
    FModel: string;
    FCommandB: string;
    FCommandA: string;
    FOrgElevationD: integer;
    FOrgElevationR: double;
    FOrgAzimuthD: integer;
    FOrgAzimuthR: double;
    FOverlap: boolean;

    FReachBound: TReachBoundEvent;
    FReachPreset: TReachPresetEvent;
    FChangeAzEl: TChangeAzElEvent;
    FMoving: boolean;
    FPresetting: boolean;
    FRelayArray: TRelayArray;
    FCurrent: boolean;
    FTempOffsetD: integer;
    FTempOffsetR: double;

    procedure GetDirectionOnRx(Sender: TObject; Str: string);
    function RoundDegree(Value: integer): integer;
    function RoundRadian(Value: double): double;
    procedure Timer1Timer(Sender: TObject);

    function GetBaudRate: string;
    function GetDataBits: string;
    function GetFlowControl: string;
    function GetParity: string;
    function GetPort: string;
    function GetStopBits: string;
    procedure SetBaudRate(const Value: string);
    procedure SetDataBits(const Value: string);
    procedure SetFlowControl(const Value: string);
    procedure SetParity(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetStopBits(const Value: string);
    procedure SetReturnHome(const Value: boolean);
    procedure SetCenterPosition(const Value: TCenterPosition);
    procedure SetModel(const Value: string);
    procedure SetCommandA(const Value: string);
    procedure SetCommandB(const Value: string);
    procedure SetRotationSpeed(const Value: string);
    procedure SetElEnable(const Value: boolean);
    procedure setAzimuthAngle(const Value: TAzimuthAngle);
    procedure ComException(Sender: TObject; ComException: TComExceptions; ComPortMessage: string; WinError: Int64; WinMessage: string);
    procedure ComError(Sender: TObject; Errors: TComErrors);
    procedure SetCurrent(const Value: boolean);
    procedure DoSetCenterPosition;
    procedure DoSetAzimuthAngle;
    procedure DoDoCmd(Cmd: string; recursive: boolean);
    procedure SetTempOffsetD(const Value: integer);
    procedure SetTempOffsetR(const Value: double);

  public
    { Public 宣言 }
    property Active: boolean read FActive;
    property Current: boolean read FCurrent write SetCurrent;
    property Model: string read FModel write SetModel;
    property CenterPosition: TCenterPosition read FCenterPosition write SetCenterPosition;
    property AzimuthAngle: TAzimuthAngle read FAzimuthAngle write setAzimuthAngle;
    property RotationSpeed: string read FRotationSpeed write SetRotationSpeed;
    property AzHomeD: integer read FAzHomeD;
    property AzHomeR: double read FAzHomeR;
    property ElHomeD: integer read FElHomeD;
    property ElHomeR: double read FElHomeR;
    property ReturnHome: boolean read FReturnHome write SetReturnHome;
    property AzOffsetD: integer read FAzOffsetD;
    property AzOffsetR: double read FAzOffsetR;
    property ElOffsetD: integer read FElOffsetD;
    property ElOffsetR: double read FElOffsetR;
    property CommandA: string read FCommandA write SetCommandA;
    property CommandB: string read FCommandB write SetCommandB;
    property AzimuthD: integer read FAzimuthD;
    property AzimuthR: double read FAzimuthR;
    property ElevationD: integer read FElevationD;
    property ElevationR: double read FElevationR;
    property AzPresetD: integer read FAzPresetD;
    property AzPresetR: double read FAzPresetR;
    property ElPresetD: integer read FElPresetD;
    property ElPresetR: double read FElPresetR;
    property ElEnable: boolean read FElEnable write SetElEnable;
    property OrgAzimuthD: integer read FOrgAzimuthD;
    property OrgAzimuthR: double read FOrgAzimuthR;
    property OrgElevationD: integer read FOrgElevationD;
    property OrgElevationR: double read FOrgElevationR;
    property Overlap: boolean Read FOverlap;
    property Moving: boolean read FMoving;
    property Presetting: boolean read FPresetting;
    property RelayArray: TRelayArray read FRelayArray;
    property ReachPreset: TReachPresetEvent read FReachPreset write FReachPreset;
    property ReachBound: TReachBoundEvent read FReachBound write FReachBound;
    property ChangeAzEl: TChangeAzElEvent read FChangeAzEl write FChangeAzEl;
    property TempOffsetD: integer read FTempOffsetD write SetTempOffsetD;
    property TempOffsetR: double read FTempOffsetR write SetTempOffsetR;


    // RS232関連
    property BaudRate: string read GetBaudRate write SetBaudRate;
    property DataBits: string read GetDataBits write SetDataBits;
    property FlowControl: string read GetFlowControl write SetFlowControl;
    property Parity: string read GetParity write SetParity;
    property Port: string read GetPort write SetPort;
    property StopBits: string read GetStopBits write SetStopBits;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Open();
    procedure Close();
    procedure DoCmd(Cmd: string);
    function  GetDirection(): boolean overload;
    procedure GoHome();
    procedure GoCCW;
    procedure GoCW;
    procedure GoDown;
    procedure GoLeft;
    procedure GoPreset(); overload;
    procedure GoPreset(Az: integer); overload;
    procedure GoPreset(Az: double); overload;
    procedure GoPreset(Az, El: integer); overload;
    procedure GoPreset(Az, El: double); overload;
    procedure GoRight;
    procedure GoUp;
    procedure ResetPreset;
    procedure SetPreset(Az: integer); overload;
    procedure SetPreset(Az: double); overload;
    procedure SetPreset(Az, El: integer); overload;
    procedure SetPreset(Az, El: double); overload;
    procedure Stop();
    procedure SetOffset(Az: integer); overload;
    procedure SetOffset(Az: Double); overload;
    procedure SetOffset(Az, El: integer); overload;
    procedure SetOffset(Az, El: Double); overload;
    procedure SetHome(Az: integer); overload;
    procedure SetHome(Az: Double); overload;
    procedure SetHome(Az, El: integer); overload;
    procedure SetHome(Az, El: Double); overload;
    function DoRecv(Len: integer): string;
  end;

  procedure Wait(t: integer);
  function GetSpecialFolder(Folder :integer):string;

const
  Pi2: double = 2 * pi;                //  2 × PI
  RadDivDeg: double = 2 * pi / 360;    //  1度あたりのラジアン値
  cAzimuthAngle: array[0..1] of string = ('P36','P45');
  cCenterPosition: array[0..1] of string = ('N','S');

implementation

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  GS-232
//
/////////////////////////////////////////////////////////////////////////////////////////////
constructor TGs232.Create(Owner: TComponent);
//var
//  i: integer;
begin
  inherited Create(Owner);
  bInitialization := true;;

  ComPort := TCPortX.Create(self);
  Timer1  := TTimer.Create(Self);
  try
    Timer1.Enabled   :=  false;
    Timer1.Interval  :=  100;     //　0.1秒ごとに方位を読み取る
    Timer1.OnTimer   :=  Timer1Timer;
  finally
  end;
  FActive       := false;
  FAzimuthD     := 0;
  FAzimuthR     := 0;
  FOrgAzimuthD  := 0;
  FOrgAzimuthR  := 0;
  FAzOffsetD    := 0;
  FAzOffsetR    := 0;
  FAzPresetD    := 0;
  FAzPresetR    := 0;
  FAzHomeD      := 0;
  FAzHomeR      := 0;
  FElevationD   := 0;
  FElevationR   := 0;
  FOrgElevationD := 0;
  FOrgElevationR := 0;
  FElOffsetD    := 0;
  FElOffsetR    := 0;
  FElPresetD    := 0;
  FElPresetR    := 0;
  FElHomeD      := 0;
  FElHomeR      := 0;
  FAzimuthAngle := az360;
  FElEnable     := false;
  FOverlap      := false;
  FMoving       := false;
  FRelayArray   := TRelayArray.Create(Self);
  FRelayArray.FChangeRelays := nil;
  FRelayArray.FChangeRelay  := nil;
end;

destructor TGs232.Destroy;
begin
  FreeAndNil(FRelayArray);
  FreeAndNil(ComPort);
  FreeAndNil(Timer1);
  inherited Destroy;
end;

procedure TGs232.Open();
begin
  Comport.Delimiter   := #10;     // GS232用固定
  Comport.TriggersOnRxChar := false;
//    Comport.OnRecvMsg := GetDirectionOnRx;   // RTX-59,GS-232共通化するため使わない
  Comport.OnException  := ComException;
  ComPort.OnError      := ComError;
  ComPort.Open;
  ComPort.Clear;

  if bInitialization then
    begin
    bInitialization := false;
    ComPort.SendStr('K0' + #13);       // Rom Initialize for RTC-59  立ち上げ後いちっどだけ処理する
    end;
  ComPort.SendStr('S' + #13);        // 動作の停止
  ComPort.SendStr('I0' + #13);       // データを自動で送らない
  DoSetAzimuthAngle;
  DoSetCenterPosition;

  iAzBefore  := 9999;
  iElBefore  := 9999;
  Timer1.Enabled := true;
  FActive := True;

  SetCenterPosition(FCenterPosition);
//  Tsl := TStringList.Create;  // for debug;
  end;

procedure TGs232.Close();
var
  MyDoc: string;
begin
  Timer1.Enabled  := false;
//  MyDoc :=GetSpecialFolder(CSIDL_PERSONAL);
//  sl.SaveToFile(MyDoc + '\test.csv');
//  FreeAndNil(tsl); // for debug

  ComPort.SendStr('' + #13);
  ComPort.Clear;
  ComPort.Close;
  FActive := false;
end;

procedure TGs232.ComException(Sender: TObject; ComException: TComExceptions;
  ComPortMessage: string; WinError: Int64; WinMessage: string);
begin
  Close;
  showMessage('ComException ' + ComPortMessage);
end;

procedure TGs232.ComError(Sender: TObject; Errors: TComErrors);
begin
  Close;
  showMessage('ComError ' + ComErrorsToStr(Errors));
end;

procedure TGs232.DoCmd(Cmd: string);          // Cmdの内容を解析しながら送る
begin
  if FActive then
    DoDoCmd(Cmd, true);
end;

procedure TGs232.DoDoCmd(Cmd: string; recursive: boolean);          // Cmdの内容を解析しながら送る
var
  b: boolean;
  i,j: integer;
  s,s1, s2,sz: string;
  sl: TStringList;
begin
  sl := TstringList.Create;
  try
    sl.CommaText := UpperCase(Cmd);
    for i := 0 to sl.Count - 1 do
      begin
      s   := sl.Strings[i];
      s1  := Copy(s,1,1);
      s2  := Copy(s, 1, 4);
      if (s2 = 'WAIT') then
        begin
        sz := Copy(s, 4, 32);
        if TryStrToInt(Copy(s, 4, 32), j) then
          begin
          Wait(j);
          end;
        continue;
        end;
      if (s1 = 'L') or (s1 = 'R') or (s1 = 'M') or (s1 = 'U') or (s1 = 'D') then
        begin
        if recursive then
          DoDoCmd(FCommandB, false);
        ComPort.SendStr(s + #13);
        continue;
        end;
      if (s1 = 'S') then
        begin
        ComPort.SendStr(s + #13);
        if recursive then
          DoDoCmd(FCommandA, false);
        continue;
        end;
      if (s1 = 'Y') then
        begin
        TryStrToInt(Copy(s, 2, 1), j);
        if Copy(s, 3, 1) = '1' then
          b := true
        else
          b := false;
        RelayArray.SetRelay(j - 1, b);
        ComPort.SendStr(s + #13);
        continue;
        end;
      ComPort.SendStr(s + #13);
      end;
  finally
    FreeAndNil(sl);
  end;
end;

function TGs232.DoRecv(Len: integer): string;
var
  s: string;
begin
  s := '';
  if FActive then
    s := ComPort.RecvStr(Len);
  result := s;
end;

function TGs232.GetBaudRate: string;
begin
  result := Comport.BaudRate;
end;

function TGs232.GetDataBits: string;
begin
  result := ComPort.DataBits;
end;

procedure TGs232.GetDirectionOnRx(Sender: TObject; str: string);
var
  s,t: string;
begin
  s := str;
  if copy(s, 1,3) = 'AZ=' then
    t := copy(s, 4,3)
  else if copy(s, 1, 2) = '+0' then
    t := copy(s, 3, 3)
  else
    t := '0';
  if not TryStrToInt(t, FAzimuthD) then
    FAzimuthD := 0;
  FAzimuthD := RoundDegree(FAzimuthD + FAzOffsetD);
  FAzimuthR := DegToRad(FAzimuthD);
  FElevationD := 0;
  FElevationR := 0;
end;

function TGs232.GetDirection(): boolean;
var
  s,t,u: string;
begin
  result := true;
  try
    if not FElEnable then
      ComPort.SendStr('C' + #13)              // Azの読み取り指示
    else
      ComPort.SendStr('C2' + #13);            // Az,Elの読み取り指示
//    wait(20);                               // 入れるとデータが読み取れないことがある
    s := ComPort.RecvStr(11);

    if copy(s, 1,3) = 'AZ='then               // GS-232B,RTC-59
      begin
      t := copy(s, 4, 3);
      u := copy(s, 11,3);
      end
    else if copy(s, 1, 2) = '+0'then          // GS-232A
      begin
      t := copy(s, 3, 3);
      u := copy(s, 8, 3);
      end
    else
      begin
      t := '0';
      u := '0';
      end;
    if not TryStrToInt(t, FOrgAzimuthD) then
      FOrgAzimuthD := 0;
    if not TryStrToInt(u, FOrgElevationD) then
      FOrgElevationD := 0;

    FAzimuthD   := RoundDegree(FOrgAzimuthD + FAzOffsetD);
    FAzimuthR   := DegToRad(FAzimuthD);
    FElevationD := RoundDegree(FOrgElevationD + FElOffsetD);
    FElevationR := DegToRad(FEleVationD);
  finally

  end;
end;

function TGs232.GetFlowControl: string;
begin
  result := ComPort.FlowControl;
end;

function TGs232.GetParity: string;
begin
  result := ComPort.Parity;
end;

function TGs232.GetPort: string;
begin
  result := ComPort.Port;
end;

function TGs232.GetStopBits: string;
begin
  result := ComPort.StopBits;
end;

procedure TGs232.GoHome();
var
  s: string;
begin
  if FActive then
    begin
    s := format('M%.3d', [RoundDegree(FAzHomeD - FAzOffsetD)]);
    DoCmd(s);
    end;
end;

procedure TGs232.GoDown;
begin
  if FActive then
    DoCmd('D');
end;

procedure TGs232.GoCCW();
begin
  if FActive then
    DoCmd('L');
end;

procedure TGs232.GoCW();
begin
  if FActive then
   DoCmd('R');
end;

procedure TGs232.GoLeft;
begin
  GoCCW;
end;

// Preset位置へ移動
procedure TGs232.GoPreset();
var
  s: string;
begin
  if FActive and FPresetting then
    begin
    if FElEnable then
      s := format('W%.3d %.3d', [RoundDegree(FAzPresetD - FAzOffsetD - FTempOffsetD),
                              RoundDegree(FElPresetD - FElOffsetD)])
    else
      s := format('M%.3d', [RoundDegree(FAzPresetD - FAzOffsetD - FTempOffsetD)]);
    DoCmd(s);
    end;
end;

procedure TGs232.GoPreset(Az: double);
begin
  GoPreset(RadToDeg(Az));
end;

procedure TGs232.GoPreset(Az: integer);  // プリセット方向へ移動
var
  i: integer;
  s: string;
begin
  if FActive then
    begin
    FPresetting := true;
    i := RoundDegree(Az);
    s := format('M%.3d'#13, [RoundDegree(i - FAzOffsetD - FTempOffsetD)]);
    DoCmd(s);

    FAzPresetD := i;
    FAzPresetR := DegToRad(i);
    end;
end;

procedure TGs232.GoPreset(Az, El: double);
begin
  GoPreset(RadToDeg(Az), RadToDeg(El));
end;

procedure TGs232.GoPreset(Az, El: integer);
var
  i, j: integer;
  s: string;
begin
  if FActive and FPresetting then
    begin
    i := RoundDegree(Az);
    j := RoundDegree(El);
    s := format('W%.3d %.3d'#13, [RoundDegree(i - FAzOffsetD - FTempOffsetD), RoundDegree(j - FElOffsetD)]);
    DoCmd(s);
    FAzPresetD := i;
    FAzPresetR := DegToRad(i);
    FElPresetD := j;
    FElPresetR := DegToRad(j);
    end;
end;

procedure TGs232.GoRight;
begin
  GoCW;
end;

procedure TGs232.GoUp;
begin
  if FActive then
    DoCmd('U');
end;

procedure TGs232.ResetPreset;
begin
  SetPreset(0,0);
  FPresetting := false;
end;

function TGs232.RoundDegree(Value: integer): integer;
var
  i:  integer;
begin
  if FAziMuthAngle = az450 then
    if(Value >= 0) and (value <= 450) then
      begin
      result := Value;
      exit;
      end;
  i := trunc(frac(Value / 360) * 360);
  if i < 0 then
    i := i + 360;
  result := i;
end;

function TGs232.RoundRadian(Value: double): double;
var
  d: double;
begin
  if FAziMuthAngle = az450 then
    if(Value >= 0) and (value <= 3 * pi) then
      begin
      result := Value;
      exit;
      end;
  d := frac(Value / Pi2) * Pi2;
  if d < 0 then
    d := d + Pi2;
  result := d;
end;

// 360°、450°の設定をする
procedure TGs232.setAzimuthAngle(const Value: TAzimuthAngle);
begin
  FAzimuthAngle := Value;
  iAzCcwBound := 0;
  if FAzimuthAngle = az360 then
    iAzCwBound := 359
  else
    iAzCwBound := 450;
  if FActive then
    DoSetAzimuthAngle;
end;

procedure TGs232.DoSetAzimuthAngle;
begin
  if FActive then
    ComPort.SendStr(cAzimuthAngle[ord(FAzimuthAngle)] + #13);
end;


// BaudRateの設定（ComPortへ直接）
procedure TGs232.SetBaudRate(const Value: string);
begin
  ComPort.BaudRate := Value;
end;

// 北センタ（南起点）、南センタ（北起点）の設定
procedure TGs232.SetCenterPosition(const Value: TCenterPosition);
begin
  FCenterPosition := Value;
  if FActive then
    DoSetCenterPosition;
end;

procedure TGs232.DoSetCenterPosition;
var
  s: string;
begin
  if FActive then
    if FModel = 'RTC-59' then
      begin
      ComPort.SendStr('Z' + cCenterPosition[Ord(FCenterPosition)] + #13);
      end
    else if FModel = 'GS-232B' then
      begin
      ComPort.SendStr('H3' + #13);
      s := ComPort.RecvStr(30);
        ShowMessage('GS-232Bの動作未確認');
      end;
end;

// DataのBit長の設定（ComPortへ直接）
procedure TGs232.SetDataBits(const Value: string);
begin
  ComPort.DataBits := Value;
end;

// GetDirectionでAzのみか、AZ&EL両方のデータを得るかどうか判断する
procedure TGs232.SetElEnable(const Value: boolean);
begin
  FElEnable := Value;
end;

// FlowControlの設定（ComPortへ直接）
procedure TGs232.SetFlowControl(const Value: string);
begin
  ComPort.FlowControl := Value;
end;

// Home Positionの記憶
procedure TGs232.SetHome(Az: Double);
begin
  SetHome(RadToDeg(Az));
end;

procedure TGs232.SetHome(Az: integer);
var
  i: integer;
begin
  i := RoundDegree(Az);
  FAzHomeD := i;
  FAzHomeR := DegToRad(i);
end;

procedure TGs232.SetHome(Az, El: Double);
begin
  SetHome(RadToDeg(Az), RadToDeg(El));
end;

procedure TGs232.SetHome(Az, El: integer);
var
  i: integer;
begin
  i := RoundDegree(Az);
  FAzHomeD := i;
  FAzHomeR := DegToRad(i);
  FElHomeD := El;
  FElHomeR := DegToRad(El);
end;

// Model名の記憶
procedure TGs232.SetModel(const Value: string);
begin
  FModel := Value;
end;

// Offset Angleの記憶
procedure TGs232.SetOffset(Az: Double);
begin
  SetOffset(RadToDeg(Az));
end;

procedure TGs232.SetOffset(Az: integer);
begin
  FAzOffsetD := Az;
  FAzOffSetR := DegToRad(Az);
end;

procedure TGs232.SetOffset(Az, El: Double);
begin
  SetOffset(RadToDeg(Az), RadToDeg(El));
end;

procedure TGs232.SetOffset(Az, El: integer);
begin
  FAzOffsetD := Az;
  FAzOffSetR := DegToRad(Az);
  FElOffsetD := El;
  FElOffSetR := DegToRad(EL);
end;

// Parityの設定（ComPortへ直接）
procedure TGs232.SetParity(const Value: string);
begin
  ComPort.Parity := Value;
end;

// Preset Angleの記憶
procedure TGs232.SetPreset(Az: double);
begin
  SetPreset(Integer(Trunc(RadToDeg(RoundRadian(Az)))));
end;

procedure TGs232.SetPreset(Az: integer);
begin
  FAzPresetD  := Az;
  FAzPresetR  := DegToRad(Az);
  FPresetting  := true;
end;

procedure TGs232.SetPreset(Az, El: double);
begin
  SetPreset(Trunc(RadToDeg(Az)), Trunc(RadToDeg(El)));
end;

procedure TGs232.SetPreset(Az, El: integer);
begin
  FAzPresetD  := Az;
  FAzPresetR  := DegToRad(Az);
  FElPresetD  := El;
  FElPresetR  := DegToRad(El);
  FPresetting  := true;
end;

// Port番号の設定（ComPortへ直接）
procedure TGs232.SetPort(const Value: string);
begin
  ComPort.Port := Value;
end;

// 終了時にHome　position へ戻るかどうかの記憶
procedure TGs232.SetReturnHome(const Value: boolean);
begin
  FReturnHome := Value;
end;

// 回転スピードの設定
procedure TGs232.SetRotationSpeed(const Value: string);
begin
  FRotationSpeed := Value;
  if FActive then
    ComPort.SendStr('X' + Copy(Value, 1, 1) + #13);
end;

// 回転終了後に実行するコマンドを記憶
procedure TGs232.SetCommandA(const Value: string);
begin
  FCommandA := Value;
end;

// 回転開始前に実行するコマンドを記憶
procedure TGs232.SetCommandB(const Value: string);
begin
  FCommandB := Value;
end;

procedure TGs232.SetCurrent(const Value: boolean);
begin
  FCurrent := Value;
  if Fcurrent then
//    Timer1.Enabled := false
  else
//    Timer1.Enabled := false;
end;

// Port番号の設定（ComPortへ直接）
procedure TGs232.SetStopBits(const Value: string);
begin
  ComPort.StopBits := value;
end;

procedure TGs232.SetTempOffsetD(const Value: integer);
begin
  FTempOffsetD := Value;
  FTempOffsetR := DegToRad(Value);
end;

procedure TGs232.SetTempOffsetR(const Value: double);
begin
  FTempOffsetR := Value;
  FTempOffsetD := trunc(RadToDeg(Value));
end;

// 回転を止める
procedure TGs232.Stop();
begin
  if FActive then
    begin
    FMoving     := false;
    DoCmd('S');
    end;
end;

// タイマーでローテタからデータを得て、前回と異なるときEventを実行する
procedure TGs232.Timer1Timer(Sender: TObject);
var
  deg: integer;
begin
  if FActive then
    begin
    FOrgAzimuthD := 0;
    GetDirection();

    if FOrgAzimuthD > 360 then
      FOverlap := true
    else
      FOverlap := false;

    if FPresetting then
      begin
      if abs(FAzimuthD - FAzPresetD + FTempOffsetD) <= 2 then
        begin
//        Stop;
        if Assigned(FReachPreset) then
          begin
          if FPresetting then
            FReachPreset(self);
          FPresetting := False;
          end;
        end;
      end;

    if FMoving then
      begin
      Deg :=  FOrgAzimuthD;
      if FCenterPosition = cpNorth then
        begin
        if (Deg >=180) and (Deg <= 359) then
          Deg := deg - 360 + 180
        else
          Deg := Deg + 180;
        end;
      if Deg >= iAzCwBound then
        begin
        Stop;
        if Assigned(FReachBound) then
          FReachBound(self, bpCWBound);
        end
      else if Deg <= iAzCcwBound then
        begin
        Stop;
        if Assigned(FReachBound) then
          FReachBound(self, bpCCWBound);
        end;
      end;

    if (abs(iAzBefore - FAzimuthD) > 1) or (abs(iElBefore - FElevationD) > 1) then
      begin
      FMoving := true;
      if Assigned(FChangeAzEl) then
        begin
        iAzBefore := FAzimuthD;
        iElBefore := FElevationD;
        FChangeAzEl(self);
        end;
      end;
    end;
end;

{ TRelayArray }
constructor TRelayArray.Create(Owner: TComponent);
var
  i: integer;
begin
  inherited;
  for i := Low(FRelay) to High(FRelay) do
    FRelay[i] := false;
end;

destructor TRelayArray.Destroy;
begin
  inherited;
end;

function TRelayArray.GetRelay(Index: integer): boolean;
begin
  if FEnable and (Index <= FCount) then
    result := FRelay[Index]
  else
    result := false;
end;

procedure TRelayArray.SetCount(const Value: Integer);
begin
  FCount := Value;
  FEnable := false;
  if Value <> 0 then
    FEnable := true;
  if Assigned(FChangeRelays) then
    FChangeRelays(self);
end;

procedure TRelayArray.setRelay(Index: integer; const Value: boolean);
begin
 if not FEnable  then
    exit;
  if (Index < 0) or (Index > FCount - 1) then
    exit;

  FRelay[Index] := value;
  if Assigned(FChangeRelay) then
    FChangeRelay(self, Index, Value);
  if Assigned(FChangeRelays) then
    FChangeRelays(self);
end;

///////////////////////////////////////////////////////////////////////////////////////////////
////
////  AppnにWaitをかける
////
///////////////////////////////////////////////////////////////////////////////////////////////

procedure Wait(t: integer);   //　t/1000秒のWaitをかける
var
  endtime : TDateTime; //終了時間
begin
  endtime := Now + t / 86400 / 1000;
  while (Now < endtime) do
    Application.ProcessMessages;
end;

function GetSpecialFolder(Folder :integer):string;
var
  Path: array[0..MAX_PATH] of Char;
  pidl: PItemIDList;
begin
  SHGetSpecialFolderLocation(Application.Handle,Folder,pidl);
  SHGetPathFromIDList(pidl, Path);
  Result :=Path;
end;

end.



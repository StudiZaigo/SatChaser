//**********************************************************************//
//                                                                      //
//  ～ 人工衛星 軌道計算コンポーネント Ver 1.00 ～                      //
//                                                                      //
//          動作環境：  Windows 95/98/NT/2000                           //
//        使用ｺﾝﾊﾟｲﾗ：  Delphi 7.0 (Build 4.457)                        //
//                                                                      //
//        ファイル名：  Orbit.pas                                       //
//                                                                      //
//          制作者名：  佐藤國夫 (JA7FKF)                               //
//                                                                      //
//      最終更新日付：  2010/03/20                                      //
//                                                                      //
//                                                                      //
//**********************************************************************//
unit Orbit;

interface

{$ObjExportAll On}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, Math, DateUtils;
type

//**********************************************************************//
//                                                                      //
//  通信の種類の定義                                                    //
//                                                                      //
//**********************************************************************//
TCommParityBits = (cpbNone, cpbOdd, cpbEven);
TCommStopBits   = (csb1, csb1p5, csb2);
TCommFlowCtrls  = (cfcRtsCts, cfcDtrDsr, cfcXonXoff, cfcHalfHigh, cfcHalfLow, cfcNone);

//**********************************************************************//
//                                                                      //
//  例外通知用のクラス                                                  //
//                                                                      //
//**********************************************************************//
EOrbitError = class(Exception);

//**********************************************************************//
//                                                                      //
//  イベント                                                            //
//                                                                      //
//**********************************************************************//
// 受信通知イベント
TOrbitTimerEvent = procedure(Sender: TObject; ReceiveSize: Integer) of Object;
// 通信エラーイベント
TOrbitErrEvent = procedure(Sender: TObject; ErrorState: Integer) of Object;


//**********************************************************************//
//                                                                      //
//  通信コンポーネントの抽象クラス                                      //
//                                                                      //
//**********************************************************************//
TCustomOrbitX = class(TComponent)
private
  FHWnd           : HWnd;                 // ウィンドウのハンドル

  FOnTimer        : TCommReceiveEvent;    // タイマー通知イベント
  FOnBreak        : TCommBreakEvent;      // ブレーク信号イベント
  FOnError        : TCommErrEvent;        // 通信エラーイベント

  Critical1       : TRTLCriticalSection;  // クリティカルセクション１
  Critical2       : TRTLCriticalSection;  // クリティカルセクション２

  function IsSignal(Signal: DWORD): Boolean;

protected
  FHandle         : THandle;              // 通信用ハンドル

  FPortNo         : Integer;              // 通信ポート番号
  FBitRate        : Integer;              // 通信速度（ボーレート）
  FCharSize       : Integer;              // １文字のビット数
  FParityBit      : TCommParityBits;      // パリティチェックの方式
  FStopBit        : TCommStopBits;        // ストップビット数
  FFlowCtrl       : TCommFlowCtrls;       // フロー制御方式

  FTimeOutTrans   : Integer;              // 送信タイムアウト時間（ｍｓ）
  FTimeOutReceive : Integer;              // 受信タイムアウト時間（ｍｓ）

  FXonChar        : Char;                 // XON文字
  FXoffChar       : Char;                 // XOFF文字

  FBufLenTrans    : Integer;              // 送信バッファ長
  FBufLenReceive  : Integer;              // 受信バッファ長

  FDCB            : TDCB;                 // ＤＣＢ構造体

  ReadHandle      : THandle;              // 読み込み終了シグナル
  RecvOverLap     : TOverlapped;          // 受信用オーバーラップ構造体

  TransBufLen     : Integer;              // 送信待機バイト数
  TransCnt        : Integer;              // 送信キューに送った回数


  procedure SetPortNo(const PortNo: Integer);
  procedure SetBitRate(const BitRate: Integer);
  procedure SetCharSize(const CharSize: Integer);
  procedure SetParityBit(const ParityBit: TCommParityBits);
  procedure SetStopBit(const StopBit: TCommStopBits);
  procedure SetFlowCtrl(const FlowCtrl: TCommFlowCtrls);

  procedure SetTimeOutTrans(const TimeOut: Integer);
  procedure SetTimeOutReceive(const TimeOut: Integer);
  procedure SetTimeOut;

  procedure SetXonChar(const XonChar: Char);
  procedure SetXoffChar(const XoffChar: Char);

  procedure SetBufLenTrans(const BufLen: Integer);
  procedure SetBufLenReceive(const BufLen: Integer);
  procedure SetBufferLength;

  procedure WndProc(var Msg: TMessage);

  procedure ReceiveEvent;
  procedure BreakEvent;
  procedure ErrEvent(const ErrorWord: DWORD);

  property PortNo         : Integer read FPortNo write SetPortNo default 1;
  property BitRate        : Integer read FBitRate write SetBitRate default 9600;
  property CharSize       : Integer read FCharSize write SetCharSize default 8;
  property ParityBit      : TCommParityBits read FParityBit write SetParityBit default cpbNone;
  property StopBit        : TCommStopBits read FStopBit write SetStopBit default csb1;
  property FlowCtrl       : TCommFlowCtrls read FFlowCtrl write SetFlowCtrl default cfcRtsCts;

  property TimeOutTrans   : Integer read FTimeOutTrans write SetTimeOutTrans default 3000;
  property TimeOutReceive : Integer read FTimeOutReceive write SetTimeOutReceive default 3000;
  property BufLenTrans    : Integer read FBufLenTrans write SetBufLenTrans default 2048;
  property BufLenReceive  : Integer read FBufLenReceive write SetBufLenReceive default 2048;

  property XonChar        : Char read FXonChar write SetXonChar default #$11;
  property XoffChar       : Char read FXoffChar write SetXoffChar default #$13;

  property Handle         : THandle read FHandle;

  property OnReceive      : TCommReceiveEvent read FOnReceive write FOnReceive;
  property OnBreak        : TCommBreakEvent read FOnBreak write FOnBreak;
  property OnError        : TCommErrEvent read FOnError write FOnError;

public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  procedure PortOpen;
  procedure PortClose;

  function TransChar(const c: Char): Boolean;
  function TransString(const s: string): Boolean;
  function TransBlock(const Data; BufSize: Integer): Boolean; virtual;

  function ReceiveChar: Integer;
  function ReceiveBlock(Buf: PChar; const MaxSize: Integer): Integer; virtual;

  procedure ClearTransBuf;
  procedure ClearReceiveBuf;
  function GetTransLength: Integer;
  function GetReceiveLength: Integer;

  procedure SetRtsSignal(const Signal: Boolean);
  procedure SetDtrSignal(const Signal: Boolean);

  function IsCtsSignal: Boolean;
  function IsDsrSignal: Boolean;
  function IsRingSignal: Boolean;
  function IsRlsdSignal: Boolean;

  procedure SendBreak;

published

end;

//**********************************************************************//
//                                                                      //
//  通信コンポーネントのクラス                                          //
//                                                                      //
//**********************************************************************//
TCommX = class(TCustomCommX)
published
  property PortNo;
  property BitRate;
  property CharSize;
  property ParityBit;
  property StopBit;
  property FlowCtrl;

  property TimeOutTrans;
  property TimeOutReceive;

  property XonChar;
  property XoffChar;

  property BufLenTrans;
  property BufLenReceive;

  property Handle;

  property OnReceive;
  property OnBreak;
  property OnError;
end;

procedure Register;

implementation

//**********************************************************************//
//                                                                      //
//  定数の定義                                                          //
//                                                                      //
//**********************************************************************//
const
  // 通信エラーイベントのマスク
  ERR_MASK = CE_FRAME or CE_OVERRUN or CE_RXPARITY;

  // 使用するイベントのマスク
  EV_MASK = EV_RXCHAR or EV_BREAK or EV_ERR;

  // ユーザーウィンドウメッセージ
  WM_COMM_RECEIVE = WM_USER + $100 + 0;
  WM_COMM_BREAK   = WM_USER + $100 + 1;
  WM_COMM_ERR     = WM_USER + $100 + 2;

  // スレッドメッセージ
  TM_COMM_TRANS = WM_USER + $100 + 0;
  TM_COMM_END   = WM_USER + $100 + 1;

  // フロー制御のセット内容
  DCB_CTS_FLOW      = $00000004;
  DCB_DSR_FLOW      = $00000008;
  DCB_DTR_ENABLE    = $00000010;
  DCB_DTR_HANDSHAKE = $00000020;
  DCB_OUT_XON_XOFF  = $00000100;
  DCB_IN_XON_XOFF   = $00000200;
  DCB_RTS_ENABLE    = $00001000;
  DCB_RTS_HANDSHAKE = $00002000;

//**********************************************************************//
//                                                                      //
//  ～ コンポーネント作成 ～                                            //
//                                                                      //
//**********************************************************************//
constructor TCustomCommX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then begin
    FHWnd := AllocateHWnd(WndProc);
  end;

  FHandle := INVALID_HANDLE_VALUE;        // ハンドルを初期化

  TransThread := Nil;                     // 送信用スレッド
  ReceiveThread := Nil;                   // 受信用スレッド

  FPortNo := 1;                           // 通信ポート番号
  FBitRate := 9600;                       // 通信速度（ｂｐｓ）
  FCharSize := 8;                         // １文字のビット数
  FParityBit := cpbNone;                  // パリティチェックの方式
  FStopBit := csb1;                       // ストップビット数
  FFlowCtrl := cfcRtsCts;                 // フロー制御方式

  FTimeOutTrans := 3000;                  // 送信タイムアウト時間（ｍｓ）
  FTimeOutReceive := 3000;                // 受信タイムアウト時間（ｍｓ）

  FXonChar := #$11;                       // XON文字
  FXoffChar := #$13;                      // XOFF文字

  FBufLenTrans := 2048;                   // 送信バッファ長
  FBufLenReceive := 2048;                 // 受信バッファ長

  FDCB.DCBLength := Sizeof(TDCB);         // ＤＣＢ構造体のサイズ

  with RecvOverLap do begin               // 受信用オーバーラップ構造体
    Internal := 0;
    InternalHigh := 0;
    Offset := 0;
    OffsetHigh := 0;
    hEvent := 0;
  end;

  TransBufLen := 0;                       // 送信待機バイト数
  TransCnt := 0;                          // 送信キューに送った回数

  FOnReceive := Nil;                      // 受信通知イベント
  FOnError := Nil;                        // 通信エラーイベント
  FOnBreak := Nil;                        // ブレーク信号イベント

  InitializeCriticalSection(Critical1);   // クリティカルセクションの初期化
  InitializeCriticalSection(Critical2);   // クリティカルセクションの初期化
end;

//**********************************************************************//
//                                                                      //
//  ～ コンポーネント破棄 ～                                            //
//                                                                      //
//**********************************************************************//
destructor TCustomCommX.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    PortClose;
  end;
  if not (csDesigning in ComponentState) then begin
    DeAllocateHWnd(FHWnd);
  end;

  DeleteCriticalSection(Critical1);       // クリティカルセクションの終了
  DeleteCriticalSection(Critical2);       // クリティカルセクションの終了

  inherited Destroy;
end;

//**********************************************************************//
//                                                                      //
//  ～ 通信ポートのオープン ～                                          //
//                                                                      //
// - input -  なし                                                      //
// - output - なし                                                      //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.PortOpen;
var
  PortName  : array [0..15] of Char;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    raise ECommError.Create('ポートは、すでにオープンされています');
  end;

  StrPCopy(PortName, '\\.\COM' + IntToStr(FPortNo));        // ポート名の作成

  FHandle := CreateFile(PortName,                           // ポート名
                        GENERIC_READ or                     // 読み込みアクセス
                        GENERIC_WRITE,                      // 書き込みアクセス
                        0,                                  // 共有の対象としない
                        Nil,                                // セキュリティ属性なし
                        OPEN_EXISTING,                      // オープン（通信では必ずこの設定）
                        FILE_FLAG_OVERLAPPED,               // オーバーラップ入出力を行う
                        0);                                 // テンプレートファイルアクセスなし

  if FHandle = INVALID_HANDLE_VALUE then begin
    raise ECommError.Create('ポートをオープンできませんでした');
  end;

  try
    TransBufLen := 0;                                       // 送信待機バイト数
    TransCnt := 0;                                          // 送信キューに送った回数

    SetBufferLength;                                        // 送受信バッファ長の設定

    PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);     // 送信バッファを破棄
    PurgeComm(FHandle, PURGE_RXABORT or PURGE_RXCLEAR);     // 受信バッファを破棄

    GetCommState(FHandle, FDCB);                            // 現在のTDCBの取得

    SetBitRate(FBitRate);                                   // 通信速度（ボーレート）の設定
    SetCharSize(FCharSize);                                 // １キャラクタのビット数の設定
    SetParityBit(FParityBit);                               // パリティチェックの方式の設定
    SetStopBit(FStopBit);                                   // ストップビット数の設定
    SetFlowCtrl(FFlowCtrl);                                 // フロー制御方式の設定

    SetTimeOut;                                             // 送受信タイムアウト時間の設定

    SetXonChar(FXonChar);                                   // XON文字
    SetXoffChar(FXoffChar);                                 // XOFF文字

    SetCommMask(FHandle, EV_MASK);                          // 使用するイベントの設定

    ReadHandle := CreateEvent(Nil, True, False, Nil);       // 読み込み終了シグナル
    RecvOverLap.hEvent := ReadHandle;                       // 受信用オーバーラップ構造体

    ThreadCreate;                                           // スレッドの生成
  except
    PortClose;                                              // 失敗時、ポートクローズ
    raise;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ スレッドの生成 ～                                                //
//                                                                      //
// - input -  なし                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.ThreadCreate;
begin
  TransThread := Nil;       // ポインタの初期化
  ReceiveThread := Nil;     // ポインタの初期化

  // 送信スレッドの生成
  try
    TransThread := TCommTransThread.Create(Self, FHandle, FHWnd);
    TransThread.HalfWaitTim := (1000 div (FBitRate div 10)) + 1;
  except
    raise ECommError.Create('送信用スレッドを生成できませんでした');
  end;
  TransThread.Resume;

  // 受信用スレッドの生成
  try
    ReceiveThread := TCommReceiveThread.Create(FHandle, FHWnd);
  except
    raise ECommError.Create('受信用スレッドを生成できませんでした');
  end;
  ReceiveThread.Resume;

  Sleep(100);
end;

//**********************************************************************//
//                                                                      //
//  ～ 通信ポートのクローズ ～                                          //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.PortClose;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    SetDtrSignal(False);                                    // ＤＴＲをオフにする
    SetRtsSignal(False);                                    // ＲＴＳをオフにする

    ClearTransBuf;                                          // 送信バッファを破棄
    ClearReceiveBuf;                                        // 受信バッファを破棄

    Sleep(100);                                             // モデムを確実にオフするため

    if TransThread <> Nil then begin
      TransThread.Terminate;                                // 送信スレッド終了
      PostThreadMessage(TransThread.ThreadID, TM_COMM_END,
                        0, 0);
    end;

    if ReceiveThread <> Nil then begin
      ReceiveThread.Terminate;                              // 受信スレッド終了
      SetEvent(ReceiveThread.ExitHandle);                   // イベント待ち終了
    end;

    CloseHandle(ReadHandle);                                // 読み込み終了シグナルのクローズ
    ReadHandle := 0;

    CloseHandle(FHandle);                                   // 通信ポートをクローズ
    FHandle := INVALID_HANDLE_VALUE;

    Sleep(200);                                             // スレッドの終了待ち
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ ポート番号の設定 ～                                              //
//                                                                      //
// - input -  PortNo  Integer   ポート番号                              //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetPortNo(const PortNo: Integer);
begin
  FPortNo := PortNo;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    PortClose;
    PortOpen;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 通信速度の設定 ～                                                //
//                                                                      //
// - input -  BitRate   Integer   通信速度（ｂｐｓ）                    //
//                                                                      //
//                                300/600/1200/2400/4800/9600           //
//                                14400/19200/38400/56000/57600         //
//                                115200/128000/256000 など             //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
// ※ 実際に設定できるかどうかは、ハードウェアやドライバによる。        //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBitRate(const BitRate: Integer);
begin
  FBitRate := BitRate;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.BaudRate := BitRate;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('通信速度の設定でエラーが発生しました');
    end;
    if TransThread <> Nil then begin
      TransThread.HalfWaitTim := (1000 div (FBitRate div 10)) + 1;
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送受信する１キャラクタのビット数の設定 ～                        //
//                                                                      //
// - input -  CharSize    Integer   ビット数    5..8                    //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
// ※ 実際に設定できるかどうかは、ハードウェアやドライバによる。        //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetCharSize(const CharSize: Integer);
begin
  FCharSize := CharSize;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.ByteSize := CharSize;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('通信ビット数の設定でエラーが発生しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ パリティチェック方式の設定 ～                                    //
//                                                                      //
// - input -  ParityBit   TCommParityBits   パリティタイプ              //
//                                                                      //
//                                          cpbNone:  なし              //
//                                          cpbOdd:   奇数              //
//                                          cpbEven:  偶数              //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetParityBit(const ParityBit: TCommParityBits);
begin
  FParityBit := ParityBit;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    case ParityBit of
      cpbNone : FDCB.Parity := NOPARITY;
      cpbOdd  : FDCB.Parity := ODDPARITY;
      cpbEven : FDCB.Parity := EVENPARITY;
    else
      exit;
    end;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('通信パリティチェックの設定でエラーが発生しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ ストップビット数の設定 ～                                        //
//                                                                      //
// - input -  StopBit   TCommStopBits   ストップビット                  //
//                                                                      //
//                                      csb1:   １ビット                //
//                                      csb1p5: １．５ビット            //
//                                      csb2:   ２ビット                //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
// ※ 実際に設定できるかどうかは、ハードウェアやドライバによる。        //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetStopBit(const StopBit: TCommStopBits);
begin
  FStopBit := StopBit;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    case StopBit of
      csb1   : FDCB.StopBits := ONESTOPBIT;
      csb1p5 : FDCB.StopBits := ONE5STOPBITS;
      csb2   : FDCB.StopBits := TWOSTOPBITS;
    else
      exit;
    end;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('通信ストップビット数の設定でエラーが発生しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ フロー制御方式の設定 ～                                          //
//                                                                      //
// - input -  FlowCtrl  TCommFlowCtrls   制御方式                       //
//                                                                      //
//                      cfcRtsCts:    RTS/CTS制御を行う                 //
//                      cfcDtrDsr:    DTR/DSR制御を行う                 //
//                      cfcXonXoff:   XON/XOFF制御を行う                //
//                      cfcHalfHigh:  半二重制御を送信時ONで行う        //
//                      cfcHalfLow:   半二重制御を送信時OFFで行う       //
//                      cfcNone:      何も制御を行わない                //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetFlowCtrl(const FlowCtrl: TCommFlowCtrls);
var
  Flag  : DWORD;
begin
  FFlowCtrl := FlowCtrl;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    Flag := 0;
    if FlowCtrl = cfcRtsCts then begin                // RTS/CTSﾌﾛｰ制御の場合
      Flag := DCB_CTS_FLOW or DCB_RTS_HANDSHAKE;
      Flag := Flag or DCB_DTR_ENABLE;
    end
    else if FlowCtrl = cfcDtrDsr then begin           // DTR/DSRﾌﾛｰ制御の場合
      Flag := DCB_DSR_FLOW or DCB_DTR_HANDSHAKE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else if FlowCtrl = cfcXonXoff then begin          // XON/XOFFﾌﾛｰ制御の場合
      Flag := DCB_OUT_XON_XOFF or DCB_IN_XON_XOFF;
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else if FlowCtrl = cfcHalfHigh then begin         // 半二重 送信時ON の場合
      Flag := Flag or DCB_DTR_ENABLE;
    end
    else if FlowCtrl = cfcHalfLow then begin          // 半二重 送信時OFF の場合
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else begin                                        // なし
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end;

    FDCB.XonLim := (FBufLenReceive div 4);            // ３／４以上 で ＸＯＮを送信
    FDCB.XoffLim := (FBufLenReceive div 4);           // １／４以下 で ＸＯＦＦを送信

    FDCB.Flags := Flag or $00000003;                  // バイナリ転送とパリティチェック有効

    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('通信フロー制御の設定で失敗しました');
    end;
  end
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送信タイムアウト時間の設定 ～                                    //
//                                                                      //
// - input -  TimeOut   Integer   送信タイムアウト時間（ｍｓ）          //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOutTrans(const TimeOut: Integer);
begin
  FTimeOutTrans := TimeOut;
  SetTimeOut;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 受信タイムアウト時間の設定 ～                                    //
//                                                                      //
// - input -  TimeOut   Integer   受信タイムアウト時間（ｍｓ）          //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOutReceive(const TimeOut: Integer);
begin
  FTimeOutReceive := TimeOut;
  SetTimeOut;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送受信タイムアウト時間の設定 ～                                  //
//                                                                      //
// - input -  なし                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOut;
var
  CommTimeOut : TCommTimeouts;    // タイムアウトパラメータ
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    GetCommTimeouts(FHandle, CommTimeOut);
    with CommTimeOut do begin
      ReadIntervalTimeout         := MAXDWORD;
      ReadTotalTimeoutMultiplier  := 0;
      ReadTotalTimeoutConstant    := DWORD(FTimeOutReceive);
      WriteTotalTimeoutMultiplier := 0;
      WriteTotalTimeoutConstant   := DWORD(FTimeOutTrans);
    end;
    if not SetCommTimeouts(FHandle, CommTimeOut) then begin
      raise ECommError.Create('通信タイムアウト時間の設定で失敗しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ Ｘｏｎ文字の設定 ～                                              //
//                                                                      //
// - input -  XonChar   Char      Ｘｏｎ文字                            //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetXonChar(const XonChar: Char);
begin
  FXonChar := XonChar;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.XonChar := XonChar;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('Ｘｏｎ文字の設定でエラーが発生しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ Ｘｏｆｆ文字の設定 ～                                            //
//                                                                      //
// - input -  XoffChar  Char      Ｘｏｆｆ文字                          //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetXoffChar(const XoffChar: Char);
begin
  FXoffChar := XoffChar;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.XoffChar := XoffChar;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('Ｘｏｆｆ文字の設定でエラーが発生しました');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送信バッファ長の設定 ～                                          //
//                                                                      //
// - input -  BufLen  Integer   送信バッファ長（バイト）                //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufLenTrans(const BufLen: Integer);
begin
  FBufLenTrans := BufLen;
  SetBufferLength;            // 送受信バッファ長の設定
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 受信バッファ長の設定 ～                                          //
//                                                                      //
// - input -  BufLen  Integer   受信バッファ長（バイト）                //
//                                                                      //
// - output - なし                                                      //
//                                                                      //
//  ※ 受信バッファ長が変わるとフロー制御のしきい値も変わるので、フ     //
//   ロー制御方式の設定を呼び出す。                                     //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufLenReceive(const BufLen: Integer);
begin
  FBufLenReceive := BufLen;
  SetBufferLength;            // 送受信バッファ長の設定
  SetFlowCtrl(FFlowCtrl);     // フロー制御方式の設定
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送受信バッファ長の設定 ～                                        //
//                                                                      //
// - input -  なし                                                      //
// - output - なし                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufferLength;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if not SetupComm(FHandle, FBufLenReceive, FBufLenTrans) then begin
      raise ECommError.Create('通信用ポートのバッファを準備できませんでした');
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ キャラクタの送信 ～                                              //
//                                                                      //
// - input -    c   Char  キャラクタ                                    //
//                                                                      //
// - output -   Boolean   正常終了時  True                              //
//                        異常終了時  False                             //
//                                                                      //
//**********************************************************************//
function TCustomCommX.TransChar(const c: Char): Boolean;
begin
  if TransBlock(c, SizeOf(c)) then begin
    Result := True;
  end
  else begin
    Result := False;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ 文字列送信 ～                                                    //
//                                                                      //
// - input -    s   string    文字列                                    //
//                                                                      //
// - output -   Boolean       正常終了時    True                        //
//                            異常終了時    False                       //
//                                                                      //
//**********************************************************************//
function TCustomCommX.TransString(const s: string): Boolean;
var
  len : Integer;
begin
  len := Length(s);
  if len <> 0 then begin
    Result := TransBlock(s[1], len);
  end
  else begin
    Result := True;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ データのブロック送信 ～                                          //
//                                                                      //
// - input -    const     Data      データバッファ                      //
//              BufSize   Integer   送信バイト数                        //
//                                                                      //
// - output -   Boolean   正常終了時  True                              //
//                        異常終了時  False                             //
//                                                                      //
//  ※ 送信バイト数のメモリを確保し、データバッファのコピーをして       //
//   から、メッセージキューにコピーのアドレスとサイズを格納するだ       //
//   けで、すぐに返る。                                                 //
//                                                                      //
//  ※ 送信が完了する前に、この関数を連続で呼び出しても問題ないが、     //
//   あまり多くのメッセージを送るとリソースが少なくなりＯＳが不安       //
//   定になるので、格納できる数は３２までにしている。                   //
//     それ以上のときは内部でループする。                               //
//                                                                      //
//**********************************************************************//
function TCustomCommX.TransBlock(const Data; BufSize: Integer): Boolean;
type
  BytePtr = ^byte;
var
  DatPtr    : BytePtr;    // 送信データへのポインタ
  BufPtr    : BytePtr;    // 送信データバッファへのポインタ
  SendSize  : Integer;    // 送信データサイズ
  cnt       : Integer;
begin
  if BufSize = 0 then begin
    Result := True;
    exit;
  end;

  DatPtr := @Data;
  repeat
    // ProcessMessages で終了していることも考えられるので、ここでチェックする
    if (FHandle = INVALID_HANDLE_VALUE) or (TransThread = Nil) then begin
      Result := False;
      exit;
    end;

    EnterCriticalSection(Critical2);
    cnt := TransCnt;
    LeaveCriticalSection(Critical2);
    if cnt < 32 then begin
      if BufSize > FBufLenTrans then begin
        SendSize := FBufLenTrans;
      end
      else begin
        SendSize := BufSize;
      end;
      BufPtr := Pointer(GlobalAlloc(LMEM_FIXED, SendSize));
      if BufPtr <> Nil then begin
        Move(DatPtr^, BufPtr^, SendSize);
        EnterCriticalSection(Critical2);
        Inc(TransBufLen, SendSize);
        Inc(TransCnt);
        LeaveCriticalSection(Critical2);
        PostThreadMessage(TransThread.ThreadID, TM_COMM_TRANS,
                          WPARAM(SendSize), LPARAM(BufPtr));
      end;
      Dec(BufSize, SendSize);
      Inc(DatPtr, SendSize);
    end
    else begin
      Application.ProcessMessages;
      Sleep(1);
    end;
  until (BufSize <= 0);

  Result := True;
end;

//**********************************************************************//
//                                                                      //
//  ～ データの受信 ～                                                  //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Integer  受信データ                                     //
//                                                                      //
//  ※ エラーや受信文字がない場合は -1 を返す。そのために Integer 型    //
//   で値を返すようにしている。                                         //
//                                                                      //
//**********************************************************************//
function TCustomCommX.ReceiveChar: Integer;
var
  c : Char;
begin
  if ReceiveBlock(@c, 1) = 1 then begin
    Result := Integer(c);
  end
  else begin
    Result := -1;         // エラー、または受信文字なし
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ データのブロック受信 ～                                          //
//                                                                      //
// - input -    Buf       PChar     格納するバッファ                    //
//              MaxSize   Integer   受信する最大サイズ                  //
//                                                                      //
// - output -   Integer   受信したサイズ（エラーのとき －１）           //
//                                                                      //
//**********************************************************************//
function TCustomCommX.ReceiveBlock(Buf: PChar; const MaxSize: Integer): Integer;
var
  ReadSize  : DWORD;        // 読みとったバイト数
  ErrorMask : DWORD;        // エラーコード
  ComStat   : TCOMSTAT;     // 通信状態
begin
  if (FHandle <> INVALID_HANDLE_VALUE) and (ReceiveThread <> Nil) and (Buf <> Nil) then begin
    if not ReadFile(FHandle, Buf^, MaxSize, ReadSize, @RecvOverLap) then begin
      if GetLastError = ERROR_IO_PENDING then begin
        while not GetOverlappedResult(FHandle, RecvOverLap, ReadSize, False) do begin
          if GetLastError = ERROR_IO_INCOMPLETE then begin
            Application.ProcessMessages;
            Sleep(1);
            if (FHandle = INVALID_HANDLE_VALUE) or (ReceiveThread = Nil) then begin
              Result := -1;
              exit;
            end;
          end
          else begin
            ClearCommError(FHandle, ErrorMask, @ComStat);
            ErrorMask := ErrorMask and ERR_MASK;
            if ErrorMask <> 0 then begin
              SendMessage(FHWnd, WM_COMM_ERR, WPARAM(0), LPARAM(ErrorMask));
            end;
          end;
        end;
      end
      else begin
        ClearCommError(FHandle, ErrorMask, @ComStat);
        ErrorMask := ErrorMask and ERR_MASK;
        if ErrorMask <> 0 then begin
          SendMessage(FHWnd, WM_COMM_ERR, WPARAM(0), LPARAM(ErrorMask));
        end;
        Result := -1;
        exit;
      end;
    end;
    Result := ReadSize;
  end
  else begin
    Result := -1;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ 送信バッファのクリア ～                                          //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//  ※ スレッドのメッセージキューに溜まったデータをクリアする方法が     //
//   わからなかったのでフラグを立ててスレッド自身にクリアさせている。   //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.ClearTransBuf;
var
  len : Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if TransThread <> Nil then begin
      TransThread.MsgClearFlg := True;                      // クリアフラグを立てる
      repeat
        EnterCriticalSection(Critical1);
        PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);
        LeaveCriticalSection(Critical1);
        Sleep(1);
        EnterCriticalSection(Critical2);
        len := TransBufLen;
        LeaveCriticalSection(Critical2);
      until len = 0;
      TransThread.MsgClearFlg := False;                     // クリアフラグを戻す
    end
    else begin
      PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ 受信バッファのクリア ～                                          //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.ClearReceiveBuf;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    PurgeComm(FHandle, PURGE_RXABORT or PURGE_RXCLEAR);
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ 送信バッファのデータ数を取得 ～                                  //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Integer   データ数（バイト）                            //
//                                                                      //
//**********************************************************************//
function TCustomCommX.GetTransLength: Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    EnterCriticalSection(Critical2);
    Result := TransBufLen;
    LeaveCriticalSection(Critical2);
  end
  else begin
    Result := 0;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ 受信バッファのデータ数を取得 ～                                  //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Integer   データ数（バイト）                            //
//                                                                      //
//**********************************************************************//
function TCustomCommX.GetReceiveLength: Integer;
var
  ErrorMask : DWORD;        // エラーコード
  ComStat   : TCOMSTAT;     // 通信状態
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    ClearCommError(FHandle, ErrorMask, @ComStat);
    Result := ComStat.cbInQue;
  end
  else begin
    Result := 0;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ ＲＴＳ信号を設定する ～                                          //
//                                                                      //
// - input -    Signal  Boolean   設定値    True:   ＯＮ                //
//                                          False:  ＯＦＦ              //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.SetRtsSignal(const Signal: Boolean);
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if Signal = True then begin
      EscapeCommFunction(FHandle, SETRTS);
    end
    else begin
      EscapeCommFunction(FHandle, CLRRTS);
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ ＤＴＲ信号を設定する ～                                          //
//                                                                      //
// - input -    Signal  Boolean   設定値    True:   ＯＮ                //
//                                          False:  ＯＦＦ              //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.SetDtrSignal(const Signal: Boolean);
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if Signal = True then begin
      EscapeCommFunction(FHandle, SETDTR);
    end
    else begin
      EscapeCommFunction(FHandle, CLRDTR);
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ ＣＴＳ信号を取得する ～                                          //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Boolean  ＣＴＳ信号                                     //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsCtsSignal: Boolean;
begin
  Result := IsSignal(MS_CTS_ON);
end;

//**********************************************************************//
//                                                                      //
//  ～ ＤＳＲ信号を取得する ～                                          //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Boolean  ＤＳＲ信号                                     //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsDsrSignal: Boolean;
begin
  Result := IsSignal(MS_DSR_ON);
end;

//**********************************************************************//
//                                                                      //
//  ～ リングインジゲータ（ＲＩ、ＣＩ）信号の検出 ～                    //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Boolean  リングインジゲータ（ＲＩ、ＣＩ）信号           //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsRingSignal: Boolean;
begin
  Result := IsSignal(MS_RING_ON);
end;

//**********************************************************************//
//                                                                      //
//  ～ ＲＬＳＤ（ＤＣＤ、ＣＤ）信号の検出 ～                            //
//                                                                      //
// - input -    なし                                                    //
//                                                                      //
// - output -   Boolean  ＲＬＳＤ信号                                   //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsRlsdSignal: Boolean;
begin
  Result := IsSignal(MS_RLSD_ON);
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 信号の検出 ～                                                    //
//                                                                      //
// - input -    Signal  DWORD   信号の種類                              //
//                                                                      //
// - output -   Boolean   信号の論理    True:   ＯＮ                    //
//                                      False:  ＯＦＦ                  //
//                                                                      //
//----------------------------------------------------------------------//
function TCustomCommX.IsSignal(Signal: DWORD): Boolean;
var
  ModemStat : DWORD;    // モデムのコントロールレジスタ値
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    GetCommModemStatus(FHandle, ModemStat);
    Result := ((ModemStat and Signal) <> 0);
  end
  else begin
    Result := False;
  end;
end;

//**********************************************************************//
//                                                                      //
//  ～ ブレーク信号の送信 ～                                            //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.SendBreak;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    SetCommBreak(FHandle);
    Sleep(300);
    ClearCommBreak(FHandle);
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ メッセージによる各動作を行う ～                                  //
//                                                                      //
// - input -    Msg   TMessage    メッセージ                            //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.WndProc(var Msg: TMessage);
begin
  with Msg do begin
    case Msg of
      WM_COMM_RECEIVE : ReceiveEvent;         // 受信通知イベント
      WM_COMM_BREAK   : BreakEvent;           // ブレーク信号イベント
      WM_COMM_ERR     : ErrEvent(lParam);     // エラーイベント
    else
      Result := DefWindowProc(FHWnd, Msg, wParam, lParam);
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 受信通知イベント ～                                              //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.ReceiveEvent;
var
  len : Integer;        // データ数
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if Assigned(FOnReceive) then begin
      len := GetReceiveLength;
      if len <> 0 then begin
        FOnReceive(Self, len);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ ブレーク信号イベント ～                                          //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.BreakEvent;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if Assigned(FOnBreak) then begin
      FOnBreak(Self);
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ エラーイベント ～                                                //
//                                                                      //
// - input -    ErrorWord   DWORD   エラーの種類                        //
//                                                                      //
//                          CE_FRAME:     フレーミングエラー            //
//                          CE_OVERRUN:   バッファオーバーラン          //
//                          CE_RXPARITY:  パリティエラー                //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//  ※ エラーの種類は組み合わせで表現される。                           //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.ErrEvent(const ErrorWord: DWORD);
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if Assigned(FOnError) then begin
      FOnError(Self, ErrorWord);
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送信を行うスレッドのコンストラクタ ～                            //
//                                                                      //
// - input -    Owner   TComponet   親コンポーネント                    //
//              Handle  THandle     通信ハンドル                        //
//              Window  THandle     ウィンドウハンドル                  //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//  ※ Window には、メッセージを処理するためのハンドルを指定する。      //
//                                                                      //
//----------------------------------------------------------------------//
constructor TCommTransThread.Create(Owner: TComponent; Handle: THandle; Window: THandle);
begin
  inherited Create(True);     // サスペンド状態で作成
  FOwner := Owner;            // オーナーの保存
  CommHandle := Handle;
  CommWindow := Window;
  MsgClearFlg := False;
  HalfWaitTim := 0;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 送信を行うスレッド ～                                            //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.Execute;
type
  BytePtr = ^Byte;
var
  WriteHandle : THandle;            // 書き込み完了シグナル
  Msg         : TMsg;               // メッセージ
  DataSize    : DWORD;              // 送信バイト数
  DataPtr     : BytePtr;            // 送信バッファへのポインタ
  ComStat     : TCOMSTAT;           // 通信状態
  TransSize   : DWORD;              // 送信できたバイト数
  ErrorMask   : DWORD;              // エラーコード
  Overlap     : TOVERLAPPED;        // オーバーラップ構造体
  HalfCtrlFlg : Boolean;            // 半二重制御フラグ
  Rslt        : Boolean;            // 返り値
begin
  WriteHandle := CreateEvent(Nil, True, False, Nil);    // 書き込み完了シグナル

  with OverLap do begin
    Internal := 0;
    InternalHigh := 0;
    Offset := 0;
    OffsetHigh := 0;
    hEvent := WriteHandle;
  end;

  repeat
    HalfCtrlFlg := False;
    WaitMessage;
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
      if (Msg.hWnd = 0) and (Msg.Message = TM_COMM_TRANS) then begin
        if (MsgClearFlg = False) and (Terminated = False) then begin
          DataSize := Msg.wParam;
          DataPtr := BytePtr(Msg.lParam);
          if HalfCtrlFlg = False then begin
            HalfCtrlFlg := True;
            HalfCtrlStart;
          end;
          while (MsgClearFlg = False) and (Terminated = False) do begin
            ResetEvent(WriteHandle);
            EnterCriticalSection(TCustomCommX(FOwner).Critical1);
            Rslt := WriteFile(CommHandle, DataPtr^, DataSize, TransSize, @OverLap);
            LeaveCriticalSection(TCustomCommX(FOwner).Critical1);
            if not Rslt then begin;
              if GetLastError = ERROR_IO_PENDING then begin
                WaitForSingleObject(WriteHandle, INFINITE);
                if not GetOverlappedResult(CommHandle, OverLap, TransSize, False) then begin
                  ClearCommError(CommHandle, ErrorMask, @ComStat);
                  ErrorMask := ErrorMask and ERR_MASK;
                  if ErrorMask <> 0 then begin
                    SendMessage(CommWindow, WM_COMM_ERR, WPARAM(0), LPARAM(ErrorMask));
                  end;
                  if TransSize < DataSize then begin
                    Inc(DataPtr, TransSize);
                    Dec(DataSize, TransSize);
                    continue;
                  end;
                end;
              end
              else begin
                ClearCommError(CommHandle, ErrorMask, @ComStat);
                ErrorMask := ErrorMask and ERR_MASK;
                if ErrorMask <> 0 then begin
                  SendMessage(CommWindow, WM_COMM_ERR, WPARAM(0), LPARAM(ErrorMask));
                end;
              end;
            end;
            Break;
          end;
        end;
        GlobalFree(HGLOBAL(Msg.lParam));
        EnterCriticalSection(TCustomCommX(FOwner).Critical2);
        if (TCustomCommX(FOwner).TransBufLen - Msg.wParam) > 0 then begin
          TCustomCommX(FOwner).TransBufLen := TCustomCommX(FOwner).TransBufLen - Msg.wParam;
        end
        else begin
          TCustomCommX(FOwner).TransBufLen := 0;
        end;
        if TCustomCommX(FOwner).TransCnt <> 0 then begin
          TCustomCommX(FOwner).TransCnt := TCustomCommX(FOwner).TransCnt - 1;
        end;
        LeaveCriticalSection(TCustomCommX(FOwner).Critical2);
      end;
    end;
    if HalfCtrlFlg = True then begin
      HalfCtrlEnd;
    end;
  until Terminated;

  Sleep(100);

  CloseHandle(WriteHandle);

  FreeOnTerminate := True;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 半二重制御の送信スタート ～                                      //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.HalfCtrlStart;
begin
  if TCustomCommX(FOwner).FFlowCtrl = cfcHalfHigh then begin
    Sleep(50);            // 衝突回避（相手のＲＴＳ操作終了を待つ）
    EscapeCommFunction(CommHandle, SETRTS);
    Sleep(HalfWaitTim);   // 送信開始時に最初の一バイト目が壊れないように
  end
  else if TCustomCommX(FOwner).FFlowCtrl = cfcHalfLow then begin
    Sleep(50);            // 衝突回避（相手のＲＴＳ操作終了を待つ）
    EscapeCommFunction(CommHandle, CLRRTS);
    Sleep(HalfWaitTim);   // 送信開始時に最初の一バイト目が壊れないように
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 半二重制御の送信終了 ～                                          //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.HalfCtrlEnd;
begin
  if TCustomCommX(FOwner).FFlowCtrl = cfcHalfHigh then begin
    Sleep(HalfWaitTim);   // ＩＣのバッファに残っている場合があるので
    EscapeCommFunction(CommHandle, CLRRTS);
  end
  else if TCustomCommX(FOwner).FFlowCtrl = cfcHalfLow then begin
    Sleep(HalfWaitTim);   // ＩＣのバッファに残っている場合があるので
    EscapeCommFunction(CommHandle, SETRTS);
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 受信を行うスレッドのコンストラクタ ～                            //
//                                                                      //
// - input -    Handle  THandle     通信ハンドル                        //
//              Window  THandle     ウィンドウハンドル                  //
//                                                                      //
// - output -   なし                                                    //
//                                                                      //
//  ※ Window には、メッセージを処理するためのハンドルを指定する。      //
//                                                                      //
//----------------------------------------------------------------------//
constructor TCommReceiveThread.Create(Handle: THandle; Window: THandle);
begin
  inherited Create(True);       // サスペンド状態で作成
  CommHandle := Handle;
  CommWindow := Window;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ 受信を行うスレッド ～                                            //
//                                                                      //
// - input -    なし                                                    //
// - output -   なし                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommReceiveThread.Execute;
var
  EvOverLap : TOVERLAPPED;      // オーバーラップ構造体
  EventMask : DWORD;            // 発生したイベント
  ErrorMask : DWORD;            // エラーコード
  ComStat   : TCOMSTAT;         // 通信状態
begin
  ExitHandle := CreateEvent(Nil, False, False, Nil);    // イベント待ち終了シグナル

  with EvOverLap do begin
    Internal := 0;
    InternalHigh := 0;
    Offset := 0;
    OffsetHigh := 0;
    hEvent := ExitHandle;
  end;

  repeat
    EventMask := 0;
    if not WaitCommEvent(CommHandle, EventMask, @EvOverLap) then begin
      WaitForSingleObject(ExitHandle, INFINITE);
    end;

    if Terminated then break;

    ClearCommError(CommHandle, ErrorMask, @ComStat);
    ErrorMask := ErrorMask and ERR_MASK;
    if ErrorMask <> 0 then begin
      SendMessage(CommWindow, WM_COMM_ERR, WPARAM(0), LPARAM(ErrorMask));
    end;

    if (EventMask and EV_BREAK) <> 0 then begin
      SendMessage(CommWindow, WM_COMM_BREAK, WPARAM(0), LPARAM(EventMask));
    end;
    if (EventMask and EV_RXCHAR) <> 0 then begin
      SendMessage(CommWindow, WM_COMM_RECEIVE, WPARAM(0), LPARAM(0));
    end;
  until Terminated;

  SetCommMask(CommHandle, 0);

  Sleep(100);

  CloseHandle(ExitHandle);
  ExitHandle := 0;

  FreeOnTerminate := True;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  ～ コンポーネントの登録 ～                                          //
//                                                                      //
//----------------------------------------------------------------------//
procedure Register;
begin
  RegisterComponents('LIB-X', [TCommX]);
end;

end.

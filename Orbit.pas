//**********************************************************************//
//                                                                      //
//  �` �l�H�q�� �O���v�Z�R���|�[�l���g Ver 1.00 �`                      //
//                                                                      //
//          ������F  Windows 95/98/NT/2000                           //
//        �g�p���ׁ߲F  Delphi 7.0 (Build 4.457)                        //
//                                                                      //
//        �t�@�C�����F  Orbit.pas                                       //
//                                                                      //
//          ����Җ��F  �������v (JA7FKF)                               //
//                                                                      //
//      �ŏI�X�V���t�F  2010/03/20                                      //
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
//  �ʐM�̎�ނ̒�`                                                    //
//                                                                      //
//**********************************************************************//
TCommParityBits = (cpbNone, cpbOdd, cpbEven);
TCommStopBits   = (csb1, csb1p5, csb2);
TCommFlowCtrls  = (cfcRtsCts, cfcDtrDsr, cfcXonXoff, cfcHalfHigh, cfcHalfLow, cfcNone);

//**********************************************************************//
//                                                                      //
//  ��O�ʒm�p�̃N���X                                                  //
//                                                                      //
//**********************************************************************//
EOrbitError = class(Exception);

//**********************************************************************//
//                                                                      //
//  �C�x���g                                                            //
//                                                                      //
//**********************************************************************//
// ��M�ʒm�C�x���g
TOrbitTimerEvent = procedure(Sender: TObject; ReceiveSize: Integer) of Object;
// �ʐM�G���[�C�x���g
TOrbitErrEvent = procedure(Sender: TObject; ErrorState: Integer) of Object;


//**********************************************************************//
//                                                                      //
//  �ʐM�R���|�[�l���g�̒��ۃN���X                                      //
//                                                                      //
//**********************************************************************//
TCustomOrbitX = class(TComponent)
private
  FHWnd           : HWnd;                 // �E�B���h�E�̃n���h��

  FOnTimer        : TCommReceiveEvent;    // �^�C�}�[�ʒm�C�x���g
  FOnBreak        : TCommBreakEvent;      // �u���[�N�M���C�x���g
  FOnError        : TCommErrEvent;        // �ʐM�G���[�C�x���g

  Critical1       : TRTLCriticalSection;  // �N���e�B�J���Z�N�V�����P
  Critical2       : TRTLCriticalSection;  // �N���e�B�J���Z�N�V�����Q

  function IsSignal(Signal: DWORD): Boolean;

protected
  FHandle         : THandle;              // �ʐM�p�n���h��

  FPortNo         : Integer;              // �ʐM�|�[�g�ԍ�
  FBitRate        : Integer;              // �ʐM���x�i�{�[���[�g�j
  FCharSize       : Integer;              // �P�����̃r�b�g��
  FParityBit      : TCommParityBits;      // �p���e�B�`�F�b�N�̕���
  FStopBit        : TCommStopBits;        // �X�g�b�v�r�b�g��
  FFlowCtrl       : TCommFlowCtrls;       // �t���[�������

  FTimeOutTrans   : Integer;              // ���M�^�C���A�E�g���ԁi�����j
  FTimeOutReceive : Integer;              // ��M�^�C���A�E�g���ԁi�����j

  FXonChar        : Char;                 // XON����
  FXoffChar       : Char;                 // XOFF����

  FBufLenTrans    : Integer;              // ���M�o�b�t�@��
  FBufLenReceive  : Integer;              // ��M�o�b�t�@��

  FDCB            : TDCB;                 // �c�b�a�\����

  ReadHandle      : THandle;              // �ǂݍ��ݏI���V�O�i��
  RecvOverLap     : TOverlapped;          // ��M�p�I�[�o�[���b�v�\����

  TransBufLen     : Integer;              // ���M�ҋ@�o�C�g��
  TransCnt        : Integer;              // ���M�L���[�ɑ�������


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
//  �ʐM�R���|�[�l���g�̃N���X                                          //
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
//  �萔�̒�`                                                          //
//                                                                      //
//**********************************************************************//
const
  // �ʐM�G���[�C�x���g�̃}�X�N
  ERR_MASK = CE_FRAME or CE_OVERRUN or CE_RXPARITY;

  // �g�p����C�x���g�̃}�X�N
  EV_MASK = EV_RXCHAR or EV_BREAK or EV_ERR;

  // ���[�U�[�E�B���h�E���b�Z�[�W
  WM_COMM_RECEIVE = WM_USER + $100 + 0;
  WM_COMM_BREAK   = WM_USER + $100 + 1;
  WM_COMM_ERR     = WM_USER + $100 + 2;

  // �X���b�h���b�Z�[�W
  TM_COMM_TRANS = WM_USER + $100 + 0;
  TM_COMM_END   = WM_USER + $100 + 1;

  // �t���[����̃Z�b�g���e
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
//  �` �R���|�[�l���g�쐬 �`                                            //
//                                                                      //
//**********************************************************************//
constructor TCustomCommX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then begin
    FHWnd := AllocateHWnd(WndProc);
  end;

  FHandle := INVALID_HANDLE_VALUE;        // �n���h����������

  TransThread := Nil;                     // ���M�p�X���b�h
  ReceiveThread := Nil;                   // ��M�p�X���b�h

  FPortNo := 1;                           // �ʐM�|�[�g�ԍ�
  FBitRate := 9600;                       // �ʐM���x�i�������j
  FCharSize := 8;                         // �P�����̃r�b�g��
  FParityBit := cpbNone;                  // �p���e�B�`�F�b�N�̕���
  FStopBit := csb1;                       // �X�g�b�v�r�b�g��
  FFlowCtrl := cfcRtsCts;                 // �t���[�������

  FTimeOutTrans := 3000;                  // ���M�^�C���A�E�g���ԁi�����j
  FTimeOutReceive := 3000;                // ��M�^�C���A�E�g���ԁi�����j

  FXonChar := #$11;                       // XON����
  FXoffChar := #$13;                      // XOFF����

  FBufLenTrans := 2048;                   // ���M�o�b�t�@��
  FBufLenReceive := 2048;                 // ��M�o�b�t�@��

  FDCB.DCBLength := Sizeof(TDCB);         // �c�b�a�\���̂̃T�C�Y

  with RecvOverLap do begin               // ��M�p�I�[�o�[���b�v�\����
    Internal := 0;
    InternalHigh := 0;
    Offset := 0;
    OffsetHigh := 0;
    hEvent := 0;
  end;

  TransBufLen := 0;                       // ���M�ҋ@�o�C�g��
  TransCnt := 0;                          // ���M�L���[�ɑ�������

  FOnReceive := Nil;                      // ��M�ʒm�C�x���g
  FOnError := Nil;                        // �ʐM�G���[�C�x���g
  FOnBreak := Nil;                        // �u���[�N�M���C�x���g

  InitializeCriticalSection(Critical1);   // �N���e�B�J���Z�N�V�����̏�����
  InitializeCriticalSection(Critical2);   // �N���e�B�J���Z�N�V�����̏�����
end;

//**********************************************************************//
//                                                                      //
//  �` �R���|�[�l���g�j�� �`                                            //
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

  DeleteCriticalSection(Critical1);       // �N���e�B�J���Z�N�V�����̏I��
  DeleteCriticalSection(Critical2);       // �N���e�B�J���Z�N�V�����̏I��

  inherited Destroy;
end;

//**********************************************************************//
//                                                                      //
//  �` �ʐM�|�[�g�̃I�[�v�� �`                                          //
//                                                                      //
// - input -  �Ȃ�                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.PortOpen;
var
  PortName  : array [0..15] of Char;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    raise ECommError.Create('�|�[�g�́A���łɃI�[�v������Ă��܂�');
  end;

  StrPCopy(PortName, '\\.\COM' + IntToStr(FPortNo));        // �|�[�g���̍쐬

  FHandle := CreateFile(PortName,                           // �|�[�g��
                        GENERIC_READ or                     // �ǂݍ��݃A�N�Z�X
                        GENERIC_WRITE,                      // �������݃A�N�Z�X
                        0,                                  // ���L�̑ΏۂƂ��Ȃ�
                        Nil,                                // �Z�L�����e�B�����Ȃ�
                        OPEN_EXISTING,                      // �I�[�v���i�ʐM�ł͕K�����̐ݒ�j
                        FILE_FLAG_OVERLAPPED,               // �I�[�o�[���b�v���o�͂��s��
                        0);                                 // �e���v���[�g�t�@�C���A�N�Z�X�Ȃ�

  if FHandle = INVALID_HANDLE_VALUE then begin
    raise ECommError.Create('�|�[�g���I�[�v���ł��܂���ł���');
  end;

  try
    TransBufLen := 0;                                       // ���M�ҋ@�o�C�g��
    TransCnt := 0;                                          // ���M�L���[�ɑ�������

    SetBufferLength;                                        // ����M�o�b�t�@���̐ݒ�

    PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);     // ���M�o�b�t�@��j��
    PurgeComm(FHandle, PURGE_RXABORT or PURGE_RXCLEAR);     // ��M�o�b�t�@��j��

    GetCommState(FHandle, FDCB);                            // ���݂�TDCB�̎擾

    SetBitRate(FBitRate);                                   // �ʐM���x�i�{�[���[�g�j�̐ݒ�
    SetCharSize(FCharSize);                                 // �P�L�����N�^�̃r�b�g���̐ݒ�
    SetParityBit(FParityBit);                               // �p���e�B�`�F�b�N�̕����̐ݒ�
    SetStopBit(FStopBit);                                   // �X�g�b�v�r�b�g���̐ݒ�
    SetFlowCtrl(FFlowCtrl);                                 // �t���[��������̐ݒ�

    SetTimeOut;                                             // ����M�^�C���A�E�g���Ԃ̐ݒ�

    SetXonChar(FXonChar);                                   // XON����
    SetXoffChar(FXoffChar);                                 // XOFF����

    SetCommMask(FHandle, EV_MASK);                          // �g�p����C�x���g�̐ݒ�

    ReadHandle := CreateEvent(Nil, True, False, Nil);       // �ǂݍ��ݏI���V�O�i��
    RecvOverLap.hEvent := ReadHandle;                       // ��M�p�I�[�o�[���b�v�\����

    ThreadCreate;                                           // �X���b�h�̐���
  except
    PortClose;                                              // ���s���A�|�[�g�N���[�Y
    raise;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �X���b�h�̐��� �`                                                //
//                                                                      //
// - input -  �Ȃ�                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.ThreadCreate;
begin
  TransThread := Nil;       // �|�C���^�̏�����
  ReceiveThread := Nil;     // �|�C���^�̏�����

  // ���M�X���b�h�̐���
  try
    TransThread := TCommTransThread.Create(Self, FHandle, FHWnd);
    TransThread.HalfWaitTim := (1000 div (FBitRate div 10)) + 1;
  except
    raise ECommError.Create('���M�p�X���b�h�𐶐��ł��܂���ł���');
  end;
  TransThread.Resume;

  // ��M�p�X���b�h�̐���
  try
    ReceiveThread := TCommReceiveThread.Create(FHandle, FHWnd);
  except
    raise ECommError.Create('��M�p�X���b�h�𐶐��ł��܂���ł���');
  end;
  ReceiveThread.Resume;

  Sleep(100);
end;

//**********************************************************************//
//                                                                      //
//  �` �ʐM�|�[�g�̃N���[�Y �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.PortClose;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    SetDtrSignal(False);                                    // �c�s�q���I�t�ɂ���
    SetRtsSignal(False);                                    // �q�s�r���I�t�ɂ���

    ClearTransBuf;                                          // ���M�o�b�t�@��j��
    ClearReceiveBuf;                                        // ��M�o�b�t�@��j��

    Sleep(100);                                             // ���f�����m���ɃI�t���邽��

    if TransThread <> Nil then begin
      TransThread.Terminate;                                // ���M�X���b�h�I��
      PostThreadMessage(TransThread.ThreadID, TM_COMM_END,
                        0, 0);
    end;

    if ReceiveThread <> Nil then begin
      ReceiveThread.Terminate;                              // ��M�X���b�h�I��
      SetEvent(ReceiveThread.ExitHandle);                   // �C�x���g�҂��I��
    end;

    CloseHandle(ReadHandle);                                // �ǂݍ��ݏI���V�O�i���̃N���[�Y
    ReadHandle := 0;

    CloseHandle(FHandle);                                   // �ʐM�|�[�g���N���[�Y
    FHandle := INVALID_HANDLE_VALUE;

    Sleep(200);                                             // �X���b�h�̏I���҂�
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �|�[�g�ԍ��̐ݒ� �`                                              //
//                                                                      //
// - input -  PortNo  Integer   �|�[�g�ԍ�                              //
//                                                                      //
// - output - �Ȃ�                                                      //
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
//  �` �ʐM���x�̐ݒ� �`                                                //
//                                                                      //
// - input -  BitRate   Integer   �ʐM���x�i�������j                    //
//                                                                      //
//                                300/600/1200/2400/4800/9600           //
//                                14400/19200/38400/56000/57600         //
//                                115200/128000/256000 �Ȃ�             //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
// �� ���ۂɐݒ�ł��邩�ǂ����́A�n�[�h�E�F�A��h���C�o�ɂ��B        //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBitRate(const BitRate: Integer);
begin
  FBitRate := BitRate;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.BaudRate := BitRate;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('�ʐM���x�̐ݒ�ŃG���[���������܂���');
    end;
    if TransThread <> Nil then begin
      TransThread.HalfWaitTim := (1000 div (FBitRate div 10)) + 1;
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ����M����P�L�����N�^�̃r�b�g���̐ݒ� �`                        //
//                                                                      //
// - input -  CharSize    Integer   �r�b�g��    5..8                    //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
// �� ���ۂɐݒ�ł��邩�ǂ����́A�n�[�h�E�F�A��h���C�o�ɂ��B        //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetCharSize(const CharSize: Integer);
begin
  FCharSize := CharSize;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.ByteSize := CharSize;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('�ʐM�r�b�g���̐ݒ�ŃG���[���������܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �p���e�B�`�F�b�N�����̐ݒ� �`                                    //
//                                                                      //
// - input -  ParityBit   TCommParityBits   �p���e�B�^�C�v              //
//                                                                      //
//                                          cpbNone:  �Ȃ�              //
//                                          cpbOdd:   �              //
//                                          cpbEven:  ����              //
//                                                                      //
// - output - �Ȃ�                                                      //
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
      raise ECommError.Create('�ʐM�p���e�B�`�F�b�N�̐ݒ�ŃG���[���������܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �X�g�b�v�r�b�g���̐ݒ� �`                                        //
//                                                                      //
// - input -  StopBit   TCommStopBits   �X�g�b�v�r�b�g                  //
//                                                                      //
//                                      csb1:   �P�r�b�g                //
//                                      csb1p5: �P�D�T�r�b�g            //
//                                      csb2:   �Q�r�b�g                //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
// �� ���ۂɐݒ�ł��邩�ǂ����́A�n�[�h�E�F�A��h���C�o�ɂ��B        //
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
      raise ECommError.Create('�ʐM�X�g�b�v�r�b�g���̐ݒ�ŃG���[���������܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �t���[��������̐ݒ� �`                                          //
//                                                                      //
// - input -  FlowCtrl  TCommFlowCtrls   �������                       //
//                                                                      //
//                      cfcRtsCts:    RTS/CTS������s��                 //
//                      cfcDtrDsr:    DTR/DSR������s��                 //
//                      cfcXonXoff:   XON/XOFF������s��                //
//                      cfcHalfHigh:  ����d����𑗐M��ON�ōs��        //
//                      cfcHalfLow:   ����d����𑗐M��OFF�ōs��       //
//                      cfcNone:      ����������s��Ȃ�                //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetFlowCtrl(const FlowCtrl: TCommFlowCtrls);
var
  Flag  : DWORD;
begin
  FFlowCtrl := FlowCtrl;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    Flag := 0;
    if FlowCtrl = cfcRtsCts then begin                // RTS/CTS�۰����̏ꍇ
      Flag := DCB_CTS_FLOW or DCB_RTS_HANDSHAKE;
      Flag := Flag or DCB_DTR_ENABLE;
    end
    else if FlowCtrl = cfcDtrDsr then begin           // DTR/DSR�۰����̏ꍇ
      Flag := DCB_DSR_FLOW or DCB_DTR_HANDSHAKE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else if FlowCtrl = cfcXonXoff then begin          // XON/XOFF�۰����̏ꍇ
      Flag := DCB_OUT_XON_XOFF or DCB_IN_XON_XOFF;
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else if FlowCtrl = cfcHalfHigh then begin         // ����d ���M��ON �̏ꍇ
      Flag := Flag or DCB_DTR_ENABLE;
    end
    else if FlowCtrl = cfcHalfLow then begin          // ����d ���M��OFF �̏ꍇ
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end
    else begin                                        // �Ȃ�
      Flag := Flag or DCB_DTR_ENABLE;
      Flag := Flag or DCB_RTS_ENABLE;
    end;

    FDCB.XonLim := (FBufLenReceive div 4);            // �R�^�S�ȏ� �� �w�n�m�𑗐M
    FDCB.XoffLim := (FBufLenReceive div 4);           // �P�^�S�ȉ� �� �w�n�e�e�𑗐M

    FDCB.Flags := Flag or $00000003;                  // �o�C�i���]���ƃp���e�B�`�F�b�N�L��

    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('�ʐM�t���[����̐ݒ�Ŏ��s���܂���');
    end;
  end
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ���M�^�C���A�E�g���Ԃ̐ݒ� �`                                    //
//                                                                      //
// - input -  TimeOut   Integer   ���M�^�C���A�E�g���ԁi�����j          //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOutTrans(const TimeOut: Integer);
begin
  FTimeOutTrans := TimeOut;
  SetTimeOut;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ��M�^�C���A�E�g���Ԃ̐ݒ� �`                                    //
//                                                                      //
// - input -  TimeOut   Integer   ��M�^�C���A�E�g���ԁi�����j          //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOutReceive(const TimeOut: Integer);
begin
  FTimeOutReceive := TimeOut;
  SetTimeOut;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ����M�^�C���A�E�g���Ԃ̐ݒ� �`                                  //
//                                                                      //
// - input -  �Ȃ�                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetTimeOut;
var
  CommTimeOut : TCommTimeouts;    // �^�C���A�E�g�p�����[�^
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
      raise ECommError.Create('�ʐM�^�C���A�E�g���Ԃ̐ݒ�Ŏ��s���܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �w���������̐ݒ� �`                                              //
//                                                                      //
// - input -  XonChar   Char      �w��������                            //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetXonChar(const XonChar: Char);
begin
  FXonChar := XonChar;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.XonChar := XonChar;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('�w���������̐ݒ�ŃG���[���������܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �w�����������̐ݒ� �`                                            //
//                                                                      //
// - input -  XoffChar  Char      �w����������                          //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetXoffChar(const XoffChar: Char);
begin
  FXoffChar := XoffChar;
  if FHandle <> INVALID_HANDLE_VALUE then begin
    FDCB.XoffChar := XoffChar;
    if not SetCommState(FHandle, FDCB) then begin
      raise ECommError.Create('�w�����������̐ݒ�ŃG���[���������܂���');
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ���M�o�b�t�@���̐ݒ� �`                                          //
//                                                                      //
// - input -  BufLen  Integer   ���M�o�b�t�@���i�o�C�g�j                //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufLenTrans(const BufLen: Integer);
begin
  FBufLenTrans := BufLen;
  SetBufferLength;            // ����M�o�b�t�@���̐ݒ�
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ��M�o�b�t�@���̐ݒ� �`                                          //
//                                                                      //
// - input -  BufLen  Integer   ��M�o�b�t�@���i�o�C�g�j                //
//                                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//  �� ��M�o�b�t�@�����ς��ƃt���[����̂������l���ς��̂ŁA�t     //
//   ���[��������̐ݒ���Ăяo���B                                     //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufLenReceive(const BufLen: Integer);
begin
  FBufLenReceive := BufLen;
  SetBufferLength;            // ����M�o�b�t�@���̐ݒ�
  SetFlowCtrl(FFlowCtrl);     // �t���[��������̐ݒ�
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ����M�o�b�t�@���̐ݒ� �`                                        //
//                                                                      //
// - input -  �Ȃ�                                                      //
// - output - �Ȃ�                                                      //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.SetBufferLength;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if not SetupComm(FHandle, FBufLenReceive, FBufLenTrans) then begin
      raise ECommError.Create('�ʐM�p�|�[�g�̃o�b�t�@�������ł��܂���ł���');
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  �` �L�����N�^�̑��M �`                                              //
//                                                                      //
// - input -    c   Char  �L�����N�^                                    //
//                                                                      //
// - output -   Boolean   ����I����  True                              //
//                        �ُ�I����  False                             //
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
//  �` �����񑗐M �`                                                    //
//                                                                      //
// - input -    s   string    ������                                    //
//                                                                      //
// - output -   Boolean       ����I����    True                        //
//                            �ُ�I����    False                       //
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
//  �` �f�[�^�̃u���b�N���M �`                                          //
//                                                                      //
// - input -    const     Data      �f�[�^�o�b�t�@                      //
//              BufSize   Integer   ���M�o�C�g��                        //
//                                                                      //
// - output -   Boolean   ����I����  True                              //
//                        �ُ�I����  False                             //
//                                                                      //
//  �� ���M�o�C�g���̃��������m�ۂ��A�f�[�^�o�b�t�@�̃R�s�[������       //
//   ����A���b�Z�[�W�L���[�ɃR�s�[�̃A�h���X�ƃT�C�Y���i�[���邾       //
//   ���ŁA�����ɕԂ�B                                                 //
//                                                                      //
//  �� ���M����������O�ɁA���̊֐���A���ŌĂяo���Ă����Ȃ����A     //
//   ���܂葽���̃��b�Z�[�W�𑗂�ƃ��\�[�X�����Ȃ��Ȃ�n�r���s��       //
//   ��ɂȂ�̂ŁA�i�[�ł��鐔�͂R�Q�܂łɂ��Ă���B                   //
//     ����ȏ�̂Ƃ��͓����Ń��[�v����B                               //
//                                                                      //
//**********************************************************************//
function TCustomCommX.TransBlock(const Data; BufSize: Integer): Boolean;
type
  BytePtr = ^byte;
var
  DatPtr    : BytePtr;    // ���M�f�[�^�ւ̃|�C���^
  BufPtr    : BytePtr;    // ���M�f�[�^�o�b�t�@�ւ̃|�C���^
  SendSize  : Integer;    // ���M�f�[�^�T�C�Y
  cnt       : Integer;
begin
  if BufSize = 0 then begin
    Result := True;
    exit;
  end;

  DatPtr := @Data;
  repeat
    // ProcessMessages �ŏI�����Ă��邱�Ƃ��l������̂ŁA�����Ń`�F�b�N����
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
//  �` �f�[�^�̎�M �`                                                  //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Integer  ��M�f�[�^                                     //
//                                                                      //
//  �� �G���[���M�������Ȃ��ꍇ�� -1 ��Ԃ��B���̂��߂� Integer �^    //
//   �Œl��Ԃ��悤�ɂ��Ă���B                                         //
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
    Result := -1;         // �G���[�A�܂��͎�M�����Ȃ�
  end;
end;

//**********************************************************************//
//                                                                      //
//  �` �f�[�^�̃u���b�N��M �`                                          //
//                                                                      //
// - input -    Buf       PChar     �i�[����o�b�t�@                    //
//              MaxSize   Integer   ��M����ő�T�C�Y                  //
//                                                                      //
// - output -   Integer   ��M�����T�C�Y�i�G���[�̂Ƃ� �|�P�j           //
//                                                                      //
//**********************************************************************//
function TCustomCommX.ReceiveBlock(Buf: PChar; const MaxSize: Integer): Integer;
var
  ReadSize  : DWORD;        // �ǂ݂Ƃ����o�C�g��
  ErrorMask : DWORD;        // �G���[�R�[�h
  ComStat   : TCOMSTAT;     // �ʐM���
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
//  �` ���M�o�b�t�@�̃N���A �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//  �� �X���b�h�̃��b�Z�[�W�L���[�ɗ��܂����f�[�^���N���A������@��     //
//   �킩��Ȃ������̂Ńt���O�𗧂ĂăX���b�h���g�ɃN���A�����Ă���B   //
//                                                                      //
//**********************************************************************//
procedure TCustomCommX.ClearTransBuf;
var
  len : Integer;
begin
  if FHandle <> INVALID_HANDLE_VALUE then begin
    if TransThread <> Nil then begin
      TransThread.MsgClearFlg := True;                      // �N���A�t���O�𗧂Ă�
      repeat
        EnterCriticalSection(Critical1);
        PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);
        LeaveCriticalSection(Critical1);
        Sleep(1);
        EnterCriticalSection(Critical2);
        len := TransBufLen;
        LeaveCriticalSection(Critical2);
      until len = 0;
      TransThread.MsgClearFlg := False;                     // �N���A�t���O��߂�
    end
    else begin
      PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR);
    end;
  end;
end;

//**********************************************************************//
//                                                                      //
//  �` ��M�o�b�t�@�̃N���A �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
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
//  �` ���M�o�b�t�@�̃f�[�^�����擾 �`                                  //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Integer   �f�[�^���i�o�C�g�j                            //
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
//  �` ��M�o�b�t�@�̃f�[�^�����擾 �`                                  //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Integer   �f�[�^���i�o�C�g�j                            //
//                                                                      //
//**********************************************************************//
function TCustomCommX.GetReceiveLength: Integer;
var
  ErrorMask : DWORD;        // �G���[�R�[�h
  ComStat   : TCOMSTAT;     // �ʐM���
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
//  �` �q�s�r�M����ݒ肷�� �`                                          //
//                                                                      //
// - input -    Signal  Boolean   �ݒ�l    True:   �n�m                //
//                                          False:  �n�e�e              //
//                                                                      //
// - output -   �Ȃ�                                                    //
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
//  �` �c�s�q�M����ݒ肷�� �`                                          //
//                                                                      //
// - input -    Signal  Boolean   �ݒ�l    True:   �n�m                //
//                                          False:  �n�e�e              //
//                                                                      //
// - output -   �Ȃ�                                                    //
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
//  �` �b�s�r�M�����擾���� �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Boolean  �b�s�r�M��                                     //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsCtsSignal: Boolean;
begin
  Result := IsSignal(MS_CTS_ON);
end;

//**********************************************************************//
//                                                                      //
//  �` �c�r�q�M�����擾���� �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Boolean  �c�r�q�M��                                     //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsDsrSignal: Boolean;
begin
  Result := IsSignal(MS_DSR_ON);
end;

//**********************************************************************//
//                                                                      //
//  �` �����O�C���W�Q�[�^�i�q�h�A�b�h�j�M���̌��o �`                    //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Boolean  �����O�C���W�Q�[�^�i�q�h�A�b�h�j�M��           //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsRingSignal: Boolean;
begin
  Result := IsSignal(MS_RING_ON);
end;

//**********************************************************************//
//                                                                      //
//  �` �q�k�r�c�i�c�b�c�A�b�c�j�M���̌��o �`                            //
//                                                                      //
// - input -    �Ȃ�                                                    //
//                                                                      //
// - output -   Boolean  �q�k�r�c�M��                                   //
//                                                                      //
//**********************************************************************//
function TCustomCommX.IsRlsdSignal: Boolean;
begin
  Result := IsSignal(MS_RLSD_ON);
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` �M���̌��o �`                                                    //
//                                                                      //
// - input -    Signal  DWORD   �M���̎��                              //
//                                                                      //
// - output -   Boolean   �M���̘_��    True:   �n�m                    //
//                                      False:  �n�e�e                  //
//                                                                      //
//----------------------------------------------------------------------//
function TCustomCommX.IsSignal(Signal: DWORD): Boolean;
var
  ModemStat : DWORD;    // ���f���̃R���g���[�����W�X�^�l
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
//  �` �u���[�N�M���̑��M �`                                            //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
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
//  �` ���b�Z�[�W�ɂ��e������s�� �`                                  //
//                                                                      //
// - input -    Msg   TMessage    ���b�Z�[�W                            //
//                                                                      //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.WndProc(var Msg: TMessage);
begin
  with Msg do begin
    case Msg of
      WM_COMM_RECEIVE : ReceiveEvent;         // ��M�ʒm�C�x���g
      WM_COMM_BREAK   : BreakEvent;           // �u���[�N�M���C�x���g
      WM_COMM_ERR     : ErrEvent(lParam);     // �G���[�C�x���g
    else
      Result := DefWindowProc(FHWnd, Msg, wParam, lParam);
    end;
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ��M�ʒm�C�x���g �`                                              //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCustomCommX.ReceiveEvent;
var
  len : Integer;        // �f�[�^��
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
//  �` �u���[�N�M���C�x���g �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
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
//  �` �G���[�C�x���g �`                                                //
//                                                                      //
// - input -    ErrorWord   DWORD   �G���[�̎��                        //
//                                                                      //
//                          CE_FRAME:     �t���[�~���O�G���[            //
//                          CE_OVERRUN:   �o�b�t�@�I�[�o�[����          //
//                          CE_RXPARITY:  �p���e�B�G���[                //
//                                                                      //
// - output -   �Ȃ�                                                    //
//                                                                      //
//  �� �G���[�̎�ނ͑g�ݍ��킹�ŕ\�������B                           //
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
//  �` ���M���s���X���b�h�̃R���X�g���N�^ �`                            //
//                                                                      //
// - input -    Owner   TComponet   �e�R���|�[�l���g                    //
//              Handle  THandle     �ʐM�n���h��                        //
//              Window  THandle     �E�B���h�E�n���h��                  //
//                                                                      //
// - output -   �Ȃ�                                                    //
//                                                                      //
//  �� Window �ɂ́A���b�Z�[�W���������邽�߂̃n���h�����w�肷��B      //
//                                                                      //
//----------------------------------------------------------------------//
constructor TCommTransThread.Create(Owner: TComponent; Handle: THandle; Window: THandle);
begin
  inherited Create(True);     // �T�X�y���h��Ԃō쐬
  FOwner := Owner;            // �I�[�i�[�̕ۑ�
  CommHandle := Handle;
  CommWindow := Window;
  MsgClearFlg := False;
  HalfWaitTim := 0;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ���M���s���X���b�h �`                                            //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.Execute;
type
  BytePtr = ^Byte;
var
  WriteHandle : THandle;            // �������݊����V�O�i��
  Msg         : TMsg;               // ���b�Z�[�W
  DataSize    : DWORD;              // ���M�o�C�g��
  DataPtr     : BytePtr;            // ���M�o�b�t�@�ւ̃|�C���^
  ComStat     : TCOMSTAT;           // �ʐM���
  TransSize   : DWORD;              // ���M�ł����o�C�g��
  ErrorMask   : DWORD;              // �G���[�R�[�h
  Overlap     : TOVERLAPPED;        // �I�[�o�[���b�v�\����
  HalfCtrlFlg : Boolean;            // ����d����t���O
  Rslt        : Boolean;            // �Ԃ�l
begin
  WriteHandle := CreateEvent(Nil, True, False, Nil);    // �������݊����V�O�i��

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
//  �` ����d����̑��M�X�^�[�g �`                                      //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.HalfCtrlStart;
begin
  if TCustomCommX(FOwner).FFlowCtrl = cfcHalfHigh then begin
    Sleep(50);            // �Փˉ���i����̂q�s�r����I����҂j
    EscapeCommFunction(CommHandle, SETRTS);
    Sleep(HalfWaitTim);   // ���M�J�n���ɍŏ��̈�o�C�g�ڂ����Ȃ��悤��
  end
  else if TCustomCommX(FOwner).FFlowCtrl = cfcHalfLow then begin
    Sleep(50);            // �Փˉ���i����̂q�s�r����I����҂j
    EscapeCommFunction(CommHandle, CLRRTS);
    Sleep(HalfWaitTim);   // ���M�J�n���ɍŏ��̈�o�C�g�ڂ����Ȃ��悤��
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ����d����̑��M�I�� �`                                          //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommTransThread.HalfCtrlEnd;
begin
  if TCustomCommX(FOwner).FFlowCtrl = cfcHalfHigh then begin
    Sleep(HalfWaitTim);   // �h�b�̃o�b�t�@�Ɏc���Ă���ꍇ������̂�
    EscapeCommFunction(CommHandle, CLRRTS);
  end
  else if TCustomCommX(FOwner).FFlowCtrl = cfcHalfLow then begin
    Sleep(HalfWaitTim);   // �h�b�̃o�b�t�@�Ɏc���Ă���ꍇ������̂�
    EscapeCommFunction(CommHandle, SETRTS);
  end;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ��M���s���X���b�h�̃R���X�g���N�^ �`                            //
//                                                                      //
// - input -    Handle  THandle     �ʐM�n���h��                        //
//              Window  THandle     �E�B���h�E�n���h��                  //
//                                                                      //
// - output -   �Ȃ�                                                    //
//                                                                      //
//  �� Window �ɂ́A���b�Z�[�W���������邽�߂̃n���h�����w�肷��B      //
//                                                                      //
//----------------------------------------------------------------------//
constructor TCommReceiveThread.Create(Handle: THandle; Window: THandle);
begin
  inherited Create(True);       // �T�X�y���h��Ԃō쐬
  CommHandle := Handle;
  CommWindow := Window;
end;

//----------------------------------------------------------------------//
//                                                                      //
//  �` ��M���s���X���b�h �`                                            //
//                                                                      //
// - input -    �Ȃ�                                                    //
// - output -   �Ȃ�                                                    //
//                                                                      //
//----------------------------------------------------------------------//
procedure TCommReceiveThread.Execute;
var
  EvOverLap : TOVERLAPPED;      // �I�[�o�[���b�v�\����
  EventMask : DWORD;            // ���������C�x���g
  ErrorMask : DWORD;            // �G���[�R�[�h
  ComStat   : TCOMSTAT;         // �ʐM���
begin
  ExitHandle := CreateEvent(Nil, False, False, Nil);    // �C�x���g�҂��I���V�O�i��

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
//  �` �R���|�[�l���g�̓o�^ �`                                          //
//                                                                      //
//----------------------------------------------------------------------//
procedure Register;
begin
  RegisterComponents('LIB-X', [TCommX]);
end;

end.

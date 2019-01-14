unit CPortX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Dialogs,
  CPort, CPortCtl;

//**********************************************************************//
//                                                                      //
//  ��O�ʒm�p�̃N���X                                                  //
//                                                                      //
//**********************************************************************//
type
  EAppError = class(Exception);

type
  TRecvMsgEvent = procedure(Sender: TObject; RecvStr: string) of object;

//**********************************************************************//
//                                                                      //
//  CommX�̊g���N���X                                          //
//                                                                      //
//**********************************************************************//
type
  TCPortX = class(TComPort)
  private
    { Private �錾 }

    FActive: boolean;
    FDelimiter: string;
    FRecvMsg: string;
    FOnRecvMsg: TRecvMsgEvent;
//    FTriggersOnRxChar: boolean;
    procedure SetBaudRate(const Value: string);
    procedure SetDataBits(const Value: string);
    procedure SetFlowControl(const Value: string);
    procedure SetParity(const Value: string);
    procedure SetStopBits(const Value: string);
    procedure SetDelimiter(const Value: string);

    procedure DoRecvMsg;
    procedure RecvOnRxChar(Sender: TObject; count: integer);
    function GetBaudRate: string;
    function GetDataBits: string;
    function GetFlowControl: string;
    function GetStopBits: string;
    function GetParity: string;
  public
    { Public �錾 }
    property Active: boolean read FActive write FActive;
    property BaudRate: string read GetBaudRate write SetBaudRate;
    property DataBits: string read GetDataBits write SetDataBits;
    property Delimiter: string read FDelimiter write SetDelimiter;
//    property Delimiter;
    property FlowControl: string read GetFlowControl write SetFlowControl;
    property RecvMsg: string read FRecvMsg;
    property Parity: string read GetParity write SetParity;
    property StopBits: string read GetStopBits write SetStopBits;
//    property TriggersOnRxChar: boolean read FTriggersOnRxChar write SetTriggersOnRxChar;
    property OnRecvMsg: TRecvMsgEvent read FOnRecvMsg write FOnRecvMsg;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Open();
    procedure Close;
    procedure Clear();
    function  RecvStr(Len: integer):string;
    procedure SendStr(str: string);
  end;

implementation

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Comm�|�[�g�̏���
//
/////////////////////////////////////////////////////////////////////////////////////////////

constructor TCPortX.Create(Owner: TComponent);
begin
  Inherited;
  try
    with Timeouts do
      begin
      //  (��M�g�[�^���^�C���A�E�g) =  ReadTotalMultiplier * (��M�\��o�C�g��) +  ReadTotalConstant
      ReadInterval        := 100;   // 0.5�b(ms)(0�Ń^�C���A�E�g�Ȃ�)
      ReadTotalMultiplier := 10;    // Read : 1�o�C�g�ɑ΂���^�C���A�E�g�搔(0�Ń^�C���A�E�g�Ȃ�)
      ReadTotalConstant   := 100;   // Read : 0�o�C�g���̃^�C���A�E�g�萔(0�Ń^�C���A�E�g�Ȃ�)
      //  (���M�g�[�^���^�C���A�E�g) =  WriteTotalMultiplier * (���M�\��o�C�g��) +  WriteTotalConstant
      WriteTotalMultiplier := 10;   // Write : 1�o�C�g�ɑ΂���^�C���A�E�g�搔(0�Ń^�C���A�E�g�Ȃ�)
      WriteTotalConstant  := 100;   // Write : 0�o�C�g���̃^�C���A�E�g�萔(0�Ń^�C���A�E�g�Ȃ�)
      end;
    OnRxChar := RecvOnRxChar;
  finally

  end;
end;

destructor TCPortX.Destroy;
begin
  Inherited;
end;

procedure TCPortX.Open();
begin
//  if FDelimiter = '' then
//    TriggersOnRxChar := false
//  else
//    TriggersOnRxChar := true;
//  FActive := true;
  inherited;
end;

procedure TCPortX.Close();
begin
  Inherited;
  FActive := false;
end;

procedure TCPortX.Clear();
begin
  ClearBuffer(true,true);
end;

function TCPortX.RecvStr(Len: integer):string;
var
  buf: string;
  j : Integer  ;
begin
  Result := '';
  j := ReadStr(buf, Len);
  try
    Result := Copy(buf, 1, j);
  except
    Result := '';
  end;
end;

procedure TCPortX.RecvOnRxChar(Sender: TObject; count: integer);
begin
  ReadStr(FRecvMsg, count);
  DoRecvMsg;
end;

procedure TCPortX.DoRecvMsg;
begin
  if Assigned(FOnRecvMsg) then
    FOnRecvMsg(self, FRecvMsg);
end;

function TCPortX.GetBaudRate: string;
begin
  result := BaudRateToStr(inherited BaudRate);
end;

function TCPortX.GetDataBits: string;
begin
  result := DataBitsToStr(inherited DataBits);
end;

function TCPortX.GetFlowControl: string;
begin
  result := FlowControlToStr(inherited FlowControl.FlowControl);
end;

function TCPortX.GetParity: string;
begin
  result := ParityToStr(inherited Parity.Bits);
end;

function TCPortX.GetStopBits: string;
begin
  result := StopBitsToStr(inherited StopBits);
end;

procedure TCPortX.SendStr(str: string);
begin
  WriteStr(Str);
end;

procedure TCPortX.SetBaudRate(const Value: string);
begin
  inherited Baudrate :=  StrToBaudRate(value);
end;

procedure TCPortX.SetDataBits(const Value: string);
begin
  inherited DataBits := StrToDataBits(Value);
end;

procedure TCPortX.SetDelimiter(const Value: string);
begin
  FDelimiter := Value;
  if Fdelimiter = '' then
    TriggersOnRxChar :=false
  else
    begin
    TriggersOnRxChar :=true;
    EventChar := FDelimiter[1];
    end;
end;

procedure TCPortX.SetFlowControl(const Value: string);
begin
  inherited FlowControl.FlowControl := StrToFlowControl(Value);
end;

procedure TCPortX.SetParity(const Value: string);
begin
  inherited Parity.Bits := StrToParity(Value);
end;

procedure TCPortX.SetStopBits(const Value: string);
begin
  inherited StopBits := StrToStopBits(VAlue);
end;

end.

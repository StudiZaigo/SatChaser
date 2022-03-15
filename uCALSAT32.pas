unit uCALSAT32;

interface

uses Classes, SysUtils, System.Types, Winapi.Windows, Winapi.Messages;

function EnumerateChileWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

type
  TCALSAT32 = class(TComponent)
  private
    Handle: THandle;

    FActive: boolean;
    FPosition: TRect;
    procedure SetActive(const Value: boolean);
    function Open: boolean;
    function GetPosition: boolean;
    { Private êÈåæ }


  public
    { Public êÈåæ }
    property Active:boolean read FActive write SetActive;
    property Position: TRect read FPosition;
  end;

implementation

function EnumerateChileWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  ClassName: string;
  Name: string;
  Text: string;
  Length: integer;
  Rect: TRect;
begin
  SetLength(ClassName, 255 + 1);
  GetClassName(hWnd, ClassName, 255);
  if ClassName = 'ThunderRT6TextBox' then
    begin
    if GetWindowRect(hWnd, Rect) then
      if Rect = [28, 386, 513, 455] then
        begin
        SetLength(Text, 255 + 1);
        SendMessage(hWnd, WM_GETTEXT, Length(Text), integer(Text));
        end;
    end;
    result := true;
end;

{ TCALSAT32 }

procedure TCALSAT32.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

function TCALSAT32.Open(): boolean;
begin
  result := false;
  Handle := FindWindow(nil, 'CALSAT32(SGP)');
  if Handle <> nil then
    result := true;
end;

function TCALSAT32.GetPosition(): boolean;
begin
  result := false;
  FPosition := [0, 0, 0, 0];
  if Open then
    begin
    result := GetWindowRect(Handle, FPosition);
    end;
end;

end.

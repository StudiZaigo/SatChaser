program SatChaser;

uses
  Windows,
  Forms,
  uMain in 'uMain.pas' {Main},
  uConstant in 'uConstant.pas';

const
  UniqueName = 'MutexSatChaserEx';

var
  hMutex: THandle;

  {$R *.res}

begin
  Application.Initialize;
{アプリケーション二重起動防止}
  hMutex := CreateMutex(nil, False, UniqueName);
  if WaitForSingleObject(hMutex, 0) <> wait_TimeOut then
    begin
    Application.Initialize;
    Application.CreateForm(TMain, Main);
  Application.Run;
    end;
end.




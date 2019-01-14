program SatChaser;

uses
  Windows,
  Forms,
  uMain in 'uMain.pas' {Main},
  uOptions in 'uOptions.pas' {Options},
  uABOUT in 'uABOUT.pas' {AboutBox};

const
  UniqueName = 'MutexSatChaserEx';

var
  hMutex: THandle;

  {$R *.res}

begin
  Application.Initialize;
{�A�v���P�[�V������d�N���h�~}
  hMutex := CreateMutex(nil, False, UniqueName);
  if WaitForSingleObject(hMutex, 0) <> wait_TimeOut then
    begin
    Application.Initialize;
    Application.CreateForm(TMain, Main);
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TOptions, Options);
    Application.Run;
    end;
end.




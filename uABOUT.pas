unit uABOUT;

interface

uses WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  uRoutine;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    lblProductName: TLabel;
    lblVersion: TLabel;
    lblCopyRight: TLabel;
    lblComment: TLabel;
    CompanyName: TLabel;
    lblCompanyName: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

procedure TAboutBox.FormCreate(Sender: TObject);
var
  ExeName: string;
begin
  ExeName := Application.ExeName;
  lblProductName.Caption  :=  GetVersionInfo(ExeName, vrProductName);
  lblVersion.Caption      :=  GetVersionInfo(ExeName, vrFileVersion);
  lblCopyRight.Caption    :=  GetVersionInfo(ExeName, vrLegalCopyright);
  lblComment.Caption      :=  GetVersionInfo(ExeName, vrComments);
  lblCompanyName.Caption  :=  GetVersionInfo(ExeName, vrCompanyName);


end;

end.


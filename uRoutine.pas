unit uRoutine;

interface

uses Classes, SysUtils, Windows, Winapi.ShellAPI,
     TlHelp32, PsAPI,        // PID�擾�̂���
     Types,
     TypInfo,
     Vcl.Dialogs , Vcl.FileCtrl,
     contnrs,          // TObjectList�̂���
     IOUtils,
     Math,
     uRecord,
     SkRegExpW,
     shlobj, ActiveX, Vcl.Controls;     // for ����Holder�̎擾 By Deco��

// System���
//Type
//  TOsVersion = record
//    Name:           string;
//    MainVersion:    integer;
//    MinorVersion:   integer;
//    end;

type
  TVerResourceKey = (
    vrComments,         // �R�����g
    vrCompanyName,      // ��Ж�
    vrFileDescription,  // ����
    vrFileVersion,      // �t�@�C���o�[�W����
    vrInternalName,     // ������
    vrLegalCopyright,   // ���쌠
    vrLegalTrademarks,  // ���W
    vrOriginalFilename, // �����t�@�C����
    vrPrivateBuild,     // �v���C�x�[�g�r���h���
    vrProductName,      // ���i��
    vrProductVersion,   // ���i�o�[�W����
    vrSpecialBuild);    // �X�y�V�����r���h���

type TSYSTEMTIME = record
    wYear:         WORD;
    wMonth:        WORD;
    wDayOfWeek:    WORD;
    wDay:          WORD;
    wHour:         WORD;
    wMinute:       WORD;
    wSecond:       WORD;
    wMilliseconds: WORD;
  end;

type TTIME_ZONE_INFORMATION = record
    Bias:          INTEGER;
    StandardName:  string[32];
    StandardDate:  TSYSTEMTIME;
    StandardBias:  INTEGER;
    DaylightName:  string[32];
    DaylightDate:  TSYSTEMTIME;
    DaylightBias:  INTEGER;
  end;

type
  TDirectionKind = (drLongitude, drLatitude);

type
  TProcessItem = class(TObject)
    ID: Cardinal;
    Name: String;
    Path: String;
  end;


//function ChangeComponent(Original: TComponent; NewClass: TComponentClass): TComponent;
function ChangeComponent(Original: TComponent; NewClass: TComponentClass):   TComponent;



function GetVersionInfo(ExeName: string; KeyWord: TVerResourceKey): string;
function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
//function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
function WinExecAndWait32(FileName: string; Visibility: Integer): Longword;
function GetWindowsDirectory: string;
procedure GetProcExeNameList(Dest: TObjectList);
function GetPidByExeName(ExeName: string): Integer;
procedure  KillProceesByExeName(ExeName: string);
function GetDirectryName(Owner: TComponent; Title: string; var Directry: string): boolean;
function GetOpenFileName(Owner: TComponent; Title, DefaltFolder, Filter: string; var FileName: string): boolean;

////////////////////////////////////////////////////////////////////////////////
//
//   ������Ɋւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////
function DblQuotedStr(value: string): string;
function CompactStr(value: string): string;
function CopyByLength(str:string; len:Integer): string;

function FormalizeDate(pDate:string):string;
function FormalizeTime(pTime:string):string;

////////////////////////////////////////////////////////////////////////////////
//
//   File�Ɋւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////
//procedure FilesList_Get(Path: String; Attr: Integer; AddNoAttr:boolean;
//                    ClrFlag: boolean; theList: TStringList; gosubdir:  boolean;
//                    addYen:boolean; NoReadOnly: Boolean);
//    //  Path        �f�B���N�g���t���p�X
//    //  Attr:       �t�@�C���̑���,�S�ĂȂ�faAnyfile(attr�����͊܂܂�Ȃ�)
//    //  AaddNoAttr  True:�����Ȃ��t�@�C����������}{���C��
//    //  clrFlag:    True:�V�K�@false:������
//    //  theList:    �ꗗ���i�[����TStringList
//    //  gosubdir:   True:�T�u�f�B���N�g������������
//    //  addYen:     True:�f�B���N�g���Ȃ�\������
//    //  noreadonly: True:ReadOnly/Hidden���͂���
//   {sample
//           FilesList_Get ('c:\windows', faAnyFile,True,True,
//                        newStringList,True,True,false);}
//
//procedure GetFilesList(Path: String; Attr: Integer; theList: TStringList);
//
//function RemoveFiles(Path: string):boolean;
//function SHCopyFile(hParent: HWND; NameFrom, NameTo: string): Boolean;
//  var
//  SFO: TSHFileOpStruct;
//  begin
//    NameFrom  := NameFrom + #0#0;
//    NameTo    := NameTo   + #0#0;
//    with SFO do begin
//      Wnd := hParent;
//      wFunc := FO_COPY;
//      pFrom := PChar(NameFrom);
//      pTo := PChar(NameTo);
//      fFlags := FOF_ALLOWUNDO;
//      fAnyOperationsAborted := false;
//      hNameMappings := nil;
//    end;
//    Result := not Boolean(SHFileOperation(SFO));
//  end;
//

function CopyFiles(Source, Destination: string):boolean;

function ExpandEnvironmentString(S: String): String;
//function GetMyDocPath(hwndOwner: THandle; var Path: string; nFolder: integer): boolean;
function SHCopyFile(hParent:HWND; NameFrom, NameTo: string): Boolean;


function  StrToDeg(Value:string; Direction: TDirectionKind; var Degree: double): boolean;
function  DegToStr(Degree: double ; Directin: TDirectionKind): string;

procedure DecodeDeg(Value: Double; var sgn:string; var deg, min, sec: Integer);
function EncodeDeg(sgn:string; var deg, min, sec: Integer; var Value: Double): boolean;

//    �����ƕ������v�Z����
function geoDistance(lat1, log1, lat2, log2, precision: double): double;
function geoDirection(lat1, log1, lat2, log2: double): Double;

function CheckGridLoc(Value: string): boolean;
function GLToDeg(GL: String; var Lon,Lat: Double): boolean;
function DegToGL(Lon, Lat: Double): string;

function isOverlap(Value1, Value2: TControl): boolean;

type
  TLocation = class(TObject)
  private
    { Private �錾 }
  public
    { Public �錾 }
  end;

const
    KeyWordStr: array [TVerResourceKey] of String = (
        'Comments',
        'CompanyName',
        'FileDescription',
        'FileVersion',
        'InternalName',
        'LegalCopyright',
        'LegalTrademarks',
        'OriginalFilename',
        'PrivateBuild',
        'ProductName',
        'ProductVersion',
        'SpecialBuild');

// Windows�̓���t�H���_�̒萔
  CSIDL_DESKTOP                       = $0000; { <desktop> }
  CSIDL_INTERNET                      = $0001; { Internet Explorer (icon on desktop) }
  CSIDL_PROGRAMS                      = $0002; { Start Menu\Programs }
  CSIDL_CONTROLS                      = $0003; { My Computer\Control Panel }
  CSIDL_PRINTERS                      = $0004; { My Computer\Printers }
  CSIDL_PERSONAL                      = $0005; { My Documents.  This is equivalent to CSIDL_MYDOCUMENTS in XP and above }
  CSIDL_FAVORITES                     = $0006; { <user name>\Favorites }
  CSIDL_STARTUP                       = $0007; { Start Menu\Programs\Startup }
  CSIDL_RECENT                        = $0008; { <user name>\Recent }
  CSIDL_SENDTO                        = $0009; { <user name>\SendTo }
  CSIDL_BITBUCKET                     = $000a; { <desktop>\Recycle Bin }
  CSIDL_STARTMENU                     = $000b; { <user name>\Start Menu }
  CSIDL_MYDOCUMENTS                   = $000c; { logical "My Documents" desktop icon }
  CSIDL_MYMUSIC                       = $000d; { "My Music" folder }
  CSIDL_MYVIDEO                       = $000e; { "My Video" folder }
  CSIDL_DESKTOPDIRECTORY              = $0010; { <user name>\Desktop }
  CSIDL_DRIVES                        = $0011; { My Computer }
  CSIDL_NETWORK                       = $0012; { Network Neighborhood (My Network Places) }
  CSIDL_NETHOOD                       = $0013; { <user name>\nethood }
  CSIDL_FONTS                         = $0014; { windows\fonts }
  CSIDL_TEMPLATES                     = $0015;
  CSIDL_COMMON_STARTMENU              = $0016; { All Users\Start Menu }
  CSIDL_COMMON_PROGRAMS               = $0017; { All Users\Start Menu\Programs }
  CSIDL_COMMON_STARTUP                = $0018; { All Users\Startup }
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019; { All Users\Desktop }
  CSIDL_APPDATA                       = $001a; { <user name>\Application Data }
  CSIDL_PRINTHOOD                     = $001b; { <user name>\PrintHood }
  CSIDL_LOCAL_APPDATA                 = $001c; { <user name>\Local Settings\Application Data (non roaming) }
  CSIDL_ALTSTARTUP                    = $001d; { non localized startup }
  CSIDL_COMMON_ALTSTARTUP             = $001e; { non localized common startup }
  CSIDL_COMMON_FAVORITES              = $001f;
  CSIDL_INTERNET_CACHE                = $0020;
  CSIDL_COOKIES                       = $0021;
  CSIDL_HISTORY                       = $0022;
  CSIDL_COMMON_APPDATA                = $0023; { All Users\Application Data }
  CSIDL_WINDOWS                       = $0024; { GetWindowsDirectory() }
  CSIDL_SYSTEM                        = $0025; { GetSystemDirectory() }
  CSIDL_PROGRAM_FILES                 = $0026; { C:\Program Files }
  CSIDL_MYPICTURES                    = $0027; { C:\Program Files\My Pictures }
  CSIDL_PROFILE                       = $0028; { USERPROFILE }
  CSIDL_SYSTEMX86                     = $0029; { x86 system directory on RISC }
  CSIDL_PROGRAM_FILESX86              = $002a; { x86 C:\Program Files on RISC }
  CSIDL_PROGRAM_FILES_COMMON          = $002b; { C:\Program Files\Common }
  CSIDL_PROGRAM_FILES_COMMONX86       = $002c; { x86 C:\Program Files\Common on RISC }
  CSIDL_COMMON_TEMPLATES              = $002d; { All Users\Templates }
  CSIDL_COMMON_DOCUMENTS              = $002e; { All Users\Documents }
  CSIDL_COMMON_ADMINTOOLS             = $002f; { All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS                    = $0030; { <user name>\Start Menu\Programs\Administrative Tools }
  CSIDL_CONNECTIONS                   = $0031; { Network and Dial-up Connections }
  CSIDL_COMMON_MUSIC                  = $0035; { All Users\My Music }
  CSIDL_COMMON_PICTURES               = $0036; { All Users\My Pictures }
  CSIDL_COMMON_VIDEO                  = $0037; { All Users\My Video }
  CSIDL_RESOURCES                     = $0038; { Resource Directory }
  CSIDL_RESOURCES_LOCALIZED           = $0039; { Localized Resource Directory }
  CSIDL_COMMON_OEM_LINKS              = $003a; { Links to All Users OEM specific apps }
  CSIDL_CDBURN_AREA                   = $003b; { USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning }
  CSIDL_COMPUTERSNEARME               = $003d; { Computers Near Me (computered from Workgroup membership) }
  CSIDL_PROFILES                      = $003e;

implementation

//{$R *.DFM}

////////////////////////////////////////////////////////////////////////////////
//
//   System���Ɋւ��鏈��
//
////////////////////////////////////////////////////////////////////////////////
//function GetOSVersuinInfo(): TOsVersion;
//var
//  OsInfo: OSVERSIONINFO;
//  OSVersion: TOsVersion;
//begin
//  OsInfo.dwOSVersionInfoSize:=sizeof(OSVERSIONINFO);
//  OsVersion.MainVersion   := 0;
//  OsVersion.MinorVersion  := 0;
//  if GetVersionEx(osInfo) then
//    begin
//    OsVersion.MainVersion   := OsInfo.dwMajorVersion;
//    OsVersion.MinorVersion  := OsInfo.dwMinorVersion;
//    OsVersion.Name := OsInfo.';
//
//    case OsInfo.dwPlatformId of
//    VER_PLATFORM_WIN32_NT: //Windows NT/2000/XP
//      begin
//      if OsInfo.dwMajorVersion <=4 then
//        OsVersion.Name := 'WinNT'
//      else if (OsInfo.dwMajorVersion =5) and (OsInfo.dwMinorVersion =0) then
//        OsVersion.Name := 'Win2000'
//      else if (OsInfo.dwMajorVersion =5) and (OsInfo.dwMinorVersion =1) then
//        OsVersion.Name := 'WinXP'
//      else
//        OsVersion.Name := 'Unknown';
//      end;
//    VER_PLATFORM_WIN32_WINDOWS:  //Windows 9x/ME
//      begin
//      if (OsInfo.dwMajorVersion =4) and (OsInfo.dwMinorVersion =0) then
//        OsVersion.Name := 'Win95'
//      else if (OsInfo.dwMajorVersion =4) and (OsInfo.dwMinorVersion =10) then
//        begin
//        if OsInfo.szCSDVersion[1] = 'A' then
//          OsVersion.Name := 'Win98SE'
//        else
//          OsVersion.Name := 'Win98';
//        end
//      else if (OsInfo.dwMajorVersion =4) and (OsInfo.dwMinorVersion =90) then
//        OsVersion.Name := 'WinME'
//      else
//        OsVersion.Name :='Unknown';
//      end;
//    else
//      OsVersion.Name := 'Unknown';
//    end; //case
//    end
//  else
//    OsVersion.Name := 'Unknown';
//  result  :=  OsVersion;
//end;


//�R���|�[�l���g����������֐�
//uses��TypInfo��ǉ��K�v
//=============================================================================
//  �R���|�[�l���g����������֐�    by Mr.X-Ray
//-----------------------------------------------------------------------------
//  �y����m�F���z
//
//  Delphi 2007 R-2 Pro, Delphi 2010 Pro, Delphi XE Pro
//=============================================================================
//function ChangeComponent(Original: TComponent; NewClass: TComponentClass): TComponent;
//var
//  New:    TComponent;
//  Stream: TStream;
//  Methods:    array of TMethod;
//  aPPropInfo: array of PPropInfo;
//  MethodCount, i: Integer;
//begin
//  SetLength(aPPropInfo, 16379);
//  MethodCount := GetPropList(Original.ClassInfo, [tkMethod], @aPPropInfo[0]);
//  SetLength(Methods, MethodCount);
//  for i := 0 to MethodCount - 1 do
//    Methods[i] := GetMethodProp(Original, aPPropInfo[i]);
//
//  Stream := TMemoryStream.Create; try
//  Stream.WriteComponent(Original);
//  New := NewClass.Create(Original.Owner);
//  if New is TControl then
//    TControl(New).Parent := TControl(Original).Parent;
//  Original.Free;
//  Stream.Position := 0;
//  Stream.ReadComponent(New);
//  finally Stream.free end;
//
//  for i := 0 to MethodCount - 1 do
//    SetMethodProp(New, aPPropInfo[i], Methods[i]);
//  Result := New;
//end;


function ChangeComponent(Original: TComponent; NewClass: TComponentClass):
  TComponent;
var
  APropList   : TPropList;
  New         : TComponent;
  Stream      : TStream;
  Methods     : array of TMethod;
  MethodCount : Integer;
  i           : Integer;
begin
  MethodCount := GetPropList(Original.ClassInfo, [tkMethod], @APropList[0]);
  SetLength(Methods, MethodCount);

  for i := 0 to MethodCount - 1 do begin
    Methods[i] := GetMethodProp(Original, APropList[i]);
  end;

  Stream := TMemoryStream.Create;
  try
    Stream.WriteComponent(Original);
    New := NewClass.Create(Original.Owner);
    if New is TControl then TControl(New).Parent := TControl(Original).Parent;
    Original.Free;
    Stream.Position := 0;
    Stream.ReadComponent(New);
  finally
    Stream.free
  end;

  for i := 0 to MethodCount - 1 do begin
    SetMethodProp(New, APropList[i], Methods[i]);
  end;
  Result := New;
end;


////-----------------------------------------------------------------------------
////  EnumWindows�̃R�[���o�b�N�֐�
////  �E�B���h�E�̃v���Z�XID��������lPar�Ɠ�����������C���̃E�C���h�E�����
////-----------------------------------------------------------------------------
//function EnumWndProc(hWindow: HWND; lPar: PCardinal):
//  Boolean; Stdcall;
//var
//  dwProcessID : Cardinal;
//begin
//  Result := True;
//
//  if IsWindowVisible(hWindow) then begin
//    GetWindowThreadProcessId(hWindow, dwProcessID);
//    if dwProcessID = lPar^ then begin
//      PostMessage(hWindow, WM_CLOSE, 0, 0);
//      Result := False;
//    end;
//  end;
//end;
//
////=============================================================================
////  �w�肵��EXE�t�@�C�����N�����Ă����炻�̃v���O���������
////=============================================================================
//procedure actOtherAppClose(aApp: string);
//var
//  ExeFullPath : String;
//  ProcessID   : Cardinal;
//begin
//  //�Ώۂ̃v���O������EXE�̃t���p�X
//  ExeFullPath := ExpandFileName('../02_Toolhelp32Snapshot\Project1.exe');
//
//  //ExeFullPath�̃v���Z�XID���擾
//  //ExeFullPath�̃v���O�������N�����Ă��Ȃ��Ǝ擾�ł��Ȃ�
//  ProcessID := GetProcessIDFromPath(ExeFullPath);
//
//  if ProcessID > 0 then begin
//    //EnumWindows�̃R�[���o�b�N�֐����ŃA�v���̕����Ƃ����s
//    EnumWindows(@EnumWndProc, LPARAM(@ProcessID));
////    MessageBox(Handle, '�����I�������܂���', '���', MB_ICONINFORMATION);
//  end else begin
////    MessageBox(Handle, '���̃A�v���͋N�����Ă��܂���', '���', MB_ICONINFORMATION);
//  end;
//
//end;
//
////-----------------------------------------------------------------------------
////  �����̎��s�t�@�C�����̃v���Z�XID���擾����֐�
////
////  �֐�QueryFullProcessImageNameW�́CDelphi XE�ɂ͂Ȃ��̂Œ�`����
////  TProcessEntry32,CreateToolhelp32Snapshot�̎g�p�ɂ́Cuses��Tlhelp32���K�v
////-----------------------------------------------------------------------------
//function GetProcessIDFromPath(AExeFullPath: String): Cardinal;
//const
//  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
//  PROCESS_NAME_NATIVE = 1;
//
//var
//  ListHandle  : Cardinal;
//  ProcEntry   : TProcessEntry32;
//  ProcessID   : DWORD;
//  hProcHandle : THandle;
//  ExePath     : String;
//  Buff        : array[0..MAX_PATH-1] of Char;
//  STR_SIZE    : DWORD;
//begin
//  Result := 0;
//
//  //�f�o�b�O�̓�����L���ɂ���
//  Privilege.SetPrivilege(SE_DEBUG_NAME, True);
//
//  //�v���Z�X�̃X�i�b�v�V���b�g�̃n���h�����擾
//  ListHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
//  if ListHandle > 0 then begin
//    try
//      //�ŏ��̃v���Z�X�Ɋւ������TProcessEntry32���R�[�h�^�Ɏ擾
//      ProcEntry.dwSize := SizeOf(TProcessEntry32);
//      Process32First(ListHandle, ProcEntry);
//      repeat
//        ExePath := '';
//        FillChar(Buff, SizeOf(Buff), #0);
//
//        //�v���Z�XID���擾
//        ProcessID := ProcEntry.th32ProcessID;
//        //�v���Z�XID�l����v���Z�X�̃I�[�v���n���h�����擾
//        hProcHandle := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,
//                                   False,
//                                   ProcessID);
//        try
//          //�I�[�v���n���h������p�X�����擾
//          if hProcHandle > 0 then begin
//            //�I�[�v���n���h������p�X�����擾
//            STR_SIZE := Length(Buff);
//            if QueryFullProcessImageNameW(hProcHandle,
//                                          0,
//                                          @Buff,
//                                          @STR_SIZE) then begin
//              ExePath := String(Buff);
//              //���s�t�@�C������������������I��
//              if String(Buff) = Trim(AExeFullPath) then begin
//                Result := ProcessID;
//                break;
//              end;
//            end;
//          end;
//
//        finally
//          CloseHandle(hProcHandle);
//        end;
//      //���̃v���Z�X�Ɋւ������TProcessEntry32���R�[�h�^�Ɏ擾
//      until Process32Next(ListHandle, ProcEntry) = False;
//    finally
//      CloseHandle(ListHandle);
//    end;
//  end;
//end;
//


















// Windows�̓���t�H���_���擾
function GetVersionInfo(ExeName: string; KeyWord: TVerResourceKey): string;
const
  Translation = '\VarFileInfo\Translation';
  FileInfo    = '\StringFileInfo\%0.4s%0.4s\';
var
  BufSize, HWnd: DWORD;
  VerInfoBuf: Pointer;
  VerData: Pointer;
  VerDataLen: Longword;
  PathLocale: String;
begin
  // �K�v�ȃo�b�t�@�̃T�C�Y���擾
  BufSize := GetFileVersionInfoSize(PChar(ExeName), HWnd);
  if BufSize <> 0 then
  begin
    // ���������m��
    GetMem(VerInfoBuf, BufSize);
    try
      GetFileVersionInfo(PChar(ExeName), 0, BufSize, VerInfoBuf);
      // �ϐ����u���b�N���̕ϊ��e�[�u�����w��
      VerQueryValue(VerInfoBuf, PChar(Translation), VerData, VerDataLen);
      if not (VerDataLen > 0) then
        raise Exception.Create('���̎擾�Ɏ��s���܂���');
      PathLocale := Format(FileInfo + KeyWordStr[KeyWord],
        [IntToHex(Integer(VerData^) and $FFFF, 4),
         IntToHex((Integer(VerData^) shr 16) and $FFFF, 4)]);
      VerQueryValue(VerInfoBuf, PChar(PathLocale), VerData, VerDataLen);
      if VerDataLen > 0 then
      begin
        // VerData�̓[���ŏI��镶����ł͂Ȃ����Ƃɒ���
        result := '';
        SetLength(result, VerDataLen);
        StrLCopy(PChar(result), VerData, VerDataLen);
      end;
    finally
      // ���
      FreeMem(VerInfoBuf);
    end;
  end;
end;


function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
var
  handle: HWnd;
  Buff: PChar;
  s: string;
begin
  GetMem(Buff, 2048);
  try
    ZeroMemory(Buff, 2048);
    SHGetSpecialFolderPath(handle , Buff, Folder, CanCreate);
    result  := Buff;
  finally
    FreeMem(Buff);
  end;
end;

function protect(pw: string): string;
var
  l: integer;
  npw: string;
begin
  Randomize;
  l := Random(Length(pw)) + 1;

end;

function unprotect(pw: string): string;
begin

end;

////////////////////////////////////////////////////////////////////////////////
//
//   �ʃv���Z�X���N�����鏈��
//
////////////////////////////////////////////////////////////////////////////////
function WinExecAndWait32(FileName: string; Visibility: Integer): Longword;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  Result := 0;

  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb          := SizeOf(TStartupInfo);
  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;

  SetLength(FileName, Length(FileName));

  //�Q�ƃJ�E���^�΍�
  UniqueString(FileName);
  if not CreateProcess(nil,
                       PChar(FileName),
                       nil,
                       nil,
                       False,
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       nil,
                       nil,
                       StartupInfo,
                       ProcessInfo) then
    begin
    Result := WAIT_FAILED;
    end
  else
    begin
    //�N�������v���Z�X���I������܂ő҂�
    while WaitForSingleObject(ProcessInfo.hProcess, 100) = WAIT_TIMEOUT do
      begin
//      Application.ProcessMessages;
      end;
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

function GetWindowsDirectory: string;
var
  Buffer:array [0..MAX_PATH-1] of Char;
begin
   Windows.GetWindowsDirectory(Buffer,MAX_PATH);
   Result:=StrPas(Buffer);
end;

function TranslateFilename(SourceFileName: String): String;
begin
  Result := SourceFileName;
  Result := StringReplace(Result, '\SystemRoot', GetWindowsDirectory, []);
  Result := StringReplace(Result, '\??\', '', []);
end;

procedure GetProcExeNameList(Dest: TObjectList);
var
  hSnapshot:    THandle;
  ProcEntry:    TProcessEntry32;
  PID:          Cardinal;
  ProcStatus:   Boolean;
  hProcess:     THandle;
  ModuleFileName:   array[0..MAX_PATH] of Char;
  Item:             TProcessItem;
begin
  Dest.Clear;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);  // �v���Z�X���X�g�𓾂�
  ProcEntry.dwSize  := SizeOf(ProcEntry);
  if hSnapshot <> $FFFFFFFF then
    begin
    try
      ProcStatus := Process32First(hSnapshot, ProcEntry);
      while ProcStatus do
        begin
        PID := ProcEntry.th32ProcessID;
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
        if hProcess <> 0 then
          begin
          Item      := TProcessItem.Create;
          Item.ID   := PID;
          Item.Name := ProcEntry.szExeFile;
          try
            if GetModuleFileNameEx(hProcess, 0, ModuleFileName, Sizeof(ModuleFileName)) = 0 then
              begin
              Item.Path := '[System]';
              end
            else
              begin
              Item.Path := TranslateFilename(ModuleFileName);
              end;
            Dest.Add(Item);
          finally
            CloseHandle(hProcess);
          end;
          end;
          ProcStatus := Process32Next(hSnapshot, ProcEntry)
        end

      finally
        CloseHandle(hSnapshot);
      end;
    end
  else
    raise Exception.Create(SysErrorMessage(GetLastError));
 end;

//  Exe������ProcessId�𓾂�
function GetPidByExeName(ExeName: string): Integer;
var
  ProcessList : TObjectList;
  i : Integer;
//  Pid : Cardinal;
//  Wnd: THandle;
  Process: TProcessItem;
begin
  result  := 0;
  ProcessList :=  TObjectList.Create;
  try
    GetProcExeNameList(ProcessList);
    for i := 0 to ProcessList.Count - 1 do
    begin
      Process := TProcessItem(ProcessList.Items[i]);
      if UpperCase(Process.Name) = UpperCase(ExeName) then
      begin
        result  := Process.id;
        exit;
      end;
    end;
  finally
    FreeAndNil(ProcessList);
  end;
end;

procedure  KillProceesByExeName(ExeName: string);
var
  ProcessList : TObjectList;
  i : Integer;
//  Pid : Cardinal;
  hProc: THandle;
  Process: TProcessItem;
begin
  ProcessList :=  TObjectList.Create;
  try
    GetProcExeNameList(ProcessList);
    for i := 0 to ProcessList.Count - 1 do
    begin
      Process := TProcessItem(ProcessList.Items[i]);
      if UpperCase(Process.Name) = UpperCase(ExeName) then
      begin
        hProc := OpenProcess(PROCESS_TERMINATE, False, Process.ID);
        try
          TerminateProcess(hProc, 0);
        finally
          CloseHandle(hProc);
        end;
      end;
    end;
  finally
    FreeAndNil(ProcessList);
  end;
end;

function GetDirectryName(Owner: TComponent; Title: string; var Directry: string): boolean;
const
  SELDIRHELP = 1000;
var
  FileOpenDialog: TFileOpenDialog;
  Dir: string;
begin
  result := false;
  if TOsVersion.Major <= 5 then       // Windows Xp�ȑO
    begin
    Dir := Directry;
    if SelectDirectory(Title,'', Dir, [sdNewFolder,sdShowEdit,sdShowShares,sdNewUI]) then
      begin
      Dir := Dir;
      result := true;
      end;
    end
  else
    begin
    FileOpenDialog  :=  TFileOpenDialog.Create(Owner);
    try
      FileOpenDialog.Options  := [fdoPickFolders];
      fileOpenDialog.Title := Title;
      fileOpenDialog.DefaultFolder := ExtractFileDir(Directry);
      fileOpenDialog.FileName      := ExtractFileName(Directry);
      if fileopendialog.Execute then
        begin
        Directry := fileOpenDialog.FileName;
        result  := true;
        end;
    finally
      FreeAndNil(FileOpenDialog);
    end;
    end;
end;

function GetOpenFileName(Owner: TComponent; Title, DefaltFolder, Filter: string; var FileName: string): boolean;
const
  SELDIRHELP = 1000;
var
  FileOpenDialog: TFileOpenDialog;
  OpenDialog: TOpenDialog;
  Filters: TStringList;
  i: integer;
  s: string;
begin
  result := false;
  Filters :=  TStringList.Create;
  try
    Filters.StrictDelimiter := true;
    s := StringReplace(Filter, '|', ',', [rfReplaceAll, rfIgnoreCase]);
    Filters.CommaText       := s;
    if TOsVersion.Major < 6 then       // Windows Xp�ȑO
      begin
      OpenDialog  :=  TOpenDialog.Create(Owner);
      try
        OpenDialog.Options        := [];
        OpenDialog.Title          := Title;
        if FileName = '' then
          begin
          OpenDialog.InitialDir   := DefaltFolder;
          OpenDialog.FileName     := '';
          end
        else
          begin
          OpenDialog.InitialDir   := ExtractFileDir(FileName);
          OpenDialog.FileName     := ExtractFileName(FileName);
          end;
        if Filter <> '' then   // Default��Ext��ݒ�
//        if Filters.Count >= 1 then
          begin
          i  :=  Pos('.', Filters.Strings[0]);
          if i <> 0 then
            OpenDialog.DefaultExt := Copy(Filters.Strings[0], i+1, 256)
          else
            OpenDialog.DefaultExt := '*';
          end
        else
          OpenDialog.DefaultExt   := '*';
        OpenDialog.Filter         := Filter;
        if OpenDialog.Execute then
          begin
          FileName := OpenDialog.FileName;
          result  := true;
          end;
      finally
        FreeAndNil(OpenDialog);
      end;
      end
    else
      begin
      FileOpenDialog  :=  TFileOpenDialog.Create(Owner);
      try
        FileOpenDialog.Options        := [];
        FileOpenDialog.Title          := Title;
        if FileName = '' then
          begin
          FileOpenDialog.DefaultFolder  := DefaltFolder;
          FileOpenDialog.FileName       := '';
          end
        else
          begin
          FileOpenDialog.DefaultFolder  := ExtractFileDir(FileName);
          FileOpenDialog.FileName       := ExtractFileName(FileName);
          end;
        for i := 0 to Filters.Count - 1 do
          if odd(i) then
            with FileOpenDialog.FileTypes.Add do
              begin
              DisplayName := Filters.Strings[i-1];
              FileMask    := Filters.Strings[i];
              end;
        if fileopendialog.Execute then
          begin
          FileName := fileOpenDialog.FileName;
          result  := true;
          end;
      finally
        FreeAndNil(FileOpenDialog);
      end;
      end;
  finally
    FreeAndNil(Filters);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//   ������Ɋւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////
function DblQuotedStr(value:string): string;
begin
  result := '"' + value + '"';
end;

function CompactStr(value:string): string;
const
  Dis = $FEE0;
var
  Str   : String;
  i     : Integer;
  Code  : Cardinal;
  AChar : Char;
  SeriesSpace: boolean;
begin
  Str := '';
  SeriesSpace := true;
  for i := 1 to Length(Value) do
    begin                          // �S�p�p���L���𔼊p�ɂ���
    Code := Ord(Value[i]);
    case Code of
      $FF00..$FF5F:
        AChar := Chr(Code - Dis);
      $3000:
        AChar := ' ';
      $000A, $000D, $0009:          //  LF,CR,TAB
        AChar := ' ';
      else
        AChar := Value[i];
    end;
    if  AChar <> ' ' then           // �A�������󔒂��󔒂�1�ɂ���
      begin
      Str :=  Str + AChar;
      SeriesSpace := false;
      end
    else
      begin
      if not SeriesSpace then
        begin
        Str :=  Str + AChar;
        SeriesSpace := true;
        end;
      end;
  end;
  result := Trim(Str);
end;

function CopyByLength(str:string; len:Integer): string;
var
  i: integer;
  s_Ansi: AnsiString;
  S_Uni: string;
begin
//  Ansi�ŕK�v�����R�s�[����@�K�v��?HAMLOG�p

  s_Ansi := AnsiString(str);
  if Length(s_Ansi) <= Len then
    begin
    Result := str;
    exit;
    end;
  s_Ansi := '';
  s_Uni := Str;
  while Length(s_Ansi) <= Len  do
    begin
    Result := String(s_Ansi);
    i := (Len - Length(s_Ansi)) div 2;
    if i = 0  then
      break;
    s_Ansi := s_Ansi + AnsiString(copy(s_Uni, 1, i));
    s_Uni  := copy(s_Uni, i + 1, 256);
    end;
end;



{
function isInteger(Text:string):boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to length(Text) do
    if not CharInSet(Text[i], ['0'..'9']) then
      exit;
  Result := true;
end;
}

////////////////////////////////////////////////////////////////////////////////
//
//   ���t�Ɋւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////
function FormalizeDate(pDate:string):string;
var
  i: Integer;
  L: integer;
  y,m,d: Word;
  v: TDateTime;
begin
  result := '';
  L := Length(pDate);
  if TryStrToDate(pDate, v) then
    begin
    result := FormatDateTime('YYYY/MM/DD', v);
    exit;
    end;

  if (L > 8) then
    exit;
  if TryStrToInt(pDate, i) then
    begin
    DecodeDate(Date, Y, M, D);  //  ���ݓ�
    if (L <= 2) then
      D := StrToInt(pDate)
    else if L <= 4 then
      begin
      D := StrToInt(copy(pDate, L-1, 2));
      M := StrToInt(copy(pDate, 1, L-2));
      end
    else
      begin
      D := StrToInt(copy(pDate, L-1, 2));
      M := StrToInt(copy(pDate, L-3, 2));
      Y := StrToInt(Copy(IntToStr(CurrentYear), 1, 8-L)
        +  copy(pDate, 1, L-4));
      end;
    if TryEncodeDate(Y, M, D, v) then
      begin
      if V > Date + 10 then
        v := EncodeDate(Y-100, M, D);
      result := FormatDateTime('YYYY/MM/DD', v);
      end;
    end;
end;

function FormalizeTime(pTime:string):string;
var
  i: integer;
  L: integer;
  H,N,F,MS: word;
  v: TDateTime;
begin
  result := '';
  L := Length(pTime);
  if (L > 2) and TryStrToTime(pTime, v) then    // 2���ȉ����ƃG���[�ɂȂ�Ȃ���
    begin
    result := FormatDateTime('hh:nn', v);
    exit;
    end;

  if (L > 4) then
    exit;
  if TryStrToInt(pTime, i) then
    begin
    DecodeTime(Time, H, N, F, MS);    // ���ݎ�
    if L <= 2 then
      N := StrToInt(pTime)
    else
      begin
      N := StrToInt(copy(pTime, L - 1, 2));
      H := StrToInt(copy(pTime, 1, L - 2));
      end;
    if TryEncodeTime(H, N, 0, 0, v) then
      result := FormatDateTime('hh:nn', v);
    end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//   �O��̧�قɊւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////

// WildCard��FileCopy����  (����File�������Copy���Ȃ�)
// �������A*�����邩�ǂ����������f���Ă��Ȃ�
function CopyFiles(Source, Destination: string):boolean;
var
  i,j: Integer;
  hDesktop: HWND;
  AFileName: string;
  APathName: string;
  AFileNameList: TStringDynArray;  //���I�z��ϐ��̐錾
  BFileName: string;
  BPathName: string;
  BFileNameList: TStringDynArray;  //���I�z��ϐ��̐錾
  sgn: boolean;
begin
  hDeskTop  := FindWindow('Progman', 'Program Manager');
  AFileName := ExtractFileName(Source);
  if Pos('*', AFileName) = 0 then
    begin
    result    := SHCopyFile(hDesktop, Source, Destination);
    end
  else
    begin
    APathName     := ExtractFilePath(Source);
    AFileNameList := TDirectory.GetFiles(APathName);  // �t�@�C�����X�g�����I�z��ɓ���
    BPathName     := ExtractFilePath(Destination);
    BFileNameList := TDirectory.GetFiles(BPathName);  // �t�@�C�����X�g�����I�z��ɓ���
    for i := 0 to High(AFileNameList) do              //High�֐��͔z���max�C���f�b�N�X�l(���łȂ�)
      begin
      sgn  := true;
      AFileName :=  ExtractFileName(AFileNameList[i]);
      for j := 0 to High(BFileNameList) do
        begin
        BFileName  :=  ExtractFileName(BFileNameList[j]);
        if AFileName = BFileName then
          begin
          sgn  := false;
          Break;
          end;
        end;
      if sgn then
        result    := SHCopyFile(hDesktop, APathName + AFileName, BPathName + AFileName);
      end;
    end;
end;



//  �t�@�C���̌���,size=0�Ȃ�폜


//function RemoveFiles(Path: string):boolean;
//var
//    i: integer;
//    s: string;
////    BackupPath: string;
////    BackupGeneration: integer;
////    Newpath: string;
////    RenamePath: string;
//    DirectryList: TstringList;
////    RegStr: string;
//begin
//    Result := true;
//    DirectryList := TStringlist.Create();
//    try
//      FilesList_Get(Path, faAnyFile, true, true, DirectryList,
//                    true, true, true);
//
//      for i := 0 to DirectryList.Count - 1 do
//        begin
//        s := DirectryList[i];
//        if not DeleteFile(pchar(s)) then
//          exit;
//        end;
//    finally
//      FreeAndNil(DirectryList);
//    end;
//end;


//      PSHFileOpStruct = PSHFileOpStructA;
//      TSHFileOpStructA = packed record
//        Wnd: HWND;                  //�\�������_�C�A���O�̐e�E�B���h�E�̃n���h��
//        wFunc: UINT;                //����@�\�������t���O�i�ȉ��ɐ����j
//        pFrom: PAnsiChar;           //���̃t�@�C����
//        pTo: PAnsiChar;             //��̃t�@�C����
//        fFlags: FILEOP_FLAGS;       //�ȉ��ɐ���
//        fAnyOperationsAborted: BOOL;//���쒆�~�� true �ɂȂ�t���O
//        hNameMappings: Pointer;     //���O�t���}�b�s���O�I�u�W�F�N�g�ւ̃n���h��
//        lpszProgressTitle: PAnsiChar; { only used if FOF_SIMPLEPROGRESS }
//      end;
//      TSHFileOpStruct = TSHFileOpStructA;
//
//    wFunc ���s���鑀��
//    const
//      FO_MOVE           = $0001;  //�ړ�
//      FO_COPY           = $0002;  //�R�s�[
//      FO_DELETE         = $0003;  //�폜
//      FO_RENAME         = $0004;  //���O�ύX
//
//    fFlags �t���O
//    const
//      FOF_MULTIDESTFILES         = $0001;  //pTo �ɕ����ݒ肷��Ƃ��Ɏw��
//      FOF_CONFIRMMOUSE           = $0002;  //�ݒ�s��
//      FOF_SILENT                 = $0004;  //�v���O���X�_�C�A���O��\�����Ȃ�
//      FOF_RENAMEONCOLLISION      = $0008;  //�����t�@�C�����Ƃ̏Փ˂̂Ƃ��V�������O�ɂ���
//      FOF_NOCONFIRMATION         = $0010;  //�\�������_�C�A���O�Ɂu���ׂĂ͂��v�Ǝ����I�ɓ�����
//      FOF_WANTMAPPINGHANDLE      = $0020;  { Fill in SHFILEOPSTRUCT.hNameMappings
//                                             Must be freed using SHFreeNameMappings }
//      FOF_ALLOWUNDO              = $0040;  //�u���ɖ߂��v��L���ɂ���
//      FOF_FILESONLY              = $0080;  //���C���h�J�[�h *.* �ł̓t�@�C���݂̂ɑ��������
//      FOF_SIMPLEPROGRESS         = $0100;  //�v���O���X�_�C�A���O�Ƀt�@�C������\�����Ȃ�
//      FOF_NOCONFIRMMKDIR         = $0200;  //�f�B���N�g�������Ƃ��ł��u�m�F�v���Ȃ�
//      FOF_NOERRORUI              = $0400;
function SHCopyFile(hParent:HWND; NameFrom, NameTo: string): Boolean;
var
  SFO: TSHFileOpStruct;
begin
  NameFrom  := NameFrom + #0#0;
  NameTo    := NameTo   + #0#0;
  with SFO do
    begin
    Wnd     := hParent;
    wFunc   := FO_COPY;
    pFrom   := PChar(NameFrom);
    pTo     := PChar(NameTo);
//    fFlags  := FOF_SILENT;
//    fFlags  := FOF_RENAMEONCOLLISION;
    fAnyOperationsAborted := true;
    hNameMappings := nil;
    end;
  Result := not Boolean(SHFileOperation(SFO));
end;

function ExpandEnvironmentString(S: String): String;
var
  n: Integer;
  Dest: String;
begin
  n := ExpandEnvironmentStrings(PChar(S), nil, 0);
  SetLength(Dest, n * 2);
  ExpandEnvironmentStrings(PChar(S), PChar(Dest), n * 2);
  SetLength(Dest, StrLen(PChar(Dest)));
  result := Dest;
end;


////////////////////////////////////////////////////////////////////////////////
//
//   �p�x�Ɋւ��鋤�ʏ���
//
////////////////////////////////////////////////////////////////////////////////
procedure DecodeDeg(Value: Double; var sgn:string; var deg, min, sec: Integer);
var
  v: double;
begin
    v := Value;
    if v >= 0 then
      sgn := '+'
    else
      begin
      sgn := '-';
      v := abs(v);
      end;
    deg := Trunc(v+0.000001);
    v := frac(v+0.000001) * 60;
    min := Trunc(v);
    v := frac(v) * 60;
    sec := (Trunc(v)+29) div 30 * 30;
end;

function EncodeDeg(sgn:string; var deg, min, sec: Integer; var Value: Double): boolean;
//var
//  v: double;
begin
  try
    Value := Deg + Min/60 + Sec/3600;
    if sgn <> '+' then
      Value := - Value;
    result  := true;
  except
    result  := false;
  end;
end;

/////////////////////////////////////////////////////////////////////
//
//   �o�x�E�ܓx�̕�����𐔒l(�x)�ɕϊ�
//    ������́A"N43,06,38","E144,07,39"�@�̌`��
//    TDirectionKind = (drLatitude, drLongitude)
//
////////////////////////////////////////////////////////////////////
function StrToDeg(Value:string; Direction: TDirectionKind; var Degree: double): boolean;
var
s1,s2: string;
   Sgn: Double;
   sl: TStringList;
begin
    Result := false;
    Value := RegReplace('[.]+', Value, ',');
    sl := TStringList.Create();
    try
      s1 := Copy(Value, 1, 1);
      s2 := Copy(Value, 2, 64);
      sl.CommaText := s2 + ',0,0,0';
      if (s1 = 'N') or (s1 = 'E') or (s1 = '+') then
        sgn := 1
      else
        sgn := -1;
      Degree := Sgn * StrToFloat(sl[0]) + StrToFloat('0' + sl[1])/60
            + StrToFloat('0' + sl[2])/3600;
      if Direction = drLatitude then
        begin
        if abs(Degree) > 90 then
          exit
        end
      else
        begin
        while Degree > 180 do
          degree := Degree - 360;
        while Degree < -180 do
          degree := Degree + 360;
        end;
    Result := true;
  finally
    FreeAndNil(sl);
  end;
end;

/////////////////////////////////////////////////////////////////////
//
//   ���l(�x)���o�x�E�ܓx�̕�����ɕϊ�
//    ������́A"N43,06,38","E144,07,39"�@�̌`��
//
////////////////////////////////////////////////////////////////////
function  DegToStr(Degree: double; Directin: TDirectionKind): string;
var
  s: string;
  sgn: string;
  deg,min,sec: Integer;
begin
    DecodeDeg(Degree, sgn, deg, min, sec);
    if Directin = drLatitude then
      begin
      if sgn = '+' then
        sgn := 'N'
      else
        sgn := 'S'
      end
    else
      begin
      if sgn = '+' then
        sgn := 'E'
      else
        sgn := 'W'
      end;
    s := sgn + IntToStr(deg) + ',' + IntToStr(min) + ',' + IntToStr(sec);
    result := s;
end;

/////////////////////////////////////////////////////////////////////
//
//   �n�����2�_�Ԃ̋����E���ʂ��v�Z����
//
////////////////////////////////////////////////////////////////////
function geoDistance(lat1, log1, lat2, log2, precision: double): double;
var
  a,b,f: double;
  p1, p2: double;
  x, L, d,decimal_no: double;
begin
  if (abs(lat1 - lat2) < 0.00001) and (abs(log1 - log2) < 0.00001) then
    begin
    result := 0;
    exit;
    end;

  lat1 := DegToRad(lat1);
  log1 := DegToRad(log1);    //  �x���b���烉�a�A���ɂ���
  lat2 := DegToRad(lat2);
  log2 := DegToRad(log2);

  a := 6378140;
  b := 6356755;
  f := (a - b) / a;

  p1 := ArcTan((b / a) * Tan(lat1));
  p2 := ArcTan((B / A) * Tan(Lat2));

  x := ArcCos(sin(p1) * Sin(P2) + Cos(p1) * Cos(p2) * cos(log1 - log2));
  L := (F / 8)
    * ((sin(X) - X) * power((sin(P1) + sin(P2)), 2)
    / power(cos(X / 2), 2) - (sin(X) - X) * power(sin(P1) - sin(P2), 2)
    / power(sin(X), 2));

  D := A * (X + L);
  decimal_no := power(10, precision);
  D := Int(D * decimal_no) / decimal_no / 1000;

  result := D;
end;

// �ܓx�o�x lat1, lng1 �̓_���o���Ƃ��āA�ܓx�o�x lat2, lng2 �ւ̕���
// �k���O�x�ŉE���̊p�x�O�`�R�U�O�x
function geoDirection(lat1, log1, lat2, log2: double): double;
var
  Lon1Rad, Lat1Rad, Lon2Rad, Lat2Rad: Double;
  LonDiff: Double;
  X,Y: Double;
  Direction: Double;
begin
//  �x���烉�W�A���ɕϊ�
  Lat1Rad := DegToRad(Lat1);
  Lat2Rad := DegToRad(Lat2);
  Lon1Rad := DegToRad(Log1);
  Lon2Rad := DegToRad(Log2);

  LonDiff := Lon2Rad - Lon1Rad;
  Y := Cos(Lat2Rad)*Sin(LonDiff);
  X := Cos(Lat1Rad)*Sin(Lat2Rad) - Sin(Lat1Rad)*Cos(Lat2Rad)*Cos(LonDiff);
  Direction := ArcTan2(Y, X);

//  ���W�A������x�ɕϊ��A�����_�ȉ��؎̂�
  Direction := Int(RadToDeg(Direction));
  if Direction < 0 then                  //0�`360 �ɂ���B
    Direction := Direction + 360;

 Result:=Direction;
end;

/////////////////////////////////////////////////////////////////////
//
//   GridLocate�v�Z�֌W
//
////////////////////////////////////////////////////////////////////
function DegToGL(Lon, Lat: Double): string;
var
  s1,s2,s3,s4,s5,s6: string;
//  sLon: String;
//  sLat: string;
  i,j,k : Integer;
begin
  while Lon > 180 do
    Lon := Lon - 360;
  while Lon < -180 do
    Lon := Lon + 360;
  while Lat > 90 do
    Lat := Lat - 90;
  while Lat < -90 do
    Lat := Lat + 90;

  Lon := Lon + 180;
  if Lon >= 360 then
    Lon := Lon - 360;            // ���o180�x�𐼌o180�x�ɂ���
  i   := Trunc(Lon / 20);
  s1  := Chr(i + 65);
  j   := Trunc((Lon - i * 20) / 2);
  s3  := Chr(j + 48);
  k   := Trunc((Lon - i * 20 - j * 2) * 12);
  s5  := Chr(k + 65);

  Lat := Lat + 90;
  if Lat >= 180 then
    Lat := Lat - 0.01;           // �k��90�x��GL�v�Z�O�̂���
  i   := Trunc(Lat / 10);
  s2  := Chr(i + 65);
  j   := Trunc((Lat - i * 10));
  s4  := Chr(j + 48);
  k   := Trunc((Lat - i * 10 - j) * 24);
  s6  := Chr(k + 65);

  Result := trim(s1+s2+s3+s4+s5+s6);
end;

function GLToDeg(GL: String; var Lon,Lat: Double): boolean;
var
  s: string;
  w1,w2,w3: byte;
begin
  Lon := 0;
  Lat := 0;
  if not CheckGridLoc(GL) then
    begin
    Result := false;
    exit;
    end;

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

function CheckGridLoc(Value: string): boolean;
var
  reg: string;
begin
  reg := '^[A-R]{2}[0-9]{2}([A-X]{2})?$';
  if RegIsMatch(reg, Value) then
    result := True
  else
    result := False;
end;




/////////////////////////////////////////////////////////////////////
//
//   Contril���d�Ȃ��Ă��邩�̔��f�@
//
////////////////////////////////////////////////////////////////////
function isOverlap(Value1, Value2: TControl): boolean;
var
  reg: string;
  a1,b1,c1,d1: integer;
  a2,b2,c2,d2: integer;
  function sub(a, b, c: integer): boolean;
  begin
    if (c <= b) and (c >= b) then
      result := true
    else
      result := false;
  end;
begin
  result := false;
  a1 := Value1.Top;
  b1 := Value1.Top + Value1.Height;
  c1 := Value1.Left;
  d1 := Value1.Left + Value1.Width;

  a2 := Value2.Top;
  b2 := Value2.Top + Value2.Height;
  c2 := Value2.Left;
  d2 := Value2.Left + Value2.Width;

  if sub(a1, b1, a2) and sub(c1, d1, c2) then
    exit;
  if sub(a1, b1, b2) and sub(c1, d1, c2) then
    exit;
  if sub(a1, b1, a2) and sub(c1, d1, d2) then
    exit;
  if sub(a1, b1, b2) and sub(c1, d1, d2) then
    exit;
  result := true;
end;




end.





{*******************************************************}
{                                                       }
{               XMLIniFile Unit ver 1.02                }
{                                                       }
{              Copyright(c) 2010-2012 DEKO              }
{                                                       }
{                                                       }
{*******************************************************}

unit XMLInifile;

{$R-,T-,H+,X+}

interface

uses
  Sysutils, Classes, XMLDoc, XMLIntf, Variants {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TXMLNodeType = (xdtUnknown, xdtBinary, xdtBool, xdtCurrency, xdtDate,
                  xdtDateTime, xdtExpandString, xdtFloat, xdtInt64, xdtInteger,
                  xdtString, xdtTime);

  TXMLIniFile = class(TObject)
  private
    FCurrentNode: IXMLNode;
    FCurrentLocationPath: string;
    FRootNode: IXMLNode;
    FXML: IXMLDocument; // not TXMLDocument
                        // http://edn.embarcadero.com/jp/article/29241
  protected
    function BuildLocationPath(LocationPath: string): string;
    procedure ChangeNode(Value: IXMLNode; const Path: string);
    function GetBaseNode(Relative: Boolean): IXMLNode;
    function GetNode(const LocationPath: string): IXMLNode;
    function GetRawNode(ParentNode: IXMLNode): IXMLNode;
    function GetRootNodeName: string;
    function GetReadNode(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
    function GetWriteNode(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
  public
    constructor Create(const FileName: string);
//    constructor Create(const FileName, root: string);
    destructor Destroy; override;
    { Methods }
    procedure CloseNode;
    function CreateNode(const TagName: string; ParentNode: IXMLNode = nil): Boolean;
    function DeleteNode(const TagName: string; ParentNode: IXMLNode = nil): Boolean;
    function GetNodeType(const TagName: String; ParentNode: IXMLNode = nil): TXMLNodeType;
    procedure GetNodeNames(Strings: TStrings; ParentNode: IXMLNode = nil);
    function HasChildNodes(ParentNode: IXMLNode = nil): Boolean;
    function NodeExists(const LocationPath: string): Boolean;
    function OpenNode(const LocationPath: string; CanCreate: Boolean): Boolean;
    procedure ReadBinaryData(const TagName: string; var Buffer: TBytes; ParentNode: IXMLNode = nil);
    function ReadBool(const TagName: string; Default: Boolean; ParentNode: IXMLNode = nil): Boolean;
    function ReadCurrency(const TagName: string; Default: Currency; ParentNode: IXMLNode = nil): Currency;
    function ReadDate(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
    function ReadDateTime(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
    {$IFDEF MSWINDOWS}
    function ReadExpandString(const TagName: string; Default: string; ParentNode: IXMLNode = nil): string; platform;
    {$ENDIF}
    function ReadFloat(const TagName: string; Default: Double; ParentNode: IXMLNode = nil): Double;
    function ReadInt64(const TagName: string; Default: Int64; ParentNode: IXMLNode = nil): Int64;
    function ReadInteger(const TagName: string; Default: Integer; ParentNode: IXMLNode = nil): Integer;
    function ReadString(const TagName: string; Default: string; ParentNode: IXMLNode = nil): string;
    function ReadTime(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
    function ChildNodeByName(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
    function ChildNodes(const Index: Integer; ParentNode: IXMLNode = nil): IXMLNode;
    function ChildNodesCount(ParentNode: IXMLNode = nil): Integer;
    procedure UpdateFile;
    procedure WriteCurrency(const TagName: string; Value: Currency; ParentNode: IXMLNode = nil);
    procedure WriteBinaryData(const TagName: string; var Buffer: TBytes; ParentNode: IXMLNode = nil);
    procedure WriteBool(const TagName: string; Value: Boolean; ParentNode: IXMLNode = nil);
    procedure WriteDate(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
    procedure WriteDateTime(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
    {$IFDEF MSWINDOWS}
    procedure WriteExpandString(const TagName, Value: string; ParentNode: IXMLNode = nil); platform;
    {$ENDIF}
    procedure WriteFloat(const TagName: string; Value: Double; ParentNode: IXMLNode = nil);
    procedure WriteInt64(const TagName: string; Value: Int64; ParentNode: IXMLNode = nil);
    procedure WriteInteger(const TagName: string; Value: Integer; ParentNode: IXMLNode = nil);
    procedure WriteString(const TagName, Value: string; ParentNode: IXMLNode = nil);
    procedure WriteTime(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
    { Properties }
    property CurrentNode: IXMLNode read FCurrentNode;
    property CurrentLocationPath: string read FCurrentLocationPath;
    property RootNode: IXMLNode read FRootNode;
    property RootNodeName: string read GetRootNodeName;
    property XMLDocument: IXMLDocument read FXML;
  end;

{ functions }
function IsLocationPathDelimiter(const LocationPath: string; Index: Integer): Boolean;
function IncludeTrailingLocationPathDelimiter(const LocationPath: string): string;
function ExcludeTrailingLocationPathDelimiter(const LocationPath: string): string;
function FormatXMLFile(const FileName: string): Boolean;

const
  LocationPathDelim = '/';

implementation

const
  sType = 'Type';
  sCRLF = #$000D#$000A;

constructor TXMLIniFile.Create(const FileName: string);
//constructor TXMLIniFile.Create(const FileName, root: string);
begin
  FXML := TXMLDocument.Create(nil);
  FXML.Active   := True;
  FXML.FileName := FileName;
  FXML.NodeIndentStr := #$0009;
  FXML.Options  := [doNodeAutoIndent];
  if (FXML.FileName <> '') and  FileExists(FXML.FileName) then
    FXML.LoadFromFile(FXML.FileName)
  else
    begin
      FXML.Encoding := 'utf-8';
      FXML.Version  := '1.0';
      FXML.AddChild('root');
//      FXML.AddChild(root);
      if FXML.FileName <> '' then
        FXML.SaveToFile(FXML.FileName);
    end;
  FRootNode     := FXML.DocumentElement;
  FCurrentNode  := FCurrentNode;
  FCurrentLocationPath := LocationPathDelim;
end;

destructor TXMLIniFile.Destroy;
begin
  FXML.Active := False;
end;

function TXMLIniFile.BuildLocationPath(LocationPath: string): string;
begin
  if Copy(LocationPath, 1, 1) = LocationPathDelim then
    result := IncludeTrailingLocationPathDelimiter(LocationPath)
  else
    result := IncludeTrailingLocationPathDelimiter(IncludeTrailingLocationPathDelimiter(CurrentLocationPath) + LocationPath);
end;

procedure TXMLIniFile.ChangeNode(Value: IXMLNode; const Path: string);
begin
  CloseNode;
  FCurrentNode := Value;
  FCurrentLocationPath := Path;
end;

function TXMLIniFile.GetBaseNode(Relative: Boolean): IXMLNode;
begin
  if (CurrentNode = nil) or (not Relative) then
    Result := RootNode
  else
    Result := CurrentNode;
end;

function TXMLIniFile.GetNode(const LocationPath: string): IXMLNode;
var
  dNode: IXMLNode;
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter       := LocationPathDelim;
    SL.DelimitedText   := BuildLocationPath(LocationPath);
    dNode := FXML.DocumentElement;
    for i:=1 to SL.Count - 2 do
      begin
        dNode := dNode.ChildNodes.FindNode(SL[i]);
        if dNode = nil then
          Break;
      end;
  finally
    SL.Free;
  end;
  result := dNode;
end;

function TXMLIniFile.GetRawNode(ParentNode: IXMLNode): IXMLNode;
begin
  if ParentNode = nil then
    result := FCurrentNode
  else
    result := ParentNode;
end;

function TXMLIniFile.GetRootNodeName: string;
begin
  FRootNode.NodeName;
end;

function TXMLIniFile.GetReadNode(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
begin
  result := GetRawNode(ParentNode).ChildNodes.FindNode(TagName);
end;

function TXMLIniFile.GetWriteNode(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
var
  dNode: IXMLNode;
begin
  dNode := GetRawNode(ParentNode);
  result := dNode.ChildNodes.FindNode(TagName);
  if result = nil then
    result := dNode.AddChild(TagName);
end;

procedure TXMLIniFile.CloseNode;
begin
  ;
end;

function TXMLIniFile.CreateNode(const TagName: string; ParentNode: IXMLNode = nil): Boolean;
begin
  try
    GetRawNode(ParentNode).AddChild(TagName);
    result := True;
  except
    result := False;
  end;
end;

function TXMLIniFile.DeleteNode(const TagName: string; ParentNode: IXMLNode = nil): Boolean;
begin
  try
    GetRawNode(ParentNode).ChildNodes.Delete(TagName);
    result := True;
  except
    result := False;
  end;
end;

procedure TXMLIniFile.GetNodeNames(Strings: TStrings; ParentNode: IXMLNode = nil);
var
  i: integer;
  dNode: IXMLNode;
begin
  dNode := GetRawNode(ParentNode);
  for i:=0 to dNode.ChildNodes.Count-1 do
    Strings.Add(dNode.ChildNodes[i].NodeName);
end;

function TXMLIniFile.GetNodeType(const TagName: String; ParentNode: IXMLNode = nil): TXMLNodeType;
begin
  result := GetRawNode(ParentNode).ChildNodes.FindNode(TagName).Attributes[sType];
end;

function TXMLIniFile.HasChildNodes(ParentNode: IXMLNode = nil): Boolean;
begin
  result := GetRawNode(ParentNode).HasChildNodes;
end;

function TXMLIniFile.NodeExists(const LocationPath: string): Boolean;
begin
  result := (GetNode(LocationPath) <> nil);
end;

function TXMLIniFile.OpenNode(const LocationPath: string; CanCreate: Boolean): Boolean;
var
  oNode: IXMLNode;
  dNode: IXMLNode;
  SL: TStringList;
  i: Integer;
begin
  if CanCreate then
    begin
      SL := TStringList.Create;
      try
        result := False;
        SL.StrictDelimiter := True;
        SL.Delimiter       := LocationPathDelim;
        SL.DelimitedText   := BuildLocationPath(LocationPath);
        dNode := FXML.DocumentElement;
        oNode := dNode;
        for i:=1 to SL.Count - 2 do
          begin
            dNode := dNode.ChildNodes.FindNode(SL[i]);
            if dNode = nil then
              dNode := oNode.AddChild(SL[i]);
            oNode := dNode;
          end;
        result := True;
      finally
        SL.Free;
      end;
    end
  else
    begin
      dNode  := GetNode(LocationPath);
      result := (dNode <> nil);
    end;
  if result then
    begin
      FCurrentNode := dNode;
      FCurrentLocationPath := BuildLocationPath(LocationPath);
    end;
end;

function TXMLIniFile.ReadCurrency(const TagName: string; Default: Currency; ParentNode: IXMLNode = nil): Currency;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    result := Default
  else
    result := StrToCurr(dNode.Text);
end;

procedure TXMLIniFile.ReadBinaryData(const TagName: string; var Buffer: TBytes; ParentNode: IXMLNode = nil);
var
  dNode: IXMLNode;
  Size: Int64;
  Dmy: String;
  i: Integer;
  SL: TStringList;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    SetLength(Buffer, 0)
  else
    begin
      SL := TStringList.Create;
      try
        Dmy := dNode.Text;
        Dmy := StringReplace(Dmy, sCRLF, '', [rfReplaceAll]);
        Dmy := StringReplace(Dmy, ' ', '', [rfReplaceAll]);
        SL.StrictDelimiter := True;
        SL.Delimiter       := ',';
        SL.DelimitedText   := Dmy;
        Dmy := '';
        Size := 0;
        SetLength(Buffer, SL.Count);
        for i:=0 to SL.Count-1 do
          begin
            Dmy := Trim(SL[i]);
            if Dmy = '' then
              Continue;
            Buffer[i] := StrToInt('0x' + Dmy);
            Inc(Size);
          end;
        SetLength(Buffer, Size);
      finally
        SL.Free;
      end;
    end;
end;

function TXMLIniFile.ReadBool(const TagName: string; Default: Boolean; ParentNode: IXMLNode = nil): Boolean;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    result := Default
  else
    result := StrToBool(dNode.Text);
end;

function TXMLIniFile.ReadDate(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    result := Default
  else
    result := VarToDateTime(dNode.Text);
end;

function TXMLIniFile.ReadDateTime(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    result := Default
  else
    result := VarToDateTime(dNode.Text);
end;

{$IFDEF MSWINDOWS}
function TXMLIniFile.ReadExpandString(const TagName: string; Default: string; ParentNode: IXMLNode = nil): string;
var
  Dmy: String;
  Size: DWORD;
begin
  Dmy := ReadString(TagName, Default, ParentNode);
  Size := ExpandEnvironmentStrings(PChar(Dmy), nil, 0);
  SetLength(result, Size * 2);
  Size := ExpandEnvironmentStrings(PChar(Dmy),PChar(result), Size * 2);
  SetLength(result, Size);
end;
{$ENDIF}

function TXMLIniFile.ReadFloat(const TagName: string; Default: Double; ParentNode: IXMLNode = nil): Double;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if (dNode = nil) or (dNode.Text = '') then    // 値が空白の時もノードなしにする  14/8/31
    result := Default
  else
    result := StrToFloat(dNode.Text);
end;

function TXMLIniFile.ReadInt64(const TagName: string; Default: Int64; ParentNode: IXMLNode = nil): Int64;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if (dNode = nil) or (dNode.Text = '') then    // 値が空白の時もノードなしにする  14/8/31
    result := Default
  else
    result := StrToInt64(dNode.Text);
end;

function TXMLIniFile.ReadInteger(const TagName: string; Default: Integer; ParentNode: IXMLNode = nil): Integer;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if (dNode = nil) or (dNode.Text = '') then    // 値が空白の時もノードなしにする  14/8/31
    result := Default
  else
    result := StrToInt(dNode.Text);
end;

function TXMLIniFile.ReadString(const TagName: string; Default: string; ParentNode: IXMLNode = nil): string;
var
  Dmy: String;
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if dNode = nil then
    result := Default
  else
    begin
      Dmy := dNode.Text;
      Dmy := StringReplace(Dmy ,'&gt;'  ,'>',[rfReplaceAll,rfIgnoreCase]); // >
      Dmy := StringReplace(Dmy ,'&lt;'  ,'<',[rfReplaceAll,rfIgnoreCase]); // <
      Dmy := StringReplace(Dmy ,'&quot;','"',[rfReplaceAll,rfIgnoreCase]); // "
      Dmy := StringReplace(Dmy ,'&amp;' ,'&',[rfReplaceAll,rfIgnoreCase]); // &
      result := Dmy;
    end;
end;

function TXMLIniFile.ReadTime(const TagName: string; Default: TDateTime; ParentNode: IXMLNode = nil): TDateTime;
var
  dNode: IXMLNode;
begin
  dNode := GetReadNode(TagName, ParentNode);
  if (dNode = nil) or (dNode.Text = '') then    // 値が空白の時もノードなしにする  14/8/31
    result := Default
  else
    result := VarToDateTime(dNode.Text);
end;

function TXMLIniFile.ChildNodeByName(const TagName: string; ParentNode: IXMLNode = nil): IXMLNode;
begin
  result := GetRawNode(ParentNode).ChildNodes.Nodes[TagName];
end;

function TXMLIniFile.ChildNodes(const Index: Integer; ParentNode: IXMLNode = nil): IXMLNode;
begin
  result := GetRawNode(ParentNode).ChildNodes.Nodes[Index];
end;

function TXMLIniFile.ChildNodesCount(ParentNode: IXMLNode = nil): Integer;
begin
  result := GetRawNode(ParentNode).ChildNodes.Count;
end;

procedure TXMLIniFile.UpdateFile;
begin
  if FXML.Modified then
    if FXML.FileName <> '' then
      FXML.SaveToFile(FXML.FileName);
end;

procedure TXMLIniFile.WriteCurrency(const TagName: string; Value: Currency; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := CurrToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtCurrency;
end;

procedure TXMLIniFile.WriteBinaryData(const TagName: string; var Buffer: TBytes; ParentNode: IXMLNode = nil);
var
  Size: Int64;
  Dmy: String;
  i: Integer;
begin
  Dmy := '';
  Size := Length(Buffer);
  for i:=0 to Size - 1 do
    begin
      Dmy := Dmy + Format('%.2x', [Buffer[i]]);
      if i < Size - 1 then
        begin
          Dmy := Dmy + ',';
          if (i mod 16) = 15 then
            Dmy := Dmy + sCRLF;
        end;
    end;
  GetWriteNode(TagName, ParentNode).Text := Dmy;
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtBinary;
end;

procedure TXMLIniFile.WriteBool(const TagName: string; Value: Boolean; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := BoolToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtBool;
end;

procedure TXMLIniFile.WriteDate(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := DateToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtDate;
end;

procedure TXMLIniFile.WriteDateTime(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := DateTimeToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtDateTime;
end;

{$IFDEF MSWINDOWS}
procedure TXMLIniFile.WriteExpandString(const TagName, Value: string; ParentNode: IXMLNode = nil);
begin
  WriteString(TagName, Value, ParentNode);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtExpandString;
end;
{$ENDIF}

procedure TXMLIniFile.WriteFloat(const TagName: string; Value: Double; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := FloatToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtFloat;
end;

procedure TXMLIniFile.WriteInt64(const TagName: string; Value: Int64; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := IntToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtInt64;
end;

procedure TXMLIniFile.WriteInteger(const TagName: string; Value: Integer; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := IntToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtInteger;
end;

procedure TXMLIniFile.WriteString(const TagName, Value: string; ParentNode: IXMLNode = nil);
var
  Dmy: String;
begin
  Dmy := Value;
  Dmy := StringReplace(Dmy ,'&','&amp;' ,[rfReplaceAll,rfIgnoreCase]); // &
  Dmy := StringReplace(Dmy ,'>','&gt;'  ,[rfReplaceAll,rfIgnoreCase]); // >
  Dmy := StringReplace(Dmy ,'<','&lt;'  ,[rfReplaceAll,rfIgnoreCase]); // <
  Dmy := StringReplace(Dmy ,'"','&quot;',[rfReplaceAll,rfIgnoreCase]); // "
  GetWriteNode(TagName, ParentNode).Text := Dmy;
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtString;
end;

procedure TXMLIniFile.WriteTime(const TagName: string; Value: TDateTime; ParentNode: IXMLNode = nil);
begin
  GetWriteNode(TagName, ParentNode).Text := TimeToStr(Value);
  GetWriteNode(TagName, ParentNode).Attributes[sType] := xdtTime;
end;

{ functions }
function IsLocationPathDelimiter(const LocationPath: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(LocationPath)) and (LocationPath[Index] = LocationPathDelim);
end;

function IncludeTrailingLocationPathDelimiter(const LocationPath: string): string;
begin
  Result := LocationPath;
  if not IsLocationPathDelimiter(Result, Length(Result)) then
    Result := Result + LocationPathDelim;
end;

function ExcludeTrailingLocationPathDelimiter(const LocationPath: string): string;
begin
  Result := LocationPath;
  if IsLocationPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function FormatXMLFile(const FileName: string): Boolean;
var
  FXML: IXMLDocument;
begin
  result := False;
  if not FileExists(FileName) then
    Exit;
  FXML := TXMLDocument.Create(nil);
  try
    FXML.LoadFromFile(FileName);
    FXML.XML.Text := XMLDoc.FormatXMLData(FXML.XML.Text);
    FXML.Active := True;
    FXML.SaveToFile(FileName);
    result := True;
  finally
  end;
end;

end. 

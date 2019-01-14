(* ***************************************************************************
  SkRegExpW.pas (SkRegExp regular expression library)
  **************************************************************************** *)
(*
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is SkRegExpW.pas(for SkRegExp Library).

  The Initial Developer of the Original Code is Shuichi Komiya.

  E-mail: shu AT k DOT email DOT ne DOT jp
  URL:    http://komish.com/softlib/skregexp.htm

  Portions created by Shuichi Komiya are
  Copyright (C) 2007-2012 Shuichi Komiya. All Rights Reserved.
*)

unit SkRegExpW;

interface

{ Jananese Extenstion Define.
  Undefine JapaneseExt if you do not use Japanese.

  日本語特有の処理を行う条件定義
  以下の定義を無効にすると、全角半角の同一視、カタカナひらがなの同一視を行わない。 }
{$DEFINE JapaneseExt}

{$IFDEF JapaneseExt}
{ (?k) と (?w) を正規表現パターン内で使えるようにする条件定義。
  無効にしても、IgnoreKana, IgnoreWidth プロパティで指定することはできる。 }
  {$DEFINE UseJapaneseOption}
{$ENDIF}

{ \h is version 1.0.x compatible. }
{ \h を Perl 互換にする。有効にすると \h は 16進数値にマッチする。 }
{.$DEFINE HIsHexDigit}

{$DEFINE CHECK_MATCH_EXPLOSION}

{$IFDEF DEBUG}
  {.$DEFINE SHOW_MATCH_PROCESS }
{$ENDIF}

uses
  SysUtils,
  Classes,
  Contnrs,
{$IFDEF DEBUG}
  ComCtrls,
{$ENDIF}
{$IFNDEF UNICODE}
  WideStrings,
  WideStrUtils,
{$ENDIF}
  SkRegExpConst,
  UnicodeProp;

const
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 16.0}
  MaxListSize = Maxint div 16;
  {$IFEND}
{$ENDIF}

type
  { Exception }
  { 例外 }
  ESkRegExp = class(Exception);

  ESkRegExpRuntime = class(ESkRegExp);

  ESkRegExpCompile = class(ESkRegExp)
  public
    ErrorPos: Integer
  end;

{$IFDEF UNICODE}
  TREStrings = TStrings;
  TREStringList = TStringList;
  REString = UnicodeString;
{$ELSE}
  TREStrings = TWideStrings;
  TREStringList = TWideStringList;
  REString = WideString;
{$ENDIF}

  { String Compare Option }
  { 文字照合オプション }
  TRECompareOption = (coIgnoreCase, coIgnoreWidth, coIgnoreKana);
  TRECompareOptions = set of TRECompareOption;

  { Regular Expression Option }
  { 正規表現オプション }
  TREOption = (roNone, roIgnoreCase, roMultiLine, roNamedGroupOnly,
    roSingleLine, roExtended, roIgnoreWidth, roIgnoreKana,
    roDefinedCharClassLegacy);
  TREOptions = set of TREOption;
  PREOptions = ^TREOptions;

  { Token }
  { トークン }
  TREToken = (tkEnd, tkChar, tkUnion, tkQuest, tkDot, tkRangeChar, tkLHead,
    tkLTail, tkEmpty, tkLPar, tkRPar, tkStar, tkPlus, tkBound,
    tkCharClassFirst, tkNegativeCharClassFirst, tkCharClassEnd, tkGroupBegin,
    tkReference, tkReferenceRelative, tkNamedReference, tkLParWithOption,
    tkWordChar, tkNEWordChar, tkDigitChar, tkNEDigitChar, tkHexDigitChar,
    tkNEHexDigitChar, tkSpaceChar, tkNESpaceChar, tkTHead, tkTTail,
    tkTTailEnd,
    tkWordBoundary, tkNEWordBoundary, tkOption, tkHorizontalSpaceChar,
    tkNEHorizontalSpaceChar, tkVerticalSpaceChar, tkNEVerticalSpaceChar,
    tkLineBreak, tkPosixBracket, tkNEPosixBracket, tkProperty, tkNEProperty,
    tkNoBackTrack, tkKeepPattern, tkAheadMatch, tkAheadNoMatch, tkBehindMatch,
    tkBehindNoMatch, tkCombiningSequence, tkGoSub, tkGoSubName,
    tkGoSubRelative, tkIfMatch, tkIfMatchRef, tkGlobalPos,
    tkBranchReset, tkFail);

  TREOperator = (opEmply, opConcat, opUnion, opGroup, opLHead, opLTail,
    opQuest, opPlus, opStar, opBound, opLoop, opNoBackTrack, opKeepPattern,
    opAheadMatch, opAheadNoMatch, opBehindMatch, opBehindNoMatch, opGoSub,
    opIfMatch, opIfThen, opFail);

  TRENFAKind = (nkNormal, nkChar, nkEmpty, nkStar, nkPlus, nkBound, nkLoop,
    nkLoopExit, nkLoopEnd, nkGroupBegin, nkGroupEnd, nkKeepPattern,
    nkNoBackTrack, nkMatchEnd, nkEnd, nkGoSub, nkAheadMatch, nkAheadNoMatch,
    nkBehindMatch, nkBehindNoMatch, nkIfMatch, nkIfThen, nkFail);

  TRELoopKind = (lkGreedy, lkReluctant, lkSimpleReluctant, lkPossessive,
    lkAny, lkCombiningSequence);

  TREQuickSearch = class
  private
    FPattern: PWideChar;
    FPatternLen: Integer;
    FSkipTable: array [0 .. 255] of Integer;
    FTextTopP, FTextEndP: PWideChar;
    FTextLen: Integer;
    FCompiled: Boolean;
    FMatchP: PWideChar;
    FFindText: REString;
    FOptions: TRECompareOptions;
    procedure SetFindText(const Value: REString);
    procedure SetOptions(const Value: TRECompareOptions);
    function GetMatchPos: Integer;
  protected
    function IsMatch(AStr: PWideChar; AOptions: TRECompareOptions): Boolean;
  public
    procedure Clear;
    procedure Compile;
    function Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
    function ExecNext: Boolean;
    property FindText: REString read FFindText write SetFindText;
    property MatchP: PWideChar read FMatchP;
    property MatchPos: Integer read GetMatchPos;
    property Options: TRECompareOptions read FOptions write SetOptions;
  end;

  TSkRegExp = class;

  TRECapture = class
  private
    FStartP: PWideChar;
    FMatched: Boolean;
    FEndP: PWideChar;
    procedure SetEndP(const Value: PWideChar); inline;
  public
    procedure Clear;
    property StartP: PWideChar read FStartP write FStartP;
    property EndP: PWideChar read FEndP write SetEndP;
    property Matched: Boolean read FMatched write FMatched;
  end;

  { マッチ結果を保持するクラス }
  TGroup = class
  private
    FRegExp: TSkRegExp;
    FGroupName: REString;
    FIndexBegin, FIndexEnd: Integer;
    FCapture: TRECapture;
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetStrings: REString;
    function GetSuccess: Boolean;
  protected
    procedure Clear;
    procedure Reset;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Assign(Source: TGroup);
    property GroupName: REString read FGroupName write FGroupName;
    property IndexBegin: Integer read FIndexBegin write FIndexBegin;
    property IndexEnd: Integer read FIndexEnd write FIndexEnd;

    property Strings: REString read GetStrings;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read GetSuccess;
    property Capture: TRECapture read FCapture;
  end;

  TGroupCollectionEnumerator = class
  private
    FIndex: Integer;
    FList: TObjectList;
  public
    constructor Create(AList: TObjectList);
    function GetCurrent: TGroup;
    function MoveNext: Boolean;
    property Current: TGroup read GetCurrent;
  end;

  TREHashItem = record
    Next: Pointer;
    Key: REString;
    Value: Integer;
  end;

  PREHashItem = ^TREHashItem;
  TREHashArray = array [0 .. 16] of PREHashItem;

  TIntDynArray = array of Integer;
  PIntDynArray = ^TIntDynArray;

  { すべてのマッチ結果を保持するクラス }
  TGroupCollection = class
  private
    FRegExp: TSkRegExp;
    FItems: TObjectList;
    FBuckets: TREHashArray;
    function GetItems(Index: Integer): TGroup;
    function GetNames(AName: REString): TGroup;
    function GetCount: Integer; inline;
  protected
    function Add(const AGroupName: REString; AEntry, AWayout: Integer): Integer;
    procedure AddGroupName(const AGroupName: REString; Index: Integer);
    procedure Clear;
    function HashOf(const Key: REString): Cardinal;
    function IsDuplicateGroupName(const AGroupName: REString): Boolean;
    procedure Reset;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Assign(Source: TGroupCollection);
    function EnumIndexOfName(const AGroupName: REString): TIntDynArray;
    function GetEnumerator: TGroupCollectionEnumerator;
    function IndexOfName(const AGroupName: REString): Integer;
    function IndexOfMatchedName(const AGroupName: REString): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGroup read GetItems; default;
    property Names[AName: REString]: TGroup read GetNames;
  end;

  TRECharMapRec = record
    Ch: UCS4Char;
    Next: Pointer;
  end;

  PRECharMapRec = ^TRECharMapRec;

  TRECharMap = class
  private
    FMap: array [0 .. 255] of Pointer;
  public
    destructor Destroy; override;
    procedure Add(Ch: UCS4Char);
    procedure Clear;
    function IsExists(AStr: PWideChar): Boolean;
  end;

  { 文字列の照合を行う基底クラス }
  TRECode = class
  private
    FRegExp: TSkRegExp;
  protected
    function GetLength: Integer; virtual;
    function GetCharLength: Integer; virtual;
  public
    constructor Create(ARegExp: TSkRegExp);
    function Compare(AStr: PWideChar): Integer; virtual;
    function CompareCode(Source: TRECode): Integer; virtual;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      overload; virtual;
    function ExecRepeat(var AStr: PWideChar; AMin, AMax: Integer): Boolean;
      overload; virtual;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; virtual;
    // 文字クラスの最適化用。重複した比較をしないため。
    function IsInclude(ACode: TRECode): Boolean; virtual;
    // この Code の末尾が ACode の先頭と一致すればTrue。繰り返しの最適化用
    function IsOverlap(ACode: TRECode): Boolean; virtual;
    // 長さを持たない照合ならTrue
{$IFDEF DEBUG}
    function GetDebugStr: REString; virtual;
{$ENDIF}
    // エレメント数
    property Length: Integer read GetLength;
    // 文字数
    property CharLength: Integer read GetCharLength;
  end;

  TRECharCode = class(TRECode)
  private
    FStrings: REString;
    FSubP: PWideChar;
    FLength: Integer;
    FCharLength: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
    FConvert: Boolean;
  protected
    function GetCharLength: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; AWChar: UCS4Char;
      AOptions: TREOptions; AConvert: Boolean = False);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
    function GetWChar: UCS4Char;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELiteralCode = class(TRECode)
  private
    FStrings: REString;
    FSubP: PWideChar;
    FLength: Integer;
    FCharLength: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  protected
    function GetCharLength: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; Str: UCS4String;
      AOptions: TREOptions); overload;
    constructor Create(ARegExp: TSkRegExp; Str: REString;
      AOptions: TREOptions); overload;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRERangeCharCode = class(TRECode)
  private
    FStartWChar, FLastWChar: UCS4Char;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  public
    constructor Create(ARegExp: TSkRegExp; AStartWChar, ALastWChar: UCS4Char;
      AOptions: TREOptions);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREAnyCharCode = class(TRECode)
  private
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREWordCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREDigitCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREHexDigitCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRESpaceCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREHorizontalSpaceCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREVerticalSpaceCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineBreakCharCode = class(TRECode)
  protected
    function GetCharLength: Integer; override;
    function GetLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRECharClassCode = class(TRECode)
  private
    FMap: TRECharMap;
    FNegative: Boolean;
    FCodeList: TObjectList;
    FOptions: TREOptions;
    FIsSimple: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean;
      AOptions: TREOptions);
    destructor Destroy; override;
    function Add(AWChar: UCS4Char): Integer; overload;
    function Add(AStartWChar, ALastWChar: UCS4Char): Integer; overload;
    function Add(Value: TRECode): Integer; overload;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
    procedure Rebuild;
    procedure Sort;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRECombiningSequence = class(TRECode)
  protected
    function GetCharLength: Integer; override;
  public
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREBoundaryCode = class(TRECode)
  private
    FNegative: Boolean;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREReferenceCode = class(TRECode)
  private
    FGroupIndex: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; AGroupIndex: Integer;
      AOptions: TREOptions); overload;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRENamedReferenceCode = class(TRECode)
  private
    FGroupName: REString;
    FGroupIndexArray: TIntDynArray;
    FCount: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; AGroupName: REString;
      AGroupIndexArray: TIntDynArray; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    procedure SetGroupIndexArray(AGroupIndexArray: TIntDynArray);
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineHeadCode = class(TRECode)
  private
    FOptions: TREOptions;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineTailCode = class(TRECode)
  private
    FOptions: TREOptions;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextHeadCode = class(TRECode)
  protected
    function GetCharLength: Integer; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextTailCode = class(TRECode)
  protected
    function GetCharLength: Integer; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextEndCode = class(TRECode)
  protected
    function GetCharLength: Integer; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREPropertyCode = class(TRECode)
  private
    FUniCodeProperty: TUnicodeProperty;
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AUnicodeProperty: TUnicodeProperty;
      ANegative: Boolean);
    function Compare(AStr: PWideChar): Integer; override;
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREGlobalPosCode = class(TRECode)
  protected
    function GetCharLength: Integer; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREIfThenReferenceCode = class(TRECode)
  private
    FGroupIndex: Integer;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupIndex: Integer);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREIfThenNamedReferenceCode = class(TRECode)
  private
    FGroupName: REString;
  protected
    function GetCharLength: Integer; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupName: REString);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREBinCode = class(TRECode)
  private
    FOp: TREOperator;
    FLeft: TRECode;
    FRight: TRECode;
    FGroupIndex, FMin, FMax: Integer;
    FMatchKind: TRELoopKind;
    FGroupName: REString;
    FHasRecursion: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AOp: TREOperator;
      ALeft, ARight: TRECode; AMin: Integer = 0; AMax: Integer = 0); overload;
    property Op: TREOperator read FOp;
    property Left: TRECode read FLeft;
    property Right: TRECode read FRight;
    property Min: Integer read FMin;
    property Max: Integer read FMax;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property GroupName: REString read FGroupName write FGroupName;
    property MatchKind: TRELoopKind read FMatchKind write FMatchKind;
    property HasRecursion: Boolean read FHasRecursion write FHasRecursion;
  end;

  TRELexMode = (lmOptimize, lmNormal);
  TREContextKind = (ctNormal, ctCharClass, ctNegativeCharClass, ctQuote);

  { トークン先読み用バッファ }
  // 今のやり方はスマートじゃないのでもっといい方法があったら教えてください。
  TRELexRec = record
    FStored: Boolean;
    FToken: TREToken;
    FOptions: TREOptions;
    FNewOptions: TREOptions;
    FP: PWideChar;
    FTokenStartP: PWideChar;
    FTopP: PWideChar;
    FWChar, FStartWChar, FLastWChar: UCS4Char;
    FMin, FMax, FLevel: Integer;
    FContext: TREContextKind;
    FUniCodeProperty: TUnicodeProperty;
    FConvert: Boolean;
    FNoBackTrack: Boolean;
    FGroupName: REString;
    FOptionList: TList;
    FIsQuote: Boolean;
  end;

  { 字句解析を行うクラス }
  TRELex = class
  private
    FRegExp: TSkRegExp;
    FToken: TREToken;
    FP: PWideChar;
    FTokenStartP: PWideChar;
    FTopP, FLastP: PWideChar;
    FWChar, FStartWChar, FLastWChar: UCS4Char;
    FMin, FMax, FLevel: Integer;
    FContext: TREContextKind;
    FUniCodeProperty: TUnicodeProperty;
    FConvert: Boolean;
    FIsQuote: Boolean;
    FGroupName: REString;
    FOptions: TREOptions;
    FNewOptions: TREOptions;
    FOptionList: TList;

    FPrevLex: array [0 .. 2] of TRELexRec;
    FPrevCount: Integer;
  protected
    procedure Error(const Msg: REString;
      const Prefix: REString = '...'); overload;
    procedure Error(const Msg: REString; APosition: Integer); overload;
    function GetErrorPositionString(APosition: Integer): REString;
    procedure SkipWhiteSpace;
    procedure LexCharClass;
    procedure LexPosixCharClass;
    procedure LexOption;
    function GetControlCode(var Len: Integer): UCS4Char;
    function GetDigit(var Len: Integer): Integer;
    function GetHexDigit(var Len: Integer): UCS4Char;
    function GetOctalDigit(var Len: Integer): UCS4Char;
    procedure LexBrace;
    procedure LexProperty(const CheckNegative: Boolean);
    procedure LexGroupName(const LastDelimiter: WideChar);
    procedure LexReference(const LastDelimiter: WideChar);
    procedure LexGoSub(const LastDelimiter: WideChar);
    procedure LexEscChar;
    procedure LexLeftPar;
    procedure PushOptions;
    procedure PopOptions;
    procedure UpdateOptions;
    procedure ClearOptionList;
    function GetRECompareOptions: TRECompareOptions;
  public
    constructor Create(ARegExp: TSkRegExp; const Expression: REString);
    destructor Destroy; override;
    procedure Assign(Source: TRELex);
    function GetCompileErrorPos: Integer;
    procedure CharNext(var P: PWideChar; const Len: Integer = 1);
    procedure CharPrev(var P: PWideChar; const Len: Integer = 1);
    procedure GetToken(Skip: Boolean = False);
    procedure PushToken;
    procedure Save;

    property Token: TREToken read FToken;
    property Min: Integer read FMin;
    property Max: Integer read FMax;
    property Level: Integer read FLevel;
    property Options: TREOptions read FOptions;
    property WChar: UCS4Char read FWChar;
    property StartWChar: UCS4Char read FStartWChar;
    property LastWChar: UCS4Char read FLastWChar;
    property UnicodeProperty: TUnicodeProperty read FUniCodeProperty;
    property Convert: Boolean read FConvert;
    property TokenStartP: PWideChar read FTokenStartP;
    property GroupName: REString read FGroupName;
  end;

  // 後方参照のエラー処理用
  TReferenceErrorRec = record
    ErrorPos: Integer;
    AObject: Pointer;
    case IsRelative: Boolean of
      True:
        (RelativeGourpIndex: Integer);
      False:
        (GroupIndex: Integer);
  end;
  PReferenceErrorRec = ^TReferenceErrorRec;

  { 構文解析を行い構文木を作るクラス }
  TREParser = class
  private
    FRegExp: TSkRegExp;
    FLex: TRELex;
    FCurrentGroup: Integer;
    FGroupLevel: Integer;
    FGroupCount: Integer;
    FHasRecursion: Boolean;
    FReferenceErrorList: TREStrings;
    FGoSubErrorList: TREStrings;
    FGroupStack: TStack;
  protected
    function NewBinCode(AOperator: TREOperator; ALeft, ARight: TRECode;
      AMin: Integer = 0; AMax: Integer = 0): TRECode;
    function NewCharClassCode(ANegative: Boolean): TRECode;
    function Term: TRECode;
    function Factor: TRECode;
    function Primay: TRECode;
    function RegExpr: TRECode;
  public
    constructor Create(ARegExp: TSkRegExp; const Expression: REString);
    destructor Destroy; override;
    procedure Parse;
  end;

  { MFA の状態を保持するクラス }
  TRENFAState = class
  private
    FKind: TRENFAKind;
    FCode: TRECode;
    FTransitTo: Integer;
    FNext: TRENFAState;
    FGroupIndex: Integer;
    FMin: Integer;
    FMax: Integer;
    FMatchKind: TRELoopKind;
    FExtendTo: Integer;
    FHasRecursion: Boolean;
{$IFDEF DEBUG}
    FIndex: Integer;
{$ENDIF}
  public
{$IFDEF DEBUG}
    property Index: Integer read FIndex write FIndex;
{$ENDIF}
    property Code: TRECode read FCode write FCode;
    property TransitTo: Integer read FTransitTo write FTransitTo;
    property Next: TRENFAState read FNext write FNext;
    property Kind: TRENFAKind read FKind write FKind;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property MatchKind: TRELoopKind read FMatchKind write FMatchKind;
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    property ExtendTo: Integer read FExtendTo write FExtendTo;
    property HasRecursion: Boolean read FHasRecursion write FHasRecursion;
{$IFDEF DEBUG}
    function GetString: REString;
{$ENDIF}
  end;

  { NFA を生成するクラス }
  TRENFA = class
  private
    FRegExp: TSkRegExp;
    FStateList: TList;
    FBEntryState, FBExitState: Integer;
    FEntryStack, FExitStack: TList;
    FEntryStackIndex, FExitStateIndex: Integer;
    FStateStack: TList;
    FStateStackIndex: Integer;
    FGroupCount: Integer;
  protected
    function GetNumber: Integer;
    procedure AddTransition(AKind: TRENFAKind; ATransFrom, ATransTo: Integer;
      ACode: TRECode; AMin: Integer = 0; AMax: Integer = 0);
    procedure GenerateStateList(ACode: TRECode; AEntry, AWayout: Integer);
    procedure ReplaceCode(var OldCode, NewCode: TRECode);
    procedure PushState(AEntry, AWayout, ANewEntry, ANewWayout: Integer);
    procedure PopState;
    function GetRealState: Integer;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Compile;
  end;

  TREMatchExplosionStateRec = record
    NFACode: TRENFAState;
    Next: Pointer;
  end;
  PREMatchExplosionStateRec = ^TREMatchExplosionStateRec;

  TREBackTrackStackRec = record
    NFACode: TRENFAState;
    Str: PWideChar;
  end;
  PREBackTrackStackRec = ^TREBackTrackStackRec;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 16.0}
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  {$IFEND}
{$ENDIF}

  { バックトラック用のステートを保存するクラス }
  TREStack = class
  private
    FRegExp: TSkRegExp;
    FCount, FSize: Integer;
    FStat, FGroup: PPointerList;
    FCheckMatchExplosion: Boolean;
  protected
    procedure Extend(ASize: Integer);
  public
    constructor Create(ARegExp: TSkRegExp;
      ACheckMatchExplosion: Boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer; inline;
    function Peek: TRENFAState;
    function Index: Integer; inline;
    procedure Push(NFACode: TRENFAState; AStr: PWideChar; IsPushGroup: Boolean); overload;
    procedure Pop(var NFACode: TRENFAState; var AStr: PWideChar);
    procedure Remove(const AIndex: Integer);
  end;

  TRELeadCode = class
  private
    FCode: TRECode;
    FOffset: Integer;
    FIsBehindMatch: Boolean;
  public
    property Code: TRECode read FCode write FCode;
    property Offset: Integer read FOffset write FOffset;
    property IsBehindMatch: Boolean read FIsBehindMatch write FIsBehindMatch;
  end;

  TRELeadCodeCollection = class
  private
    FList: TObjectList;
    function GetItem(Index: Integer): TRELeadCode;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Value: TRECode; AOffset: Integer;
      ABehindMatch: Boolean): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TRELeadCode read GetItem; default;
  end;

  TRELeadCharMode = (lcmNone, lcmFirstLiteral, lcmSimple, lcmTextTop,
    lcmLineTop, lcmLastLiteral, lcmHasLead, lcmMap);

  { 従来型NFAバックトラック照合エンジンクラス
    NFAと言っても状態機械ではなく、NFA状態をバイトコードとみなして処理している。 }
  TREMatchEngine = class
  private
    FRegExp: TSkRegExp;
    FGroups: TGroupCollection;
    FStateList: TList;
    FLeadCode: TRELeadCodeCollection;
    FTailCode: TObjectList;
    FLeadStrings: TREQuickSearch;
    FIsNotSimplePattern: Boolean;
    FIsPreMatch: Boolean;
    FLeadCharMode: TRELeadCharMode;
    FMap: TRECharMap;
    FPreMatchLength: Integer;
    FIsBehindMatch: Boolean;
    FSpecialMatchStack: TList;
  protected
    procedure GenerateLeadCode;
    procedure GenerateTailCode;
    function MatchCore(var NFACode: TRENFAState; var AStr: PWideChar)
      : Boolean; overload;
    function MatchCore(var NFACode, EndCode: TRENFAState; var AStr: PWideChar;
      ACheckExplosion: Boolean = True): Boolean; overload;
    function MatchEntry(var AStr: PWideChar): Boolean;
    function MatchRecursion(var NFACode: TRENFAState; EndCode: TRENFAState;
      var AStr: PWideChar): Boolean;
    function MatchPrim(NFACode: TRENFAState; var AStr: PWideChar;
      Stack: TREStack): TRENFAState;

    procedure OptimizeLoop;

    function IsLeadStrings(var AStr: PWideChar): Boolean;

    procedure SetupLeadStrings;
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
    procedure MatchProcess(NFACode: TRENFAState; AStr: PWideChar);
{$ENDIF}
{$ENDIF}
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    function Match(AStr: PWideChar): Boolean;
    procedure Optimize;
  end;

  TRECharTypeFunc = function(W: UCS4Char): Boolean;
  TSkRegExpReplaceFunction = function(ARegExp: TSkRegExp): REString of object;
  TSkRegExpReplaceEvent = procedure(Sender: TObject; var ReplaceWith: REString) of object;

  TSkRegExp = class
  private
    FMatchEngine: TREMatchEngine;
    FCode: TRECode;
    FCodeList: TList;
    FBinCodeList: TList;
    FCompiled: Boolean;
    FTextTopP, FTextEndP: PWideChar;
    FMatchTopP, FMatchEndP: PWideChar;
    FEOL: REString;
    FEOLHeadP, FEOLTailP: PWideChar;
    FEOLLen: Integer;
    FGroups: TGroupCollection;

{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
    FMatchProcess: TREStrings;
{$ENDIF}
{$ENDIF}
    FInputString: REString;
    FExpression: REString;
    FOptions: TREOptions;

{$IFDEF CHECK_MATCH_EXPLOSION}
    FMatchExplosionState: array of PREMatchExplosionStateRec;
{$ENDIF}
    //
    FStateList: TList;
    FEntryState, FExitState: Integer;

    FOnMatch: TNotifyEvent;

    FLexMode: TRELexMode;

    FGlobalStartP, FGlobalEndP: PWideChar;
    FSuccess: Boolean;

    FReplaceFunc: TSkRegExpReplaceFunction;
    FOnReplace: TSkRegExpReplaceEvent;

    procedure SetExpression(const Value: REString);
    procedure SetEOL(const Value: REString);
    procedure SetInputString(const Value: REString);
    function GetGroupCount: Integer;
    function GetVersion: REString;
    function GetOptions(const Index: Integer): Boolean;
    procedure SetOptions(const Index: Integer; const Value: Boolean);
    function GetMatchStr(Index: Integer): REString;
    function GetMatchLen(Index: Integer): Integer;
    function GetMatchPos(Index: Integer): Integer;
    function GetNamedGroupStr(Name: REString): REString;
    function GetNamedGroupPos(Name: REString): Integer;
    function GetNamedGroupLen(Name: REString): Integer;
{$IFDEF JapaneseExt}
    procedure SetIgnoreZenHan(const Value: Boolean);
    function GetIgnoreZenHan: Boolean;
{$ENDIF}
    function GetDefineCharClassLegacy: Boolean;
    procedure SetDefineCharClassLegacy(const Value: Boolean);
    procedure SetLexMode(const Value: TRELexMode); inline;
    function GetGroupNameFromIndex(Index: Integer): REString;
    function GetIndexFromGroupName(Name: REString): Integer;
  protected
    IsWord: TRECharTypeFunc;
    IsDigit: TRECharTypeFunc;
    IsSpace: TRECharTypeFunc;

    procedure ClearCodeList;
    { FBinCodeListをクリアする。
      FBinCodeListはTREBinCodeのリスト。
      NFAを生成した後は不要なので、生成後呼び出してクリアする。 }
    procedure ClearBinCodeList;
    { 改行があった場合の位置補正用。
      AStrがFEOLと等しければTRUE。LenにはFEOLの長さが返る。
      等しくなければFALSEを返す。Lenの値はパラメータのまま。 }
    function IsEOL(AStr: PWideChar; out Len: Integer): Boolean;
    procedure ClearStateList;
{$IFDEF CHECK_MATCH_EXPLOSION}
    procedure ClearExplosionState;
{$ENDIF}
    procedure Error(const ErrorMes: REString);

    function MatchCore(AStr: PWideChar): Boolean;

    procedure DoReplaceFunc(Sender: TObject; var ReplaceWith: REString);
  public
    constructor Create;
    destructor Destroy; override;

    { 正規表現を構文解析し、NFAを生成する }
    procedure Compile;

    { 最初のマッチを実行する }
    function Exec(const AInputStr: REString): Boolean;
    { 次のマッチを実行する }
    function ExecNext: Boolean;
    { AOffsetの位置から AMaxLenght の範囲でマッチを実行する。AMaxLengthを指定した場合の動作に注意。ヘルプ参照 }
    function ExecPos(AOffset: Integer = 1; AMaxLength: Integer = 0): Boolean;

    function Substitute(const ATemplate: REString): REString;

    function Replace(const Input, Replacement: REString; Count: Integer = 0;
      AOffset: Integer = 1): REString; overload;
    function Replace(const Input: REString; AReplaceFunc: TSkRegExpReplaceFunction;
      Count: Integer = 0; AOffset: Integer = 1): REString; overload;
    procedure Split(const Input: REString; APieces: TREStrings;
      Count: Integer = 0; AOffset: Integer = 1);

    class function RegIsMatch(const ARegExpStr, AInputStr: REString;
      AOptions: TREOptions = []): Boolean;
    class function RegMatch(const ARegExpStr, AInputStr: REString; AMatches: TREStrings;
      AOptions: TREOptions = []): Boolean;
    class function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
      AOptions: TREOptions = []): REString; overload;
    class function RegReplace(const ARegExpStr, AInputStr:REString;
      AReplaceFunc: TSkRegExpReplaceFunction;
      AOptions: TREOptions = []): REString; overload;
    class procedure RegSplit(const ARegExpStr, AInputStr: REString; APieces: TREStrings;
      AOptions: TREOptions = []);

    class function DecodeEscape(const S: REString): REString;
    class function EncodeEscape(const Str: REString): REString;

{$IFDEF DEBUG}
    procedure DumpParse(TreeView: TTreeView);
    procedure DumpNFA(ADest: TStrings);
    function DumpLeadCode: REString;
    procedure DumpMatchProcess(ADest: TStrings);
{$ENDIF}
    { 正規表現の文字列 }
    property Expression: REString read FExpression write SetExpression;
    { 改行文字を設定。この設定はマッチ位置の補正用のみに使われる。
      マッチ時の改行文字の処理にはIsLineSeparatorを使う。 }
    property EOL: REString read FEOL write SetEOL;
    { グループの数を返す。グループは 0 から GroupCount まで。 }
    property GroupCount: Integer read GetGroupCount;
    { 検索対象の文字列 }
    property InputString: REString read FInputString write SetInputString;

    // 正規表現オプション
    property Options: TREOptions read FOptions write FOptions;
    property IgnoreCase: Boolean index 0 read GetOptions write SetOptions;
    property MultiLine: Boolean index 1 read GetOptions write SetOptions;
    property NamedGroupOnly: Boolean index 2 read GetOptions write SetOptions;
    property SingleLine: Boolean index 3 read GetOptions write SetOptions;
    property Extended: Boolean index 4 read GetOptions write SetOptions;
{$IFDEF JapaneseExt}
    property IgnoreWidth: Boolean index 5 read GetOptions write SetOptions;
    property IgnoreKana: Boolean index 6 read GetOptions write SetOptions;
    property IgnoreZenHan: Boolean read GetIgnoreZenHan write SetIgnoreZenHan;
{$ENDIF}
    property DefinedCharClassLegacy: Boolean read GetDefineCharClassLegacy
      write SetDefineCharClassLegacy;

    property Groups: TGroupCollection read FGroups;

    property Match[Index: Integer]: REString read GetMatchStr;
    property MatchPos[Index: Integer]: Integer read GetMatchPos;
    property MatchLen[Index: Integer]: Integer read GetMatchLen;

    property NamedGroup[Name: REString]: REString read GetNamedGroupStr;
    property NamedGroupPos[Name: REString]: Integer read GetNamedGroupPos;
    property NamedGroupLen[Name: REString]: Integer read GetNamedGroupLen;

    property GroupNameFromIndex[Index: Integer]: REString
      read GetGroupNameFromIndex;
    property IndexFromGroupName[Name: REString]: Integer
      read GetIndexFromGroupName;

    property Version: REString read GetVersion;

    property LexMode: TRELexMode read FLexMode write SetLexMode;
    // マッチが成功したら True
    property Success: Boolean read FSuccess;

    // Event
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
    property OnReplace: TSkRegExpReplaceEvent read FOnReplace write FOnReplace;
  end;

function RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions = []): Boolean; inline;
function RegMatch(const ARegExpStr, AInputStr: REString; AMatches: TREStrings;
  AOptions: TREOptions = []): Boolean; inline;
function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
  AOptions: TREOptions = []): REString; inline; overload;
function RegReplace(const ARegExpStr, AInputStr:REString;
  AReplaceFunc: TSkRegExpReplaceFunction;
  AOptions: TREOptions = []): REString; overload;
procedure RegSplit(const ARegExpStr, AInputStr: REString; APieces: TREStrings;
  AOptions: TREOptions = []); inline;

function DecodeEscape(const S: REString): REString; inline;
function EncodeEscape(const Str: REString): REString; inline;

{$IFDEF DEBUG}
function REStrLJComp(AStr, ASubStr: PWideChar; ALen: Integer;
  AOptions: TRECompareOptions): Integer;
function RECompareString(SourceP: PWideChar; DestP: PWideChar; DestLen: Integer;
  Options: TRECompareOptions): Integer;
function REStrPos(AStr: PWideChar; ALen: Integer; APattern: PWideChar;
  APatternLen: Integer; AOptions: TRECompareOptions): PWideChar;
function REStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function REStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function ToUCS4Char(AStr: PWideChar): UCS4Char; inline;
{$ENDIF}

const
  SkRegExpDefaultOptions: TREOptions = [];

implementation

uses
  Windows;

const
  CONST_VERSION = '1.3.4';
  CONST_LoopMax = MaxInt;
  CONST_LoopLimit = $7FFF;
  CONST_REStack_Size = 128;
  DefaultEOL: REString = #$000D#$000A;

  CONST_WIDE_HAT = $FF3E;
  CONST_WIDE_DOLL = $FF04;

{$IFDEF JapaneseExt}
  Dakuten = $FF9E;
  Handakuten = $FF9F;

  HanKanaToZenTable: array [$FF61 .. $FF9F, 0 .. 2] of UCS4Char =
    (($3002, $0, $0), ($300C, $0, $0), ($300D, $0, $0), ($3001, $0, $0),
    ($30FB, $0, $0), ($30F2, $0, $0), ($30A1, $0, $0), ($30A3, $0, $0),
    ($30A5, $0, $0), ($30A7, $0, $0), ($30A9, $0, $0), ($30E3, $0, $0),
    ($30E5, $0, $0), ($30E7, $0, $0), ($30C3, $0, $0), ($30FC, $0, $0),
    ($30A2, $0, $0), ($30A4, $0, $0), ($30A6, $30F4, $0), ($30A8, $0, $0),
    ($30AA, $0, $0), ($30AB, $30AC, $0), ($30AD, $30AE, $0), ($30AF, $30B0, $0),
    ($30B1, $30B2, $0), ($30B3, $30B4, $0), ($30B5, $30B6, $0),
    ($30B7, $30B8, $0), ($30B9, $30BA, $0), ($30BB, $30BC, $0),
    ($30BD, $30BE, $0), ($30BF, $30C0, $0), ($30C1, $30C2, $0),
    ($30C4, $30C5, $0), ($30C6, $30C7, $0), ($30C8, $30C9, $0), ($30CA, $0, $0),
    ($30CB, $0, $0), ($30CC, $0, $0), ($30CD, $0, $0), ($30CE, $0, $0),
    ($30CF, $30D0, $30D1), ($30D2, $30D3, $30D4), ($30D5, $30D6, $30D7),
    ($30D8, $30D9, $30DA), ($30DB, $30DC, $30DD), ($30DE, $0, $0),
    ($30DF, $0, $0), ($30E0, $0, $0), ($30E1, $0, $0), ($30E2, $0, $0),
    ($30E4, $0, $0), ($30E6, $0, $0), ($30E8, $0, $0), ($30E9, $0, $0),
    ($30EA, $0, $0), ($30EB, $0, $0), ($30EC, $0, $0), ($30ED, $0, $0),
    ($30EF, $0, $0), ($30F3, $0, $0), ($309B, $0, $0), ($309C, $0, $0));

  HanAnkToZenTable: array [$0020 .. $007E] of UCS4Char = ($3000, $FF01, $FF02,
    $FF03, $FF04, $FF05, $FF06, $FF07, $FF08, $FF09, $FF0A, $FF0B, $FF0C, $FF0D,
    $FF0E, $FF0F, $FF10, $FF11, $FF12, $FF13, $FF14, $FF15, $FF16, $FF17, $FF18,
    $FF19, $FF1A, $FF1B, $FF1C, $FF1D, $FF1E, $FF1F, $FF20, $FF21, $FF22, $FF23,
    $FF24, $FF25, $FF26, $FF27, $FF28, $FF29, $FF2A, $FF2B, $FF2C, $FF2D, $FF2E,
    $FF2F, $FF30, $FF31, $FF32, $FF33, $FF34, $FF35, $FF36, $FF37, $FF38, $FF39,
    $FF3A, $FF3B, $FFE5, $FF3D, $FF3E, $FF3F, $FF40, $FF41, $FF42, $FF43, $FF44,
    $FF45, $FF46, $FF47, $FF48, $FF49, $FF4A, $FF4B, $FF4C, $FF4D, $FF4E, $FF4F,
    $FF50, $FF51, $FF52, $FF53, $FF54, $FF55, $FF56, $FF57, $FF58, $FF59, $FF5A,
    $FF5B, $FF5C, $FF5D, $FF5E);

  ZenHiraganaToKatakanaTable: array [$3041 .. $3094] of UCS4Char = ($30A1,
    $30A2, $30A3, $30A4, $30A5, $30A6, $30A7, $30A8, $30A9, $30AA, $30AB, $30AC,
    $30AD, $30AE, $30AF, $30B0, $30B1, $30B2, $30B3, $30B4, $30B5, $30B6, $30B7,
    $30B8, $30B9, $30BA, $30BB, $30BC, $30BD, $30BE, $30BF, $30C0, $30C1, $30C2,
    $30C3, $30C4, $30C5, $30C6, $30C7, $30C8, $30C9, $30CA, $30CB, $30CC, $30CD,
    $30CE, $30CF, $30D0, $30D1, $30D2, $30D3, $30D4, $30D5, $30D6, $30D7, $30D8,
    $30D9, $30DA, $30DB, $30DC, $30DD, $30DE, $30DF, $30E0, $30E1, $30E2, $30E3,
    $30E4, $30E5, $30E6, $30E7, $30E8, $30E9, $30EA, $30EB, $30EC, $30ED, $30EE,
    $30EF, $30F0, $30F1, $30F2, $30F3, $30F4);
{$ENDIF}

function RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions): Boolean;
begin
  Result := TSkRegExp.RegIsMatch(ARegExpStr, AInputStr, AOptions);
end;

function RegMatch(const ARegExpStr, AInputStr: REString; AMatches: TREStrings;
  AOptions: TREOptions = []): Boolean;
begin
  Result := TSkRegExp.RegMatch(ARegExpStr, AInputStr, AMatches, AOptions);
end;

function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
  AOptions: TREOptions = []): REString;
begin
  Result := TSkRegExp.RegReplace(ARegExpStr, AInputStr, AReplaceStr, AOptions);
end;

function RegReplace(const ARegExpStr, AInputStr:REString;
  AReplaceFunc: TSkRegExpReplaceFunction;
  AOptions: TREOptions = []): REString;
begin
  Result := TSkRegExp.RegReplace(ARegExpStr, AInputStr, AReplaceFunc, AOptions);
end;

procedure RegSplit(const ARegExpStr, AInputStr: REString; APieces: TREStrings;
  AOptions: TREOptions = []);
begin
  TSkRegExp.RegSplit(ARegExpStr, AInputStr, APieces, AOptions);
end;

function DecodeEscape(const S: REString): REString;
begin
  Result := TSkRegExp.DecodeEscape(S);
end;

function EncodeEscape(const Str: REString): REString;
begin
  Result := TSkRegExp.EncodeEscape(Str);
end;

// ==========サポートルーチン==========

function Max(a, b: Integer): Integer; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Integer): Integer; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

{$IFNDEF UNICODE}

function IsLeadChar(C: WideChar): Boolean; inline;
begin
  Result := (C >= #$D800) and (C <= #$DFFF);
end;
{$ENDIF}

function REOptionsToRECompareOptions(AREOptions: TREOptions): TRECompareOptions;
begin
  Result := [];
  if roIgnoreCase in AREOptions then
    Include(Result, coIgnoreCase);
  if roIgnoreKana in AREOptions then
    Include(Result, coIgnoreKana);
  if roIgnoreWidth in AREOptions then
    Include(Result, coIgnoreWidth);
end;

// ==========日本語処理用ルーチン==========

{$IFDEF JapaneseExt}

function IsDaku(S: PWideChar): Boolean; inline;
begin
  Result := ((S + 1)^ = #$FF9E) and (HanKanaToZenTable[UCS4Char(S^), 1] <> 0);
end;

function IsHanDaku(S: PWideChar): Boolean; inline;
begin
  Result := ((S + 1)^ = #$FF9F) and (HanKanaToZenTable[UCS4Char(S^), 2] <> 0);
end;

function IsHalfKatakana(S: UCS4Char): Boolean; inline; overload;
begin
  Result := (S >= $FF61) and (S <= $FF9F);
end;

function IsHalfKatakana(S: WideChar): Boolean; inline; overload;
begin
  Result := (S >= #$FF61) and (S <= #$FF9F);
end;

function IsWideHiragana(S: PWideChar): Boolean; inline;
begin
  Result := (S^ >= #$3041) and (S^ <= #$3094);
end;

function IsWideKatakana(Ch: WideChar): Boolean; inline;
begin
  Result := (Ch >= #$30A1) and (Ch <= #$30FE);
end;

function IsHalfAnk(S: UCS4Char): Boolean; inline; overload;
begin
  Result := (S >= $20) and (S <= $7E);
end;

function IsHalfAnk(S: WideChar): Boolean; inline; overload;
begin
  Result := (S >= #$0020) and (S <= #$007E);
end;

function IsWideAnk(Ch: WideChar): Boolean; inline;
begin
  Result := (Ch = #$3000) or (Ch = #$FFE5) or
    ((Ch >= #$FF01) and (Ch <= #$FF5E))
end;

function ConvertString(const S: REString; Flag: DWORD): REString;
var
  L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, L * 2);
{$IFDEF UNICODE}
  L := LCMapString(GetUserDefaultLCID, Flag, PWideChar(S), Length(S),
    PWideChar(Result), Length(Result));
{$ELSE}
  L := LCMapStringW(GetUserDefaultLCID, Flag, PWideChar(S), Length(S),
    PWideChar(Result), Length(Result));
{$ENDIF}
  SetLength(Result, L);
end;

function ToWide(const S: REString): REString; inline;
begin
  Result := ConvertString(S, LCMAP_FULLWIDTH);
end;

function ToHalf(const S: REString): REString; inline;
begin
  Result := ConvertString(S, LCMAP_HALFWIDTH);
end;

function ToHiragana(const S: REString): REString; inline;
begin
  Result := ConvertString(S, LCMAP_HIRAGANA);
end;

function ToKatakana(const S: REString): REString; inline;
begin
  Result := ConvertString(S, LCMAP_KATAKANA);
end;
{$ENDIF}

// ==========文字入出力用ルーチン==========

function ToUCS4Char(AStr: PWideChar): UCS4Char; inline;
begin
  if (AStr^ >= #$D800) and (AStr^ <= #$DBFF) then
    Result := ((WORD(AStr^) and $03FF) shl 10) +
      ((WORD((AStr + 1)^) and $03FF) + $10000)
  else
    Result := UCS4Char(AStr^);
end;

function UCS4CharToString(AWChar: UCS4Char): REString; inline;
var
  H, L: Cardinal;
begin
  if AWChar >= $10000 then
  begin
    H := $D800 + (AWChar - $10000) shr 10;
    L := $DC00 + (AWChar and $03FF);
    Result := WideChar(H) + WideChar(L);
  end
  else
    Result := WideChar(AWChar);
end;

function RECharArrayToString(CharArray: UCS4String): REString;
var
  I: Integer;
begin
  for I := 0 to Length(CharArray) - 1 do
    Result := Result + UCS4CharToString(CharArray[I]);
end;

function GetREChar(AStr: PWideChar; var Len: Integer;
  Options: TRECompareOptions): UCS4Char;
var
  Buf: array [0 .. 2] of WideChar;
begin
  Len := 0;

  if coIgnoreCase in Options then
  begin
    if (AStr^ >= #$D800) and (AStr^ <= #$DBFF) then
    begin
      Buf[0] := AStr^;
      Buf[1] := (AStr + 1)^;
      Buf[2] := #0000;
      CharUpperBuffW(@Buf, 2);

      Result := ((WORD(Buf[0]) and $03FF) shl 10) +
        ((WORD(Buf[1]) and $03FF) + $10000);
      Len := 2;
    end
    else
    begin
      Buf[0] := AStr^;
      Buf[1] := #0000;
      Buf[2] := #0000;
      CharUpperBuffW(@Buf, 1);
      Result := UCS4Char(Buf[0]);
      Len := 1;
    end;
  end
  else
  begin
    if (AStr^ >= #$D800) and (AStr^ <= #$DBFF) then
    begin
      Result := ((WORD(AStr^) and $03FF) shl 10) +
        ((WORD((AStr + 1)^) and $03FF) + $10000);
      Len := 2;
    end
    else
    begin
      Result := UCS4Char(AStr^);
      Len := 1;
    end;
  end;

{$IFDEF JapaneseExt}
  if coIgnoreWidth in Options then
  begin
    if IsHalfAnk(Result) then
      Result := HanAnkToZenTable[Result]
    else if IsHalfKatakana(Result) then
    begin
      if IsDaku(AStr) then
      begin
        Result := HanKanaToZenTable[Result, 1];
        Len := 2;
      end
      else if IsHanDaku(AStr) then
      begin
        Result := HanKanaToZenTable[Result, 2];
        Len := 2;
      end
      else
        Result := HanKanaToZenTable[Result, 0];
    end;
  end;

  if coIgnoreKana in Options then
  begin
    if ((Result >= $3041) and (Result <= $3094)) then
      Result := ZenHiraganaToKatakanaTable[Result]
    else if (Result = $309D) then
      Result := $30FD
    else if Result = $309E then
      Result := $30FE
  end;
{$ENDIF}
end;

function ConvertREString(const S: REString; AOptions: TRECompareOptions)
  : REString;
var
  P: PWideChar;
  I, Len: Integer;
begin
  P := PWideChar(S);
  Len := Length(S);

  if coIgnoreCase in AOptions then
    CharUpperBuffW(P, Len);

  SetLength(Result, Len);

  if coIgnoreWidth in AOptions then
  begin
    I := 1;
    while I <= Len do
    begin
      if coIgnoreWidth in AOptions then
      begin
        if IsHalfAnk(P^) then
          Result[I] := WideChar(HanAnkToZenTable[UCS4Char(P^)])
        else if IsHalfKatakana(P^) then
        begin
          if IsDaku(P) then
          begin
            Result[I] := WideChar(HanKanaToZenTable[UCS4Char(P^), 1]);
            Inc(P);
          end
          else if IsHanDaku(P) then
          begin
            Result[I] := WideChar(HanKanaToZenTable[UCS4Char(P^), 2]);
            Inc(P);
          end
          else
            Result[I] := WideChar(HanKanaToZenTable[UCS4Char(P^), 0]);
        end
        else
          Result[I] := P^;

        if coIgnoreKana in AOptions then
        begin
          if (P^ >= #$3041) and (P^ <= #$3094) then
            Result[I] := WideChar(ZenHiraganaToKatakanaTable[UCS4Char(P^)])
          else if (P^ = #$309D) then
            Result[I] := #$30FD
          else if P^ = #$309E then
            Result[I] := #$30FE
          else
            Result[I] := P^;
        end;
      end;

      Inc(P);
      Inc(I);
    end;
  end
  else
    Move(P^, Result[1], Len * Sizeof(WideChar));
end;

// ==========文字種判定用ルーチン==========

function IsLineSeparator(P: WideChar): Boolean; inline;
begin
  Result := (P = #$000A) or (P = #$000B) or (P = #$000C) or (P = #$000D) or
    (P = #$0085) or (P = #$2020) or (P = #$2029);
end;

function IsAnkWord(P: PWideChar): Boolean; inline; overload;
begin
  Result := (P^ = '_') or ((P^ >= 'A') and (P^ <= 'Z')) or
    ((P^ >= 'a') and (P^ <= 'z')) or ((P^ >= '0') and (P^ <= '9'));
end;

function IsAnkWord(W: UCS4Char): Boolean; inline; overload;
begin
  Result := (W = UCS4Char('_')) or ((W >= UCS4Char('A')) and (W <= UCS4Char('Z')
    )) or ((W >= UCS4Char('a')) and (W <= UCS4Char('z'))) or
    ((W >= UCS4Char('0')) and (W <= UCS4Char('9')));
end;

function IsAnkDigit(P: PWideChar): Boolean; inline; overload;
begin
  Result := (P^ >= '0') and (P^ <= '9');
end;

function IsAnkDigit(W: UCS4Char): Boolean; inline; overload;
begin
  Result := (W >= UCS4Char('0')) and (W <= UCS4Char('9'));
end;

function IsAnkSpace(P: PWideChar): Boolean; inline; overload;
begin
  Result := (P^ = #$0009) or (P^ = #$000A) or (P^ = #$000B) or (P^ = #$000C) or
    (P^ = #$000D) or (P^ = #$00020);
end;

function IsAnkSpace(W: UCS4Char): Boolean; inline; overload;
begin
  Result := (W = $9) or (W = $A) or (W = $B) or (W = $C) or (W = $D) or
    (W = $20);
end;

function IsHexDigit(Ch: UCS4Char): Boolean; inline; overload;
begin
  Result := ((Ch >= $30) and (Ch <= $39)) or ((Ch >= $41) and (Ch <= $46)) or
    ((Ch >= $61) and (Ch <= $66));
end;

function IsHorizontalWhiteSpace(P: PWideChar): Boolean; inline; overload;
begin
  case P^ of
    #$0009, #$0020, #$00A0, #$1680, #$180E, #$2000 .. #$200A, #$202F,
      #$205F, #$3000:
      Result := True
  else
    Result := False;
  end;
end;

function IsHorizontalWhiteSpace(W: UCS4Char): Boolean; inline; overload;
begin
  case W of
    0009, $0020, $00A0, $1680, $180E, $2000 .. $200A, $202F, $205F, $3000:
      Result := True
  else
    Result := False;
  end;
end;

function IsVerticalWhiteSpace(P: PWideChar): Boolean; inline; overload;
begin
  case P^ of
    #$000A, #$000B, #$000C, #$000D, #$0085, #$2028, #$2029:
      Result := True
  else
    Result := False;
  end;
end;

function IsVerticalWhiteSpace(W: UCS4Char): Boolean; inline; overload;
begin
  case W of
    $000A, $000B, $000C, $000D, $0085, $2028, $2029:
      Result := True
  else
    Result := False;
  end;
end;

// ==========文字列検索用ルーチン==========

{$IFDEF UNICODE}
function REStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; inline;
begin
  Result := StrLComp(Str1, Str2, MaxLen);
end;
{$ELSE UNICODE}
function REStrLComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
var
  I: Cardinal;
begin
  Result := 0;
  I := 1;

  while I <= MaxLen do
  begin
    if Str1^ <> Str2^ then
    begin
      Result := Ord(Str1^) - Ord(Str2^);
      Exit;
    end;

    Inc(Str1);
    Inc(Str2);
    Inc(I);
  end;
end;
{$ENDIF UNICODE}

function REStrLJComp(AStr, ASubStr: PWideChar; ALen: Integer;
  AOptions: TRECompareOptions): Integer;
var
  W1, W2: Integer;
  L1, L2: Integer;
  I: Integer;
begin
  Result := 0;

  I := 1;
  while I <= ALen do
  begin
    W1 := GetREChar(AStr, L1, AOptions);
    W2 := GetREChar(ASubStr, L2, AOptions);
    if W1 <> W2 then
    begin
      Result := W1 - W2;
      Exit;
    end;

    Inc(AStr, L1);
    Inc(ASubStr, L2);
    Inc(I, L1);
  end;
end;

function REStrLIComp(Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; inline;
begin
{$IFDEF UNICODE}
  Result := AnsiStrLIComp(Str1, Str2, MaxLen);
{$ELSE}
  Result := REStrLJComp(Str1, Str2, MaxLen, [coIgnoreCase])
{$ENDIF}
end;

{
  AStr: 検索対象の文字列
  ASubStr: 検索文字列
  ALen: 検索文字列の文字数
  AOptions: 検索オプション

  大文字小文字を区別しない
  全角半角を区別しない
  ひらがなカタカナを区別しない

}
function RECompareString(SourceP: PWideChar; DestP: PWideChar; DestLen: Integer;
  Options: TRECompareOptions): Integer;
begin
  if (coIgnoreWidth in Options) or (coIgnoreKana in Options) then
    Result := REStrLJComp(SourceP, DestP, DestLen, Options)
  else if coIgnoreCase in Options then
    Result := REStrLIComp(SourceP, DestP, DestLen)
  else if DestLen = 1 then
    Result := Ord(SourceP^) - Ord(DestP^)
  else
    Result := REStrLComp(SourceP, DestP, DestLen);
end;

function REStrPos(AStr: PWideChar; ALen: Integer; APattern: PWideChar;
  APatternLen: Integer; AOptions: TRECompareOptions): PWideChar;
var
  TextEndP: PWideChar;
begin
  Result := nil;

  TextEndP := AStr + ALen;

  if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) then
  begin
    while AStr <= TextEndP do
    begin
      if REStrLJComp(AStr, APattern, APatternLen, AOptions) = 0 then
      begin
        Result := AStr;
        Exit;
      end;
      Inc(AStr);
    end;
  end
  else if coIgnoreCase in AOptions then
  begin
    while AStr <= TextEndP do
    begin
      if REStrLIComp(AStr, APattern, APatternLen) = 0 then
      begin
        Result := AStr;
        Exit;
      end;
      Inc(AStr);
    end;
  end
  else
  begin
    while AStr <= TextEndP do
    begin
      if REStrLComp(AStr, APattern, APatternLen) = 0 then
      begin
        Result := AStr;
        Exit;
      end;
      Inc(AStr);
    end;
  end;
end;

// ==========文字位置移動用ルーチン==========

procedure CharNext(var P: PWideChar; Len: Integer = 1); inline;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if IsLeadChar(P^) then
      Inc(P);
    Inc(P);
  end;
end;

procedure CharNextForCombiningSequence(var P: PWideChar; Len: Integer = 1);
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if not IsUnicodeProperty(ToUCS4Char(P), upM) then
    begin
      Inc(P);

      while IsUnicodeProperty(ToUCS4Char(P), upM) do
        Inc(P);
    end;
  end;
end;

procedure CharPrev(var P: PWideChar; Len: Integer = 1); inline;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    Dec(P);
    if IsLeadChar(P^) then
      Dec(P);
  end;
end;

{ TREQuickSearch }

procedure TREQuickSearch.Clear;
begin
  FFindText := '';
  FPattern := nil;
  FPatternLen := 0;
  FTextTopP := nil;
  FTextEndP := nil;
  FTextLen := 0;
  FOptions := [];
  FMatchP := nil;
  FCompiled := False;
end;

procedure TREQuickSearch.Compile;
var
  I, Low: Integer;
begin
  if FPatternLen > 2 then
  begin
    if FOptions <> [] then
      FFindText := ConvertREString(FFindText, FOptions);

    for I := 0 to 255 do
      FSkipTable[I] := FPatternLen;

    for I := 0 to FPatternLen - 1 do
    begin
      Low := Integer(FPattern[I]) and $FF;
      FSkipTable[ Low] := FPatternLen - I;
    end;
  end;
  FCompiled := True;
end;

function TREQuickSearch.Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
begin
  if not FCompiled then
    Compile;

  FTextTopP := AStr;
  FTextLen := ATextLen;
  FTextEndP := AStr + ATextLen;

  Result := IsMatch(AStr, FOptions);
end;

function TREQuickSearch.ExecNext: Boolean;
var
  AStr: PWideChar;
begin
  AStr := FMatchP + FPatternLen;

  Result := IsMatch(AStr, FOptions);
end;

function TREQuickSearch.GetMatchPos: Integer;
begin
  if FMatchP <> nil then
    Result := FMatchP - FTextTopP + 1
  else
    Result := 0;
end;

function TREQuickSearch.IsMatch(AStr: PWideChar;
  AOptions: TRECompareOptions): Boolean;
var
  Index, L, Low: Integer;
  WChar: UCS4Char;
  P: PWideChar;
begin
  Result := False;
  FMatchP := nil;

  if FTextLen < FPatternLen then
    Exit;

  { パターンが1-2文字ならQuick Seachの意味がない }
  if FPatternLen = 1 then
  begin
    if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) or
      (coIgnoreCase in AOptions) then
    begin
      WChar := GetREChar(FPattern, L, AOptions);
      while AStr <= FTextEndP do
      begin
        if WChar = GetREChar(AStr, L, AOptions) then
        begin
          FMatchP := AStr;
          Result := True;
          Exit;
        end;
        Inc(AStr, L);
      end;
    end
    else
    begin
      P := PWideChar(FPattern);
      while AStr <= FTextEndP do
      begin
        if AStr^ = P^ then
        begin
          FMatchP := AStr;
          Result := True;
          Exit;
        end;
        Inc(AStr);
      end;
    end;
  end
  else if FPatternLen = 2 then
  begin
    L := FTextEndP - AStr;
    P := REStrPos(AStr, L, PWideChar(FPattern), FPatternLen, AOptions);
    Result := P <> nil;
    if Result then
    begin
      FMatchP := P;
      Exit;
    end;
  end
  else
  begin
    Index := 0;

    if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) then
    begin
      while Index <= FTextLen - FPatternLen do
      begin
        if REStrLJComp(AStr + Index, FPattern, FPatternLen, AOptions) = 0 then
        begin
          FMatchP := AStr + Index;
          Result := True;
          Exit;
        end;

        Low := Integer(GetREChar(AStr + Index + FPatternLen, L,
          FOptions)) and $FF;
        Inc(Index, FSkipTable[ Low]);
      end;
    end
    else if coIgnoreCase in AOptions then
    begin
      while Index <= FTextLen - FPatternLen do
      begin
        if REStrLIComp(AStr + Index, FPattern, FPatternLen) = 0 then
        begin
          FMatchP := AStr + Index;
          Result := True;
          Exit;
        end;

        Low := Integer(GetREChar(AStr + Index + FPatternLen, L,
          FOptions)) and $FF;
        Inc(Index, FSkipTable[ Low]);
      end;
    end
    else
    begin
      while Index <= FTextLen - FPatternLen do
      begin
        if REStrLComp(AStr + Index, FPattern, FPatternLen) = 0 then
        begin
          FMatchP := AStr + Index;
          Result := True;
          Exit;
        end;

        Low := Integer(AStr[Index + FPatternLen]) and $FF;
        Inc(Index, FSkipTable[ Low]);
      end;
    end;
  end;
end;

procedure TREQuickSearch.SetFindText(const Value: REString);
begin
  if FFindText <> Value then
  begin
    FFindText := Value;
    FPattern := PWideChar(FFindText);
    FPatternLen := Length(FFindText);
    FCompiled := False;
  end;
end;

procedure TREQuickSearch.SetOptions(const Value: TRECompareOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    FCompiled := False;
  end;
end;

{ TRECapture }

procedure TRECapture.Clear;
begin
  FStartP := nil;
  FEndP := nil;
  FMatched := False;
end;

procedure TRECapture.SetEndP(const Value: PWideChar);
begin
  if (FStartP <> nil) and (not FMatched) then
    FMatched := True;
  FEndP := Value;
end;

{ TREGroup }

procedure TGroup.Assign(Source: TGroup);
begin
  FRegExp := Source.FRegExp;
  FGroupName := Source.FGroupName;
  FIndexBegin := Source.FIndexBegin;
  FIndexEnd := Source.FIndexEnd;
  FCapture.StartP := Source.FCapture.StartP;
  FCapture.EndP := Source.FCapture.EndP;
  FCapture.Matched := Source.FCapture.Matched;
end;

procedure TGroup.Clear;
begin
  FGroupName := '';
  FIndexBegin := 0;
  FIndexEnd := 0;

  FCapture.StartP := nil;
  FCapture.EndP := nil;
  FCapture.Matched := False;
end;

constructor TGroup.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FCapture := TRECapture.Create;
  FGroupName := '';
  FIndexBegin := 0;
  FIndexEnd := 0;
end;

destructor TGroup.Destroy;
begin
  FCapture.Free;
  inherited;
end;

function TGroup.GetIndex: Integer;
begin
  if FCapture.Matched then
    Result := FCapture.StartP - FRegExp.FTextTopP + 1
  else
    Result := 0;
end;

function TGroup.GetLength: Integer;
begin
  if FCapture.Matched then
    Result := FCapture.EndP - FCapture.StartP
  else
    Result := 0;
end;

function TGroup.GetStrings: REString;
begin
  if FCapture.Matched then
    SetString(Result, FCapture.StartP, FCapture.EndP - FCapture.StartP)
  else
    Result := '';
end;

function TGroup.GetSuccess: Boolean;
begin
  Result := FCapture.Matched;
end;

procedure TGroup.Reset;
begin
  FCapture.Clear;
end;

{ TREGroupCollectionEnumerator }

constructor TGroupCollectionEnumerator.Create(AList: TObjectList);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGroupCollectionEnumerator.GetCurrent: TGroup;
begin
  Result := FList[FIndex] as TGroup;
end;

function TGroupCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TREGroupCollection }

function TGroupCollection.Add(const AGroupName: REString;
  AEntry, AWayout: Integer): Integer;
var
  Item: TGroup;
begin
  Item := TGroup.Create(FRegExp);
  Item.GroupName := AGroupName;
  Item.IndexBegin := AEntry;
  Item.IndexEnd := AWayout;
  Result := FItems.Add(Item);

  if AGroupName <> '' then
    AddGroupName(AGroupName, Result);
end;

procedure TGroupCollection.AddGroupName(const AGroupName: REString;
  Index: Integer);
var
  H: Cardinal;
  S, D: PREHashItem;
begin
  New(D);
  D.Key := AGroupName;
  D.Value := Index;

  H := HashOf(AGroupName);

  S := FBuckets[H];
  if S <> nil then
  begin
    D.Next := S;
    FBuckets[H] := D;
  end
  else
  begin
    D.Next := nil;
    FBuckets[H] := D;
  end;
end;

procedure TGroupCollection.Assign(Source: TGroupCollection);
var
  I: Integer;
  Item: TGroup;
begin
  Clear;

  FRegExp := Source.FRegExp;
  for I := 0 to Source.FItems.Count - 1 do
  begin
    Item := TGroup.Create(FRegExp);
    Item.Assign(Source.FItems[I] as TGroup);
    if I <> 0 then
    begin
      if Item.GroupName <> '' then
        AddGroupName(Item.GroupName, I);
      FItems.Add(Item);
    end
    else
      FItems[0] := Item;
  end;
end;

procedure TGroupCollection.Clear;
begin
  FItems.Clear;
  FItems.Add(TGroup.Create(FRegExp));
end;

constructor TGroupCollection.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FItems := TObjectList.Create;
  FItems.Add(TGroup.Create(FRegExp));
end;

destructor TGroupCollection.Destroy;
var
  I: Integer;
  S, D: PREHashItem;
begin
  for I := 0 to System.Length(FBuckets) - 1 do
  begin
    S := FBuckets[I];
    while S <> nil do
    begin
      D := S.Next;
      Dispose(S);
      S := D;
    end;
  end;
  FItems.Free;
  inherited;
end;

function TGroupCollection.EnumIndexOfName(const AGroupName: REString)
  : TIntDynArray;
var
  H: Cardinal;
  S: PREHashItem;
  LCount: Integer;
begin
  LCount := 0;
  SetLength(Result, Count);

  H := HashOf(AGroupName);
  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      Result[LCount] := S.Value;
      Inc(LCount);
    end;
    S := S.Next;
  end;
  SetLength(Result, LCount);
end;

function TGroupCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGroupCollection.GetEnumerator: TGroupCollectionEnumerator;
begin
  Result := TGroupCollectionEnumerator.Create(FItems);
end;

function TGroupCollection.GetItems(Index: Integer): TGroup;
begin
  if (Index < 0) or (Index > FItems.Count - 1) then
    raise ESkRegExpRuntime.CreateFmt(sRangeOverGroupNumber, [Index]);

  Result := FItems[Index] as TGroup;
end;

function TGroupCollection.GetNames(AName: REString): TGroup;
var
  Index: Integer;
begin
  Index := IndexOfMatchedName(AName);
  if Index = -1 then
    raise ESkRegExpRuntime.CreateFmt(sMissingGroupName, [AName]);

  Result := FItems[Index] as TGroup
end;

function TGroupCollection.HashOf(const Key: REString): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to System.Length(Key) do
    Result := ((Result shl 2) or (Result shr (Sizeof(Result) * 8 - 2)))
      xor Ord(Key[I]);
  Result := Result mod Cardinal(System.Length(FBuckets));
end;

function TGroupCollection.IndexOfName(const AGroupName: REString): Integer;
var
  H: Cardinal;
  S: PREHashItem;
begin
  Result := -1;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      Result := S.Value;
      Exit;
    end;
    S := S.Next;
  end;
end;

function TGroupCollection.IndexOfMatchedName(const AGroupName
  : REString): Integer;
var
  H: Cardinal;
  S: PREHashItem;
begin
  Result := -1;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if (S.Key = AGroupName) and GetItems(S.Value).Capture.Matched then
    begin
      Result := S.Value;
      Exit;
    end;
    S := S.Next;
  end;
end;

function TGroupCollection.IsDuplicateGroupName(const AGroupName
  : REString): Boolean;
var
  H: Cardinal;
  S: PREHashItem;
  M: Integer;
begin
  Result := False;
  M := 0;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      Inc(M);
      if M > 1 then
      begin
        Result := True;
        Exit;
      end;
    end;
    S := S.Next;
  end;
end;

procedure TGroupCollection.Reset;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TGroup(FItems[I]).Reset;
end;

{ TRECharMap }

procedure TRECharMap.Add(Ch: UCS4Char);
var
  P, Prev: PRECharMapRec;
begin
  P := FMap[Ord(Ch) and $FF];
  if P = nil then
  begin
    New(P);
    P^.Ch := Ch;
    P^.Next := nil;
    FMap[Ord(Ch) and $FF] := P;
  end
  else
  begin
    repeat
      if P^.Ch = Ch then
        Exit;
      Prev := P;
      P := P.Next;
    until P = nil;

    New(P);
    P^.Ch := Ch;
    P^.Next := nil;
    Prev.Next := P;
  end;
end;

procedure TRECharMap.Clear;
var
  I: Integer;
  P, Next: PRECharMapRec;
begin
  for I := 0 to 255 do
  begin
    if FMap[I] <> nil then
    begin
      P := FMap[I];
      Next := P.Next;
      Dispose(P);
      while Next <> nil do
      begin
        P := Next;
        Next := P.Next;
        Dispose(P);
      end;
    end;
    FMap[I] := nil;
  end;
end;

destructor TRECharMap.Destroy;
begin
  Clear;
  inherited;
end;

function TRECharMap.IsExists(AStr: PWideChar): Boolean;
var
  Ch: Cardinal;
  P: PRECharMapRec;
begin
  Result := False;
  Ch := ToUCS4Char(AStr);

  P := FMap[Ord(Ch) and $FF];
  if P = nil then
    Exit;

  if P.Ch = Ch then
  begin
    Result := True;
    Exit;
  end;

  while P.Next <> nil do
  begin
    if P.Ch = Ch then
    begin
      Result := True;
      Exit;
    end;
    P := P.Next;
  end;
end;

{ TRECode }

function TRECode.Compare(AStr: PWideChar): Integer;
begin
  Result := 0;
end;

function TRECode.CompareCode(Source: TRECode): Integer;
begin
  Result := 0;
end;

constructor TRECode.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
end;

function TRECode.ExecRepeat(var AStr: PWideChar; AMin, AMax: Integer): Boolean;
var
  I: Integer;
  Len: Integer;
begin
  Result := False;

  for I := 1 to AMin do
  begin
    if (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) then
      Inc(AStr, Len)
    else
      Exit;
  end;

  Result := True;
  if AMin = AMax then
    Exit;

  AMax := AMax - AMin;

  for I := 1 to AMax do
  begin
    if (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) then
      Inc(AStr, Len)
    else
      Break;
  end;
end;

function TRECode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
end;

function TRECode.ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len: Integer;
begin
  Result := IsStar;
  StartP := AStr;

  while (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) do
    Inc(AStr, Len);

  if not Result then
    Result := AStr - StartP > 0;
end;

function TRECode.GetCharLength: Integer;
begin
  Result := 1;
end;

{$IFDEF DEBUG}

function TRECode.GetDebugStr: REString;
begin

end;
{$ENDIF}

function TRECode.GetLength: Integer;
begin
  Result := 1;
end;

function TRECode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;
end;

function TRECode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := True;
end;

{ TRECharCode }

function TRECharCode.Compare(AStr: PWideChar): Integer;
begin
  Result := RECompareString(AStr, FSubP, FLength, FCompareOptions);
end;

function TRECharCode.CompareCode(Dest: TRECode): Integer;
var
  Ch1, Ch2, Start, Last: UCS4Char;
begin
  Ch1 := GetWChar;

  if Dest is TRECharCode then
  begin
    Ch2 := (Dest as TRECharCode).GetWChar;
    if Ch1 > Ch2 then
      Result := 1
    else if Ch1 < Ch2 then
      Result := -1
    else
      Result := 0;
  end
  else if Dest is TRERangeCharCode then
  begin
    Start := (Dest as TRERangeCharCode).FStartWChar;
    Last := (Dest as TRERangeCharCode).FLastWChar;

    if Ch1 < Start then
      Result := -1
    else if Ch1 > Last then
      Result := 1
    else
      Result := 0;
  end
  else if (Dest is TREWordCharCode) then
  begin
    if FRegExp.IsWord(Ch1) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREDigitCharCode) then
  begin
    if FRegExp.IsDigit(Ch1) then
      Result := 0
    else
      Result := 1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    if FRegExp.IsSpace(Ch1) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    if IsUnicodeProperty(Ch1, (Dest as TREPropertyCode).FUniCodeProperty) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    if IsHexDigit(Ch1) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    if IsVerticalWhiteSpace(Ch1) then
      Result := 0
    else
      Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    if IsHorizontalWhiteSpace(Ch1) then
      Result := 0
    else
      Result := -1;
  end
  else
    Result := -1;
end;

constructor TRECharCode.Create(ARegExp: TSkRegExp; AWChar: UCS4Char;
  AOptions: TREOptions; AConvert: Boolean);
var
  I: Integer;
begin
  inherited Create(ARegExp);
  FStrings := UCS4CharToString(AWChar);
  FLength := System.Length(FStrings);
  FSubP := PWideChar(FStrings);
  FConvert := AConvert;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);

  FCharLength := 0;
  for I := 1 to System.Length(FStrings) do
  begin
    if IsLeadChar(FStrings[I]) then
      Dec(FCharLength);
    Inc(FCharLength);
  end;
end;

function TRECharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := RECompareString(AStr, FSubP, FLength, FCompareOptions) = 0;

  if Result then
    Len := FLength
  else
    Len := 0;
end;

function TRECharCode.ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len: Integer;
begin
  Result := IsStar;
  StartP := AStr;

  if (roIgnoreKana in FOptions) or (roIgnoreWidth in FOptions) then
  begin
    while REStrLJComp(AStr, PWideChar(FStrings), FLength,
      FCompareOptions) = 0 do
      Inc(AStr, FLength);
  end
  else if roIgnoreCase in FOptions then
  begin
    while REStrLIComp(AStr, PWideChar(FStrings), FLength) = 0 do
      Inc(AStr, FLength);
  end
  else
  begin
    if FLength = 1 then
    begin
      while AStr^ = FSubP^ do
        Inc(AStr);
    end
    else
    begin
      while REStrLComp(AStr, PWideChar(FStrings), FLength) = 0 do
        Inc(AStr, FLength);
    end
  end;

  Len := AStr - StartP;
  if not Result then
    Result := Len > 0;
end;

function TRECharCode.GetCharLength: Integer;
begin
  Result := FCharLength;
end;

{$IFDEF DEBUG}

function TRECharCode.GetDebugStr: REString;
var
  I: Integer;
  S: REString;
  IsW: Boolean;
begin
  I := 1;
  while I <= System.Length(FStrings) do
  begin
    IsW := IsLeadChar(FStrings[I]);

    if IsUnicodeProperty(ToUCS4Char(@FStrings[I]), upPrint) then
    begin
      if IsW then
      begin
        S := S + FStrings[I];
        Inc(I);
      end;
      S := S + FStrings[I];
    end
    else
    begin
      if IsW then
      begin
        S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
        Inc(I);
      end;
      S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
    end;
    Inc(I);
  end;

  Result := Format(sLiteral, [S]);
end;
{$ENDIF}

function TRECharCode.GetLength: Integer;
begin
  Result := FLength;
end;

function TRECharCode.GetWChar: UCS4Char;
var
  L: Integer;
begin
  Result := GetREChar(FSubP, L, FCompareOptions);
end;

function TRECharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if (ACode is TRECharCode) then
    Result := GetWChar = (ACode as TRECharCode).GetWChar
  else
    Result := False;
end;

function TRECharCode.IsOverlap(ACode: TRECode): Boolean;
var
  S: REString;
  LOptions: TRECompareOptions;
  L: Integer;
begin
  if ACode is TRECharCode then
  begin
    S := (ACode as TRECharCode).FStrings;
    LOptions := (ACode as TRECharCode).FCompareOptions;
    L := Min(System.Length(S), FLength);
    Result := RECompareString(FSubP, PWideChar(S), L, LOptions) = 0;
    // Result := REStrPos(FSubP, FSize, PWideChar(S), Length(S), LOptions) <> nil;
  end
  else if ACode is TRELiteralCode then
  begin
    S := (ACode as TRELiteralCode).FStrings;
    LOptions := (ACode as TRELiteralCode).FCompareOptions;
    L := Min(System.Length(S), FLength);
    Result := RECompareString(FSubP, PWideChar(S), L, LOptions) = 0;
    // Result := REStrPos(FSubP, FSize, PWideChar(S), Length(S), LOptions) <> nil;
  end
  else if ACode is TREWordCharCode then
  begin
    Result := FRegExp.IsWord(ToUCS4Char(FSubP));
    if (ACode as TREWordCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TREDigitCharCode then
  begin
    Result := FRegExp.IsDigit(ToUCS4Char(FSubP));
    if (ACode as TREDigitCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TREHexDigitCharCode then
  begin
    Result := IsHexDigit(ToUCS4Char(FSubP));
    if (ACode as TREHexDigitCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TRESpaceCharCode then
  begin
    Result := FRegExp.IsSpace(ToUCS4Char(FSubP));
    if (ACode as TRESpaceCharCode).FNegative then
      Result := not Result;
  end
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TRELiteralCode }

constructor TRELiteralCode.Create(ARegExp: TSkRegExp; Str: UCS4String;
  AOptions: TREOptions);
var
  I: Integer;
begin
  inherited Create(ARegExp);
  FStrings := RECharArrayToString(Str);
  FSubP := PWideChar(FStrings);
  FLength := System.Length(FStrings);
  FSubP := PWideChar(FStrings);
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);

  FCharLength := 0;
  for I := 1 to System.Length(FStrings) do
  begin
    if IsLeadChar(FStrings[I]) then
      Dec(FCharLength);
    Inc(FCharLength);
  end;
end;

constructor TRELiteralCode.Create(ARegExp: TSkRegExp; Str: REString;
  AOptions: TREOptions);
var
  I: Integer;
begin
  inherited Create(ARegExp);
  FStrings := Str;
  FSubP := PWideChar(FStrings);
  FLength := System.Length(FStrings);
  FSubP := PWideChar(FStrings);
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);

  FCharLength := 0;
  for I := 1 to System.Length(FStrings) do
  begin
    if IsLeadChar(FStrings[I]) then
      Dec(FCharLength);
    Inc(FCharLength);
  end;
end;

function TRELiteralCode.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len: Integer;
begin
  Result := IsStar;
  StartP := AStr;

  if (roIgnoreKana in FOptions) or (roIgnoreWidth in FOptions) then
  begin
    while REStrLJComp(AStr, PWideChar(FStrings), FLength,
      FCompareOptions) = 0 do
      Inc(AStr, FLength);
  end
  else if roIgnoreCase in FOptions then
  begin
    while REStrLIComp(AStr, PWideChar(FStrings), FLength) = 0 do
      Inc(AStr, FLength);
  end
  else
  begin
    if FLength = 1 then
    begin
      while AStr^ = PWideChar(FStrings)^ do
        Inc(AStr);
    end
    else
    begin
      while REStrLComp(AStr, PWideChar(FStrings), FLength) = 0 do
        Inc(AStr, FLength);
    end
  end;

  Len := AStr - StartP;
  if not Result then
    Result := Len > 0;
end;

function TRELiteralCode.GetCharLength: Integer;
begin
  Result := FCharLength;
end;

{$IFDEF DEBUG}

function TRELiteralCode.GetDebugStr: REString;
var
  I: Integer;
  S: REString;
  IsW: Boolean;
begin
  I := 1;
  while I <= System.Length(FStrings) do
  begin
    IsW := IsLeadChar(FStrings[I]);

    if IsUnicodeProperty(ToUCS4Char(@FStrings[I]), upPrint) then
    begin
      if IsW then
      begin
        S := S + FStrings[I];
        Inc(I);
      end;
      S := S + FStrings[I];
    end
    else
    begin
      if IsW then
      begin
        S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
        Inc(I);
      end;
      S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
    end;
    Inc(I);
  end;

  Result := Format(sLiteral, [S]);
end;
{$ENDIF DEBUG}

function TRELiteralCode.GetLength: Integer;
begin
  Result := FLength;
end;

function TRELiteralCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := RECompareString(AStr, FSubP, FLength, FCompareOptions) = 0;

  if Result then
    Len := FLength
  else
    Len := 0;
end;

function TRELiteralCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;

  if not(ACode is TRELiteralCode) then
    Exit;

  Result := (ACode as TRELiteralCode).FStrings = FStrings;
end;

function TRELiteralCode.IsOverlap(ACode: TRECode): Boolean;
var
  S: REString;
  LOptions: TRECompareOptions;
  L: Integer;
begin
  if ACode is TRECharCode then
  begin
    S := (ACode as TRECharCode).FStrings;
    LOptions := (ACode as TRECharCode).FCompareOptions;
    L := Min(System.Length(S), FLength);
    Result := RECompareString(FSubP, PWideChar(S), L, LOptions) = 0;
    // Result := REStrPos(FSubP, FSize, PWideChar(S), Length(S), LOptions) <> nil;
  end
  else if ACode is TRELiteralCode then
  begin
    S := (ACode as TRELiteralCode).FStrings;
    LOptions := (ACode as TRELiteralCode).FCompareOptions;
    L := Min(System.Length(S), FLength);
    Result := RECompareString(FSubP, PWideChar(S), L, LOptions) = 0;
    // Result := REStrPos(FSubP, FSize, PWideChar(S), Length(S), LOptions) <> nil;
  end
  else if ACode is TREWordCharCode then
  begin
    Result := FRegExp.IsWord(ToUCS4Char(FSubP));
    if (ACode as TREWordCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TREDigitCharCode then
  begin
    Result := FRegExp.IsDigit(ToUCS4Char(FSubP));
    if (ACode as TREDigitCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TREHexDigitCharCode then
  begin
    Result := IsHexDigit(ToUCS4Char(FSubP));
    if (ACode as TREHexDigitCharCode).FNegative then
      Result := not Result;
  end
  else if ACode is TRESpaceCharCode then
  begin
    Result := FRegExp.IsSpace(ToUCS4Char(FSubP));
    if (ACode as TRESpaceCharCode).FNegative then
      Result := not Result;
  end
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TRERangeCharCode }

function TRERangeCharCode.Compare(AStr: PWideChar): Integer;
var
  Ch: UCS4Char;
  L: Integer;
begin
  Ch := GetREChar(AStr, L, FCompareOptions);
  if Ch < FStartWChar then
    Result := -1
  else if Ch > FLastWChar then
    Result := 1
  else
    Result := 0;
end;

function TRERangeCharCode.CompareCode(Dest: TRECode): Integer;
var
  Ch1, Ch2: UCS4Char;
begin
  if Dest is TRECharCode then
  begin
    Ch1 := (Dest as TRECharCode).GetWChar;
    if Ch1 < FStartWChar then
      Result := 1
    else if Ch1 > FLastWChar then
      Result := -1
    else
      Result := 0;
  end
  else if Dest is TRERangeCharCode then
  begin
    Ch1 := (Dest as TRERangeCharCode).FStartWChar;
    Ch2 := (Dest as TRERangeCharCode).FLastWChar;

    if Ch2 < FStartWChar then
      Result := 1
    else if Ch1 > FLastWChar then
      Result := -1
    else
      Result := 0;
  end
  else if Dest is TREWordCharCode then
  begin
    if FRegExp.IsWord(FStartWChar) and FRegExp.IsWord(FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREDigitCharCode then
  begin
    if FRegExp.IsDigit(FStartWChar) and FRegExp.IsDigit(FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    if FRegExp.IsSpace(FStartWChar) and FRegExp.IsSpace(FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    if IsUnicodeProperty(FStartWChar, (Dest as TREPropertyCode)
      .FUniCodeProperty) and IsUnicodeProperty(FLastWChar,
      (Dest as TREPropertyCode).FUniCodeProperty) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    if IsHexDigit(FStartWChar) and IsHexDigit(FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    if IsVerticalWhiteSpace(FStartWChar) and
      IsVerticalWhiteSpace(FLastWChar) then
      Result := 0
    else
      Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    if IsHorizontalWhiteSpace(FStartWChar) and
      IsHorizontalWhiteSpace(FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else
    Result := -1;
end;

constructor TRERangeCharCode.Create(ARegExp: TSkRegExp;
  AStartWChar, ALastWChar: UCS4Char; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FStartWChar := AStartWChar;
  FLastWChar := ALastWChar;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TRERangeCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  W: UCS4Char;
begin
  W := GetREChar(AStr, Len, FCompareOptions);
  Result := (W >= FStartWChar) and (W <= FLastWChar);
  if not Result then
    Len := 0;
end;

{$IFDEF DEBUG}

function TRERangeCharCode.GetDebugStr: REString;
var
  S1, S2: REString;
begin
  if IsUnicodeProperty(FStartWChar, upCntrl) then
    S1 := Format('($%x)', [FStartWChar])
  else
  begin
    S1 := WideChar(FStartWChar);
    S1 := Format('%s ($%x)', [S1, FStartWChar]);
  end;

  if IsUnicodeProperty(FLastWChar, upCntrl) then
    S2 := Format('($%x)', [FLastWChar])
  else
  begin
    S2 := WideChar(FLastWChar);
    S2 := Format('%s ($%x)', [S2, FLastWChar]);
  end;

  Result := Format('%s ～ %s', [S1, S2]);
end;
{$ENDIF}

function TRERangeCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TRECharCode then
    Result := (FStartWChar >= (ACode as TRECharCode).GetWChar) and
      (FLastWChar <= (ACode as TRECharCode).GetWChar)
  else if ACode is TRERangeCharCode then
    Result := (FStartWChar >= (ACode as TRERangeCharCode).FStartWChar) and
      (FLastWChar <= (ACode as TRERangeCharCode).FLastWChar)
  else
    Result := False;
end;

{ TREAnyCharCode }

constructor TREAnyCharCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TREAnyCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  StartP: PWideChar;
begin
  Result := False;
  Len := 0;

  if FRegExp.FMatchEndP = AStr then
    Exit;

  StartP := AStr;

  if (roSingleLine in FOptions) then
  begin
    Result := True;

    Inc(AStr);
  end
  else
  begin
    if not IsLineSeparator(AStr^) then
    begin
      Inc(AStr);

      Result := True;
    end;
  end;
  if Result then
    Len := AStr - StartP;
end;

function TREAnyCharCode.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
begin
  Result := IsStar;

  StartP := AStr;

  if roSingleLine in FOptions then
  begin
    AStr := FRegExp.FMatchEndP;
  end
  else
  begin
    while (AStr <> FRegExp.FMatchEndP) and not IsLineSeparator(AStr^) do
      Inc(AStr);
  end;

  if not Result then
    Result := AStr - StartP > 0;
end;

{$IFDEF DEBUG}

function TREAnyCharCode.GetDebugStr: REString;
begin
  Result := sAnyChar;
end;
{$ENDIF}

function TREAnyCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREAnyCharCode then
    Result := True
  else
    Result := False;
end;

{ TREWordCharCode }

function TREWordCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := FRegExp.IsWord(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREWordCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if FRegExp.IsWord((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if FRegExp.IsWord((Dest as TRERangeCharCode).FStartWChar) and
      FRegExp.IsWord((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := 0;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := 0;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := 0
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := -1
  end
  else
    Result := -1;
end;

constructor TREWordCharCode.Create(ARegExp: TSkRegExp; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

function TREWordCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  if IsLeadChar(AStr^) then
  begin
    Result := FRegExp.IsWord(ToUCS4Char(AStr));
    Len := 2;
  end
  else
  begin
    Result := FRegExp.IsWord(ToUCS4Char(AStr));
    Len := 1;
  end;

  if FNegative then
    Result := not Result;
  if not Result then
    Len := 0;
end;

{$IFDEF DEBUG}

function TREWordCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sWordChar
  else
    Result := sNegativeWordChar;
end;
{$ENDIF}

function TREWordCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREWordCharCode then
    Result := (ACode as TREWordCharCode).FNegative = FNegative
  else if ACode is TREDigitCharCode then
  begin
    if FNegative then
      Result := (ACode as TREDigitCharCode).FNegative
    else
      Result := not(ACode as TREDigitCharCode).FNegative;
  end
  else if ACode is TRECharCode then
  begin
    Result := FRegExp.IsWord((ACode as TRECharCode).GetWChar);
    if FNegative then
      Result := not Result;
  end
  else if ACode is TRERangeCharCode then
  begin
    Result := FRegExp.IsWord((ACode as TRERangeCharCode).FStartWChar) and
      FRegExp.IsWord((ACode as TRERangeCharCode).FLastWChar);
    if FNegative then
      Result := not Result;
  end
  else
    Result := False;
end;

function TREWordCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if (ACode is TRECharCode) then
    Result := FRegExp.IsWord((ACode as TRECharCode).GetWChar)
  else if (ACode is TRELiteralCode) then
    Result := FRegExp.IsWord
      (ToUCS4Char(PWideChar((ACode as TRELiteralCode).FStrings)))
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TREDigitCharCode }

function TREDigitCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := FRegExp.IsDigit(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREDigitCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if FRegExp.IsDigit((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if FRegExp.IsDigit((Dest as TRERangeCharCode).FStartWChar) and
      FRegExp.IsDigit((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := -1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := 0;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := -1
  end
  else
    Result := -1;
end;

constructor TREDigitCharCode.Create(ARegExp: TSkRegExp; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

function TREDigitCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  if AStr = FRegExp.FMatchEndP then
    Exit;

  if IsLeadChar(AStr^) then
  begin
    Result := FRegExp.IsDigit(ToUCS4Char(AStr));
    Len := 2;
  end
  else
  begin
    Result := FRegExp.IsDigit(ToUCS4Char(AStr));
    Len := 1;
  end;

  if FNegative then
    Result := not Result;
  if not Result then
    Len := 0;
end;

{$IFDEF DEBUG}

function TREDigitCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sDigitChar
  else
    Result := sNegativeDigitChar;
end;
{$ENDIF}

function TREDigitCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREDigitCharCode then
    Result := FNegative and (ACode as TREDigitCharCode).FNegative
  else if ACode is TRECharCode then
  begin
    Result := FRegExp.IsDigit((ACode as TRECharCode).GetWChar);
    if FNegative then
      Result := not Result;
  end
  else if ACode is TRERangeCharCode then
  begin
    Result := FRegExp.IsDigit((ACode as TRERangeCharCode).FStartWChar) and
      FRegExp.IsDigit((ACode as TRERangeCharCode).FLastWChar);
    if FNegative then
      Result := not Result;
  end
  else
    Result := False;
end;

function TREDigitCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode is TRECharCode then
    Result := FRegExp.IsDigit((ACode as TRECharCode).GetWChar)
  else if ACode is TRELiteralCode then
    Result := FRegExp.IsDigit
      (ToUCS4Char(PWideChar((ACode as TRELiteralCode).FStrings)))
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TREHexDigitCharCode }

function TREHexDigitCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := IsHexDigit(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREHexDigitCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if IsHexDigit((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if IsHexDigit((Dest as TRERangeCharCode).FStartWChar) and
      IsHexDigit((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := 0;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := -1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := 0
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := -1
  end
  else
    Result := -1;
end;

constructor TREHexDigitCharCode.Create(ARegExp: TSkRegExp; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

function TREHexDigitCharCode.IsEqual(AStr: PWideChar; var Len: Integer)
  : Boolean;
begin
  Result := False;
  Len := 0;
  if AStr = FRegExp.FMatchEndP then
    Exit;

  if IsLeadChar(AStr^) then
  begin
    Result := IsHexDigit(ToUCS4Char(AStr));
    Len := 2;
  end
  else
  begin
    Result := IsHexDigit(ToUCS4Char(AStr));
    Len := 1;
  end;

  if FNegative then
    Result := not Result;
  if not Result then
    Len := 0;
end;

{$IFDEF DEBUG}

function TREHexDigitCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sHexDigitChar
  else
    Result := sNegativeHexDigitChar;
end;
{$ENDIF}

function TREHexDigitCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREHexDigitCharCode then
    Result := FNegative and (ACode as TREHexDigitCharCode).FNegative
  else if ACode is TRECharCode then
  begin
    Result := IsHexDigit((ACode as TRECharCode).GetWChar);
    if FNegative then
      Result := not Result;
  end
  else if ACode is TRERangeCharCode then
  begin
    Result := FRegExp.IsDigit((ACode as TRERangeCharCode).FStartWChar) and
      IsHexDigit((ACode as TRERangeCharCode).FLastWChar);
    if FNegative then
      Result := not Result;
  end
  else if ACode is TREWordCharCode then
    Result := FNegative and (ACode as TREHexDigitCharCode).FNegative
  else
    Result := False;
end;

function TREHexDigitCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode is TRECharCode then
    Result := IsHexDigit((ACode as TRECharCode).GetWChar)
  else if ACode is TRELiteralCode then
    Result := IsHexDigit(ToUCS4Char(PWideChar((ACode as TRELiteralCode)
      .FStrings)))
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TRESpaceCharCode }

function TRESpaceCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := FRegExp.IsSpace(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TRESpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if FRegExp.IsSpace((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if FRegExp.IsSpace((Dest as TRERangeCharCode).FStartWChar) and
      FRegExp.IsSpace((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := -1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := -1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := 0;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := -1
  end
  else
    Result := -1;
end;

constructor TRESpaceCharCode.Create(ARegExp: TSkRegExp; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

function TRESpaceCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  if AStr = FRegExp.FMatchEndP then
    Exit;

  if IsLeadChar(AStr^) then
  begin
    Result := FRegExp.IsSpace(ToUCS4Char(AStr));
    Len := 2;
  end
  else
  begin
    Result := FRegExp.IsSpace(ToUCS4Char(AStr));
    Len := 1;
  end;
  if FNegative then
    Result := not Result;
  if not Result then
    Len := 0;
end;

{$IFDEF DEBUG}

function TRESpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sSpaceChar
  else
    Result := sNegativeSpaceChar;
end;
{$ENDIF}

function TRESpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TRESpaceCharCode then
    Result := FNegative = (ACode as TRESpaceCharCode).FNegative
  else if ACode is TRECharCode then
  begin
    Result := FRegExp.IsSpace((ACode as TRECharCode).GetWChar);
    if FNegative then
      Result := not Result;
  end
  else if ACode is TRERangeCharCode then
    Result := FRegExp.IsSpace((ACode as TRERangeCharCode).FStartWChar) and
      FRegExp.IsSpace((ACode as TRERangeCharCode).FLastWChar)
  else
    Result := False;
end;

function TRESpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode is TRECharCode then
    Result := FRegExp.IsSpace((ACode as TRECharCode).GetWChar)
  else if ACode is TRELiteralCode then
    Result := FRegExp.IsSpace
      (ToUCS4Char(PWideChar((ACode as TRELiteralCode).FStrings)))
  else if ACode.CharLength = 0 then
    Result := False
  else
    Result := True;
end;

{ TRHorizontalSpaceCharCode }

function TREHorizontalSpaceCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := IsHorizontalWhiteSpace(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREHorizontalSpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if IsHorizontalWhiteSpace((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if IsHorizontalWhiteSpace((Dest as TRERangeCharCode).FStartWChar) and
      IsHorizontalWhiteSpace((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := -1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := -1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := 0
  end
  else
    Result := -1;
end;

constructor TREHorizontalSpaceCharCode.Create(ARegExp: TSkRegExp;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

{$IFDEF DEBUG}

function TREHorizontalSpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sHorizontalSpaceChar
  else
    Result := sNegativeHorizontalSpaceChar;
end;
{$ENDIF}

function TREHorizontalSpaceCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  Result := IsHorizontalWhiteSpace(AStr);
  if FNegative then
    Result := not Result;

  if Result then
    Len := 1;
end;

function TREHorizontalSpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREHorizontalSpaceCharCode;
end;

function TREHorizontalSpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := ACode is TREHorizontalSpaceCharCode;
end;

{ TREVerticalSpaceCharCode }

function TREVerticalSpaceCharCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := IsVerticalWhiteSpace(ToUCS4Char(AStr));
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREVerticalSpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if IsVerticalWhiteSpace((Dest as TRECharCode).GetWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if IsVerticalWhiteSpace((Dest as TRERangeCharCode).FStartWChar) and
      IsVerticalWhiteSpace((Dest as TRERangeCharCode).FLastWChar) then
      Result := 0
    else
      Result := -1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := -1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := -1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := -1
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := 0
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := -1
  end
  else
    Result := -1;
end;

constructor TREVerticalSpaceCharCode.Create(ARegExp: TSkRegExp;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

{$IFDEF DEBUG}

function TREVerticalSpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sVerticalSpaceChar
  else
    Result := sNegativeVerticalSpaceChar;
end;
{$ENDIF}

function TREVerticalSpaceCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;
  if AStr = FRegExp.FMatchEndP then
    Exit;

  Result := IsVerticalWhiteSpace(AStr);
  if FNegative then
    Result := not Result;

  if Result then
    Len := 1;
end;

function TREVerticalSpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREVerticalSpaceCharCode;
end;

function TREVerticalSpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := ACode is TREVerticalSpaceCharCode;
end;

{ TRELineBreakCharCode }

constructor TRELineBreakCharCode.Create(ARegExp: TSkRegExp);
begin
  inherited Create(ARegExp);
end;

function TRELineBreakCharCode.GetCharLength: Integer;
begin
  Result := 2;
end;

{$IFDEF DEBUG}

function TRELineBreakCharCode.GetDebugStr: REString;
begin
  Result := sLineBreakChar;
end;
{$ENDIF}

function TRELineBreakCharCode.GetLength: Integer;
begin
  Result := 2;
end;

function TRELineBreakCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if AStr^ = #$000D then
  begin
    Inc(AStr);
    if AStr^ = #$000A then
      Len := 2
    else
      Len := 1;
    Result := True;
  end
  else if (AStr^ = #$000A) or (AStr^ = #$000B) or (AStr^ = #$000C) or
    (AStr^ = #$0085) or (AStr^ = #$2028) or (AStr^ = #$2029) then
  begin
    Len := 1;
    Result := True;
  end;
end;

function TRELineBreakCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineBreakCharCode;
end;

function TRELineBreakCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineBreakCharCode;
end;

{ TRECharClassCode }

function TRECharClassCode.Add(AStartWChar, ALastWChar: UCS4Char): Integer;
begin
  if (roIgnoreCase in FOptions) then
  begin
    if (IsUnicodeProperty(AStartWChar, upUpper) and
      IsUnicodeProperty(ALastWChar, upUpper)) or
      (IsUnicodeProperty(AStartWChar, upLower) and IsUnicodeProperty(ALastWChar,
      upLower)) then
    begin
      CharUpperBuffW(@AStartWChar, 1);
      CharUpperBuffW(@ALastWChar, 1);
    end
    else
      Exclude(FOptions, roIgnoreCase);
  end;
  Result := FCodeList.Add(TRERangeCharCode.Create(FRegExp, AStartWChar,
    ALastWChar, FOptions));
end;

function TRECharClassCode.Add(AWChar: UCS4Char): Integer;
var
  LStr, SubStr: REString;
begin
  Result := 0;
  FMap.Add(AWChar);

  LStr := UCS4CharToString(AWChar);

  if roIgnoreCase in FOptions then
  begin
    SubStr := AnsiUpperCase(LStr);
    if LStr <> SubStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));
    SubStr := AnsiLowerCase(LStr);
    if LStr <> SubStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));
  end;

  if roIgnoreKana in FOptions then
  begin
    SubStr := ToWide(LStr);
    if SubStr <> LStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));
    SubStr := ToHalf(LStr);
    if SubStr <> LStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));
  end;

  if roIgnoreWidth in FOptions then
  begin
    SubStr := ToHiragana(LStr);
    if SubStr <> LStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));

    SubStr := ToKatakana(LStr);
    if SubStr <> LStr then
      FMap.Add(ToUCS4Char(PWideChar(SubStr)));
  end;

  FIsSimple := True;
end;

function TRECharClassCode.Add(Value: TRECode): Integer;
begin
  Result := FCodeList.Add(Value);
end;

constructor TRECharClassCode.Create(ARegExp: TSkRegExp; ANegative: Boolean;
  AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FMap := TRECharMap.Create;
  FCodeList := TObjectList.Create;
  FNegative := ANegative;
  FOptions := AOptions;
end;

destructor TRECharClassCode.Destroy;
begin
  FCodeList.Free;
  FMap.Free;
  inherited;
end;

function TRECharClassCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
label
  Final;
var
  I, C: Integer;
begin
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
  begin
    Result := False;
    Exit;
  end;

  Result := FNegative;

  if FIsSimple then
  begin
    if FMap.IsExists(AStr) then
    begin
      Result := not Result;
      goto Final;
    end;
  end;

  for I := 0 to FCodeList.Count - 1 do
  begin
    C := TRECode(FCodeList[I]).Compare(AStr);
    if C = 0 then
    begin
      Result := not Result;
      Break;
    end
    else if C < 0 then
      Break;
  end;

Final:
  if Result then
  begin
    if IsLeadChar(AStr^) then
      Len := 2
    else
      Len := 1;
  end;
end;

{$IFDEF DEBUG}

function TRECharClassCode.GetDebugStr: REString;
var
  I: Integer;
begin
  Result := sMsgRange;
  for I := 0 to FCodeList.Count - 1 do
    if I > 0 then
      Result := Result + ', ' + TRECode(FCodeList[I]).GetDebugStr
    else
      Result := Result + TRECode(FCodeList[I]).GetDebugStr;
end;
{$ENDIF} // DEBUG

function TRECharClassCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;
end;

function TRECharClassCode.IsOverlap(ACode: TRECode): Boolean;
var
  LCode: TRECode;
  I: Integer;
begin
  Result := FNegative;

  if ACode is TRECharCode then
  begin
    if FMap.IsExists((ACode as TRECharCode).FSubP) then
    begin
      Result := not Result;
      Exit;
    end;
  end;

  for I := 0 to FCodeList.Count - 1 do
  begin
    LCode := TRECode(FCodeList[I]);
    if LCode.IsOverlap(ACode) then
    begin
      Result := not Result;
      Exit;
    end;
  end;
end;

procedure TRECharClassCode.Rebuild;

  procedure RebuildSub(Index: Integer);
  var
    Source, Dest: TRECode;
    I: Integer;
  begin
    Source := TRECode(FCodeList[Index]);
    for I := FCodeList.Count - 1 downto 0 do
    begin
      if I <> Index then
      begin
        Dest := TRECode(FCodeList[I]);
        if Source.IsInclude(Dest) then
          FCodeList.Delete(I);
      end;
    end;
  end;

var
  I: Integer;
begin
  I := 0;

  while I < FCodeList.Count do
  begin
    RebuildSub(I);
    Inc(I);
  end;
  Sort;
end;

function RECompareCode(Item1, Item2: Pointer): Integer;
begin
  Result := TRECode(Item1).CompareCode(TRECode(Item2));
end;

procedure TRECharClassCode.Sort;
begin
  FCodeList.Sort(@RECompareCode);
end;

{ TRECombiningSequence }

function TRECombiningSequence.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  StartP: PWideChar;
begin
  Result := False;
  StartP := AStr;
  Len := 0;

  if AStr >= FRegExp.FMatchEndP then
    Exit;

  if not IsUnicodeProperty(ToUCS4Char(AStr), upM) then
  begin
    Inc(AStr);
    Result := True;

    while (AStr < FRegExp.FMatchEndP) and
      IsUnicodeProperty(ToUCS4Char(AStr), upM) do
      Inc(AStr);
  end;

  if Result then
    Len := AStr - StartP;
end;

function TRECombiningSequence.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
begin
  Result := True;
  AStr := FRegExp.FMatchEndP;
end;

function TRECombiningSequence.GetCharLength: Integer;
begin
  Result := -1;
end;

{$IFDEF DEBUG}

function TRECombiningSequence.GetDebugStr: REString;
begin
  Result := sCombiningSequence;
end;
{$ENDIF}

function TRECombiningSequence.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRECombiningSequence;
end;

{ TREBoundaryCode }

constructor TREBoundaryCode.Create(ARegExp: TSkRegExp; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

function TREBoundaryCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  PrevType, CurType: Boolean;
begin
  Len := 0;

  if not FNegative then
  begin
    if AStr = FRegExp.FMatchTopP then
      PrevType := False
    else
    begin
      Dec(AStr);

      PrevType := FRegExp.IsWord(ToUCS4Char(AStr));

      Inc(AStr);
    end;

    if AStr = FRegExp.FMatchEndP then
      CurType := False
    else
      CurType := FRegExp.IsWord(ToUCS4Char(AStr));

    Result := PrevType <> CurType;
  end
  else
  begin
    if AStr <> FRegExp.FMatchTopP then
    begin
      Dec(AStr);

      PrevType := not FRegExp.IsWord(ToUCS4Char(AStr));

      Inc(AStr);
    end
    else
      PrevType := True;

    if AStr <> FRegExp.FMatchEndP then
      CurType := not FRegExp.IsWord(ToUCS4Char(AStr))
    else
      CurType := True;

    Result := PrevType = CurType;
  end;
end;

function TREBoundaryCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TREBoundaryCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sBoundaryCode
  else
    Result := sNegativeBoundaryCode;
end;
{$ENDIF}

function TREBoundaryCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREBoundaryCode;
end;

{ TREReferenceCode }

constructor TREReferenceCode.Create(ARegExp: TSkRegExp; AGroupIndex: Integer;
  AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FGroupIndex := AGroupIndex;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TREReferenceCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  S: REString;
  ARefTagNo: Integer;
begin
  Result := False;
  Len := 0;

  ARefTagNo := FGroupIndex;

  if not FRegExp.FGroups[ARefTagNo].Capture.Matched then
    Exit;

  S := FRegExp.FGroups[ARefTagNo].Strings;

  Result := RECompareString(AStr, PWideChar(S), System.Length(S),
    FCompareOptions) = 0;
  if Result then
    Len := System.Length(S);
end;

function TREReferenceCode.GetCharLength: Integer;
begin
  Result := -1;
end;

{$IFDEF DEBUG}

function TREReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sFmtGroupReference, [FGroupIndex]);
end;
{$ENDIF}

function TREReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREReferenceCode;
  if Result then
    Result := (ACode as TREReferenceCode).FGroupIndex = FGroupIndex;
end;

{ TRENamedReferenceCode }

constructor TRENamedReferenceCode.Create(ARegExp: TSkRegExp;
  AGroupName: REString; AGroupIndexArray: TIntDynArray; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FGroupName := AGroupName;
  FGroupIndexArray := AGroupIndexArray;
  FCount := System.Length(FGroupIndexArray);
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TRENamedReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  S: REString;
  I, ARefTagNo: Integer;
begin
  Result := False;
  Len := 0;

  for I := 0 to FCount - 1 do
  begin
    ARefTagNo := FGroupIndexArray[I];

    if FRegExp.FGroups[ARefTagNo].Capture.Matched then
    begin
      S := FRegExp.FGroups[ARefTagNo].Strings;

      if RECompareString(AStr, PWideChar(S), System.Length(S),
        FCompareOptions) = 0 then
      begin
        Result := True;
        Len := System.Length(S);
        Exit;
      end;
    end;
  end;
end;

function TRENamedReferenceCode.GetCharLength: Integer;
begin
  Result := -1;
end;

{$IFDEF DEBUG}

function TRENamedReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sFmtGroupNameReference, [FGroupName]);
end;
{$ENDIF}

function TRENamedReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRENamedReferenceCode;
  if Result then
    Result := (ACode as TRENamedReferenceCode)
      .FGroupIndexArray = FGroupIndexArray;
end;

procedure TRENamedReferenceCode.SetGroupIndexArray(
  AGroupIndexArray: TIntDynArray);
begin
  FGroupIndexArray := AGroupIndexArray;
  FCount := System.Length(FGroupIndexArray);
end;

{ TRELineHeadCode }

constructor TRELineHeadCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
end;

function TRELineHeadCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;

  if roMultiLine in FOptions then
  begin
    if (AStr = FRegExp.FMatchTopP) then
      Result := True
    else
    begin
      Dec(AStr);
      Result := IsLineSeparator(AStr^);
    end;
  end
  else
    Result := AStr = FRegExp.FMatchTopP;
end;

function TRELineHeadCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TRELineHeadCode.GetDebugStr: REString;
begin
  Result := sHeadOfLineCode
end;
{$ENDIF}

function TRELineHeadCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineHeadCode;
end;

{ TRELineTailCode }

constructor TRELineTailCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
end;

function TRELineTailCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  if roMultiLine in FOptions then
  begin
    if (AStr = FRegExp.FMatchEndP) then
      Result := True
    else
      Result := IsLineSeparator(AStr^);
  end
  else
    Result := AStr = FRegExp.FMatchEndP;
end;

function TRELineTailCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TRELineTailCode.GetDebugStr: REString;
begin
  Result := sEndOfLineCode;
end;
{$ENDIF}

function TRELineTailCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineTailCode;
end;

{ TRETextHeadCode }

function TRETextHeadCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := FRegExp.FMatchTopP = AStr;
end;

function TRETextHeadCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TRETextHeadCode.GetDebugStr: REString;
begin
  Result := sTextHeadCode;
end;
{$ENDIF}

function TRETextHeadCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextHeadCode;
end;

{ TRETextTailCode }

function TRETextTailCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  while IsLineSeparator(AStr^) do
    Inc(AStr);

  Result := FRegExp.FMatchEndP = AStr;
end;

function TRETextTailCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TRETextTailCode.GetDebugStr: REString;
begin
  Result := sTextTailCode;
end;
{$ENDIF}

function TRETextTailCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextTailCode;
end;

{ TRETextEndCode }

function TRETextEndCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := FRegExp.FMatchEndP = AStr;
end;

function TRETextEndCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TRETextEndCode.GetDebugStr: REString;
begin
  Result := sTextEndCode;
end;
{$ENDIF}

function TRETextEndCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextEndCode;
end;

{ TREPosixCode }

function TREPropertyCode.Compare(AStr: PWideChar): Integer;
var
  b: Boolean;
begin
  b := UnicodeProp.IsUnicodeProperty(ToUCS4Char(AStr), FUniCodeProperty);
  if FNegative then
    b := not b;

  if b then
    Result := 0
  else
    Result := 1;
end;

function TREPropertyCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TRECharCode then
  begin
    if UnicodeProp.IsUnicodeProperty((Dest as TRECharCode).GetWChar,
      FUniCodeProperty) then
      Result := 0
    else
      Result := 1;
  end
  else if Dest is TRERangeCharCode then
  begin
    if UnicodeProp.IsUnicodeProperty((Dest as TRERangeCharCode).FStartWChar,
      FUniCodeProperty) and UnicodeProp.IsUnicodeProperty
      ((Dest as TRERangeCharCode).FLastWChar, FUniCodeProperty) then
      Result := 0
    else
      Result := 1;
  end
  else if Dest is TREWordCharCode then
  begin
    Result := 1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := 1;
  end
  else if (Dest is TRESpaceCharCode) then
  begin
    Result := 1;
  end
  else if (Dest is TREPropertyCode) then
  begin
    if FUniCodeProperty = (Dest as TREPropertyCode).FUniCodeProperty then
      Result := 0
    else
      Result := 1;
  end
  else if (Dest is TREHexDigitCharCode) then
  begin
    Result := 1
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := 11
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := 1
  end
  else
    Result := 1;
end;

constructor TREPropertyCode.Create(ARegExp: TSkRegExp;
  AUnicodeProperty: TUnicodeProperty; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FUniCodeProperty := AUnicodeProperty;
  FNegative := ANegative;
end;

function TREPropertyCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := IsUnicodeProperty(ToUCS4Char(AStr), FUniCodeProperty);
  if FNegative then
    Result := not Result;
  if not Result then
    Len := 0
  else
  begin
    if IsLeadChar(AStr^) then
      Len := 2
    else
      Len := 1;
  end;
end;

{$IFDEF DEBUG}

function TREPropertyCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sPropertyCode
  else
    Result := sNegativePropertyCode
end;
{$ENDIF}

function TREPropertyCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREPropertyCode) and
    ((ACode as TREPropertyCode).FUniCodeProperty = FUniCodeProperty);
end;

{ TREGlobalPosCode }

function TREGlobalPosCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TREGlobalPosCode.GetDebugStr: REString;
begin
  Result := sGlobalPos;
end;
{$ENDIF}

function TREGlobalPosCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := AStr = FRegExp.FGlobalEndP;
end;

function TREGlobalPosCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREGlobalPosCode;
end;

{ TREIfThenReferenceCode }

constructor TREIfThenReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupIndex: Integer);
begin
  inherited Create(ARegExp);
  FGroupIndex := AGroupIndex;
end;

function TREIfThenReferenceCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TREIfThenReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sIfThenReference, [FGroupIndex]);
end;
{$ENDIF}

function TREIfThenReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := FRegExp.FGroups[FGroupIndex].Capture.Matched;
end;

function TREIfThenReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREIfThenReferenceCode) and
    ((ACode as TREIfThenReferenceCode).FGroupIndex = FGroupIndex);
end;

{ TREIfThenNamedReferenceCode }

constructor TREIfThenNamedReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupName: REString);
begin
  inherited Create(ARegExp);
  FGroupName := AGroupName;
end;

function TREIfThenNamedReferenceCode.GetCharLength: Integer;
begin
  Result := 0;
end;

{$IFDEF DEBUG}

function TREIfThenNamedReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sIfThenNamedReference, [FGroupName]);
end;
{$ENDIF}

function TREIfThenNamedReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  Index: Integer;
  LMatchData: TGroupCollection;
begin
  Result := False;
  Len := 0;

  LMatchData := FRegExp.FGroups;

  Index := LMatchData.IndexOfMatchedName(FGroupName);
  if Index = -1 then
    Exit;

  Result := LMatchData[Index].Capture.Matched;
end;

function TREIfThenNamedReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREIfThenNamedReferenceCode) and
    ((ACode as TREIfThenNamedReferenceCode).FGroupName = FGroupName);
end;

{ TREBinCode }

constructor TREBinCode.Create(ARegExp: TSkRegExp; AOp: TREOperator;
  ALeft, ARight: TRECode; AMin, AMax: Integer);
begin
  inherited Create(ARegExp);
  FOp := AOp;
  FLeft := ALeft;
  FRight := ARight;
  FMin := AMin;
  FMax := AMax;
  FMatchKind := lkGreedy;
end;

{ TRELex }

procedure TRELex.Assign(Source: TRELex);
begin
  FToken := Source.FToken;
  FP := Source.FP;
  FTokenStartP := Source.FTokenStartP;
  FTopP := Source.FTopP;
  FLastP := Source.FLastP;
  FWChar := Source.FWChar;
  FStartWChar := Source.FStartWChar;
  FLastWChar := Source.FLastWChar;
  FMin := Source.FMin;
  FMax := Source.FMax;
  FLevel := Source.FLevel;
  FContext := Source.FContext;
  FUniCodeProperty := Source.FUniCodeProperty;
  FConvert := Source.FConvert;
  FIsQuote := Source.FIsQuote;
  FGroupName := Source.FGroupName;
  FOptions := Source.FOptions;
  FNewOptions := Source.FNewOptions;
  FOptionList.Assign(Source.FOptionList);
end;

procedure TRELex.CharNext(var P: PWideChar; const Len: Integer);
begin
  // TRELexでは文字の読み出しにGetRECharを使っているため、
  // ここでサロゲートを処理する必要はない。
  if P^ <> #0000 then
    Inc(P, Len);
  if roExtended in FOptions then
    while (P^ <> #0) and ((P^ = ' ') or (P^ = #9) or (P^ = #10) or
      (P^ = #13)) do
      Inc(P);
end;

procedure TRELex.CharPrev(var P: PWideChar; const Len: Integer);
begin
  // TRELexでは文字の読み出しにGetRECharを使っているため、
  // ここでサロゲートを処理する必要はない。
  if P > FTopP then
    Dec(P, Len);
  if roExtended in FOptions then
    while (P^ <> #0) and ((P^ = ' ') or (P^ = #9) or (P^ = #10) or
      (P^ = #13)) do
      Dec(P);
end;

procedure TRELex.ClearOptionList;
var
  I: Integer;
  P: PREOptions;
begin
  for I := 0 to FOptionList.Count - 1 do
    if FOptionList[I] <> nil then
    begin
      P := FOptionList[I];
      Dispose(P);
    end;
  FOptionList.Clear;
end;

constructor TRELex.Create(ARegExp: TSkRegExp; const Expression: REString);
begin
  inherited Create;
  FOptionList := TList.Create;
  FRegExp := ARegExp;
  FOptions := FRegExp.FOptions;
  FP := PWideChar(Expression);
  FTopP := FP;
  FLastP := FP + Length(Expression);

  FPrevCount := 0;
  FPrevLex[0].FOptionList := TList.Create;
  FPrevLex[1].FOptionList := TList.Create;
  FPrevLex[2].FOptionList := TList.Create;
end;

destructor TRELex.Destroy;
begin
  FPrevLex[0].FOptionList.Free;
  FPrevLex[1].FOptionList.Free;
  FPrevLex[2].FOptionList.Free;

  ClearOptionList;
  FOptionList.Free;
  inherited;
end;

procedure TRELex.Error(const Msg, Prefix: REString);
var
  S, T: REString;
begin
  T := GetErrorPositionString(GetCompileErrorPos);
  CharNext(FP);
  SetString(S, FTokenStartP, FP - FTokenStartP);
  if S <> '' then
    S := S + Prefix;
  S := Format(Msg, [S]);

  raise ESkRegExpCompile.CreateFmt('%s; %s', [S, T]);
end;

procedure TRELex.Error(const Msg: REString; APosition: Integer);
var
  S, T: REString;
begin
  T := GetErrorPositionString(APosition);
  CharNext(FP);
  SetString(S, FTokenStartP, FP - FTokenStartP);
  S := Format(Msg, [S]);
  raise ESkRegExpCompile.Create(Format('%s; %s', [S, T]));
end;

function TRELex.GetCompileErrorPos: Integer;
begin
  if FP <> nil then
    Result := FP - FTopP + 1
  else
    Result := 0;
end;

function TRELex.GetControlCode(var Len: Integer): UCS4Char;
var
  P: PWideChar;
begin
  Result := 0;
  CharNext(FP);

  P := FP;
  if ((P^ >= '@') and (P^ <= '_')) or ((P^ >= 'a') and (P^ <= 'z')) then
  begin
    if P^ = '\' then
    begin
      Inc(P);
      if P^ <> '\' then
        Error(sInvalidEscapeCharacterSyntax, GetCompileErrorPos);
      Inc(Len);
    end;

    Result := UCS4Char(P^);
    if (Result >= UCS4Char('a')) and (Result <= UCS4Char('z')) then
      Dec(Result, $20);
    Result := Result xor $40;
  end
  else
    Error(sInvalidEscapeCharacterSyntax, GetCompileErrorPos);
end;

function TRELex.GetDigit(var Len: Integer): Integer;
var
  P: PWideChar;
begin
  P := FP;
  Result := 0;

  while (P^ >= '0') and (P^ <= '9') do
  begin
    Result := Result * 10 + (Integer(P^) - Integer('0'));
    CharNext(P);
  end;
  Len := P - FP;
end;

function TRELex.GetErrorPositionString(APosition: Integer): REString;
begin
  if APosition > 0 then
    Result := Copy(FRegExp.Expression, 1, APosition) + ' <-- ' +
      Copy(FRegExp.Expression, APosition + 1, MaxInt);
end;

function TRELex.GetHexDigit(var Len: Integer): UCS4Char;
var
  P: PWideChar;
  I, L: Integer;
  IsBrace, Is6: Boolean;
begin
  Result := 0;
  CharNext(FP);

  if FP^ = '{' then
  begin
    CharNext(FP);
    IsBrace := True;
    L := 6;
  end
  else
  begin
    IsBrace := False;
    L := 2;
  end;

  P := FP;
  Len := 0;
  Is6 := True;

  for I := 1 to L do
  begin
    case P^ of
      '0' .. '9':
        Result := (Result shl 4) + (UCS4Char(P^) - UCS4Char('0'));
      'A' .. 'F':
        Result := (Result shl 4) + (UCS4Char(P^) - UCS4Char('7'));
      'a' .. 'f':
        Result := (Result shl 4) + (UCS4Char(P^) - UCS4Char('W'));
      '}':
        begin
          if IsBrace then
          begin
            Is6 := False;
            Inc(Len);
            Break;
          end;
        end
    else
      begin
        FP := P;
        Error(sHexDigitIsRequired);
      end;
    end;
    CharNext(P);
    Inc(Len);
  end;

  if IsBrace and Is6 then
  begin
    if P^ <> '}' then
    begin
      FP := P;
      Error(sMissingRightBraceOnEscx);
    end;
    Inc(Len);
  end;

  if Len = 0 then
  begin
    FP := P;
    Error(sHexDigitIsRequired);
  end;
  if (DWORD(Result) > $10FFFF) then
  begin
    FP := P;
    Error(sCodePointRangeOver);
  end;
end;

function TRELex.GetOctalDigit(var Len: Integer): UCS4Char;
var
  P: PWideChar;
  I: Integer;
begin
  Result := 0;
  Len := 0;

  P := FP;

  for I := 1 to 3 do
  begin
    case P^ of
      '0' .. '7':
        Result := (Result shl 3) + (UCS4Char(P^) - UCS4Char('0'));
    else
      Break;
    end;
    Inc(Len);
    CharNext(P);
  end;
end;

function TRELex.GetRECompareOptions: TRECompareOptions;
begin
  Result := REOptionsToRECompareOptions(FOptions);
end;

procedure TRELex.GetToken(Skip: Boolean);
var
  L: Integer;
begin
  if not Skip then
    Save;

  FConvert := False;
  FWChar := 0;
  FStartWChar := 0;
  FLastWChar := 0;
  FMin := 0;
  FMax := 0;
  FLevel := 0;
  FGroupName := '';
  FTokenStartP := FP;

  L := 1;

  if roExtended in FOptions then
    SkipWhiteSpace;

  if FP^ = #0 then
  begin
    if FContext <> ctNormal then
      Error(sUnmatchedBigPar);

    FToken := tkEnd;
    Exit;
  end;

  if FContext = ctCharClass then
  begin
    if FP^ = ']' then
    begin
      if (FP + 1)^ <> ']' then
      begin
        FToken := tkCharClassEnd;
        FContext := ctNormal;
        CharNext(FP, 1);
      end
      else
      begin
        FWChar := UCS4Char(']');
        FToken := tkChar;
        CharNext(FP, 1);
      end;
    end
    else
      LexCharClass;
  end
  else if FContext = ctQuote then
  begin
    if REStrLComp(FP, '\E', 2) = 0 then
    begin
      FContext := ctNormal;
      CharNext(FP, 2);
      GetToken(True);
      Exit;
    end;
    FWChar := GetREChar(FP, L, GetRECompareOptions);
    FToken := tkChar;
    CharNext(FP, L);
  end
  else
  begin
    if not FIsQuote then
    begin
      case FP^ of
        '|':
          FToken := tkUnion;
        '(':
          begin
            CharNext(FP);
            LexLeftPar;

            Exit;
          end;
        ')':
          FToken := tkRPar;
        '*':
          begin
            FToken := tkStar;
            FMin := 0;
            FMax := 0;
          end;
        '+':
          begin
            FToken := tkPlus;
            FMin := 1;
            FMax := 0;
          end;
        '?':
          FToken := tkQuest;
        '.':
          FToken := tkDot;
        '\':
          begin
            LexEscChar;
            Exit;
          end;
        '[':
          begin
            CharNext(FP);
            if FP^ = '^' then
            begin
              FContext := ctCharClass;
              FToken := tkNegativeCharClassFirst;
            end
            else if FP^ = ':' then
              LexPosixCharClass
            else
            begin
              CharPrev(FP);
              FContext := ctCharClass;
              FToken := tkCharClassFirst;
            end;
          end;
        '{':
          begin
            CharNext(FP);
            if (FP^ >= '0') and (FP^ <= '9') then
            begin
              LexBrace;
              Exit;
            end
            else
            begin
              CharPrev(FP);
              FWChar := GetREChar(FP, L, GetRECompareOptions);
              FToken := tkChar;
            end;
          end;
        '^':
          begin
            FWChar := GetREChar(FP, L, GetRECompareOptions);
            FToken := tkLHead;
            FConvert := True;
          end;
        '$':
          begin
            FWChar := GetREChar(FP, L, GetRECompareOptions);
            FToken := tkLTail;
            FConvert := True;
          end;
      else
        begin
          FWChar := GetREChar(FP, L, GetRECompareOptions);
          FToken := tkChar;
        end;
      end;
    end
    else
    begin
      FWChar := GetREChar(FP, L, GetRECompareOptions);
      FToken := tkChar;
    end;
    CharNext(FP, L);
  end;
end;

procedure TRELex.LexBrace;
var
  I, L: Integer;
begin
  SkipWhiteSpace;

  I := GetDigit(L);
  CharNext(FP, L);

  if I > CONST_LoopLimit then
    Error(sQuantifierIsTooLarge);

  FMin := I;

  if FP^ = ',' then
  begin
    CharNext(FP);
    SkipWhiteSpace;
    if (FP^ >= '0') and (FP^ <= '9') then
    begin
      I := GetDigit(L);
      CharNext(FP, L);

      if I > CONST_LoopLimit then
        Error(sQuantifierIsTooLarge);

      if I < FMin then
        Error(sCantDoMaxLessMin);

      FMax := I;
    end
    else
      FMax := CONST_LoopMax;
  end
  else
    FMax := FMin;

  if FP^ <> '}' then
    Error(sUnmatchedCurlyBracket);

  CharNext(FP);
  FToken := tkBound;
end;

procedure TRELex.LexCharClass;

  function GetConvertREChar(AWChar: UCS4Char; AOptions: TRECompareOptions): UCS4Char;
  var
    S: REString;
  begin
    S := UCS4CharToString(AWChar);
    if coIgnoreWidth in AOptions then
      S := ToWide(S);
    if coIgnoreKana in AOptions then
      S := ToKatakana(S);

    Result := ToUCS4Char(PWideChar(S));
  end;

var
  L: Integer;
begin
  if FP^ = '\' then
  begin
    LexEscChar;
    L := 0;
  end
  else if REStrLComp(FP, '[:', 2) = 0 then
  begin
    CharNext(FP);
    LexPosixCharClass;
    L := 0;
  end
  else
  begin
    FWChar := GetREChar(FP, L, []);
    FToken := tkChar;
  end;

  CharNext(FP, L);

  if (FToken = tkChar) and (FP^ = '-') then
  begin
    CharNext(FP);
    FStartWChar := GetConvertREChar(FWChar, GetRECompareOptions);

    if FP^ <> ']' then
    begin
      if FP^ = '\' then
      begin
        LexEscChar;
        L := 0;

        if FToken <> tkChar then
          Error(sInvalideBigParRange, '');
      end
      else
      begin
        FWChar := GetREChar(FP, L, []);
        FWChar := GetConvertREChar(FWChar, GetRECompareOptions);
      end;

      if FStartWChar > FWChar then
        Error(sInvalideBigParRange, '');

      CharNext(FP, L);

      FLastWChar := FWChar;

      FToken := tkRangeChar;
    end
    else
    begin
      CharPrev(FP);
      FToken := tkChar;
      Exit;
    end;
  end;
end;

procedure TRELex.LexEscChar;
var
  L, LTagNo: Integer;
begin
  L := 1;
  CharNext(FP);

  if FP^ = #0 then
    Error(sRegExpNotCompleted, GetCompileErrorPos);

  case FP^ of
    'A', 'B', 'E', 'G', 'K', 'Q', 'X', 'R', 'Z', 'b', 'g', 'k', 'z':
      begin
        if FContext = ctNormal then
        begin
          case FP^ of
            'A':
              FToken := tkTHead;
            'B':
              FToken := tkNEWordBoundary;
            'Q':
              begin
                FContext := ctQuote;
                CharNext(FP);
                GetToken(True);
                Exit;
              end;
            'Z':
              FToken := tkTTail;
            'b':
              FToken := tkWordBoundary;
            'k':
              begin
                CharNext(FP);
                case FP^ of
                  '<':
                    LexReference('>');
                  '''':
                    LexReference('''');
                  '{':
                    LexReference('}');
                  '1' .. '9', '+', '-':
                    LexReference(#0000);
                else
                  Error(sGroupNumberIsEmpty);
                end;
                Exit;
              end;
            'R':
              FToken := tkLineBreak;
            'z':
              FToken := tkTTailEnd;
            'K':
              FToken := tkKeepPattern;
            'X':
              FToken := tkCombiningSequence;
            'G':
              FToken := tkGlobalPos;
            'g':
              begin
                CharNext(FP);
                case FP^ of
                  '<':
                    LexGoSub('>');
                  '''':
                    LexGoSub('''');
                  '{':
                    LexReference('}');
                  '1' .. '9', '-':
                    LexReference(#0000);
                else
                  Error(sGroupNumberIsEmpty);
                end;
                Exit;
              end;
          end;
        end
        else
        begin
          FWChar := GetREChar(FP, L, GetRECompareOptions);
          FToken := tkChar;
        end;
      end;
    '1' .. '9':
      begin
        if FContext = ctNormal then
        begin
          LTagNo := GetDigit(L);

          if (LTagNo >= 1) and (LTagNo <= 9) then
          begin
            FMin := LTagNo;
            FToken := tkReference;
          end
          else
          begin
            if FRegExp.FGroups.Count < LTagNo then
            begin
              FWChar := GetOctalDigit(L);
              FToken := tkChar;
            end
            else
            begin
              FMin := LTagNo;
              FToken := tkReference;
            end;
          end;
        end
        else
        begin
          FWChar := GetOctalDigit(L);
          FToken := tkChar;
        end;
      end;
    'p', 'P':
      begin
        if FP^ = 'P' then
          FToken := tkNEProperty
        else
          FToken := tkProperty;

        CharNext(FP);
        if FP^ = '{' then
        begin
          LexProperty(FToken = tkProperty);
          Exit;
        end
        else
        begin
          case FP^ of
            'L':
              FUniCodeProperty := upL;
            'M':
              FUniCodeProperty := upM;
            'N':
              FUniCodeProperty := upN;
            'P':
              FUniCodeProperty := upP;
            'S':
              FUniCodeProperty := upS;
            'Z':
              FUniCodeProperty := upZ;
          else
            Error(sInvalidProperty);
          end;
        end;
      end;
    'D':
      FToken := tkNEDigitChar;
    'H':
{$IFDEF HIsHexDigit}
      FToken := tkNEHexDigitChar;
{$ELSE}
      FToken := tkNEHorizontalSpaceChar;
{$ENDIF}
    'S':
      FToken := tkNESpaceChar;
    'V':
      FToken := tkNEVerticalSpaceChar;
    'W':
      FToken := tkNEWordChar;
    'd':
      FToken := tkDigitChar;
    'h':
{$IFDEF HIsHexDigit}
      FToken := tkHexDigitChar;
{$ELSE}
      FToken := tkHorizontalSpaceChar;
{$ENDIF}
    's':
      FToken := tkSpaceChar;
    'v':
      FToken := tkVerticalSpaceChar;
    'w':
      FToken := tkWordChar;
  else
    begin
      case FP^ of
        'n':
          FWChar := $A;
        'c':
          FWChar := GetControlCode(L);
        'x':
          FWChar := GetHexDigit(L);
        '0':
          FWChar := GetOctalDigit(L);
        't':
          FWChar := 9;
        'r':
          FWChar := $D;
        'f':
          FWChar := $C;
        'a':
          FWChar := 7;
        'e':
          FWChar := $1B;
      else
        FWChar := GetREChar(FP, L, GetRECompareOptions);
      end;
      FToken := tkChar;
    end;
  end;
  CharNext(FP, L);
end;

procedure TRELex.LexGoSub(const LastDelimiter: WideChar);
var
  ATag, L: Integer;
  StartP: PWideChar;
  S: REString;
  IsMinus, IsPlus: Boolean;
begin
  if LastDelimiter = #0000 then
  begin
    if (FP^ = '-') or (FP^ = '+') then
    begin
      IsMinus := FP^ = '-';

      CharNext(FP);

      FToken := tkGoSubRelative;
    end
    else
    begin
      IsMinus := False;
      FToken := tkGoSub;
    end;

    ATag := GetDigit(L);

    if IsMinus then
      FMin := 0 - ATag
    else
      FMin := ATag;

    if L = 0 then
      Error(sGroupNumberIsEmpty);

    CharNext(FP, L);
  end
  else
  begin
    CharNext(FP);

    StartP := FP;

    IsMinus := FP^ = '-';
    IsPlus := FP^ = '+';

    if IsMinus or IsPlus then
    begin
      FToken := tkGoSubRelative;
      CharNext(FP);
    end
    else
      FToken := tkGoSub;

    if IsAnkDigit(FP) then
    begin
      ATag := GetDigit(L);

      if IsMinus then
        FMin := 0 - ATag
      else
        FMin := ATag;

      if L = 0 then
        Error(sGroupNumberIsEmpty);

      CharNext(FP, L);
      if FP^ <> LastDelimiter then
        Error(sNotTerminated);

      CharNext(FP, L);
    end
    else
    begin
      while FP^ <> #0 do
      begin
        if FP^ = LastDelimiter then
        begin
          SetString(S, StartP, FP - StartP);
          FGroupName := S;
          FToken := tkGoSubName;
          CharNext(FP);
          Exit;
        end;
        if UnicodeProp.IsWord(ToUCS4Char(FP)) then
          CharNext(FP)
        else
          Error(sInvalidCharInGroupName);
      end;
      Error(sNoEndOfGroup);
    end;
  end;
end;

procedure TRELex.LexGroupName(const LastDelimiter: WideChar);
var
  S: REString;
  StartP: PWideChar;
begin
  CharNext(FP);

  if (FP^ >= '0') and (FP^ <= '9') then
    Error(sInvalidCharInGroupName);

  StartP := FP;

  while FP^ <> #0 do
  begin
    if FP^ = LastDelimiter then
    begin
      SetString(S, StartP, FP - StartP);
      FGroupName := S;
      CharNext(FP);
      Exit;
    end
    else
    begin
      if not UnicodeProp.IsWord(ToUCS4Char(FP)) then
        Error(sInvalidCharInGroupName);

      CharNext(FP);
    end;
  end;
  Error(sNoEndOfGroup);
end;

procedure TRELex.LexLeftPar;
var
  L: Integer;
  S: REString;
  StartP: PWideChar;
begin
  L := 1;
  if FP^ = '?' then
  begin
    CharNext(FP);
    case FP^ of
      '-':
        begin
          CharNext(FP);
          if not IsAnkDigit(FP) then
          begin
            CharPrev(FP);
            LexOption;
          end
          else
          begin
            FMin := 0 - GetDigit(L);
            CharNext(FP, L);
            if FP^ <> ')' then
              Error(sUnmatchedSmallPar);
            FToken := tkGoSubRelative;
          end;
        end;
      'i', 'm', 'n', 's', 'x', 'w', 'k':
        LexOption;
      '#':
        begin
          CharNext(FP);
          while FP^ <> #0 do
          begin
            if FP^ = ')' then
            begin
              CharNext(FP);
              GetToken(True);
              Exit;
            end;
            CharNext(FP);
          end;
        end;
      '>':
        FToken := tkNoBackTrack;
      ':':
        FToken := tkLPar;
      '''':
        begin
          LexGroupName('''');
          FToken := tkGroupBegin;
          Exit;
        end;
      '<':
        begin
          CharNext(FP);
          if FP^ = '=' then
            FToken := tkBehindMatch
          else if FP^ = '!' then
            FToken := tkBehindNoMatch
          else
          begin
            CharPrev(FP);
            LexGroupName('>');
            FToken := tkGroupBegin;
            Exit;
          end;
        end;
      ')':
        begin
          FToken := tkEmpty;
        end;
      'P':
        begin
          CharNext(FP);
          if FP^ = '<' then
          begin
            LexGroupName('>');
            FToken := tkGroupBegin;
            Exit;
          end
          else if FP^ = '=' then
          begin
            LexGroupName(')');
            FToken := tkNamedReference;
            Exit;
          end
          else if FP^ = '>' then
          begin
            LexGroupName(')');
            FToken := tkGoSubName;
            Exit;
          end
          else
            Error(sGroupNameIsEmpty, '...)');
        end;
      '+':
        begin
          CharNext(FP);
          if IsAnkDigit(FP) then
          begin
            FMin := GetDigit(L);
            CharNext(FP, L);
            if FP^ <> ')' then
              Error(sUnmatchedSmallPar);
            FToken := tkGoSubRelative;
          end
          else
            Error(sGroupNumberIsEmpty);
        end;
      '=':
        FToken := tkAheadMatch;
      '!':
        begin
          CharNext(FP);
          if FP^ = ')' then
            FToken := tkFail
          else
          begin
            FToken := tkAheadNoMatch;
            Exit;
          end;
        end;
      '(':
        begin
          CharNext(FP);
          if IsAnkDigit(FP) then
          begin
            FMin := GetDigit(L);
            CharNext(FP, L);
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end
            else
              Error(sNotRecognized, '...)');
          end
          else if FP^ = '<' then
          begin
            LexGroupName('>');
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end;
          end
          else if FP^ = '''' then
          begin
            LexGroupName('''');
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end;
          end
          else
          begin
            Dec(FP);
            FToken := tkIfMatch;
            Exit;
          end;
        end;
      'R':
        begin
          FMin := 0;
          CharNext(FP);
          if FP^ <> ')' then
            Error(sUnmatchedSmallPar);

          FToken := tkGoSub;
        end;
      '&':
        begin
          CharNext(FP);
          StartP := FP;
          while FP^ <> #0000 do
          begin
            if FP^ = ')' then
            begin
              SetString(S, StartP, FP - StartP);
              CharNext(FP);
              FGroupName := S;
              FToken := tkGoSubName;
              Exit;
            end;
            CharNext(FP);
          end;
          Error(sUnmatchedSmallPar);
        end;
      '0' .. '9':
        begin
          FMin := GetDigit(L);
          CharNext(FP, L);
          if FP^ <> ')' then
            Error(sUnmatchedSmallPar);

          FToken := tkGoSub;
        end;
      '|': // branch reset
        FToken := tkBranchReset;
    else // case
      Error(sNotRecognized);
    end;
  end
  else // if
  begin
    if not(roNamedGroupOnly in FOptions) then
      FToken := tkGroupBegin
    else
      FToken := tkLPar;
    Exit;
  end;
  CharNext(FP, L);
end;

{ .$WARNINGS OFF }
{$WARNINGS ON}

procedure TRELex.LexOption;
var
  IsInclude: Boolean;
  AOption: TREOption;
begin
  FNewOptions := FOptions;
  AOption := roNone;

  while FP^ <> #0 do
  begin
    if FP^ = '-' then
    begin
      IsInclude := False;
      CharNext(FP);
    end
    else
      IsInclude := True;

    case FP^ of
      'i':
        AOption := roIgnoreCase;
      'm':
        AOption := roMultiLine;
      'n':
        AOption := roNamedGroupOnly;
      's':
        AOption := roSingleLine;
      'x':
        AOption := roExtended;
{$IFDEF UseJapaneseOption}
      'w':
        AOption := roIgnoreWidth;
      'k':
        AOption := roIgnoreKana;
{$ENDIF}
    else
      Error(sNotRecognized);
    end;

    if AOption <> roNone then
    begin
      if IsInclude then
        Include(FNewOptions, AOption)
      else
        Exclude(FNewOptions, AOption);

    end;

    CharNext(FP);

    if FP^ = ')' then
    begin
      FToken := tkOption;
      Exit;
    end
    else if FP^ = ':' then
    begin
      FToken := tkLParWithOption;
      Exit;
    end;
  end;
  Error(sOptionNotCompleted);
end;

procedure TRELex.LexPosixCharClass;
{$WARNINGS OFF}
  function GetPosixType(const S: REString): TUnicodeProperty;
  begin
    Result := upAlnum;
    if SameStr(S, 'alnum') then
      Result := upAlnum
    else if SameStr(S, 'alpha') then
      Result := upAlpha
    else if SameStr(S, 'ascii') then
      Result := upAscii
    else if SameStr(S, 'blank') then
      Result := upBlank
    else if SameStr(S, 'cntrl') then
      Result := upCntrl
    else if SameStr(S, 'digit') then
      Result := upDigit
    else if SameStr(S, 'graph') then
      Result := upSpace
    else if SameStr(S, 'lower') then
      Result := upLower
    else if SameStr(S, 'print') then
      Result := upPrint
    else if SameStr(S, 'punct') then
      Result := upPunct
    else if SameStr(S, 'space') then
      Result := upSpace
    else if SameStr(S, 'upper') then
      Result := upUpper
    else if SameStr(S, 'xdigit') then
      Result := upXDigit
    else if SameStr(S, 'word') then
      Result := upWord
    else
    begin
      CharNext(FP);
      Error(Format(sPosixClassUnkown, [S]));
    end;
  end;
{$WARNINGS ON}

var
  IsNegative: Boolean;
  L: Integer;
  P: PWideChar;
  S: REString;
begin
  CharNext(FP);
  if FP^ = '^' then
  begin
    IsNegative := True;
    CharNext(FP);
  end
  else
    IsNegative := False;

  P := FP;

  while P^ <> #0 do
  begin
    if (P^ = ']') or (P^ = #0) then
    begin
      FWChar := GetREChar(FP, L, []);
      FToken := tkChar;
      CharNext(FP, L);
      Exit;
    end
    else if P^ = ':' then
    begin
      SetString(S, FP, P - FP);
      FUniCodeProperty := GetPosixType(S);

      if IsNegative then
        FToken := tkNEPosixBracket
      else
        FToken := tkPosixBracket;

      CharNext(P);
      if P^ <> ']' then
      begin
        FP := P;
        Error(sUnmatchedBigPar);
      end;

      CharNext(P);
      FP := P;

      Break;
    end;
    CharNext(P);
  end;

  if not (FContext in [ctCharClass, ctNegativeCharClass]) then
    Error(sPosixClassSupportedOnlyClass);
end;

procedure TRELex.LexProperty(const CheckNegative: Boolean);
var
  Index: Integer;
  S, S2, PreStr: REString;
  StartP: PWideChar;
begin
  CharNext(FP);
  if CheckNegative and (FP^ = '^') then
  begin
    FToken := tkNEProperty;
    CharNext(FP);
  end;

  StartP := FP;

  while FP^ <> #0 do
  begin
    if FP^ = '}' then
    begin
      SetString(S, StartP, FP - StartP);
      // S := PreStr + S;
      if PropertyNames.Find(S, Index) then
        FUniCodeProperty := TUnicodeProperty(PropertyNames.Objects[Index])
      else
      begin
        if UpperCase(Copy(S, 1, 2)) = 'IS' then
        begin
          S2 := 'In' + Copy(S, 3, MaxInt);
          if PropertyNames.Find(S2, Index) then
            FUniCodeProperty := TUnicodeProperty(PropertyNames.Objects[Index])
          else
            Error(Format(sPropertyUnknown, [S]));
        end
        else
          Error(Format(sPropertyUnknown, [S]));
      end;

      CharNext(FP);
      Exit;
    end
    else if (FP^ = '-') or (FP^ = '_') or IsAnkSpace(FP) then
    begin
      SetString(S, StartP, FP - StartP);
      PreStr := PreStr + S;
      CharNext(FP);
      StartP := FP;
    end
    else if IsAnkWord(FP) or (FP^ = '&') then
      CharNext(FP)
    else
      Error(sInvalidProperty);
  end;
  Error(sMissingRightBrace);
end;

procedure TRELex.LexReference(const LastDelimiter: WideChar);
var
  L: Integer;
  StartP: PWideChar;
  S: REString;
  IsMinus: Boolean;
begin
  if LastDelimiter = #0000 then
  begin
    if FP^ = '-' then
    begin
      CharNext(FP);

      FToken := tkReferenceRelative;
    end
    else
      FToken := tkReference;

    FMin := GetDigit(L);

    if L = 0 then
      Error(sGroupNumberIsEmpty);

    CharNext(FP, L);
  end
  else
  begin
    CharNext(FP);

    StartP := FP;

    IsMinus := FP^ = '-';

    if IsMinus then
    begin
      FToken := tkReferenceRelative;
      CharNext(FP);
    end
    else
      FToken := tkReference;

    if IsAnkDigit(FP) then
    begin
      FMin := GetDigit(L);

      if L = 0 then
        Error(sGroupNumberIsEmpty);

      CharNext(FP, L);
      if FP^ <> LastDelimiter then
        Error(sNotTerminated);

      CharNext(FP, L);
    end
    else
    begin
      if LastDelimiter = '>' then
      begin
        while FP^ <> #0 do
        begin
          if (FP^ = '>') then
          begin
            SetString(S, StartP, FP - StartP);
            FGroupName := S;
            FToken := tkNamedReference;
            if FP^ = '>' then
            begin
              CharNext(FP);
              Exit;
            end;
          end;
          if UnicodeProp.IsWord(ToUCS4Char(FP)) then
            CharNext(FP)
          else
            Error(sInvalidCharInGroupName);
        end;
      end
      else
      begin
        while FP^ <> #0 do
        begin
          if FP^ = LastDelimiter then
          begin
            SetString(S, StartP, FP - StartP);
            FGroupName := S;
            FToken := tkNamedReference;
            CharNext(FP);
            Exit;
          end;
          if UnicodeProp.IsWord(ToUCS4Char(FP)) then
            CharNext(FP)
          else
            Error(sInvalidCharInGroupName);
        end;
      end;
      Error(sNoEndOfGroup);
    end;
  end;
end;

procedure TRELex.PopOptions;
var
  AOptions: PREOptions;
begin
  if FOptionList.Count = 0 then
    Exit;
  AOptions := PREOptions(FOptionList[FOptionList.Count - 1]);
  FOptions := AOptions^;
  FOptionList.Delete(FOptionList.Count - 1);
  Dispose(AOptions);
end;

procedure TRELex.PushOptions;
var
  AOptions: PREOptions;
begin
  New(AOptions);
  AOptions^ := FOptions;
  FOptionList.Add(AOptions);
end;

procedure TRELex.PushToken;
var
  I: Integer;
begin
  case FPrevCount of
    0:
      I := 2;
    1:
      I := 0;
  else
    I := 1;
  end;

  if not FPrevLex[I].FStored then
    Exit;

  FPrevLex[I].FStored := False;

  FToken := FPrevLex[I].FToken;
  FOptions := FPrevLex[I].FOptions;
  FNewOptions := FPrevLex[I].FNewOptions;
  FP := FPrevLex[I].FP;
  FTokenStartP := FPrevLex[I].FTokenStartP;
  FTopP := FPrevLex[I].FTopP;
  FWChar := FPrevLex[I].FWChar;
  FStartWChar := FPrevLex[I].FStartWChar;
  FLastWChar := FPrevLex[I].FLastWChar;
  FMin := FPrevLex[I].FMin;
  FMax := FPrevLex[I].FMax;
  FLevel := FPrevLex[I].FLevel;
  FContext := FPrevLex[I].FContext;
  FUniCodeProperty := FPrevLex[I].FUniCodeProperty;
  FConvert := FPrevLex[I].FConvert;
  FGroupName := FPrevLex[I].FGroupName;
  FOptionList.Assign(FPrevLex[I].FOptionList);
  FIsQuote := FPrevLex[I].FIsQuote;

  case FPrevCount of
    0:
      FPrevCount := 2;
    1:
      FPrevCount := 0;
    2:
      FPrevCount := 1;
  end;

end;

procedure TRELex.Save;
var
  I: Integer;
begin
  case FPrevCount of
    0:
      I := 0;
    1:
      I := 1;
  else
    I := 2;
  end;

  FPrevLex[I].FStored := True;

  FPrevLex[I].FToken := FToken;
  FPrevLex[I].FOptions := FOptions;
  FPrevLex[I].FNewOptions := FNewOptions;
  FPrevLex[I].FP := FP;
  FPrevLex[I].FTokenStartP := FTokenStartP;
  FPrevLex[I].FTopP := FTopP;
  FPrevLex[I].FWChar := FWChar;
  FPrevLex[I].FStartWChar := FStartWChar;
  FPrevLex[I].FLastWChar := FLastWChar;
  FPrevLex[I].FMin := FMin;
  FPrevLex[I].FMax := FMax;
  FPrevLex[I].FLevel := FLevel;
  FPrevLex[I].FContext := FContext;
  FPrevLex[I].FUniCodeProperty := FUniCodeProperty;
  FPrevLex[I].FConvert := FConvert;
  FPrevLex[I].FGroupName := FGroupName;
  FPrevLex[I].FOptionList.Assign(FOptionList);

  case FPrevCount of
    0:
      FPrevCount := 1;
    1:
      FPrevCount := 2;
    2:
      FPrevCount := 0;
  end;
end;

procedure TRELex.SkipWhiteSpace;
begin
  while (FP^ <> #0) and ((FP^ = ' ') or (FP^ = #9) or (FP^ = #10) or
    (FP^ = #13)) do
    Inc(FP);
end;

procedure TRELex.UpdateOptions;
begin
  FOptions := FNewOptions;
end;

{ TREParser }

constructor TREParser.Create(ARegExp: TSkRegExp; const Expression: REString);
begin
  inherited Create;
  FRegExp := ARegExp;
  FLex := TRELex.Create(ARegExp, Expression);
  FGroupStack := TStack.Create;
  FReferenceErrorList := TREStringList.Create;
  FGoSubErrorList := TREStringList.Create;
end;

destructor TREParser.Destroy;
var
  I: Integer;
  P: PReferenceErrorRec;
begin
  for I := 0 to FReferenceErrorList.Count - 1 do
  begin
    P := PReferenceErrorRec(FReferenceErrorList.Objects[I]);
    Dispose(P);
  end;
  FReferenceErrorList.Free;

  for I := 0 to FGoSubErrorList.Count - 1 do
  begin
    P := PReferenceErrorRec(FGoSubErrorList.Objects[I]);
    Dispose(P);
  end;
  FGoSubErrorList.Free;

  FLex.Free;
  FGroupStack.Free;
  inherited;
end;

function TREParser.Factor: TRECode;

  procedure SetMinMatch(BinCode: TRECode);
  begin
    if not(BinCode is TREBinCode) then
      FLex.Error(sQuestPosInaccurate);

    (BinCode as TREBinCode).MatchKind := lkReluctant;
  end;

  procedure SetMaxMatch(BinCode: TRECode);
  begin
    if not(BinCode is TREBinCode) then
      FLex.Error(sQuestPosInaccurate);

    (BinCode as TREBinCode).MatchKind := lkPossessive;
  end;

  procedure CheckAheadMatch(ACode: TRECode);
  begin
    if ACode is TREBinCode then
    begin
      with ACode as TREBinCode do
      begin
        if (FOp = opAheadMatch) or (FOp = opAheadNoMatch) then
          FLex.Error(sLoopOfMatchAheadNotSpecified);
      end;
    end;
  end;

  procedure CheckEmptyLoop(ACode: TRECode);
  begin
    if (ACode is TREBoundaryCode) or (ACode is TRETextHeadCode) or
      (ACode is TRETextTailCode) or (ACode is TRETextEndCode) then
      FLex.Error(sLoopOfLengthZeroCannotSpecified);
  end;

var
  LMin, LMax: Integer;
  LToken: TREToken;
  LMatchKind: TRELoopKind;
begin
  Result := Primay;

  if FLex.Token in [tkStar, tkPlus, tkQuest, tkBound] then
  begin
    LToken := FLex.Token;
    LMin := FLex.Min;
    LMax := FLex.Max;

    CheckAheadMatch(Result);
    CheckEmptyLoop(Result);

    FLex.GetToken;

    if FLex.Token = tkQuest then
    begin
      LMatchKind := lkReluctant;
      FLex.GetToken;
    end
    else if FLex.Token = tkPlus then
    begin
      LMatchKind := lkPossessive;
      FLex.GetToken;
    end
    else
      LMatchKind := lkGreedy;

    case LToken of
      tkStar:
        begin
          if (Result is TREAnyCharCode) and (LMatchKind = lkGreedy) then
          begin
            Result := NewBinCode(opStar, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := lkAny;
          end
          else if (Result is TRECombiningSequence) and (LMatchKind = lkGreedy)
            and (LMatchKind = lkGreedy) then
          begin
            Result := NewBinCode(opStar, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := lkCombiningSequence;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
        end;
      tkPlus:
        begin
          if not(Result is TREBinCode) and (LMatchKind = lkGreedy) then
          begin
            Result := NewBinCode(opPlus, Result, nil, 1, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, 1, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
        end;
      tkQuest:
        begin
          // Result := NewBinCode(opLoop, Result, nil, 0, 1);
          Result := NewBinCode(opQuest, Result, nil);
          TREBinCode(Result).MatchKind := LMatchKind;
        end;
      tkBound:
        begin
          if not(Result is TREBinCode) and (LMatchKind = lkGreedy) then
          begin
            Result := NewBinCode(opBound, Result, nil, LMin, LMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, LMin, LMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
        end;
    end;
  end;
end;

function TREParser.NewBinCode(AOperator: TREOperator; ALeft, ARight: TRECode;
  AMin, AMax: Integer): TRECode;
begin
  Result := TREBinCode.Create(FRegExp, AOperator, ALeft, ARight, AMin, AMax);
  FRegExp.FBinCodeList.Add(Result);
end;

function TREParser.NewCharClassCode(ANegative: Boolean): TRECode;
var
  CharClass: TRECharClassCode;
begin
  CharClass := TRECharClassCode.Create(FRegExp, ANegative, FLex.Options);
  FRegExp.FCodeList.Add(CharClass);
  Result := CharClass;

  FLex.GetToken;
  case FLex.Token of
    tkChar:
      CharClass.Add(FLex.FWChar);
    tkRangeChar:
      CharClass.Add(FLex.StartWChar, FLex.LastWChar);
    tkWordChar:
      CharClass.Add(TREWordCharCode.Create(FRegExp, False));
    tkDigitChar:
      CharClass.Add(TREDigitCharCode.Create(FRegExp, False));
    tkSpaceChar:
      CharClass.Add(TRESpaceCharCode.Create(FRegExp, False));
    tkNEWordChar:
      CharClass.Add(TREWordCharCode.Create(FRegExp, True));
    tkNEDigitChar:
      CharClass.Add(TREDigitCharCode.Create(FRegExp, True));
    tkNESpaceChar:
      CharClass.Add(TRESpaceCharCode.Create(FRegExp, True));
    tkPosixBracket:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, False));
    tkNEPosixBracket:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, True));
    tkProperty:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, False));
    tkNEProperty:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, True));
    tkHexDigitChar:
      CharClass.Add(TREHexDigitCharCode.Create(FRegExp, False));
    tkNEHexDigitChar:
      CharClass.Add(TREHexDigitCharCode.Create(FRegExp, True));
    tkHorizontalSpaceChar:
      CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, False));
    tkNEHorizontalSpaceChar:
      CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, True));
    tkVerticalSpaceChar:
      CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, False));
    tkNEVerticalSpaceChar:
      CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, True));
  else
    FLex.Error(sInvalidCharactorClass);
  end;

  FLex.GetToken;
  while (FLex.Token = tkRangeChar) or (FLex.Token = tkChar) or
    (FLex.Token = tkWordChar) or (FLex.Token = tkNEWordChar) or
    (FLex.Token = tkDigitChar) or (FLex.Token = tkNEDigitChar) or
    (FLex.Token = tkSpaceChar) or (FLex.Token = tkNESpaceChar) or
    (FLex.Token = tkPosixBracket) or (FLex.Token = tkNEPosixBracket) or
    (FLex.Token = tkProperty) or (FLex.Token = tkNEProperty) or
    (FLex.Token = tkHexDigitChar) or (FLex.Token = tkNEHexDigitChar) or
    (FLex.Token = tkHorizontalSpaceChar) or
    (FLex.Token = tkNEHorizontalSpaceChar) or (FLex.Token = tkVerticalSpaceChar)
    or (FLex.Token = tkNEVerticalSpaceChar) do
  begin
    case FLex.Token of
      tkChar:
        CharClass.Add(FLex.WChar);
      tkRangeChar:
        CharClass.Add(FLex.StartWChar, FLex.LastWChar);
      tkWordChar:
        CharClass.Add(TREWordCharCode.Create(FRegExp, False));
      tkDigitChar:
        CharClass.Add(TREDigitCharCode.Create(FRegExp, False));
      tkSpaceChar:
        CharClass.Add(TRESpaceCharCode.Create(FRegExp, False));
      tkNEWordChar:
        CharClass.Add(TREWordCharCode.Create(FRegExp, True));
      tkNEDigitChar:
        CharClass.Add(TREDigitCharCode.Create(FRegExp, True));
      tkNESpaceChar:
        CharClass.Add(TRESpaceCharCode.Create(FRegExp, True));
      tkPosixBracket:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, False));
      tkNEPosixBracket:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, True));
      tkProperty:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, False));
      tkNEProperty:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, True));
      tkHexDigitChar:
        CharClass.Add(TREHexDigitCharCode.Create(FRegExp, False));
      tkNEHexDigitChar:
        CharClass.Add(TREHexDigitCharCode.Create(FRegExp, True));
      tkHorizontalSpaceChar:
        CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, False));
      tkNEHorizontalSpaceChar:
        CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, True));
      tkVerticalSpaceChar:
        CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, False));
      tkNEVerticalSpaceChar:
        CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, True));
    else
      FLex.Error(sInvalidCharactorClass);
    end;
    FLex.GetToken;
  end;
  if FLex.Token <> tkCharClassEnd then
    FLex.Error(sUnmatchedBigPar);

  CharClass.Rebuild;
end;

procedure TREParser.Parse;
var
  I, P: Integer;
  RefError: PReferenceErrorRec;
begin
  FGroupCount := 0;
  FCurrentGroup := 0;
  FGroupLevel := 0;

  FLex.GetToken;
  FRegExp.FCode := RegExpr;
  if FLex.Token <> tkEnd then
  begin
    if FLex.Token = tkRPar then
      FLex.Error(sUnmatchedSmallPar)
    else
      FLex.Error(sRegExpNotCompleted);
  end;

  for I := 0 to FReferenceErrorList.Count - 1 do
  begin
    RefError := PReferenceErrorRec(FReferenceErrorList.Objects[I]);
    if FReferenceErrorList[I] <> '' then
    begin
      if FRegExp.FGroups.IndexOfName(FReferenceErrorList[I]) = -1 then
      begin
        FLex.Error(Format(sInvalidGroupName, [FReferenceErrorList[I]]),
          RefError.ErrorPos);
      end
      else
      begin
        TRENamedReferenceCode(RefError.AObject).SetGroupIndexArray(
          FRegExp.FGroups.EnumIndexOfName(FReferenceErrorList[I]));
      end;
    end
    else
    begin
      if RefError.GroupIndex > FGroupCount then
        FLex.Error(Format(sInvalidGroupNumber, [RefError.GroupIndex]),
          RefError.ErrorPos)
      else if (0 - RefError.GroupIndex + 1 > FGroupCount) then
        FLex.Error(Format(sInvalidGroupNumber, [RefError.GroupIndex]),
          RefError.ErrorPos)
    end;
  end;

  for I := 0 to FGoSubErrorList.Count - 1 do
  begin
    RefError := PReferenceErrorRec(FGoSubErrorList.Objects[I]);
    if FGoSubErrorList[I] <> '' then
    begin
      P := FRegExp.FGroups.IndexOfName(FGoSubErrorList[I]);
      if P = -1 then
        FLex.Error(Format(sInvalidGroupName, [FGoSubErrorList[I]]),
          RefError.ErrorPos);

      if FRegExp.FGroups.IsDuplicateGroupName(FGoSubErrorList[I]) then
        FLex.Error(Format(sCannotCallMultipleDefineGroupName,
          [FGoSubErrorList[I]]), RefError.ErrorPos);
    end
    else
    begin
      if RefError.GroupIndex > FGroupCount then
        FLex.Error(Format(sInvalidGroupNumber, [RefError.GroupIndex]),
          RefError.ErrorPos)
      else if (0 - RefError.GroupIndex + 1 > FGroupCount) then
        FLex.Error(Format(sInvalidGroupNumber, [RefError.GroupIndex]),
          RefError.ErrorPos)
    end;
  end;
end;

function TREParser.Primay: TRECode;

  function CheckBehindMatchSub(ACode: TRECode; IsMatch: Boolean;
    var ErrCode: Integer): Boolean;
  var
    SubCode: TREBinCode;
  begin
    if (ACode is TREBinCode) then
    begin
      SubCode := (ACode as TREBinCode);
      if SubCode.FOp = opUnion then
      begin
        Result := False;
        ErrCode := 1;
      end
      else if not IsMatch and (SubCode.FOp = opGroup) then
      begin
        Result := False;
        ErrCode := 2;
      end
      else
      begin
        if (SubCode.Left <> nil) and not CheckBehindMatchSub(SubCode.Left,
          IsMatch, ErrCode) then
        begin
          Result := False;
          Exit;
        end;
        if (SubCode.Right <> nil) and not CheckBehindMatchSub(SubCode.Right,
          IsMatch, ErrCode) then
        begin
          Result := False;
          Exit;
        end;
        Result := True;
      end;
    end
    else
      Result := True;
  end;

  procedure CheckBehindMatch(ACode: TRECode; IsMatch: Boolean);
  var
    ErrCode: Integer;
    Ret: Boolean;
  begin
    ErrCode := 0;

    if (ACode is TREBinCode) then
    begin
      if (ACode as TREBinCode).FOp = opUnion then
        Ret := True
      else
        Ret := CheckBehindMatchSub(ACode, IsMatch, ErrCode);
    end
    else
      Ret := True;

    if not Ret then
      if ErrCode = 1 then
        FLex.Error(sBehindMatchNotVariableLength)
      else
        FLex.Error(sBehindMatchNotGroup);
  end;

type
  TMatchLengthRec = record
    AMin, AMax: Integer;
    HasQuest: Boolean;
  end;

  PMatchLengthRec = ^TMatchLengthRec;

  function GetMatchLengthSub(ACode: TRECode;
    var MatchLen: TMatchLengthRec): Integer;
  var
    SubCode: TRECode;
    BinCode: TREBinCode;
    L, R: Integer;
  begin
    Result := 0;
    SubCode := ACode;

    if (SubCode is TREBinCode) then
    begin
      BinCode := (SubCode as TREBinCode);

      if BinCode.Op = opGroup then
        Result := Result + GetMatchLengthSub(BinCode.Left, MatchLen)
      else if BinCode.Op = opConcat then
      begin
        Result := Result + GetMatchLengthSub(BinCode.Left, MatchLen);
        Result := Result + GetMatchLengthSub(BinCode.Right, MatchLen);
      end
      else if BinCode.Op = opUnion then
      begin
        L := GetMatchLengthSub(BinCode.Left, MatchLen);
        R := GetMatchLengthSub(BinCode.Right, MatchLen);
        if L > R then
        begin
          MatchLen.AMax := Max(MatchLen.AMax, L);
          if not MatchLen.HasQuest then
            MatchLen.AMin := Max(MatchLen.AMin, R);;
        end
        else
        begin
          MatchLen.AMax := Max(MatchLen.AMax, R);
          if not MatchLen.HasQuest then
            MatchLen.AMin := Max(MatchLen.AMin, L);;
        end;
      end
      else if BinCode.Op = opQuest then
      begin
        MatchLen.AMin := Min(MatchLen.AMin, 0);
        MatchLen.AMax := Max(MatchLen.AMax, GetMatchLengthSub(BinCode.Left,
          MatchLen));
      end;
    end
    else if ACode.CharLength <> 0 then
    begin
      if ACode is TRECharCode then
        Inc(Result)
      else if ACode is TRELiteralCode then
        Inc(Result, (ACode as TRELiteralCode).Length);
    end;
  end;

  procedure GetMatchLength(ACode: TRECode; var MatchLen: TMatchLengthRec);
  begin
    MatchLen.AMin := 0;
    MatchLen.AMax := 0;
    MatchLen.HasQuest := False;

    if ACode is TRELiteralCode then
    begin
      MatchLen.AMax := GetMatchLengthSub(ACode, MatchLen);
      MatchLen.AMin := MatchLen.AMax;
    end
    else
    begin
      if GetMatchLengthSub(ACode, MatchLen) = 1 then
      begin
        MatchLen.AMin := 1;
        MatchLen.AMax := 1;
      end;
    end;
  end;

  procedure CheckRecursion(SubCode: TRECode);
  begin
    if SubCode is TREBinCode then
    begin
      if (SubCode as TREBinCode).Op = opGoSub then
        FLex.Error(sNeverEndingRecursion)
      else if (SubCode as TREBinCode).Op = opUnion then
      begin
        CheckRecursion((SubCode as TREBinCode).Left);
        CheckRecursion((SubCode as TREBinCode).Right);
      end;
    end
  end;

// 10進表記の数値を8進数に変換。あくまでも10進表記
  function ToOctal(const ADigit: Integer): Integer;
  var
    I: Integer;
    S: REString;
  begin
    Result := 0;
    S := IntToStr(ADigit);

    for I := 1 to Length(S) do
    begin
      case S[I] of
        '0' .. '7':
          Result := (Result shl 3) + (Integer(S[I]) - Integer('0'));
      else
        Break;
      end;
    end;
  end;

var
  LGroupNo, LGroupBuffer: Integer;
  LGroupName: REString;
  SubCode, CondCode: TRECode;
  LMatchLen: TMatchLengthRec;
  RefError: PReferenceErrorRec;
  LGroupArray: TIntDynArray;

  LOptions: TREOptions;
  LWChar: UCS4Char;
  LConvert: Boolean;
  Str: UCS4String;
  Len: Integer;
begin
  Result := nil;

  case FLex.Token of
    tkChar:
      begin
        Len := 1;
        LWChar := FLex.WChar;
        LOptions := FLex.Options;
        LConvert := FLex.Convert;

        SetLength(Str, Len);
        Str[Len - 1] := FLex.FWChar;
        Inc(Len);

        FLex.GetToken;
        while (FLex.Token = tkChar) and (FLex.Options = LOptions) do
        begin
          FLex.GetToken;
          if FLex.Token in [tkQuest, tkStar, tkBound, tkPlus] then
          begin
            FLex.PushToken;
            Break;
          end;

          FLex.PushToken;

          SetLength(Str, Len);
          Str[Len - 1] := FLex.FWChar;
          Inc(Len);
          FLex.GetToken;
        end;

        if Length(Str) > 1 then
          Result := TRELiteralCode.Create(FRegExp, Str, FLex.Options)
        else
          Result := TRECharCode.Create(FRegExp, LWChar, LOptions, LConvert);

        FRegExp.FCodeList.Add(Result);
      end;
    tkLHead:
      begin
        Result := TRECharCode.Create(FRegExp, FLex.WChar, FLex.Options,
          FLex.Convert);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkLTail:
      begin
        Result := TRECharCode.Create(FRegExp, FLex.WChar, FLex.Options,
          FLex.Convert);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkDot:
      begin
        Result := TREAnyCharCode.Create(FRegExp, FLex.Options);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkWordChar, tkNEWordChar:
      begin
        Result := TREWordCharCode.Create(FRegExp, FLex.Token = tkNEWordChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkDigitChar, tkNEDigitChar:
      begin
        Result := TREDigitCharCode.Create(FRegExp, FLex.Token = tkNEDigitChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkHexDigitChar, tkNEHexDigitChar:
      begin
        Result := TREHexDigitCharCode.Create(FRegExp,
          FLex.Token = tkNEHexDigitChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkSpaceChar, tkNESpaceChar:
      begin
        Result := TRESpaceCharCode.Create(FRegExp, FLex.Token = tkNESpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkCharClassFirst, tkNegativeCharClassFirst:
      begin
        Result := NewCharClassCode(FLex.Token = tkNegativeCharClassFirst);
        FLex.GetToken;
      end;
    tkTHead:
      begin
        Result := TRETextHeadCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkTTail:
      begin
        Result := TRETextTailCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkTTailEnd:
      begin
        Result := TRETextEndCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkReference, tkReferenceRelative:
      begin
        if FLex.Token = tkReference then
        begin
          LGroupNo := FLex.Min;
          if LGroupNo > FGroupCount then
          begin
            New(RefError);
            LGroupNo := FLex.Min;
            RefError.AObject := nil;
            RefError.GroupIndex := LGroupNo;
            RefError.ErrorPos := FLex.GetCompileErrorPos;
            FReferenceErrorList.AddObject('', TObject(RefError));
          end;
        end
        else
        begin
          if FGroupCount < FLex.Min then
            FLex.Error(Format(sInvalidGroupNumber, [0 - FLex.Min]));

          LGroupNo := FGroupCount - FLex.Min + 1;
        end;

        Result := TREReferenceCode.Create(FRegExp, LGroupNo, FLex.Options);
        FRegExp.FCodeList.Add(Result);

        FLex.GetToken;
      end;
    tkNamedReference:
      begin
        LGroupArray := FRegExp.FGroups.EnumIndexOfName(FLex.GroupName);
        if Length(LGroupArray) = 0 then
        begin
          New(RefError);
          RefError.GroupIndex := -1;
          RefError.AObject := nil;
          RefError.ErrorPos := FLex.GetCompileErrorPos;
          FReferenceErrorList.AddObject(FLex.GroupName, TObject(RefError));
        end;

        if (FCurrentGroup > 0) then
        begin
          while LGroupArray[0] >= FCurrentGroup do
            LGroupArray := Copy(LGroupArray, 1, MaxInt);
        end;

        Result := TRENamedReferenceCode.Create(FRegExp, FLex.GroupName,
          LGroupArray, FLex.Options);

        if Length(LGroupArray) = 0 then
          PReferenceErrorRec(
            FReferenceErrorList.Objects[FReferenceErrorList.Count - 1]).AObject := Result;

        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkWordBoundary, tkNEWordBoundary:
      begin
        Result := TREBoundaryCode.Create(FRegExp,
          FLex.Token = tkNEWordBoundary);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkHorizontalSpaceChar, tkNEHorizontalSpaceChar:
      begin
        Result := TREHorizontalSpaceCharCode.Create(FRegExp,
          FLex.Token = tkNEHorizontalSpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkVerticalSpaceChar, tkNEVerticalSpaceChar:
      begin
        Result := TREVerticalSpaceCharCode.Create(FRegExp,
          FLex.Token = tkNEVerticalSpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkLineBreak:
      begin
        Result := TRELineBreakCharCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkGroupBegin:
      begin
        FLex.PushOptions;

        Inc(FGroupCount);
        Inc(FGroupLevel);
        FGroupStack.Push(Pointer(FCurrentGroup));
        FCurrentGroup := FGroupCount;

        LGroupName := FLex.FGroupName;

        if FGroupCount > FRegExp.FGroups.Count - 1 then
          LGroupNo := FRegExp.FGroups.Add(FLex.GroupName, -1, -1)
        else
          LGroupNo := FGroupCount;

        FLex.GetToken;
        Result := NewBinCode(opGroup, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        FCurrentGroup := Integer(FGroupStack.Pop);

        Dec(FGroupLevel);

        if FHasRecursion then
          CheckRecursion(Result);

        (Result as TREBinCode).GroupIndex := LGroupNo;
        if LGroupName <> '' then
          (Result as TREBinCode).GroupName := LGroupName;
        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkLPar:
      begin
        FLex.PushOptions;

        FLex.GetToken;
        Result := RegExpr;
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkLParWithOption:
      begin
        FLex.PushOptions;
        FLex.UpdateOptions;

        FLex.GetToken;
        Result := RegExpr;
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkOption:
      begin
        FLex.UpdateOptions;
        if FGroupLevel = 0 then
          FRegExp.Options := FLex.Options;
        FLex.GetToken;
        Result := RegExpr;
      end;
    tkProperty:
      begin
        Result := TREPropertyCode.Create(FRegExp, FLex.UnicodeProperty, False);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkNEProperty:
      begin
        Result := TREPropertyCode.Create(FRegExp, FLex.UnicodeProperty, True);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkNoBackTrack:
      begin
        FLex.GetToken;
        Result := NewBinCode(opNoBackTrack, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkKeepPattern:
      begin
        FLex.GetToken;
        Result := NewBinCode(opKeepPattern, RegExpr, nil);
      end;
    tkFail:
      begin
        FLex.GetToken;
        Result := NewBinCode(opFail, RegExpr, nil);
      end;
    tkCombiningSequence:
      begin
        Result := TRECombiningSequence.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkGlobalPos:
      begin
        Result := TREGlobalPosCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkGoSub, tkGoSubRelative:
      begin
        if FLex.Token = tkGoSub then
        begin
          LGroupNo := FLex.Min;
          if LGroupNo > FGroupCount then
          begin
            New(RefError);
            RefError.IsRelative := False;
            RefError.GroupIndex := FLex.Min;
            RefError.AObject := nil;
            RefError.ErrorPos := FLex.GetCompileErrorPos;
            FReferenceErrorList.AddObject('', TObject(RefError));
          end;
        end
        else
        begin
          LGroupBuffer := FLex.Min;

          if LGroupBuffer < 0 then
            LGroupNo := FGroupCount - LGroupBuffer - 1
          else
            LGroupNo := FGroupCount + LGroupBuffer;

          if LGroupNo < 1 then
            FRegExp.Error(Format(sRangeOverGroupNumber, [LGroupBuffer]));

          if LGroupNo > FGroupCount then
          begin
            New(RefError);
            RefError.IsRelative := True;
            RefError.RelativeGourpIndex := FLex.Min;
            RefError.AObject := nil;
            RefError.ErrorPos := FLex.GetCompileErrorPos;
            FReferenceErrorList.AddObject('', TObject(RefError));
          end;

          if LGroupNo < 0 then
            LGroupNo := FGroupCount - LGroupNo - 1
          else
            LGroupNo := FGroupCount + LGroupNo;
        end;

        Result := NewBinCode(opGoSub, nil, nil);
        (Result as TREBinCode).GroupIndex := LGroupNo;

        FHasRecursion := not FHasRecursion and (LGroupNo = FGroupLevel);

        (Result as TREBinCode).HasRecursion := FHasRecursion;

        FLex.GetToken;
      end;
    tkGoSubName:
      begin
        LGroupNo := FRegExp.FGroups.IndexOfName(FLex.GroupName);
        if LGroupNo = -1 then
        begin
          New(RefError);
          RefError.GroupIndex := -1;
          RefError.AObject := nil;
          RefError.ErrorPos := FLex.GetCompileErrorPos;
          FGoSubErrorList.AddObject(FLex.GroupName, TObject(RefError));
        end;

        Result := NewBinCode(opGoSub, nil, nil);

        (Result as TREBinCode).GroupName := FLex.GroupName;
        (Result as TREBinCode).GroupIndex := LGroupNo;

        if LGroupNo <> -1 then
          FHasRecursion := LGroupNo <= FGroupLevel
        else
          FHasRecursion := False;

        (Result as TREBinCode).HasRecursion := FHasRecursion;
        FLex.GetToken;
      end;
    tkAheadMatch:
      begin
        FLex.GetToken;
        Result := NewBinCode(opAheadMatch, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkAheadNoMatch:
      begin
        FLex.GetToken;
        Result := NewBinCode(opAheadNoMatch, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkBehindMatch:
      begin
        FLex.GetToken;
        SubCode := RegExpr;
        Result := NewBinCode(opBehindMatch, SubCode, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        CheckBehindMatch(SubCode, True);

        GetMatchLength(SubCode, LMatchLen);

        (Result as TREBinCode).FMax := LMatchLen.AMax;
        (Result as TREBinCode).FMin := LMatchLen.AMin;
        FLex.GetToken;
      end;
    tkBehindNoMatch:
      begin
        FLex.GetToken;
        SubCode := RegExpr;
        Result := NewBinCode(opBehindNoMatch, SubCode, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        CheckBehindMatch(SubCode, False);
        CheckBehindMatch(SubCode, True);

        GetMatchLength(SubCode, LMatchLen);
        (Result as TREBinCode).FMax := LMatchLen.AMax;
        (Result as TREBinCode).FMin := LMatchLen.AMin;
        FLex.GetToken;
      end;
    tkIfMatchRef:
      begin
        FLex.PushOptions;

        if FLex.Min = 0 then
        begin
          CondCode := TREIfThenNamedReferenceCode.Create(FRegExp, FLex.GroupName);
          FRegExp.FCodeList.Add(CondCode);
        end
        else
        begin
          CondCode := TREIfThenReferenceCode.Create(FRegExp, FLex.Min);
          FRegExp.FCodeList.Add(CondCode);
        end;

        FLex.GetToken;
        Result := Term;
        if FLex.Token = tkUnion then
        begin
          FLex.GetToken;
          SubCode := Term;
        end
        else
          SubCode := nil;

        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        Result := NewBinCode(opIfThen, Result, SubCode);
        Result := NewBinCode(opIfMatch, CondCode, Result);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkIfMatch:
      begin
        FLex.PushOptions;

        FLex.GetToken;
        CondCode := Primay;

        if (CondCode is TREBinCode) and
            not((CondCode as TREBinCode).Op in [opAheadMatch, opAheadNoMatch,
            opBehindMatch, opBehindNoMatch]) then
          FLex.Error(sInvalideCondition);

        Result := Term;
        if FLex.Token = tkUnion then
        begin
          FLex.GetToken;
          SubCode := Term;
        end
        else
          SubCode := nil;

        if FLex.Token <> tkRPar then
          FLex.Error(sContainsTooManyBranches);

        Result := NewBinCode(opIfThen, Result, SubCode);
        Result := NewBinCode(opIfMatch, CondCode, Result);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkBranchReset:
      begin
        FLex.PushOptions;
        LGroupNo := FGroupCount;
        LGroupBuffer := LGroupNo;

        FLex.GetToken;

        Result := Term;
        while FLex.Token = tkUnion do
        begin
          LGroupBuffer := FGroupCount;
          FGroupCount := LGroupNo;
          FLex.GetToken;
          Result := NewBinCode(opUnion, Result, Term);
        end;

        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        FGroupLevel := LGroupBuffer;

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkEmpty:
      begin
        Result := NewBinCode(opEmply, nil, nil);
        FLex.GetToken;
      end;
  else
    FLex.Error(sNotRecognized);
  end;
end;

function TREParser.RegExpr: TRECode;
begin
  Result := Term;
  while FLex.Token = tkUnion do
  begin
    FLex.GetToken;
    Result := NewBinCode(opUnion, Result, Term);
  end;
end;

function TREParser.Term: TRECode;
begin
  if (FLex.Token = tkUnion) or (FLex.Token = tkRPar) or
    (FLex.Token = tkEnd) then
    Result := NewBinCode(opEmply, nil, nil)
  else
  begin
    Result := Factor;
    while (FLex.Token <> tkUnion) and (FLex.Token <> tkRPar) and
      (FLex.Token <> tkEnd) do
      Result := NewBinCode(opConcat, Result, Factor);
  end;
end;

{ TRENFAState }

{$IFDEF DEBUG}

function TRENFAState.GetString: REString;
var
  MatchTypeStr: REString;
begin
  if FKind = nkEmpty then
    Result := Format(sFmtDumpNFA_Empty, [FTransitTo])
  else if FKind = nkLoop then
  begin
    case FMatchKind of
      lkReluctant:
        MatchTypeStr := 'R';
      lkPossessive:
        MatchTypeStr := 'P';
    else
      MatchTypeStr := 'G';
    end;
    Result := Format(sFmtDumpNFA_Loop, [MatchTypeStr, FMin, FMax, FTransitTo]);
  end
  else if FKind = nkEnd then
    Result := Format(sFmtDumpNFA_EndStr, [FTransitTo])
  else if FKind = nkLoopExit then
    Result := Format(sFmtDumpNFA_LoopExit, [FTransitTo])
  else if FKind = nkLoopEnd then
    Result := Format(sFmtDumpNFA_LoopEnd, [FTransitTo])
    // else if FKind = nkQuest then
    // Result :=  Format(sFmtDumpNFA_Quest, [FTransitTo])
  else if FKind = nkStar then
    Result := Format(sFmtDumpNFA_Star, [FTransitTo, FCode.GetDebugStr])
  else if FKind = nkPlus then
    Result := Format(sFmtDumpNFA_Plus, [FTransitTo, FCode.GetDebugStr])
  else if FKind = nkBound then
    Result := Format(sFmtDumpNFA_Bound, [FTransitTo])
  else if FKind = nkMatchEnd then
    Result := Format(sFmtDumpNFA_MatchEnd, [FTransitTo])
  else if FKind = nkGroupBegin then
    Result := Format(sFmtDumpNFA_GroupStart, [FGroupIndex, FTransitTo])
  else if FKind = nkGroupEnd then
    Result := Format(sFmtDumpNFA_GroupEnd, [FGroupIndex, FTransitTo])
  else if FKind = nkNoBackTrack then
    Result := Format(sFmtDumpNFA_NoBackTrackBegin, [FTransitTo])
  else if FKind = nkFail then
    Result := Format(sFmtDumpNFA_Fail, [FTransitTo])
  else if FKind = nkAheadMatch then
    Result := Format(sFmtDumpNFA_AheadMatch, [FTransitTo])
  else if FKind = nkAheadNoMatch then
    Result := Format(sFmtDumpNFA_AheadNoMatch, [FTransitTo])
  else if FKind = nkBehindMatch then
    Result := Format(sFmtDumpNFA_BehindMatch, [FTransitTo])
  else if FKind = nkBehindNoMatch then
    Result := Format(sFmtDumpNFA_BehindNoMatch, [FTransitTo])
  else if FKind = nkGoSub then
    Result := Format(sFmtDumpNFA_GoSub, [FGroupIndex, FTransitTo])
  else if FKind = nkIfMatch then
    Result := Format(sFmtDumpNFA_IfMatch, [FTransitTo])
  else if FKind = nkIfThen then
    Result := Format(sFmtDumpNFA_IfThen, [FTransitTo])
  else if FKind = nkKeepPattern then
    Result := Format(sFmtDumpNFA_KeepPattern, [FTransitTo])
  else
  begin
    if FCode <> nil then
      Result := Format(sFmtDumpNFA_Null, [(FCode as TRECode).GetDebugStr,
        FTransitTo]);
  end;

end;

{$ENDIF}
{ TRENFA }

procedure TRENFA.AddTransition(AKind: TRENFAKind; ATransFrom, ATransTo: Integer;
  ACode: TRECode; AMin: Integer; AMax: Integer);
var
  NFACode: TRENFAState;
begin
  NFACode := TRENFAState.Create;
  with NFACode do
  begin
{$IFDEF DEBUG}
    Index := ATransFrom;
{$ENDIF}
    Kind := AKind;
    Code := ACode;
    TransitTo := ATransTo;
    Next := TRENFAState(FStateList[ATransFrom]);
    Min := AMin;
    Max := AMax;
  end;
  FStateList[ATransFrom] := NFACode;
end;

procedure TRENFA.Compile;
begin
  FRegExp.ClearStateList;

  FRegExp.FEntryState := GetNumber;
  FBEntryState := FRegExp.FEntryState;
  FRegExp.FExitState := GetNumber;
  FBExitState := FRegExp.FExitState;

  FEntryStack.Clear;
  FExitStack.Clear;
  FEntryStackIndex := 0;
  FExitStateIndex := 0;
  FGroupCount := 0;

  AddTransition(nkEnd, FRegExp.FExitState, -1, nil);
  GenerateStateList(FRegExp.FCode, FRegExp.FEntryState, FRegExp.FExitState);
end;

constructor TRENFA.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FStateList := ARegExp.FStateList;
  FEntryStack := TList.Create;
  FExitStack := TList.Create;
  FStateStack := TList.Create;
  FEntryStackIndex := 0;
  FExitStateIndex := 0;
  FStateStackIndex := 0;
end;

destructor TRENFA.Destroy;
begin
  FStateStack.Free;
  FEntryStack.Free;
  FExitStack.Free;
  inherited;
end;

procedure TRENFA.GenerateStateList(ACode: TRECode; AEntry, AWayout: Integer);
var
  State1, State2, Index: Integer;
  SubCode: TRECode;
  NFACode: TRENFAState;
begin
  if ACode is TREBinCode then
  begin
    with ACode as TREBinCode do
    begin
      case Op of
        opUnion:
          begin
            GenerateStateList(Right, AEntry, AWayout);
            GenerateStateList(Left, AEntry, AWayout);
          end;
        opConcat:
          begin
            State1 := GetNumber;
            GenerateStateList(Left, AEntry, State1);
            GenerateStateList(Right, State1, AWayout);
          end;
        opStar:
          begin
            AddTransition(nkStar, AEntry, AWayout, Left, 0, CONST_LoopMax);

            NFACode := FStateList[AEntry];
            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
            if AEntry = FBEntryState then
              FBEntryState := AWayout;
          end;
        opPlus:
          begin
            AddTransition(nkPlus, AEntry, AWayout, Left, 1, CONST_LoopMax);

            NFACode := FStateList[AEntry];
            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
          end;
        opBound:
          begin
            AddTransition(nkBound, AEntry, AWayout, Left, FMin, FMax);

            NFACode := FStateList[AEntry];
            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
          end;
        opLoop:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkLoop, AEntry, State1, nil, FMin, FMax);

            NFACode := FStateList[AEntry];
            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkLoopExit, State1, AWayout, nil);
            AddTransition(nkLoopEnd, State2, State1, nil);

            if (FMin = 0) and (AEntry = FBEntryState) then
              FBEntryState := AWayout;
          end;
        opQuest:
          begin
            if FMatchKind <> lkReluctant then
            begin
              AddTransition(nkEmpty, AEntry, AWayout, nil);

              NFACode := FStateList[AEntry];
              NFACode.MatchKind := MatchKind;
            end;

            GenerateStateList(Left, AEntry, AWayout);

            if FMatchKind = lkReluctant then
            begin
              AddTransition(nkEmpty, AEntry, AWayout, nil);
              NFACode := FStateList[AEntry];
              NFACode.MatchKind := MatchKind;
            end;

            if AEntry = FBEntryState then
              FBEntryState := AWayout;
          end;
        opGroup:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            if GroupIndex > 0 then
            begin
              FRegExp.FGroups[GroupIndex].IndexBegin := AEntry;
              FRegExp.FGroups[GroupIndex].IndexEnd := State2;
            end;

            AddTransition(nkGroupBegin, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.GroupIndex := GroupIndex;

            FStateStack.Add(Pointer(AEntry));
            Inc(FGroupCount);

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            Dec(FGroupCount);
            FStateStack.Delete(FStateStack.Count - 1);

            AddTransition(nkGroupEnd, State2, AWayout, nil);
            NFACode := FStateList[State2];
            NFACode.GroupIndex := GroupIndex;
          end;
        opNoBackTrack:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkNoBackTrack, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil);
          end;
        opKeepPattern:
          begin
            State1 := GetNumber;

            AddTransition(nkKeepPattern, AEntry, State1, nil);
            GenerateStateList(Left, State1, AWayout);
          end;
        opFail:
          begin
            State1 := GetNumber;

            AddTransition(nkFail, AEntry, State1, nil);
            GenerateStateList(Left, State1, AWayout);
          end;
        opEmply:
          begin
            AddTransition(nkEmpty, AEntry, AWayout, nil);
          end;
        opGoSub:
          begin
            AddTransition(nkGoSub, AEntry, AWayout, nil);
            NFACode := FStateList[AEntry];
            NFACode.HasRecursion := HasRecursion;

            if GroupIndex = -1 then
            begin
              Index := FRegExp.FGroups.IndexOfName(GroupName);
              Assert(Index <> -1, 'BUG?: Not Define Group Index');
              NFACode.GroupIndex := Index;
            end
            else
              NFACode.GroupIndex := GroupIndex;
          end;
        opAheadMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkAheadMatch, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil)
          end;
        opAheadNoMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkAheadNoMatch, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil);
          end;
        opBehindMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkBehindMatch, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.Max := FMax;
            NFACode.Min := FMin;
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil)
          end;
        opBehindNoMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkBehindNoMatch, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.ExtendTo := State2;
            NFACode.Max := FMax;
            NFACode.Min := FMin;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil);
          end;
        opIfMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            GenerateStateList(Left, AEntry, State2);
            AddTransition(nkIfMatch, AEntry, State1, nil);
            NFACode := FStateList[AEntry];
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Right, State1, AWayout);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil);
          end;
        opIfThen:
          begin
            if Right <> nil then
            begin
              State1 := GetNumber;

              AddTransition(nkIfThen, AEntry, State1, nil);
              GenerateStateList(Right, State1, AWayout);
            end;

            State2 := GetNumber;
            AddTransition(nkIfThen, AEntry, State2, nil);

            GenerateStateList(Left, State2, AWayout);
          end;
      end;
    end;
  end
  else
  begin
    if (AEntry = FBEntryState) and (ACode is TRECharCode) and
      (ACode as TRECharCode).FConvert and
      (((ACode as TRECharCode).GetWChar = UCS4Char('^')) or
      ((roIgnoreWidth in (ACode as TRECharCode).FOptions) and
      ((ACode as TRECharCode).GetWChar = CONST_WIDE_HAT))) then
    begin
      SubCode := TRELineHeadCode.Create(FRegExp, (ACode as TRECharCode)
        .FOptions);
      ReplaceCode(ACode, SubCode);
      AddTransition(nkNormal, AEntry, AWayout, SubCode);
    end
    else if (AWayout = FBExitState) and (ACode is TRECharCode) and
      (ACode as TRECharCode).FConvert and
      (((ACode as TRECharCode).GetWChar = UCS4Char('$')) or
      ((roIgnoreWidth in (ACode as TRECharCode).FOptions) and
      ((ACode as TRECharCode).GetWChar = CONST_WIDE_DOLL))) then
    begin
      SubCode := TRELineTailCode.Create(FRegExp, (ACode as TRECharCode)
        .FOptions);
      ReplaceCode(ACode, SubCode);
      AddTransition(nkNormal, AEntry, AWayout, SubCode);
    end
    else
    begin
      if (ACode is TRECharCode) or (ACode is TRELiteralCode) then
        AddTransition(nkChar, AEntry, AWayout, ACode)
      else
        AddTransition(nkNormal, AEntry, AWayout, ACode);
    end;
  end;
end;

function TRENFA.GetNumber: Integer;
begin
  Result := FStateList.Add(nil);
end;

function TRENFA.GetRealState: Integer;
begin
  Result := Integer(FStateStack[FStateStackIndex]);
end;

procedure TRENFA.PopState;
begin
  if FEntryStackIndex > 0 then
  begin
    Dec(FEntryStackIndex);
    FBEntryState := Integer(FEntryStack[FEntryStackIndex]);
    FEntryStack.Delete(FEntryStackIndex);
  end;
  if FExitStateIndex > 0 then
  begin
    Dec(FExitStateIndex);
    FBExitState := Integer(FExitStack[FExitStateIndex]);
    FExitStack.Delete(FExitStateIndex);
  end;
end;

procedure TRENFA.PushState(AEntry, AWayout, ANewEntry, ANewWayout: Integer);
begin
  if AEntry = FBEntryState then
  begin
    FEntryStack.Add(Pointer(FBEntryState));
    FBEntryState := ANewEntry;
    Inc(FEntryStackIndex);
  end;
  if AWayout = FBExitState then
  begin
    FExitStack.Add(Pointer(FBExitState));
    FBExitState := ANewWayout;
    Inc(FExitStateIndex);
  end;
end;

procedure TRENFA.ReplaceCode(var OldCode, NewCode: TRECode);

  procedure ReplaceCodeSub(var SourceCode: TRECode;
    var OldCode, NewCode: TRECode);
  var
    Code, SubCode: TREBinCode;
    I: Integer;
  begin
    if SourceCode is TREBinCode then
    begin
      Code := SourceCode as TREBinCode;
      if Assigned(Code.Left) then
        ReplaceCodeSub(Code.FLeft, OldCode, NewCode);
      if Assigned(Code.Right) then
        ReplaceCodeSub(Code.FRight, OldCode, NewCode);
    end
    else
    begin
      if SourceCode = OldCode then
      begin
        I := FRegExp.FCodeList.IndexOf(OldCode);
        if I = -1 then
          FRegExp.Error(sFatalError);

        SubCode := FRegExp.FCodeList[I];
        SubCode.Free;
        FRegExp.FCodeList[I] := NewCode;

        SourceCode := NewCode;
        Exit;
      end;
    end;
  end;

{$IFNDEF DEBUG}

var
  TempCode: TRECode;
  Index: Integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  ReplaceCodeSub(FRegExp.FCode, OldCode, NewCode);
{$ELSE}
  Index := FRegExp.FCodeList.IndexOf(OldCode);
  if Index = -1 then
    FRegExp.Error(sNotFoundOldCode);

  TempCode := FRegExp.FCodeList[Index];
  FRegExp.FCodeList[Index] := NewCode;
  TempCode.Free;
{$ENDIF}
end;

{ TREStack }

procedure TREStack.Clear;
var
  I: Integer;
  MatchRecList: TList;
  LStat: PREBackTrackStackRec;
  P: TRECapture;
  J: Integer;
begin
  for I := FCount - 1 downto 0 do
  begin
    LStat := FStat^[I];
    if LStat <> nil then
    begin
      Dispose(LStat);
      FStat^[I] := nil;
    end;

    if FGroup^[I] <> nil then
    begin
      MatchRecList := FGroup^[I];
      for J := MatchRecList.Count - 1 downto 0 do
      begin
        P := MatchRecList[J];
        if Assigned(P) then
          P.Free;
      end;
      MatchRecList.Free;
      FGroup^[I] := nil;
    end;
  end;

  FCount := 0;
end;

function TREStack.Count: Integer;
begin
  Result := FCount;
end;

constructor TREStack.Create(ARegExp: TSkRegExp; ACheckMatchExplosion: Boolean);
begin
  inherited Create;
  FRegExp := ARegExp;
  FSize := CONST_REStack_Size;
  GetMem(FStat, FSize * Sizeof(Pointer));
  GetMem(FGroup, FSize * Sizeof(Pointer));
  FCount := 0;
  FCheckMatchExplosion := ACheckMatchExplosion;
end;

destructor TREStack.Destroy;
begin
  Clear;

  FreeMem(FStat);
  FreeMem(FGroup);
  inherited;
end;

function TREStack.Index: Integer;
begin
  Result := FCount - 1;
end;

function TREStack.Peek: TRENFAState;
var
  Index: Integer;
begin
  Index := FCount - 1;
  if Index = -1 then
  begin
    Result := nil;
    Exit;
  end;

  Result := PREBackTrackStackRec(FStat^[Index]).NFACode;
end;

procedure TREStack.Pop(var NFACode: TRENFAState; var AStr: PWideChar);
label
  ReStart;
var
  I, Index: Integer;
  MatchRecList: TList;
  P: TRECapture;
  LStat: PREBackTrackStackRec;
{$IFDEF CHECK_MATCH_EXPLOSION}
  S, D, N: PREMatchExplosionStateRec;
{$ENDIF}
begin
ReStart:
  Index := FCount - 1;
  if Index = -1 then
  begin
    NFACode := nil;
    Exit;
  end;

  LStat := FStat^[Index];

  NFACode := LStat.NFACode;
  if LStat.Str <> nil then
    AStr := LStat.Str;

  Dispose(LStat);
  FStat^[Index] := nil;

  if FGroup^[Index] <> nil then
  begin
    MatchRecList := FGroup^[Index];
    for I := 0 to MatchRecList.Count - 1 do
    begin
      P := MatchRecList[I];
      FRegExp.FGroups[I].Capture.StartP := P.StartP;
      FRegExp.FGroups[I].Capture.EndP := P.EndP;
      FRegExp.FGroups[I].Capture.Matched := P.Matched;
      P.Free;
    end;
    MatchRecList.Free;
  end;

  Dec(FCount);

{$IFDEF CHECK_MATCH_EXPLOSION}
  if FCheckMatchExplosion and (NFACode.Kind in [nkEmpty, nkLoop, nkStar, nkPlus,
    nkBound]) then
  begin
    Index := AStr - FRegExp.FTextTopP;

    if FRegExp.FMatchExplosionState[Index] <> nil then
    begin
      S := FRegExp.FMatchExplosionState[Index];
      D := S;
      while S <> nil do
      begin
        if S.NFACode = NFACode then
          goto ReStart;

        D := S;
        S := S.Next;
      end;

      New(N);
      N.NFACode := NFACode;
      N.Next := nil;
      D.Next := N;
    end
    else
    begin
      New(N);
      N.NFACode := NFACode;
      N.Next := nil;

      FRegExp.FMatchExplosionState[Index] := N;
    end;
  end;
{$ENDIF}
end;

procedure TREStack.Push(NFACode: TRENFAState; AStr: PWideChar;
  IsPushGroup: Boolean);
var
  I: Integer;
  MatchRecList: TList;
  LStat: PREBackTrackStackRec;
  P: TRECapture;
begin
  if FCount + 1 >= FSize then
    Extend(FSize div 4);

  New(LStat);
  LStat.NFACode := NFACode;
  LStat.Str := AStr;
  FStat^[FCount] := LStat;

  if IsPushGroup then
  begin
    MatchRecList := TList.Create;
    for I := 0 to FRegExp.FGroups.Count - 1 do
    begin
      P := TRECapture.Create;
      P.StartP := FRegExp.FGroups[I].Capture.StartP;
      P.EndP := FRegExp.FGroups[I].Capture.EndP;
      P.Matched := FRegExp.FGroups[I].Capture.Matched;
      MatchRecList.Add(P);
    end;
    FGroup^[FCount] := MatchRecList;
  end
  else
    FGroup^[FCount] := nil;

  Inc(FCount);
end;

procedure TREStack.Remove(const AIndex: Integer);
var
  I, L: Integer;
  NFACode: TRENFAState;
  AStr: PWideChar;
begin
  L := FCount - AIndex - 1;
  for I := 1 to L do
    Pop(NFACode, AStr);
end;

procedure TREStack.Extend(ASize: Integer);
begin
  if ASize < CONST_REStack_Size then
    ASize := CONST_REStack_Size;

  FSize := FSize + ASize;

  ReallocMem(FStat, FSize * Sizeof(Pointer));
  ReallocMem(FGroup, FSize * Sizeof(Pointer));
end;

{ TRELeadCodeCollection }

function TRELeadCodeCollection.Add(Value: TRECode; AOffset: Integer;
  ABehindMatch: Boolean): Integer;
var
  Item: TRELeadCode;
begin
  Item := TRELeadCode.Create;
  Item.Code := Value;
  Item.Offset := AOffset;
  Item.IsBehindMatch := ABehindMatch;
  Result := FList.Add(Item);
end;

procedure TRELeadCodeCollection.Clear;
begin
  FList.Clear;
end;

constructor TRELeadCodeCollection.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

procedure TRELeadCodeCollection.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TRELeadCodeCollection.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRELeadCodeCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TRELeadCodeCollection.GetItem(Index: Integer): TRELeadCode;
begin
  Result := FList[Index] as TRELeadCode;
end;

{ TREMatchEngine }

constructor TREMatchEngine.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FLeadCode := TRELeadCodeCollection.Create;
  // FLeadCode.OwnsObjects := False;
  FTailCode := TObjectList.Create;
  FTailCode.OwnsObjects := False;
  FLeadStrings := TREQuickSearch.Create;
  FMap := TRECharMap.Create;

  FSpecialMatchStack := TList.Create;

  if not Assigned(FRegExp.FStateList) then
    FRegExp.Error('bug: StateList not initialized.');
  FStateList := FRegExp.FStateList;

  if not Assigned(FRegExp.FGroups) then
    FRegExp.Error('bug: MatchData not initialized.');
  FGroups := FRegExp.FGroups;
end;

destructor TREMatchEngine.Destroy;
begin
  FSpecialMatchStack.Free;

  FMap.Free;
  FTailCode.Free;
  FLeadCode.Free;
  FLeadStrings.Free;
  inherited;
end;

procedure TREMatchEngine.GenerateLeadCode;
var
  IsNotLead: Boolean;

  procedure RebuildSub(Index: Integer);
  var
    Source, Dest: TRECode;
    I: Integer;
  begin
    Source := FLeadCode[Index].Code;
    for I := FLeadCode.Count - 1 downto 0 do
    begin
      if I <> Index then
      begin
        Dest := FLeadCode[I].Code;
        if Source.IsInclude(Dest) then
          FLeadCode.Delete(I);
      end;
    end;
  end;

  procedure GetLeadCode(NFACode: TRENFAState; AOffset: Integer;
    ABehindMatch: Boolean);
  var
    SubCode: TRENFAState;
  begin
    while NFACode.Kind = nkGroupBegin do
    begin
      if NFACode.Next <> nil then
        GetLeadCode(NFACode.Next, AOffset, ABehindMatch);

      NFACode := FStateList[NFACode.TransitTo];
    end;

    while NFACode <> nil do
    begin
      case NFACode.Kind of
        nkChar:
          FLeadCode.Add(NFACode.Code, AOffset, ABehindMatch);
        nkNormal:
          if (NFACode.Code is TREAnyCharCode) then
          begin
            SubCode := FStateList[NFACode.TransitTo];
            Inc(AOffset);
            GetLeadCode(SubCode, AOffset, ABehindMatch);
          end
          else if not(NFACode.Code is TRECombiningSequence) then
            FLeadCode.Add(NFACode.Code, AOffset, ABehindMatch)
          else
          begin
            FLeadCode.Add(nil, AOffset, ABehindMatch);
            Exit;
          end;
        nkStar:
          begin
            FLeadCode.Add(NFACode.Code, AOffset, ABehindMatch);
            GetLeadCode(FStateList[NFACode.TransitTo], AOffset, ABehindMatch);
            FIsNotSimplePattern := False;
          end;
        nkPlus:
          begin
            FIsNotSimplePattern := True;
            FLeadCode.Add(NFACode.Code, AOffset, ABehindMatch);
          end;
        nkBound:
          begin
            FIsNotSimplePattern := True;
            FLeadCode.Add(NFACode.Code, AOffset, ABehindMatch);
            if NFACode.Min = 0 then
              GetLeadCode(FStateList[NFACode.TransitTo], AOffset, ABehindMatch);
          end;
        nkLoop:
          begin
            SubCode := FStateList[NFACode.TransitTo];
            if NFACode.Min = 0 then
            begin
              GetLeadCode(FStateList[SubCode.TransitTo], AOffset, ABehindMatch);
              FIsNotSimplePattern := True;
            end;

            GetLeadCode(SubCode.Next, AOffset, ABehindMatch);
          end;
        nkAheadMatch:
          GetLeadCode(FStateList[NFACode.TransitTo], AOffset, ABehindMatch);
        nkBehindMatch:
          GetLeadCode(FStateList[NFACode.TransitTo], AOffset, True);
        nkEmpty:
          GetLeadCode(FStateList[NFACode.TransitTo], AOffset, ABehindMatch);
      else
        begin
          FLeadCode.Add(nil, AOffset, ABehindMatch);
          Exit;
        end;
      end;

      NFACode := NFACode.Next;
    end;
  end;

var
  I: Integer;
begin
  FIsNotSimplePattern := False;
  IsNotLead := False;
  FPreMatchLength := 0;
  FIsBehindMatch := False;

  FLeadCode.Clear;
  GetLeadCode(FStateList[FRegExp.FEntryState], 0, False);

  if FLeadCode.Count = 0 then
    Exit;

  if not IsNotLead then
  begin
    for I := 0 to FLeadCode.Count - 1 do
    begin
      if FLeadCode[I].Code = nil then
      begin
        IsNotLead := True;
        Break;
      end;
    end;
  end;

  if IsNotLead then
  begin
    FLeadCode.Clear;
  end
  else
  begin
    I := 0;

    while I < FLeadCode.Count do
    begin
      RebuildSub(I);
      Inc(I);
    end;
  end;
end;

procedure TREMatchEngine.GenerateTailCode;
var
  I, LExitState: Integer;
  NFACode: TRENFAState;
begin
  FTailCode.Clear;

  LExitState := FRegExp.FExitState;

  for I := 0 to FStateList.Count - 1 do
  begin
    NFACode := FStateList[I];

    while NFACode <> nil do
    begin
      if (NFACode.TransitTo = LExitState) then
      begin
        if (NFACode.Kind = nkChar) and
          ((NFACode.Code is TRELiteralCode) or
          (NFACode.Code is TRECharCode)) then
        begin
          FTailCode.Add(NFACode.Code);
        end
        else
          FTailCode.Add(nil);
      end
      else if NFACode.Kind = nkGroupEnd then
        LExitState := I;

      NFACode := NFACode.Next;
    end;
  end;
end;

function TREMatchEngine.Match(AStr: PWideChar): Boolean;
var
  P: PWideChar;
  IsCheck: Boolean;
begin
  Result := False;
  FStateList := FRegExp.FStateList;
  FGroups := FRegExp.FGroups;

  if FLeadCharMode = lcmLastLiteral then
  begin
    if not FLeadStrings.Exec(AStr, FRegExp.FMatchEndP - AStr) then
      Exit;
  end;

  case FLeadCharMode of
    lcmFirstLiteral:
      begin
        P := AStr;
        if FLeadStrings.Exec(P, FRegExp.FMatchEndP - P) then
        begin
          repeat
            P := FLeadStrings.MatchP;
            if FIsBehindMatch and
              (FRegExp.FMatchEndP > P + FPreMatchLength) then
              Inc(P, FPreMatchLength)
            else if (FPreMatchLength > 0) and
              (P >= FRegExp.FMatchTopP + FPreMatchLength) then
              Dec(P, FPreMatchLength);

            if MatchEntry(P) then
            begin
              Result := True;
              Exit;
            end;

          until not FLeadStrings.ExecNext;
        end;
      end;
    lcmSimple:
      begin
        if FLeadStrings.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          Result := True;
          FGroups[0].Capture.StartP := AStr + FLeadStrings.MatchPos - 1;
          FGroups[0].Capture.EndP := FLeadStrings.MatchP +
            Length(FLeadStrings.FindText);
        end;
      end;
    lcmTextTop:
      begin
        if MatchEntry(AStr) then
        begin
          Result := True;
          Exit;
        end;
      end;
    lcmLineTop:
      begin
        while AStr <= FRegExp.FMatchEndP do
        begin
          if MatchEntry(AStr) then
          begin
            Result := True;
            Exit;
          end;

          while not IsLineSeparator(AStr^) and (AStr <= FRegExp.FMatchEndP) do
            Inc(AStr);

          if IsLineSeparator(AStr^) then
          begin
            repeat
              Inc(AStr);
            until not IsLineSeparator(AStr^);
          end
          else
            Break;
        end;
      end;
  else
    begin
      if FLeadCharMode = lcmMap then
      begin
        while AStr <= FRegExp.FMatchEndP do
        begin
          if FMap.IsExists(AStr) then
          begin
            if MatchEntry(AStr) then
            begin
              Result := True;
              Exit;
            end;
          end
          else
            Inc(AStr);
        end;
      end
      else
      begin
        if AStr <> nil then
        begin
          while AStr <= FRegExp.FMatchEndP do
          begin

            if FLeadCharMode = lcmHasLead then
              IsCheck := IsLeadStrings(AStr)
            else
              IsCheck := True;

            if IsCheck then
            begin
              if MatchEntry(AStr) then
              begin
                Result := True;
                Exit;
              end;
            end
            else
              Inc(AStr);
          end;
        end;
      end;
    end;
  end;
end;

function TREMatchEngine.MatchCore(var NFACode: TRENFAState;
  var AStr: PWideChar): Boolean;
var
  Stack: TREStack;
begin
  Stack := TREStack.Create(FRegExp);
  try
    Result := False;

    while NFACode <> nil do
    begin
      NFACode := MatchPrim(NFACode, AStr, Stack);

      if (NFACode = nil) then
      begin
        if (FGroups[0].Capture.EndP <> nil) then
        begin
          Result := True;
          Exit;
        end
        else
          Stack.Pop(NFACode, AStr);
      end;

      if (NFACode <> nil) and (NFACode.Kind = nkMatchEnd) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    Stack.Free;
  end;
end;

function TREMatchEngine.MatchCore(var NFACode, EndCode: TRENFAState;
  var AStr: PWideChar; ACheckExplosion: Boolean): Boolean;
var
  Stack: TREStack;
  Index: Integer;
  SaveP: PWideChar;
begin
  Stack := TREStack.Create(FRegExp, ACheckExplosion);
  try
    Result := False;
    SaveP := AStr;
    Index := Stack.Index;

    while NFACode <> nil do
    begin
      NFACode := MatchPrim(NFACode, AStr, Stack);

      if (NFACode = nil) then
      begin
        if FGroups[0].Capture.EndP <> nil then
        begin
          Result := True;
          Exit;
        end
        else if Stack.Index > Index then
          Stack.Pop(NFACode, AStr);
      end;

      if (NFACode <> nil) and (NFACode = EndCode) then
      begin
        NFACode := MatchPrim(NFACode, AStr, Stack);
        Result := True;
        Exit;
      end;
    end;

    if not Result then
      AStr := SaveP;
  finally
    Stack.Free;
  end;
end;

function TREMatchEngine.MatchEntry(var AStr: PWideChar): Boolean;
var
  NFACode: TRENFAState;
  SaveP: PWideChar;
begin
  SaveP := AStr;
  FGroups.Reset;

  NFACode := FStateList[FRegExp.FEntryState];
  FGroups[0].Capture.StartP := AStr;
  Result := MatchCore(NFACode, AStr);
  if not Result then
  begin
    AStr := SaveP;
    Inc(AStr);
  end;

end;

function TREMatchEngine.MatchPrim(NFACode: TRENFAState; var AStr: PWideChar;
  Stack: TREStack): TRENFAState;

  function MatchLoop(EntryCode, EndCode: TRENFAState; var AStr: PWideChar;
    Stack: TREStack): Boolean;
  var
    NFACode: TRENFAState;
    Index: Integer;
    SaveP: PWideChar;
  begin
    Result := False;
    Index := Stack.Index;
    SaveP := AStr;
    NFACode := EntryCode;

    while NFACode <> nil do
    begin
      while NFACode <> nil do
      begin
        NFACode := MatchPrim(NFACode, AStr, Stack);

        if (NFACode <> nil) and
            ((NFACode = EndCode) or (NFACode.Kind = nkMatchEnd)) then
        begin
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
          MatchProcess(NFACode, AStr);
{$ENDIF}
{$ENDIF}
          Result := True;
          Break;
        end
      end;

      if (NFACode = nil) then
      begin
        if FGroups[0].Capture.EndP <> nil then
          Exit;

        if Stack.Index > Index then
          Stack.Pop(NFACode, AStr)
        else
          Break;
      end
      else
      begin
        if (AStr = SaveP) then
        begin
          if Stack.Index > Index then
            Stack.Pop(NFACode, AStr)
          else
            Break;
        end
        else
          Break;
      end;
    end;
  end;

  procedure BranchSetup(NFACode: TRENFAState; AStr: PWideChar;
    IsPushGroup: Boolean);
  var
    Len: Integer;
  begin
    if NFACode <> nil then
    begin
      if NFACode.Kind in [nkChar, nkNormal] then
      begin
        if NFACode.Code.IsEqual(AStr, Len) then
          Stack.Push(NFACode, AStr, IsPushGroup)
        else if NFACode.Next <> nil then
          BranchSetup(NFACode.Next, AStr, IsPushGroup);
      end
      else
        Stack.Push(NFACode, AStr, IsPushGroup);
    end;
  end;

var
  I, Len, LMin, LMax, Index, LEntry, BaseIndex: Integer;
  EntryCode, NextCode, EndCode, SubCode: TRENFAState;
  LMatchKind: TRELoopKind;
  SubP, SaveP: PWideChar;
  IsMatched: Boolean;
begin
  Result := nil;

{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
  MatchProcess(NFACode, AStr);
  try
{$ENDIF}
{$ENDIF}
    if NFACode.Next <> nil then
    begin
      if NFACode.Kind in [nkLoopExit, nkLoopEnd, nkIfThen] then
        // nkLoopExitがここに入ってくることはありえない。
        // スタックへ退避せず次へ遷移
        NFACode := FStateList[NFACode.TransitTo]
      else if NFACode.Kind <> nkIfMatch then
        // 分岐があればスタックへ退避
        BranchSetup(NFACode.Next, AStr, False);
    end;

    case NFACode.Kind of
      nkNormal, nkChar:
        begin
          if NFACode.Code.IsEqual(AStr, Len) then
          begin
            Inc(AStr, Len);
            Result := FStateList[NFACode.TransitTo];
          end
          else
            Result := nil;
        end;
      nkStar, nkPlus:
        begin
          SubP := AStr;
          LMatchKind := NFACode.MatchKind;
          NextCode := FStateList[NFACode.TransitTo];
          EntryCode := NFACode;

          if EntryCode.Code.ExecRepeat(AStr, NFACode.Kind = nkStar) then
          begin
            if LMatchKind = lkAny then
            begin
              // Plus でマッチしたときは SubP を１単位進める
              if NFACode.Kind = nkPlus then
                CharNext(SubP);

              Result := NextCode;
              SaveP := AStr;
              while not MatchCore(Result, AStr) do
              begin
                AStr := SaveP;
                if SubP < AStr then
                begin
                  CharPrev(AStr);
                  SaveP := AStr;
                  Result := NextCode;
                end
                else
                  Break;
              end;
            end
            else if LMatchKind = lkCombiningSequence then
            begin
              Result := NextCode;
              SaveP := AStr;
              while not MatchCore(Result, AStr) do
              begin
                AStr := SaveP;
                if SubP < AStr then
                begin
                  Dec(AStr);
                  while (SubP < AStr) and
                    IsUnicodeProperty(ToUCS4Char(AStr), upM) do
                  begin
                    Dec(AStr);
                  end;
                  SaveP := AStr;
                  Result := NextCode;
                end
                else
                  Break;
              end;
            end
            else if LMatchKind <> lkPossessive then
            begin
              // Plus でマッチしたときは SubP を１単位進める
              Len := EntryCode.Code.CharLength;
              if NFACode.Kind = nkPlus then
                CharNext(SubP, Len);

              while SubP < AStr do
              begin
                BranchSetup(NextCode, SubP, True);
                while EntryCode.Next <> nil do
                begin
                  BranchSetup(EntryCode.Next, AStr, True);
                  EntryCode := EntryCode.Next;
                end;

                CharNext(SubP, Len);
              end;
              Result := NextCode;
            end
            else
            begin
              Result := NextCode;
            end;
          end
          else
            Result := nil;
        end;
      nkBound:
        begin
          SubP := AStr;
          LMatchKind := NFACode.MatchKind;
          LMin := NFACode.Min;
          LMax := NFACode.Max;

          NextCode := FStateList[NFACode.TransitTo];
          EntryCode := NFACode;

          if EntryCode.Code.ExecRepeat(AStr, LMin, LMax) then
          begin
            Len := EntryCode.Code.CharLength;

            // LMin 分は確定
            if LMin > 0 then
              CharNext(SubP, LMin * Len);

            if LMatchKind = lkAny then
            begin
              SaveP := AStr;
              Result := MatchPrim(NextCode, AStr, Stack);
              while Result = nil do
              begin
                AStr := SaveP;
                if SubP < AStr then
                begin
                  CharPrev(AStr, Len);
                  SaveP := AStr;
                end
                else
                  Break;

                Result := MatchPrim(NextCode, AStr, Stack);
              end;
            end
            else if LMatchKind <> lkPossessive then
            begin
              SaveP := AStr;
              Result := MatchPrim(NextCode, AStr, Stack);
              while Result = nil do
              begin
                AStr := SaveP;
                if SubP < AStr then
                begin
                  CharPrev(AStr, Len);
                  SaveP := AStr;
                end
                else
                  Break;

                Result := MatchPrim(NextCode, AStr, Stack);
              end
            end
            else
              Result := NextCode;
          end
          else
            Result := nil;
        end;
      nkLoop:
        begin
          LMatchKind := NFACode.MatchKind;
          LMin := NFACode.Min;
          LMax := NFACode.Max;

          // ループの終了を登録
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];
          // ループの次を登録
          NextCode := FStateList[NFACode.TransitTo];
          // ループの入口を登録
          EntryCode := NFACode.Next;

          if LMin > 0 then
          begin
            for I := 1 to LMin do
            begin
              if not MatchLoop(EntryCode, EndCode, AStr, Stack) then
                Exit;
            end;

            if LMin = LMax then
            begin
              // LMin = LMaxならバックトラックの必要がない
              Result := NextCode;
              Exit;
            end;
          end
          else
            Stack.Push(NextCode, AStr, True);

          if LMax <> CONST_LoopMax then
            LMax := LMax - LMin;

          if LMatchKind = lkGreedy then
          begin
            NFACode := EntryCode;
            IsMatched := True;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              Stack.Push(NextCode, AStr, True);
              IsMatched := MatchLoop(NFACode, EndCode, AStr, Stack);

              if not IsMatched or (SubP = AStr) then
                Break;
            end;

            if IsMatched then
              Result := NextCode;
          end
          else if LMatchKind = lkSimpleReluctant then
          begin
          end
          else if LMatchKind = lkReluctant then
          begin
            IsMatched := True;
            NFACode := EntryCode;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              Result := NextCode;
              if MatchCore(Result, AStr) then
                Exit;

              IsMatched := MatchLoop(NFACode, EndCode, AStr, Stack);

              if not IsMatched or (SubP = AStr) then
                Break;
            end;

            if IsMatched then
              Result := NextCode;
          end
          else if LMatchKind = lkPossessive then
          begin
            BaseIndex := Stack.Index;
            NFACode := EntryCode;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              IsMatched := MatchLoop(NFACode, EndCode, AStr, Stack);

              if not IsMatched or (SubP = AStr) then
                Break;
            end;
            Stack.Remove(BaseIndex);
            Result := NextCode;
          end;
        end;
      nkEnd:
        begin
          FGroups[0].Capture.EndP := AStr;
          Result := nil;
          Exit;
        end;
      nkFail:
        begin
          Result := nil;
        end;
      nkEmpty, nkLoopExit, nkLoopEnd, nkIfThen:
        Result := FStateList[NFACode.TransitTo];
      nkGroupBegin:
        begin
          FGroups[NFACode.GroupIndex].Capture.StartP := AStr;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkGroupEnd:
        begin
          FGroups[NFACode.GroupIndex].Capture.EndP := AStr;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkNoBackTrack:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];
          SubP := AStr;
          if MatchCore(NFACode, EndCode, AStr) then
          begin
            Result := FStateList[EndCode.TransitTo];
          end
          else
          begin
            AStr := SubP;
            Result := nil;
          end;
        end;
      nkKeepPattern:
        begin
          Stack.Clear;
          FGroups[0].Capture.StartP := AStr;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkGoSub:
        begin
          Index := NFACode.GroupIndex;

          if Index > 0 then
          begin
            SubP := AStr;
            Index := NFACode.GroupIndex;

            LEntry := FGroups[Index].IndexBegin;
            EntryCode := FStateList[LEntry];
            while EntryCode.Kind <> nkGroupBegin do
              EntryCode := EntryCode.Next;

            LEntry := FGroups[Index].IndexEnd;
            EndCode := FStateList[LEntry];
            EndCode.Max := EndCode.Max + 1;
            EndCode.ExtendTo := NFACode.TransitTo;

            if NFACode.HasRecursion then
            begin
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
              MatchProcess(EntryCode, AStr);
{$ENDIF}
{$ENDIF}
              while EndCode.Kind <> nkGroupEnd do
                EndCode := EndCode.Next;

              if MatchRecursion(EntryCode, EndCode, AStr) then
              begin
                Result := FStateList[NFACode.TransitTo];

{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
                MatchProcess(EndCode, AStr);
{$ENDIF}
{$ENDIF}
              end
              else
              begin
                Result := nil;
                AStr := SubP;
              end;
            end
            else
            begin
              if MatchCore(EntryCode, EndCode, AStr) then
              begin
                Result := FStateList[NFACode.TransitTo];
              end
              else
              begin
                AStr := SubP;
                Result := nil;
              end;
            end;
          end
          else
          begin
            SubP := AStr;
            EntryCode := FStateList[FRegExp.FEntryState];

            if MatchCore(EntryCode, AStr) then
            begin
              Result := FStateList[NFACode.TransitTo];
            end
            else
            begin
              AStr := SubP;
              Result := nil;
            end;
          end;
        end;
      nkMatchEnd:
        begin
          Result := NFACode;
          Exit;
        end;
      nkAheadMatch:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];

          SubP := AStr;
          if MatchCore(NFACode, EndCode, SubP) then
            Result := FStateList[NFACode.TransitTo]
          else
            Result := nil;
        end;
      nkAheadNoMatch:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          Result := FStateList[EndCode.TransitTo];
          NFACode := FStateList[NFACode.TransitTo];

          SubP := AStr;
          if MatchCore(NFACode, EndCode, SubP) then
          begin
            Result := nil;
            Exit;
          end;
        end;
      nkBehindMatch:
        begin
          SubP := AStr;
          LMax := NFACode.Max;
          LMin := NFACode.Min;

          EndCode := FStateList[NFACode.ExtendTo];
          EntryCode := FStateList[NFACode.TransitTo];
          if FRegExp.FMatchTopP <= (SubP - LMax) then
            Len := LMax
          else if FRegExp.FMatchTopP <= (SubP - LMin) then
            Len := LMin
          else
            Len := 0;

          if Len > 0 then
          begin
            CharPrev(SubP, Len);
            SaveP := SubP;
            NFACode := EntryCode;

            IsMatched := MatchCore(NFACode, EndCode, SubP, False);
            while not IsMatched and (SubP < AStr) do
            begin
              SubP := SaveP;
              CharNext(SubP);
              SaveP := SubP;
              NFACode := EntryCode;
              IsMatched := MatchCore(NFACode, EndCode, SubP, False);
            end;

            if not IsMatched then
            begin
              Result := nil;
              Exit;
            end
            else
              Result := FStateList[NFACode.TransitTo];
          end
          else
          begin
            Result := nil;
            Exit;
          end;
        end;
      nkBehindNoMatch:
        begin
          SubP := AStr;
          LMax := NFACode.Max;
          LMin := NFACode.Min;

          EndCode := FStateList[NFACode.ExtendTo];
          Result := FStateList[EndCode.TransitTo];
          EntryCode := FStateList[NFACode.TransitTo];

          if FRegExp.FMatchTopP <= (SubP - LMax) then
            Len := LMax
          else if FRegExp.FMatchTopP <= (SubP - LMin) then
            Len := LMin
          else
            Len := 0;

          if Len > 0 then
          begin
            CharPrev(SubP, Len);
            SaveP := SubP;
            NFACode := EntryCode;

            IsMatched := MatchCore(NFACode, EndCode, SubP, False);
            while not IsMatched and (SubP < AStr) do
            begin
              SubP := SaveP;
              CharNext(SubP);
              SaveP := SubP;
              NFACode := EntryCode;
              IsMatched := MatchCore(NFACode, EndCode, SubP, False);
            end;

            if IsMatched then
            begin
              Result := nil;
              Exit;
            end;
          end;
        end;
      nkIfMatch:
        begin
          SubP := AStr;
          EntryCode := NFACode.Next;
          EndCode := FStateList[NFACode.ExtendTo];
          NextCode := FStateList[NFACode.TransitTo];
          if NextCode.Next <> nil then
            SubCode := FStateList[NextCode.Next.TransitTo]
          else
            SubCode := nil;

          if MatchCore(EntryCode, EndCode, SubP) then
            Result := NextCode
          else
            Result := SubCode;
        end;
    end;

{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
  finally
    if Result = nil then
      FRegExp.FMatchProcess.Add('...fail');
  end;
{$ENDIF}
{$ENDIF}
end;

{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}

procedure TREMatchEngine.MatchProcess(NFACode: TRENFAState; AStr: PWideChar);
var
  LeftStr, RightStr: REString;
begin
  if (AStr - FRegExp.FMatchTopP) > 20 then
    SetString(LeftStr, AStr - 20, 20)
  else
    SetString(LeftStr, FRegExp.FMatchTopP, AStr - FRegExp.FMatchTopP);

  if (FRegExp.FMatchEndP - AStr) > 20 then
    SetString(RightStr, AStr, (AStr + 20) - AStr)
  else
    SetString(RightStr, AStr, FRegExp.FMatchEndP - AStr);

  FRegExp.FMatchProcess.Add(Format('<%s>  <%s> : %2d: %s', [LeftStr, RightStr,
    NFACode.Index, NFACode.ToString]));
end;
{$ENDIF SHOW_MATCH_PROCESS}
{$ENDIF DEBUG}

function TREMatchEngine.MatchRecursion(var NFACode: TRENFAState;
  EndCode: TRENFAState; var AStr: PWideChar): Boolean;
var
  Stack: TREStack;
  TopP: PWideChar;
  Index: Integer;
begin
  Stack := TREStack.Create(FRegExp, True);
  try
    Result := False;
    Index := Stack.Index;
    TopP := AStr;

    NFACode := FStateList[NFACode.TransitTo];

    while NFACode <> nil do
    begin
      NFACode := MatchPrim(NFACode, AStr, Stack);

      if NFACode = nil then
      begin
        if Stack.Index > Index then
          Stack.Pop(NFACode, AStr);
      end;

      if NFACode <> nil then
      begin
        if NFACode = EndCode then
        begin
          Result := True;
          if Stack.Index > Index then
          begin
            if TopP = AStr then
              Stack.Pop(NFACode, AStr)
            else
              Break;
          end
          else
            Break;
        end;
      end;
    end;

  finally
    Stack.Free;
  end;
end;

procedure TREMatchEngine.Optimize;
begin
  OptimizeLoop;
  GenerateLeadCode;
  GenerateTailCode;
  SetupLeadStrings;
  FIsPreMatch := FLeadCode.Count > 0;
end;

procedure TREMatchEngine.OptimizeLoop;
var
  I: Integer;
  NFACode, SubCode, NextCode: TRENFAState;
begin
  for I := 0 to FStateList.Count - 1 do
  begin
    NFACode := FStateList[I];

    while NFACode <> nil do
    begin
      case NFACode.Kind of
        nkStar, nkPlus, nkBound:
          begin
            // 量指定子の次の部分式が、量指定子の対象となる文字を含むか？
            // 含まなければバックトラックの必要はないので強欲に変更。
            NextCode := FStateList[NFACode.TransitTo];

            while NextCode.Kind in [nkGroupEnd, nkGroupBegin] do
              NextCode := FStateList[NextCode.TransitTo];

            if (NextCode.Kind = nkEnd) or
              ((NextCode.Code <> nil) and not NFACode.Code.IsOverlap
              (NextCode.Code)) then
              NFACode.MatchKind := lkPossessive
          end;
        nkLoop:
          begin
            if NFACode.FMatchKind = lkReluctant then
            begin
              SubCode := FStateList[NFACode.TransitTo];
              NextCode := FStateList[SubCode.TransitTo];

              if (NextCode.Next = nil) and (NextCode.Kind = nkChar) and
                ((NextCode.Code is TRECharCode) or
                (NextCode.Code is TRELiteralCode)) then
                // NFACode.MatchKind := lkSimpleReluctant;
                { TODO: 意外に速くならない？ }
                  ;
            end;
          end;
      end;

      NFACode := NFACode.Next;
    end;
  end;
end;

function TREMatchEngine.IsLeadStrings(var AStr: PWideChar): Boolean;
var
  I, L: Integer;
begin
  Result := False;

  for I := 0 to FLeadCode.Count - 1 do
  begin
    if FLeadCode[I].Code.IsEqual(AStr + FLeadCode[I].Offset, L) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TREMatchEngine.SetupLeadStrings;
var
  NFACode, NextCode: TRENFAState;
  IsLiteral: Boolean;
  I, LOffset: Integer;
  LStr, SubStr: REString;
  LOptions: TRECompareOptions;
begin
  FLeadStrings.Clear;

  IsLiteral := False;

  FLeadCharMode := lcmNone;

  if FLeadCode.Count = 1 then
  begin
    if FLeadCode[0].IsBehindMatch then
    begin
      FPreMatchLength := FLeadCode[0].Code.CharLength;
      FIsBehindMatch := True;
    end
    else
    begin
      FIsBehindMatch := False;
      FPreMatchLength := FLeadCode[0].Offset;
    end;

    if (FLeadCode[0].Code is TRECharCode) then
    begin
      FLeadStrings.FindText := (FLeadCode[0].Code as TRECharCode).FStrings;
      FLeadStrings.Options := (FLeadCode[0].Code as TRECharCode)
        .FCompareOptions;
      IsLiteral := True;
    end
    else if (FLeadCode[0].Code is TRELiteralCode) then
    begin
      FLeadStrings.FindText := (FLeadCode[0].Code as TRELiteralCode).FStrings;
      FLeadStrings.Options := (FLeadCode[0].Code as TRELiteralCode)
        .FCompareOptions;
      IsLiteral := True;
    end
    else if (FLeadCode[0].Code is TRELineHeadCode) then
    begin
      if not(roMultiLine in (FLeadCode[0].Code as TRELineHeadCode)
        .FOptions) then
        FLeadCharMode := lcmTextTop
      else
        FLeadCharMode := lcmLineTop;
      Exit;
    end
    else if (FLeadCode[0].Code is TRETextHeadCode) then
    begin
      FLeadCharMode := lcmTextTop;
      Exit;
    end;

    if IsLiteral then
    begin
      NFACode := FStateList[FRegExp.FEntryState];
      NextCode := FStateList[NFACode.TransitTo];

      if not FIsNotSimplePattern and (NFACode.Next = nil) and
        (NextCode.Kind = nkEnd) then
        FLeadCharMode := lcmSimple
      else
        FLeadCharMode := lcmFirstLiteral;
    end
    else
      FLeadCharMode := lcmHasLead;
  end
  else if FLeadCode.Count > 1 then
  begin
    IsLiteral := True;
    LOffset := FLeadCode[0].Offset;

    for I := 0 to FLeadCode.Count - 1 do
    begin
      if (not(FLeadCode[I].Code is TRECharCode) and
        not(FLeadCode[I].Code is TRELiteralCode)) or
        (FLeadCode[I].Offset <> LOffset) then
      begin
        IsLiteral := False;
        Break;
      end;
    end;

    if IsLiteral then
    begin
      FMap.Clear;

      for I := 0 to FLeadCode.Count - 1 do
      begin
        if FLeadCode[I].Code is TRECharCode then
        begin
          LStr := (FLeadCode[I].Code as TRECharCode).FStrings;
          LOptions := (FLeadCode[I].Code as TRECharCode).FCompareOptions;
        end
        else if FLeadCode[I].Code is TRELiteralCode then
        begin
          LStr := (FLeadCode[I].Code as TRELiteralCode).FStrings;
          LOptions := (FLeadCode[I].Code as TRELiteralCode).FCompareOptions;
        end;

        FMap.Add(ToUCS4Char(@LStr[1]));

        if coIgnoreCase in LOptions then
        begin
          SubStr := AnsiUpperCase(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));
          SubStr := AnsiLowerCase(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));
        end;

        if coIgnoreWidth in LOptions then
        begin
          SubStr := ToWide(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));
          SubStr := ToHalf(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));
        end;

        if coIgnoreKana in LOptions then
        begin
          SubStr := ToHiragana(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));

          SubStr := ToKatakana(LStr);
          if SubStr <> LStr then
            FMap.Add(ToUCS4Char(@SubStr[1]));
        end;
      end;
      FLeadCharMode := lcmMap;
    end
    else
      FLeadCharMode := lcmHasLead;
  end
  else
  begin
    if FTailCode.Count = 1 then
    begin
      if (FTailCode[0] is TRECharCode) then
      begin
        FLeadStrings.FindText := (FTailCode[0] as TRECharCode).FStrings;
        FLeadStrings.Options := (FTailCode[0] as TRECharCode).FCompareOptions;
        FLeadCharMode := lcmLastLiteral;
      end
      else if (FTailCode[0] is TRELiteralCode) then
      begin
        FLeadStrings.FindText := (FTailCode[0] as TRELiteralCode).FStrings;
        FLeadStrings.Options := (FTailCode[0] as TRELiteralCode)
          .FCompareOptions;
        FLeadCharMode := lcmLastLiteral;
      end
    end;
  end;
end;

{ TSkRegExp }

procedure TSkRegExp.ClearBinCodeList;
var
  I: Integer;
begin
  for I := 0 to FBinCodeList.Count - 1 do
    if FBinCodeList[I] <> nil then
      TRECode(FBinCodeList[I]).Free;
  FBinCodeList.Clear;
end;

procedure TSkRegExp.ClearCodeList;
var
  I: Integer;
begin
  for I := 0 to FCodeList.Count - 1 do
    if FCodeList[I] <> nil then
      TRECode(FCodeList[I]).Free;
  FCodeList.Clear;
end;

{$IFDEF CHECK_MATCH_EXPLOSION}

procedure TSkRegExp.ClearExplosionState;
var
  I: Integer;
  S, D: PREMatchExplosionStateRec;
begin
  for I := 0 to Length(FMatchExplosionState) - 1 do
  begin
    S := FMatchExplosionState[I];
    while S <> nil do
    begin
      D := S.Next;
      Dispose(S);
      S := D;
    end;
    FMatchExplosionState[I] := nil;
  end;
end;
{$ENDIF}

procedure TSkRegExp.ClearStateList;
var
  I: Integer;
  Code, Next: TRENFAState;
begin
  if FStateList <> nil then
  begin
    for I := 0 to FStateList.Count - 1 do
    begin
      Code := FStateList[I];
      while Code <> nil do
      begin
        Next := Code.Next;
        Code.Free;
        Code := Next;
      end;
    end;
    FStateList.Clear;
  end;
end;

procedure TSkRegExp.Compile;
var
  Parser: TREParser;
  NFA: TRENFA;
begin
  if not FCompiled then
  begin
    ClearCodeList;
    ClearBinCodeList;
    FGroups.Clear;

    Parser := TREParser.Create(Self, FExpression);
    try
      Parser.Parse;
      NFA := TRENFA.Create(Self);
      try
        NFA.Compile;
        FMatchEngine.Optimize;
      finally
        NFA.Free;
      end;
    finally
      Parser.Free;
    end;
{$IFNDEF DEBUG}
    ClearBinCodeList;
{$ENDIF}
    FCompiled := True;
  end;
end;

constructor TSkRegExp.Create;
begin
  inherited Create;
  FGroups := TGroupCollection.Create(Self);
  FCodeList := TList.Create;
  FBinCodeList := TList.Create;
  FStateList := TList.Create;

  FOptions := SkRegExpDefaultOptions;
  SetEOL(DefaultEOL);

  IsWord := UnicodeProp.IsWord;
  IsDigit := UnicodeProp.IsDigit;
  IsSpace := UnicodeProp.IsSpace;

  FMatchEngine := TREMatchEngine.Create(Self);
  FLexMode := lmOptimize;
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
  FMatchProcess := TREStringList.Create;
{$ENDIF}
{$ENDIF}
end;

class function TSkRegExp.DecodeEscape(const S: REString): REString;
var
  StartP: PWideChar;

  function GetErrorStopedString(const ErrMes: REString; P: PWideChar): REString;
  var
    S: REString;
  begin
    Inc(P);
    SetString(S, StartP, P - StartP);
    S := S + ' <-- ';
    Result := Format(ErrMes, [S]);
  end;

var
  P: PWideChar;
  I, J, N: Integer;
begin
  Result := '';
  if S = '' then
    Exit;

  SetLength(Result, System.Length(S));
  J := 1;

  P := PWideChar(S);
  StartP := P;

  while P^ <> #0 do
  begin
    if P^ = '\' then
    begin
      Inc(P);
      case P^ of
        '0' .. '3':
          begin
            N := 0;
            for I := 1 to 3 do
            begin
              case P^ of
                '0' .. '7':
                  N := (N shl 3) + Ord(P^) - Ord('0');
              else
                Break;
              end;
              Inc(P);
            end;
            Result[J] := WideChar(N);
            Inc(J);
            Continue;
          end;
        'c':
          begin
            Inc(P);
            if ((P^ >= '@') and (P^ <= '_')) or
              ((P^ >= 'a') and (P^ <= 'z')) then
            begin
              if P^ = '\' then
              begin
                Inc(P);
                if P^ <> '\' then
                  raise ESkRegExp.Create
                    (GetErrorStopedString(sInvalidEscapeCharacterSyntax, P));
              end;

              N := Ord(P^);
              if (P^ >= 'a') and (P^ <= 'z') then
                Dec(N, $20);
              N := N xor $40;
              Result[J] := WideChar(N);
            end;
          end;
        'x':
          begin
            N := 0;
            Inc(P);

            if P^ = '{' then
            begin
              Inc(P);

              for I := 1 to 6 do
              begin
                case P^ of
                  '0' .. '9':
                    N := (N shl 4) + Ord(P^) - Ord('0');
                  'A' .. 'F':
                    N := (N shl 4) + Ord(P^) - Ord('7');
                  'a' .. 'f':
                    N := (N shl 4) + Ord(P^) - Ord('W');
                  '}':
                    Break;
                else
                  raise ESkRegExp.Create
                    (GetErrorStopedString(sHexDigitIsRequired, P));
                end;
                Inc(P);
              end;

              if P^ <> '}' then
                raise Exception.Create
                  (GetErrorStopedString(sUnmatchedCurlyBracket, P));

              Result[J] := WideChar(N);
            end
            else
            begin
              for I := 1 to 2 do
              begin
                case P^ of
                  '0' .. '9':
                    N := (N shl 4) + Ord(P^) - Ord('0');
                  'A' .. 'F':
                    N := (N shl 4) + Ord(P^) - Ord('7');
                  'a' .. 'f':
                    N := (N shl 4) + Ord(P^) - Ord('W');
                else
                  raise Exception.Create
                    (GetErrorStopedString(sHexDigitIsRequired, P));
                end;
                Inc(P);
              end;

              Result[J] := WideChar(N);
              Inc(J);

              Continue;
            end;
          end;
        't':
          Result[J] := #0009;
        'n':
          Result[J] := #$000A;
        'r':
          Result[J] := #$000D;
        'f':
          Result[J] := #$000C;
        'a':
          Result[J] := #0007;
        'e':
          Result[J] := #$001B;
      else
        Result[J] := P^;
      end;
    end
    else
      Result[J] := P^;

    Inc(P);
    Inc(J);
  end;
  SetLength(Result, J - 1);
end;

destructor TSkRegExp.Destroy;
begin
{$IFDEF CHECK_MATCH_EXPLOSION}
  ClearExplosionState;
{$ENDIF}
  ClearStateList;
  ClearBinCodeList;
  ClearCodeList;

  FMatchEngine.Free;
  FStateList.Free;
  FBinCodeList.Free;
  FCodeList.Free;
  FGroups.Free;
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
  FMatchProcess.Free;
{$ENDIF}
{$ENDIF}
  inherited;
end;

procedure TSkRegExp.DoReplaceFunc(Sender: TObject; var ReplaceWith: REString);
begin
  if Assigned(FReplaceFunc) then
    ReplaceWith := FReplaceFunc(Self);
end;

{$IFDEF DEBUG}

function TSkRegExp.DumpLeadCode: REString;
begin
  if FMatchEngine.FLeadStrings.FindText <> '' then
    Result := sFmtDumpLeadCodeExist + FMatchEngine.FLeadStrings.FindText
  else
    Result := sFmtDumpLeadCodeNotExist;
end;


procedure TSkRegExp.DumpMatchProcess(ADest: TStrings);
begin
{$IFDEF SHOW_MATCH_PROCESS}
  ADest.Text := FMatchProcess.Text;
{$ENDIF}
end;

procedure TSkRegExp.DumpNFA(ADest: TStrings);
var
  I: Integer;
  Code: TRENFAState;
  Str: REString;
begin
  ADest.Clear;
  ADest.BeginUpDate;
  for I := 0 to FStateList.Count - 1 do
  begin
    Code := FStateList[I];
    if I = FEntryState then
      Str := Format(sFmtDumpNFA_Start, [I])
    else if I = FExitState then
      Str := Format(sFmtDumpNFA_End, [I])
    else
      Str := Format(sFmtDumpNFA_Status, [I]);
    while Code <> nil do
    begin
      Str := Str + Code.GetString;

      Code := Code.Next;
    end;
    ADest.Add(Str);
  end;
  ADest.Add('');
  ADest.Add(DumpLeadCode);
  ADest.EndUpDate;
end;

procedure TSkRegExp.DumpParse(TreeView: TTreeView);

  function Add(Node: TTreeNode; const S: REString): TTreeNode;
  begin
    Result := TreeView.Items.AddChild(Node, Format('%s', [S]));
  end;

  procedure DumpParseSub(Code: TRECode; Node: TTreeNode);
  var
    ANode: TTreeNode;
  begin
    if Code is TREBinCode then
    begin
      with Code as TREBinCode do
      begin
        case Op of
          opUnion:
            ANode := Add(Node, sBinCode_Union);
          opConcat:
            ANode := Add(Node, sBinCode_Concat);
          opEmply:
            ANode := Add(Node, sBinCode_Emply);
          opLoop:
            ANode := Add(Node, sBinCode_Loop);
          opPlus:
            ANode := Add(Node, sBinCode_Plus);
          opStar:
            ANode := Add(Node, sBinCode_Star);
          opQuest:
            ANode := Add(Node, sBinCode_Quest);
          opBound:
            ANode := Add(Node, sBinCode_Bound);
          opLHead:
            ANode := Add(Node, sBinCode_LHead);
          opLTail:
            ANode := Add(Node, sBinCode_LTail);
          opGroup:
            ANode := Add(Node, sBinCode_Group);
          opNoBackTrack:
            ANode := Add(Node, sBinCode_NoBackTrack);
          opKeepPattern:
            ANode := Add(Node, sBinCode_KeepPattern);
          opFail:
            ANode := Add(Node, sBinCode_Fail);
          opAheadMatch:
            ANode := Add(Node, sBinCode_AheadMatch);
          opBehindMatch:
            ANode := Add(Node, sBinCode_BehindMatch);
          opAheadNoMatch:
            ANode := Add(Node, sBinCode_AheadNoMatch);
          opBehindNoMatch:
            ANode := Add(Node, sBinCode_BehindNoMatch);
          opGoSub:
            ANode := Add(Node, sBinCode_GroupCall);
          opIfMatch:
            ANode := Add(Node, sBinCode_IfMatch);
          opIfThen:
            ANode := Add(Node, sBinCode_IfThen);
        else
          raise ESkRegExp.Create(sBinCode_Raise);
        end;
        if Left <> nil then
          DumpParseSub(Left, ANode);
        if Right <> nil then
          DumpParseSub(Right, ANode);
      end;
    end
    else
      TreeView.Items.AddChild(Node, (Code as TRECode).GetDebugStr);
  end;

begin
  TreeView.Items.Clear;
  DumpParseSub(FCode, nil);
  TreeView.FullExpand;
end;

{$ENDIF}

function TSkRegExp.Exec(const AInputStr: REString): Boolean;
var
  P: PWideChar;
begin
  if not FCompiled then
    Compile;

  SetInputString(AInputStr);

  FMatchTopP := FTextTopP;
  FMatchEndP := FTextEndP;

  P := FTextTopP;

  Result := MatchCore(P);
end;

function TSkRegExp.ExecNext: Boolean;
var
  P: PWideChar;
begin
  Result := False;

  if not FCompiled then
    Error(sExecFuncNotCall);

  if not FSuccess then
    Exit;

  if FGlobalEndP - FGlobalStartP > 0 then
    P := FGlobalEndP
  else
    P := FGlobalEndP + 1;

  Result := MatchCore(P);
end;

function TSkRegExp.ExecPos(AOffset, AMaxLength: Integer): Boolean;
var
  P: PWideChar;
  L: Integer;
begin
  Result := False;

  if (AOffset < 1) then
    Exit;

  if FMatchTopP = nil then
    Exit;

  if not FCompiled then
    Compile;

  if AOffset > 1 then
  begin
    P := FTextTopP + AOffset - 1;
    // Pが改行文字だった場合の調整
    if IsEOL(P, L) then
      Inc(P, L)
    else if (FEOLLen = 2) and (P^ = FEOLTailP^) then
      Inc(P)
    else if IsLineSeparator(P^) then
      Inc(P);
  end
  else
    P := FTextTopP;

  if AMaxLength > 0 then
  begin
    if AOffset + AMaxLength > Length(FInputString) then
      FMatchEndP := FTextEndP
    else
      FMatchEndP := P + AMaxLength;

    // ALength が指定されたときは、指定範囲内が文字列全体だと扱う。
    FMatchTopP := P;
  end
  else
  begin
    FMatchEndP := FTextEndP;
    FMatchTopP := FTextTopP;
  end;

  Result := MatchCore(P);
end;

{$IFDEF JapaneseExt}

function TSkRegExp.GetIgnoreZenHan: Boolean;
begin
  Result := (roIgnoreWidth in FOptions) and (roIgnoreKana in FOptions);
end;

{$ENDIF JapaneseExt}

function TSkRegExp.GetIndexFromGroupName(Name: REString): Integer;
var
  LIntArray: TIntDynArray;
begin
  LIntArray := FGroups.EnumIndexOfName(Name);

  for Result in LIntArray do
    if FGroups[Result].Capture.Matched then
      Exit;
  if Length(LIntArray) > 0 then
    Result := LIntArray[0]
  else
    Result := -1;
end;

function TSkRegExp.GetMatchLen(Index: Integer): Integer;
begin
  Result := FGroups[Index].Length;
end;

function TSkRegExp.GetMatchPos(Index: Integer): Integer;
begin
  Result := FGroups[Index].Index;
end;

function TSkRegExp.GetMatchStr(Index: Integer): REString;
begin
  Result := FGroups[Index].Strings;
end;

function TSkRegExp.GetOptions(const Index: Integer): Boolean;
var
  LOption: TREOption;
begin
  case Index of
    0:
      LOption := roIgnoreCase;
    1:
      LOption := roMultiLine;
    2:
      LOption := roNamedGroupOnly;
    3:
      LOption := roSingleLine;
    4:
      LOption := roExtended;
    5:
      LOption := roIgnoreWidth;
  else
    LOption := roIgnoreKana;
  end;
  Result := LOption in FOptions;
end;

function TSkRegExp.GetNamedGroupLen(Name: REString): Integer;
var
  LGroup: TGroup;
begin
  LGroup := FGroups.Names[Name];
  if LGroup <> nil then
    Result := FGroups.Names[Name].Length
  else
    Result := -1;
end;

function TSkRegExp.GetNamedGroupPos(Name: REString): Integer;
var
  LGroup: TGroup;
begin
  LGroup := FGroups.Names[Name];
  if LGroup <> nil then
    Result := FGroups.Names[Name].Index
  else
    Result := -1;
end;

function TSkRegExp.GetNamedGroupStr(Name: REString): REString;
var
  LGroup: TGroup;
begin
  LGroup := FGroups.Names[Name];
  if LGroup <> nil then
    Result := FGroups.Names[Name].Strings
  else
    Result := '';
end;

function TSkRegExp.GetDefineCharClassLegacy: Boolean;
begin
  Result := roDefinedCharClassLegacy in FOptions;
end;

function TSkRegExp.GetGroupCount: Integer;
begin
  Result := FGroups.Count - 1;
end;

function TSkRegExp.GetGroupNameFromIndex(Index: Integer): REString;
begin
  Result := FGroups[Index].GroupName;
end;

function TSkRegExp.GetVersion: REString;
begin
  Result := CONST_VERSION;
end;

function TSkRegExp.IsEOL(AStr: PWideChar; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  if AStr^ = FEOLHeadP^ then
  begin
    if (FEOLLen = 2) and ((AStr + 1)^ = FEOLTailP^) then
      Len := 2
    else
      Len := 1;
    Result := True;
  end;
end;

function TSkRegExp.MatchCore(AStr: PWideChar): Boolean;
begin
{$IFDEF DEBUG}
{$IFDEF SHOW_MATCH_PROCESS}
  FMatchProcess.Clear;
{$ENDIF}
{$ENDIF}
  FSuccess := FMatchEngine.Match(AStr);
  if FSuccess then
  begin
    FGlobalStartP := FGroups[0].Capture.StartP;
    FGlobalEndP := FGroups[0].Capture.EndP;
    if Assigned(FOnMatch) then
      FOnMatch(Self);
  end
  else
  begin
    FGlobalStartP := FMatchTopP;
    FGlobalEndP := FMatchTopP;
  end;
  Result := FSuccess;
end;

class function TSkRegExp.RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions): Boolean;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    R.NamedGroupOnly := True;
    Result := R.Exec(AInputStr);
  finally
    R.Free;
  end;
end;

class function TSkRegExp.RegMatch(const ARegExpStr, AInputStr: REString;
  AMatches: TREStrings; AOptions: TREOptions): Boolean;
var
  R: TSkRegExp;
  I: Integer;
begin
  R := TSkRegExp.Create;
  try
    AMatches.Clear;
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    if R.Exec(AInputStr) then
    begin
      for I := 0 to R.GroupCount do
        AMatches.Add(R.Match[I]);
      Result := True;
    end
    else
      Result := False;
  finally
    R.Free;
  end;
end;

class function TSkRegExp.RegReplace(const ARegExpStr, AInputStr: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; AOptions: TREOptions): REString;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.Options := AOptions;
    R.Expression := ARegExpStr;
    Result := R.Replace(AInputStr, AReplaceFunc)
  finally
    R.Free;
  end;
end;

class function TSkRegExp.RegReplace(const ARegExpStr, AInputStr,
  AReplaceStr: REString; AOptions: TREOptions): REString;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    Result := R.Replace(AInputStr, AReplaceStr);
  finally
    R.Free;
  end;
end;

class procedure TSkRegExp.RegSplit(const ARegExpStr, AInputStr: REString;
  APieces: TREStrings; AOptions: TREOptions);
var
  R: TSkRegExp;
begin
  APieces.Clear;

  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    R.Split(AInputStr, APieces);
  finally
    R.Free;
  end;
end;

function TSkRegExp.Replace(const Input: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; Count, AOffset: Integer): REString;
begin
  FReplaceFunc := AReplaceFunc;
  FOnReplace := DoReplaceFunc;
  try
    Result := Replace(Input, '', Count, AOffset);
  finally
    FOnReplace := nil;
    FReplaceFunc := nil;
  end;
end;

function TSkRegExp.Replace(const Input, Replacement: REString;
  Count, AOffset: Integer): REString;
var
  Index, LCount: Integer;
  RepStr: REString;
  LReplacement: REString;
begin
  Result := '';
  LCount := 0;
  InputString := Input;
  Index := 1;
  LReplacement := DecodeEscape(Replacement);

  if ExecPos(AOffset) then
  begin
    repeat
      if MatchLen[0] > 0 then
      begin
        if (Count > 0) then
        begin
          Inc(LCount);
          if (LCount > Count) then
            Break;
        end;

        RepStr := Substitute(LReplacement);
        if Assigned(FOnReplace) then
          FOnReplace(Self, RepStr);

        Result := Result + Copy(Input, Index, MatchPos[0] - Index) + RepStr;
        Index := MatchPos[0] + MatchLen[0];
      end;
    until not ExecNext;
  end;

  Result := Result + Copy(Input, Index, MaxInt);
end;

class function TSkRegExp.EncodeEscape(const Str: REString): REString;
var
  StartP: PWideChar;

  function GetErrorStopedString(const ErrMes: REString; P: PWideChar): REString;
  var
    S: REString;
  begin
    Inc(P);
    SetString(S, StartP, P - StartP);
    S := S + ' <-- ';
    Result := Format(ErrMes, [S]);
  end;

var
  P: PWideChar;
begin
  if Str = '' then
    Exit;

  P := PWideChar(Str);
  StartP := P;

  while P^ <> #0 do
  begin
    case P^ of
      #0009:
        Result := Result + '\t';
      #$000A:
        Result := Result + '\n';
      #$000D:
        Result := Result + '\r';
      #$000C:
        Result := Result + '\f';
      #$0007:
        Result := Result + '\a';
      #$001B:
        Result := Result + '\e';
    else
      begin
        if (P^ >= #$0000) and (P^ <= #$0020) then
          Result := Result + Format('\x%.2x', [Ord(P^)])
        else
          Result := Result + P^;
      end;
    end;
    Inc(P);
  end;
end;

procedure TSkRegExp.Error(const ErrorMes: REString);
begin
  raise ESkRegExpRuntime.Create(ErrorMes);
end;

procedure TSkRegExp.SetDefineCharClassLegacy(const Value: Boolean);
begin
  if Value then
  begin
    if not(roDefinedCharClassLegacy in FOptions) then
    begin
      IsWord := IsAnkWord;
      IsDigit := IsAnkDigit;
      IsSpace := IsAnkSpace;

      FCompiled := False;
      Include(FOptions, roDefinedCharClassLegacy);
    end;
  end
  else
  begin
    if roDefinedCharClassLegacy in FOptions then
    begin
      IsWord := UnicodeProp.IsWord;
      IsDigit := UnicodeProp.IsDigit;
      IsSpace := UnicodeProp.IsSpace;

      FCompiled := False;
      Exclude(FOptions, roDefinedCharClassLegacy);
    end;
  end;
end;

procedure TSkRegExp.SetEOL(const Value: REString);
begin
  if FEOL <> Value then
  begin
    FEOL := Value;
    FEOLLen := Length(FEOL);
    if FEOLLen > 2 then
      Error(sEOLRangeOver);

    if FEOLLen = 2 then
    begin
      FEOLHeadP := @FEOL[1];
      FEOLTailP := @FEOL[2];
    end
    else
    begin
      FEOLHeadP := @FEOL[1];
      FEOLTailP := nil;
    end;
    FCompiled := False;
  end;
end;

procedure TSkRegExp.SetExpression(const Value: REString);
begin
  if FExpression <> Value then
  begin
    FExpression := Value;
    FCompiled := False;
  end;
end;

{$IFDEF JapaneseExt}

procedure TSkRegExp.SetIgnoreZenHan(const Value: Boolean);
begin
  IgnoreWidth := Value;
  IgnoreKana := Value;
end;
{$ENDIF}

procedure TSkRegExp.SetInputString(const Value: REString);
var
  L: Integer;
begin
  FInputString := Value;
  L := Length(FInputString);
{$IFDEF CHECK_MATCH_EXPLOSION}
  ClearExplosionState;
  SetLength(FMatchExplosionState, L + 1);
{$ENDIF}
  FTextTopP := PWideChar(FInputString);
  FTextEndP := FTextTopP + L;
  FSuccess := False;
  FMatchTopP := FTextTopP;
  FMatchEndP := FTextEndP;
  FGlobalStartP := FTextTopP;
  FGlobalEndP := FTextTopP;
  FGroups.Reset;
end;

procedure TSkRegExp.SetLexMode(const Value: TRELexMode);
begin
  FLexMode := Value;
end;

procedure TSkRegExp.SetOptions(const Index: Integer; const Value: Boolean);
var
  LOption: TREOption;
begin
  case Index of
    0:
      LOption := roIgnoreCase;
    1:
      LOption := roMultiLine;
    2:
      LOption := roNamedGroupOnly;
    3:
      LOption := roSingleLine;
    4:
      LOption := roExtended;
    5:
      LOption := roIgnoreWidth;
  else
    LOption := roIgnoreKana;
  end;

  if (LOption = roNone) or (Value and (LOption in FOptions)) or
    (not Value and not(LOption in FOptions)) then
    Exit;

  if Value then
    Include(FOptions, LOption)
  else
    Exclude(FOptions, LOption);

  FCompiled := False;
end;

procedure TSkRegExp.Split(const Input: REString; APieces: TREStrings;
  Count, AOffset: Integer);
var
  Index, LCount: Integer;
  S: REString;
begin
  Index := 1;
  LCount := 0;
  APieces.Clear;

  InputString := Input;

  if ExecPos(AOffset) then
  begin
    repeat
      if MatchLen[0] > 0 then
      begin
        S := Copy(Input, Index, MatchPos[0] - Index);
        APieces.Add(S);

        Index := MatchPos[0] + MatchLen[0];

        Inc(LCount);
        if (Count > 0) and (LCount >= Count - 1) then
          Break;
      end
      until not ExecNext;

    APieces.Add(Copy(Input, Index, MaxInt));
  end;
end;

function TSkRegExp.Substitute(const ATemplate: REString): REString;
var
  K: Integer;
  LGroupName: REString;
  I, L: Integer;
begin
  Result := '';
  if ATemplate = '' then
    Exit;

  if not FSuccess then
    Exit;

  I := 1;
  L := System.Length(ATemplate);

  while I <= L do
  begin
    if ATemplate[I] = '$' then
    begin
      Inc(I);
      if (ATemplate[I] >= '0') and (ATemplate[I] <= '9') then
      begin
        K := (Integer(ATemplate[I]) - Integer('0'));
        if K <= FGroups.Count - 1 then
          Result := Result + FGroups[K].Strings;
      end
      else if ATemplate[I] = '{' then
      begin
        Inc(I);
        LGroupName := '';
        while (ATemplate[I] <> '}') and (ATemplate[I] <> #0000) do
        begin
          LGroupName := LGroupName + ATemplate[I];
          Inc(I);
        end;

        if FGroups.IndexOfName(LGroupName) <> -1 then
          Result := Result + FGroups.Names[LGroupName].Strings;
      end
      else if ATemplate[I] = '&' then
        Result := Result + FGroups[0].Strings
      else if ATemplate[I] = '$' then
        Result := Result + '$'
      else if ATemplate[I] = '`' then
      begin
        Result := Result + Copy(FInputString, 1, FGroups[0].Index - 1);
      end
      else if ATemplate[I] = '''' then
      begin
        Result := Result + Copy(FInputString, FGroups[0].
          Index + FGroups[0].Length, MaxInt);
      end
      else if ATemplate[I] = '_' then
      begin
        Result := Result + FInputString;
      end
      else if ATemplate[I] = '+' then
      begin
        for I := GroupCount downto 1 do
        begin
          if FGroups[I].Index > 0 then
          begin
            Result := Result + FGroups[I].Strings;
            Break;
          end;
        end;
      end
      else
        Result := Result + ATemplate[I];
    end
    else
      Result := Result + ATemplate[I];

    Inc(I);
  end;
end;

end.

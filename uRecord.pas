unit uRecord;

interface

const
  kTrue:  integer = -1;
  kFalse: Integer =  0;
  kkHz: Double = 1000;
  kMHz: Double = 1000000;

  kGlobeRadius: double = 6367.0;   // 地球の半径  km
  kFileAction: array[0..3] of string = ('インポート','アペンド','エクスポート','アップデート');
  kQsoStatus: array[0..6] of String =('Clear', 'Setting', 'Browse', 'Edit', 'Tramp', 'Insert', 'Close');
  kBand: array[0..2] of String = ('Band', 'Band_F', 'Band_M');
//type
//  TAct=(actCheck,actSelect,actInsert,actUpdate,actDelete);

type
  TProcessing=(prRealTime, prBatch);
  TQsoState=(lgClear,lgSetting,lgBrowse,lgEdit,lgTramp,lgInsert,lgClose);
  TDataInput=(diManual, diAuto);
  TFileAction=(faImport, faAppend, faExport, faUpdate);
  TBand=(bBand, bBand_F, bBand_M);
  TPtt=(PttToggle, PttOff, PttOn);

type TQso = record                 //  現在QSOの内容でDBに保存しないField
    Area:           string;
    CountryName:    string;
    RegionName:     string;
    RegionPhonetic: string;
    EntityName:     string;
    IOTAName:       string;
    EtcName:        array[1..5] of string;
    Latitude:       string;
    Longitude:      string;
    Comment:        string;
    isJarlMember:   boolean;
    isLicensed:     boolean;

    FreqFormat:     string;
    DefaultFreqFormat:  string;
    DefaultMode:    string;
    DefaultReport:  string;

    MyCallsign:     string;
    MyEntity:       string;
    MyCountry:      string;
    MyRegion:       string;
    MyGridLoc:      string;
    MyRIG:          string;
    MyAnt:          string;
    MyMemo:         string;
    MyCountryName:  string;
    MyRegionName:   string;
    MyEntityName:   string;
    MyLatitude:     string;
    MyLongitude:    string;
    MyArea:         string;
    result:         boolean;
  end;

type TPastQso = record           //  同一 Callsignでの前の値
    Callsign:       string;
    OrgCallsign:    string;
    Name:           string;
    Entity:         string;
    Country:        string;
    Region:         string;
    Continent:      string;
    ItuZone:        string;
    CQZone:         string;
    Iota:           string;
    Qsl:            string;
    QSLManager:     string;
    result:         boolean;
  end;

type TPrevQso = record             //  直前のQSO内容
    Num:            integer;
    CQ:             smallint;
    OnDateTime:     TDateTime;
    Freq:           Int64;
    RecvFreq:       Int64;
    Route:          string;
    Repeater:       string;
    Mode:           string;
    HisReport:      string;
    MyReport:       string;
  end;

type TMyDataRec = record           //  TMｙDataの値
    MyCallsign:     string;
    MyEntity:       string;
    MyCountry:      string;
    MyRegion:       string;
    MyGridLoc:      string;
    MyRIG:          string;
    MyAnt:          string;
    MyMemo:         string;
    MyCountryName:  string;
    MyRegionName:   string;
    MyEntityName:   string;
    MyLatitude:     string;
    MyLongitude:    string;
    MyArea:         string;
  end;

Type
    TOptionSummary = Record
      Callsign:       Integer;
      Entity:         Integer;
      Region:         Integer;
      Prefix:         Integer;
      Suffix:         Integer;
      JCA:            Integer;
      NotJAExec:      boolean;
      Band:           array[0..20] of boolean;
    End;

type
    POptionsData = ^TOptionsData;
    TOptionsData = record
      Callsign:           string;
      Name:               string;
      Address:            string;
      Country:            string;
      Region:             string;
      Entity:             string;
      Longitude:          Double;
      Latitude:           Double;
      GridLocator:        string;

      DbPath:             string;
      AutoBackupPath1:    string;
      AutoBackupPath2:    string;
      AutoBackup:         boolean;
      BackupGeneration:   Integer;
      JournalGeneration:  Integer;
      Precedence:         boolean;
      RealTimeInput:      boolean;
      CopyOnDate:         boolean;
      LogTabOrder:        string;
      DisplayItems:       string;          // conmma text
      NonDisplayItems:    string;          // conmma text
      Internet:           boolean;
      Jarl:               boolean;
      JarlUrl:            string;
      Mic:                boolean;
      MicUrl:             string;
      LoTw:               boolean;
      LotwLocation:       string;
      LotwIntervalMin:    Integer;
      Summary:            TOptionSummary;
    end;

type TCallsignRec = record
    Callsign:       string;
    OrgCallsign:    string;
    Prefix:         string;
    Suffix:         string;
    Area:           string;
    result:         boolean;
  end;

type TCallbook = record
    Callsign:       string;
    Name:           string;
    Entity:         string;
    Country:        string;
    Region:         string;
    Comment:        string;
    result:         boolean;
  end;


type TCallbookEx = record
    Callsign:       string;
    OnDate:         tDate;
    Country:        string;
    Region:         string;
    Entity:         string;
    GridLoc:        string;
    Continent:      string;
    ITUZone:        string;
    CQZone:         string;
    Latitude:       string;
    Longitude:      string;
    result:         boolean;
  end;

type TCountryRec = record
    Country:        string;
    Name:           string;
    FmDate:         TDateTime;
    Name_jp:        string;
    Result:         boolean;
  end;

type TEtcRec = record
    Table:          string;
    Code:           string;
    Name:           string;
    FmDate:         TDateTime;
    Result:         boolean;
  end;

type TFreqRec = record
    Freq:           Int64;
    Band:           Int64;
    DefaultMode:    string;
    Result:         boolean;
  end;

type TIotaRec = record
    Entity:         string;
    Iota:           string;
    Name:           string;
    Result:         boolean;
  end;

type TModeRec = record
    Mode:           string;
    Report:         string;
    ReportRegEx:    string;
    DefaultReport:  string;
    FreqFormat:     string;
    Result:         boolean;
  end;

type TEntityRec = record
    Entity:         string;
    Name:           string;
    EntityCode:     string;
    Country:        string;
    Continent:      string;
    ITUZone:        string;
    CQZone:         string;
    Latitude:       string;
    Longitude:      string;
    Continents:     string;
    ITUZones:       string;
    CQZones:        string;
    HamLog:         string;
    result:         boolean;
  end;

type THamLog = record
    HamLog:         string;
    Entity:         string;
    result:         boolean;
  end;

type TRegionRec = record
    Country:        string;
    Region:         string;
    Name:           string;
    Name1:          string;
    Name2:          string;
    Name3:          string;
    Phonetic:       string;
    Rank:           string;
    Latitude:       string;
    Longitude:      string;
    FmDate:         TDate;
    ToDate:         TDate;
    result:         boolean;
  end;

type TRepeaterRec = record
    Route:          string;
    Repeater:       string;
    UplinkFreq:     Int64;
    DownlinkFreq:   Int64;
    result:         boolean;
  end;

type TRouteRec = record
    Route:          string;
    NeedRepeter:    boolean;
    result:         boolean;
  end;

type TZipCodeRec = record
    Country:        string;
    ZipCode:        string;
    Region:         string;
    Name:           string;
    PrimaryCity:    string;
    result:         boolean;
  end;

type TFilterKind = (flNone, flCallsign, flEntity, flRegion, flPrefix, flNum,
                   flOnDate, flMemo);

type TFilter = record
    Kind:       TFilterKind;
    Callsign:   string;
    Entity:     string;
    Country:    string;
    Region:     string;
    FmOnDate:   TDate;
    ToOnDate:   TDate;
    FmNum:      integer;
    ToNum:      integer;
    Prefix:     string;
    Suffix:     string;
    FmDate:     boolean;
    Memo1:      string;
    Memo2:      string;
    IsCondOr:   boolean;
    Sql:        string;
  end;



type TGridLocRec = record
    GridLoc: string;
    Longitude: Double;
    Latitude:  Double;
    end;

//type
//  TQslState=(smNone, smQso, smEQsl, smQsl);

type TSummary = record
      Kind:       integer;
      Callsign:   string;
      Entity:     string;
      Country:    string;
      Region:     string;
      Prefix:     string;
      Suffix:     string;
  end;

type
  TQslState = (qsNone, qsQso, qsEQsl, qsQsl);

type
  TDisplay = (dsByMode, dsByCollMode, dsAllModeOnly, dsNone);

type
  TCategory = (ctCallsign, ctEntity, ctRegion, ctPrefix, ctSuffix, ctJCA, ctIota, ctGridLoc);

type TFindJpnKind = (fjPrefecture, fjCity, fjCounty, fjWard, fjTown, fjVillage);

type TFindUsaKind = (fuState, fuCounty, fuZipCode);

type TFindJpnRec = record
    Kind:         TFindJpnKind;
//    Key: word;
    Area:         string;
    Date:         TDate;
    Region:       string;
    RegionName:   string;
    Latitude:     string;
    Longitude:    string;
    end;

type TFindKind = (fiNone, fiCallsign, fiEntity, fiRegion, fiPrefix, fiGridLoc,
                  fiIota, fiNum, fiOnDate, fiMemo);

type TFindLogRec = record
    Kind:         TFindKind;
    Callsign:     string;
    Entity:       string;
    Country:      string;
    Region:       string;
    Prefix:       string;
    Suffix:       string;
    GridLoc:      string;
    Iota:         string;
    FmNum:        integer;
    ToNum:        Integer;
    OnDateTime:   tDateTime;
    Memo:         string;
    end;



implementation

end.

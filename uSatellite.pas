unit uSatellite;

//////////////////////////////////////////////////////////////////////////
//
//    参考資料
//      １．天体の位置計算 増補版   著者：長沢工氏　　発行：地人書館
//      2.  Calsat32のホームぺージ  作者：相田政則氏（JR1HUO）
//             http://homepage1.nifty.com/aida/jr1huo_calsat32/index.html
//
//////////////////////////////////////////////////////////////////////////

{ 用語
    Orbital Elments:    軌道要素
    Right Aceention of Ascending NODE(RAAN);   昇交点赤経(度)　
    Inclination:        軌道傾斜角（Rad)
    Argument of Perigee(ARGP):   近地点引数


    Epoch:              元期
    Eccentricity:       離心率
    Mean Anomaly:       平均近点角(Rad)
    Mean Motion:        平均運動（rad/日）
    Decay Rate:         衰弱率 平均運動の変化 摂動1
    Semi Major Axis:    長半径
    EpochRev:           周回番号

}

interface

uses Classes, SysUtils, Dialogs, Math, DateUtils, System.UITypes;

type
//**********************************************************************//
//                                                                      //
//  観測地点（運用地点）クラス                                          //
//                                                                      //
//**********************************************************************//
  TObservationPoint = record
    Longitude: double;         //  経度
    Latitude: double;          //  緯度
    Altitude: double;          //  標高
    TimeOffset: double;        //  時差
    LocalTimeStr: string;      //  JST等の文字列
  end;

//**********************************************************************//
//                                                                      //
//  球面座標クラス                                                　　　//
//                                                                      //
//**********************************************************************//
  TSphericalCoord = record
    Azimuth: double;            // 方位角
    Elevation: double;          // 仰角
    Range: double;              // 距離
    end;

//**********************************************************************//
//                                                                      //
//  直交座標クラス                                                　　　//
//                                                                      //
//**********************************************************************//
  T2DCoordinates = record
    X: double;
    Y: double;
  end;

  T3DCoordinates = record
    X: double;
    Y: double;
    Z: double;
    end;

//**********************************************************************//
//                                                                      //
//  衛星の軌道要素クラス                                                //
//                                                                      //
//**********************************************************************//
  TOrbitalElments = record
    Satellite: String;         // 衛星名
    Epoch_Y: Integer;          // 元期(年)
    Epoch_D: double;           // 元期(連続日、時間)
    DecayRate: double;         // n1: 衰弱率                 Decay Rate, Drug 平均運動の変化 摂動1
    Inclination: double;       // i:  軌道傾斜角（Rad)       Inclination
    RAAN:  double;             // Ω: 昇交点赤経(度)　       Right Aceention of Ascending NODE
    Eccentricity: double;      // e:  離心率                 Eccentricity
    ARGP:  double;             // ω: 近地点引数             Argument of Perigee
    MeanAnomaly: double;       // M or l:  平均近点角(Rad)   Mean Anomaly
    MeanMotion: double;        // n:  平均運動（rad/日）     Mean Motion
    EpochRev:Integer;          // 周回番号
    Epoch: TDateTime;          // 元期(年月日時分・・・)
  end;

//**********************************************************************//
//                                                                      //
//  衛星の軌道クラス                                                　　//
//                                                                      //
//**********************************************************************//
  TOrbit = record
    Satellite: String;          // 衛星名
    Epoch_Y: Integer;           // 元期(年)
    Epoch_D: double;            // 元期(連続日、時間)
    DecayRate: double;          // n1: 衰弱率                 Decay Rate, Drug 平均運動の変化 摂動1
    Inclination: double;        // i:  軌道傾斜角（Rad)       Inclination
    RAAN:  double;              // Ω: 昇交点赤経(度)　       Right Aceention of Ascending NODE
    Eccentricity: double;       // e:  離心率                 Eccentricity
    ARGP:  double;              // ω: 近地点引数             Argument of Perigee
    MeanAnomaly: double;        // M or l:  平均近点角(Rad)   Mean Anomaly
    MeanMotion: double;         // n: 平均運動（rad/日）     Mean Motion
    EpochRev:Integer;           // 周回番号
    Epoch: TDateTime;           // 元期
//  ここまでは、 TOrbitalElmentsto同じ項目順
    EpochMJD: Double;
    MeanMotionS: double;        // n: 平均運動（rad/Sec）     Mean Motion
    ElapsedTime: TDateTime;     // 経過時間
    EccentricAanomaly: double;  // 離心近点角
    MajorAxis: double;          // 軌道長半径
    MinorAxis: double;          // 軌道短半径
    PrecessionConst: double;    // 歳差定数

    OrbitalSurface: T2DCoordinates;   // 軌道面内のx,y座標
    EquatorialCoord: T3DCoordinates;  // 地心赤道直交座標
    GGeocentricCoord: T3DCoordinates; // G径地心直交座標
    SatCoord: TSphericalCoord;        // 衛星の方位角,仰角、距離
    AngleMomentumRate: Double;
    NodePrecessionRate: double;
    PerigeePrecessionRate: double;
  end;

//**********************************************************************//
//                                                                      //
//  衛星の可視時間帯クラス                                              //
//                                                                      //
//**********************************************************************//
  TVisibleTimeSlot = record
    AosAz: double;
    MaxAz: double;
    MaxEl: double;
    LosAz: double;
    AosDateTime: TDateTime;
    MaxDateTime: TDateTime;
    LosDateTime: TDateTime;
  end;

//**********************************************************************//
//                                                                      //
//  軌道計算のクラス                                                    //
//                                                                      //
//**********************************************************************//
type
  TSatellite = class(TComponent)
  private
    { Private 宣言 }
// 定数式で表せない定数。constructorで設定する
    GravityConst23: double;     //  地心重力定数 の 2/3乗
    EquatorRadius2: double;
    EarthEccentricty2: double;  //  地球の離心率の2乗
    PolarRadius:    double;
    PolarRadius2: double;
    EarthLotation: double;      //  地球の回転数(Rad/Day)
    EarthLotationD: double;     //      回転数(Rad/Day)
    EarthLotationS: double;     //  回転数(Rad/Sec)

    ALON, ALAT: double;         // Spin Axis Attitude
//    Ax, Ay, Az: double;
//    Ox,Oy,Oz: double;
//    Ux,Uy,Uz: double;
//    Ex,Nx,Ey,Ny,Ez,Nz: double;
//
//  軌道計算(AOS,Los,Max)計算の為
//    OrbitEle_N: TOrbitalElments;           //  衛星の現在位置

    Az_d, El_d: double;

//  観測地点の中間計算値
//    SinISinK, SinICosK, CosISinK, CosICosK: double;

//  衛星の中間計算値
    SinRAAN, CosRAAN, SinARGP, cosARGP: double;

    FObsPoint: TObservationPoint;
    FOrbitalElments: TOrbitalElments;
    FOrbit: TOrbit;

    FElementFile: string;
    FGreenwichSiderealTime: TDateTime;
    FEccentricAnomaly: double;
    FRadiusVector: double;
    FTrueAnormaly: double;
    FCartesianCoordinatesX: Double;
    FCartesianCoordinatesY: Double;
    FTVisibleTimeSlot: TVisibleTimeSlot;
    FVisibleTimeSlot: TVisibleTimeSlot;
//    FJCartesianCoord: TCartesianCoord;
    FGCartesianCoord: T3DCoordinates;
    FCenteredCartesianCoord: T3DCoordinates;

    procedure SetElementFile(const Value: string);
    procedure InitOrbitPlane;

//    procedure setGreenwichSiderealTime(const Value: read);
//    function SetOrbitaEle(FileName, Sat: string): boolean;


    procedure SetOrbitalElments(const Value: TOrbitalElments);



  public
    { Public 宣言 }
    property ElementFile: string read FElementFile write SetElementFile;
    property ObsPoint: TObservationPoint read FObsPoint;
    property OrbitalElments: TOrbitalElments read FOrbitalElments write SetOrbitalElments;
    property Orbit: TOrbit read FOrbit;
    property GCartesianCoord: T3DCoordinates read FGCartesianCoord;  // 観測点G系地心直交座標

//    property DebugKeplerEle: TOrbitalElments read FDebugKeplerEle write SetDebugKeplerEle;
    property VisibleTimeSlot: TVisibleTimeSlot read FVisibleTimeSlot;
    property GCenteredCartesianCoord: T3DCoordinates read FCenteredCartesianCoord;  // G系測心直交座標系
    property GreenwichSiderealTime: TDateTime read FGreenwichSiderealTime;   // グリニッチ平均恒星時
    property EccentricAnomaly: double read FEccentricAnomaly;   //離心近点角
    property TrueAnormaly: double read FTrueAnormaly;   // 真近点角
    property RadiusVector: double read FRadiusVector;   // 動径
    property CartesianCoordinatesX: Double read FCartesianCoordinatesX;   // 衛星軌道面の位置直交座標 (近地点方向)
    property CartesianCoordinatesY: Double read FCartesianCoordinatesY;   // 衛星軌道面の位置直交座標 (Xの反時計回り90度方向)
    constructor Create(AOwner: TComponent); override;

    function SetOrbitaEle(Sat: string): boolean;
//    function CalcGreenwichSiderealTime(const Value: TDateTime): TDateTime;
//    procedure CalcOrbitPlane(MajorAxis, Eccentricity, MeanAnomaly: double);
    procedure CalcAosLos(DT: TDateTime);
    procedure CalcOrbit(Ut: TDateTime);

    procedure SetObsPoint(Longitude, Latitude, Altitude, TimeOffset: Double; LocalTimeStr: string);
    procedure SetObsPoint1(ObsPoint: TObservationPoint);
    procedure CalcObsPoint();




  end;

function DateTimeToMJD(DT: TDateTime): double;
function MJDToDateTime(MJD: Double): TDateTime;
function Kepler(U0, E: double): Double;
function UtcToGMeanSiderealTime(Ut: TDateTime): TDateTime;
function GLToDeg(GL: String; var Lon,Lat: Double): boolean;

const
  Pi2: double = 2 * pi;                               //  2 × PI
  RadDivDeg: double = 2 * pi / 360;                   //  1度あたりのラジアン値
  UniversalGravityConst: Double = 6.67428E-11;        //  万有引力の定数　 m3/s2/kg
  GravityConst: Double = 3.986005E5;                  //  地心重力定数 or 地心引力定数 m3/s2
  ZoneCoeff: double = 1.08263E-3;                     //  ゾーン係数
  InverseFlattering: double = 1 / 298.257224;         //  地球逆扁平率
  EquatorRadius: double  = 6377.397155;               //  地球(赤道)の半径 単位:km(WGS-84)
  EarthEccentricty: double  = 0.081819191042;         //  地球の離心率
  MeanYear: double = 365.25;                          //  (Days)
  TropicalYear: double = 365.2421874;                 //  回帰年、太陽年(Days)

implementation

constructor TSatellite.Create(AOwner: TComponent);
begin
  inherited;
//  定数の計算
  GravityConst23    := Power(GravityConst, 2/3);     //  地心重力定数 の 2/3乗
  EquatorRadius2    := Sqr(EquatorRadius);
  PolarRadius       := EquatorRadius * (1 - InverseFlattering);   // 地球の極半径
  PolarRadius2      := Sqr(PolarRadius);
  EarthEccentricty2 := Sqr(EarthEccentricty);         //  地球の離心率の2乗
  EarthLotation     := Pi2 / TropicalYear;            //  地球の回転数(Rad/Day)
  EarthLotationD    := Pi2 + TropicalYear;            //  回転数(Rad/Day)
  EarthLotationS    := EarthLotationD / 24 / 60 /60;  //  回転数(Rad/Sec)
  ALON := DegToRad(180); ALAT := DegToRad(0);         //  SAT Attitude

// レコードの初期化
  with FObsPoint do
    begin
    TimeOffset      := 9;
    LocalTimeStr    := 'JST';
    end;
  with FOrbit do
    begin
    Satellite       := '';
    Epoch           := StrToDatetime('2000/1/1');
    EpochMJD        := DateTimeToMJD(Epoch);
    end;

end;

//////////////////////////////////////////////////////////////////////////
//
//  NASA2Lineファイル名を設定する
//
//////////////////////////////////////////////////////////////////////////
procedure TSatellite.SetElementFile(const Value: string);
begin
  FElementFile := Value;
  if not FileExists(FElementFile) then
    begin
    MessageDlg(FElementFile + ' Not found', mtError, [mbOk], 0);
    Exit;
    end;
end;

//////////////////////////////////////////////////////////////////////////
//
//  観測地点のデータを設定・計算する
//
//////////////////////////////////////////////////////////////////////////
procedure TSatellite.SetObsPoint(Longitude, Latitude, Altitude, TimeOffset: Double; LocalTimeStr: string);
begin
  FObsPoint.Longitude     := Longitude;           //  単位 : Rad
  FObsPoint.Latitude      := Latitude;            //  単位 : Rad
  FObsPoint.Altitude      := Altitude;            //  単位：km
  FObsPoint.TimeOffset    := TimeOffset;          //  日単位の時差
  FObsPoint.LocalTimeStr  := LocalTimeStr;        //  ローカルタイムの文字
  CalcObsPoint
end;

procedure TSatellite.SetObsPoint1(ObsPoint: TObservationPoint);
begin
  FObsPoint := ObsPoint;
  CalcObsPoint;
end;

procedure TSatellite.CalcObsPoint();
var
  N: double;
  SinLat, CosLat, SinLon, CosLon: double;
begin;
  with FObsPoint do
    begin
//  観測点をG系直交座標系へ変換 (P185)
    N := EquatorRadius / Sqrt(1 - EarthEccentricty2 * Sqr(sin(Latitude)));    // 東西線曲率半径

    SinLat    := sin(Latitude);
    CosLat    := cos(Latitude);
    SinLon    := sin(Longitude);
    CosLon    := cos(Longitude);
    end;
//  Property GCartesianCoordに結果を設定する
  with FGCartesianCoord do
    begin
    X := (N + FObsPoint.Altitude) * CosLat * CosLon;;
    Y := (N + FObsPoint.Altitude) * CosLat * SinLon;
    Z := (N * (1 - EarthEccentricty2) + FObsPoint.Altitude) * SinLat;
    end;
end;

///////////////////////////////////////////////////////////////////////////
//
//  サテライト名からNASA 2ラインを読み込み
//  軌道要素データを設定する。
//
///////////////////////////////////////////////////////////////////////////
function TSatellite.SetOrbitaEle(Sat: string): boolean;
var
  F: TextFile;
  Line0: string;
  Line1: string;
  Line2: string;
  s: string;
  i: integer;
begin
  result := true;
//  ファイルの存在を確認
  if not FileExists(FElementFile) then
    begin
    result := false;
    MessageDlg('"' + FElementFile + '" not found', mtError, [mbOk], 0);
    Exit;
    end;

//  軌道要素の読み込み
  Line1 := ''; Line2 := '';
  AssignFile(F, FElementFile);    { File selected in dialog }
  Reset(F);
  while not Eof(F) do
    begin
    Readln(F, s);                 { Read first line of file }
    s := Trim(s);
    if s = Sat then
      begin
      Line0 := s;
      Readln(F, Line1);           { Read first line of file }
      Readln(F, Line2);           { Read first line of file }
      break;
      end;
    end;
  CloseFile(F);

  if (Line1 = '') or (line2 = '') then
    begin
    MessageDlg('"' + Sat + '" not found in ' + FElementFile, mtError, [mbOk], 0);
    result := false;
    Exit;
    end;

  try
//  データは弧度値(Radian)に変換
    with FOrbitalElments do
      begin
      Satellite     := trim(Copy(Line0, 1, 24));
      i := StrToInt(Copy(Line1, 19, 02));                               // 元期 年
      if i > 70 then
        Epoch_Y     := i + 1900
      else
        Epoch_Y     := i + 2000;
      Epoch_D       := StrToFloat(Copy(Line1, 21, 12));                 // 元期　通算日
      Epoch         := StrToDateTime(IntToStr(Epoch_Y) + '/1/1') + Epoch_D;
      DecayRate     := StrToFloat(Copy(Line1, 34, 10)) * Pi2;           // Rad/Day
      MeanMotion    := StrToFloat(Copy(Line2, 53, 11)) * Pi2;           // 平均運動回転 Rad/Day
      DecayRate     := StrToFloat(Copy(Line1, 34, 10)) * Pi2;           // 平均運動の変化
      Inclination   := DegToRad(StrToFloat(Copy(Line2, 09, 08)));       // 軌道傾斜角(Rad)
      RAAN          := DegToRad(StrToFloat(Copy(Line2, 18, 08)));       // 昇交点赤経(Rad)
      Eccentricity  := StrToFloat('0.' + Copy(Line2, 27, 07));          // 離心率　
      Argp          := DegToRad(StrToFloat(Copy(Line2, 35, 08)));       // 近地点引数(Rad)
      MeanAnomaly   := DegToRad(StrToFloat(Copy(Line2, 44, 08)));       // 平均近点離角(Rad)
      EpochRev      := StrToInt(Copy(Line2, 64, 05));                   // 周回番号
//      InitOrbitPlane;
      end;
  except
    result := false;
    exit;
  end;
end;

procedure TSatellite.SetOrbitalElments(const Value: TOrbitalElments);
begin
  FOrbitalElments := Value;
//  InitOrbitPlane;
end;

procedure TSatellite.InitOrbitPlane();
var
    SinInc, CosInc: double;
    PC: double;
begin
  with FOrbit do
    begin
    Satellite       := FOrbitalElments.Satellite;
    Epoch_Y         := FOrbitalElments.Epoch_Y;
    Epoch_D         := FOrbitalElments.Epoch_D;
    Epoch           := FOrbitalElments.Epoch;
    DecayRate       := FOrbitalElments.DecayRate;
    MeanMotion      := FOrbitalElments.MeanMotion;       // 平均移動
    Inclination     := FOrbitalElments.Inclination;      // 軌道傾斜角
    RAAN            := FOrbitalElments.RAAN;             // 昇交点赤経
    Eccentricity    := FOrbitalElments.Eccentricity;     // 離心率　
    Argp            := FOrbitalElments.ARGP;             // 近地点引数
    MeanAnomaly     := FOrbitalElments.MeanAnomaly;      // 平均近点離角
    EpochRev        := FOrbitalElments.EpochRev;         // 周回番号

    MeanMotionS     := MeanMotion / 86400;                            // 秒あたり平均運動
    MajorAxis       := Power(GravityConst / Sqr(MeanMotionS), 1/3);   // 衛星の長半径
    MinorAxis       := MajorAxis * Sqrt(1 - Sqr(Eccentricity));       // 衛星の短半径
    SinInc          := Sin(Inclination);
    CosInc          := Cos(Inclination);
    PC              := EquatorRadius * MajorAxis/Sqr(MinorAxis);      // Precession constant(歳差定数) rad/day
    PrecessionConst         := 1.5 * ZoneCoeff * Sqr(PC) * MeanMotion;
    NodePrecessionRate      := -PrecessionConst * CosInc;             // Node precession rate, rad/day
    PerigeePrecessionRate   := PrecessionConst * (5 * Sqr(CosInc) - 1) / 2;   // Perigee precession rate, rad/day
    AngleMomentumRate       := -2 * DecayRate / MeanMotion / 3;       // Drag coeff. (Angular momentum rate)/(Ang mom)  s^-1
    end;

////  以下、太陽に関する処理だろう! 必要性？
//// Sidereal and Solar data. Rarely needs changing. Valid to year ~2015
//    YG := 2000; G0 := 98.9821;                        // GHAA, Year YG, Jan 0.0
//    MAS0  := 356.0507; MASD := 0.98560028;            // MA Sun and rate, deg, deg/day
//    INS   := DegToRAD(23.4393); CNS := COS(INS); SNS := SIN(INS); // Sun's inclination
//    EQC1 :=0.03342; EQC2 := 0.00035;                  // Sun's Equation of centre terms
//
//// Bring Sun data to Satellite Epoch
//    TEG  := FOrbit.Epoch - EncodeDate(YG, 1, 1) - 1;  // Elapsed Time: Epoch - YG
//    GHAE := DegToRAD(G0) + TEG * EarthLotationS;      // GHA Aries, epoch
//    MRSE := DegToRAD(G0) + TEG * EarthLotationS + PI; // Mean RA Sun at Sat epoch
//    MASE := DEgToRAD(MAS0 + MASD * TEG);              // Mean MA Sun  ..

// Antenna unit vector in orbit plane coordinates.
//    CosALON := COS(ALON); SinALON := SIN(ALON);
//    CosALAT := COS(ALAT); SinALAT := SIN(ALAT);
//    Ax := -CosALAT * CosALON; Ay := -CosALAT * SinALON; Az := -SinALAT;
end;

procedure TSatellite.CalcOrbit(Ut: TDateTime);
var
  Sx,Sy: double;
  Gs: double;
  CosGs, SinGs: double;
  Xs, Ys, Zs: double;
  Xg, Yg, Zg: double;
  CosIncl, SinIncl: double;
  CosLon, SinLon: double;
  CosLat, SinLat: double;
  W: double;
//  DC,DT,KD,KDP,MA: double;
//  DR: integer;
begin
  InitOrbitPlane;
  with FOrbit do
    begin
    ElapsedTime  := (Ut - Epoch); //  エポックからの経過時間を計算

    MeanMotion := MeanMotion + DecayRate * ElapsedTime / 2;
    EpochRev :=  EpochRev   + Trunc(MeanMotion * (ElapsedTime + 1) / Pi2);    // なぜ1日を加えないといけないか？

    W := 0.174 / Power((MajorAxis / EquatorRadius), 3.5 ) * ElapsedTime;
    ARGP := ARGP + W * (2 -  2.5 * Sqr(Sin(Inclination)));
    RAAN := RAAN - W * Cos(Inclination);
    MeanAnomaly := MeanAnomaly + MeanMotion * ElapsedTime;
    MeanAnomaly := Frac(MeanAnomaly / Pi2) * Pi2;   // 0から2πに丸める

//
//    DC :=  -2 * DecayRate / MeanMotion / 3;
//    DT  := DC * ElapsedTime / 2;    // Linear drag terms
//    KD  := 1 + 4 * DT;
//    KDP := 1 - 7 * DT;
//    MA                := FOrbitalElments.MeanAnomaly
//                      +  FOrbitalElments.MeanMotion * (1 - 3 * DT);      // 平均近点角(Rad)
//    DR       := Trunc(MA / Pi2);                           // 0から2πに丸める
//    EpochRev :=  EpochRev   + DR;    // なぜ1日を加えないといけないか？


//    Eccentricity  := FOrbitalElments.Eccentricity;
//    MeanAnomaly   := Frac(FOrbitalElments.MeanAnomaly / Pi2) * Pi2;   // 0から2πに丸める


// 平均近点角、離心率から離心近点角を求める
    EccentricAanomaly := Kepler(MeanAnomaly, Eccentricity);   //  ケプラーの方程式

// 軌道面内のx,y座標
    Sx := MajorAxis * (cos(EccentricAanomaly) - Eccentricity);
    Sy := MajorAxis * SQRT(1 - Sqr(Eccentricity))* sin(EccentricAanomaly);
    OrbitalSurface.x := Sx;
    OrbitalSurface.y := Sy;

// 地心赤道直交座標
    CosRAAN := COS(RAAN); SinRAAN := SIN(RAAN);
    CosARGP := COS(ARGP); SinARGP := SIN(ARGP);
    CosIncl := COS(Inclination); SinIncl := SIN(Inclination);
    Xs := Sx * (CosRAAN * CosARGP - SinRAAN * CosIncl * SinARGP) - Sy * (CosRAAN * SinARGP + SinRAAN * CosIncl * CosARGP);
    Ys := Sx * (SinRAAN * CosArgp + CosRAAN * CosIncl * SinARGP) - Sy * (SinRAAN * SinARGP - CosRAAN * CosIncl * CosARGP);
    Zs := Sx * SinIncl * SinARGP + Sy * SinIncl * CosARGP;
    EquatorialCoord.X := Xs;
    EquatorialCoord.Y := Ys;
    EquatorialCoord.Z := Zs;

    Gs := Frac(UtcToGMeanSiderealTime(UT) / 12 * pi);  // グリニッジ平均恒星時 時分秒をRadに変換
    CosGs := COS(Gs); SinGs := SIN(Gs);
    Xg :=   Xs * CosGs + Ys * SinGs;    // G系地心直交座標系を求める
    Yg := - Xs * SinGs + Ys * CosGs;
    Zg :=   Zs;
    GGeocentricCoord.X := Xg;
    GGeocentricCoord.Y := Yg;
    GGeocentricCoord.Z := Zg;


                                                                         // 地心赤道直交座標系にする

//
//    Ud := Us - GCartesianCoord.X;             // 地平直交座標系に変換する
//    Vd := Vs - GCartesianCoord.Y;
//    Wd := Ws - GCartesianCoord.Z;
//    CosLon := Cos(ObsPoint.Longitude);
//    SinLon := Sin(ObsPoint.Longitude);
//    CosLat := Cos(obsPoint.Latitude);
//    SinLat := Sin(ObsPoint.Latitude);
//    Xd :=   SinLat * CosLon * Ud + SinLat * SinLon * Vd - CosLat * Wd;
//    Yd :=          - SinLon * Ud +          CosLon * Vd;
//    Zd :=   CosLat * CosLon * Ud + CosLat * SinLon * Vd + SinLat * Wd;
//    Rd := Sqrt(sqr(Xd) + sqr(Yd));
//
//    Az := ArcTan2(Yd, Xd);                            // Azimuth
//    if Xd < 0 then
//      AZ := AZ + pi;
//    EL := ArcSin(Zd / Rd);                            // Elevation
//
//    FOrbit.Cordinate.X := Xd;
//    FOrbit.Cordinate.Y := Yd;
//    FOrbit.Cordinate.Z := Zd;
//
//    SatCoord.Azimuth   := Az;
//    SatCoord.Elevation := El;
//    SatCoord.Range     := Rd;
//
//
//////  Solve M = EA - EC*SIN(EA) for EA given M, by Newton's Method
//////    EA := MeanAnomaly;
//////    repeat
//////      begin
//////      CosEA := Cos(EA); SinEA := Sin(EA);
//////      DNOM := 1 - Eccentricity * CosEA;
//////      D := (EA - Eccentricity * SinEA - MeanAnomaly) / DNOM;
//////      EA := EA - D;
//////      end;
//////    until (Abs(D) < 1E-5);
////
////    A := MajorAxis * KD;
////    B := MinorAxis * KD;
////    RS := A * DNOM; // ?
////
//////  Calc satellite position & velocity in plane of ellipse
////    Sx := A * (CosEA - Eccentricity); Vx := -A * SinEA / DNOM * MeanMotionS;
////    Sy := B * SinEA;                  Vy :=  B * CosEA / DNOM * MeanMotionS;
////
////    ARGP := FOrbitalElments.ARGP + PerigeePrecessionRate * ElapsedTime * KDP;
////    RAAN := FOrbitalElments.RAAN + NodePrecessionRate    * ElapsedTime * KDP;
////
////    CosRAAN := COS(RAAN); SinRAAN := SIN(RAAN);
////    CosARGP := COS(ARGP); SinARGP := SIN(ARGP);
//////    CosInc := COS(InClination); SinInc := SIN(InClination); // Initで処理している
////
////// 2260 REM Plane -> celestial coordinate transformation, [C] = [RAAN]*[IN]*[AP]
////// 天球座標変換
////    CXx := CosARGP * CosRAAN - SinARGP * CosInc * SinRAAN;
////      CXy := -CosARGP * CosRAAN - CosARGP * CosInc * SinRAAN;
////        CXz := SinInc * SinRAAN;
////    CYx := CosARGP * SinRAAN + SinARGP * CosInc * CosRAAN;
////      CYy := -SinARGP * SinRAAN + CosARGP * CosInc * CosRAAN;
////        CYz := -SinInc * CosRAAN;
////    CZx := SinARGP * SinInc;
////      CZy :=  CosARGP * SinInc;
////        CZz := CosInc;
////
//////  Compute SATellite's position vector, ANTenna axis unit vector
//////  and VELocity in CELESTIAL coordinates. (Note: Sz=0, Vz=0)
//////  SATelliteの位置ベクトル、ANTenna軸の単位ベクトルを計算します
////    SATx := Sx * CXx + Sy * CXy; ANTx := ax * CXx + ay * CXy + az * CXz; VELx := Vx * CXx + Vy * CXy;
////    SATy := Sx * CYx + Sy * CYy; ANTy := ax * CYx + ay * CYy + az * CYz; VELy := Vx*CYx+Vy*CYy;
////    SATz := Sx * CZx + Sy * CZy; ANTz := ax * CZx + ay * CZy + az * CZz; VELz := Vx*CZx+Vy*CZy;
////
//////  Also express SAT,ANT and VEL in GEOCENTRIC coordinates:
//////  SAT、ANT、VELを天動説座標で表現します
////    GHAA := GHAE + WE * T;           // GHA Aries at elapsed time T
////    CosGHAA := COS(-GHAA); SinGHAA := SIN(-GHAA);
////    Sx := SATx * CosGHAA - SATy * SinGHAA; Ax := ANTx * CosGHAA - ANTy * SinGHAA; Vx := VELx * CosGHAA - VELy * SinGHAA;
////    Sy := SATx * SinGHAA + SATy * CosGHAA; Ay := ANTx * SinGHAA + ANTy * CosGHAA; Vy := VELx * SinGHAA + VELy * CosGHAA;
////    Sz := SATz;            Az := ANTz;            Vz := VELz;
////
//////  Compute and manipulate range/velocity/antenna vectors
////    Rx := Sx - Ox; Ry := Sy - Oy; Rz := Sz - Oz;    // Rangevec = Satvec - Obsvec
////    R  := SQRT(Sqr(Rx) + Sqr(Ry) + Sqr(Rz));        // Range magnitude
////    Rx := Rx / R; Ry := Ry / R; Rz := Rz/R;         // Normalise Range vector 正規化
////    U  := Rx * Ux + Ry * Uy + Rz * Uz;              // UP    Component of unit range
////    E  := Rx * Ex + Ry * Ey;                        // EAST   do   (Ez=0)
////    N  := Rx * Nx + Ry * Ny + Rz * Nz;              // NORTH   do
////    Az := ArcTan2(E, N);                            // Azimuth
////    if AZ < 0 then
////      AZ := AZ + pi;
////    EL := ArcSin(U);                                // Elevation
    end;
end;

procedure TSatellite.CalcAosLos(DT: TDateTime);
var
  UT: Tdatetime;
  i: integer;
  isAOS: boolean;
  Az, El: double;
const
  Sec: double = 1 / 24 / 60 /60;    // 秒/日
  Minute: double = 1 / 24 / 60;    //  分/日
begin
  with FVisibleTimeSlot do
    begin
    AosAz := 0; MaxAz := 0; LosAz := 0;
    MaxEl := 0;
    AosDateTime := 0; MaxDateTime := 0; LosDateTime := 0;

    isAOS := false;
    Ut := RecodeSecond(DT, 0);  // 秒以下を切り捨て 2020-01-05 追加
    for i := 0 to 3000 do       // 300分(5時間)後までで最初のAOS等を計算する
      begin
      CalcOrbit(Ut);
      Az := Trunc(Az_d);
      El := Trunc(EL_d);
      if not isAOS then
        begin
        if (AosAz = 0) and (El >= 0) then    //  AOSの判断
          begin
          AosAz := Az;
          AosDateTime := UT;
    //        FEpochRev := OrbitEle_N.EpochRev; // atodeshusei;
          isAOS := true;
          end
        end
      else                 //　AOSに達した後
        begin
        if El > MaxEl then   //  Max Elの判断
          begin
          MaxEl := El;
          MaxAz := Az;
          MaxDateTime := UT;
          end;
        if (LosAz = 0) and (El < 0) then //  LOSの判断
          begin
          LosAz := Az;
          LosDateTime := UT;
          Exit;
          end;
        end;
      Ut := Ut + Minute;
      end;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//  Utcからグリニッジ平均恒星時に変換する。 (P27,115)
//
////////////////////////////////////////////////////////////////////////////////
function UtcToGMeanSiderealTime(Ut: TDateTime): TDateTime;
var
  Days, TU, Tg0: Double;
begin
//  1899/12/31 12:00 殻の経過日数(K)を求める
//  この式を適用できるのは、1900/3/1～2100/2/28の期間のみ
  days := Ut - StrToDateTime('1899/12/31 12:00:00');
  Tu := Days / 36525;

//  平均太陽 mean sunを求める
//  w1 := StrToTime('06:38:45.836');
//  w2 := 8640184.542/ 3600 / 24;
//  w3 := 0.0929 / 3600 / 24;
//  Tg0 := w1 + w2 + w3;   // 平均太陽 mean sun
//  Tg0 := StrToTime('06:38:45.836') + 8640184.542/ 3600 / 24 * TU + 0.0929 / 3600 / 24 * TU * TU;   // 上4行を1行にまとめる
  Tg0 := 0.276919398 + 100.002135902 * TU + 1.075E-6 * TU * TU;   // 上行の置き換え
  result := Frac(Tg0); // resultは、時分秒単位
end;

////////////////////////////////////////////////////////////////////////////////
//  ユリウス恒星日を求めるには、DateUtilsのDateTimeToJulianDate関数で求める。
//  逆方向変換は、JulianDateToDateTime関数で求める。
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//
//  Delphi の TDateTime値を修正ユリウス日（Modeified Julian Day;MJD)）に変換する。
//
//  Delphi の TDateTime 値の整数部は西暦 1899 年 12 月 30 日からの経過日数を示します。
//  小数部はその日の経過時間（24 時間制）です。
//
//  修正ユリアス日は、1858年11月17日0時を起算開始日とした通算日
//  修正ユリアス日　＝　ユリアス日 －　2400000.5　の関係がある
//
//  通常 1899年12月30日 以前を計算対象としなければ、特にMJDに変換する必要ない
//  1899年12月30日以前は、TDateTimeを調べなければ分からない (多分 大丈夫)
//
////////////////////////////////////////////////////////////////////////////////
function DateTimeToMJD(DT: TDateTime): double;
begin
  Result := DateTimeToModifiedJulianDate(DT);
end;

function MJDToDateTime(MJD: Double): TDateTime;
begin
  result := ModifiedJulianDateToDateTime(MJD);
end;

///////////////////////////////////////////////////////////////////////////
//
//  ケプラーの方程式で離心近点角(U)を求める
//
///////////////////////////////////////////////////////////////////////////
function Kepler(U0, E: double): Double;
var
  U, UD: double;
begin
  U := U0;
  Repeat
    UD := (U0 - U + E * sin(U)) / (1- E * cos(U));
    U := U + UD
  Until IsZero(UD);
  result := U;
end;

function GLToDeg(GL: String; var Lon,Lat: Double): boolean;
var
  s: string;
  w1,w2,w3: byte;
begin
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

end.

unit uSatellite;

//////////////////////////////////////////////////////////////////////////
//
//    �Q�l����
//      �P�D�V�̂̈ʒu�v�Z �����   ���ҁF����H���@�@���s�F�n�l����
//      2.  Calsat32�̃z�[���؁[�W  ��ҁF���c�������iJR1HUO�j
//             http://homepage1.nifty.com/aida/jr1huo_calsat32/index.html
//
//////////////////////////////////////////////////////////////////////////

{ �p��
    Orbital Elments:    �O���v�f
    Right Aceention of Ascending NODE(RAAN);   ����_�Ԍo(�x)�@
    Inclination:        �O���X�Ίp�iRad)
    Argument of Perigee(ARGP):   �ߒn�_����


    Epoch:              ����
    Eccentricity:       ���S��
    Mean Anomaly:       ���ϋߓ_�p(Rad)
    Mean Motion:        ���ω^���irad/���j
    Decay Rate:         ���㗦 ���ω^���̕ω� �ۓ�1
    Semi Major Axis:    �����a
    EpochRev:           ����ԍ�

}

interface

uses Classes, SysUtils, Dialogs, Math, DateUtils, System.UITypes;

type
//**********************************************************************//
//                                                                      //
//  �ϑ��n�_�i�^�p�n�_�j�N���X                                          //
//                                                                      //
//**********************************************************************//
  TObservationPoint = record
    Longitude: double;         //  �o�x
    Latitude: double;          //  �ܓx
    Altitude: double;          //  �W��
    TimeOffset: double;        //  ����
    LocalTimeStr: string;      //  JST���̕�����
  end;

//**********************************************************************//
//                                                                      //
//  ���ʍ��W�N���X                                                �@�@�@//
//                                                                      //
//**********************************************************************//
  TSphericalCoord = record
    Azimuth: double;            // ���ʊp
    Elevation: double;          // �p
    Range: double;              // ����
    end;

//**********************************************************************//
//                                                                      //
//  �������W�N���X                                                �@�@�@//
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
//  �q���̋O���v�f�N���X                                                //
//                                                                      //
//**********************************************************************//
  TOrbitalElments = record
    Satellite: String;         // �q����
    Epoch_Y: Integer;          // ����(�N)
    Epoch_D: double;           // ����(�A�����A����)
    DecayRate: double;         // n1: ���㗦                 Decay Rate, Drug ���ω^���̕ω� �ۓ�1
    Inclination: double;       // i:  �O���X�Ίp�iRad)       Inclination
    RAAN:  double;             // ��: ����_�Ԍo(�x)�@       Right Aceention of Ascending NODE
    Eccentricity: double;      // e:  ���S��                 Eccentricity
    ARGP:  double;             // ��: �ߒn�_����             Argument of Perigee
    MeanAnomaly: double;       // M or l:  ���ϋߓ_�p(Rad)   Mean Anomaly
    MeanMotion: double;        // n:  ���ω^���irad/���j     Mean Motion
    EpochRev:Integer;          // ����ԍ�
    Epoch: TDateTime;          // ����(�N���������E�E�E)
  end;

//**********************************************************************//
//                                                                      //
//  �q���̋O���N���X                                                �@�@//
//                                                                      //
//**********************************************************************//
  TOrbit = record
    Satellite: String;          // �q����
    Epoch_Y: Integer;           // ����(�N)
    Epoch_D: double;            // ����(�A�����A����)
    DecayRate: double;          // n1: ���㗦                 Decay Rate, Drug ���ω^���̕ω� �ۓ�1
    Inclination: double;        // i:  �O���X�Ίp�iRad)       Inclination
    RAAN:  double;              // ��: ����_�Ԍo(�x)�@       Right Aceention of Ascending NODE
    Eccentricity: double;       // e:  ���S��                 Eccentricity
    ARGP:  double;              // ��: �ߒn�_����             Argument of Perigee
    MeanAnomaly: double;        // M or l:  ���ϋߓ_�p(Rad)   Mean Anomaly
    MeanMotion: double;         // n: ���ω^���irad/���j     Mean Motion
    EpochRev:Integer;           // ����ԍ�
    Epoch: TDateTime;           // ����
//  �����܂ł́A TOrbitalElmentsto�������ڏ�
    EpochMJD: Double;
    MeanMotionS: double;        // n: ���ω^���irad/Sec�j     Mean Motion
    ElapsedTime: TDateTime;     // �o�ߎ���
    EccentricAanomaly: double;  // ���S�ߓ_�p
    MajorAxis: double;          // �O�������a
    MinorAxis: double;          // �O���Z���a
    PrecessionConst: double;    // �΍��萔

    OrbitalSurface: T2DCoordinates;   // �O���ʓ���x,y���W
    EquatorialCoord: T3DCoordinates;  // �n�S�ԓ��������W
    GGeocentricCoord: T3DCoordinates; // G�a�n�S�������W
    SatCoord: TSphericalCoord;        // �q���̕��ʊp,�p�A����
    AngleMomentumRate: Double;
    NodePrecessionRate: double;
    PerigeePrecessionRate: double;
  end;

//**********************************************************************//
//                                                                      //
//  �q���̉����ԑуN���X                                              //
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
//  �O���v�Z�̃N���X                                                    //
//                                                                      //
//**********************************************************************//
type
  TSatellite = class(TComponent)
  private
    { Private �錾 }
// �萔���ŕ\���Ȃ��萔�Bconstructor�Őݒ肷��
    GravityConst23: double;     //  �n�S�d�͒萔 �� 2/3��
    EquatorRadius2: double;
    EarthEccentricty2: double;  //  �n���̗��S����2��
    PolarRadius:    double;
    PolarRadius2: double;
    EarthLotation: double;      //  �n���̉�]��(Rad/Day)
    EarthLotationD: double;     //      ��]��(Rad/Day)
    EarthLotationS: double;     //  ��]��(Rad/Sec)

    ALON, ALAT: double;         // Spin Axis Attitude
//    Ax, Ay, Az: double;
//    Ox,Oy,Oz: double;
//    Ux,Uy,Uz: double;
//    Ex,Nx,Ey,Ny,Ez,Nz: double;
//
//  �O���v�Z(AOS,Los,Max)�v�Z�̈�
//    OrbitEle_N: TOrbitalElments;           //  �q���̌��݈ʒu

    Az_d, El_d: double;

//  �ϑ��n�_�̒��Ԍv�Z�l
//    SinISinK, SinICosK, CosISinK, CosICosK: double;

//  �q���̒��Ԍv�Z�l
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
    { Public �錾 }
    property ElementFile: string read FElementFile write SetElementFile;
    property ObsPoint: TObservationPoint read FObsPoint;
    property OrbitalElments: TOrbitalElments read FOrbitalElments write SetOrbitalElments;
    property Orbit: TOrbit read FOrbit;
    property GCartesianCoord: T3DCoordinates read FGCartesianCoord;  // �ϑ��_G�n�n�S�������W

//    property DebugKeplerEle: TOrbitalElments read FDebugKeplerEle write SetDebugKeplerEle;
    property VisibleTimeSlot: TVisibleTimeSlot read FVisibleTimeSlot;
    property GCenteredCartesianCoord: T3DCoordinates read FCenteredCartesianCoord;  // G�n���S�������W�n
    property GreenwichSiderealTime: TDateTime read FGreenwichSiderealTime;   // �O���j�b�`���ύP����
    property EccentricAnomaly: double read FEccentricAnomaly;   //���S�ߓ_�p
    property TrueAnormaly: double read FTrueAnormaly;   // �^�ߓ_�p
    property RadiusVector: double read FRadiusVector;   // ���a
    property CartesianCoordinatesX: Double read FCartesianCoordinatesX;   // �q���O���ʂ̈ʒu�������W (�ߒn�_����)
    property CartesianCoordinatesY: Double read FCartesianCoordinatesY;   // �q���O���ʂ̈ʒu�������W (X�̔����v���90�x����)
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
  Pi2: double = 2 * pi;                               //  2 �~ PI
  RadDivDeg: double = 2 * pi / 360;                   //  1�x������̃��W�A���l
  UniversalGravityConst: Double = 6.67428E-11;        //  ���L���͂̒萔�@ m3/s2/kg
  GravityConst: Double = 3.986005E5;                  //  �n�S�d�͒萔 or �n�S���͒萔 m3/s2
  ZoneCoeff: double = 1.08263E-3;                     //  �]�[���W��
  InverseFlattering: double = 1 / 298.257224;         //  �n���t�G����
  EquatorRadius: double  = 6377.397155;               //  �n��(�ԓ�)�̔��a �P��:km(WGS-84)
  EarthEccentricty: double  = 0.081819191042;         //  �n���̗��S��
  MeanYear: double = 365.25;                          //  (Days)
  TropicalYear: double = 365.2421874;                 //  ��A�N�A���z�N(Days)

implementation

constructor TSatellite.Create(AOwner: TComponent);
begin
  inherited;
//  �萔�̌v�Z
  GravityConst23    := Power(GravityConst, 2/3);     //  �n�S�d�͒萔 �� 2/3��
  EquatorRadius2    := Sqr(EquatorRadius);
  PolarRadius       := EquatorRadius * (1 - InverseFlattering);   // �n���̋ɔ��a
  PolarRadius2      := Sqr(PolarRadius);
  EarthEccentricty2 := Sqr(EarthEccentricty);         //  �n���̗��S����2��
  EarthLotation     := Pi2 / TropicalYear;            //  �n���̉�]��(Rad/Day)
  EarthLotationD    := Pi2 + TropicalYear;            //  ��]��(Rad/Day)
  EarthLotationS    := EarthLotationD / 24 / 60 /60;  //  ��]��(Rad/Sec)
  ALON := DegToRad(180); ALAT := DegToRad(0);         //  SAT Attitude

// ���R�[�h�̏�����
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
//  NASA2Line�t�@�C������ݒ肷��
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
//  �ϑ��n�_�̃f�[�^��ݒ�E�v�Z����
//
//////////////////////////////////////////////////////////////////////////
procedure TSatellite.SetObsPoint(Longitude, Latitude, Altitude, TimeOffset: Double; LocalTimeStr: string);
begin
  FObsPoint.Longitude     := Longitude;           //  �P�� : Rad
  FObsPoint.Latitude      := Latitude;            //  �P�� : Rad
  FObsPoint.Altitude      := Altitude;            //  �P�ʁFkm
  FObsPoint.TimeOffset    := TimeOffset;          //  ���P�ʂ̎���
  FObsPoint.LocalTimeStr  := LocalTimeStr;        //  ���[�J���^�C���̕���
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
//  �ϑ��_��G�n�������W�n�֕ϊ� (P185)
    N := EquatorRadius / Sqrt(1 - EarthEccentricty2 * Sqr(sin(Latitude)));    // �������ȗ����a

    SinLat    := sin(Latitude);
    CosLat    := cos(Latitude);
    SinLon    := sin(Longitude);
    CosLon    := cos(Longitude);
    end;
//  Property GCartesianCoord�Ɍ��ʂ�ݒ肷��
  with FGCartesianCoord do
    begin
    X := (N + FObsPoint.Altitude) * CosLat * CosLon;;
    Y := (N + FObsPoint.Altitude) * CosLat * SinLon;
    Z := (N * (1 - EarthEccentricty2) + FObsPoint.Altitude) * SinLat;
    end;
end;

///////////////////////////////////////////////////////////////////////////
//
//  �T�e���C�g������NASA 2���C����ǂݍ���
//  �O���v�f�f�[�^��ݒ肷��B
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
//  �t�@�C���̑��݂��m�F
  if not FileExists(FElementFile) then
    begin
    result := false;
    MessageDlg('"' + FElementFile + '" not found', mtError, [mbOk], 0);
    Exit;
    end;

//  �O���v�f�̓ǂݍ���
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
//  �f�[�^�͌ʓx�l(Radian)�ɕϊ�
    with FOrbitalElments do
      begin
      Satellite     := trim(Copy(Line0, 1, 24));
      i := StrToInt(Copy(Line1, 19, 02));                               // ���� �N
      if i > 70 then
        Epoch_Y     := i + 1900
      else
        Epoch_Y     := i + 2000;
      Epoch_D       := StrToFloat(Copy(Line1, 21, 12));                 // �����@�ʎZ��
      Epoch         := StrToDateTime(IntToStr(Epoch_Y) + '/1/1') + Epoch_D;
      DecayRate     := StrToFloat(Copy(Line1, 34, 10)) * Pi2;           // Rad/Day
      MeanMotion    := StrToFloat(Copy(Line2, 53, 11)) * Pi2;           // ���ω^����] Rad/Day
      DecayRate     := StrToFloat(Copy(Line1, 34, 10)) * Pi2;           // ���ω^���̕ω�
      Inclination   := DegToRad(StrToFloat(Copy(Line2, 09, 08)));       // �O���X�Ίp(Rad)
      RAAN          := DegToRad(StrToFloat(Copy(Line2, 18, 08)));       // ����_�Ԍo(Rad)
      Eccentricity  := StrToFloat('0.' + Copy(Line2, 27, 07));          // ���S���@
      Argp          := DegToRad(StrToFloat(Copy(Line2, 35, 08)));       // �ߒn�_����(Rad)
      MeanAnomaly   := DegToRad(StrToFloat(Copy(Line2, 44, 08)));       // ���ϋߓ_���p(Rad)
      EpochRev      := StrToInt(Copy(Line2, 64, 05));                   // ����ԍ�
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
    MeanMotion      := FOrbitalElments.MeanMotion;       // ���ψړ�
    Inclination     := FOrbitalElments.Inclination;      // �O���X�Ίp
    RAAN            := FOrbitalElments.RAAN;             // ����_�Ԍo
    Eccentricity    := FOrbitalElments.Eccentricity;     // ���S���@
    Argp            := FOrbitalElments.ARGP;             // �ߒn�_����
    MeanAnomaly     := FOrbitalElments.MeanAnomaly;      // ���ϋߓ_���p
    EpochRev        := FOrbitalElments.EpochRev;         // ����ԍ�

    MeanMotionS     := MeanMotion / 86400;                            // �b�����蕽�ω^��
    MajorAxis       := Power(GravityConst / Sqr(MeanMotionS), 1/3);   // �q���̒����a
    MinorAxis       := MajorAxis * Sqrt(1 - Sqr(Eccentricity));       // �q���̒Z���a
    SinInc          := Sin(Inclination);
    CosInc          := Cos(Inclination);
    PC              := EquatorRadius * MajorAxis/Sqr(MinorAxis);      // Precession constant(�΍��萔) rad/day
    PrecessionConst         := 1.5 * ZoneCoeff * Sqr(PC) * MeanMotion;
    NodePrecessionRate      := -PrecessionConst * CosInc;             // Node precession rate, rad/day
    PerigeePrecessionRate   := PrecessionConst * (5 * Sqr(CosInc) - 1) / 2;   // Perigee precession rate, rad/day
    AngleMomentumRate       := -2 * DecayRate / MeanMotion / 3;       // Drag coeff. (Angular momentum rate)/(Ang mom)  s^-1
    end;

////  �ȉ��A���z�Ɋւ��鏈�����낤! �K�v���H
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
    ElapsedTime  := (Ut - Epoch); //  �G�|�b�N����̌o�ߎ��Ԃ��v�Z

    MeanMotion := MeanMotion + DecayRate * ElapsedTime / 2;
    EpochRev :=  EpochRev   + Trunc(MeanMotion * (ElapsedTime + 1) / Pi2);    // �Ȃ�1���������Ȃ��Ƃ����Ȃ����H

    W := 0.174 / Power((MajorAxis / EquatorRadius), 3.5 ) * ElapsedTime;
    ARGP := ARGP + W * (2 -  2.5 * Sqr(Sin(Inclination)));
    RAAN := RAAN - W * Cos(Inclination);
    MeanAnomaly := MeanAnomaly + MeanMotion * ElapsedTime;
    MeanAnomaly := Frac(MeanAnomaly / Pi2) * Pi2;   // 0����2�΂Ɋۂ߂�

//
//    DC :=  -2 * DecayRate / MeanMotion / 3;
//    DT  := DC * ElapsedTime / 2;    // Linear drag terms
//    KD  := 1 + 4 * DT;
//    KDP := 1 - 7 * DT;
//    MA                := FOrbitalElments.MeanAnomaly
//                      +  FOrbitalElments.MeanMotion * (1 - 3 * DT);      // ���ϋߓ_�p(Rad)
//    DR       := Trunc(MA / Pi2);                           // 0����2�΂Ɋۂ߂�
//    EpochRev :=  EpochRev   + DR;    // �Ȃ�1���������Ȃ��Ƃ����Ȃ����H


//    Eccentricity  := FOrbitalElments.Eccentricity;
//    MeanAnomaly   := Frac(FOrbitalElments.MeanAnomaly / Pi2) * Pi2;   // 0����2�΂Ɋۂ߂�


// ���ϋߓ_�p�A���S�����痣�S�ߓ_�p�����߂�
    EccentricAanomaly := Kepler(MeanAnomaly, Eccentricity);   //  �P�v���[�̕�����

// �O���ʓ���x,y���W
    Sx := MajorAxis * (cos(EccentricAanomaly) - Eccentricity);
    Sy := MajorAxis * SQRT(1 - Sqr(Eccentricity))* sin(EccentricAanomaly);
    OrbitalSurface.x := Sx;
    OrbitalSurface.y := Sy;

// �n�S�ԓ��������W
    CosRAAN := COS(RAAN); SinRAAN := SIN(RAAN);
    CosARGP := COS(ARGP); SinARGP := SIN(ARGP);
    CosIncl := COS(Inclination); SinIncl := SIN(Inclination);
    Xs := Sx * (CosRAAN * CosARGP - SinRAAN * CosIncl * SinARGP) - Sy * (CosRAAN * SinARGP + SinRAAN * CosIncl * CosARGP);
    Ys := Sx * (SinRAAN * CosArgp + CosRAAN * CosIncl * SinARGP) - Sy * (SinRAAN * SinARGP - CosRAAN * CosIncl * CosARGP);
    Zs := Sx * SinIncl * SinARGP + Sy * SinIncl * CosARGP;
    EquatorialCoord.X := Xs;
    EquatorialCoord.Y := Ys;
    EquatorialCoord.Z := Zs;

    Gs := Frac(UtcToGMeanSiderealTime(UT) / 12 * pi);  // �O���j�b�W���ύP���� �����b��Rad�ɕϊ�
    CosGs := COS(Gs); SinGs := SIN(Gs);
    Xg :=   Xs * CosGs + Ys * SinGs;    // G�n�n�S�������W�n�����߂�
    Yg := - Xs * SinGs + Ys * CosGs;
    Zg :=   Zs;
    GGeocentricCoord.X := Xg;
    GGeocentricCoord.Y := Yg;
    GGeocentricCoord.Z := Zg;


                                                                         // �n�S�ԓ��������W�n�ɂ���

//
//    Ud := Us - GCartesianCoord.X;             // �n���������W�n�ɕϊ�����
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
//////    CosInc := COS(InClination); SinInc := SIN(InClination); // Init�ŏ������Ă���
////
////// 2260 REM Plane -> celestial coordinate transformation, [C] = [RAAN]*[IN]*[AP]
////// �V�����W�ϊ�
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
//////  SATellite�̈ʒu�x�N�g���AANTenna���̒P�ʃx�N�g�����v�Z���܂�
////    SATx := Sx * CXx + Sy * CXy; ANTx := ax * CXx + ay * CXy + az * CXz; VELx := Vx * CXx + Vy * CXy;
////    SATy := Sx * CYx + Sy * CYy; ANTy := ax * CYx + ay * CYy + az * CYz; VELy := Vx*CYx+Vy*CYy;
////    SATz := Sx * CZx + Sy * CZy; ANTz := ax * CZx + ay * CZy + az * CZz; VELz := Vx*CZx+Vy*CZy;
////
//////  Also express SAT,ANT and VEL in GEOCENTRIC coordinates:
//////  SAT�AANT�AVEL��V�������W�ŕ\�����܂�
////    GHAA := GHAE + WE * T;           // GHA Aries at elapsed time T
////    CosGHAA := COS(-GHAA); SinGHAA := SIN(-GHAA);
////    Sx := SATx * CosGHAA - SATy * SinGHAA; Ax := ANTx * CosGHAA - ANTy * SinGHAA; Vx := VELx * CosGHAA - VELy * SinGHAA;
////    Sy := SATx * SinGHAA + SATy * CosGHAA; Ay := ANTx * SinGHAA + ANTy * CosGHAA; Vy := VELx * SinGHAA + VELy * CosGHAA;
////    Sz := SATz;            Az := ANTz;            Vz := VELz;
////
//////  Compute and manipulate range/velocity/antenna vectors
////    Rx := Sx - Ox; Ry := Sy - Oy; Rz := Sz - Oz;    // Rangevec = Satvec - Obsvec
////    R  := SQRT(Sqr(Rx) + Sqr(Ry) + Sqr(Rz));        // Range magnitude
////    Rx := Rx / R; Ry := Ry / R; Rz := Rz/R;         // Normalise Range vector ���K��
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
  Sec: double = 1 / 24 / 60 /60;    // �b/��
  Minute: double = 1 / 24 / 60;    //  ��/��
begin
  with FVisibleTimeSlot do
    begin
    AosAz := 0; MaxAz := 0; LosAz := 0;
    MaxEl := 0;
    AosDateTime := 0; MaxDateTime := 0; LosDateTime := 0;

    isAOS := false;
    Ut := RecodeSecond(DT, 0);  // �b�ȉ���؂�̂� 2020-01-05 �ǉ�
    for i := 0 to 3000 do       // 300��(5����)��܂łōŏ���AOS�����v�Z����
      begin
      CalcOrbit(Ut);
      Az := Trunc(Az_d);
      El := Trunc(EL_d);
      if not isAOS then
        begin
        if (AosAz = 0) and (El >= 0) then    //  AOS�̔��f
          begin
          AosAz := Az;
          AosDateTime := UT;
    //        FEpochRev := OrbitEle_N.EpochRev; // atodeshusei;
          isAOS := true;
          end
        end
      else                 //�@AOS�ɒB������
        begin
        if El > MaxEl then   //  Max El�̔��f
          begin
          MaxEl := El;
          MaxAz := Az;
          MaxDateTime := UT;
          end;
        if (LosAz = 0) and (El < 0) then //  LOS�̔��f
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
//  Utc����O���j�b�W���ύP�����ɕϊ�����B (P27,115)
//
////////////////////////////////////////////////////////////////////////////////
function UtcToGMeanSiderealTime(Ut: TDateTime): TDateTime;
var
  Days, TU, Tg0: Double;
begin
//  1899/12/31 12:00 �k�̌o�ߓ���(K)�����߂�
//  ���̎���K�p�ł���̂́A1900/3/1�`2100/2/28�̊��Ԃ̂�
  days := Ut - StrToDateTime('1899/12/31 12:00:00');
  Tu := Days / 36525;

//  ���ϑ��z mean sun�����߂�
//  w1 := StrToTime('06:38:45.836');
//  w2 := 8640184.542/ 3600 / 24;
//  w3 := 0.0929 / 3600 / 24;
//  Tg0 := w1 + w2 + w3;   // ���ϑ��z mean sun
//  Tg0 := StrToTime('06:38:45.836') + 8640184.542/ 3600 / 24 * TU + 0.0929 / 3600 / 24 * TU * TU;   // ��4�s��1�s�ɂ܂Ƃ߂�
  Tg0 := 0.276919398 + 100.002135902 * TU + 1.075E-6 * TU * TU;   // ��s�̒u������
  result := Frac(Tg0); // result�́A�����b�P��
end;

////////////////////////////////////////////////////////////////////////////////
//  �����E�X�P���������߂�ɂ́ADateUtils��DateTimeToJulianDate�֐��ŋ��߂�B
//  �t�����ϊ��́AJulianDateToDateTime�֐��ŋ��߂�B
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//
//  Delphi �� TDateTime�l���C�������E�X���iModeified Julian Day;MJD)�j�ɕϊ�����B
//
//  Delphi �� TDateTime �l�̐������͐��� 1899 �N 12 �� 30 ������̌o�ߓ����������܂��B
//  �������͂��̓��̌o�ߎ��ԁi24 ���Ԑ��j�ł��B
//
//  �C�������A�X���́A1858�N11��17��0�����N�Z�J�n���Ƃ����ʎZ��
//  �C�������A�X���@���@�����A�X�� �|�@2400000.5�@�̊֌W������
//
//  �ʏ� 1899�N12��30�� �ȑO���v�Z�ΏۂƂ��Ȃ���΁A����MJD�ɕϊ�����K�v�Ȃ�
//  1899�N12��30���ȑO�́ATDateTime�𒲂ׂȂ���Ε�����Ȃ� (���� ���v)
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
//  �P�v���[�̕������ŗ��S�ߓ_�p(U)�����߂�
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

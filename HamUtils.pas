unit HamUtils;


interface

uses
  Windows, Messages, SysUtils, Math, DateUtils;

function DateTimeToMJD(DT: TDateTime): double;
function Kepler(U0, E: double): Double;
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
//procedure Wait(t: integer);

const
  Pi2: double = 2 * pi;                //  2 �~ PI

implementation

{$R *.dfm}

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Delphi �� TDateTime �l�����i�C���j�����E�X���iModeified Julian Day;MJD)�j�ɕϊ�����B
//
//  Delphi �� TDateTime �l�̐������͐��� 1899 �N 12 �� 30 ������̌o�ߓ����������܂��B
//  �������͂��̓��̌o�ߎ��ԁi24 ���Ԑ��j�ł��B
//
//  �������A�X���́A1858�N11��17��0�����N�Z�J�n���Ƃ����ʎZ��
//  �������A�X���@���@�����A�X�� �|�@2400000.5�@�̊֌W������
//
//  �ʏ� 1899�N12��30�� �ȑO���v�Z�ΏۂƂ��Ȃ���΁A����MJD�ɕϊ�����K�v�Ȃ�
//  1899�N12��30���ȑO�́ATDateTime�𒲂ׂȂ���Ε�����Ȃ� (���� ���v)
//
/////////////////////////////////////////////////////////////////////////////////////////////

function DateTimeToMJD(DT: TDateTime): double;
var
  Y, M, D: word;
  MJD: double;
begin
  DecodeDate(DT, Y, M, D);
  if M <= 2 then
    begin
    M := M + 12;
    Y := Y - 1;
    end;
  MJD := int(365.25 * Y) + int(y / 400) - int(y / 100) + int(30.59 * (m - 2)) + d - 678912
       + Frac(DT);
  result := MJD;
end;

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  �P�v���[�̕������ŗ��S�ߓ_�p(U)�����߂�
//
/////////////////////////////////////////////////////////////////////////////////////////////
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

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Ut����O���j�b�W���ύP�����ɕϊ�����B (P27,151)
//
/////////////////////////////////////////////////////////////////////////////////////////////
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
var
  Y,M,D: Integer;
  Lt, K, TU, Tg0: Double;
begin
//  1899/12/31 12:00 �k�̌o�ߓ���(K)�����߂�
//  ���̎���K�p�ł���̂́A1900/3/1�`2100/2/28�̊��Ԃ̂�
  Y := YearOf(Ut) - 1900;
  M := MonthOf(Ut);
  D := DayOf(Ut);
  if M < 3 then
    begin
    Y := Y - 1;
    M := M + 2;
    end;
  K := 365 * Y + 30 * M + D - 33.5 + Int(0.6 * (M + 1)) + Int(Y / 4);
//  1899/12/31 12:00 �k�̌o�ߓ���(K)��,36525����1�Ƃ����P�ʁiTU�j�����߂�
  TU := K / 36525;
//  UT 00:00�̃O���j�b�W�P���������߂�i24���Ԉȏ�́A�Ō�ɐ؂�̂ĂĂ���j
  Tg0 := (23925.836 + 8640184.542 * TU + 0.0929 * TU * TU) / 86400;
//  ���߂鎞���̃O���j�b�W�P���������߂�
//  ���ʂ́A���t�`���ł���̂ŁA�����ȉ��̌��̂ݗL��
  Lon := Lon / Pi2;
  Lt := Frac(Ut);
  result := frac(Tg0 + Lon + Lt + 0.00273791 * Lt);
end;



{
procedure Wait(t: integer);
var
  endtime : TDateTime; //�I������
begin
  endtime := Now + t / 86400 / 1000;
//�@�@while (Now < endtime) do ;
  while (Now < endtime) do
    Application.ProcessMessages;
end;
}

end.

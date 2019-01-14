unit HamUtils;


interface

uses
  Windows, Messages, SysUtils, Math, DateUtils;

function DateTimeToMJD(DT: TDateTime): double;
function Kepler(U0, E: double): Double;
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
//procedure Wait(t: integer);

const
  Pi2: double = 2 * pi;                //  2 × PI

implementation

{$R *.dfm}

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Delphi の TDateTime 値を準（修正）ユリウス日（Modeified Julian Day;MJD)）に変換する。
//
//  Delphi の TDateTime 値の整数部は西暦 1899 年 12 月 30 日からの経過日数を示します。
//  小数部はその日の経過時間（24 時間制）です。
//
//  準ユリアス日は、1858年11月17日0時を起算開始日とした通算日
//  準ユリアス日　＝　ユリアス日 －　2400000.5　の関係がある
//
//  通常 1899年12月30日 以前を計算対象としなければ、特にMJDに変換する必要ない
//  1899年12月30日以前は、TDateTimeを調べなければ分からない (多分 大丈夫)
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
//  ケプラーの方程式で離心近点角(U)を求める
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
//  Utからグリニッジ平均恒星時に変換する。 (P27,151)
//
/////////////////////////////////////////////////////////////////////////////////////////////
function UtToSiderealTime(Ut: TDateTime; Lon: double): TDateTime;
var
  Y,M,D: Integer;
  Lt, K, TU, Tg0: Double;
begin
//  1899/12/31 12:00 殻の経過日数(K)を求める
//  この式を適用できるのは、1900/3/1～2100/2/28の期間のみ
  Y := YearOf(Ut) - 1900;
  M := MonthOf(Ut);
  D := DayOf(Ut);
  if M < 3 then
    begin
    Y := Y - 1;
    M := M + 2;
    end;
  K := 365 * Y + 30 * M + D - 33.5 + Int(0.6 * (M + 1)) + Int(Y / 4);
//  1899/12/31 12:00 殻の経過日数(K)を,36525日を1とした単位（TU）を求める
  TU := K / 36525;
//  UT 00:00のグリニッジ恒星時を求める（24時間以上は、最後に切り捨てている）
  Tg0 := (23925.836 + 8640184.542 * TU + 0.0929 * TU * TU) / 86400;
//  求める時刻のグリニッジ恒星時を求める
//  結果は、日付形式であるので、少数以下の桁のみ有効
  Lon := Lon / Pi2;
  Lt := Frac(Ut);
  result := frac(Tg0 + Lon + Lt + 0.00273791 * Lt);
end;



{
procedure Wait(t: integer);
var
  endtime : TDateTime; //終了時間
begin
  endtime := Now + t / 86400 / 1000;
//　　while (Now < endtime) do ;
  while (Now < endtime) do
    Application.ProcessMessages;
end;
}

end.

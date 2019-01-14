(* ***************************************************************************
  SkRegExpConst.pas (SkRegExp regular expression library)
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

  The Original Code is SkRegExpConst.pas(for SkRegExp Library).

  The Initial Developer of the Original Code is Shuichi Komiya.

  E-mail: shu AT k DOT email DOT ne DOT jp
  URL:    http://komish.com/softlib/skregexp.htm

  Portions created by Shuichi Komiya are
  Copyright (C) 2007-2011 Shuichi Komiya. All Rights Reserved.

*)

unit SkRegExpConst;

interface

{$DEFINE JapaneseMessage}

resourcestring

{$IFDEF JapaneseMessage}
  { Error Message Japanese }

  { Complile error messages }
  sNotRecognized                     = '%s認識できない正規表現です';
  sHexDigitIsRequired                = '%s16進数が必要です';
  sCodePointRangeOver                = '%sコードポイントの範囲を超えています';
  sLoopOfMatchAheadNotSpecified      = '先読みでの繰り返しは指定できません';
  sUnmatchedBigPar                   = '%s [ がマッチしません';
  sMissingRightBraceOnEscx           = '%s } がありません';
  sQuantifierIsTooLarge              = '%s繰り返しの数が大きすぎます';
  sCantDoMaxLessMin                  = '%s最大値より最小値のほうが大きいです';
  sPosixClassUnkown                  = '[:%s:] はPOSIXクラスではありません';
  sInvalideBigParRange               = '文字クラスの範囲指定が違います [%s]';
  sGroupNameIsEmpty                  = '%sグループ名がありません';
  sGroupNumberIsEmpty                = '%sグループ番号がありません';
  sNoEndOfGroup                      = '%sグループが終了していません';
  sUnmatchedSmallPar                 = '%s ( がマッチしません';
  sOptionNotCompleted                = '%sオプションの指定が終了していません';
  sPropertyUnknown                   = '{%s} はプロパティではありません';
  sInvalidProperty                   = '%sプロパティが不正です';
  sMissingRightBrace                 = '%s ] がありません';
  sQuestPosInaccurate                = '?の位置が不正です';
  sLoopOfLengthZeroCannotSpecified   = '%s長さゼロの繰り返しはできません';
  sRegExpNotCompleted                = '%s正規表現が正しく終了していません';
  sInvalidCharactorClass             = '%s不正な文字クラスです';
  sCannotCallMultipleDefineGroupName = '多重定義されたグループ名<%s>の部分式は呼び出せません';
  sInvalidGroupNumber                = 'グループ番号<%d>はこの位置からは参照できません';
  sInvalidGroupName                  = 'グループ名<%s>の書式が間違っています';
  sInvalidCharInGroupName            = 'グループ名の中に不正な文字があります';
  sBehindMatchNotGroup               = '%s戻り読み内でグループは使えません';
  sNeverEndingRecursion              = '%s終了しないかもしない再帰があります';
  sBehindMatchNotVariableLength      = '%s戻り読み内で可変長文字列は使えません';
  sInvalideCondition                 = '%s条件式が不正です';
  sContainsTooManyBranches           = '条件構文の分岐が多すぎます';
  sNotTerminated                     = '%s正規表現が終了していません';
  sInvalidEscapeCharacterSyntax      = '%sエスケープ文字の書式が間違っています';
  sPosixClassSupportedOnlyClass      = '%sPOSIX文字クラスが使えるのは文字クラスの中だけです';
  // end of compile error

  { Runtime error messages }
  sFatalError                        = '致命的エラー';
  sExecFuncNotCall                   = 'Execメソッドが実行されていません';
  sEOLRangeOver                      = '改行文字に指定できるは2文字以内です';
  sUnmatchedCurlyBracket             = '%s { がマッチしません';
  sRangeOverGroupNumber              = 'グループ番号<%d>は範囲を越えています';
  sMissingGroupName                  = 'グループ名<%s>は存在しません';
  sNotFoundOldCode                   = 'BUG: not found OldCode';

{$ELSE}
  { Error Message English }

  { Complile error messages }
  sNotRecognized                     = '%s not recognized';
  sHexDigitIsRequired                = '%sHex-digit is required';

  sCodePointRangeOver                = '%sCodepoint range over';
  sLoopOfMatchAheadNotSpecified      = 'Loop of match ahead not specified';
  sUnmatchedBigPar                   = '%sUnmatched [';
  sMissingRightBraceOnEscx           = '%sMissing right brace on \x{}';
  sQuantifierIsTooLarge              = '%sQuantifier is too large';
  sCantDoMaxLessMin                  = '%sCan''t do {n,m} with n > m';
  sPosixClassUnkown                  = 'Posix class [:%s:] unknown';
  sInvalideBigParRange               = 'Invalid [] range [%s]';
  sGroupNameIsEmpty                  = '%sgroup name is empty';
  sGroupNumberIsEmpty                = '%sGroup number is empty';
  sNoEndOfGroup                      = '%sNo end of group';
  sUnmatchedSmallPar                 = '%sUnmatched (';
  sOptionNotCompleted                = '%sOption not completed';
  sPropertyUnknown                   = 'Property {%s} unkown';
  sInvalidProperty                   = '%sInvalide property';
  sMissingRightBrace                 = '%sMissing right brace';
  sQuestPosInaccurate                = '%s? position is inaccurate';
  sLoopOfLengthZeroCannotSpecified   = '%sLoop of Length zero cannot specified';
  sRegExpNotCompleted                = '%sRegular expression not completed';
  sInvalidCharactorClass             = '%sInvalid charactor class';
  sCannotCallMultipleDefineGroupName =
    'Multiple define group name <%s> can not call'; // check!
  sInvalidGroupNumber                = 'Invalid group number <%d>';
  sInvalidGroupName                  = 'Invalid group name <%s>';
  sInvalidCharInGroupName            = '%sInvalid char in group name';
  sBehindMatchNotGroup               = '%sBehind match not group';
  sNeverEndingRecursion              = '%sNever ending recursion';
  sBehindMatchNotVariableLength      = '%sBehind match not variable length';
  sInvalideCondition                 = '%sInvalid Condition';
  sContainsTooManyBranches           = 'Contains too many Branches';
  sNotTerminated                     = '%sNot terminated';
  sInvalidEscapeCharacterSyntax      = '%sInvalid escape charactoer syntax';
  sPosixClassSupportedOnlyClass      = '%sPOSIX named classes are supported only within a class';
  .
  // end of compile error

  { Runtime error messages }
  sFatalError                        = 'Fatal error';
  sExecFuncNotCall                   = 'Exec function not call';
  sEOLRangeOver                      = 'EOL range over';
  sUnmatchedCurlyBracket             = '%sUnmatched {';
  sRangeOverGroupNumber              = 'Range over group number <%d>';
  sMissingGroupName                  = 'Missing group number <%s>';
  sNotFoundOldCode                   = 'BUG: not found OldCode';

{$ENDIF}  // end of error message

{$IFDEF DEBUG}
  { for debug }
  sLiteral                           = 'EXACT <%s> ';
  sAnyChar                           = 'ANY ';
  sWordChar                          = 'WORD ';
  sNegativeWordChar                  = 'NWORD ';
  sDigitChar                         = 'DIGIT ';
  sNegativeDigitChar                 = 'NDIGIT ';
  sHexDigitChar                      = 'HEXD ';
  sNegativeHexDigitChar              = 'NHEXD ';
  sSpaceChar                         = 'SPACE ';
  sNegativeSpaceChar                 = 'NSPACE ';
  sHorizontalSpaceChar               = 'HSPACE ';
  sNegativeHorizontalSpaceChar       = 'NHSPACE ';
  sVerticalSpaceChar                 = 'VSPACE ';
  sNegativeVerticalSpaceChar         = 'NVSPACE ';
  sLineBreakChar                     = 'LINEBREK ';
  sMsgRange                          = 'RANGE ';
  sCombiningSequence                 = 'COMBSQ ';
  sBoundaryCode                      = 'BOUND ';
  sNegativeBoundaryCode              = 'NBOUND ';
  sFmtGroupReference                 = 'REF%d ';
  sFmtGroupNameReference             = 'REF[%s]';
  sHeadOfLineCode                    = 'BOL';
  sEndOfLineCode                     = 'EOL';
  sTextHeadCode                      = 'SBOS';
  sTextTailCode                      = 'SEOL';
  sTextEndCode                       = 'EOS';
  sPropertyCode                      = 'POSIXB';
  sNegativePropertyCode              = 'NPOSIXB';
  // 1.1.0 add
  sIfThenReference                   = 'Cond<%d>';
  sIfThenNamedReference              = 'CondName<%s>';
  sGlobalPos                         = 'GPOS: ';

  sFmtDumpLeadCodeExist              = 'LeadChar: ';
  sFmtDumpLeadCodeNotExist           = 'LeadChar : No';

  sBinCode_Raise                     = 'bug: not define operator';

  sFmtDumpNFA_Start                  = '%2d : ';
  sFmtDumpNFA_End                    = '%2d : ';
  sFmtDumpNFA_EndStr                 = 'END';
  sFmtDumpNFA_Status                 = '%2d : ';
  sFmtDumpNFA_Empty                  = 'NOTHING (%d) :';
  sFmtDumpNFA_LoopExit               = 'LOOPEXIT (%d) :';
  sFmtDumpNFA_LoopEnd                = 'LOOPEND (%d) :';
  sFmtDumpNFA_Star                   = 'STAR (%d) %s:';
  sFmtDumpNFA_Plus                   = 'PLUS (%d) %s:';
  sFmtDumpNFA_Bound                  = 'BOUND (%d) :';

  sFmtDumpNFA_Loop                   = 'LOOP%s [%d, %d] (%d) :';
  sFmtDumpNFA_Quest                  = 'QUEST (%d) :';
  sFmtDumpNFA_AheadMatch             = 'AMATCH (%d) :';
  sFmtDumpNFA_AheadNoMatch           = 'NAMATCH (%d) :';
  sFmtDumpNFA_BehindMatch            = 'BMATCH (%d) :';
  sFmtDumpNFA_BehindNoMatch          = 'NBMATCH (%d) :';
  sFmtDumpNFA_MatchEnd               = 'EOMATCH (%d) :';
  sFmtDumpNFA_GroupStart             = 'OPEN%d (%d) :';
  sFmtDumpNFA_GroupEnd               = 'CLOSE%d (%d) :';
  sFmtDumpNFA_NoBackTrackBegin       = 'NBACKTRB (%d) :';
  sFmtDumpNFA_Null                   = '%s (%d)  :';
  // 0.9.4 add
  sFmtDumpNFA_GoSub                  = 'GOSUB [%d] (%d) :';
  // 1.1.0 add
  sFmtDumpNFA_IfMatch                = 'IFMATCH (%d) :';
  // 1.1.0 add
  sFmtDumpNFA_IfThen                 = 'IFTHEN (%d) :';
  sFmtDumpNFA_KeepPattern            = 'KEEPL (%d) :';
  // 1.3.0 add
  sFmtDumpNFA_Fail                   = 'FAIL (%d) :';

  //
  sBinCode_Union                     = 'Union "|"';
  sBinCode_Concat                    = 'Concat';
  sBinCode_Emply                     = 'Empty';
  sBinCode_Plus                      = 'Plus "+"';
  sBinCode_Star                      = 'Star "*"';
  sBinCode_Loop                      = 'Repeat ';
  sBinCode_Quest                     = 'Quest "?"';
  sBinCode_Bound                     = 'Bound';
  sBinCode_LHead                     = 'Line head';
  sBinCode_LTail                     = 'Line tail';
  sBinCode_Group                     = 'Group';
  sBinCode_AheadMatch                = 'Ahead match';
  sBinCode_BehindMatch               = 'Behind match';
  sBinCode_AheadNoMatch              = 'Ahead no match';
  sBinCode_BehindNoMatch             = 'Behind no match';
  sBinCode_NoBackTrack               = 'No backtrack';

  // 0.9.4 add
  sBinCode_GroupCall                 = 'Call';
  // 1.1.0 add
  sBinCode_IfMatch                   = 'IfMatch';
  sBinCode_IfThen                    = 'IfThen';
  sBinCode_KeepPattern               = 'KeepLeft';
  // 1.3.0 add
  sBinCode_Fail                      = 'Fail';

{$ENDIF}  // end of Debug;

implementation

end.

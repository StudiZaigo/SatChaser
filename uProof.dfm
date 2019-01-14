object frmProof: TfrmProof
  Left = 653
  Top = 356
  BorderStyle = bsDialog
  Caption = #26657#27491
  ClientHeight = 434
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lblStep: TLabel
    Left = 16
    Top = 8
    Width = 35
    Height = 12
    Caption = 'lblStep'
  end
  object lblComment: TLabel
    Left = 16
    Top = 149
    Width = 350
    Height = 12
    Caption = 'A=B , C=D '#12395#12394#12387#12383#12425#12289#12467#12531#12488#12525#12540#12521#12398#38651#28304#12434#19968#26086#20999#12426#12289#20877#12403#20837#12428#30452#12377
  end
  object btnGoBack: TButton
    Left = 192
    Top = 208
    Width = 65
    Height = 25
    Caption = #25147#12427
    TabOrder = 0
  end
  object btnGoForward: TButton
    Left = 272
    Top = 208
    Width = 65
    Height = 25
    Caption = #36914#12416
    TabOrder = 1
    OnClick = btnGoForwardClick
  end
  object btnExit: TButton
    Left = 352
    Top = 208
    Width = 65
    Height = 25
    Caption = #32066#20102
    Default = True
    TabOrder = 2
    OnClick = btnExitClick
  end
  object pnlStep2: TPanel
    Left = 16
    Top = 192
    Width = 337
    Height = 65
    TabOrder = 3
    object lblE: TLabel
      Left = 64
      Top = 8
      Width = 7
      Height = 12
      Caption = 'E'
    end
    object txtE: TEdit
      Left = 48
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 0
      Text = 'txtE'
    end
  end
  object pnlStep1: TPanel
    Left = 8
    Top = 24
    Width = 337
    Height = 57
    TabOrder = 4
    object lblA: TLabel
      Left = 64
      Top = 8
      Width = 8
      Height = 12
      Caption = 'A'
    end
    object lblB: TLabel
      Left = 128
      Top = 8
      Width = 8
      Height = 12
      Caption = 'B'
    end
    object Label11: TLabel
      Left = 96
      Top = 32
      Width = 12
      Height = 12
      Caption = #65309
    end
    object txtA: TEdit
      Left = 48
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 0
      Text = 'txtA'
    end
    object txtB: TEdit
      Left = 120
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 1
      Text = 'txtB'
    end
  end
  object pnlStep3: TPanel
    Left = 8
    Top = 280
    Width = 337
    Height = 57
    TabOrder = 5
    object lblC: TLabel
      Left = 64
      Top = 8
      Width = 8
      Height = 12
      Caption = 'C'
    end
    object lblD: TLabel
      Left = 128
      Top = 8
      Width = 8
      Height = 12
      Caption = 'D'
    end
    object Label3: TLabel
      Left = 96
      Top = 32
      Width = 12
      Height = 12
      Caption = #65309
    end
    object txtC: TEdit
      Left = 48
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 0
      Text = 'txtc'
    end
    object txtD: TEdit
      Left = 112
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 1
      Text = 'txtD'
    end
  end
  object pnlStep4: TPanel
    Left = 18
    Top = 352
    Width = 335
    Height = 57
    TabOrder = 6
    object lblF: TLabel
      Left = 64
      Top = 8
      Width = 7
      Height = 12
      Caption = 'F'
    end
    object txtF: TEdit
      Left = 48
      Top = 24
      Width = 41
      Height = 20
      Enabled = False
      TabOrder = 0
      Text = 'txtA'
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 200
  end
end

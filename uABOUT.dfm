object AboutBox: TAboutBox
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = #12496#12540#12472#12519#12531#24773#22577
  ClientHeight = 213
  ClientWidth = 324
  Color = clCream
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 9658155
  Font.Height = -11
  Font.Name = #12513#12452#12522#12458
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 17
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 308
    Height = 161
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 20
      Top = 16
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000080020000000000000000000000000000
        0000000099D77900808080000000800000808000008000008080000080000000
        80008000FFFFFF00C0C0C0000000FF0000FFFF0000FF0000FFFF0000FF000000
        FF00FF0006666666666666666666666666666666AAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAA6A88888888888888888888888888888A6A88888888888888888888888
        888888A6A88888888888888888888888888888A6A88888888888888888888888
        888888A6A88888888888888888888888888888A6A888AAAAA888AAAA88AAA888
        AAA888A6A888AA888A8A888A8A888A8A888A88A6A888AA888A8A888A888AA88A
        888888A6A888AA888A88AAAA88A8888AAAAA88A6A888AAAAA888888A8A888A8A
        888A88A6A888AA888A88AAA888AAA888AAA888A6A888AA888A88888888888888
        888888A6A888AA888A88888888888888888888A6A888AAAAA888888888888888
        888888A6A88888888888888888888888888888A6A88888888888888888888888
        888888A6A88888888888888888888888888888A6A888888888888888888AAA88
        888888A6A88AAAAAA888AAA888A888A8888888A6A88AA888888A888A88AAAA88
        888888A6A88AA888888A888A88A88888888888A6A88AA888888A888A88AAAA88
        888888A6A88AA888888A888A88A88A88888888A6A88AA8888888AAA8888AAAA8
        888888A6A88AA8888888888888888888888888A6A88AA8888888888888888888
        888888A6A88AA8888888888888888888888888A6A88888888888888888888888
        888888A6A88888888888888888888888888888A6AAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAA080000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000001}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 88
      Top = 16
      Width = 43
      Height = 17
      Caption = 'Product'
      IsControl = True
    end
    object Version: TLabel
      Left = 93
      Top = 39
      Width = 40
      Height = 17
      Caption = 'Version'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 80
      Width = 53
      Height = 17
      Caption = 'Copyright'
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 101
      Width = 65
      Height = 16
      AutoSize = False
      Caption = 'Comment'
      WordWrap = True
      IsControl = True
    end
    object lblProductName: TLabel
      Left = 154
      Top = 16
      Width = 87
      Height = 17
      Caption = 'lblProductName'
    end
    object lblVersion: TLabel
      Left = 154
      Top = 40
      Width = 53
      Height = 17
      Caption = 'lblVersion'
    end
    object lblCopyRight: TLabel
      Left = 79
      Top = 80
      Width = 69
      Height = 17
      Caption = 'lblCopyRight'
    end
    object lblComment: TLabel
      Left = 79
      Top = 101
      Width = 66
      Height = 17
      Caption = 'lblComment'
    end
    object CompanyName: TLabel
      Left = 8
      Top = 123
      Width = 37
      Height = 17
      Caption = 'Author'
      WordWrap = True
      IsControl = True
    end
    object lblCompanyName: TLabel
      Left = 79
      Top = 123
      Width = 95
      Height = 17
      Caption = 'lblCompanyName'
    end
  end
  object OKButton: TButton
    Left = 127
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end

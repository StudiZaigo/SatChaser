object Main: TMain
  Left = 669
  Top = 378
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'SatChaser on CALSAT32'
  ClientHeight = 124
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clYellow
  Font.Height = -17
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 636
    Height = 124
    Align = alClient
    Color = clDefault
    ParentBackground = False
    PopupMenu = PopupMenu1
    TabOrder = 0
    object lblYMD: TLabel
      Left = 475
      Top = 5
      Width = 153
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = 'lblYMD'
    end
    object lblAOSAz: TLabel
      Left = 122
      Top = 65
      Width = 58
      Height = 15
      Caption = 'lblAOSAz'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clFuchsia
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentFont = False
    end
    object ImgGreen: TImage
      Left = 361
      Top = 96
      Width = 6
      Height = 12
      Picture.Data = {
        07544269746D6170B6000000424DB60000000000000076000000280000000600
        0000100000000100040000000000400000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF0088888800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800}
      Visible = False
    end
    object ImgRed: TImage
      Left = 373
      Top = 96
      Width = 6
      Height = 12
      Picture.Data = {
        07544269746D6170B6000000424DB60000000000000076000000280000000600
        0000100000000100040000000000400000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00888888009999980099999800999998009999980099999800999998009999
        9800999998009999980099999800999998009999980099999800999998009999
        9800}
      Visible = False
    end
    object btnTracking: TSpeedButton
      Left = 503
      Top = 23
      Width = 113
      Height = 38
      GroupIndex = 1
      Down = True
      Caption = '&Tracking On'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clPurple
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentFont = False
      OnClick = btnTrackingClick
    end
    object lblSatelliteAz: TLabel
      Left = -1
      Top = 26
      Width = 95
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblSatelliteAz'
    end
    object lblSatelliteEl: TLabel
      Left = 4
      Top = 44
      Width = 90
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblSatelliteEl'
    end
    object Label1: TLabel
      Left = 43
      Top = 5
      Width = 51
      Height = 15
      Alignment = taRightJustify
      Caption = 'Satellite'
      Color = clWhite
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblAntennaAz: TLabel
      Left = 92
      Top = 26
      Width = 96
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblAntennaAz'
    end
    object lblAntennaEl: TLabel
      Left = 99
      Top = 44
      Width = 91
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblAntennaEl'
    end
    object Label2: TLabel
      Left = 137
      Top = 5
      Width = 53
      Height = 15
      Alignment = taRightJustify
      Caption = 'Antenna'
      Color = clWhite
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object ImgFlipMode: TImage
      Left = 289
      Top = 48
      Width = 6
      Height = 12
      Picture.Data = {
        07544269746D6170B6000000424DB60000000000000076000000280000000600
        0000100000000100040000000000400000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF0088888800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800}
    end
    object ImgOverlap: TImage
      Left = 289
      Top = 30
      Width = 6
      Height = 12
      Picture.Data = {
        07544269746D6170B6000000424DB60000000000000076000000280000000600
        0000100000000100040000000000400000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF0088888800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAAA800AAAA
        A800}
    end
    object lblRotatorAz: TLabel
      Left = 196
      Top = 26
      Width = 90
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblRotatorAz'
    end
    object lblRotatorEl: TLabel
      Left = 201
      Top = 44
      Width = 85
      Height = 17
      Alignment = taRightJustify
      Caption = 'lblRotatorEl'
    end
    object Label3: TLabel
      Left = 236
      Top = 5
      Width = 47
      Height = 15
      Alignment = taRightJustify
      Caption = 'Rotator'
      Color = clWhite
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblRotateModeName: TLabel
      Left = 312
      Top = 5
      Width = 44
      Height = 15
      Alignment = taCenter
      Caption = 'Manual'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clFuchsia
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentFont = False
    end
    object bbOff: TSpeedButton
      Left = 16
      Top = 86
      Width = 109
      Height = 22
      Caption = 'Off'
    end
    object btnStopTracking: TSpeedButton
      Left = 131
      Top = 86
      Width = 145
      Height = 22
      Caption = 'StopTracking'
    end
    object lblSatellite: TLabel
      Left = 16
      Top = 65
      Width = 64
      Height = 15
      Caption = 'lblSatellite'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clFuchsia
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 15
      Top = 26
      Width = 16
      Height = 15
      Caption = 'Az'
      Color = clWhite
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label5: TLabel
      Left = 15
      Top = 44
      Width = 11
      Height = 15
      Caption = 'El'
      Color = clWhite
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'MS UI Gothic'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object imgFocused: TImage
      Left = 15
      Top = 7
      Width = 25
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000F00
        00000F0000000100040000000000780000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00000000000000000000000000000000000000007770000000000077777770
        0000000777777777000000077777777700000077777777777000007777777777
        7000007777777777700000077777777700000007777777770000000077777770
        0000000000777000000000000000000000000000000000000000}
    end
    object btnTest: TButton
      Left = 397
      Top = 65
      Width = 73
      Height = 25
      Caption = 'btnTest'
      TabOrder = 0
      Visible = False
      OnClick = btnTestClick
    end
    object Panel2: TPanel
      Left = 312
      Top = 23
      Width = 161
      Height = 38
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'Panel2'
      TabOrder = 1
      object bbLeft: TSpeedButton
        Left = 7
        Top = 3
        Width = 33
        Height = 33
        AllowAllUp = True
        GroupIndex = 2
        Glyph.Data = {
          06020000424D0602000000000000760000002800000019000000190000000100
          0400000000009001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          08FFFFFFFFFFF0000000FFFFFFFFFFF008FFFFFFFFFFF0000000FFFFFFFFFF00
          08FFFFFFFFFFF0000000FFFFFFFFF00008FFFFFFFFFFF0000000FFFFFFFF0000
          08888888888880000000FFFFFFF0000000000000000080000000FFFFFF000000
          00000000000080000000FFFFF000000000000000000080000000FFFF00000000
          00000000000080000000FFF00000000000000000000080000000FF0000000000
          00000000000080000000FFF00000000000000000000080000000FFFF00000000
          00000000000080000000FFFFF000000000000000000080000000FFFFFF000000
          00000000000080000000FFFFFFF00000000000000000F0000000FFFFFFFF0000
          0FFFFFFFFFFFF0000000FFFFFFFFF0000FFFFFFFFFFFF0000000FFFFFFFFFF00
          0FFFFFFFFFFFF0000000FFFFFFFFFFF00FFFFFFFFFFFF0000000FFFFFFFFFFFF
          0FFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000}
        Spacing = 0
        OnClick = bbGroupeDownClick
      end
      object bbRight: TSpeedButton
        Tag = 1
        Left = 46
        Top = 3
        Width = 33
        Height = 33
        AllowAllUp = True
        GroupIndex = 2
        Glyph.Data = {
          06020000424D0602000000000000760000002800000019000000190000000100
          0400000000009001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          08FFFFFFFFFFF0000000FFFFFFFFFFFF008FFFFFFFFFF0000000FFFFFFFFFFFF
          0008FFFFFFFFF0000000FFFFFFFFFFFF00008FFFFFFFF0000000FF8888888888
          000008FFFFFFF0000000F000000000000000008FFFFFF0000000F00000000000
          00000008FFFFF0000000F00000000000000000008FFFF0000000F00000000000
          0000000008FFF0000000F0000000000000000000008FF0000000F00000000000
          00000000000FF0000000F000000000000000000000FFF0000000F00000000000
          000000000FFFF0000000F0000000000000000000FFFFF0000000F00000000000
          0000000FFFFFF0000000F00000000000000000FFFFFFF0000000FFFFFFFFFFFF
          00000FFFFFFFF0000000FFFFFFFFFFFF0000FFFFFFFFF0000000FFFFFFFFFFFF
          000FFFFFFFFFF0000000FFFFFFFFFFFF00FFFFFFFFFFF0000000FFFFFFFFFFFF
          0FFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000}
        Spacing = 0
        OnClick = bbGroupeDownClick
      end
      object bbUp: TSpeedButton
        Tag = 3
        Left = 124
        Top = 3
        Width = 33
        Height = 33
        AllowAllUp = True
        GroupIndex = 2
        Glyph.Data = {
          06020000424D0602000000000000760000002800000019000000190000000100
          0400000000009001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFF8888
          8888888FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF0000000000088888FF0000000FF0000000000
          00000000000FF0000000FFF0000000000000000000FFF0000000FFFF00000000
          000000000FFFF0000000FFFFF000000000000000FFFFF0000000FFFFFF000000
          0000000FFFFFF0000000FFFFFFF00000000000FFFFFFF0000000FFFFFFFF0000
          00000FFFFFFFF0000000FFFFFFFFF0000000FFFFFFFFF0000000FFFFFFFFFF00
          000FFFFFFFFFF0000000FFFFFFFFFFF000FFFFFFFFFFF0000000FFFFFFFFFFFF
          0FFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000}
        Spacing = 0
        OnClick = bbGroupeDownClick
      end
      object bbDown: TSpeedButton
        Tag = 2
        Left = 85
        Top = 3
        Width = 33
        Height = 33
        AllowAllUp = True
        GroupIndex = 2
        Glyph.Data = {
          06020000424D0602000000000000760000002800000019000000190000000100
          0400000000009001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          08FFFFFFFFFFF0000000FFFFFFFFFFF0008FFFFFFFFFF0000000FFFFFFFFFF00
          0008FFFFFFFFF0000000FFFFFFFFF00000008FFFFFFFF0000000FFFFFFFF0000
          000008FFFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFF000000
          00000008FFFFF0000000FFFFF0000000000000008FFFF0000000FFFF00000000
          0000000008FFF0000000FFF00000000000000000008FF0000000FF0000000000
          00000000000FF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          0000008FFFFFF0000000FFFFFFF000000000008FFFFFF0000000FFFFFFF00000
          000000FFFFFFF0000000FFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFF
          FFFFFFFFFFFFF0000000}
        Spacing = 0
        OnClick = bbGroupeDownClick
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 560
    Top = 64
  end
  object ActionList1: TActionList
    Left = 520
    Top = 64
    object actOptions: TAction
      Caption = 'Options'
      ShortCut = 16474
      OnExecute = actOptionsExecute
    end
    object actTerminal: TAction
      Caption = 'Terminal'
    end
    object actExit: TAction
      Caption = 'Exit'
      ShortCut = 16472
      OnExecute = actExitExecute
    end
    object actGotoParking: TAction
      Caption = 'Go to Parking'
      ShortCut = 16464
      OnExecute = actGotoParkingExecute
    end
    object actAboutBox: TAction
      Caption = 'AboutBox'
      ShortCut = 16449
      OnExecute = actAboutBoxExecute
    end
    object actDebug: TAction
      Caption = 'Debug'
      ShortCut = 16452
      OnExecute = actDebugExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 432
    Top = 64
    object Options1: TMenuItem
      Action = actOptions
    end
    object erminal1: TMenuItem
      Action = actTerminal
      ShortCut = 16468
    end
    object GotoParking1: TMenuItem
      Action = actGotoParking
    end
    object AboutBox1: TMenuItem
      Action = actAboutBox
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Action = actExit
    end
  end
end

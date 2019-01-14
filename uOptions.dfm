object Options: TOptions
  Left = 746
  Top = 215
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 241
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 351
    Height = 241
    ActivePage = tabApp
    Align = alClient
    TabOrder = 0
    object tabApp: TTabSheet
      Caption = 'Application'
      object Label7: TLabel
        Left = 9
        Top = 12
        Width = 57
        Height = 12
        Caption = 'Parent App'
      end
      object Label14: TLabel
        Left = 9
        Top = 84
        Width = 103
        Height = 12
        Caption = 'Satellite information'
      end
      object Label16: TLabel
        Left = 9
        Top = 36
        Width = 37
        Height = 12
        Caption = 'INIFILE'
      end
      object cmbApp: TComboBox
        Left = 81
        Top = 9
        Width = 89
        Height = 20
        TabOrder = 0
        Text = 'Calsat32'
        OnChange = cmbAppChange
        Items.Strings = (
          'CALSAT32'
          'CALSAT99')
      end
      object edtInifile: TEdit
        Left = 8
        Top = 56
        Width = 265
        Height = 20
        TabOrder = 1
      end
      object btnFile1: TButton
        Left = 280
        Top = 54
        Width = 57
        Height = 25
        Caption = 'Refer'#12539#12539
        TabOrder = 2
      end
      object edtElements: TEdit
        Left = 8
        Top = 104
        Width = 265
        Height = 20
        TabOrder = 3
      end
      object btnFile2: TButton
        Left = 280
        Top = 102
        Width = 57
        Height = 25
        Caption = 'Refer'#12539#12539
        TabOrder = 4
        OnClick = btnFile2Click
      end
      object btnOk0: TButton
        Left = 8
        Top = 168
        Width = 57
        Height = 33
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 5
        OnClick = btnOk0Click
      end
      object btnCancel0: TButton
        Left = 80
        Top = 168
        Width = 57
        Height = 33
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 6
        OnClick = btnCancel0Click
      end
      object edtRegKey: TEdit
        Left = 8
        Top = 136
        Width = 265
        Height = 20
        TabOrder = 7
        Text = 'edtRegKey'
      end
    end
    object tabCom: TTabSheet
      Caption = 'Com port'
      object Label1: TLabel
        Left = 15
        Top = 14
        Width = 41
        Height = 12
        Caption = 'Port No.'
      end
      object Label2: TLabel
        Left = 15
        Top = 38
        Width = 54
        Height = 12
        Caption = 'Baud Rate'
      end
      object Label3: TLabel
        Left = 15
        Top = 62
        Width = 49
        Height = 12
        Caption = 'Data Bits'
      end
      object Label4: TLabel
        Left = 15
        Top = 86
        Width = 30
        Height = 12
        Caption = 'Parity'
      end
      object Label5: TLabel
        Left = 15
        Top = 110
        Width = 48
        Height = 12
        Caption = 'Stop Bits'
      end
      object Label6: TLabel
        Left = 15
        Top = 134
        Width = 65
        Height = 12
        Caption = 'Flow Cotorol'
      end
      object cmbPort: TComboBox
        Left = 112
        Top = 8
        Width = 97
        Height = 20
        Sorted = True
        TabOrder = 0
        Text = 'Com3'
        OnChange = cmbPortChange
      end
      object cmbBaudRate: TComboBox
        Left = 112
        Top = 32
        Width = 97
        Height = 20
        TabOrder = 1
        Text = '9600'
        Items.Strings = (
          '1200'
          '2400'
          '4800'
          '9600'
          '19200'
          '57600'
          '115200'
          '144000'
          '384000')
      end
      object cmbDataBits: TComboBox
        Left = 112
        Top = 56
        Width = 97
        Height = 20
        TabOrder = 2
        Text = '8'
        Items.Strings = (
          '8')
      end
      object cmbParityBits: TComboBox
        Left = 112
        Top = 80
        Width = 97
        Height = 20
        TabOrder = 3
        Text = #12497#12522#12486#12451#12394#12375
        Items.Strings = (
          'None'
          'Odd'
          'Even'
          '')
      end
      object cmbStopBits: TComboBox
        Left = 112
        Top = 104
        Width = 97
        Height = 20
        TabOrder = 4
        Text = '1'
        Items.Strings = (
          '1'
          '1.5'
          '2')
      end
      object cmbFlowControl: TComboBox
        Left = 112
        Top = 128
        Width = 97
        Height = 20
        TabOrder = 5
        Text = #12394#12375
        Items.Strings = (
          'None'
          'Hardware'
          'Software')
      end
      object btnCancel1: TButton
        Left = 256
        Top = 48
        Width = 65
        Height = 33
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 7
        OnClick = btnCancel0Click
      end
      object btnOk1: TButton
        Left = 256
        Top = 9
        Width = 65
        Height = 33
        Cancel = True
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 6
        OnClick = btnOk0Click
      end
    end
    object tabGs232: TTabSheet
      Caption = 'GS232'
      ImageIndex = 2
      object grpElevation: TGroupBox
        Left = 3
        Top = 80
        Width = 246
        Height = 39
        Caption = 'Elevation rotator'
        TabOrder = 0
        object rbtEl90: TRadioButton
          Left = 93
          Top = 19
          Width = 72
          Height = 17
          Caption = '90'#176
          TabOrder = 0
        end
        object rbtEl180: TRadioButton
          Left = 22
          Top = 19
          Width = 65
          Height = 17
          Caption = '180'#176
          Checked = True
          TabOrder = 1
          TabStop = True
        end
      end
      object grpParking: TGroupBox
        Left = 3
        Top = 125
        Width = 249
        Height = 76
        Caption = 'Parking'
        TabOrder = 1
        object Label12: TLabel
          Left = 16
          Top = 24
          Width = 41
          Height = 12
          Caption = 'Azimuth'
        end
        object Label13: TLabel
          Left = 16
          Top = 48
          Width = 47
          Height = 12
          Caption = 'Elevation'
        end
        object edtParkingAz: TEdit
          Left = 69
          Top = 20
          Width = 33
          Height = 20
          TabOrder = 0
          Text = '0'
        end
        object edtParkingEl: TEdit
          Left = 69
          Top = 43
          Width = 33
          Height = 20
          TabOrder = 1
          Text = '90'
        end
        object cbxGoParking: TCheckBox
          Left = 120
          Top = 52
          Width = 110
          Height = 17
          Caption = 'Go to Pparking'
          TabOrder = 2
        end
        object btnGetPosition: TButton
          Left = 120
          Top = 18
          Width = 110
          Height = 28
          Caption = 'Get position'
          TabOrder = 3
          OnClick = btnGetPositionClick
        end
      end
      object btnOk2: TButton
        Left = 272
        Top = 24
        Width = 57
        Height = 33
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 2
        OnClick = btnOk0Click
      end
      object btnCancel2: TButton
        Left = 272
        Top = 64
        Width = 57
        Height = 33
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 3
        OnClick = btnCancel0Click
      end
      object grpAzimuth: TGroupBox
        Left = 9
        Top = 3
        Width = 243
        Height = 71
        Caption = 'Azimuth rotator'
        TabOrder = 4
        object Label15: TLabel
          Left = 116
          Top = 42
          Width = 31
          Height = 12
          Caption = 'Speed'
        end
        object Label8: TLabel
          Left = 16
          Top = 42
          Width = 32
          Height = 12
          Caption = 'Offset'
        end
        object rbtAz450: TRadioButton
          Left = 24
          Top = 16
          Width = 49
          Height = 17
          Caption = '450'#176
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbtAz360: TRadioButton
          Left = 79
          Top = 15
          Width = 57
          Height = 17
          Caption = '360'#176
          TabOrder = 1
        end
        object cmbSpeed: TComboBox
          Left = 159
          Top = 39
          Width = 72
          Height = 20
          TabOrder = 3
          Text = 'Low'
          Items.Strings = (
            'Low'
            'Medium 1'
            'Medium 2'
            'High')
        end
        object edtOffset: TEdit
          Left = 56
          Top = 39
          Width = 49
          Height = 20
          TabOrder = 2
          Text = 'edtOffset'
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 300
    Top = 183
  end
end

object Options: TOptions
  Left = 746
  Top = 215
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 248
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 351
    Height = 248
    ActivePage = tabGs232
    Align = alClient
    TabOrder = 0
    object tabApp: TTabSheet
      Caption = 'Application'
      object Label7: TLabel
        Left = 9
        Top = 12
        Width = 70
        Height = 15
        Caption = 'Parent App'
      end
      object Label14: TLabel
        Left = 9
        Top = 84
        Width = 151
        Height = 15
        Caption = 'Satellite information file'
      end
      object Label16: TLabel
        Left = 9
        Top = 36
        Width = 46
        Height = 15
        Caption = 'INIFILE'
      end
      object cmbApp: TComboBox
        Left = 85
        Top = 9
        Width = 108
        Height = 23
        TabOrder = 0
        Text = 'CALSAT32'
        OnChange = cmbAppChange
        Items.Strings = (
          'CALSAT32')
      end
      object edtInifile: TEdit
        Left = 8
        Top = 56
        Width = 265
        Height = 23
        TabOrder = 1
      end
      object btnFile1: TButton
        Left = 280
        Top = 54
        Width = 57
        Height = 25
        Caption = 'Refer'#12539#12539
        TabOrder = 2
        OnClick = btnFile1Click
      end
      object edtElements: TEdit
        Left = 8
        Top = 104
        Width = 265
        Height = 23
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
        Height = 23
        TabOrder = 7
        Text = 'edtRegKey'
      end
    end
    object tabGs232: TTabSheet
      Caption = 'GS232'
      ImageIndex = 2
      object Label9: TLabel
        Left = 13
        Top = 10
        Width = 47
        Height = 15
        Caption = 'Interval'
      end
      object Label10: TLabel
        Left = 144
        Top = 10
        Width = 39
        Height = 15
        Caption = 'mSec.'
      end
      object grpParking: TGroupBox
        Left = 3
        Top = 149
        Width = 262
        Height = 68
        Caption = 'Parking'
        TabOrder = 2
        object Label12: TLabel
          Left = 12
          Top = 18
          Width = 51
          Height = 15
          Caption = 'Azimuth'
        end
        object Label13: TLabel
          Left = 12
          Top = 44
          Width = 57
          Height = 15
          Caption = 'Elevation'
        end
        object edtParkingAz: TEdit
          Left = 77
          Top = 17
          Width = 33
          Height = 23
          Alignment = taRightJustify
          ImeMode = imDisable
          NumbersOnly = True
          TabOrder = 0
          Text = '0'
        end
        object edtParkingEl: TEdit
          Left = 77
          Top = 42
          Width = 33
          Height = 23
          Alignment = taRightJustify
          ImeMode = imDisable
          NumbersOnly = True
          TabOrder = 1
          Text = '90'
        end
        object cbxGoParking: TCheckBox
          Left = 133
          Top = 46
          Width = 110
          Height = 17
          Caption = 'Go to Parking'
          TabOrder = 2
        end
        object btnGetPosition: TButton
          Left = 133
          Top = 12
          Width = 110
          Height = 28
          Caption = 'Get Position'
          TabOrder = 3
          OnClick = btnGetPositionClick
        end
      end
      object btnOk2: TButton
        Left = 272
        Top = 45
        Width = 57
        Height = 33
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 3
        OnClick = btnOk0Click
      end
      object btnCancel2: TButton
        Left = 272
        Top = 89
        Width = 57
        Height = 33
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 4
        OnClick = btnCancel0Click
      end
      object grpAzimuth: TGroupBox
        Left = 3
        Top = 30
        Width = 250
        Height = 71
        Caption = 'Azimuth rotator'
        TabOrder = 1
        object Label15: TLabel
          Left = 129
          Top = 46
          Width = 39
          Height = 15
          Caption = 'Speed'
        end
        object Label8: TLabel
          Left = 129
          Top = 23
          Width = 41
          Height = 15
          Caption = 'Offset'
        end
        object rgpAzimuth: TRadioGroup
          Left = 3
          Top = 17
          Width = 115
          Height = 39
          Color = clBtnFace
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            '450'#176
            '360'#176)
          ParentColor = False
          TabOrder = 0
        end
        object cmbRotateSpeed: TComboBox
          Left = 180
          Top = 43
          Width = 60
          Height = 23
          TabOrder = 2
          Text = 'Low'
          Items.Strings = (
            'Low'
            'Medium 1'
            'Medium 2'
            'High')
        end
        object edtAzOffset: TEdit
          Left = 176
          Top = 14
          Width = 45
          Height = 23
          Alignment = taRightJustify
          ImeMode = imDisable
          TabOrder = 1
          Text = 'edtAzOffset'
        end
      end
      object edtIntervalTime: TEdit
        Left = 81
        Top = 3
        Width = 57
        Height = 23
        Alignment = taRightJustify
        ImeMode = imDisable
        MaxLength = 5
        NumbersOnly = True
        TabOrder = 0
        Text = 'edtIntervalTime'
      end
      object rgpElevation: TRadioGroup
        Left = 3
        Top = 104
        Width = 210
        Height = 39
        Caption = 'Elevation rotator'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          '180'#176
          '90'#176
          'None')
        TabOrder = 5
      end
    end
    object tabCom: TTabSheet
      Caption = 'COM port'
      object Label1: TLabel
        Left = 15
        Top = 14
        Width = 53
        Height = 15
        Caption = 'Port No.'
      end
      object Label2: TLabel
        Left = 15
        Top = 44
        Width = 66
        Height = 15
        Caption = 'Baud Rate'
      end
      object Label3: TLabel
        Left = 15
        Top = 74
        Width = 59
        Height = 15
        Caption = 'Data Bits'
      end
      object Label4: TLabel
        Left = 15
        Top = 105
        Width = 36
        Height = 15
        Caption = 'Parity'
      end
      object Label5: TLabel
        Left = 15
        Top = 135
        Width = 59
        Height = 15
        Caption = 'Stop Bits'
      end
      object Label6: TLabel
        Left = 15
        Top = 166
        Width = 81
        Height = 15
        Caption = 'Flow Cotorol'
      end
      object cmbComPort: TComboBox
        Left = 112
        Top = 8
        Width = 97
        Height = 23
        Sorted = True
        TabOrder = 0
        Text = 'Com3'
        OnChange = cmbComPortChange
      end
      object cmbBaudRate: TComboBox
        Left = 112
        Top = 39
        Width = 97
        Height = 23
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
        Top = 70
        Width = 97
        Height = 23
        TabOrder = 2
        Text = '8'
        Items.Strings = (
          '8')
      end
      object cmbParity: TComboBox
        Left = 112
        Top = 101
        Width = 97
        Height = 23
        TabOrder = 3
        Text = #12497#12522#12486#12451#12394#12375
        Items.Strings = (
          'None'
          'Odd'
          'Even'
          '')
      end
      object cmbStopBits: TComboBox
        Left = 113
        Top = 132
        Width = 97
        Height = 23
        TabOrder = 4
        Text = '1'
        Items.Strings = (
          '1'
          '2')
      end
      object cmbFlowControl: TComboBox
        Left = 113
        Top = 163
        Width = 97
        Height = 23
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
  end
  object OpenDialog1: TOpenDialog
    Left = 300
    Top = 183
  end
end

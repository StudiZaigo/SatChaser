object Terminal: TTerminal
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Terminal'
  ClientHeight = 408
  ClientWidth = 367
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object edtMsg: TEdit
    Left = 9
    Top = 377
    Width = 117
    Height = 21
    CharCase = ecUpperCase
    ImeMode = imDisable
    TabOrder = 0
    Text = 'EDTMSG'
    OnKeyDown = edtMsgKeyDown
    OnKeyPress = edtMsgKeyPress
  end
  object memMsg: TMemo
    Left = 8
    Top = 8
    Width = 351
    Height = 361
    TabStop = False
    Lines.Strings = (
      'memMsg')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnClose: TButton
    Left = 294
    Top = 375
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object btnClear: TButton
    Left = 223
    Top = 375
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Clear'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object btnSend: TButton
    Left = 132
    Top = 375
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object ActionList1: TActionList
    Left = 128
    Top = 152
    object actClose: TAction
      Caption = 'Close'
      ShortCut = 16472
    end
  end
end

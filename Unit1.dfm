object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 456
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 192
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'http://www.uszcn.com'
  end
  object Button1: TButton
    Left = 448
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Visit'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 152
    Top = 144
    Width = 513
    Height = 65
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 152
    Top = 215
    Width = 513
    Height = 50
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button2: TButton
    Left = 560
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 4
    OnClick = Button2Click
  end
  object WebBrowser1: TWebBrowser
    Left = 152
    Top = 280
    Width = 513
    Height = 150
    TabOrder = 5
    ControlData = {
      4C00000005350000810F00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 40
    Top = 176
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    MaxLineAction = maException
    Port = 0
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 40
    Top = 272
  end
  object IdCookieManager1: TIdCookieManager
    Left = 40
    Top = 368
  end
end

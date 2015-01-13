unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls, IdCookieManager,
  Vcl.OleCtrls, SHDocVw, IdMultipartFormData;

type
  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    IdCookieManager1: TIdCookieManager;
    Button2: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit4: TEdit;
    Button3: TButton;

    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Button1Click(Sender: TObject);
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Params: TStrings;
  i: integer;
begin
  IdSSLIOHandlerSocketOpenSSL1.SSLOptions.Method := sslvSSLv3;
  IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
  IdHTTP1.AllowCookies := TRUE;

  // Request Header
  IdHTTP1.Request.UserAgent :=
    'Mozilla/5.0 (Windows NT 6.1; rv:34.0) Gecko/20100101 Firefox/34.0';
  IdHTTP1.Request.Accept :=
    'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  IdHTTP1.Request.AcceptLanguage := 'en-us,zh-cn;q=0.7,en;q=0.3';
  // IdHTTP1.Request.AcceptEncoding := 'gzip, deflate';
  IdHTTP1.Request.Connection := 'keep-alive';

  // Proxy Setting
  // IdHTTP1.ProxyParams.ProxyServer := '127.0.0.1';
  // IdHTTP1.ProxyParams.ProxyPort := 8080;

  // Memo1.Text := IdHTTP1.Get(Edit1.Text);

  Params := TStringList.Create;
  Params.Add('login=' + Edit2.Text);
  Params.Add('rememberMe=false');
  Params.Add('password=' + Edit3.Text);

  try
    Memo1.Text := IdHTTP1.Post(Edit1.Text, Params);
  except
    on E: Exception do
    begin
      Memo1.Text := 'Not Logged In.' + E.Message;
    end;
  end;

  for i := 0 to IdHTTP1.CookieManager.CookieCollection.Count - 1 do
    Memo2.Text := Memo2.Text + #13#10 + IdHTTP1.CookieManager.CookieCollection.
      Cookies[i].ClientCookie;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ResponseData: TStringStream;
  i: integer;
begin
  try
    IdHTTP1.Request.Referer := 'http://store.nike.com/cn/zh_cn/pd/';
    Memo1.Text := IdHTTP1.Get
      ('https://secure-store.nike.com/ap/services/jcartService?callback=nike_Cart_hanleJCartResponse&action=addItem&lang_locale=zh_CN&country=CN&catalogId=4&productId='
      + Edit4.Text +
      '&price=1699.0&siteId=null&line1=Air+Jordan+XX9&line2=%E7%94%B7%E5%AD%90%E7%AF%AE%E7%90%83%E9%9E%8B&passcode=null&sizeType=null&skuAndSize=10702226%3A40.5&qty=1&rt=&view=3&skuId=10702226&displaySize=40.5&_=1420792220969');
  except
    on E: Exception do
    begin
      Memo1.Text := 'ERR:' + E.Message;
    end;
  end;
    for i := 0 to IdHTTP1.CookieManager.CookieCollection.Count - 1 do
    Memo2.Text := Memo2.Text + #13#10 + IdHTTP1.CookieManager.CookieCollection.
      Cookies[i].ClientCookie;
end;

end.

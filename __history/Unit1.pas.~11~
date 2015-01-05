unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls, IdCookieManager,
  Vcl.OleCtrls, SHDocVw;

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
    WebBrowser1: TWebBrowser;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  IdSSLIOHandlerSocketOpenSSL1.SSLOptions.Method := sslvSSLv3;
  IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
  IdHTTP1.AllowCookies := TRUE;

//Request Header
  IdHTTP1.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; rv:34.0) Gecko/20100101 Firefox/34.0';
  IdHTTP1.Request.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
  IdHTTP1.Request.AcceptLanguage := 'en-us,zh-cn;q=0.7,en;q=0.3';
//  IdHTTP1.Request.AcceptEncoding := 'gzip, deflate';
  IdHTTP1.Request.Connection := 'keep-alive';

//Proxy Setting
//  IdHTTP1.ProxyParams.ProxyServer := '127.0.0.1';
//  IdHTTP1.ProxyParams.ProxyPort := 8080;


  Memo1.Text := IdHTTP1.Get(Edit1.Text);

  Memo2.Text := IdHTTP1.CookieManager.CookieCollection.Cookies[0].ClientCookie;



end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

end.

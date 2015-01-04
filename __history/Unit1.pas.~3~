unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, Vcl.StdCtrls, IdCookieManager;

type
  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    IdCookieManager1: TIdCookieManager;     //搞清楚Cookie的管理
    procedure Button1Click(Sender: TObject);
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
  Memo1.Text := IdHTTP1.Get(Edit1.Text);
end;

end.

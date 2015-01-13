unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OLB_HTTP, Unit1, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm1;
  testUser: TBuyer; // 账户信息
  testItem: TStoreItem; // 商品信息
  testHTTP: TWebAccess; // 会话进程

implementation

{$R *.dfm}



procedure TForm2.Button1Click(Sender: TObject);
begin
    TForm1.
end;

end.

unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Memo1: TMemo;
    Button1: TButton;
    Edit4: TEdit;
    Label4: TLabel;

    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses OLB_HTTP;

var
  testHTTP: TWebAccess;
  testUser: TBuyer;
  testItem: TStoreItem;

procedure TForm2.Button1Click(Sender: TObject);

begin
  testHTTP := TWebAccess.Create();

  testUser.BuyerID := 1;
  testUser.BuyerName := 'test';
  testUser.StoreAccount := Edit1.Text;
  testUser.StorePassword := Edit2.Text;

  testItem.StoreItemID := Edit3.Text;
  testItem.StoreItemProperty := Edit4.Text;

  Memo1.Text := testHTTP.PlaceOrder(testUser, testItem);
end;

end.

unit OLB_HTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdCookieManager, Vcl.Forms;

type
  TBuyer = record                    //本地抢购账户
    BuyerID : integer;               //本地账户ID
    BuyerName : string;              //本地账户名称
    StoreAccount : string;           //本地账户关联的网店账户用户名
    StorePassword : string;          //本地账户关联的网店账户密码
  end;

  TStoreItem = record                //商品信息
    StoreItemID : string;            //商品货号
    StoreItemProperty :string;       //商品属性
  end;

  TWebAccess = class(TThread)
  private
   { Private declarations }
    FBuyer : TBuyer;                  //每个访问线程中包含一个本地用户
    FThreadID : Integer;              //线程ID
    FIdHTTP : TIdHTTP;                //HTTP协议栈
    FCookie : TIdCookieManager;       //Cookie
    FOpenSSL : TIdSSLIOHandlerSocketOpenSSL;      //SSL v3

    FTempInfo : string;               //过程消息

    procedure InitCookies;            //初始化Cookie
    procedure InitHTTP;               //初始化HTTP
    function HTTPGetAction;           //HTTP GET
    function HTTPPostAction;          //HTTP POST
    procedure PlaceOrder;             //下订单的过程
    procedure ShowError;              //错误提示

   protected
     { Protected declarations }
    procedure Execute; override;

   public
     { Public declarations }
    property ThreadID : Integer read FThreadID write FThreadID;
    constructor Create(CreateSuspended : Boolean; ID : Integer; Buyer : TBuyer); overload;
    function DecodeUTF8toAnsi;        //UTF8-ANSI编码转换
    function ExtractHtmlTagValues;    //取得HTML标签对应值
    function GetAttributeByName;      //用已知的属性值取另外一个属性值

  end;

implementation

  constructor TWebAccess.Create(CreateSuspended: Boolean; ID: Integer; Buyer: TBuyer);
  begin
    FBuyer := Buyer;
    FThreadID := ID;
    InitHTTP;
    inherited Create(CreateSuspended);
    FreeOnTerminate := True;
  end;

  procedure TWebAccess.Execute;        //考虑使用多线程，异步
  begin
    try
      PlaceOrder;
    except
      on E : Exception do
      begin
        FTempInfo := E.Message;
        Synchronize(ShowError);
      end;
    end;
  end;

  procedure TWebAccess.InitHTTP;
  begin
    FIdHTTP := TIdHTTP.Create();
    FCookie := TIdCookieManager.Create();

    FIdHTTP.CookieManager := FCookie;
    FIdHTTP.AllowCookies := TRUE;
    FIdHTTP.HandleRedirects := TRUE;
    //完成HTTP REQ的设置
  end;

  procedure TWebAccess.InitCookies;
  begin
    //登陆取得Cookie，然后添加必要内容
    FCookie.AddServerCookie('sth=value', 'nike.com');
  end;

  procedure TWebAccess.PlaceOrder;
  begin
  //登陆
  //获取目标页面
  //获取目标状态
  //加入购物车
  end;

  procedure TWebAccess.ShowError;
  begin
    //Form1.Memo1.Lines.Add( '（' + IntToStr(FUser.ID) + '）' + FUser.UserName + '：' + TempMsg);
    Application.MessageBox(FTempInfo, 'ERROR',0);
  end;

  function TWebAccess.HTTPGetAction;
  begin

  end;


end.

unit OLB_HTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Stdctrls,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdCookieManager, Vcl.Forms,
  IdMultipartFormData;

type
  TBuyer = record // 本地抢购账户
    BuyerID: integer; // 本地账户ID
    BuyerName: string; // 本地账户名称
    StoreAccount: string; // 本地账户关联的网店账户用户名
    StorePassword: string; // 本地账户关联的网店账户密码
  end;

  TStoreItem = record // 商品信息
    StoreItemID: string; // 商品货号
    StoreItemProperty: string; // 商品属性
  end;

  TWebAccess = class(TThread)
  private
    { Private declarations }
    FBuyer: TBuyer; // 每个访问线程中包含一个本地用户
    FThreadID: integer; // 线程ID
    FIdHTTP: TIdHTTP; // HTTP协议栈
    FCookie: TIdCookieManager; // Cookie
    FOpenSSL: TIdSSLIOHandlerSocketOpenSSL; // SSL v3

    FTempInfo: string; // 过程消息

    procedure InitCookies; // 初始化Cookie
    procedure InitHTTP; // 初始化HTTP
    function HTTPGetAction(URL: string; MaxRetryTime: integer): string;
    // HTTP GET
    function HTTPPostAction(URL: string; PostReqData: TStringStream;
      MaxRetryTime: integer): string; // HTTP POST
    procedure PlaceOrder; // 下订单的过程
    procedure ShowError; // 错误提示

  protected
    { Protected declarations }
    procedure Execute; override;

  public
    { Public declarations }
    property ThreadID: integer read FThreadID write FThreadID;
    constructor Create(CreateSuspended: Boolean; ID: integer;
      Buyer: TBuyer); overload;
    function DecodeUTF8toAnsi; // UTF8-ANSI编码转换
    function ExtractHtmlTagValues; // 取得HTML标签对应值
    function GetAttributeByName; // 用已知的属性值取另外一个属性值

  end;

implementation

constructor TWebAccess.Create(CreateSuspended: Boolean; ID: integer;
  Buyer: TBuyer);
begin
  FBuyer := Buyer;
  FThreadID := ID;
  InitHTTP;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TWebAccess.Execute; // 考虑使用多线程，异步
begin
  try
    PlaceOrder;
  except
    on E: Exception do
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
  FIdHTTP.AllowCookies := True;
  FIdHTTP.HandleRedirects := True;
  // 完成HTTP REQ的设置
end;

procedure TWebAccess.InitCookies;
begin
  // 登陆取得Cookie，然后添加必要内容
  FCookie.AddServerCookie('sth=value', 'nike.com');
end;

procedure TWebAccess.PlaceOrder;
begin
  // 登陆
  // 获取目标页面
  // 获取目标状态
  // 加入购物车
end;

procedure TWebAccess.ShowError;
begin
  // Form1.Memo1.Lines.Add( '（' + IntToStr(FUser.ID) + '）' + FUser.UserName + '：' + TempMsg);
  Application.MessageBox(FTempInfo, 'ERROR', 0);
end;

function TWebAccess.HTTPGetAction(URL: string; MaxRetryTime: integer): string;
var
  ResponseData: TStringStream;
begin
  ResponseData := TStringStream.Create('');
  FOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FOpenSSL.SSLOptions.Method := sslvSSLv3;

  try
    try
      FIdHTTP.IOHandler := FOpenSSL;
      FIdHTTP.Get(URL, ResponseData);
      Result := ResponseData.DataString;
    except
      Dec(MaxRetryTime);
      if MaxRetryTime <= 0 then
      begin
        Result := '';
        Exit;
      end;
      Result := HTTPGetAction(URL, MaxRetryTime);
    end;
  finally
    FreeAndNil(ResponseData);
    FreeAndNil(FOpenSSL);
  end;
end;

function TWebAccess.HTTPPostAction(URL: string; PostReqData: TStringStream;
  MaxRetryTime: integer): string;
var
  ResponseData: TMemoryStream;
begin
  ResponseData := TMemoryStream.Create;
  FOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FOpenSSL.SSLOptions.Method := sslvSSLv3;

  try
    try
      if FIdHTTP = nil then
        Exit;
      FIdHTTP.IOHandler := FOpenSSL;
      FIdHTTP.Post(URL, PostReqData, ResponseData);
      SetLength(Result, ResponseData); // SetLength()设定数组长度
      ResponseData.Position := 0;

      Result := PChar(ResponseData.Memory); // 强制指定为字符型指针，并返回该地址内容
    except
      Dec(MaxRetryTime);
      if MaxRetryTime <= 0 then
      begin
        Result := '';
        Exit;
      end;
      Result := HTTPPostAction(URL, PostReqData, MaxRetryTime);
    end;
  finally
    FIdHTTP.Disconnect;
    FreeAndNil(ResponseData);
    FreeAndNil(FOpenSSL);
  end;
end;

end.

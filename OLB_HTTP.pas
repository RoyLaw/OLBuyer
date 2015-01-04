unit OLB_HTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdCookieManager;

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

implementation

end.

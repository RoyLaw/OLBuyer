unit OLB_HTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdCookieManager;

type
  TBuyer = record                    //���������˻�
    BuyerID : integer;               //�����˻�ID
    BuyerName : string;              //�����˻�����
    StoreAccount : string;           //�����˻������������˻��û���
    StorePassword : string;          //�����˻������������˻�����
  end;

  TStoreItem = record                //��Ʒ��Ϣ
    StoreItemID : string;            //��Ʒ����
    StoreItemProperty :string;       //��Ʒ����
  end;

implementation

end.

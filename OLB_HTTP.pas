unit OLB_HTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Stdctrls, StrUtils,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdBaseComponent, IdURI,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdCookieManager, Vcl.Forms,
  IdMultipartFormData;

type
  TBuyer = record // ���������˻�
    BuyerID: integer; // �����˻�ID
    BuyerName: string; // �����˻�����
    StoreAccount: string; // �����˻������������˻��û���
    StorePassword: string; // �����˻������������˻�����
  end;

  TStoreItem = record // ��Ʒ��Ϣ
    StoreItemID: string; // ��Ʒ����
    StoreItemProperty: string; // ��Ʒ����
  end;

  TWebAccess = class(TThread)
  private
    { Private declarations }
    FBuyer: TBuyer; // ÿ�������߳��а���һ�������û�
    FThreadID: integer; // �߳�ID
    FIdHTTP: TIdHTTP; // HTTPЭ��ջ
    FCookie: TIdCookieManager; // Cookie
    FOpenSSL: TIdSSLIOHandlerSocketOpenSSL; // SSL v3

    FTempInfo: string; // ������Ϣ

    procedure InitCookies; // ��ʼ��Cookie
    procedure InitHTTP; // ��ʼ��HTTP
    function HTTPGetAction(URL: string; MaxRetryTime: integer): string;
    // HTTP GET
    function HTTPPostAction(URL: string; PostReqData: TStringStream;
      MaxRetryTime: integer): string; // HTTP POST
    procedure ShowError; // ������ʾ
    function DeleteHtmlTag(HtmlSource: string): string; // ɾ��HTML��ǩ����ȡ��ҳ����
    function GetURLList(Data: String): TStringList;

  protected
    { Protected declarations }
    procedure Execute; override;

  public
    { Public declarations }
    property ThreadID: integer read FThreadID write FThreadID;
    constructor Create(CreateSuspended: Boolean; ID: integer;
      Buyer: TBuyer); overload;

    procedure PlaceOrder; // �¶����Ĺ���

    function DecodeUTF8toAnsi(const UString: UTF8String): WideString;
    // UTF8-ANSI����ת��
    function ExtractHtmlTagValues(const HtmlText: string;
      TagName, AttribName: string; var Values: TStringList): integer;
    // ȡ��HTML��ǩ��Ӧֵ
    function GetAttributeByName(const HtmlText: string;
      TagName, AttribName, KnownAttrName, KnowAttrValue: string): string;
    // ����֪������ֵȡ����һ������ֵ

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

procedure TWebAccess.Execute; // ����ʹ�ö��̣߳��첽
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
  // ���HTTP REQ������
end;

procedure TWebAccess.InitCookies;
var
  Domain: TIdURI;
begin
  // ��½ȡ��Cookie��Ȼ����ӱ�Ҫ����
  Domain := TIdURI.Create('nike.com');
  FCookie.AddServerCookie('sth=value', Domain);
end;

procedure TWebAccess.PlaceOrder;
begin
  // ��½
  // ��ȡĿ��ҳ��
  // ��ȡĿ��״̬
  // ���빺�ﳵ
end;

procedure TWebAccess.ShowError;
begin
  // Form1.Memo1.Lines.Add( '��' + IntToStr(FUser.ID) + '��' + FUser.UserName + '��' + TempMsg);
  Application.MessageBox(Pchar(FTempInfo), Pchar('ERROR'), 0);
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
      SetLength(Result, ResponseData.Size); // SetLength()�趨���鳤��
      ResponseData.Position := 0;

      Result := Pchar(ResponseData.Memory); // ǿ��ָ��Ϊ�ַ���ָ�룬�����ظõ�ַ����
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

function TWebAccess.DeleteHtmlTag(HtmlSource: string): string;
var
  i: integer;
  s: string;
begin
  s := HtmlSource;
  i := pos('<', s);
  while i > 0 do
  begin
    delete(s, i, pos('>', s) - i + 1);
    i := pos('<', s);
  end;
  Result := s;
end;

function TWebAccess.DecodeUTF8toAnsi(const UString: UTF8String): WideString;
var
  lenSrc, lenDst: integer;
begin
  lenSrc := Length(UString);
  if (lenSrc = 0) then
    Exit;
  lenDst := MultiByteToWideChar(CP_UTF8, 0, Pointer(UString), lenSrc, nil, 0);
  SetLength(Result, lenDst);
  MultiByteToWideChar(CP_UTF8, 0, Pointer(UString), lenSrc,
    Pointer(Result), lenDst);
end;

function TWebAccess.GetAttributeByName(const HtmlText: string;
  TagName, AttribName, KnownAttrName, KnowAttrValue: string): string;
  function FindFirstCharAfterSpace(const Line: string;
    StartPos: integer): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := StartPos to Length(Line) do
    begin
      if (Line[i] <> ' ') then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;

  function FindFirstSpaceAfterChars(const Line: string;
    StartPos: integer): integer;
  begin
    Result := PosEx(' ', Line, StartPos);
  end;

  function FindFirstSpaceBeforeChars(const Line: string;
    StartPos: integer): integer;
  var
    i: integer;
  begin
    Result := 1;
    for i := StartPos downto 1 do
    begin
      if (Line[i] = ' ') then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;

var
  InnerTag: string;
  LastPos, LastInnerPos: integer;
  SPos, LPos, RPos: integer;
  AttribValue: string;
  ClosingChar: char;
  TempAttribName: string;
  IsSearched: Boolean;
  SearchedValue: string;
begin
  Result := '';
  LastPos := 1;
  while (True) do
  begin
    // find outer tags '<' & '>'
    LPos := PosEx('<', HtmlText, LastPos);
    if (LPos <= 0) then
      break;
    RPos := PosEx('>', HtmlText, LPos + 1);
    if (RPos <= 0) then
      LastPos := LPos + 1
    else
      LastPos := RPos + 1;

    // get inner tag
    InnerTag := Copy(HtmlText, LPos + 1, RPos - LPos - 1);

    InnerTag := Trim(InnerTag); // remove spaces
    if (Length(InnerTag) < Length(TagName)) then
      continue;
    IsSearched := False;
    SearchedValue := '';

    // check tag name
    if (SameText(Copy(InnerTag, 1, Length(TagName)), TagName)) then
    begin
      // found tag
      AttribValue := '';
      LastInnerPos := Length(TagName) + 1;
      while (LastInnerPos < Length(InnerTag)) do
      begin
        // find first '=' after LastInnerPos
        RPos := PosEx('=', InnerTag, LastInnerPos);
        if (RPos <= 0) then
          break;

        // this way you can check for multiple attrib names and not a specific attrib
        SPos := FindFirstSpaceBeforeChars(InnerTag, RPos);
        TempAttribName := Trim(Copy(InnerTag, SPos, RPos - SPos));
        if (True) then
        begin
          // found correct tag
          LPos := FindFirstCharAfterSpace(InnerTag, RPos + 1);
          if (LPos <= 0) then
          begin
            LastInnerPos := RPos + 1;
            continue;
          end;
          LPos := FindFirstCharAfterSpace(InnerTag, LPos);
          // get to first char after '='
          if (LPos <= 0) then
            continue;
          if ((InnerTag[LPos] <> '"') and (InnerTag[LPos] <> '''')) then
          begin
            // AttribValue is not between '"' or ''' so get it
            RPos := FindFirstSpaceAfterChars(InnerTag, LPos + 1);
            if (RPos <= 0) then
              AttribValue := Copy(InnerTag, LPos, Length(InnerTag) - LPos + 1)
            else
              AttribValue := Copy(InnerTag, LPos, RPos - LPos + 1);
          end
          else
          begin
            // get url between '"' or '''
            ClosingChar := InnerTag[LPos];
            RPos := PosEx(ClosingChar, InnerTag, LPos + 1);
            if (RPos <= 0) then
              AttribValue := Copy(InnerTag, LPos + 1,
                Length(InnerTag) - LPos - 1)
            else
              AttribValue := Copy(InnerTag, LPos + 1, RPos - LPos - 1)
          end;

          if (SameText(TempAttribName, KnownAttrName)) and (AttribValue <> '')
          then
          begin
            if AttribValue = KnowAttrValue then
            begin
              IsSearched := True;
              if SearchedValue <> '' then
              begin
                Result := SearchedValue;
                break;
              end;
            end
            else
              continue;
          end;

          if (SameText(TempAttribName, AttribName)) and (AttribValue <> '') then
          begin
            if IsSearched then
            begin
              Result := AttribValue;
              break;
            end
            else
              SearchedValue := AttribValue;
          end;
        end;

        if (RPos <= 0) then
          LastInnerPos := Length(InnerTag)
        else
          LastInnerPos := RPos + 1;
      end;
    end;
  end;
end;

function TWebAccess.ExtractHtmlTagValues(const HtmlText: string;
  TagName, AttribName: string; var Values: TStringList): integer;
  function FindFirstCharAfterSpace(const Line: string;
    StartPos: integer): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := StartPos to Length(Line) do
    begin
      if (Line[i] <> ' ') then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;

  function FindFirstSpaceAfterChars(const Line: string;
    StartPos: integer): integer;
  begin
    Result := PosEx(' ', Line, StartPos);
  end;

  function FindFirstSpaceBeforeChars(const Line: string;
    StartPos: integer): integer;
  var
    i: integer;
  begin
    Result := 1;
    for i := StartPos downto 1 do
    begin
      if (Line[i] = ' ') then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;

var
  InnerTag: string;
  LastPos, LastInnerPos: integer;
  SPos, LPos, RPos: integer;
  AttribValue: string;
  ClosingChar: char;
  TempAttribName: string;
begin
  Result := 0;
  LastPos := 1;
  while (True) do
  begin
    // find outer tags '<' & '>'
    LPos := PosEx('<', HtmlText, LastPos);
    if (LPos <= 0) then
      break;
    RPos := PosEx('>', HtmlText, LPos + 1);
    if (RPos <= 0) then
      LastPos := LPos + 1
    else
      LastPos := RPos + 1;

    // get inner tag
    InnerTag := Copy(HtmlText, LPos + 1, RPos - LPos - 1);
    InnerTag := Trim(InnerTag); // remove spaces
    if (Length(InnerTag) < Length(TagName)) then
      continue;

    // check tag name
    if (SameText(Copy(InnerTag, 1, Length(TagName)), TagName)) then
    begin
      // found tag
      AttribValue := '';
      LastInnerPos := Length(TagName) + 1;
      while (LastInnerPos < Length(InnerTag)) do
      begin
        // find first '=' after LastInnerPos
        RPos := PosEx('=', InnerTag, LastInnerPos);
        if (RPos <= 0) then
          break;

        // this way you can check for multiple attrib names and not a specific attrib
        SPos := FindFirstSpaceBeforeChars(InnerTag, RPos);
        TempAttribName := Trim(Copy(InnerTag, SPos, RPos - SPos));
        if (True) then
        begin
          // found correct tag
          LPos := FindFirstCharAfterSpace(InnerTag, RPos + 1);
          if (LPos <= 0) then
          begin
            LastInnerPos := RPos + 1;
            continue;
          end;
          LPos := FindFirstCharAfterSpace(InnerTag, LPos);
          // get to first char after '='
          if (LPos <= 0) then
            continue;
          if ((InnerTag[LPos] <> '"') and (InnerTag[LPos] <> '''')) then
          begin
            // AttribValue is not between '"' or ''' so get it
            RPos := FindFirstSpaceAfterChars(InnerTag, LPos + 1);
            if (RPos <= 0) then
              AttribValue := Copy(InnerTag, LPos, Length(InnerTag) - LPos + 1)
            else
              AttribValue := Copy(InnerTag, LPos, RPos - LPos + 1);
          end
          else
          begin
            // get url between '"' or '''
            ClosingChar := InnerTag[LPos];
            RPos := PosEx(ClosingChar, InnerTag, LPos + 1);
            if (RPos <= 0) then
              AttribValue := Copy(InnerTag, LPos + 1,
                Length(InnerTag) - LPos - 1)
            else
              AttribValue := Copy(InnerTag, LPos + 1, RPos - LPos - 1)
          end;
          if (SameText(TempAttribName, AttribName)) and (AttribValue <> '') then
          begin
            Values.Add(AttribValue);
            inc(Result);
          end;
        end;

        if (RPos <= 0) then
          LastInnerPos := Length(InnerTag)
        else
          LastInnerPos := RPos + 1;
      end;
    end;
  end;
end;

// ��Ԫ�ص�ֵ
function GetValByName(s, Sub: string): string;
var
  EleS, EleE, iPos: integer;
  ELeStr, ValSt: string;
  St, Ct: integer;

  function FindEleRange(str: string; front: Boolean; posi: integer): integer;
  var
    i: integer;
  begin
    if front then
    begin
      for i := posi - 1 downto 1 do
        if str[i] = '<' then
        begin
          Result := i;
          break;
        end;
    end
    else
    begin
      for i := posi + 1 to Length(str) do
        if str[i] = '>' then
        begin
          Result := i;
          break;
        end;
    end;
  end;

  function FindEnd(str: string; posi: integer): integer;
  var
    i: integer;
  begin
    for i := posi to Length(str) do
    begin
      if (str[i] = '"') or (str[i] = '''') or (str[i] = ' ') then
      begin
        Result := i - 1;
        break;
      end;
    end;
  end;

begin
  iPos := pos('name="' + lowercase(Sub) + '"', lowercase(s));
  if iPos = 0 then
    iPos := pos('name=' + lowercase(Sub), lowercase(s));
  if iPos = 0 then
    iPos := pos('name=''' + lowercase(Sub) + '''', lowercase(s));
  if iPos = 0 then
    Exit;
  EleS := FindEleRange(s, True, iPos);
  EleE := FindEleRange(s, False, iPos);
  ELeStr := Copy(s, EleS, EleE - EleS + 1);
  ValSt := 'value="';
  iPos := pos(ValSt, ELeStr);
  if iPos = 0 then
  begin
    ValSt := 'value=''';
    iPos := pos(ValSt, ELeStr);
  end;
  if iPos = 0 then
  begin
    ValSt := 'value=';
    iPos := pos(ValSt, ELeStr);
  end;
  St := iPos + Length(ValSt);
  Ct := FindEnd(ELeStr, St) - St + 1;
  Result := Copy(ELeStr, St, Ct);
end;

// ���ȡ����ҳ�е��������ӣ��Դ������޸���Ҳ����ʵ�ֲ�������ͼƬ�ȵ�
function TWebAccess.GetURLList(Data: String): TStringList;
var
  i: integer;
  List: TStringList;
  tmp: String;
  function Split(Data, Node: String): TStringList;
  var
    Count, i, j: integer;
    function GetFieldCount(Data, Node: String): integer;
    var
      i: integer;
    begin
      Result := -1;
      i := pos(Node, Data);
      if i = 0 then
        Exit;
      Result := 0;
      while i <> 0 do
      begin
        inc(Result);
        delete(Data, 1, i + Length(Node) - 1);
        i := pos(Node, Data);
      end;
    end;

  begin
    Result := TStringList.Create;
    Count := GetFieldCount(Data, Node);
    for i := 0 to Count - 1 do
    begin
      j := pos(Node, Data);
      Result.Add(Copy(Data, 1, j - 1));
      delete(Data, 1, j + Length(Node) - 1);
    end;
    Result.Add(Data);
  end;

begin
  Result := TStringList.Create;
  try
    List := Split(Data, 'href=');
    for i := 1 to List.Count - 1 do
    begin
      tmp := List[i];
      tmp := Copy(tmp, 0, pos('</a>', tmp) - 1);
      tmp := Copy(tmp, 0, pos('>', tmp) - 1);
      if pos(' ', tmp) <> 0 then
        tmp := Copy(tmp, 0, pos(' ', tmp) - 1);
      tmp := StringReplace(tmp, char(34), '', [rfReplaceAll, rfIgnoreCase]);
      tmp := StringReplace(tmp, char(39), '', [rfReplaceAll, rfIgnoreCase]);
      // TVarCompareResult
      // error if not Compare(CI.Key, tmp) then Continue;
      if Copy(tmp, 1, 7) <> 'http://' then
      begin
        if Copy(tmp, 1, 1) = '.' then
          tmp := StringReplace(tmp, '.', '', []);
        if Copy(tmp, 1, 1) = '.' then
          tmp := StringReplace(tmp, '.', '', []);
        try
          tmp := 'http://' + FIdHTTP.URL.Host + ':' + FIdHTTP.URL.Port +
            FIdHTTP.URL.Path + tmp;
        except
        end;
      end;
      if Result.IndexOf(tmp) <> -1 then
        continue;
      Result.Add(tmp);
    end;
    FreeAndNil(List);
  except
  end;
end;

end.

unit aws_ses;

{$mode objfpc}{$H+}

interface

uses
  //rtl
  classes,
  sysutils,
  //synapse
  synautil,
  synacode,
  //aws
  aws_base,
  aws_client;

type

  ISESObjects = Interface;
  TSESMessage = class;

  ISESRegion = interface(IInterface)
  ['{B72DE2B7-600D-47E1-9CB1-8BE7CBFBB907}']
    function SESObjects: ISESObjects;
  end;

  ISESObject = interface(IInterface)
  ['{CE161E39-4FA9-46C5-B528-268506DC4740}']
    function Name: string;
    function Stream: IAWSStream;
  end;

  ISESObjects = interface(IInterface)
  ['{FF36521F-A15F-4C98-84DB-4669236F37A8}']
    function SendEmail(Message: TSESMessage): ISESObject;
  end;

  TSESMessage = class
  private
    FTOName: String;
    FTOAddress: String;
    FFrom: String;
    FSubject: String;
    FMessage: String;
  published
    property TOName: String read FTOName write FTOName;
    property TOAddress: String read FTOAddress write FTOAddress;
    property From: String read FFrom write FFrom;
    property Subject: String read FSubject write FSubject;
    property Message: String read FMessage write FMessage;
  end;


  { TSESRegion }

  TSESRegion = class sealed(TInterfacedObject, ISESRegion)
  private
    FClient: IAWSClient;
  public
    constructor Create(Client: IAWSClient; sRegion: String);
    function SESObjects: ISESObjects;
  end;

  { TSESObjects }

  TSESObjects = class sealed(TInterfacedObject, ISESObjects)
  private
    FClient: IAWSClient;
  public
    constructor Create(Client: IAWSClient);
    function SendEmail(Message: TSESMessage): ISESObject;
  end;

var
  sAWS_SES_URL: String;

implementation

{ TSESObjects }

constructor TSESObjects.Create(Client: IAWSClient);
begin
  inherited Create;
  FClient := Client;
end;

function TSESObjects.SendEmail(Message: TSESMessage): ISESObject;
const
  sAnd = '&';
var
  sConteudo: String;
  oStream: TStringStream;
  oAwsStream: TAWSStream;
begin

  sConteudo:='Action=SendEmail';
  sConteudo:=sConteudo+sAnd+'Destination.ToAddresses.member.1='+EncodeURLElement(Message.TOAddress);
  sConteudo:=sConteudo+sAnd+'Source='                          +EncodeURLElement(Message.From);
  sConteudo:=sConteudo+sAnd+'Message.Subject.Data='            +EncodeURLElement(Message.Subject);
  sConteudo:=sConteudo+sAnd+'Message.Body.Text.Data='          +EncodeURLElement(Message.Message);

  oStream := TStringStream.Create(sConteudo);
  oAwsStream := TAWSStream.Create(oStream);

  FClient.Send(
    TAWSRequest.Create(
      'POST', '', sAWS_SES_URL, '', '/', 'application/x-www-form-urlencoded', 'AWS3', '', '/', oAwsStream)
  );

end;

{ TSESRegion }

constructor TSESRegion.Create(Client: IAWSClient; sRegion: String);
begin
  inherited Create;
  FClient := Client;
  sAWS_SES_URL:='email.'+sRegion+'.amazonaws.com';
end;

function TSESRegion.SESObjects: ISESObjects;
begin
  Result := TSESObjects.Create(FClient);
end;

end.


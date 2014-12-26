unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, EditBtn,
  //aws
  aws_auth,
  aws_client,
  aws_s3;

type
  TfrmMain = class(TForm)
    edtAcessKeyId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btnTestAccess: TBitBtn;
    pnlServices: TPanel;
    Label3: TLabel;
    edtBucketName: TEdit;
    btnBucketCheck: TButton;
    btnBucketCreate: TButton;
    edtSecretKey: TEdit;
    btnBucketDelete: TButton;
    Label4: TLabel;
    fneFile: TFileNameEdit;
    btnFileUpload: TButton;
    edtContentType: TEdit;
    btnObjectDelete: TButton;
    edtResource: TEdit;
    Label5: TLabel;
    edtObject: TEdit;
    Label6: TLabel;
    Bevel1: TBevel;
    Label7: TLabel;
    mmoResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestAccessClick(Sender: TObject);
    procedure btnBucketCheckClick(Sender: TObject);
    procedure btnBucketCreateClick(Sender: TObject);
    procedure btnBucketDeleteClick(Sender: TObject);
    procedure btnFileUploadClick(Sender: TObject);
    procedure btnObjectDeleteClick(Sender: TObject);
  private
    FS3Client: TAWSS3Client;
    FRegion: IS3Region;
    procedure ShowLastError(const AMsg: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FS3Client := TAWSS3Client.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FS3Client.Free;
end;

procedure TfrmMain.btnTestAccessClick(Sender: TObject);
var
  Cred: IAWSCredentials;
  Client: IAWSClient;
begin
  Cred := TAWSCredentials.Create(edtAcessKeyId.Text, edtSecretKey.Text, True);
  Client := TAWSClient.Create(Cred);
  FRegion := TS3Region.Create(Client);
  if FRegion.IsOnline then
  begin
    pnlServices.Visible := True;
    ShowMessage('Ok!');
  end
  else
  begin
    pnlServices.Visible := False;
    ShowLastError('Access denied.');
  end;
end;

procedure TfrmMain.btnBucketCheckClick(Sender: TObject);
var
  Res: IS3Result;
begin
  Res := FRegion.Buckets.Check(edtBucketName.Text);
  if Res.Success then
    ShowMessage('The bucket exists and you have access!')
  else
    ShowMessage('Error: ' + IntToStr(Res.ResultCode));
end;

procedure TfrmMain.btnBucketCreateClick(Sender: TObject);
var
  Res: IS3Result;
begin
  Res := FRegion.Buckets.Put(edtBucketName.Text, edtResource.Text);
  if Res.Success then
    ShowMessage('Bucket was created!')
  else
    ShowMessage('Error: ' + IntToStr(Res.ResultCode));
end;

procedure TfrmMain.btnBucketDeleteClick(Sender: TObject);
var
  Res: IS3Result;
begin
  Res := FRegion.Buckets.Delete(edtBucketName.Text, edtResource.Text);
  if Res.Success then
    ShowMessage('Bucket was deleted!')
  else
    ShowMessage('Error: ' + IntToStr(Res.ResultCode));
end;

procedure TfrmMain.btnFileUploadClick(Sender: TObject);
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  FS3Client.PUTObject(edtBucketName.Text, edtContentType.Text, ExtractFileName(fneFile.FileName), fneFile.FileName);
  if FS3Client.HTTP.ResultCode = 200 then
    ShowMessage('The file was uploaded!')
  else
    ShowLastError('The file wasn''t uploaded.');
end;

procedure TfrmMain.btnObjectDeleteClick(Sender: TObject);
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  FS3Client.DELETEObject(edtBucketName.Text, ExtractFileName(fneFile.FileName));
  if FS3Client.HTTP.ResultCode = 204 then  // No Content response
    ShowMessage('The file was deleted!')
  else
    ShowLastError('The file wasn''t deleted.');
end;

procedure TfrmMain.ShowLastError(const AMsg: string);
var
  S: string;
begin
  with FS3Client.HTTP do
    S := AMsg + #13#13 + Format('ERROR: #%d %s', [ResultCode, ResultString]);
  ShowMessage(S);
end;

end.


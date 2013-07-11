unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, EditBtn, AWSS3;

type
  TfrmMain = class(TForm)
    edtAcessKeyId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
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
    btnFileDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnBucketCheckClick(Sender: TObject);
    procedure btnBucketCreateClick(Sender: TObject);
    procedure btnBucketDeleteClick(Sender: TObject);
    procedure btnFileUploadClick(Sender: TObject);
    procedure btnFileDeleteClick(Sender: TObject);
  private
    FS3Client: TAWSS3Client;
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

procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  FS3Client.AccessKeyId := edtAcessKeyId.Text;
  FS3Client.SecretKey := edtSecretKey.Text;

  FS3Client.GETService;

  with FS3Client.HTTP do
  begin
    if ResultCode = 200 then
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
end;

procedure TfrmMain.btnBucketCheckClick(Sender: TObject);
begin
  FS3Client.GETBucket(edtBucketName.Text, '/');
  if FS3Client.HTTP.ResultCode = 200 then
    ShowMessage('The bucket exists and you have access!')
  else
    ShowLastError('The bucket do not exists or you do not have access.');
end;

procedure TfrmMain.btnBucketCreateClick(Sender: TObject);
begin
  FS3Client.PUTBucket(edtBucketName.Text, '/');
  if FS3Client.HTTP.ResultCode = 200 then
    ShowMessage('The bucket was created!')
  else
    ShowLastError('The bucket wasn''t created.');
end;

procedure TfrmMain.btnBucketDeleteClick(Sender: TObject);
begin
  FS3Client.DELETEBucket(edtBucketName.Text, '/');
  if FS3Client.HTTP.ResultCode = 204 then  // No Content response
    ShowMessage('The bucket was deleted!')
  else
    ShowLastError('The bucket wasn''t deleted.');
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

procedure TfrmMain.btnFileDeleteClick(Sender: TObject);
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


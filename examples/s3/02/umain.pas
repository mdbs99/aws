unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, EditBtn,
  //aws
  aws_credentials,
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
    fneFile: TFileNameEdit;
    btnFileUpload: TButton;
    edtContentType: TEdit;
    btnObjectDelete: TButton;
    edtBucketSubResource: TEdit;
    Label5: TLabel;
    edtObjectName: TEdit;
    Label6: TLabel;
    Bevel1: TBevel;
    Label7: TLabel;
    edtObjectSubResource: TEdit;
    Label9: TLabel;
    btnBucketGet: TButton;
    btnObjectCreate: TButton;
    btnFileDownload: TButton;
    procedure btnTestAccessClick(Sender: TObject);
    procedure btnBucketCheckClick(Sender: TObject);
    procedure btnBucketCreateClick(Sender: TObject);
    procedure btnBucketDeleteClick(Sender: TObject);
    procedure btnFileUploadClick(Sender: TObject);
    procedure btnObjectDeleteClick(Sender: TObject);
    procedure fneFileChange(Sender: TObject);
    procedure btnBucketGetClick(Sender: TObject);
    procedure btnObjectCreateClick(Sender: TObject);
    procedure btnFileDownloadClick(Sender: TObject);
  private
    FRegion: IS3Region;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnTestAccessClick(Sender: TObject);
begin
  FRegion := TS3Region.Create(
    TAWSClient.Create(
      TAWSSignatureVersion1.New(
        TAWSCredentials.New(
          edtAcessKeyId.Text,
          edtSecretKey.Text, True
        )
      )
    )
  );
  if FRegion.Online then
  begin
    pnlServices.Visible := True;
  end
  else
  begin
    pnlServices.Visible := False;
    ShowMessage('Access denied.');
  end;
end;

procedure TfrmMain.btnBucketCheckClick(Sender: TObject);
begin
  if FRegion.Buckets.Check(edtBucketName.Text) then
    ShowMessage('The bucket exists and you have access!')
  else
    ShowMessage('Access denied.');
end;

procedure TfrmMain.btnBucketCreateClick(Sender: TObject);
begin
  FRegion.Buckets.Put(edtBucketName.Text, edtBucketSubResource.Text);
  ShowMessage('Success!')
end;

procedure TfrmMain.btnBucketDeleteClick(Sender: TObject);
begin
  FRegion.Buckets.Delete(edtBucketName.Text, edtBucketSubResource.Text);
  ShowMessage('Success!')
end;

procedure TfrmMain.btnFileUploadClick(Sender: TObject);
var
  Bkt: IS3Bucket;
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  if not LazFileUtils.FileExistsUTF8(fneFile.FileName) then
  begin
    ShowMessage('File not exists');
    fneFile.SetFocus;
    Exit;
  end;

  Bkt := FRegion.Buckets.Get(edtBucketName.Text, edtBucketSubResource.Text);
  Bkt.Objects.Put(edtObjectName.Text, edtContentType.Text, fneFile.FileName, edtObjectSubResource.Text);
  ShowMessage('Success!')
end;

procedure TfrmMain.btnFileDownloadClick(Sender: TObject);
var
  Bkt: IS3Bucket;
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  Bkt := FRegion.Buckets.Get(edtBucketName.Text, edtBucketSubResource.Text);
  Bkt.Objects.Get(
    edtObjectName.Text,
    edtObjectSubResource.Text
  ).Stream.SaveToFile(fneFile.FileName);
  ShowMessage('Success!')
end;

procedure TfrmMain.btnObjectCreateClick(Sender: TObject);
var
  Bkt: IS3Bucket;
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  Bkt := FRegion.Buckets.Get(edtBucketName.Text, edtBucketSubResource.Text);
  Bkt.Objects.Put(edtObjectName.Text, edtObjectSubResource.Text);
  ShowMessage('Success!')
end;

procedure TfrmMain.btnObjectDeleteClick(Sender: TObject);
var
  Bkt: IS3Bucket;
begin
  if edtBucketName.Text = '' then
  begin
    ShowMessage('Define a Bucket.');
    edtBucketName.SetFocus;
    Exit;
  end;

  Bkt := FRegion.Buckets.Get(edtBucketName.Text, edtBucketSubResource.Text);
  Bkt.Objects.Delete(edtObjectName.Text);
  ShowMessage('Success!');
end;

procedure TfrmMain.fneFileChange(Sender: TObject);
begin
  edtObjectName.Text:= ExtractFileName(fneFile.FileName);
end;

procedure TfrmMain.btnBucketGetClick(Sender: TObject);
begin
  FRegion.Buckets.Get(edtBucketName.Text, edtBucketSubResource.Text);
  ShowMessage('The bucket exists and you have access!')
end;

end.


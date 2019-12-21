program ProjetoProtoJson;

uses
  Vcl.Forms,
  ProtoJson in 'ProtoJson.pas' {Form1},
  Assis.Json in 'Assis.Json.pas';
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

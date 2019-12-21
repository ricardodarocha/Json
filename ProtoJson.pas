unit ProtoJson;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Assis.Json, Vcl.ComCtrls, Vcl.Grids,
  Vcl.ValEdit, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.StorageBin;

const
  EmptyJson = '{}';

type
  TEmpresa = class
  private
    FFantasia: string;
    FDono: String;
    procedure SetDono(const Value: String);
    procedure SetFantasia(const Value: string);
  published
    property Fantasia: string read FFantasia write SetFantasia;
    property Dono: String read FDono write SetDono;
  end;
  TPessoa = class
  public
    nome: String;
    idade: integer;
    constructor Create(aNOME: String; aIDADE: Integer = 0);
  end;
  TForm1 = class(TForm)
    btnPessoa: TButton;
    Memo1: TMemo;
    Botao1: TButton;
    Botao2: TButton;
    Botao3: TButton;
    BotaoA: TButton;
    BotaoB: TButton;
    BotaoC: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    btnInt: TButton;
    Edit1: TEdit;
    btnInt64: TButton;
    Edit2: TEdit;
    btnDouble: TButton;
    Edit3: TEdit;
    btnDate: TButton;
    btnTime: TButton;
    CheckBoxDateTimeKind: TCheckBox;
    CheckBox2: TCheckBox;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    btnParse: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ValueListEditor1: TValueListEditor;
    btnAtribuir: TButton;
    btnLimpar: TButton;
    btnEmpresa: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    btnAdicionar: TButton;
    Label7: TLabel;
    comboAtribuirCampo: TComboBox;
    edtNovoValor: TEdit;
    btnAtribuirNovo: TButton;
    comboObtemCampo: TComboBox;
    btnObterValor: TButton;
    lbValue: TLabel;
    TabPedidos: TFDMemTable;
    btnDataset: TButton;
    btnSubnivel: TButton;
    procedure Limpar(Sender: TObject);
    procedure ParserNumber(Sender: TObject);
    procedure BotaoAClick(Sender: TObject);
    procedure ParseBoolean(Sender: TObject);
    procedure ParseInteiro(Sender: TObject);
    procedure ParseNumerico(Sender: TObject);
    procedure ParseInteiroGrande(Sender: TObject);
    procedure ParseData(Sender: TObject);
    procedure ParseHora(Sender: TObject);
    procedure ParseAsJson(Sender: TObject);
    procedure Atribuir(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParseEmpresa(Sender: TObject);
    procedure Adicionar(Sender: TObject);
    procedure ObterValor(Sender: TObject);
    procedure btnAtribuirNovoClick(Sender: TObject);
    procedure btnDatasetClick(Sender: TObject);
    procedure comboAtribuirCampoSelect(Sender: TObject);
    procedure Subnivel(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  uses System.Json;

{$R *.dfm}

procedure TForm1.BotaoAClick(Sender: TObject);
  var Texto: Json;
begin
  Texto.Read(TButton(sender).Caption);
  Memo1.Lines.Add(Texto);

  Texto.Read(TButton(sender).Name, TButton(sender).Caption);
  Memo1.Lines.Add(Texto);
end;

procedure TForm1.btnAtribuirNovoClick(Sender: TObject);
var Temp: Json;
begin
  if ComboAtribuirCampo.ItemIndex = -1 then
    showmessage('Escolhe uma chave')else
  begin

    Temp := Memo1.Lines.Text;
    //  lbValue.Caption := Temp.Value; //<-- returns entire json
    Temp.Value[ComboAtribuirCampo.Text] := edtNovoValor.Text;
    memo1.Lines.Text := Temp;
    ParseasJson(btnParse);
  end;
end;

procedure TForm1.btnDatasetClick(Sender: TObject);
var Pedidos: Json;
begin
  Pedidos.Read(TabPedidos);
  Memo1.Lines.Text := Pedidos;
end;

procedure TForm1.Adicionar(Sender: TObject);
begin
  ValueListEditor1.Strings.Add('');
end;

procedure TForm1.ParseInteiro(Sender: TObject);

  var NumeroInteiro: Integer;
      NumeroJson: Json;
begin
  NumeroInteiro := StrToInt(Edit1.Text);
  NumeroJson.Read(NumeroInteiro);
  Memo1.Lines.Add(NumeroJson);
end;

procedure TForm1.ParseInteiroGrande(Sender: TObject);
var NumeroGrande: Int64;
    NumeroJson: Json;
begin
  NumeroGrande := StrToInt64(edit2.Text);
  NumeroJson.Read(NumeroGrande);
  Memo1.Lines.Add(NumeroJson)
end;

procedure TForm1.Atribuir(Sender: TObject);
var I: Integer;
    Exemplo: Json;

    Chave, Valor: string;
begin
  Exemplo := EmptyJson;
  for I := 1 to ValueListEditor1.strings.Count do
  begin
    Chave := ValueListEditor1.Keys[i];
    Valor := ValueListEditor1.Values[ValueListEditor1.Keys[i]];
    Exemplo.Add(Chave, Valor);
  end;

  memo1.Lines.Text := Exemplo;

end;

procedure TForm1.ObterValor(Sender: TObject);
var Temp: Json;
begin
  if comboObtemCampo.ItemIndex = -1 then
    showmessage('Escolhe uma chave') else
  begin

    Temp := Memo1.Lines.Text;
    //  lbValue.Caption := Temp.Value; //<-- returns entire json
    lbValue.Caption := Temp.Value[comboObtemCampo.Text];

  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  memo1.Top := 217;
  memo1.Height := 81;
end;

procedure TForm1.comboAtribuirCampoSelect(Sender: TObject);
var ValorAntigo: Json;
begin
  ValorAntigo := Memo1.Lines.Text;
  edtNovoValor.Text := ValorAntigo.Value[comboAtribuirCampo.Text];
end;

procedure TForm1.ParseEmpresa(Sender: TObject);
var
  Emp: Json;
begin

  //TObject
  var Empresa := TEmpresa.Create;
  Empresa.Fantasia := 'Holder';
  Empresa.Dono := 'Gabriel';

  //Json
  Emp.Read(TObject(Empresa)); //Convert Objeto em Json
  Memo1.Lines.Text := Emp;    //Mostra o JSON no Memo

  {todo: Crie novas propriedades para a classe TEmpresa e veja o que acontece}

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sleep(3000);
end;

procedure TForm1.ParseAsJson(Sender: TObject);
var Pessoa: Json;
  I : Integer;
  Key: String;
  JValue: TJsonValue;
begin
  Pessoa := Memo1.Lines.Text;
  ValueListEditor1.Strings.Clear;

  {$REGION 'Trata Json como TJson'}

  if Pessoa.asJsonObject.count > 0 then
    Label1.Caption :=  Pessoa.asJsonObject.Get(0).JsonString.Value  + ' = ' + Pessoa.asJsonObject.Get(0).JsonValue.Value;

  if Pessoa.asJsonObject.count > 1 then
    Label2.Caption :=  Pessoa.asJsonObject.Get(1).JsonString.Value  + ' = ' + Pessoa.asJsonObject.Get(1).JsonValue.Value;

  for I := 0 to Pessoa.asJsonObject.Count -1 do
  begin
    if Pessoa.asJsonObject.Get(i).JsonValue.Value <> '' then
      ValueListEditor1.Strings.AddPair(Pessoa.asJsonObject.Get(i).JsonString.Value, Pessoa.asJsonObject.Get(i).JsonValue.Value)
    else
      ValueListEditor1.Strings.AddPair(Pessoa.asJsonObject.Get(i).JsonString.Value, Pessoa.asJsonObject.Get(i).JsonValue.ToJSON)
  end;

 {$ENDREGION}

 {$REGION 'Preenche COMBOBOX para pegar chave pelo nome'}
 comboAtribuirCampo.Clear;
 comboObtemCampo.Clear;
  for Key in Pessoa.Keys do
  begin
    comboAtribuirCampo.Items.Add(Key);
    comboObtemCampo.Items.Add(Key);
  end;

 {$ENDREGION}


end;

procedure TForm1.ParseBoolean(Sender: TObject);
var J: Json;
  Condicao: Boolean;
begin
  Condicao := TButton(Sender).Tag > 0; /// Your Logic Test

  J.Read(  Condicao  );
  Memo1.Lines.Add(J);

  J.Read('Ready',  Condicao  );
  Memo1.Lines.Add(J);
end;

procedure TForm1.ParseData(Sender: TObject);
  var Data: TDatetime;
      DataJson: Json;
begin
  Data := DateTimePicker1.Date;
  if CheckBoxDateTimeKind.Checked then
    Data := Data + DateTimePicker2.Time;

  DataJson.ReadDate(Data);
  Memo1.Lines.Add(DataJson)
end;

procedure TForm1.ParseHora(Sender: TObject);
  var Hora: TTime;
      HoraJson: Json;
begin
  Hora := DateTimePicker2.Time;
  HoraJson.ReadTime(Hora);
  Memo1.Lines.Add(HoraJson)
end;

procedure TForm1.Limpar(Sender: TObject);
var
  ObjetoPessoa: TPessoa;
  JsonPessoa: Json;
begin
  ObjetoPessoa := TPessoa.Create('Ricardo', 30);

  JsonPessoa.Read(TObject(ObjetoPessoa)); //Converta um OBJETO em JSON com uma linha de código *sujeito a revisão

  ObjetoPessoa.Free;

  {$REGION 'LimparMemo'}
  memo1.Clear;
  ValueListEditor1.Tag := 0;
  ValueListEditor1.Strings.Clear;
  {$ENDREGION}

  Memo1.Lines.Add(JsonPessoa); {Todo: Debugue e veja que JsonPessoa é String}

  {------------------------------------------------------------------------
   *  Objetos complexos com vários níveis ainda precisam ser debugados
      Objetos com Array ainda não fazem parse corretamente
      Objetos com DinArray não possuem suporte RTTI}
end;

procedure TForm1.ParseNumerico(Sender: TObject);
  var NumeroDecimal: Double;
      NumeroJson: Json;

begin
  NumeroDecimal := strToFloat(Edit3.Text);
  NumeroJson.Read(NumeroDecimal);
  Memo1.Lines.Add(NumeroJson);
end;

procedure TForm1.ParserNumber(Sender: TObject);
  var Number: Json;
begin
  if tag = 1 then
  begin
    Number.Read(TButton(sender).tag);
    Memo1.Lines.Add(Number);
  end else
  begin
    Number.Read(TButton(sender).Name, TButton(sender).tag);
    Memo1.Lines.Add(Number);
  end;
end;


procedure TForm1.Subnivel(Sender: TObject);
var Sub: Json;
begin
  Sub := Memo1.Lines.Text;
  Sub.Value['Teste']:= '{"subnivel":"a"}';
  Memo1.Lines.Text := Sub;
end;

{ TPessoa }

constructor TPessoa.Create(aNOME: String; aIDADE: Integer);
begin
  Nome := aNOME;
  Idade := aIDADE;
end;
{ TEmpresa }

procedure TEmpresa.SetDono(const Value: String);
begin
  FDono := Value;
end;

procedure TEmpresa.SetFantasia(const Value: string);
begin
  FFantasia := Value;
end;

end.

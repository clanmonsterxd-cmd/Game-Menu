unit unit_captcha;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TFormCaptcha }
  TFormCaptcha = class(TForm)
    Hintergrund: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImageClick(Sender: TObject);
  private
    InstructionLabel: TLabel;
    CorrectSelections: Integer;
    procedure LoadCaptchaImages;
  public
    function ShowCaptcha: Boolean;
  end;

function LoadBitmapFromResource(const ResName: string): TBitmap;

implementation

function LoadBitmapFromResource(const ResName: string): TBitmap;
var
  ResStream: TResourceStream;
begin
  Result := TBitmap.Create;
  ResStream := TResourceStream.Create(HInstance, ResName, Windows.RT_RCDATA);
  try
    Result.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;

{$R *.lfm}
{$R captcha.res}

{ TFormCaptcha }

procedure TFormCaptcha.FormCreate(Sender: TObject);
begin
  ClientWidth := 640;
  ClientHeight := 710;  // etwas höher, da Font größer ist
  Color:= clBlack;

  Position := poScreenCenter;

  Hintergrund:= TImage.Create(Self);

  // Hinweis-Label erstellen
  InstructionLabel := TLabel.Create(Self);
  InstructionLabel.Parent := Self;
  InstructionLabel.Font.Color:= clWhite;
  InstructionLabel.WordWrap := True;
  InstructionLabel.Caption := 'Beweise, dass du kein Roboter bist!' + sLineBreak +
      'Klicke alle Straßenlaternen an.';
  InstructionLabel.Font.Size := 12;
  InstructionLabel.Font.Style := [fsBold];
  InstructionLabel.Left := 10;
  InstructionLabel.Top := 10;
  InstructionLabel.Width := ClientWidth - 20;
  InstructionLabel.Height := 44;
  InstructionLabel.Alignment := taCenter;
  InstructionLabel.Anchors := [akTop, akLeft, akRight];

  LoadCaptchaImages;
end;

function TFormCaptcha.ShowCaptcha: Boolean;
begin
  CorrectSelections := 0;
  Result := (ShowModal = mrOk);
end;

procedure TFormCaptcha.LoadCaptchaImages;
var
  Grid: array[0..8] of string;
  IsTrafficLight: array[0..8] of Boolean = (True, True, True, True, True, False, False, False, False);
  Indices: array[0..8] of Integer;
  i, j, tmp, posX, posY: Integer;
  img: TImage;
  marginX, marginY: Integer;
  startX, startY: Integer;
begin
  // Ressourcen-Namen vorbereiten
  for i := 0 to 8 do
    Grid[i] := 'CAPTCHA' + IntToStr(i + 1);

  // Initialisiere das Index-Array (0..8)
  for i := 0 to 8 do
    Indices[i] := i;

  Randomize;
  // Fisher-Yates Shuffle für zufällige Reihenfolge
  for i := High(Indices) downto 1 do
  begin
    j := Random(i + 1);
    tmp := Indices[i];
    Indices[i] := Indices[j];
    Indices[j] := tmp;
  end;

  marginX := 10;
  marginY := 10;
  startX := marginX;
  startY := 80;

  // Erstelle die Bilder in zufälliger Reihenfolge
  for i := 0 to 8 do
  begin
    img := TImage.Create(Self);
    img.Cursor:= crHandPoint;
    img.Width := 200;
    img.Height := 200;
    img.Stretch := True;
    img.Proportional := True;
    img.Center := True;

    // Tag setzen basierend auf dem Originalindex (nicht der zufälligen Position)
    img.Tag := Ord(IsTrafficLight[Indices[i]]);

    img.Picture.Bitmap := LoadBitmapFromResource(Grid[Indices[i]]);
    posX := startX + (i mod 3) * (img.Width + marginX);
    posY := startY + (i div 3) * (img.Height + marginY);

    img.Left := posX;
    img.Top := posY;
    img.OnClick := @ImageClick;
    img.Parent := Self;
  end;
end;

procedure TFormCaptcha.ImageClick(Sender: TObject);
var
  img: TImage;

  procedure ConvertToGrayScale(Bmp: TBitmap);
  var
    x, y: Integer;
    r, g, b, gray: Byte;
    p: PRGBQuad;
  begin
    Bmp.PixelFormat := pf32bit;
    for y := 0 to Bmp.Height - 1 do
    begin
      p := Bmp.ScanLine[y];
      for x := 0 to Bmp.Width - 1 do
      begin
        r := p^.rgbRed;
        g := p^.rgbGreen;
        b := p^.rgbBlue;
        // Luminanzformel (ungefähr)
        gray := Round(0.3 * r + 0.59 * g + 0.11 * b);
        p^.rgbRed := gray;
        p^.rgbGreen := gray;
        p^.rgbBlue := gray;
        Inc(p);
      end;
    end;
  end;

begin
  img := TImage(Sender);
  img.Enabled := False;
  ConvertToGrayScale(img.Picture.Bitmap);
  img.Repaint;  // Bild aktualisieren

  if img.Tag = 0 then
  begin
    Inc(CorrectSelections);
    if CorrectSelections = 4 then
    begin
      ShowMessage('Gut gemacht. Du bist (vielleicht) kein Roboter.');
      ModalResult := mrOk;
    end;
  end
  else
  begin
    ShowMessage('Du hast eine Straßenlaterne angeklickt. Test nicht bestanden!' + sLineBreak +
      '...oder doch bestanden?');
    ModalResult := mrCancel;
  end;
end;

end.

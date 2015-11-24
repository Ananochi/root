Sub MatrixMerge()

' dimming stuff
Dim StrFile As String
Dim inputBook As Workbook
Dim ws
Dim sagsCol As New Collection

Set SaveAsPath = Application.FileDialog(msoFileDialogFolderPicker)
SaveAsPath.AllowMultiSelect = False
SaveAsPath.Title = "Vælg den folder hvor matrix arkene ligger"
SaveAsPath.Show

' opsæt til at pege på det directory som indeholder inputfilerne!
StrFileString = SaveAsPath.SelectedItems(1)
StrFile = Dir(StrFileString & "/")
' For stien på resultatarket
myPath = Application.ActiveWorkbook.Name

Set ws = ActiveSheet

rowFirst = InputBox("Hvilken række findes den første ansøgning på?", "Start værdi")
rowLast = InputBox("Hvilken række findes den sidste ansøgning på?", "Slut værdi")

' Danner collection af "valide" sagsnumre --> selve opslagsmekanikken
' sagsCol.Item(nøgle) er måden at lave opslag på!
For j = rowFirst To rowLast
    aKode = ws.Range("A" & j).Value
    atekst = j
    sagsCol.Add Item:=atekst, Key:=aKode
Next j

inputRowStart = InputBox("Fra hvilken række findes der ansøgninger i inputarkene (matrix ark)?", "Vælg input start række")

' looper over hver enkelt fil - sålænge at mappen, StrFile, ikke er tom, do ...
Do While Len(StrFile) > 0
    
    If StrFile <> myPath Then
    ' Hvis input arket ikke er resultat arket --> Festen fortsætter!
        
        ' Danner stien på inputfilen og åbner denne!
        sheetPath = Application.ActiveWorkbook.Path & "\" & StrFile
        Set inputBook = Workbooks.Open(Filename:=sheetPath)
        
        ' looper over alle ansøgninger der findes i resultatarket
        ' skal her opsættes præcis hvor mange ansøgninger der findes i resultatarket
        toCount = 184 'inputBook.Worksheets("Sheet1").UsedRange.Rows.Count
        For i = inputRowStart To toCount
        
            ' ansøgning der skal slåes op på!
            ansId = inputBook.Worksheets("Sheet1").Range("A" & i).Value
            
            ' tjekker om ansøgnings ID findes i collection
            ' notetoself: har forsøgt error handling her ("on error goto ..."), men errors bliver ikke fanget
            ' Dog, hvis 'main' er korrekt opsat, vil alle ansID'er også findes i collection
            sagsCol.Item (ansId)
            
            rowNum = sagsCol.Item(ansId)
            
            ' Række P --> Q
            If (ws.Range("R" & rowNum).Value) <> "" Then
            
                ws.Range("R" & rowNum).Value = ws.Range("R" & rowNum).Value & vbCrLf & StrFile & ": " & inputBook.Worksheets("Sheet1").Range("R" & i).Value
                
                Else
                
                    If inputBook.Worksheets("Sheet1").Range("R" & i).Value = "" Then
                    
                    ws.Range("R" & rowNum).Value = inputBook.Worksheets("Sheet1").Range("R" & i).Value
                    
                    Else
                    
                    ws.Range("R" & rowNum).Value = StrFile & ": " & inputBook.Worksheets("Sheet1").Range("R" & i).Value
                    
                    End If
                
            End If
            
            ' Række Q --> R
            If (ws.Range("S" & rowNum).Value) <> "" Then
            
                ws.Range("S" & rowNum).Value = ws.Range("S" & rowNum).Value & vbCrLf & StrFile & ": " & inputBook.Worksheets("Sheet1").Range("S" & i).Value
                
                Else
                
                    If inputBook.Worksheets("Sheet1").Range("S" & i).Value = "" Then
                    
                    ws.Range("S" & rowNum).Value = inputBook.Worksheets("Sheet1").Range("S" & i).Value
                    
                    Else
                    
                    ws.Range("S" & rowNum).Value = StrFile & ": " & inputBook.Worksheets("Sheet1").Range("S" & i).Value
                    
                    End If
                
            End If
            
            ' Række T --> S
            If (ws.Range("T" & rowNum).Value) <> "" Then
            
                ws.Range("T" & rowNum).Value = ws.Range("T" & rowNum).Value & vbCrLf & StrFile & ": " & inputBook.Worksheets("Sheet1").Range("T" & i).Value
                
                Else
                
                    If inputBook.Worksheets("Sheet1").Range("T" & i).Value = "" Then
                    
                    ws.Range("T" & rowNum).Value = inputBook.Worksheets("Sheet1").Range("T" & i).Value
                    
                    Else
                    
                    ws.Range("T" & rowNum).Value = StrFile & ": " & inputBook.Worksheets("Sheet1").Range("T" & i).Value
                    
                    End If
                
            End If
            
            ' Række U --> T
            If (ws.Range("U" & rowNum).Value) <> "" Then
            
                ws.Range("U" & rowNum).Value = ws.Range("U" & rowNum).Value & vbCrLf & StrFile & ": " & inputBook.Worksheets("Sheet1").Range("U" & i).Value
                
                Else
                
                    If inputBook.Worksheets("Sheet1").Range("U" & i).Value = "" Then
                    
                    ws.Range("U" & rowNum).Value = inputBook.Worksheets("Sheet1").Range("U" & i).Value
                    
                    Else
                    
                    ws.Range("U" & rowNum).Value = StrFile & ": " & inputBook.Worksheets("Sheet1").Range("U" & i).Value
                    
                    End If
                
            End If
            
        Next i
        
    Else
    ' Vi forsøger at inputte fra resultat arket --> Videre!
    
    End If
    
    StrFile = Dir
    
Loop

inputBook.Close

End Sub




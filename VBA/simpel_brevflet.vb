Sub VurderingsFlet()

' Dimming/Setting of objects
Dim wordDok As Object
Dim wordObject As Object
Dim ws
Set ws = Ark1

Set wordObject = CreateObject("Word.Application")
wordObject.Visible = True ' can be set to false

' ----------------------------------------------------------
' TODO: INSERT TO/FROM ROWS
' ----------------------------------------------------------
For i = 2 To 195 

    ' ------------------------------------------------------
	' TODO: UPDATE WITH CORRECT COLUMNS AND NAMES
	' ------------------------------------------------------
    bookmark = ws.Range(Cells(i, 1), Cells(i, 1)).Text
    
    ' ------------------------------------------------------
	' TODO: INSERT FILEPATH TO WORD FILE
	' ------------------------------------------------------
    Set wordDok = wordObject.Documents.Open("C:\Users\b006572\Documents\Innovationsfonden\Birthe Schouby\Afslag_Tilskud\afslag_brev.docx")
        
        With wordDok
        
                ' ------------------------------------------
				' TODO: UPDATE WITH CORRESPONDING BOOKMARKS
				' ------------------------------------------
                .Bookmarks("bookmark").Range.Text = bookmark
                        
            ' ----------------------------------------------
			' TODO: UPDATE TO FILE DESTINATION + FILENAME
			' ----------------------------------------------
            If Dir("C:\Users\b006572\Documents\" & bookmark & " - NAME OF FILE.docx") <> "" Then
                    Kill "C:\Users\b006572\Documents\" & bookmark & " - NAME OF FILE.docx"
            End If
                .SaveAs ("C:\Users\b006572\Documents\" & bookmark & " - NAME OF FILE.docx")
                .Close               
        End With
        
Next i

' closing stuff ...
wordObject.Quit
Set wordDok = Nothing
Set wordObject = Nothing

End Sub








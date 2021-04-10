DECLARE SUB ItemAction (Action!)
DECLARE SUB MoveNorth ()
DECLARE SUB MoveSouth ()
DECLARE SUB MoveEast ()
DECLARE SUB MoveWest ()
DECLARE SUB UseItem ()
DECLARE SUB DropItem ()
DECLARE SUB GetItem ()
DECLARE FUNCTION GetCommand! ()
DECLARE SUB PrintRoom ()
Type Room
    name As String * 32
    description As String * 127
    north As Integer
    south As Integer
    west As Integer
    east As Integer
    item As Integer
End Type

Type ItemDefinition
    name As String * 32
    description As String * 127
    Action As Integer
    x As Integer
    y As Integer
    GetAction As Integer
    gx As Integer
    gy As Integer
End Type

Dim Shared CurrentRoom As Integer 'Variable for the current room
Dim Shared Rooms(100) As Room 'Array for defining rooms
Dim Shared Items(100) As ItemDefinition 'Array for definins items
Dim Shared Flag As Integer 'Flag for game conditional
Dim Shared health As Integer 'Variable for health
health = 20

On Timer(3) GoSub background

GoSub LoadRooms
Timer On

Cls

x:
If health < 1 Then
    Print "You died."
    End
End If
PrintRoom
x = GetCommand
Select Case x
    Case 1
        MoveNorth
    Case 2
        MoveSouth
    Case 3
        MoveWest
    Case 4
        MoveEast
    Case 5
        GetItem
    Case 6
        DropItem
    Case 7
        UseItem
    Case Else
        Print "Internal Error - Terminating program"
        End
End Select
GoTo x
End

LoadRooms:
For x = 1 To 29
    Read n$, d$, north, south, west, east, item
    Rooms(x).name = n$
    Rooms(x).description = d$
    Rooms(x).north = north
    Rooms(x).south = south
    Rooms(x).west = west
    Rooms(x).east = east
    Rooms(x).item = item
Next x
CurrentRoom = 1
For x = 1 To 7
    Read n$, d$, a, px, py, ga
    Items(x).name = n$
    Items(x).description = d$
    Items(x).Action = a
    Items(x).x = px
    Items(x).y = py
    Items(x).GetAction = ga
Next x
Return
End

Data "garden","It appears you are deep withen the garden",4,2,6,4,0
Data "garden","It appears you are deep withen the garden",2,2,1,1,0
Data "garden","It appears you are deep withen the garden",5,1,1,3,0
Data "garden","It appears you are deep withen the garden",6,2,4,4,0
Data "garden","There is an exit to the north",7,5,3,5,0
Data "garden","It appears you are deep withen the garden",3,4,1,4,6
Data "garden","There are exits to the north, south, and east",10,5,7,8,0
Data "garden","You can go west",8,8,7,8,0
Data "master bedroom (of Brutus)","There is an exit to the east",12,9,9,10,2
Data "atrium","You can leave from any direction",13,7,9,11,0
Data "bedroom (of Lucius)","There is only one exit from this small room to the west",11,11,10,11,0
Data "vault","You can't see much at all",12,12,12,13,5
Data "entry hall","You can go north, south, or east",16,10,13,14,0
Data "closet","You can't see much at all",14,14,13,14,8
Data "path","",24,15,15,16,4
Data "path","",16,13,15,17,4
Data "path","",25,17,16,17,7
Data "path","",26,20,18,19,0
Data "path","",19,21,18,19,0
Data "path","",18,22,20,23,0
Data "forum","You can go north.  You see Caesar.",19,21,21,21,0
Data "path","",20,22,22,22,0
Data "knife room","",20,23,23,23,1
Data "meeting place","You see your fellow conspirators waiting for you to show them something",24,15,24,24,3
Data "path","",25,17,15,26,4
Data "path","",29,18,25,27,0
Data "theater","You enjoy the play",27,27,26,27,0
Data "spring","",28,28,28,29,6
Data "forest","You can't see much at all",29,26,28,29,3



Data "Knife","Et tu Brut‚. Then fall Caesar. YOU WIN!!!!!",4,21,0,0
Data "Scroll","",5,24,0,0
Data "Scroll","",0,0,0,0
Data "Rock","",0,0,0,0
Data "Pouch of 75 drachmas","",0,0,0,0
Data "Food","",2,0,0,2
Data "Wood","",0,0,0,0
Data "Robe","",0,0,0,0


End

background:
health = health - 1
Return
End

FUNCTION GetInventory()
    GetInventory = Rooms(0).item
END FUNCTION

SUB SetInventory(newInventory)
   Rooms(0).item = newInventory
END SUB

Sub DropItem
    If GetInventory = 0 Then
        Print "You have nothing to set down."
        Exit Sub
    End If
    Rooms(CurrentRoom).item = GetInventory
    SetInventory(0)
End Sub

Function GetCommand
    Print
    Print "***** Command *****"
    Print "1 - Move North"
    Print "2 - Move South"
    Print "3 - Move West"
    Print "4 - Move East"
    Print "5 - Get Item"
    Print "6 - Set Item Down"
    Print "7 - Use Item"
    Print "0 - Quit"
    retry:
    Input "> ", x
    If x = 0 Then
        Print "Bye Bye!"
        End
    End If
    If x < 1 Or x > 7 Or x <> Int(x) Then
        Print "Retry - Invalid Value"
        GoTo retry
    End If
    GetCommand = x
End Function

Sub GetItem
    If Rooms(CurrentRoom).item = 0 Then
        Print "There is nothing here to pick up."
        Exit Sub
    End If
    SetInventory Rooms(CurrentRoom).item
    Rooms(CurrentRoom).item = 0
    Action = Items(GetInventory).GetAction
    ItemAction Action
End Sub

Sub ItemAction (Action)
    item = GetInventory
    x = Items(item).x
    y = Items(item).y
    If item = 0 Then
        Print "You have nothing to use."                          f
        Exit Sub
    End If
    Select Case Action
        Case 0
        Case 1
            If CurrentRoom = x Then
                CurrentRoom = y
            Else
                Print "I think you had better learn to use this before you kill some one!"
            End If
        Case 2
            health = health + 10
            SetInventory(0)
            Print "You feel better after eating the food."
        Case 3
            health = 0
            Print "You died."
            Sleep 2
            Print "Tough luck!"
            End
        Case 4
            If CurrentRoom = x And Flag Then
                Print Items(GetInventory).description
                End
            End If
        Case 5
            If CurrentRoom = x Then
                Flag = -1
            End If
        Case 6
            Print
            Print Items(item).description
            Print
        Case Else
            Print "Not even I know what this does!!!"
    End Select
    If health > 50 Then health = 50
End Sub

Sub MoveEast
    If CurrentRoom = Rooms(CurrentRoom).east Then
        Print "You cannot go east."
        Exit Sub
    End If
    CurrentRoom = Rooms(CurrentRoom).east
End Sub

Sub MoveNorth
    If CurrentRoom = Rooms(CurrentRoom).north Then
        Print "You cannot go north."
        Exit Sub
    End If
    CurrentRoom = Rooms(CurrentRoom).north
End Sub

Sub MoveSouth
    If CurrentRoom = Rooms(CurrentRoom).south Then
        Print "You cannot go south."
        Exit Sub
    End If
    CurrentRoom = Rooms(CurrentRoom).south
End Sub

Sub MoveWest
    If CurrentRoom = Rooms(CurrentRoom).west Then
        Print "You cannot go west."
        Exit Sub
    End If
    CurrentRoom = Rooms(CurrentRoom).west
End Sub

Sub pause
    a$ = ""
    While a$ = ""
        a$ = InKey$
    Wend
End Sub

Sub PrintRoom
    pname$ = LTrim$(RTrim$(Rooms(CurrentRoom).name))
    pdes$ = LTrim$(RTrim$(Rooms(CurrentRoom).description))
    
    Print "You are in the "; pname$
    If pdes$ <> "" Then
        Print pdes$
        Print
    End If
    If Rooms(CurrentRoom).item <> 0 Then
        Print
        pitem$ = LTrim$(RTrim$(Items(Rooms(CurrentRoom).item).name))
        Print "There is a "; pitem$; " here"
    End If
    If GetInventory <> 0 Then
        Print
        pitem$ = LTrim$(RTrim$(Items(GetInventory).name))
        Print "You are holding a "; pitem$; "."
    End If
    Print
    Print "        "; Chr$(218); String$(50, 196); Chr$(191)
    Print "Health  "; Chr$(179); String$(health, 219); String$(50 - health, 32); Chr$(179)
    Print "        "; Chr$(192); String$(50, 196); Chr$(217)
End Sub

Sub UseItem
    Action = Items(GetInventory).Action
    ItemAction Action
End Sub


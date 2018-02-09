Public Class MiniMax
    Dim Team As String
    Dim Map(2, 2) As String
    Dim Reward As Integer
    Dim Layer As Integer = 0
    Dim XWin As Boolean
    Dim OWin As Boolean
    Dim bestStateX As Integer
    Dim BestStateY As Integer
    Dim BestScore
    Dim Scores As New List(Of Integer(,))
    Dim StateScores(2, 2) As Integer
    Dim MoveX As Integer = 0
    Dim MoveY As Integer = 0
    Dim EndOFTurn As Boolean = False
    Sub New(T)
        Team = T
    End Sub
    Sub MakeMove()
        Map = Module1.State.Clone
        Layer = 0
        FindMax()
        Scores.Clear()

    End Sub

    Sub FindMax()
        Reward = 0
        For x = 0 To 2
            For y = 0 To 2
                StateScores(x, y) = 0
            Next
        Next
        Scores.Add(StateScores.Clone)
        For x = 0 To 2
            If Reward = 10 Then
                Exit For
            End If
            For y = 0 To 2
                If Layer = 0 And x = 2 And y = 2 Then
                    Console.WriteLine()
                End If

                Scores(Layer)(x, y) = 0
                If Map(x, y) = "." Then
                    Map(x, y) = "1"
                    Win()
                    If XWin Then
                        Reward = 10
                    End If
                    If Reward = 10 Then
                        Scores(Layer)(x, y) += Reward
                    Else
                        EndOFTurn = False
                        Layer += 1
                        FindMin()
                        Layer -= 1
                        If Not EndOFTurn Then


                            Scores(Layer)(x, y) += Scores(Layer + 1)(MoveX, MoveY)
                            Scores.RemoveAt(Layer + 1)
                            Map(MoveX, MoveY) = "."
                        End If
                    End If
                    Map(x, y) = "."

                Else
                    Scores(Layer)(x, y) = -100
                End If
                If Reward = 10 Then
                    Exit For
                End If
            Next
        Next


        For x = 0 To 2
            For y = 0 To 2
                If Scores(Layer)(x, y) >= Scores(Layer)(MoveX, MoveY) Then
                    MoveX = x
                    MoveY = y
                End If
            Next
        Next

        If Scores(Layer)(MoveX, MoveY) = -100 Then
            EndOFTurn = True
        End If
        If Not EndOFTurn Then
            Map(MoveX, MoveY) = "1"
        End If

        If Layer = 0 Then
            Module1.State(MoveX, MoveY) = "1"
        End If
    End Sub
    Sub FindMin()
        For x = 0 To 2
            For y = 0 To 2
                StateScores(x, y) = 0
            Next
        Next
        Scores.Add(StateScores.Clone)
        Reward = 0
        For x = 0 To 2
            If Reward = -10 Then
                Exit For
            End If
            For y = 0 To 2
                If Map(x, y) = "." Then
                    Map(x, y) = "-1"
                    Win()
                    If OWin Then
                        Reward = -10

                    End If
                    If Reward = -10 Then
                        Scores(Layer)(x, y) += Reward
                    Else
                        EndOFTurn = False
                        Layer += 1
                        FindMax()
                        Layer -= 1
                        If Not EndOFTurn Then
                            Scores(Layer)(x, y) += Scores(Layer + 1)(MoveX, MoveY)
                            Map(MoveX, MoveY) = "."
                            Scores.RemoveAt(Layer + 1)
                        End If
                    End If

                    Map(x, y) = "."
                Else
                    Scores(Layer)(x, y) = 100
                End If
                If Reward = -10 Then
                    Exit For
                End If
            Next
        Next

        For x = 0 To 2
            For y = 0 To 2
                If Scores(Layer)(x, y) <= Scores(Layer)(MoveX, MoveY) Then
                    MoveX = x
                    MoveY = y
                End If
            Next
        Next
        If Scores(Layer)(MoveX, MoveY) = -100 Then
            EndOFTurn = True
        End If
        If Not EndOFTurn Then

            Map(MoveX, MoveY) = "-1"
        End If
    End Sub

    Sub Win()
        XWin = False
        OWin = False
        For y = 0 To 2

            If Map(0, y) <> "." Then

                If Map(0, y) = Map(1, y) And Map(1, y) = Map(2, y) Then

                    If Map(0, y) = "-1" Then
                        OWin = True
                    Else
                        XWin = True
                    End If

                End If

            End If

        Next


        For x = 0 To 2

            If Map(x, 0) <> "." Then

                If Map(x, 0) = Map(x, 1) And Map(x, 1) = Map(x, 2) Then

                    If Map(x, 0) = "-1" Then
                        OWin = True
                    Else
                        XWin = True
                    End If

                End If

            End If

        Next

        If Map(0, 0) <> "." Then
            If Map(0, 0) = Map(1, 1) And Map(1, 1) = Map(2, 2) Then
                If Map(0, 0) = "-1" Then
                    OWin = True
                Else
                    XWin = True
                End If
            End If
        End If

        If Map(2, 0) <> "." Then
            If Map(2, 0) = Map(1, 1) And Map(1, 1) = Map(0, 2) Then
                If Map(2, 0) = "-1" Then
                    OWin = True
                Else
                    XWin = True
                End If
            End If
        End If
    End Sub
End Class
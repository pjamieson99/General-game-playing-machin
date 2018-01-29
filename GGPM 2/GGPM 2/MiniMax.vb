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
    Sub New(T)
        Team = T
    End Sub
    Sub MakeMove()

    End Sub

    Sub FindMax()
        Dim StateScores(2, 2) As Integer
        Reward = 0
        For x = 0 To 2
            If Reward <> 0 Then
                Exit For
            End If
            For y = 0 To 2
                If Map(x, y) = "." Then
                    Map(x, y) = "1"
                    Win()
                    If XWin Then
                        Reward = 10
                    End If
                    If Reward = 10 Then
                        StateScores(x, y) += Reward
                    Else
                        Layer += 1
                        FindMin()
                        Layer -= 1
                        StateScores(x, y) += Reward
                    End If
                End If
                If Reward <> 0 Then
                    Exit For
                End If
            Next
        Next
        If Layer = 0 Then
            For x = 0 To 2
                For y = 0 To 2

                Next
            Next
        End If
    End Sub
    Sub FindMin()
        Dim StateScores(2, 2) As Integer
        Reward = 0
        For x = 0 To 2
            If Reward <> 0 Then
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
                        StateScores(x, y) += Reward
                    Else
                        Layer += 1
                        FindMax()
                        Layer -= 1
                    End If
                    If Reward <> 0 Then
                        Exit For
                    End If
                End If
            Next
        Next

    End Sub

    Sub Win()
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

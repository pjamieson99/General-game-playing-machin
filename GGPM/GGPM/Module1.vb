Module Module1
    Public Iteration As Integer = 1

    Public State(2, 2) As String

    Public Win0 As Boolean = False
    Public WInX As Boolean = False
    Public DoBothTeams As Boolean
    Dim Net As NeuralNet
    Dim TrainingNet As NeuralNet
    Sub Main()
        Net = New NeuralNet("1")
        TrainingNet = New NeuralNet("0.5")

        SetMap()

        Do

            Net.PlayGame()
            TrainingNet.PlayGame()
            Iteration += 1


        Loop Until Iteration = 600000

        Dim FileWriter As IO.StreamWriter
        FileWriter = New IO.StreamWriter("errors1.csv")
        For X = 0 To Net.NetworkErrors1.Count - 1
            FileWriter.WriteLine(X & "," & Net.NetworkErrors1(X))
        Next
        FileWriter.Close()
        FileWriter = New IO.StreamWriter("errors2.csv")
        For X = 0 To TrainingNet.NetworkErrors2.Count - 1
            FileWriter.WriteLine(X & "," & TrainingNet.NetworkErrors2(X))
        Next
        FileWriter.Close()


        FileWriter = New IO.StreamWriter("errors3.csv")
        For X = 0 To TrainingNet.NetworkErrors2.Count - 1
            FileWriter.WriteLine(X & "," & TrainingNet.OutError(X))
        Next
        FileWriter.Close()


        FileWriter = New IO.StreamWriter("errors4.csv")
        For X = 0 To Net.OutError.Count - 1
            FileWriter.WriteLine(X & "," & Net.OutError(X))
        Next
        FileWriter.Close()
        Test()
    End Sub

    Sub SetMap()
        For x = 0 To 2
            For y = 0 To 2
                State(x, y) = "."
            Next
        Next
    End Sub

    Sub CheckWin()
        For y = 0 To 2

            If State(0, y) <> "." Then

                If State(0, y) = State(1, y) And State(1, y) = State(2, y) Then

                    If State(0, y) = "0.5" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next


        For x = 0 To 2

            If State(x, 0) <> "." Then

                If State(x, 0) = State(x, 1) And State(x, 1) = State(x, 2) Then

                    If State(x, 0) = "0.5" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next

        If State(0, 0) <> "." Then
            If State(0, 0) = State(1, 1) And State(1, 1) = State(2, 2) Then
                If State(0, 0) = "0.5" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If

        If State(2, 0) <> "." Then
            If State(2, 0) = State(1, 1) And State(1, 1) = State(0, 2) Then
                If State(2, 0) = "0.5" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If
    End Sub


    Sub Test()
        'For y = 0 To 2
        '    For x = 0 To 2
        '        State(x, y) = "."
        '    Next
        'Next


        'For x = 0 To 1
        '    State(x, 0) = "0.5"
        'Next

        'State(0, 1) = "1"
        'State(2, 2) = "1"
        'Exploration = 0
        'FeedForward()


        For x = 0 To 2
            For l = 0 To 2
                State(l, x) = "."
            Next
        Next

        Net.Exploration = 0
        For x = 0 To 6
            Net.FeedForward()


            Dim n As Integer
            Dim Y As Integer
            n = Console.ReadLine

            Y = Console.ReadLine
            State(n, Y) = "0.5"
        Next
    End Sub
End Module
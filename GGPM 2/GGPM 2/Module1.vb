Module Module1

    Public Iteration As Integer = 1 'this is the number of iterations the program has currently ran
    Public State(2, 2) As String 'this is a map of the board
    Public NumOfIterations As Integer = 100000

    Public Net As NeuralNet 'these are the two neural networks that will train against each other
    Public TrainingNet As NeuralNet

    Dim LetNetworkLearn As Boolean = False 'this variables sets whether the training neural network just sets its values to a previous version of the main network, or learns and trains alongside the main network.

    Sub Main()

        Net = New NeuralNet("1") 'creates the neural networks
        TrainingNet = New NeuralNet("-1")

        If Not LetNetworkLearn Then
            TrainingNet.Threshold = 0.9 'this is an optional variable as to whether the neural network being trained against has a certain percentage of choosing a random move. this is only applicable if the network isnt being trained.
        End If

        SetMap() 'sets the initial board state to an empty one

        Do
            Net.PlayGame() 'train the neural network by playing a game and training of the game
            If LetNetworkLearn Then 'decides whether the network will be trained or will copy the values of the main network
                TrainingNet.PlayGame()
            Else
                CopyNetworkValues() 'copies the main network's values to be used for the training network to improve as the main network does, constantly providing a challenge to the main network
            End If

            Iteration += 1 'increases the number of iterations the networks have been trained
        Loop Until Iteration = NumOfIterations 'loops until the max nuymber of iterations is reached

        Test() ' tests the program
    End Sub

    Sub CopyNetworkValues() 'copies the values of the main network onto the training network
        TrainingNet.IHWeights = Net.IHWeights.Clone
        TrainingNet.HOWeights = Net.HOWeights.Clone
        TrainingNet.InputBias = Net.InputBias.Clone
        TrainingNet.HiddenBias = Net.HiddenBias.Clone
    End Sub

    Sub SetMap() ' create an empty board
        For x = 0 To 2
            For y = 0 To 2
                State(x, y) = "."
            Next
        Next
    End Sub



    Sub Test()
        Dim NotTraining As Double = False
        For y = 0 To 2
            For x = 0 To 2
                State(x, y) = "."
            Next
        Next
        Dim n As Integer
        Dim m As Integer
        Dim EndProgram As Char
        Dim Pos1 As String
        Dim Pos2 As String
        Dim Pos3 As String

        Do
            For y = 0 To 2
                If State(0, y) = "." Then
                    Pos1 = " "
                ElseIf State(0, y) = "1" Then
                    Pos1 = "X"
                Else
                    Pos1 = "O"
                End If

                If State(0, y) = "." Then
                    Pos2 = " "
                ElseIf State(0, y) = "1" Then
                    Pos2 = "X"
                Else
                    Pos2 = "O"
                End If

                If State(0, y) = "." Then
                    Pos3 = " "
                ElseIf State(0, y) = "1" Then
                    Pos3 = "X"
                Else
                    Pos3 = "O"
                End If

                Console.WriteLine(" " & Pos1 & " | " & Pos2 & " | " & Pos3)
                If y <> 2 Then
                    Console.WriteLine("-----------")
                End If
            Next
            Net.Exploration = 0
            Net.FeedForward(State.Clone, False)
            n = Console.ReadLine()
            m = Console.ReadLine()
            State(n, m) = "-1"
            Console.WriteLine("Do you want to end the program (Y/N): ")
            EndProgram = Console.ReadLine
        Loop Until EndProgram = "Y"
    End Sub

End Module
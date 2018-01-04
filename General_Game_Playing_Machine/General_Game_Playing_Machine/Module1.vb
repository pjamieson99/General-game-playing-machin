Module Module1
    Dim NoOfInputs As Integer = 2
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 1
    Dim OutputNeuron As Double

    Dim NoOfHiddens As Integer = 2
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

    Dim HOWeights(NoOfHiddens - 1) As Double

    Dim Rnd As New Random

    Dim OutputError As Double

    Dim Outputs As List(Of List(Of Double))

    Public Iteration As Integer = 1

    Dim NoOfIterations As Integer = 100

    Dim ImmediateQValue(NoOfInputs - 1, NoOfIterations - 1) As Double
    Dim QValue As Double

    Public State(2, 2) As String

    Dim Win0 As Boolean = False
    Dim WInX As Boolean = False
    Dim Team As Char = "."

    Dim Threshold As Double = 0.8
    Dim Action As Integer

    Dim Exploration As Double
    Dim CheckAction(2, 2) As String
    Dim QList(8) As Double

    Dim LargestQ As Integer

    Dim Reward As Double

    Dim Batches As New List(Of Batch)
    Dim Batchx As Batch

    Dim OldInputNeurons(NoOfInputs - 1) As Double

    Dim ReplayLimit As Integer = 10

    Dim Gamma As Double = 0.4

    Dim LearningRate As Double = 0.01


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

                    If State(0, y) = "-1" Then
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

                    If State(x, 0) = "-1" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next

        If State(0, 0) <> "." Then
            If State(0, 0) = State(1, 1) And State(1, 1) = State(2, 2) Then
                If State(0, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If

        If State(2, 0) <> "." Then
            If State(2, 0) = State(1, 1) And State(1, 1) = State(0, 2) Then
                If State(2, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If
    End Sub





    Sub SetInputs()
        If Iteration = 1 Then
            For x = 0 To NoOfInputs - 1
                OldInputNeurons(x) = 0.1
            Next
        Else
            OldInputNeurons = InputNeurons
        End If

        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) <> "." And CheckAction(x, y) <> "/" Then
                    InputNeurons(x + y * 3) = Convert.ToInt32(CheckAction(x, y))
                Else
                    InputNeurons(x + y * 3) = 0.1
                End If

            Next
        Next
    End Sub

     Sub SetIHWeights()


        For x = 0 To NoOfHiddens * NoOfInputs - 1
            IHWeights(x) = Rnd.Next(1, 101) / 100
        Next
    End Sub

    Sub SetHOWeights()

        For y = 0 To NoOfHiddens - 1

            HOWeights(y) = Rnd.Next(1, 101) / 100

        Next
    End Sub

    Sub SetHiddenNeurons(input() As Double)
        For y = 0 To NoOfInputs - 1
            For x = 0 To NoOfHiddens - 1
                HiddenNeurons(x) += IHWeights(x + y * NoOfHiddens) * input(y)
            Next
        Next

        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = Tanh(HiddenNeurons(x))
        Next
    End Sub




    Sub SetOutputNeurons(hidden() As Double)
        For y = 0 To NoOfHiddens - 1

            OutputNeuron += HOWeights(y) * hidden(y)

        Next


        OutputNeuron = Tanh(OutputNeuron)

        QList(Action) = OutputNeuron
    End Sub


    Function Tanh(t)
        Return Math.Tanh(t)
    End Function



    Sub FeedForward()
        CheckAction = State


        If Exploration > Threshold Then
            Action = Rnd.Next(0, 9)
            Dim n As Integer = Math.Floor(Action / 3)
            Dim m As Integer = Action - n * 3
            State(m, n) = "1"

        Else


            For y = 0 To 2

                For x = 0 To 2
                    If State(x, y) = "." Then
                        Action = x + y * 3
                        CheckAction(x, y) = "1"
                        State(x, y) = "/"


                        SetInputs()
                        SetHiddenNeurons(InputNeurons)
                        SetOutputNeurons(HiddenNeurons)
                        CheckAction = State
                    Else
                        QList(x + y * 3) = Nothing
                    End If
                Next
            Next

            For x = 0 To 8
                If QList(x) <> 0 Then
                    LargestQ = x
                    Exit For
                End If
            Next

            For x = 0 To 8
                If QList(x) > QList(LargestQ) And QList(x) <> 0 Then
                    LargestQ = x
                End If
            Next

            For y = 0 To 2
                For x = 0 To 2
                    If State(x, y) = "/" Then
                        State(x, y) = "."
                    End If
                Next
            Next


            Dim p As Integer = Math.Floor(LargestQ / 3)
            Dim q As Integer = LargestQ - p * 3

            State(q, p) = "1"

        End If


        CheckWin()
    End Sub


    Sub TrainNet()
        Dim TrainingInputs(NoOfInputs - 1) As Double
        TrainingInputs = Batchx.OldInputs

        Dim TargetOutput As Double
        TargetOutput = Batchx.ChangeInReward

        SetHiddenNeurons(TrainingInputs)
        SetOutputNeurons(HiddenNeurons)

        Dim TargetError As Double

        TargetError = 0.5 * (TargetOutput - OutputNeuron) ^ 2



        Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            DeltaHOWeights(x) = -(TargetOutput - OutputNeuron) * (1 - Math.Tanh(OutputNeuron)) * HiddenNeurons(x)
        Next





        Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1)

        Dim DeltaNetOutput As Double


        DeltaNetOutput = -(TargetOutput - OutputNeuron) * (1 - Math.Tanh(OutputNeuron))

        Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double

        For x = 0 To NoOfHiddens - 1
            DeltaHiddenError(x) = HOWeights(x) * DeltaNetOutput
        Next


        For y = 0 To NoOfInputs - 1
            For x = 0 To NoOfHiddens - 1
                DeltaIHWeights(x + (y * NoOfHiddens)) = DeltaHiddenError(x) * (1 - Math.Tanh(HiddenNeurons(x))) * InputNeurons(y)
            Next
        Next


        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            HOWeights(x) -= LearningRate * DeltaHOWeights(x)
        Next

        For x = 0 To NoOfInputs * NoOfHiddens - 1
            IHWeights(x) -= LearningRate * DeltaIHWeights(x)
        Next
    End Sub


    Sub Main()
        SetMap()
        SetIHWeights()
        SetHOWeights()
        'Dim Train As TrainingNetwork
        'Train = New TrainingNetwork

        'Do
        '    CheckWin()
        '    If WInX Then
        '        Reward = 1
        '    ElseIf Win0 Then
        '        Reward = -1
        '    Else
        '        Reward = -0.1
        '    End If

        '    If WInX Or Win0 Then
        '        For y = 0 To 2
        '            For x = 0 To 2
        '                State(x, y) = "."

        '            Next
        '        Next

        '        Batches.Clear()

        '    End If

        '    Dim CheckFullBoard As Boolean = True

        '    For y = 0 To 2
        '        For x = 0 To 2
        '            If State(x, y) = "." Then
        '                CheckFullBoard = False
        '            End If
        '        Next
        '    Next
        '    If CheckFullBoard Then
        '        Win0 = True
        '        WInX = True
        '    End If

        '    If Iteration > 1 Then

        '        If Iteration > 2 Then
        '            For Each batch In Batches
        '                batch.ChangeInReward += Reward * Gamma ^ (Iteration - 1)
        '            Next
        '        End If

        '        Batchx = New Batch(OldInputNeurons, InputNeurons, Reward)
        '        Batches.Add(Batchx)
        '        If Batches.Count >= ReplayLimit Or Win0 Or WInX Then
        '            Batchx = Batches(Rnd.Next(0, Batches.Count))
        '            TrainNet()
        '        End If

        '    End If

        '    Win0 = False
        '    WInX = False

        '    Exploration = Rnd.Next(0, 101) / 100
        '    'Exploration *= 0.99 ^ Iteration
        '    FeedForward()

        '    Train.Main()



        '    Iteration += 1
        'Loop Until Iteration = 50000
        'Test()



        XORProblem()

        TestProblem()
    End Sub

    Sub TestProblem()
        InputNeurons(0) = 1
        InputNeurons(1) = -1

        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)

        Console.WriteLine(OutputNeuron)


        InputNeurons(0) = -1
        InputNeurons(1) = -1

        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)

        Console.WriteLine(OutputNeuron)



        InputNeurons(0) = 1
        InputNeurons(1) = 1

        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)

        Console.WriteLine(OutputNeuron)



        InputNeurons(0) = -1
        InputNeurons(1) = 1

        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)

        Console.WriteLine(OutputNeuron)
        Console.ReadLine()
    End Sub

    Sub XORProblem()
        Dim Count As Integer = 1
        Dim Target As Double
        LearningRate = 0.5
        Do
            For x = 0 To NoOfInputs - 1
                InputNeurons(x) = Rnd.Next(0, 2)
                If InputNeurons(x) = 0 Then
                    InputNeurons(x) = -1
                End If
            Next

            If InputNeurons(0) = 1 And InputNeurons(1) = -1 Then
                Target = 1
            End If

            If InputNeurons(0) = 1 And InputNeurons(1) = 1 Then
                Target = -1
            End If
            If InputNeurons(0) = -1 And InputNeurons(1) = -1 Then
                Target = -1
            End If
            If InputNeurons(0) = -1 And InputNeurons(1) = 1 Then
                Target = 1
            End If

            SetHiddenNeurons(InputNeurons)
            SetOutputNeurons(HiddenNeurons)

            Dim TargetError As Double

            TargetError = 0.5 * (Target - OutputNeuron) ^ 2



            Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

            For x = 0 To NoOfHiddens * NoOfOutputs - 1
                DeltaHOWeights(x) = (-(Target - OutputNeuron)) * (1 - Math.Tanh(OutputNeuron) ^ 2) * HiddenNeurons(x)
            Next





            Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1) As Double



            Dim DeltaNetOutput As Double


            DeltaNetOutput = -(Target - OutputNeuron) * (1 - Math.Tanh(OutputNeuron) ^ 2)

            Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double

            For x = 0 To NoOfHiddens - 1
                DeltaHiddenError(x) = HOWeights(x) * DeltaNetOutput
            Next





            For y = 0 To NoOfInputs - 1
                For x = 0 To NoOfHiddens - 1

                    DeltaIHWeights(x + (y * NoOfHiddens)) = DeltaHiddenError(x) * (1 - Math.Tanh(HiddenNeurons(x)) ^ 2) * InputNeurons(y)
                Next
            Next


            For x = 0 To NoOfHiddens * NoOfOutputs - 1
                HOWeights(x) -= LearningRate * DeltaHOWeights(x)
            Next

            For x = 0 To NoOfInputs * NoOfHiddens - 1
                IHWeights(x) -= LearningRate * DeltaIHWeights(x)
            Next
            Count += 1
        Loop Until Count = 10000
    End Sub
    Sub Test()
        For y = 0 To 2
            For x = 0 To 2
                State(x, y) = "."
            Next
        Next


        For x = 0 To 1
            State(x, 0) = "1"
        Next

        State(0, 1) = "-1"
        State(2, 2) = "-1"
        Exploration = 0
        FeedForward()

    End Sub







End Module

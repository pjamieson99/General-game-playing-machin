


Public Class TrainingNetwork
    Dim NoOfInputs As Integer = 9
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 2
    Dim OutputNeuron As Double

    Dim NoOfHiddens As Integer = 2
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

    Dim HOWeights(NoOfHiddens - 1) As Double

    Dim Rnd As New Random

    Dim OutputError As Double

    Dim Outputs As List(Of List(Of Double))

    Dim Iteration As Integer = 1

    Dim NoOfIterations As Integer = 100

    Dim ImmediateQValue(NoOfInputs - 1, NoOfIterations - 1) As Double
    Dim QValue As Double

    Dim Reward As Double
    Dim Batches As New List(Of Batch)
    Dim BatchX As Batch
    Dim OldInputNeurons(NoOfInputs - 1) As Double

    Dim Win0 As Boolean = False
    Dim WInX As Boolean = False
    Dim Team As Char = "."

    Dim Threshold As Double = 0.8
    Dim Action As Integer

    Dim Exploration As Double
    Dim CheckAction(2, 2) As String
    Dim QList(8) As Double

    Dim LargestQ As Integer

    Dim ReplayLimit As Integer = 10

    Dim Gamma As Double = 0.8

    Dim LearningRate As Double = 0.01

    Sub New()

        SetIHWeights()
        SetHOWeights()

    End Sub





    Sub CheckWin()
        For y = 0 To 2

            If Module1.State(0, y) <> "." Then

                If Module1.State(0, y) = Module1.State(1, y) And Module1.State(1, y) = Module1.State(2, y) Then

                    If Module1.State(0, y) = "-1" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next


        For x = 0 To 2

            If Module1.State(x, 0) <> "." Then

                If Module1.State(x, 0) = Module1.State(x, 1) And Module1.State(x, 1) = Module1.State(x, 2) Then

                    If Module1.State(x, 0) = "-1" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next

        If Module1.State(0, 0) <> "." Then
            If Module1.State(0, 0) = Module1.State(1, 1) And Module1.State(1, 1) = Module1.State(2, 2) Then
                If Module1.State(0, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If

        If Module1.State(2, 0) <> "." Then
            If Module1.State(2, 0) = Module1.State(1, 1) And Module1.State(1, 1) = Module1.State(0, 2) Then
                If Module1.State(2, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If
    End Sub





    Sub SetInputs()


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



    Sub SetOutputNeurons(hidden)
        For y = 0 To NoOfHiddens - 1

            OutputNeuron += HOWeights(y) * hidden(y)

        Next


        OutputNeuron = Sigmoid(OutputNeuron)

        QList(Action) = OutputNeuron
    End Sub


    Function Sigmoid(t)
        Return (1 / (1 + Math.E ^ -t))
    End Function



    Sub FeedForward()
        CheckAction = Module1.State
        Exploration = Rnd.Next(0, 101) / 100
        '   Exploration *= 0.99 ^ Module1.Iteration
        If Exploration > Threshold Then
            Action = Rnd.Next(0, 9)
            Dim n As Integer = Math.Floor(Action / 3)
            Dim m As Integer = Action - n * 3
            Module1.State(m, n) = "-1"

        Else


            For y = 0 To 2

                For x = 0 To 2
                    CheckAction = Module1.State
                    If Module1.State(x, y) = "." Then

                        Action = x + y * 3
                        CheckAction(x, y) = "-1"
                        Module1.State(x, y) = "/"


                        SetInputs()
                        SetHiddenNeurons(InputNeurons)
                        SetOutputNeurons(HiddenNeurons)
                        CheckAction = Module1.State
                    Else
                        QList(x + y * 3) = Nothing
                    End If
                Next
            Next

            For x = 0 To 8
                If QList(x) <> Nothing Then
                    LargestQ = x
                End If
            Next

            For x = 0 To 8
                If QList(x) > QList(LargestQ) And QList(x) <> 0 Then
                    LargestQ = x
                End If
            Next


            For y = 0 To 2
                For x = 0 To 2
                    If Module1.State(x, y) = "/" Then
                        Module1.State(x, y) = "."
                    End If
                Next
            Next


            Dim p As Integer = Math.Floor(LargestQ / 3)
            Dim q As Integer = LargestQ - p * 3

            Module1.State(q, p) = "-1"

        End If

        CheckWin()
    End Sub



    Sub Main()





        FeedForward()



        CheckWin()
        If WInX Then
            Reward = -1
        ElseIf Win0 Then
            Reward = 1
        Else
            Reward = -0.1
        End If

        If WInX Or Win0 Then
            Batches.Clear()

        End If
        If Iteration > 1 Then

                If Iteration > 2 Then
                    For Each batch In Batches
                        batch.ChangeInReward += Reward * Gamma ^ (Iteration - 1)
                    Next
                End If

                Batchx = New Batch(OldInputNeurons, InputNeurons, Reward)
                Batches.Add(Batchx)
            If Batches.Count >= ReplayLimit Or Win0 Or WInX Then
                BatchX = Batches(Rnd.Next(0, Batches.Count))
                TrainNet()
            End If

        End If
        WInX = False
        Win0 = False
    End Sub

    Sub TrainNet()
        Dim TrainingInputs(NoOfInputs - 1) As Double
        TrainingInputs = BatchX.OldInputs

        Dim TargetOutput As Double
        TargetOutput = BatchX.ChangeInReward

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
End Class

﻿Public Class NeuralNet

    Dim SkipTurn As Boolean = False

    Public NetworkErrors1 As New List(Of Double)
    Public NetworkErrors2 As New List(Of Double)
    Public IterationError1 As New List(Of Double)
    Public IterationError2 As New List(Of Double)

    Public OutError As New List(Of Double)

    Dim NoOfInputs As Integer = 9
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 9
    Dim OutputNeuron(NoOfOutputs - 1) As Double

    Dim NoOfHiddens As Integer = 10
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

    Dim HOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

    Dim NumofBatches As Integer

    Dim MaxQ As Double

    Dim ChooseBatch As Integer

    Dim Counter As Integer = 1

    Dim OutputError As Double

    Dim Rnd As New Random

    Dim p As Integer
    Dim q As Integer

    Dim QValue As Double

    Dim Threshold As Double = 0.8
    Dim Action As Integer

    Public Exploration As Double
    Dim CheckAction(2, 2) As String
    Dim QList(8) As Double

    Dim LargestQ As Integer

    Dim Reward As Double

    Dim NewInputNeurons(8) As Double
    Dim CopyBatches As New List(Of Batch)
    Dim Batches As New List(Of Batch)
    Dim Batchx As Batch
    Dim ReplayBatches As New List(Of List(Of Batch))

    Dim OldInputNeurons(8) As Double

    Dim ReplayLimit As Integer = 10

    Dim Gamma As Double = 0.9

    Dim LearningRate As Double = 0.00001

    Dim InputBias(NoOfHiddens - 1) As Double

    Dim HiddenBias(NoOfOutputs - 1) As Double
    Dim Team As String


    Dim n As Integer
    Dim m As Integer

    Dim TrainingInputs(NoOfInputs - 1) As Double
    Dim Target(NoOfOutputs - 1) As Double
    Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaHiddenBias(NoOfOutputs - 1) As Double
    Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1) As Double
    Dim DeltaNetOutput(NoOfOutputs - 1) As Double
    Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaInputError(NoOfHiddens - 1) As Double
    Dim DeltaInputBias(NoOfHiddens - 1) As Double

    Dim InGame As Boolean


    Dim RewardCounter As Integer


    Dim SampleBatches As Integer
    Dim SampleBatch As Integer

    Dim FindingMaxQ As Boolean

    Dim CheckFullBoard As Boolean

    Dim TrainingState(2, 2) As String
    Dim TrainingAction As Integer

    Dim MiniBatch As New List(Of Batch)

    Dim RemoveBatch As Batch

    Dim AddBatch As Batch

    Dim Point1 As Integer
    Dim Point2 As Integer

    Dim NextMaxQ As Double
    Sub New(T)
        Team = T
        SetIHWeights()
        SetHOWeights()
    End Sub

    Sub SetInputs(Board(,) As String)

        For y = 0 To 2
            For x = 0 To 2
                If Board(x, y) <> "." And Board(x, y) <> "/" Then
                    InputNeurons(x + y * 3) = Convert.ToDouble(Board(x, y))
                Else
                    InputNeurons(x + y * 3) = 0
                End If

            Next
        Next
    End Sub



    'set the ihweights
    Sub SetIHWeights()


        For x = 0 To NoOfHiddens * NoOfInputs - 1
            IHWeights(x) = Rnd.Next(-100, 101) / 100
        Next

        For x = 0 To NoOfHiddens - 1
            InputBias(x) = Rnd.Next(-100, 101) / 100

        Next

    End Sub

    'set howweights
    Sub SetHOWeights()
        For x = 0 To NoOfOutputs - 1
            For y = 0 To NoOfHiddens - 1

                HOWeights(y + x * NoOfHiddens) = Rnd.Next(-10, 11) / 100

            Next
        Next
        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) = Rnd.Next(-10, 11) / 100
        Next
    End Sub

    Sub SetHiddenNeurons(input() As Double)

        'rest hidden neurons
        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = 0
        Next

        'multiply ihweights by the input connected and add it to the hidden neuron
        For y = 0 To NoOfInputs - 1
            For x = 0 To NoOfHiddens - 1
                HiddenNeurons(x) += IHWeights(x + y * NoOfHiddens) * input(y)
            Next
        Next


        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) += InputBias(x)
        Next

        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = Tanh(HiddenNeurons(x))
        Next
    End Sub

    Sub SetOutputNeurons(hidden() As Double)
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) = 0
        Next
        For x = 0 To NoOfHiddens - 1
            For y = 0 To NoOfOutputs - 1

                OutputNeuron(y) += HOWeights(y + x * NoOfOutputs) * hidden(x)

            Next
        Next
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) += HiddenBias(x)
        Next
        For x = 0 To NoOfOutputs - 1
            OutputNeuron(x) = Tanh(OutputNeuron(x))
        Next
        For x = 0 To NoOfOutputs - 1
            QList(x) = OutputNeuron(x)
        Next
    End Sub

    Function Tanh(t)
        Return Math.Tanh(t)
    End Function

    Function derivative(t)
        Return (1 - Math.Tanh(t) ^ 2)
    End Function

    Sub XORfunction()
        For x = 0 To 1
            InputNeurons(x) = Rnd.Next(0, 2)
            If InputNeurons(x) = 0 Then
                InputNeurons(x) = -1
            End If
        Next

        If InputNeurons(0) = 1 And InputNeurons(1) = 1 Then
            Reward = -1
        End If
        If InputNeurons(0) = -1 And InputNeurons(1) = 1 Then
            Reward = 1
        End If
        If InputNeurons(0) = 1 And InputNeurons(1) = -1 Then
            Reward = 1
        End If
        If InputNeurons(0) = -1 And InputNeurons(1) = -1 Then
            Reward = -1
        End If

        TrainNet()
    End Sub
    Sub Test()
        InputNeurons(0) = -1
        InputNeurons(1) = -1
        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)
        Console.WriteLine(InputNeurons(0))
        Console.WriteLine(InputNeurons(1))
        Console.WriteLine(OutputNeuron(0))


        InputNeurons(0) = -1
        InputNeurons(1) = 1
        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)
        Console.WriteLine(InputNeurons(0))
        Console.WriteLine(InputNeurons(1))
        Console.WriteLine(OutputNeuron(0))

        InputNeurons(0) = 1
        InputNeurons(1) = -1
        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)
        Console.WriteLine(InputNeurons(0))
        Console.WriteLine(InputNeurons(1))
        Console.WriteLine(OutputNeuron(0))

        InputNeurons(0) = 1
        InputNeurons(1) = 1
        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)
        Console.WriteLine(InputNeurons(0))
        Console.WriteLine(InputNeurons(1))
        Console.WriteLine(OutputNeuron(0))

        Console.ReadLine()
    End Sub
    Sub TestSin()
        For x = 0 To 5
            Dim Value As Double = Rnd.Next(0, 361)
            InputNeurons(0) = Value / 360
            SetHiddenNeurons(InputNeurons)
            SetOutputNeurons(HiddenNeurons)

            Console.WriteLine(Value)
            Console.WriteLine(OutputNeuron)
            Console.WriteLine()
        Next
    End Sub
    Sub SinProblem()
        Dim Value As Double = Rnd.Next(0, 361)
        InputNeurons(0) = Value / 360
        Reward = Math.Sin((Value * Math.PI) / 180)
        TrainNet()
    End Sub

    Sub TrainNet(Optional TrainingBatch As Batch = Nothing)
        Dim FindMaxQ As Boolean = True

        If TrainingBatch Is Nothing Then
            'CheckAction = TrainingState.Clone()
            'FeedForward(FindMaxQ)

            WInX = False
            Win0 = False
            CheckWin()

            If Reward <> 0 Or WInX Or Win0 Or IsBoardFull() Then
                MaxQ = 0
            End If

            For x = 0 To NoOfOutputs - 1
                If x <> TrainingAction Then
                    Target(x) = QList(x)
                Else
                    Target(x) = Reward + (Gamma * MaxQ)
                End If
            Next

            FeedForward(TrainingState.Clone, True, True)

            'do i need to feedforward training data or just the new state, check that actioni s not being updated anywhere, 

        Else
            TrainingState = TrainingBatch.State.Clone
            Dim Training As Boolean = True
            Exploration = 0

            FeedForward(TrainingBatch.NewState.Clone, True, True)
            NextMaxQ = MaxQ

            If TrainingBatch.Reward <> 0 Then
                NextMaxQ = 0
            Else
                WInX = False
                Win0 = False
                CheckWin()
                If WInX Or Win0 Or IsBoardFull() Then

                    NextMaxQ = 0
                Else
                End If
            End If


            FeedForward(TrainingState.Clone, True, True)

            For x = 0 To NoOfOutputs - 1
                If x <> TrainingBatch.Action Then
                    Target(x) = QList(x)
                Else
                    TrainingState = TrainingBatch.NewState.Clone
                    Exploration = 0
                    If TrainingBatch.Reward <> 0 Then
                        OutputNeuron(x) = 0
                    Else
                        WInX = False
                        Win0 = False
                        CheckWin()
                        If WInX Or Win0 Or IsBoardFull() Then
                            OutputNeuron(x) = 0
                            NextMaxQ = 0


                        End If
                    End If




                End If
            Next

            Target(TrainingBatch.Action) = TrainingBatch.Reward + (Gamma * NextMaxQ)

            TrainingAction = TrainingBatch.Action
        End If
        'backprop


        For y = 0 To NoOfHiddens
            For x = 0 To NoOfOutputs - 1
                If y = NoOfHiddens Then
                    DeltaHiddenBias(x) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * 1
                Else
                    DeltaHOWeights(x + y * NoOfOutputs) = -(Target(x) - OutputNeuron(x)) * derivative(OutputNeuron(x)) * HiddenNeurons(y)

                End If
            Next
        Next



        'If Iteration = 7000 Then

        '    Console.WriteLine()
        'End If
        If Team = "1" Then
            '     NetworkErrors1.Add((OutputNeuron(TrainingAction) - Target(TrainingAction)))
            NetworkErrors1.Add(Target(TrainingAction))

            IterationError1.Add(Module1.Iteration)
            OutError.Add(OutputNeuron(TrainingAction))
        Else
            '    NetworkErrors2.Add((OutputNeuron(TrainingAction) - Target(TrainingAction)))
            IterationError1.Add(Module1.Iteration)
            OutError.Add(OutputNeuron(TrainingAction))
            NetworkErrors2.Add(Target(TrainingAction))

        End If

        For x = 0 To NoOfOutputs - 1
            DeltaNetOutput(x) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x))
        Next


        For x = 0 To NoOfHiddens - 1
            DeltaHiddenError(x) = 0
        Next


        For y = 0 To NoOfHiddens - 1
            For x = 0 To NoOfOutputs - 1
                DeltaHiddenError(y) += HOWeights(x + y * NoOfOutputs) * DeltaNetOutput(x)
            Next
        Next

        For y = 0 To NoOfHiddens - 1
            For x = 0 To NoOfOutputs - 1
                DeltaHOWeights(x + y * NoOfOutputs) = (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * HiddenNeurons(y)
            Next
        Next




        For x = 0 To NoOfHiddens - 1
            DeltaInputBias(x) = 0
        Next

        For x = 0 To NoOfHiddens - 1
            DeltaInputError(x) = 0
        Next


        For y = 0 To NoOfInputs
            For x = 0 To NoOfHiddens - 1
                If y = NoOfInputs Then
                    DeltaInputBias(x) += InputBias(x) * DeltaHiddenError(x) * derivative(HiddenNeurons(x))
                End If
                DeltaInputError(x + y * NoOfHiddens) += IHWeights(x + y * NoOfHiddens) * DeltaHiddenError(x) * derivative(HiddenNeurons(x))
            Next
        Next







        'For x = 0 To NoOfHiddens - 1
        '    DeltaInputBias(x) = InputBias( * derivative(HiddenNeurons(x)) * 1
        'Next





        'For y = 0 To NoOfInputs - 1
        '    For x = 0 To NoOfHiddens - 1
        '        DeltaIHWeights(x + (y * NoOfHiddens)) = DeltaHiddenError(x) * derivative(HiddenNeurons(x)) * InputNeurons(y)
        '    Next
        'Next

        'update weights
        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            HOWeights(x) -= LearningRate * DeltaHOWeights(x)
        Next

        For x = 0 To NoOfHiddens - 1
            InputBias(x) -= LearningRate * DeltaInputBias(x)
        Next

        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) -= LearningRate * DeltaHiddenBias(x)

        Next

        For x = 0 To NoOfInputs * NoOfHiddens - 1
            IHWeights(x) -= LearningRate * DeltaIHWeights(x)
        Next


    End Sub

    Sub FindReward()
        CheckAction = Module1.State.Clone
        Module1.WInX = False
        Module1.Win0 = False
        Module1.CheckWin()

        'find reward


        If Module1.WInX And Team = "1" Then
            Reward = 0.9
        ElseIf Module1.Win0 And Team = "1" Then
            Reward = -0.9


        ElseIf Module1.WInX And Team = "-1" Then
            Reward = -0.9
        ElseIf Module1.Win0 And Team = "-1" Then
            Reward = 0.9
        Else
                Reward = 0
            End If

    End Sub

    Sub PlayGame()
        InGame = True

        While InGame

            CheckAction = Module1.State.Clone
            Reward = 0


            Exploration = Rnd.Next(0, 101) / 100
            TrainingState = Module1.State.Clone
            FeedForward(CheckAction, False)

            TrainingAction = Action
            Win0 = False
            WInX = False
            CheckWin()

            CheckAction = Module1.State.Clone
            If IsBoardFull() Or WInX Or Win0 Then
                InGame = False
            End If

            If Team = "1" And InGame Then
                Exploration = Rnd.Next(0, 101) / 100
                Module1.TrainingNet.FeedForward(CheckAction, False)
            ElseIf InGame Then
                Exploration = Rnd.Next(0, 101) / 100
                Module1.Net.FeedForward(CheckAction, False)
            End If

            Win0 = False
            WInX = False
            CheckWin()
            CheckAction = Module1.State.Clone
            If IsBoardFull() Or WInX Or Win0 Then
                InGame = False
            End If

            FindReward()
            Exploration = 0
            If InGame Then
                FeedForward(CheckAction, True)
            Else
                MaxQ = 0
            End If

            Target(TrainingAction) = Reward + (Gamma * MaxQ)
            TrainNet()

            Module1.WInX = False
            Module1.Win0 = False
            Module1.CheckWin()

            CheckAction = Module1.State.Clone
            If Module1.WInX = True Or Module1.Win0 = True Or IsBoardFull() = True Then
                For y = 0 To 2
                    For x = 0 To 2
                        Module1.State(x, y) = "."
                    Next
                Next
                InGame = False
            End If
            AddBatch = New Batch(TrainingState.Clone, TrainingAction, Module1.State.Clone, Reward)
            MiniBatch.Add(AddBatch)

            Counter = 0
            While MiniBatch.Count > ReplayLimit
                RemoveBatch = MiniBatch(Counter)
                MiniBatch.Remove(RemoveBatch)
                Counter += 1
            End While

            If MiniBatch.Count = ReplayLimit Then
                ChooseBatch = Rnd.Next(0, MiniBatch.Count)
                TrainNet(MiniBatch(ChooseBatch))
            End If
        End While


    End Sub

    Sub SetOldInputs()
        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) = "." Then
                    OldInputNeurons(x + y * 3) = 0
                Else
                    OldInputNeurons(x + y * 3) = Convert.ToDouble(CheckAction(x, y))

                End If
            Next
        Next
    End Sub
    Sub FeedForward(Board(,) As String, DontMove As Boolean, Optional Training As Boolean = False)

        Dim m As Integer = Rnd.Next(0, 3)
        Dim N As Integer = Rnd.Next(0, 3)

        If Not Training Then
            Board = Module1.State.Clone
        End If

        '     SetOldInputs()
        If Exploration > Threshold Then
            Do
                m = Rnd.Next(0, 3)
                N = Rnd.Next(0, 3)
            Loop Until CheckAction(m, N) = "."

            Board(m, N) = Team
            Action = N * 3 + m

        Else
            SetInputs(Board)
            SetHiddenNeurons(InputNeurons)
            SetOutputNeurons(HiddenNeurons)

            For y = 0 To 2
                For x = 0 To 2
                    If Board(x, y) <> "." Then
                        OutputNeuron(x + y * 3) = -100
                        QList(x + y * 3) = -100
                    End If
                Next
            Next

            LargestQ = 0
            For x = 0 To NoOfOutputs - 1
                If QList(x) > QList(LargestQ) Then
                    LargestQ = x
                End If
            Next

            Point2 = Math.Floor(LargestQ / 3)
            Point1 = LargestQ - Point2 * 3



            Board(Point1, Point2) = Team

            Action = LargestQ
        End If


        If Not DontMove Then
            State = Board.Clone
        Else
            MaxQ = OutputNeuron(LargestQ)
        End If


    End Sub

    Sub CheckWin()
        For y = 0 To 2

            If CheckAction(0, y) <> "." Then

                If CheckAction(0, y) = CheckAction(1, y) And CheckAction(1, y) = CheckAction(2, y) Then

                    If CheckAction(0, y) = "-1" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next


        For x = 0 To 2

























            If CheckAction(x, 0) <> "." Then

                If CheckAction(x, 0) = CheckAction(x, 1) And CheckAction(x, 1) = CheckAction(x, 2) Then

                    If CheckAction(x, 0) = "-1" Then
                        Win0 = True
                    Else
                        WInX = True
                    End If

                End If

            End If

        Next

        If CheckAction(0, 0) <> "." Then
            If CheckAction(0, 0) = CheckAction(1, 1) And CheckAction(1, 1) = CheckAction(2, 2) Then
                If CheckAction(0, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If

        If CheckAction(2, 0) <> "." Then
            If CheckAction(2, 0) = CheckAction(1, 1) And CheckAction(1, 1) = CheckAction(0, 2) Then
                If CheckAction(2, 0) = "-1" Then
                    Win0 = True
                Else
                    WInX = True
                End If
            End If
        End If
    End Sub

    Function IsBoardFull()
        CheckFullBoard = True

        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) = "." Then
                    CheckFullBoard = False
                End If
            Next
        Next

        Return CheckFullBoard
    End Function
End Class
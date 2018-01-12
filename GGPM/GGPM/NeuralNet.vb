Public Class NeuralNet
    Dim SkipTurn As Boolean = False

    Public NetworkErrors1 As New List(Of Double)
    Public NetworkErrors2 As New List(Of Double)
    Public IterationError1 As New List(Of Double)
    Public IterationError2 As New List(Of Double)
    Public OutError As New List(Of Double)
    Dim NoOfInputs As Integer = 18
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 1
    Dim OutputNeuron As Double

    Dim NoOfHiddens As Integer = 10
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

    Dim HOWeights(NoOfHiddens - 1) As Double

    Dim NumofBatches As Integer


    Dim Counter As Integer = 1

    Dim OutputError As Double

    Dim Rnd As New Random

    Dim p As Integer
    Dim q As Integer

    Dim QValue As Double

    Dim Threshold As Double = 0.00001
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

    Dim ReplayLimit As Integer = 100

    Dim Gamma As Double = 0.5

    Dim LearningRate As Double = 0.001

    Dim InputBias(NoOfHiddens - 1) As Double

    Dim HiddenBias(NoOfOutputs - 1) As Double
    Dim Team As String


    Dim n As Integer
    Dim m As Integer

    Dim TrainingInputs(NoOfInputs - 1) As Double
    Dim Target As Double
    Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaHiddenBias As Double
    Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1) As Double
    Dim DeltaNetOutput As Double
    Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double
    Dim DeltaInputError(NoOfHiddens - 1) As Double
    Dim DeltaInputBias(NoOfHiddens - 1) As Double



    Dim RewardCounter As Integer
    Dim RemoveBatch As Integer

    Dim SampleBatches As Integer
    Dim SampleBatch As Integer

    Dim CheckFullBoard As Boolean
    Sub New(T)
        Team = T
        SetIHWeights()
        SetHOWeights()
    End Sub

    Sub PlayGame()

        FindReward()
        If Not SkipTurn And Not Module1.DoBothTeams Then
            Exploration = Rnd.Next(0, 100001) / 100000
            FeedForward()
        End If
        SkipTurn = False
        Threshold = 0.000001 * Module1.Iteration

    End Sub

    'put items from checkaction into the inputneurons
    Sub SetOldInputs()

        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) <> "." And CheckAction(x, y) <> "/" Then
                    InputNeurons(x + y * 3) = Convert.ToDouble(CheckAction(x, y))
                Else
                    InputNeurons(x + y * 3) = 0
                End If

            Next
        Next
    End Sub

    Sub SetNewInputs()

        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) <> "." And CheckAction(x, y) <> "/" Then
                    InputNeurons(x + y * 3 + 9) = Convert.ToDouble(CheckAction(x, y))
                Else
                    InputNeurons(x + y * 3 + 9) = 0
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

        For y = 0 To NoOfHiddens - 1

            HOWeights(y) = Rnd.Next(-100, 101) / 100

        Next
        HiddenBias(0) = Rnd.Next(-100, 101) / 100

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
        OutputNeuron = 0
        For y = 0 To NoOfHiddens - 1

            OutputNeuron += HOWeights(y) * hidden(y)

        Next
        For x = 0 To NoOfOutputs - 1
            OutputNeuron += HiddenBias(x)
        Next

        OutputNeuron = Tanh(OutputNeuron)

        QList(Action) = OutputNeuron
    End Sub

    Function Tanh(t)
        Return (1 / (1 + Math.E ^ -t))
    End Function

    Function derivative(t)
        Return t(1 - t)
    End Function

    Sub FeedForward()

        'set checkaction to the current state
        CheckAction = Module1.State.Clone

        SetOldInputs()
        For x = 0 To 8
            OldInputNeurons(x) = InputNeurons(x)
        Next
        If Exploration > Threshold Then


            'set the action to a random value
            Do
                Action = Rnd.Next(0, 9)
                n = Math.Floor(Action / 3)
                m = Action - n * 3
            Loop Until Module1.State(m, n) = "."

            Module1.State(m, n) = Team

        Else


            For y = 0 To 2

                For x = 0 To 2
                    'if the point in state is empty
                    If Module1.State(x, y) = "." Then
                        Action = x + y * 3

                        'set the checkaction to 1
                        CheckAction(x, y) = Team

                        'feedforward through neural network
                        SetNewInputs()
                        SetHiddenNeurons(InputNeurons)
                        SetOutputNeurons(HiddenNeurons)

                        'mark the state with a check
                        Module1.State(x, y) = "/"

                        'mark the checkaction with a check 
                        CheckAction = Module1.State.Clone
                    Else

                        'make it unavailable
                        QList(x + y * 3) = -100
                    End If
                Next
            Next


            LargestQ = 0

            'find the largest calculated q value
            For x = 0 To 8

                If QList(x) > QList(LargestQ) And QList(x) <> 0 Then
                    LargestQ = x
                End If
            Next

            'reset the state back to normal

            For y = 0 To 2
                For x = 0 To 2
                    If Module1.State(x, y) = "/" Then
                        Module1.State(x, y) = "."
                    End If
                Next
            Next

            'set the leargest q value position to the next move
            p = Math.Floor(LargestQ / 3)
            q = LargestQ - p * 3

            Module1.State(q, p) = Team





        End If
        CheckAction = Module1.State.Clone

        SetNewInputs()
        For x = 0 To 8
            NewInputNeurons(x) = InputNeurons(x + 9)
        Next
        InputNeurons = InputNeurons

    End Sub

    Sub TrainNet()

        'retrieve inputs from batch
        For y = 0 To 2
            For x = 0 To 2
                CheckAction(x, y) = Batchx.OldInputs(x + y * 3)
            Next
        Next
        SetOldInputs()
        For y = 0 To 2
            For x = 0 To 2
                CheckAction(x, y) = Batchx.NewInputs(x + y * 3)
            Next
        Next
        SetNewInputs()

        'retrieve target reward from batch

        Target = (Batchx.ChangeInReward + 1) / 2

        'feedforward


        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)

        'backprop

        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            DeltaHOWeights(x) = (-(Target - OutputNeuron)) * (1 - OutputNeuron ^ 2) * HiddenNeurons(x)
        Next

        DeltaHiddenBias = (OutputNeuron - Target) * (1 - OutputNeuron ^ 2) * 1

        IterationError1.Add(Module1.Iteration)
        If Team = "1" Then
            NetworkErrors1.Add((OutputNeuron - Target))

            OutError.Add(OutputNeuron)

        Else
            NetworkErrors2.Add((OutputNeuron - Target))
            IterationError1.Add(Module1.Iteration)
            OutError.Add(OutputNeuron)
        End If









        DeltaNetOutput = (OutputNeuron - Target) * (1 - OutputNeuron ^ 2)



        For x = 0 To NoOfHiddens - 1
            DeltaHiddenError(x) = HOWeights(x) * DeltaNetOutput
        Next


        For x = 0 To NoOfHiddens - 1
            DeltaInputError(x) = InputBias(x) * DeltaNetOutput
        Next





        For x = 0 To NoOfHiddens - 1
            DeltaInputBias(x) = 0
        Next


        For x = 0 To NoOfHiddens - 1

            DeltaInputBias(x) = DeltaInputError(x) * (1 - HiddenNeurons(x) ^ 2) * 1

        Next



        For y = 0 To NoOfInputs - 1
            For x = 0 To NoOfHiddens - 1

                DeltaIHWeights(x + (y * NoOfHiddens)) = DeltaHiddenError(x) * (1 - HiddenNeurons(x) ^ 2) * InputNeurons(y)
            Next
        Next

        'update weights
        For x = 0 To NoOfHiddens * NoOfOutputs - 1
            HOWeights(x) -= LearningRate * DeltaHOWeights(x)
        Next

        For x = 0 To NoOfHiddens - 1
            InputBias(x) -= LearningRate * DeltaInputBias(x)
        Next

        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) -= LearningRate * DeltaHiddenBias

        Next

        For x = 0 To NoOfInputs * NoOfHiddens - 1
            IHWeights(x) -= LearningRate * DeltaIHWeights(x)
        Next


    End Sub

    Sub FindReward()
        CheckAction = Module1.State.Clone


        Module1.CheckWin()

        Reward = 0
        'find reward
        If Module1.WInX And Team = "1" Then
            Reward = 1
        ElseIf Module1.Win0 And Team = "1" Then
            Reward = -1

        End If
        If Module1.WInX And Team = "0.5" Then
            Reward = -1
        ElseIf Module1.Win0 And Team = "0.5" Then
            Reward = 1

        End If

        'if this is not the first move of the game



        'reduce the reward by how far away the move is from when the reward was obtained
        RewardCounter = Batches.Count

        'update the reward for each batch with the current reward
        For Each batch In Batches

            batch.ChangeInReward += Reward * Gamma ^ (RewardCounter)

            RewardCounter -= 1

        Next


        'create a new batch from the new move
        If Counter > 1 Then
            Batchx = New Batch(OldInputNeurons, NewInputNeurons, Reward)
            Batches.Add(Batchx)

        End If

        'if game is over or enough moves have been made add the game sets
        If Win0 Or WInX Or IsBoardFull() Then
            Counter = 1

            'You need to stop the cloning from happening here
            '
            '
            ReplayBatches.Add(New List(Of Batch)(Batches))
            '      ReplayBatches(ReplayBatches.Count - 1).AddRange(Batches.ToList())
            '
            '
            '



            'remove previous batches to make space for new batches
            If ReplayBatches.Count > ReplayLimit Then
                RemoveBatch = 0
                Do
                    Batches = ReplayBatches(RemoveBatch)
                    ReplayBatches.Remove(Batches)
                    RemoveBatch += 1
                Loop Until ReplayBatches.Count <= ReplayLimit

            End If

            'select random num of batches and train network on them

            'reset map
            If Module1.DoBothTeams = True Then

                    For y = 0 To 2
                        For x = 0 To 2
                            Module1.State(x, y) = "."

                        Next
                    Next

                    Module1.DoBothTeams = False
                Else
                    Module1.DoBothTeams = True
                End If
                Batches.Clear()
                If Team = "0.5" Then
                    SkipTurn = True
                End If
            End If

            'reset whoever won
            Module1.Win0 = False
        Module1.WInX = False
        If ReplayBatches.Count >= ReplayLimit Then



            SampleBatches = Rnd.Next(0, ReplayBatches.Count)
            SampleBatch = Rnd.Next(0, ReplayBatches(SampleBatches).Count)
            Batchx = ReplayBatches(SampleBatches)(SampleBatch)
            TrainNet()




        End If
        'move onto next position of batch
        If Not SkipTurn And Not DoBothTeams Then
            Counter += 1
        End If
    End Sub

    Function IsBoardFull()
        CheckFullBoard = True

        For y = 0 To 2
            For x = 0 To 2
                If Module1.State(x, y) = "." Then
                    CheckFullBoard = False
                End If
            Next
        Next

        Return CheckFullBoard
    End Function


End Class

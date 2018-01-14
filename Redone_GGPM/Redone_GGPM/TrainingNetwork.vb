Public Class TrainingNetwork
    Public Class NeuralNet

        Dim NoOfInputs As Integer = 9
        Dim InputNeurons(NoOfInputs - 1) As Double

        Dim NoOfOutputs As Integer = 1
        Dim OutputNeuron As Double

        Dim NoOfHiddens As Integer = 20
        Dim HiddenNeurons(NoOfHiddens - 1) As Double

        Dim IHWeights(NoOfHiddens * NoOfInputs - 1) As Double

        Dim HOWeights(NoOfHiddens - 1) As Double

        Dim NumofBatches As Integer

        Dim Counter As Integer = 1

        Dim OutputError As Double

        Dim Outputs As List(Of List(Of Double))

        Dim Rnd As New Random

        Dim QValue As Double

        Public State(2, 2) As String

        Dim Threshold As Double = 0.9
        Dim Action As Integer

        Dim Exploration As Double
        Dim CheckAction(2, 2) As String
        Dim QList(8) As Double

        Dim LargestQ As Integer

        Dim Reward As Double

        Dim Batches As New List(Of Batch)
        Dim Batchx As Batch
        Dim ReplayBatches As New List(Of List(Of Batch))

        Dim OldInputNeurons(NoOfInputs - 1) As Double

        Dim ReplayLimit As Integer = 100

        Dim Gamma As Double = 0.5

        Dim LearningRate As Double = 0.1

        Dim InputBias(NoOfHiddens - 1) As Double

        Dim HiddenBias(NoOfOutputs - 1) As Double

        Dim Train As TrainingNetwork

        Sub New()
            SetIHWeights()
            SetHOWeights()
        End Sub

        Sub PlayGame()


            Exploration = Rnd.Next(0, 101) / 100
            FeedForward()

            FindReward()
        End Sub

        'put items from checkaction into the inputneurons
        Sub SetInputs()

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
            Return Math.Tanh(t)
        End Function

        Function derivative(t)
            Return (1 - t ^ 2)
        End Function

        Sub FeedForward()

            'set checkaction to the current state
            CheckAction = State.Clone


            'copy the pervious neurons
            If Counter <> 1 Then
                OldInputNeurons = InputNeurons.Clone
            Else

                For n = 0 To NoOfInputs - 1
                    OldInputNeurons(n) = 0
                Next
            End If


            If Exploration > Threshold Then
                Dim n As Integer
                Dim m As Integer

                'set the action to a random value
                Do
                    Action = Rnd.Next(0, 9)
                    n = Math.Floor(Action / 3)
                    m = Action - n * 3
                Loop Until State(m, n) = "."

                State(m, n) = "1"

            Else


                For y = 0 To 2

                    For x = 0 To 2
                        'if the point in state is empty
                        If State(x, y) = "." Then
                            Action = x + y * 3

                            'set the checkaction to 1
                            CheckAction(x, y) = "1"

                            'feedforward through neural network
                            SetInputs()
                            SetHiddenNeurons(InputNeurons)
                            SetOutputNeurons(HiddenNeurons)

                            'mark the state with a check
                            State(x, y) = "/"

                            'mark the checkaction with a check 
                            CheckAction = State.Clone
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
                        If State(x, y) = "/" Then
                            State(x, y) = "."
                        End If
                    Next
                Next

                'set the leargest q value position to the next move
                Dim p As Integer = Math.Floor(LargestQ / 3)
                Dim q As Integer = LargestQ - p * 3

                State(q, p) = "1"

                CheckAction = State.Clone
                SetInputs()

            End If
        End Sub

        Sub TrainNet()

            'retrieve inputs from batch
            Dim TrainingInputs(NoOfInputs - 1) As Double
            TrainingInputs = Batchx.OldInputs

            'retrieve target reward from batch
            Dim Target As Double
            Target = (Batchx.ChangeInReward + 1) / 2

            'feedforward
            SetHiddenNeurons(InputNeurons)
            SetOutputNeurons(HiddenNeurons)


            'backprop
            Dim DeltaHOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

            For x = 0 To NoOfHiddens * NoOfOutputs - 1
                DeltaHOWeights(x) = (-(Target - OutputNeuron)) * (1 - OutputNeuron ^ 2) * HiddenNeurons(x)
            Next

            Dim DeltaHiddenBias As Double = (OutputNeuron - Target) * (1 - OutputNeuron ^ 2) * 1





            Dim DeltaIHWeights(NoOfInputs * NoOfHiddens - 1) As Double



            Dim DeltaNetOutput As Double


            DeltaNetOutput = (OutputNeuron - Target) * (1 - OutputNeuron ^ 2)

            Dim DeltaHiddenError(NoOfHiddens * NoOfOutputs - 1) As Double

            For x = 0 To NoOfHiddens - 1
                DeltaHiddenError(x) = HOWeights(x) * DeltaNetOutput
            Next

            Dim DeltaInputError(NoOfHiddens - 1) As Double
            For x = 0 To NoOfHiddens - 1
                DeltaInputError(x) = InputBias(x) * DeltaNetOutput
            Next


            Dim DeltaInputBias(NoOfHiddens - 1) As Double


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

            Module1.CheckWin()


            'find reward
            If Module1.WInX Then
                Reward = 1
            ElseIf Module1.Win0 Then
                Reward = -1
            Else
                Reward = -0.1
            End If


            'if this is not the first move of the game
            If Counter > 1 Then


                'reduce the reward by how far away the move is from when the reward was obtained
                Dim RewardCounter As Integer = Counter

                'update the reward for each batch with the current reward
                For Each batch In Batches

                    batch.ChangeInReward += Reward * Gamma ^ (RewardCounter - 1)

                    RewardCounter -= 1

                Next
            End If

            'create a new batch from the new move
            Batchx = New Batch(OldInputNeurons, InputNeurons, Reward)
            Batches.Add(Batchx)

            'if game is over or enough moves have been made add the game sets
            If Win0 Or WInX Or IsBoardFull() Then
                Counter = 0

                ReplayBatches.Add(Batches)

                'select a random batch to use as training data
                NumofBatches = 0
                For Each batch In ReplayBatches
                    NumofBatches += batch.Count
                Next

                'remove previous batches to make space for new batches
                If NumofBatches >= ReplayLimit Then
                    Dim RemoveBatch As Integer = 0
                    Do
                        Batches = ReplayBatches(RemoveBatch)
                        ReplayBatches.Remove(Batches)
                        RemoveBatch += 1
                    Loop Until NumofBatches <= ReplayLimit

                    'select random num of batches and train network on them
                End If
                For y = 0 To 100
                    For x = 0 To ReplayBatches.Count
                        Dim SampleBatches As Integer = Rnd.Next(0, ReplayBatches.Count)
                        Dim SampleBatch As Integer = Rnd.Next(0, ReplayBatches(SampleBatches).Count)
                        Batchx = ReplayBatches(SampleBatches)(SampleBatch)
                        TrainNet()

                    Next
                Next


                'reset map
                For y = 0 To 2
                    For x = 0 To 2
                        State(x, y) = "."

                    Next
                Next

            End If

            'reset whoever won
            Module1.Win0 = False
            Module1.WInX = False

            'move onto next position of batch
            Counter += 1
        End Sub

        Function IsBoardFull()
            Dim CheckFullBoard As Boolean = True

            For y = 0 To 2
                For x = 0 To 2
                    If State(x, y) = "." Then
                        CheckFullBoard = False
                    End If
                Next
            Next

            Return CheckFullBoard
        End Function


    End Class



End Class

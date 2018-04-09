Public Class NeuralNet

    Dim BatchCounter As Integer = 0 'this is a count that counts the number of times the network is trained each move

    Dim NoOfInputs As Integer = 9 'this is the number of input neurons along with the variable of the input neurons
    Dim InputNeurons(NoOfInputs - 1) As Double

    Dim NoOfOutputs As Integer = 9 'this is the number of output neruons along with the output neuron variable
    Dim OutputNeuron(NoOfOutputs - 1) As Double

    Dim NoOfHiddens As Integer = 15 'this is the number of hidden neurons, these are a variable that can be changed to affect efficiency of the program
    Dim HiddenNeurons(NoOfHiddens - 1) As Double

    Public IHWeights(NoOfHiddens * NoOfInputs - 1) As Double 'these are the weights attahced between the input, hidden, and output neurons
    Public HOWeights(NoOfHiddens * NoOfOutputs - 1) As Double

    Dim NumofBatches As Integer = -400 'this is another variable that can be changed, it determines the amount of initial batches must be created before experience replay statrts, in this case it is 400

    Dim MaxQ As Double 'this is the largest q value produced from a feedforward of the network.

    Dim XWin As Boolean 'these are booleans stating which side won
    Dim OWin As Boolean

    Dim ChooseBatch As Integer 'this is a random value that selects a random batch from the stored batches

    Dim RemoveBatchCounter As Integer = 1 'this is a value that determines the number of batches that need to be removed from the list of batches to prevent overflowing

    Dim Rnd As New Random 'allows for random values to be produced

    Public Threshold As Double = 0 'this is a variable that can be changed to effect the chance a random move will be placed instead of choosing the 'thought' best move
    Dim Action As Integer

    Dim BatchLimit As Integer = 440

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

    Dim ReplayLimit As Integer = 40

    Dim NumOfDraws As Integer = 0

    Dim Gamma As Double = 1

    Dim LearningRate As Double = 0.001

    Public InputBias(NoOfHiddens - 1) As Double

    Public HiddenBias(NoOfOutputs - 1) As Double
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
        Team = T ' set team and weights
        SetIHWeights()
        SetHOWeights()
    End Sub

    Sub SetInputs(Board(,) As String) ' set input neurons to board positions
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


    Sub SetIHWeights() 'set the input to hidden weights
        For x = 0 To NoOfHiddens * NoOfInputs - 1
            IHWeights(x) = Rnd.Next(-100, 101) / 100
        Next
        For x = 0 To NoOfHiddens - 1
            InputBias(x) = Rnd.Next(-100, 101) / 100
        Next
    End Sub


    Sub SetHOWeights() 'set the hidden to output weights
        For x = 0 To NoOfOutputs - 1
            For y = 0 To NoOfHiddens - 1
                HOWeights(y + x * NoOfHiddens) = Rnd.Next(-100, 101) / 100
            Next
        Next
        For x = 0 To NoOfOutputs - 1
            HiddenBias(x) = Rnd.Next(-100, 101) / 100
        Next
    End Sub

    Sub SetHiddenNeurons(input() As Double) ' feedforward to hidden neurons
        'reset hidden neurons
        For x = 0 To NoOfHiddens - 1
            HiddenNeurons(x) = 0
        Next
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

    Sub SetOutputNeurons(hidden() As Double) 'feedforward output neurons
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
        For x = 0 To NoOfOutputs - 1 ' set q values to output neurons
            QList(x) = OutputNeuron(x)
        Next
    End Sub

    Function Tanh(t) ' use activation function on t
        Return Math.Tanh(t)
    End Function

    Function derivative(t) ' find derivative of t
        Return (1 - Math.Tanh(t) ^ 2)
    End Function

    Sub TrainNet(Optional TrainingBatch As Batch = Nothing)
        Exploration = 0
        If TrainingBatch Is Nothing Then ' set the targets
            FeedForward(TrainingState.Clone, True, True) 'retrieve q values for state

            For x = 0 To NoOfOutputs - 1
                If x <> TrainingAction Then
                    If QList(x) = -100 Then 'if this move is illegal make it -1 reward
                        Target(x) = -1
                    Else
                        Target(x) = QList(x)

                    End If
                Else
                    Target(x) = Reward + (Gamma * NextMaxQ)
                End If
            Next
        Else
            TrainingState = TrainingBatch.BoardState.Clone
            Dim Training As Boolean = True
            FeedForward(TrainingBatch.NewState.Clone, True, True) ' find largest q value
            NextMaxQ = MaxQ
            CheckAction = TrainingBatch.NewState.Clone

            If TrainingBatch.Reward <> 0 Or IsBoardFull() Then 'if last move only use immediate reward
                NextMaxQ = 0
            End If

            FeedForward(TrainingState.Clone, True, True)
            'set targets
            For x = 0 To NoOfOutputs - 1 'if this move is illegal make it -1 reward
                If x <> TrainingBatch.Action Then
                    If QList(x) = -100 Then
                        Target(x) = -1
                    Else
                        Target(x) = QList(x)
                    End If
                End If
            Next

            Target(TrainingBatch.Action) = TrainingBatch.Reward + (Gamma * NextMaxQ)
            TrainingAction = TrainingBatch.Action
        End If


        'BackPropagation
        'Find Gradients of errors
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

        If TrainingBatch Is Nothing Then 'count number of times going through training
        Else
            BatchCounter += 1
        End If

        'update delta weights
        For y = 0 To NoOfHiddens
            For x = 0 To NoOfOutputs - 1
                If y = NoOfHiddens Then
                    DeltaHiddenBias(x) += (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * 1
                Else
                    DeltaHOWeights(x + y * NoOfOutputs) += (OutputNeuron(x) - Target(x)) * derivative(OutputNeuron(x)) * HiddenNeurons(y)
                End If
            Next
        Next
        For y = 0 To NoOfInputs
            For x = 0 To NoOfHiddens - 1
                If y = NoOfInputs Then
                    DeltaInputBias(x) += DeltaHiddenError(x) * derivative(HiddenNeurons(x)) * 1
                Else
                    DeltaIHWeights(x + (y * NoOfHiddens)) += DeltaHiddenError(x) * derivative(HiddenNeurons(x)) * InputNeurons(y)
                End If
            Next
        Next


        If BatchCounter = 20 Then
            If TrainingBatch Is Nothing Then

            Else ' find average of delta weights
                For y = 0 To NoOfHiddens
                    For x = 0 To NoOfOutputs - 1
                        If y = NoOfHiddens Then
                            DeltaHiddenBias(x) /= 20
                        Else
                            DeltaHOWeights(x + y * NoOfOutputs) /= 20
                        End If
                    Next
                Next
                For y = 0 To NoOfInputs
                    For x = 0 To NoOfHiddens - 1
                        If y = NoOfInputs Then
                            DeltaInputBias(x) /= 20
                        Else
                            DeltaIHWeights(x + (y * NoOfHiddens)) /= 20
                        End If
                    Next
                Next
            End If

            For x = 0 To NoOfHiddens * NoOfOutputs - 1 'update weights
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

            For y = 0 To NoOfHiddens 'reset deltaweights to 0
                For x = 0 To NoOfOutputs - 1
                    If y = NoOfHiddens Then
                        DeltaHiddenBias(x) = 0
                    Else
                        DeltaHOWeights(x + y * NoOfOutputs) = 0
                    End If
                Next
            Next
            For y = 0 To NoOfInputs
                For x = 0 To NoOfHiddens - 1
                    If y = NoOfInputs Then
                        DeltaInputBias(x) = 0
                    Else
                        DeltaIHWeights(x + (y * NoOfHiddens)) = 0
                    End If
                Next
            Next
        End If
    End Sub

    Sub FindReward()
        CheckAction = Module1.State.Clone
        CheckWin()

        'find reward
        If XWin And Team = "1" Then
            Reward = 0.9
        ElseIf OWin And Team = "1" Then
            Reward = -0.9
        ElseIf XWin And Team = "-1" Then
            Reward = -0.9
        ElseIf OWin And Team = "-1" Then
            Reward = 0.9
        Else
            Reward = 0
        End If

    End Sub

    Sub PlayGame()

        Exploration = Rnd.Next(0, 100) / 100
        If Exploration >= 0.5 Then 'make random player start
            TrainingNet.FeedForward(Module1.State, False)
        End If

        If Threshold < 0.9 Then 'cap the threshold at 0.9
            Threshold += 1 / NumOfIterations ' increase the threshold value so that it increases proportionally to the number of iterations there will be

        End If
        InGame = True
        While InGame
            CheckAction = Module1.State.Clone 'set the team board to the current game state
            Reward = 0

            SetUpTrainingBatch() ' make a move and set up the training batch

            CheckWin()
            InGame = Not IsGameFinished() ' check if game is finished

            OpponentMove() ' opponents turn

            CheckAction = Module1.State.Clone 'update training board

            CheckWin()
            InGame = Not IsGameFinished() ' check if game is finished

            FindReward() ' get reward for current game state

            Target(TrainingAction) = Reward + (Gamma * NextMaxQ) ' find target

            BatchCounter = 20
            TrainNet() ' immediately train net without going through the batchcounters

            NumofBatches += 1 'add the batch to the minibatch list
            AddBatch = New Batch(TrainingState.Clone, TrainingAction, Module1.State.Clone, Reward)
            MiniBatch.Add(AddBatch)

            EmptyBoard() ' empty the board if the game is finished

            ExperienceReplay() ' train on previous batches
        End While

    End Sub

    Sub FindMaxQ() 'find the largest q value for the current state
        Exploration = 0
        If InGame Then
            FeedForward(CheckAction, True)
            NextMaxQ = MaxQ
        Else
            NextMaxQ = 0
        End If
    End Sub

    Sub ExperienceReplay() ' train on previous batches selected at random
        BatchCounter = 0
        If NumofBatches >= ReplayLimit Then
            For x = 0 To 19
                ChooseBatch = Rnd.Next(0, MiniBatch.Count)
                TrainNet(MiniBatch(ChooseBatch))
            Next
            NumofBatches = 0
        End If

        RemoveBatchCounter = 0 'remove batches if the list gets too big
        While MiniBatch.Count > BatchLimit
            MiniBatch.RemoveAt(RemoveBatchCounter)
            RemoveBatchCounter += 1
        End While
    End Sub

    Sub EmptyBoard()
        CheckWin()  'reset board to empty if game is finished
        If XWin = True Or OWin = True Or IsBoardFull() = True Then
            For y = 0 To 2
                For x = 0 To 2
                    Module1.State(x, y) = "."
                Next
            Next
            InGame = False
        End If
    End Sub

    Sub OpponentMove() ' make opponents move on current board
        If Team = "1" And InGame Then
            Module1.TrainingNet.Exploration = Rnd.Next(0, 5000001) / 5000000
            Module1.TrainingNet.FeedForward(Module1.State.Clone, False)
        ElseIf InGame Then
            Module1.Net.Exploration = Rnd.Next(0, 5000001) / 5000000
            Module1.Net.FeedForward(Module1.State.Clone, False)
        End If
    End Sub
    Function IsGameFinished() ' check if a team has one or the board is full
        If IsBoardFull() Or XWin Or OWin Then
            Return True
        Else
            Return False
        End If
    End Function

    Sub SetUpTrainingBatch() 'find the training batch values
        Exploration = Rnd.Next(0, 5000001) / 5000000
        TrainingState = Module1.State.Clone
        FeedForward(CheckAction, False)
        TrainingAction = Action
        CheckAction = Module1.State.Clone
    End Sub


    Sub FeedForward(Board(,) As String, DontMove As Boolean, Optional Training As Boolean = False)

        Dim m As Integer = Rnd.Next(0, 3)
        Dim N As Integer = Rnd.Next(0, 3)

        If Not Training Then
            Board = Module1.State.Clone ' set the board to the current game state
        End If

        '     SetOldInputs()
        If Exploration > Threshold Then ' if selecting a random move
            Do ' choose a random move 
                m = Rnd.Next(0, 3)
                N = Rnd.Next(0, 3)
            Loop Until Board(m, N) = "."

            Board(m, N) = Team ' set the action and place the move
            Action = N * 3 + m

        Else

            SetInputs(Board) ' forward propogate the board's state
            ForwardProp()

            For y = 0 To 2
                For x = 0 To 2
                    If Board(x, y) <> "." Then 'if position on board is taken make it null
                        QList(x + y * 3) = -100
                    End If
                Next
            Next

            LargestQ = 0
            For x = 0 To NoOfOutputs - 1 ' find the largest q value
                If QList(x) > QList(LargestQ) Then
                    LargestQ = x
                End If
            Next

            Point2 = Math.Floor(LargestQ / 3) ' set the points to the largest q value position
            Point1 = LargestQ - Point2 * 3

            If Not DontMove Then ' set board and action
                Board(Point1, Point2) = Team
                Action = LargestQ
            End If
        End If

        If Not DontMove Then 'update the game state if it is playing the game
            State = Board.Clone
        Else
            MaxQ = OutputNeuron(LargestQ)
        End If


    End Sub

    Sub ForwardProp() ' forward propogate through neural network
        SetHiddenNeurons(InputNeurons)
        SetOutputNeurons(HiddenNeurons)
    End Sub

    Sub CheckWin() ' check all possible win moves
        OWin = False
        XWin = False
        For y = 0 To 2
            If CheckAction(0, y) <> "." Then
                If CheckAction(0, y) = CheckAction(1, y) And CheckAction(1, y) = CheckAction(2, y) Then
                    If CheckAction(0, y) = "-1" Then
                        OWin = True
                    Else
                        XWin = True
                    End If
                End If
            End If
        Next
        For x = 0 To 2
            If CheckAction(x, 0) <> "." Then
                If CheckAction(x, 0) = CheckAction(x, 1) And CheckAction(x, 1) = CheckAction(x, 2) Then
                    If CheckAction(x, 0) = "-1" Then
                        OWin = True
                    Else
                        XWin = True
                    End If
                End If
            End If
        Next
        If CheckAction(0, 0) <> "." Then
            If CheckAction(0, 0) = CheckAction(1, 1) And CheckAction(1, 1) = CheckAction(2, 2) Then
                If CheckAction(0, 0) = "-1" Then
                    OWin = True
                Else
                    XWin = True
                End If
            End If
        End If
        If CheckAction(2, 0) <> "." Then
            If CheckAction(2, 0) = CheckAction(1, 1) And CheckAction(1, 1) = CheckAction(0, 2) Then
                If CheckAction(2, 0) = "-1" Then
                    OWin = True
                Else
                    XWin = True
                End If
            End If
        End If
    End Sub

    Function IsBoardFull() ' check if baord is full
        CheckFullBoard = True
        For y = 0 To 2
            For x = 0 To 2
                If CheckAction(x, y) = "." Then
                    CheckFullBoard = False
                End If
            Next
        Next
        If CheckFullBoard = True Then
            Console.WriteLine()
        End If
        Return CheckFullBoard
    End Function
End Class
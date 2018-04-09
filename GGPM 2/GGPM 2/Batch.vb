Public Class Batch 'this class is for when a 'batch', a set of info about a certain move is stored to be trained on later
    Public BoardState(,) As String 'this records the state beofore a mvoe was made
    Public Action As Integer 'this records the position that the move was made by the player
    Public NewState(,) As String 'this records the state after the player and opponent made the move
    Public Reward As Double 'this records the immediate reward after the player and opponent's move

    Sub New(S(,) As String, A As Integer, NS(,) As String, R As Double) 'this creates the batch and sets the values
        BoardState = S.Clone
        Action = A
        NewState = NS.Clone
        Reward = R
    End Sub
End Class
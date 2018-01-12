Public Class Batch
    Public OldInputs() As Double
    Public NewInputs() As Double
    Public ChangeInReward As Double

    Sub New(O() As Double, N() As Double, R As Double)
        OldInputs = O.Clone
        NewInputs = N.Clone
        ChangeInReward = R
    End Sub
End Class
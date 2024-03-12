' Define ExprC Type
Public Enum ExprCType
    NumC
    IdC
    StrC
    IfC
    LamC
    AppC
End Enum

' Define ExprC Class
Public Class ExprC
    Public Property Type As ExprCType
    Public Property Num As Double?
    Public Property Id As String
    Public Property Str As String
    Public Property Condition As ExprC
    Public Property ThenExpr As ExprC
    Public Property ElseExpr As ExprC
    Public Property Ids As List(Of String)
    Public Property Body As ExprC
    Public Property Func As ExprC
    Public Property Arguments As List(Of ExprC)
End Class

' Define Value Type
Public Enum ValueType
    NumV
    StrV
    CloV
    BoolV
    PrimV
End Enum

' Define Value Class
Public Class Value
    Public Property Type As ValueType
    Public Property Num As Double?
    Public Property Str As String
    Public Property Args As List(Of String)
    Public Property Body As ExprC
    Public Property Env As Environment
    Public Property Bool As Boolean?
    Public Property Op As String
End Class

' Define Environment Class
Public Class Environment
    Public Property Bindings As List(Of Bind)
End Class

' Define Bind Class
Public Class Bind
    Public Property Name As String
    Public Property Val As Value
End Class

' Top Level Environment
Public Class TopEnv
    Public Shared ReadOnly Bindings As List(Of Bind) = New List(Of Bind) From {
        New Bind With {.Name = "+", .Val = New Value With {.Type = ValueType.PrimV, .Op = "+"}},
        New Bind With {.Name = "-", .Val = New Value With {.Type = ValueType.PrimV, .Op = "-"}},
        New Bind With {.Name = "*", .Val = New Value With {.Type = ValueType.PrimV, .Op = "*"}},
        New Bind With {.Name = "/", .Val = New Value With {.Type = ValueType.PrimV, .Op = "/"}},
        New Bind With {.Name = "<=", .Val = New Value With {.Type = ValueType.PrimV, .Op = "<="}},
        New Bind With {.Name = "equal?", .Val = New Value With {.Type = ValueType.PrimV, .Op = "equal?"}},
        New Bind With {.Name = "error", .Val = New Value With {.Type = ValueType.PrimV, .Op = "error"}},
        New Bind With {.Name = "true", .Val = New Value With {.Type = ValueType.BoolV, .Bool = True}},
        New Bind With {.Name = "false", .Val = New Value With {.Type = ValueType.BoolV, .Bool = False}}
    }
End Class


' Create instances of ExprC
Dim numExpr As New ExprC With {.Type = ExprCType.NumC, .Num = 10.5}
Dim idExpr As New ExprC With {.Type = ExprCType.IdC, .Id = "x"}
Dim strExpr As New ExprC With {.Type = ExprCType.StrC, .Str = "Hello, World!"}
Dim ifExpr As New ExprC With {.Type = ExprCType.IfC, .Condition = idExpr, .ThenExpr = numExpr, .ElseExpr = strExpr}
Dim lamExpr As New ExprC With {.Type = ExprCType.LamC, .Ids = New List(Of String) From {"x", "y"}, .Body = idExpr}
Dim appExpr As New ExprC With {.Type = ExprCType.AppC, .Func = idExpr, .Arguments = New List(Of ExprC) From {numExpr, strExpr}}
